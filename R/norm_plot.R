#' Normality Assessment Plot with Shapiro-Wilk and Kolmogorov–Smirnov Tests
#'
#' This function visualizes the distribution of multiple numeric variables using
#' boxplots or histograms with overlaid normal distribution curves. It automatically
#' selects the appropriate normality test based on sample size: the Shapiro–Wilk test
#' is applied when sample size is ≤ 5000, while the Kolmogorov–Smirnov test is used for
#' larger samples (> 5000). The resulting p-values are displayed directly on the plots.
#'
#' @param data A data frame containing the variables to be tested and plotted.
#' @param vars A character vector of column names (numeric variables) to be assessed for normality.
#' @param geom Character string specifying the plot type. Options are `"box"` for boxplots
#'   and `"hist"` for histograms with normal curves.
#' @param color_bar Fill color for boxplots or histograms.
#' @param color_line Color of the normal distribution curve (only used for histograms).
#' @param xlab X-axis label.
#' @param ylab Y-axis label.
#' @param bins Number of bins used in histograms.
#' @param label_color Color of the normality test p-value text labels.
#' @param label_size Numeric size of the p-value text labels.
#' @param label_vjust Vertical justification of the p-value labels.
#' @param label_hjust Horizontal justification of the p-value labels.
#' @param alpha_bar Transparency level for boxplots or histogram bars.
#' @param sample_size Maximum sample size used for the normality test. When the total
#'   sample size exceeds 5000, the Kolmogorov–Smirnov test is applied automatically.
#' @param label_fraction Fraction of plot height used to automatically position p-value labels.
#' @param position Optional named list of manual `(x, y)` positions for p-value placement per variable.
#' @param p.ypos Optional numeric value or named list to override automatic y-positions for p-values.
#'
#' @return A `ggplot` object displaying the selected normality plots with test p-values.
#'
#' @importFrom ggplot2 ggplot geom_boxplot geom_histogram geom_line geom_text
#'   facet_wrap labs theme_minimal theme element_text
#' @importFrom dplyr select group_by summarise mutate left_join rowwise ungroup
#' @importFrom tidyr pivot_longer drop_na
#' @importFrom purrr map_dbl
#' @importFrom stats shapiro.test ks.test dnorm
#' @importFrom rlang %||%
#' @import ggplot2
#'
#' @export
#'
#' @examples
#' ## Load example dataset safely
#' data(diamonds, package = "ggplot2")
#' ## Example 1: Boxplots with Shapiro-Wilk test (n <= 5000)
#' plot_norm(
#'   data = diamonds[1:4000, ],
#'   vars = c("carat", "x", "y"),
#'   geom = "box"
#' )
#'
#' ## Example 2: Histograms with Shapiro-Wilk test (n <= 5000)
#' plot_norm(
#'   data = diamonds[1:4000, ],
#'   vars = c("carat", "x", "y"),
#'   geom = "hist",
#'   bins = 20,
#'   p.ypos = 0.6
#' )
#'
#' ## Example 3: Kolmogorov-Smirnov test automatically applied (n > 5000)
#' plot_norm(
#'   data = diamonds[1:6000, ],
#'   vars = c("carat", "x"),
#'   geom = "hist",
#'   bins = 25
#' )
#'
#' ## Example 4: CO2 dataset (base R)
#' plot_norm(
#'   data = CO2,
#'   vars = c("uptake", "conc"),
#'   geom = "hist",
#'   bins = 3
#' )
plot_norm <- function(data, vars,
                           geom = c("box", "hist"),
                           color_bar = "#377eb8",
                           color_line = "darkred",
                           xlab = NULL,
                           ylab = NULL,
                           bins = 20,
                           label_color = "black",
                           label_size = 3.5,
                           label_vjust = 0,
                           label_hjust = 0,
                           alpha_bar = 0.5,
                           sample_size = 5000,
                           label_fraction = 0.05,
                           position = NULL,
                           p.ypos = NULL) {

  geom <- match.arg(geom)

  # Convert to long format
  df_long <- data %>%
    select(all_of(vars)) %>%
    pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value") %>%
    drop_na()

  # Shapiro-Wilk test on a sample
  test_fun <- function(x) {
    x_sample <- if (length(x) > sample_size) sample(x, sample_size) else x
    shapiro.test(x_sample)
  }

  # Compute p-values
  test_res <- df_long %>%
    group_by(Variable) %>%
    summarise(p = test_fun(Value)$p.value, .groups = "drop") %>%
    mutate(label = ifelse(p < 0.001, "p < 0.001", paste0("p = ", signif(p, 3))))

  # Manual positions override
  if (!is.null(position)) {
    # Named list: list(var1 = c(x, y), var2 = c(x, y))
    test_res <- test_res %>%
      rowwise() %>%
      mutate(
        x_pos = if (Variable %in% names(position)) position[[Variable]][1] else 1,
        y_pos = if (Variable %in% names(position)) position[[Variable]][2] else max(df_long$Value[df_long$Variable == Variable], na.rm = TRUE)
      ) %>%
      ungroup()
  } else {
    # Automatic placement
    if (geom == "box") {
      y_pos <- df_long %>%
        group_by(Variable) %>%
        summarise(max_val = max(Value, na.rm = TRUE), .groups = "drop") %>%
        mutate(y_pos = max_val * (1 + label_fraction))
      test_res <- test_res %>% left_join(y_pos, by = "Variable") %>%
        mutate(x_pos = 1)

    } else if (geom == "hist") {
      max_density <- df_long %>%
        group_by(Variable) %>%
        summarise(max_density = max(hist(Value, plot = FALSE)$density), .groups = "drop") %>%
        summarise(max_density = max(max_density)) %>%
        pull(max_density)

      test_res <- test_res %>%
        mutate(y_pos = max_density * (1 + label_fraction),
               x_pos = purrr::map_dbl(Variable, function(v) {
                 mean(range(df_long$Value[df_long$Variable == v], na.rm = TRUE))
               }))
    }
  }

  # Override y_pos with p.ypos if provided
  if (!is.null(p.ypos)) {
    if (is.list(p.ypos)) {
      # Named list per variable
      test_res <- test_res %>%
        rowwise() %>%
        mutate(y_pos = if (Variable %in% names(p.ypos)) p.ypos[[Variable]] else y_pos) %>%
        ungroup()
    } else {
      # Single numeric value for all variables
      test_res <- test_res %>% mutate(y_pos = p.ypos)
    }
  }

  # Plotting
  if (geom == "box") {
    p <- ggplot(df_long, aes(x = "", y = Value)) +
      geom_boxplot(fill = color_bar, alpha = alpha_bar) +
      facet_wrap(~Variable, scales = "free_y") +
      geom_text(data = test_res, aes(x = x_pos, y = y_pos, label = label),
                inherit.aes = FALSE,
                color = label_color, size = label_size,
                vjust = label_vjust, hjust = label_hjust) +
      labs(x = xlab %||% "", y = ylab %||% "Value") +
      theme_minimal() +
      theme(strip.text = element_text(face = "bold"))

  } else if (geom == "hist") {
    # Normal curves
    norm_curves <- df_long %>%
      group_by(Variable) %>%
      summarise(mean = mean(Value), sd = sd(Value),
                min = min(Value), max = max(Value), .groups = "drop") %>%
      rowwise() %>%
      do({
        var_name <- .$Variable
        x <- seq(.$min, .$max, length.out = 100)
        y <- dnorm(x, mean = .$mean, sd = .$sd)
        data.frame(Variable = var_name, x = x, y = y)
      }) %>%
      ungroup()

    p <- ggplot(df_long, aes(x = Value)) +
      geom_histogram(aes(y = ..density..), bins = bins,
                     fill = color_bar, color = "black", alpha = alpha_bar) +
      geom_line(data = norm_curves, aes(x = x, y = y), color = color_line, size = 1) +
      facet_wrap(~Variable, scales = "free_y") +
      geom_text(data = test_res, aes(x = x_pos, y = y_pos, label = label),
                inherit.aes = FALSE,
                color = label_color, size = label_size,
                vjust = label_vjust, hjust = label_hjust) +
      labs(x = xlab %||% "Value", y = ylab %||% "Density") +
      theme_minimal() +
      theme(legend.position = "none",
            strip.text = element_text(face = "bold"))
  }

  return(p)
}





