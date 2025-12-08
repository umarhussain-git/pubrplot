
#' Publication-Quality Numeric Plot with Optional Grouping and Statistical Tests
#'
#' Creates a publication-ready plot for numeric variables, including bar plots, violin plots, boxplots,
#' and combinations (violin + box, violin + jitter, box + jitter). Supports error bars (SD, SE, CI),
#' group comparisons, and automatic or specified statistical tests with optional post-hoc annotations.
#'
#' @param data A data frame containing the variables to plot.
#' @param var Numeric variable to plot (unquoted).
#' @param by Optional grouping variable (unquoted) to create separate groups.
#' @param geom_type Type of plot: "bar", "violin", "box", "violin_box", "violin_jitter", "box_jitter".
#' @param error Type of error to display for bar plots: "sd", "se", or "ci".
#' @param test.type Statistical test type: "auto", "parametric", or "nonparametric".
#' @param vjust Vertical adjustment for text labels. Default is 0.
#' @param rotate Logical, whether to rotate x-axis labels. Default is FALSE.
#' @param x.lab Label for x-axis. Default is "Group".
#' @param y.lab Label for y-axis. Defaults to variable name.
#' @param text.size Size of labels above bars or violin/box plots. Default is 3.5.
#' @param color.violin Fill color for violin plots. Can be a vector of colors per group.
#' @param color.box Fill color for boxplots inside violins. Can be a vector of colors per group.
#' @param box.color Outline color for boxplots. Default is "black".
#' @param color.jitter Color of jittered points. Default is "black".
#' @param jitter.size Size of jittered points. Default is 1.5.
#' @param ptext.size Size of text for post-hoc annotations. Default is 3.
#' @param theme_fun Theme function from `ggthemes` or `ggplot2` for styling. Default is `ggthemes::theme_stata`.
#' @param bar.width Width of bars for bar plots. Default is 0.85.
#' @param box.width Width of boxplots inside violin. Default is 0.2.
#' @param show.posthoc Logical, whether to display post-hoc test results. Default is TRUE.
#' @param err.mult Multiplier for error bars (SD/SE/CI). Default is 1.5.
#' @param position.p Optional vector `c(x, y)` to place post-hoc text manually.
#' @param jitter.width Width of jitter for points in violin_jitter or box_jitter plots. Default is 0.1.
#'
#' @return A `ggplot2` object representing the numeric variable plot.
#' @export  plot_numeric
#'
#' @examples
#' # Violin + Box plot for iris dataset
#' plot_numeric(
#'   data = iris,
#'   var = Sepal.Length,
#'   by = Species,
#'   geom_type = "violin_box",
#'   box.width = 0.1,
#'   color.violin = c("#377eb8", "#ff7f00", "#4daf4a"),
#'   color.box = c("darkgreen", "#a65628", "#f781bf"),
#'   box.color = "black",
#'   color.jitter = "red",
#'   position.p = c(1,9),
#'   jitter.size = 2,
#'   ptext.size = 4,
#'   show.posthoc = TRUE
#' )
#'
#' # Simple bar plot with error bars
#' plot_numeric(
#'   data = iris,
#'   var = Sepal.Length,
#'   by = Species,
#'   geom_type = "bar",
#'   error = "se"
#' )
#'
#' # Violin plot with jitter points
#' plot_numeric(
#'   data = iris,
#'   var = Sepal.Length,
#'   by = Species,
#'   geom_type = "violin_jitter"
#' )
plot_numeric <- function(data, var, by,
                         geom_type = c("bar", "violin", "box", "violin_box",
                                       "violin_jitter", "box_jitter"),
                         error = c("sd", "se", "ci"),
                         test.type = c("auto", "parametric", "nonparametric"),
                         vjust = 0,
                         rotate = FALSE,
                         x.lab = "Group",
                         y.lab = NULL,
                         text.size = 3.5,        # labels above bars
                         color.violin = NULL,    # violin fill
                         color.box = NULL,       # box fill inside violin
                         box.color = "black",    # box outline
                         color.jitter = "black", # jitter points
                         jitter.size = 1.5,      # jitter size
                         ptext.size = 3,         # posthoc text size
                         theme_fun = ggthemes::theme_stata,
                         bar.width = 0.85,
                         box.width = 0.2,
                         show.posthoc = TRUE,
                         err.mult = 1.5,
                         position.p = NULL,
                         jitter.width = 0.1) {

  var <- ensym(var)
  by  <- ensym(by)
  y.lab <- ifelse(is.null(y.lab), as_name(var), y.lab)

  geom_type <- match.arg(geom_type)
  error <- match.arg(error)
  test.type <- match.arg(test.type)

  # Summary stats for bar plots
  df_sum <- data %>%
    group_by(!!by) %>%
    summarise(
      n = n(),
      mean = mean(!!var, na.rm = TRUE),
      sd = sd(!!var, na.rm = TRUE),
      se = sd / sqrt(n),
      ci = qt(0.975, df = n - 1) * se,
      .groups = "drop"
    )
  df_sum$err <- dplyr::case_when(
    error == "sd" ~ df_sum$sd,
    error == "se" ~ df_sum$se,
    error == "ci" ~ df_sum$ci
  )
  df_sum$label <- round(df_sum$mean, 2)
  n_groups <- nlevels(as.factor(data[[as_name(by)]]))

  normal <- if (test.type == "auto") shapiro.test(data[[as_name(var)]])$p.value > 0.05 else test.type == "parametric"

  # Statistical tests
  posthoc_text <- NULL
  post_test_name <- NULL
  if (n_groups == 2 & normal) {
    test <- t.test(as.formula(paste(as_name(var), "~", as_name(by))), data = data)
    p_overall <- test$p.value; test_name <- "t-test"
  } else if (n_groups == 2 & !normal) {
    test <- wilcox.test(as.formula(paste(as_name(var), "~", as_name(by))), data = data)
    p_overall <- test$p.value; test_name <- "Wilcoxon"
  } else if (n_groups > 2 & normal) {
    fit <- aov(as.formula(paste(as_name(var), "~", as_name(by))), data = data)
    p_overall <- summary(fit)[[1]][["Pr(>F)"]][1]; test_name <- "ANOVA"
    if (show.posthoc) {
      tuk <- TukeyHSD(fit)[[1]] %>% as.data.frame()
      tuk$Comparison <- gsub("-", " vs ", rownames(tuk))
      tuk$p_txt <- ifelse(tuk$`p adj` < 0.001, "<0.001", signif(tuk$`p adj`, 2))
      posthoc_text <- paste0(tuk$Comparison, " = ", tuk$p_txt)
      post_test_name <- "Tukey"
    }
  } else {
    fit <- kruskal.test(as.formula(paste(as_name(var), "~", as_name(by))), data = data)
    p_overall <- fit$p.value; test_name <- "Kruskal"
    if (show.posthoc) {
      dunn <- rstatix::dunn_test(data, as.formula(paste(as_name(var), "~", as_name(by))), p.adjust.method = "bonferroni")
      dunn$p_txt <- ifelse(dunn$p.adj < 0.001, "<0.001", signif(dunn$p.adj, 2))
      posthoc_text <- paste0(dunn$group1, " vs ", dunn$group2, " = ", dunn$p_txt)
      post_test_name <- "Dunn"
    }
  }

  # Base plot
  g <- ggplot(data, aes(x = !!by, y = !!var))

  # Bar plot
  if (geom_type == "bar") {
    g <- ggplot(df_sum, aes(x = !!by, y = mean, fill = !!by)) +
      geom_col(width = bar.width, color = "black") +
      geom_errorbar(aes(ymin = mean - err*err.mult, ymax = mean + err*err.mult),
                    width = 0.1, linewidth = 0.8) +
      geom_text(aes(y = mean + err*err.mult + 0.02*max(mean + err*err.mult), label = label),
                vjust = 0, size = text.size)
  }

  # Violin layer
  if (geom_type %in% c("violin", "violin_box", "violin_jitter")) {
    g <- g + geom_violin(aes(fill = !!by), trim = FALSE, alpha = 0.5)
    if (!is.null(color.violin)) g <- g + scale_fill_manual(values = color.violin)
  }

  # Box inside violin
  if (geom_type %in% c("violin_box", "box", "box_jitter", "violin_jitter")) {
    if (!is.null(color.box) & geom_type == "violin_box") {
      # Add boxes per group to control color
      for (i in seq_along(levels(data[[as_name(by)]]))) {
        grp <- levels(data[[as_name(by)]])[i]
        g <- g + geom_boxplot(
          data = filter(data, !!by == grp),
          aes(x = !!by, y = !!var),
          fill = color.box[i],
          color = box.color,
          width = box.width,
          alpha = 0.7
        )
      }
    } else {
      g <- g + geom_boxplot(width = box.width, fill = NA, color = box.color, alpha = 0.7)
    }
  }

  # Jitter points
  if (geom_type %in% c("violin_jitter", "box_jitter")) {
    g <- g + geom_jitter(width = jitter.width, size = jitter.size, color = color.jitter)
  }

  # Labels, theme, posthoc
  g <- g +
    labs(x = x.lab, y = y.lab) +
    theme_fun(base_size = 13) +
    theme(
      legend.position = "",
      legend.title = element_blank(),
      plot.margin = margin(10, 140, 10, 10)
    ) +
    coord_cartesian(clip = "off")

  # Posthoc text
  if (show.posthoc) {
    overall_text <- paste0(test_name, ": ", ifelse(p_overall < 0.001, "<0.001", signif(p_overall, 3)))
    post_lines <- if(!is.null(posthoc_text)) {
      n_lines <- min(3, length(posthoc_text))
      paste0(post_test_name, " post-hoc:\n", paste(posthoc_text[1:n_lines], collapse = "\n"))
    } else NULL
    full_text <- paste(overall_text, post_lines, sep = "\n")
    if (is.null(position.p)) {
      x_pos <- n_groups + 0.3
      y_pos <- max(df_sum$mean + df_sum$err*err.mult, na.rm = TRUE) * 0.95
    } else {
      x_pos <- position.p[1]; y_pos <- position.p[2]
    }
    g <- g + annotate(
      "text", x = x_pos, y = y_pos,
      label = full_text,
      hjust = 0, vjust = 1,
      size = ptext.size, fontface = "italic"
    )
  }

  if (rotate) g <- g + theme(axis.text.x = element_text(angle = 45, hjust = 1))

  return(g)
}


