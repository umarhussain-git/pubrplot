#' Bar Plot for Categorical Data with Optional Grouping
#'
#' Creates a publication-quality bar plot for a categorical variable, with optional grouping by another variable.
#' Automatically calculates counts and percentages and can display them on the bars. Also performs Chi-square or Fisher exact test if `by` is provided.
#'
#' @param data A data frame containing the variables to plot.
#' @param var The main categorical variable to display on the x-axis (unquoted or quoted).
#' @param by Optional grouping variable for stacked/dodged bars (unquoted or quoted). Default is NULL.
#' @param vjust Vertical adjustment for text labels. Default is -0.3.
#' @param hjust Horizontal adjustment for text labels. Default is 0.5.
#' @param axis.label.angle Angle of x-axis labels. Default is 45.
#' @param label Optional custom labels for factor levels of `var`.
#' @param border.color Optional color for bar borders. Default is NULL.
#' @param label.color Color of the text labels on bars. Default is "black".
#' @param x.lab Label for x-axis. Default is "Group".
#' @param y.lab Label for y-axis. Default is "Percentage (%)".
#' @param fill.lab Legend title for the fill variable. Default is "Variable".
#' @param text.size Size of the text labels. Default is 3.
#' @param color.bar Optional vector of colors for bars.
#' @param theme_fun Theme function from ggthemes (or ggplot2) for styling. Default is `ggthemes::theme_stata`.
#' @param bar.width Width of the bars. Default is 0.8.
#' @param y.expand Factor to expand the y-axis for space above the highest bar. Default is 1.12.
#'
#' @import rlang
#' @import ggthemes
#' @import ggplot2
#' @import tibble
#'
#' @return A `ggplot2` object representing the bar plot.
#' @export  plot_bar
#'
#' @examples
#' # Example using CO2 dataset
#' plot_bar(
#'   CO2,
#'   var = "Type",
#'   by = "Treatment",
#'   fill.lab = "Plant Type",
#'   color.bar = c("lightblue","lightgreen"),
#'   border.color = "black",
#'   bar.width = 0.5,
#'   text.size = 3,
#'   label = c("Quebec","Mississippi")
#' )
#'
#' # Example using diamonds dataset
#' plot_bar(
#'   ggplot2::diamonds,
#'   var = "cut",
#'   by = "color",
#'   y.lab = "Distribution (%)",
#'   fill.lab = "Cut",
#'   text.size = 2,
#'   bar.width = 0.9,
#'   color.bar = c("#a465db","steelblue","darkgreen","darkred","#fcba03")
#' )
#'
#' # Simple bar plot without grouping
#' plot_bar(ggplot2::diamonds, var = "cut")
#'
plot_bar <- function(data, var, by = NULL,
                     vjust = -0.3, hjust = 0.5,
                     axis.label.angle = 45,
                     label = NULL,
                     border.color = NULL,
                     label.color = "black",
                     x.lab = "Group",
                     y.lab = "Percentage (%)",
                     fill.lab = "Variable",
                     text.size = 3,
                     color.bar = NULL,
                     theme_fun = ggthemes::theme_stata,
                     bar.width = 0.8,
                     y.expand = 1.12) {

  if (is.character(var)) var <- sym(var) else var <- ensym(var)
  if (!is.null(by)) {
    if (is.character(by)) by <- sym(by) else by <- ensym(by)
  }

  if (!is.null(by)) {
    # full contingency table
    tab <- table(data[[as_name(var)]], data[[as_name(by)]])

    # choose test based on counts
    if (any(tab < 5)) {
      test <- fisher.test(tab)
      test_name <- "Fisher exact"
    } else {
      test <- chisq.test(tab)
      test_name <- "Chi-square"
    }
    pval <- ifelse(test$p.value < 0.001,
                   "p < 0.001",
                   paste0("p = ", signif(test$p.value, 3)))
    subtitle <- paste(test_name, pval)

    # aggregation for plot
    df_plot <- data %>%
      group_by(!!by, !!var) %>%
      summarise(n = n(), .groups = "drop") %>%
      group_by(!!by) %>%
      mutate(pct = 100 * n / sum(n),
             label_text = paste0(n, "\n(", round(pct,1), "%)"))

    if (!is.null(label)) {
      df_plot[[as_name(var)]] <- factor(df_plot[[as_name(var)]],
                                        levels = levels(as.factor(df_plot[[as_name(var)]])),
                                        labels = label)
    }

    ymax <- max(df_plot$pct) * y.expand

    g <- ggplot(df_plot, aes(x = !!by, y = pct, fill = !!var))

    # Conditionally add color
    if (!is.null(border.color)) {
      g <- g + geom_col(position = position_dodge(width = bar.width),
                        width = bar.width,
                        color = border.color)
    } else {
      g <- g + geom_col(position = position_dodge(width = bar.width),
                        width = bar.width)
    }

    g <- g + geom_text(aes(label = label_text),
                       position = position_dodge(width = bar.width),
                       vjust = vjust, hjust = hjust,
                       size = text.size,
                       color = label.color) +
      scale_y_continuous(limits = c(0, ymax)) +
      labs(x = x.lab, y = y.lab, fill = fill.lab, subtitle = subtitle) +
      theme_fun(base_size = 13) +
      theme(
        legend.position = "top",
        legend.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = axis.label.angle, hjust = 1)
      )

  } else {
    df_plot <- data %>%
      group_by(!!var) %>%
      summarise(n = n(), .groups = "drop") %>%
      mutate(pct = 100 * n / sum(n),
             label_text = paste0(n, "\n(", round(pct,1), "%)"))

    if (!is.null(label)) {
      df_plot[[as_name(var)]] <- factor(df_plot[[as_name(var)]],
                                        levels = levels(as.factor(df_plot[[as_name(var)]])),
                                        labels = label)
    }

    ymax <- max(df_plot$pct) * y.expand

    g <- ggplot(df_plot, aes(x = !!var, y = pct, fill = !!var))

    if (!is.null(border.color)) {
      g <- g + geom_col(width = bar.width, color = border.color)
    } else {
      g <- g + geom_col(width = bar.width)
    }

    g <- g + geom_text(aes(label = label_text),
                       vjust = vjust, hjust = hjust,
                       size = text.size,
                       color = label.color) +
      scale_y_continuous(limits = c(0, ymax)) +
      labs(x = x.lab, y = y.lab, fill = fill.lab) +
      theme_fun(base_size = 13) +
      theme(
        legend.position = "none",
        legend.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = axis.label.angle, hjust = 1)
      )
  }

  if (!is.null(color.bar)) g <- g + scale_fill_manual(values = color.bar)

  return(g)
}








