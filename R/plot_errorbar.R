#' Plot Mean with Error Bars
#'
#' This function creates a line plot with points and customizable error bars
#' (standard deviation, standard error, or confidence interval) for a numeric
#' variable grouped by a categorical variable. Mean values can optionally be
#' displayed above the points.
#'
#' @param data A data frame containing the variables to plot.
#' @param var A numeric variable to be summarized and plotted.
#' @param by A grouping (categorical) variable to calculate summary statistics by.
#' @param error Type of error to display: "sd" (standard deviation), "se" (standard error), or "ci" (95% confidence interval). Default is "sd".
#' @param err.mult Numeric multiplier for the error bars. Useful to extend or shrink error bars. Default is 1.5.
#' @param point.shape Shape of the points. Default is 19 (solid circle).
#' @param point.size Size of the points. Default is 3.
#' @param line.color Color of the connecting line. Default is "blue".
#' @param line.size Thickness of the connecting line. Default is 1.
#' @param color.point Color of the points. Default is "black".
#' @param color.error Color of the error bars. Default is "black".
#' @param show.mean Logical; if TRUE, mean values are displayed above points. Default is TRUE.
#' @param text.size Size of the mean value text labels. Default is 3.5.
#' @param err.width Width of the error bars (horizontal whiskers). Default is 0.05.
#' @param x.lab Label for the x-axis. Default is "Group".
#' @param y.lab Label for the y-axis. If NULL, uses the name of `var`.
#' @param title Plot title. Default is NULL.
#' @param rotate Logical; if TRUE, rotates x-axis labels by 45 degrees. Default is FALSE.
#' @param theme_fun ggplot2 theme function to customize the plot appearance. Default is `ggthemes::theme_stata`.
#'
#' @return A ggplot2 object displaying the line plot with points and error bars.
#' @export
#'
#' @examples
#' plot_errorbar(
#'   data = iris,
#'   var = Sepal.Length,
#'   by = Species,
#'   error = "se",
#'   err.mult = 1,
#'   point.shape = 19,
#'   point.size = 3,
#'   line.color = "red",
#'   line.size = 0.5,
#'   color.point = "blue",
#'   color.error = "blue",
#'   show.mean = TRUE,
#'   text.size = 3,
#'   err.width = 0.05,
#'   title = "Mean Sepal Length by Species",
#'   rotate = TRUE
#' )
plot_errorbar <- function(data, var, by,
                          error = c("sd", "se", "ci"),
                          err.mult = 1.5,
                          point.shape = 19,
                          point.size = 3,
                          line.color = "blue",
                          line.size = 1,
                          color.point = "black",
                          color.error = "black",
                          show.mean = TRUE,       # optional mean label
                          text.size = 3.5,
                          err.width = 0.05,       # whisker width
                          x.lab = "Group",
                          y.lab = NULL,
                          title = NULL,
                          rotate = FALSE,
                          theme_fun = ggthemes::theme_stata) {  # default theme

  var <- ensym(var)
  by  <- ensym(by)
  y.lab <- ifelse(is.null(y.lab), as_name(var), y.lab)

  error <- match.arg(error)

  # Summary stats
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

  # Plot
  g <- ggplot(df_sum, aes(x = !!by, y = mean, group = 1)) +
    geom_line(color = line.color, size = line.size) +
    geom_point(shape = point.shape, size = point.size, color = color.point) +
    geom_errorbar(aes(ymin = mean - err*err.mult, ymax = mean + err*err.mult),
                  width = err.width, color = color.error, linewidth = 0.8) +
    labs(x = x.lab, y = y.lab, title = title) +
    theme_fun(base_size = 13)

  # Optional mean labels
  if (show.mean) {
    g <- g + geom_text(aes(y = mean + err*err.mult + 0.02*max(mean + err*err.mult), label = label),
                       size = text.size, vjust = 0)
  }

  if (rotate) g <- g + theme(axis.text.x = element_text(angle = 45, hjust = 1))

  return(g)
}

