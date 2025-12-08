#' Line Plot with Error Bars by Group and Time
#'
#' This function creates a line plot showing the mean of a numeric variable
#' over time for different groups, with optional error bars (standard deviation,
#' standard error, or 95% confidence interval). Multiple groups are displayed
#' on the same plot with customizable colors, point shapes, and line thickness.
#'
#' @param data A data frame containing the variables to plot.
#' @param var A numeric variable to summarize and plot.
#' @param time A variable representing time points (x-axis). Converted to factor if not already.
#' @param group A grouping variable (color/line grouping) for the plot.
#' @param error Type of error to display: "sd" (standard deviation), "se" (standard error), or "ci" (95% confidence interval). Default is "sd".
#' @param err.mult Numeric multiplier for the error bars. Default is 1.5.
#' @param point.shape Shape of the points. Default is 19 (solid circle).
#' @param point.size Size of the points. Default is 3.
#' @param line.size Thickness of the lines. Default is 1.
#' @param color.lines Vector of colors for the lines/groups. Default is c("red", "blue").
#' @param show.mean Logical; if TRUE, mean values can optionally be displayed above points. Default is FALSE.
#' @param text.size Size of mean value text labels (if `show.mean = TRUE`). Default is 3.5.
#' @param err.width Width of the error bars. Default is 0.05.
#' @param x.lab Label for the x-axis. Default is "Time".
#' @param y.lab Label for the y-axis. If NULL, uses the name of `var`.
#' @param title Plot title. Default is NULL.
#' @param theme_fun ggplot2 theme function to customize plot appearance. Default is `ggthemes::theme_stata`.
#'
#' @import dplyr
#' @return A `ggplot` object displaying the line plot with optional error bars for multiple groups.
#' @export
#'
#' @examples
#' set.seed(123)
#' n_subj <- 10
#' time_points <- c("T1","T2","T3")
#' groups <- c("DrugA","DrugB")
#'
#' df <- expand.grid(
#'   id = 1:n_subj,
#'   time = time_points,
#'   group = groups
#' )
#'
#' # Arrange by group, id, time
#' df <- dplyr::arrange(df, group, id, time)
#'
#' # Add BMI column
#' df <- dplyr::mutate(df,
#'   BMI = dplyr::case_when(
#'     time == "T1" & group == "DrugA" ~ 29 + stats::rnorm(dplyr::n(), 0, 0.3),
#'     time == "T2" & group == "DrugA" ~ 26 + stats::rnorm(dplyr::n(), 0, 0.3),
#'     time == "T3" & group == "DrugA" ~ 22 + stats::rnorm(dplyr::n(), 0, 0.3),
#'     time == "T1" & group == "DrugB" ~ 28 + stats::rnorm(dplyr::n(), 0, 0.3),
#'     time == "T2" & group == "DrugB" ~ 25 + stats::rnorm(dplyr::n(), 0, 0.2),
#'     time == "T3" & group == "DrugB" ~ 21 + stats::rnorm(dplyr::n(), 0, 0.2)
#'   )
#' )
plot_line <- function(data, var, time, group,
                             error = c("sd", "se", "ci"),
                             err.mult = 1.5,
                             point.shape = 19,
                             point.size = 3,
                             line.size = 1,
                             color.lines = c("red", "blue"),
                             show.mean = FALSE,
                             text.size = 3.5,
                             err.width = 0.05,
                             x.lab = "Time",
                             y.lab = NULL,
                             title = NULL,
                             theme_fun = ggthemes::theme_stata) {

  error <- match.arg(error)
  y.lab <- ifelse(is.null(y.lab), deparse(substitute(var)), y.lab)

  # Ensure time is factor
  data <- data %>% mutate({{time}} := factor({{time}}))

  # Summary stats per group and time
  df_sum <- data %>%
    group_by({{group}}, {{time}}) %>%
    summarise(
      n = n(),
      mean = mean({{var}}, na.rm = TRUE),
      sd = sd({{var}}, na.rm = TRUE),
      se = sd / sqrt(n),
      ci = qt(0.975, df = n - 1) * se,
      .groups = "drop"
    )

  df_sum$err <- dplyr::case_when(
    error == "sd" ~ df_sum$sd,
    error == "se" ~ df_sum$se,
    error == "ci" ~ df_sum$ci
  )

  # Plot both groups on same axes
  g <- ggplot(df_sum, aes(x = {{time}}, y = mean, group = {{group}}, color = {{group}})) +
    geom_line(size = line.size) +
    geom_point(shape = point.shape, size = point.size) +
    geom_errorbar(aes(ymin = mean - err*err.mult, ymax = mean + err*err.mult),
                  width = err.width, linewidth = 0.8) +
    labs(x = x.lab, y = y.lab, title = title) +
    scale_color_manual(values = color.lines) +
    theme_fun(base_size = 13)

  return(g)
}


