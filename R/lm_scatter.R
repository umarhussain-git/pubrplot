#' Scatter Plot with Linear Regression and Equation Annotation
#'
#' This function creates a scatter plot of a numeric outcome against a numeric predictor,
#' optionally grouped by a factor (`by`). A linear regression line is added with optional
#' standard error (SE) shading, and the regression equation and R² value are displayed on the plot.
#'
#' @param data A data frame containing the variables to plot.
#' @param outcome Character string. Name of the numeric outcome variable.
#' @param predictor Character string. Name of the numeric predictor variable.
#' @param by Character string, optional. Name of a factor variable for grouping/faceting.
#' @param point_color Color for the scatter plot points (default: "#377eb8").
#' @param line_color Color for the regression line (default: "#e41a1c").
#' @param se_fill Fill color for the confidence interval shading around the regression line (default: "#e41a1c55").
#' @param line_size Numeric. Line width for the regression line (default: 1).
#' @param se Logical. Whether to display the standard error shading around the regression line (default: TRUE).
#' @param facet_scales Character. Scales argument for `facet_wrap` ("free", "fixed", "free_x", "free_y") (default: "free").
#' @param eq_position Numeric vector of length 2. Relative position of regression equation on the plot: `c(x_pos, y_pos)` (default: c(0.05, 0.95)).
#' @param ncol_by Numeric. Number of columns for faceting (passed to `facet_wrap`) (default: NULL, automatic).
#' @importFrom stats coef
#' @return A ggplot2 object of the scatter plot with regression line and annotated equation.
#'
#' @export
#'
#' @examples
#' # Basic scatter plot with regression line and equation
#' plot_scatter(mtcars, "mpg", "wt")
#'
#' # Scatter plot grouped by cylinder
#' plot_scatter(mtcars, "mpg", "wt", by = "cyl",
#'            point_color = "blue",
#'            line_color = "red",
#'            se_fill = "#ff000055",
#'            line_size = 0.9,
#'            se = TRUE,
#'            eq_position = c(0.5, 0.95),
#'            ncol_by = 2)
plot_scatter <- function(data, outcome, predictor, by = NULL,
                         point_color = "#377eb8",
                         line_color = "#e41a1c",
                         se_fill = "#e41a1c55",
                         line_size = 1,
                         se = TRUE,
                         facet_scales = "free",
                         eq_position = c(0.05, 0.95),
                         ncol_by = NULL) {

  if (!is.null(by)) data[[by]] <- as.factor(data[[by]])

  get_eq_label <- function(df) {
    fit <- lm(as.formula(paste(outcome, "~", predictor)), data = df)
    fit_stats <- broom::glance(fit)
    slope <- coef(fit)[2]
    intercept <- coef(fit)[1]
    paste0("y = ", round(slope, 2), "x + ", round(intercept, 2),
           "\n R^2 = ", round(fit_stats$r.squared, 2))
  }

  if (is.null(by)) {
    eq_label <- get_eq_label(data)
    x_pos <- min(data[[predictor]], na.rm = TRUE) + eq_position[1] * diff(range(data[[predictor]]))
    y_pos <- min(data[[outcome]], na.rm = TRUE) + eq_position[2] * diff(range(data[[outcome]]))

    p <- ggplot(data, aes(x = .data[[predictor]], y = .data[[outcome]])) +
      geom_point(color = point_color) +
      geom_smooth(method = "lm", se = se, color = line_color, fill = se_fill, linewidth = line_size) +
      annotate("text", x = x_pos, y = y_pos, label = eq_label, hjust = 0, vjust = 1, size = 4)

  } else {
    eq_labels <- data %>%
      group_by(.data[[by]]) %>%
      summarise(xmin = min(.data[[predictor]], na.rm = TRUE),
                xmax = max(.data[[predictor]], na.rm = TRUE),
                ymin = min(.data[[outcome]], na.rm = TRUE),
                ymax = max(.data[[outcome]], na.rm = TRUE),
                .groups = "drop") %>%
      rowwise() %>%
      mutate(label = get_eq_label(data[data[[by]] == .data[[by]], ]),
             xpos = xmin + eq_position[1] * (xmax - xmin),
             ypos = ymin + eq_position[2] * (ymax - ymin))

    p <- ggplot(data, aes(x = .data[[predictor]], y = .data[[outcome]])) +
      geom_point(aes(color = .data[[by]])) +
      geom_smooth(aes(group = .data[[by]]), method = "lm", se = se,
                  color = line_color, fill = se_fill, linewidth = line_size) +
      geom_text(data = eq_labels, aes(x = xpos, y = ypos, label = label),
                inherit.aes = FALSE, hjust = 0, vjust = 1, size = 4) +
      facet_wrap(as.formula(paste("~", by)), scales = facet_scales, ncol = ncol_by)
  }

  p + theme_minimal(base_size = 12)
}

