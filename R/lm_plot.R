#' Plot Linear Regression Estimates with Confidence Intervals
#'
#' This function fits univariate and multivariate linear regression models
#' for a given outcome and a set of predictors. It returns a ggplot showing
#' point estimates and 95% confidence intervals for each predictor. Reference
#' levels of factors can optionally be added, and univariate and multivariate
#' results are plotted side by side.
#'
#' @param data A data frame containing the outcome and predictor variables.
#' @param outcome A string specifying the outcome (dependent) variable.
#' @param predictors A character vector of predictor (independent) variables.
#' @param label_vjust Vertical adjustment for text labels. Default is -0.8.
#' @param label_hjust Horizontal adjustment for text labels. Default is 0.4.
#' @param label_size Size of text labels. Default is 3.5.
#' @param label_color Color of text labels. Default is "black".
#' @param point_color Vector of colors for the points. Default is c("steelblue", "firebrick").
#' @param point_shape Shape of the points. Default is 15.
#' @param ref Logical; if TRUE, adds reference levels for factor variables. Default is TRUE.
#'
#' @return A `ggplot` object showing regression estimates with 95% confidence intervals
#'         for both univariate and multivariate models.
#'  @import broom
#' @importFrom stats lm nobs
#' @importFrom broom tidy
#' @importFrom dplyr mutate filter case_when arrange n bind_rows
#' @importFrom tibble tibble
#' @import ggplot2
#' @export
#'
#' @examples
#' mtcars2 <- dplyr::mutate(
#'   mtcars,
#'   cyl = factor(cyl),
#'   am = factor(am, labels = c("Automatic", "Manual")),
#'   gear = factor(gear)
#' )
#'
#' plot_lm(
#'   data = mtcars2,
#'   outcome = "mpg",
#'   predictors = c("cyl", "hp", "wt", "am", "gear"),
#'   point_shape = 18
#' )
#'
#' plot_lm(
#'   data = mtcars2,
#'   outcome = "mpg",
#'   predictors = c("cyl", "hp", "wt", "am", "gear"),
#'   point_shape = 18
#' )
plot_lm <- function(data, outcome, predictors,
                    label_vjust = -0.8,
                    label_hjust = 0.4,
                    label_size = 3.5,
                    label_color = "black",
                    point_color = c("steelblue", "firebrick"),
                    point_shape = 15,   # <-- shape argument
                    ref = TRUE) {

  # Convert character predictors to factor
  for (var in predictors) {
    if (is.character(data[[var]])) data[[var]] <- as.factor(data[[var]])
  }

  get_lm <- function(formula) {
    lm(formula, data = data) %>% broom::tidy(conf.int = TRUE)
  }

  add_reference <- function(var, fit_tidy) {
    if ((is.factor(data[[var]]) || is.character(data[[var]])) && ref) {
      ref_level <- levels(as.factor(data[[var]]))[1]
      ref_row <- tibble(
        term = paste0(var, "_", ref_level, "_(ref)"),
        estimate = NA, conf.low = NA, conf.high = NA,
        p.value = NA,
        model = unique(fit_tidy$model)
      )
      fit_tidy <- bind_rows(ref_row, fit_tidy)
    }
    fit_tidy$term <- paste0(fit_tidy$term)
    fit_tidy
  }

  # --- Univariate ---
  uni_list <- lapply(predictors, function(var) {
    fit <- get_lm(as.formula(paste(outcome, "~", var))) %>%
      filter(term != "(Intercept)")
    fit$model <- "Univariate \n Linear Regression"
    fit <- add_reference(var, fit)
    fit
  })
  uni_fit <- bind_rows(uni_list)

  # --- Multivariate ---
  formula_multi <- as.formula(paste(outcome, "~", paste(predictors, collapse = " + ")))
  multi_fit <- get_lm(formula_multi) %>% filter(term != "(Intercept)")
  multi_fit$model <- "Multivariate \n Linear Regression"
  for (var in predictors) multi_fit <- add_reference(var, multi_fit)
  multi_mod <- lm(formula_multi, data = data)

  # Multivariate stats
  multi_stats <- tibble(
    model = "Multivariate \n Linear Regression",
    r2 = summary(multi_mod)$r.squared,
    adj_r2 = summary(multi_mod)$adj.r.squared,
    n = nobs(multi_mod)
  )

  # --- Combine ---
  plot_data <- bind_rows(uni_fit, multi_fit) %>%
    mutate(
      term = gsub("[[:space:]\\.]", "_", term),
      Label = ifelse(is.na(estimate), ifelse(ref, "(ref)", ""),
                     paste0(round(estimate,2), " (", round(conf.low,2), "-",
                            round(conf.high,2), "), p=",
                            ifelse(p.value < 0.001, "<0.001", round(p.value,3))))
    )

  # --- Plot ---
  p <- ggplot(plot_data, aes(y = term)) +
    geom_point(data = plot_data %>% filter(!is.na(estimate)),
               aes(x = estimate, color = model),
               position = position_dodge(width = 0.8),
               size = 3.5, shape = point_shape) +   # <-- shape argument
    geom_errorbarh(data = plot_data %>% filter(!is.na(estimate)),
                   aes(xmin = conf.low, xmax = conf.high, color = model),
                   position = position_dodge(width = 0.8), height = 0.05) +
    geom_text(aes(x = ifelse(is.na(estimate), 0, estimate), label = Label),
              color = label_color,
              hjust = label_hjust, vjust = label_vjust,
              size = label_size,
              position = position_dodge(width = 0.6)) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    facet_wrap(~model, ncol = 2, scales = "free_y") +
    labs(x = paste0("R^2=", round(multi_stats$r2,2),
                    ", adj,R^2=", round(multi_stats$adj_r2,2),
                    ", n=", multi_stats$n,
                    "\nEstimates (95% CI)"),
         y = "", color = "Model") +
    scale_color_manual(values = point_color) +
    theme_minimal(base_size = 12) +
    theme(strip.text = element_text(face = "bold"),
          legend.position = "none")

  p
}



