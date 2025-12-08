#' Plot Odds Ratios from Logistic Regression
#'
#' This function fits univariate and multivariate logistic regression models
#' and plots odds ratios with 95% confidence intervals. Reference levels can
#' optionally be displayed.
#'
#' @param data A data frame containing the outcome and predictors.
#' @param outcome Name of the binary outcome variable (as string).
#' @param predictors Vector of predictor variable names (as strings).
#' @param label_vjust Vertical adjustment for labels (default -0.8).
#' @param label_hjust Horizontal adjustment for labels (default 0.5).
#' @param label_size Size of the text labels (default 3.5).
#' @param label_color Color of the text labels (default "black").
#' @param point_color Colors for points corresponding to univariate and multivariate models (default c("steelblue", "firebrick")).
#' @param ref Logical, whether to show reference levels (default TRUE).
#'
#' @return A ggplot object showing odds ratios with confidence intervals.
#' @export plot_or
#'
#' @importFrom broom tidy
#' @importFrom stats glm binomial
#' @importFrom dplyr bind_rows filter mutate
#' @importFrom ggplot2 ggplot aes geom_point geom_errorbarh geom_text geom_vline facet_wrap scale_x_log10 labs scale_color_manual theme_minimal theme element_text position_dodge
#'
#' @examples
#' # Load built-in infertility dataset
#' infert1 <- datasets::infert
#' infert$case <- factor(infert$case, levels = c(0,1), labels = c("Control","Infertile"))
#' infert$induced <- factor(infert$induced, levels = c(0,1), labels = c("No","Yes"))
#' infert$spontaneous <- factor(infert$spontaneous, levels = c(0,1), labels = c("No","Yes"))
#'
#' # Plot with reference levels
#' plot_or(
#'   data = infert1,
#'   outcome = "case",
#'   predictors = c("parity","induced","spontaneous","age"),
#'   ref = TRUE
#' )
#'
#' # Plot without reference levels
#' plot_or(
#'   data = infert1,
#'   outcome = "case",
#'   predictors = c("parity","induced","spontaneous","age"),
#'   ref = FALSE
#' )
plot_or <- function(data, outcome, predictors,
                    label_vjust = -0.8,
                    label_hjust = 0.5,
                    label_size = 3.5,
                    label_color = "black",
                    point_color = c("steelblue", "firebrick"),
                    ref = TRUE) {   # <-- reference optional

  # Convert character predictors to factor
  for (var in predictors) {
    if (is.character(data[[var]])) data[[var]] <- as.factor(data[[var]])
  }

  get_logistic <- function(formula) {
    glm(formula, data = data, family = binomial) %>%
      broom::tidy(conf.int = TRUE, exponentiate = TRUE)
  }

  add_reference <- function(var, fit_tidy) {
    if ((is.factor(data[[var]]) || is.character(data[[var]])) && ref) {
      ref_level <- levels(as.factor(data[[var]]))[1]
      ref_row <- tibble(
        term = paste0(var, "_", ref_level, "_(ref)"),
        estimate = NA, conf.low = NA, conf.high = NA, p.value = NA,
        model = unique(fit_tidy$model)
      )
      fit_tidy <- bind_rows(ref_row, fit_tidy)
    }
    fit_tidy$term <- paste0(fit_tidy$term)
    fit_tidy
  }

  # --- Univariate ---
  uni_list <- lapply(predictors, function(var) {
    fit <- get_logistic(as.formula(paste(outcome, "~", var))) %>%
      filter(term != "(Intercept)")
    fit$model <- "Univariate \n Logistic Regression"
    fit <- add_reference(var, fit)
    fit
  })
  uni_fit <- bind_rows(uni_list)

  # --- Multivariate ---
  formula_multi <- as.formula(paste(outcome, "~", paste(predictors, collapse = " + ")))
  multi_fit <- get_logistic(formula_multi) %>% filter(term != "(Intercept)")
  multi_fit$model <- "Multivariate \n Logistic Regression"
  for (var in predictors) multi_fit <- add_reference(var, multi_fit)

  # --- Combine data ---
  plot_data <- bind_rows(uni_fit, multi_fit) %>%
    mutate(
      term = gsub("[[:space:]\\.]", "_", term),
      Label = ifelse(
        is.na(estimate),
        ifelse(ref, "(ref)", ""),
        paste0(
          round(estimate, 2), " (", round(conf.low, 2), "-", round(conf.high, 2), "), ",
          "p=",
          ifelse(p.value < 0.001, "<0.001", round(p.value, 3))
        )
      )
    )


  # --- Plot ---
  ggplot(plot_data, aes(y = term)) +
    geom_point(data = plot_data %>% filter(!is.na(estimate)),
               aes(x = estimate, color = model),
               position = position_dodge(width = 0.8), size = 3.5) +
    geom_errorbarh(data = plot_data %>% filter(!is.na(estimate)),
                   aes(xmin = conf.low, xmax = conf.high, color = model),
                   position = position_dodge(width = 0.8), height = 0.05) +
    geom_text(aes(x = ifelse(is.na(estimate), 1, estimate), label = Label),
              color = label_color,
              hjust = label_hjust, vjust = label_vjust,
              size = label_size,
              position = position_dodge(width = 0.6)) +
    geom_vline(xintercept = 1, linetype = "dashed") +
    scale_x_log10() +
    labs(x = "Odds Ratio (95% CI)", y = "", color = "Model") +
    scale_color_manual(values = point_color) +
    facet_wrap(~model, ncol = 2, scales = "free_x") +
    theme_minimal(base_size = 12 ) +
    theme(strip.text = element_text(face = "bold"),
          legend.position = "none")   # <-- hide legend
}



