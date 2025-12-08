#' Correlation Heatmap Plot
#'
#' Creates a publication-ready correlation heatmap for numeric variables in a data frame.
#' Each tile shows the correlation coefficient, with optional significance stars.
#'
#' @param data A data frame containing numeric variables to correlate.
#' @param method Correlation method: "pearson", "spearman", or "kendall". Default is "pearson".
#' @param conf.level Confidence level for correlation confidence intervals. Default is 0.95.
#' @param stars Logical. If TRUE, adds significance stars based on p-values. Default is TRUE.
#' @param plot.title Character string specifying the plot title. If NULL, a default title is used.
#' @param var.labels Optional character vector of variable labels to replace column names in the plot. Must match number of numeric columns.
#'
#' @import dplyr
#' @import ggplot2
#' @importFrom stats cor.test
#' @return A ggplot object showing the correlation heatmap with correlation coefficients and significance stars.
#' @export
#'
#' @examples
#' plot_cor(mtcars)
#' plot_cor(mtcars, var.labels = colnames(mtcars))
#' plot_cor(mtcars, method = "spearman", stars = FALSE)
plot_cor <- function(data,
                      method = "pearson",
                      conf.level = 0.95,
                      stars = TRUE,
                      plot.title = NULL,
                      var.labels = NULL) {
  # fix global variable notes for R CMD check
  Var1 <- Var2 <- r <- label <- NULL

  # keep only numeric columns
  data <- data[, sapply(data, is.numeric), drop = FALSE]
  vars <- colnames(data)

  # handle var.labels
  if (!is.null(var.labels)) {
    if (length(var.labels) != length(vars)) stop("var.labels length must match number of numeric columns")
  } else {
    var.labels <- vars
  }

  results <- expand.grid(Var1 = vars, Var2 = vars, stringsAsFactors = FALSE)
  results$r <- NA
  results$p <- NA
  results$lowCI <- NA
  results$upCI <- NA
  results$stars <- ""

  # loop over all variable pairs
  for (i in seq_len(nrow(results))) {
    v1 <- results$Var1[i]
    v2 <- results$Var2[i]

    if (v1 == v2) {
      results$r[i] <- 1
      results$stars[i] <- ""
      results$lowCI[i] <- NA
      results$upCI[i] <- NA
    } else {
      test <- tryCatch(
        suppressWarnings(stats::cor.test(data[[v1]], data[[v2]], method = method, conf.level = conf.level)),
        error = function(e) NULL
      )

      if (!is.null(test)) {
        results$r[i] <- test$estimate
        results$p[i] <- test$p.value

        # safe assignment of confidence interval
        if (!is.null(test$conf.int) && length(test$conf.int) == 2) {
          results$lowCI[i] <- test$conf.int[1]
          results$upCI[i] <- test$conf.int[2]
        } else {
          results$lowCI[i] <- NA
          results$upCI[i] <- NA
        }

        if (stars && !is.na(results$p[i])) {
          if (results$p[i] < 0.001) results$stars[i] <- "***"
          else if (results$p[i] < 0.01) results$stars[i] <- "**"
          else if (results$p[i] < 0.05) results$stars[i] <- "*"
        }
      } else {
        results$r[i] <- NA
        results$p[i] <- NA
        results$lowCI[i] <- NA
        results$upCI[i] <- NA
        results$stars[i] <- ""
      }
    }
  }

  # text labels
  results$label <- ifelse(is.na(results$p),
                          sprintf("%.2f", results$r),
                          sprintf("%.2f%s", results$r, results$stars))

  # replace variable names with var.labels
  results$Var1 <- factor(results$Var1, levels = vars, labels = var.labels)
  results$Var2 <- factor(results$Var2, levels = vars, labels = var.labels)

  # default title
  if (is.null(plot.title)) {
    plot.title <- paste0("Correlation plot (", method, " method)")
  }

  # generate heatmap
  ggplot2::ggplot(results, ggplot2::aes(x = Var1, y = Var2, fill = r)) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::geom_text(ggplot2::aes(label = label), size = 4) +
    ggplot2::scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0,
                                  limits = c(-1, 1)) +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                   axis.title = ggplot2::element_blank()) +
    ggplot2::labs(fill = "r") +
    ggplot2::ggtitle(plot.title)
}



