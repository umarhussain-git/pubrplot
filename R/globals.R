#' @importFrom stats TukeyHSD aov as.formula chisq.test fisher.test kruskal.test qt t.test wilcox.test lm nobs
#' @importFrom graphics hist
NULL

# Declare global variables to avoid R CMD check notes
if (getRversion() >= "2.15.1") utils::globalVariables(c(
  ".", "..density..",
  "Value", "Variable", "max_val",
  "x_pos", "label",
  "err", "pct", "label_text",
  "sd", "se",
  "term", "estimate", "conf.low", "conf.high", "p.value", "model", "Label",
  "xmin", "xmax", "ymin", "ymax", "xpos", "ypos"  # <-- added here
))
