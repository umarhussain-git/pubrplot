Here is a polished **README

````markdown
# pubrplot  
### Publication-Ready Plots and Statistical Visualizations

`pubrplot` is an R package designed to help researchers, clinicians, and data analysts generate high-quality, publication-ready plots with minimal effort. It provides an easy-to-use interface for visualizing numeric and categorical data using standardized, aesthetic defaults suitable for scientific journals.

---

## 📦 Package Information

**Package:** pubrplot  
**Type:** R Package  
**Version:** 0.0.1  
**Title:** Publication-Ready Plots and Statistical Visualizations  

**Description:**  
Provides functions to create high-quality, publication-ready plots for numeric and categorical data, including bar plots, violin plots, boxplots, line plots, error bars, correlation plots, linear model plots, odds ratio plots, and normality plots.

**Authors@R:**  
```r
person("Umar", "Hussain", email = "drumarhussain@gmail.com",
       role = c("aut", "cre"))
````

**License:** MIT + file LICENSE
**Encoding:** UTF-8
**Roxygen:** markdown = TRUE
**RoxygenNote:** 7.3.2

**Imports:**
`ggplot2`, `dplyr`, `ggthemes`, `rlang`, `broom`,
`tidyr`, `rstatix`, `purrr`, `tibble`

**Maintainer:**
Umar Hussain [drumarhussain@gmail.com](mailto:drumarhussain@gmail.com)

---

## 🚀 Installation

Once the package is hosted on GitHub:

```r
# Install development version from GitHub
remotes::install_github("umarhussain-git/pubrplot")

```

Load the package:

```r
library(pubrplot)
```

---

## 📊 Example Usage

### 1. Bar Plot (Single Variable)

```r
plot_bar(ggplot2::diamonds,
         var = "cut")
```

### 2. Grouped Bar Plot

```r
plot_bar(
  diamonds,
  var = "cut",
  by = "color",
  fill.lab = "color",
  border.color = "black",
  bar.width = 0.8,
  text.size = 3
)
```

### 3. Bar Plot with Custom Colors & Theme

```r
plot_bar(
  data = diamonds,
  var = "cut",
  by = "color",
  theme_fun = theme_economist,
  color.bar = c("firebrick", "lightgreen", "steelblue",
                "darkgreen", "orange")
)
```

---

## 📈 Numeric Data Plots

### 4. Numeric Plot with Significance Annotation

```r
plot_numeric(data = ToothGrowth,
             var = "Petal.Length",
             by = "supp",
             bar.width = 0.3,
             position.p = c(0.5, 30),
             ptext.size = 5)
```

### 5. Violin + Box Plot Combination

```r
plot_numeric(data = iris,
             geom_type = "violin_box",
             var = "Sepal.Width",
             by = "Species",
             bar.width = 0.3,
             position.p = c(0.5, 6),
             color.box = c("tomato", "orange", "steelblue"),
             ptext.size = 3)
```

---

## 🔍 Error Bar Plot

```r
plot_errorbar(data = iris,
              var = "Sepal.Width",
              by = "Species",
              error = "se",
              show.mean = TRUE,
              color.error = "blue",
              color.point = "blue")
```

---

## 📉 Linear Model Plotting

Prepare data:

```r
mtcars2 <- dplyr::mutate(
  mtcars,
  cyl = factor(cyl),
  am = factor(am, labels = c("Automatic", "Manual")),
  gear = factor(gear)
)
```

### 6. Multiple Linear Regression Visualization

```r
plot_lm(
  data = mtcars2,
  outcome = "mpg",
  predictors = c("cyl", "hp", "wt", "am", "gear"),
  point_shape = 18
)
```

---

## 🧪 Future Enhancements

* Regression diagnostics plots
* Odds ratio forest plots
* Automated normality report
* Journal-ready color palettes

---

---

## 👤 Author

**Umar Hussain**
Orthodontist & R Programmer
Email: **[drumarhussain@gmail.com](mailto:drumarhussain@gmail.com)**

---

 Create beautiful, publication-ready R plots with **pubrplot**!





```
