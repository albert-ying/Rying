# ggsave2 and geom_pvalue

This R package provides two useful functions for working with ggplot2 plots: `ggsave2` and `geom_pvalue`. Additionally, it includes a custom point
style called `geom_point_border`.

## Installation

To install this package, you can use the following command in your R console:

```r
# Replace 'path/to/package' with the actual path to the package folder
install.packages("path/to/package", repos = NULL, type = "source")
```

## Usage

### ggsave2

The `ggsave2` function allows you to save a ggplot2 plot to multiple filetypes at once.

Example:

```r
library(ggplot2)

p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point(aes(color = factor(cyl))) +
  theme_minimal()

ggsave2("my_plot", plot = p, filetypes = c("pdf", "png"))
```

### geom_pvalue

The `geom_pvalue` function adds p-values to a ggplot2 plot using the results from a statistical test generated by the rstatix package.

Example:

```r
library(rstatix)
library(ggpubr)
library(ggplot2)

t_res <- ToothGrowth %>%
  t_test(len ~ dose)

ggplot(ToothGrowth, aes(as.character(dose), len)) +
  geom_point() +
  geom_boxplot() +
  geom_pvalue(t_res)
```

### geom_point_border

The `geom_point_border` function creates a custom point style with a border effect for ggplot2 plots.

Example:

```r
library(rstatix)
library(ggpubr)
library(ggplot2)

t_res <- ToothGrowth %>%
  t_test(len ~ dose)

ggplot(ToothGrowth, aes(as.character(dose), len)) +
  geom_point_border(aes(color = len), size = 10, stroke_size = 1, alpha = 0.3)
```

