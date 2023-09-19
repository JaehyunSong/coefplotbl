# {coefplotbl} 0.0.1

Coefficient plot with table.

## Installation

```r
devtools::install_github("JaehyunSong/coefplotbl")
# or
remotes::install_github("JaehyunSong/coefplotbl")
# or
pacman::p_install_gh("JaehyunSong/coefplotbl")
```

## Usages

```r
library(coefplotbl)
# or
pacman::p_load_gh("JaehyunSong/coefplotbl")

fit <- lm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width + Species, data = iris)
coefplotbl(fit)

# Example 2: Display only plot & 90\% CI
coefplotbl(fit, add_tbl = FALSE, alpha = 0.1)

# Example 3: Recoding covariate names
coefplotbl(fit, coef_rename = c("Sepal.Width"  = "Sepal Width",
                                "Petal.Length" = "Petal Length",
                                "Petal.Width"  = "Petal Width"))

# Example 4: Highlight specific covariates
coefplotbl(fit, coef_rename = c("Sepal.Width"  = "Sepal Width",
                                "Petal.Length" = "Petal Length",
                                "Petal.Width"  = "Petal Width"),
           highlight = c("Petal.Length", "Petal.Width"))

# Example 5: Omit specific covariates
coefplotbl(fit, coef_omit = c("Sepal.Width"))

# Example 6: Display p-value instead of confidential interval (4-digit)
coefplotbl(fit, statistics = "p", digits = 4)
```
