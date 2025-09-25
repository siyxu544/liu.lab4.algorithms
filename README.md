
# liu.lab4.algorithms

<!-- badges: start -->
<!-- badges: end -->

The goal of liu.lab4.algorithms is to provide an R implementation of a multiple linear regression mode. This package was created for Lab 4 in the course 732A94 Advanced R Programming at Linköping University.

## Installation

You can install the development version of liu.lab4.algorithms from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("siyxu544/liu.lab4.algorithms")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(liu.lab4.algorithms)
basic example code
linreg_model <- linreg(Petal.Length ~ Sepal.Length + Sepal.Width, data = iris)
print(linreg_model)
plot(linreg_model)
resid(linreg_model)
pred(linreg_model)
coef(linreg_model)
summary(linreg_model)
```

