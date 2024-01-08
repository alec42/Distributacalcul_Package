[![](https://www.r-pkg.org/badges/version/Distributacalcul)](https://CRAN.R-project.org/package=Distributacalcul)
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Distributacalcul

<!-- badges: start -->
<!-- badges: end -->

The goal of Distributacalcul is to simplify the life of students and
scientists by offering premade functions for various functions of
probability distributions. Functions calculate moments of the
distribution (mean, variance, kth moment) as well as the expected value
of functions of the distribution (truncated mean, stop-loss, mean excess
loss, etc.). In addition, the package includes some risk measures
(Value-at-Risk and Tail Value-at-Risk).

In addition, this package has recently added probability functions for
various bivariate copulas. Functions calculate the density associated
with the copula, the distribution function and also simulations.

This package depends primarily the stats package.

## Installation

You can install the released version of Distributacalcul from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("Distributacalcul")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("alec42/Distributacalcul_Package")
```

## Example

This is a basic example which shows you the typical use of the
functions:

``` r
library(Distributacalcul)
##  Various moments:
expValBeta(shape1 = 2, shape2 = 4) # E[X]
#> [1] 0.3333333
varBeta(shape1 = 2, shape2 = 4) # V(X)
#> [1] 0.03174603
kthMomentBeta(k = 3, shape1 = 2, shape2 = 4) # E[X^k]
#> [1] 0.07142857

##  Expected value of functions:
expValLimBeta(d = 0.3, shape1 = 2, shape2 = 4) # E[min(X; d)]
#> [1] 2.19811
expValTruncBeta(d = .3, shape1 = 2, shape2 = 4, less.than.d = TRUE) # E[X * 1_{X <= d}]
#> [1] 0.08523
expValTruncBeta(d = .3, shape1 = 2, shape2 = 4, less.than.d = FALSE) # E[X * 1_{X > d}]
#> [1] 0.2481033
meanExcessBeta(d = .3, shape1 = 2, shape2 = 4) # E[(X - d | X > d)]
#> [1] 0.169697
stopLossBeta(d = .3, shape1 = 2, shape2 = 4) # E[max(X - d, 0)]
#> [1] 0.4065693

##  Risk measures:
TVatRBeta(kap = 0.99, shape1 = 2, shape2 = 4) # TVaR_{k}(X)
#> [1] 0.8239414
VatRBeta(kap = 0.99, shape1 = 2, shape2 = 4) # VaR_{k}(X) = F_X^(-1)(k)
#> [1] 0.7779277
```

## Syntax:

| Function                        | Syntax                  |
|---------------------------------|-------------------------|
| Mean                            | expValDistribution      |
| K-th moment                     | kthMomentDistribution   |
| Truncated mean                  | expValTruncDistribution |
| Limited expected value          | expValLimDistribution   |
| Variance                        | varDistribution         |
| Stop-loss                       | stopLossDistribution    |
| Excess of mean                  | meanExcessDistribution  |
| Moment Generating Function      | mgfDistribution         |
| Probability Generating Function | pgfDistribution         |
| Density                         | dDistribution           |
| Cumulative density function     | pDistribution           |
| Value-at-Risk (percentile)      | VatRDistribution        |
| Tail Value-at-Risk              | TVatRDistribution       |
| Copula Density                  | cdCopula                |
| Copula Distribution Function    | cCopula                 |
| Copula Simulation Function      | crCopula                |

# Included distributions and functions

## Continuous distributions

|                                             | Erlang | Inverse Gaussian | Weibull | Burr |
|:-------------------------------------------:|:------:|:----------------:|:-------:|:----:|
|                  **Mean**                   |   X    |        X         |    X    |  X   |
|               **kth moment**                |   X    |                  |    X    |  X   |
|                **Variance**                 |   X    |        X         |    X    |  X   |
|             **Truncated mean**              |   X    |        X         |    X    |  X   |
|              **Limited mean**               |   X    |        X         |    X    |  X   |
|                **Stop-loss**                |   X    |        X         |    X    |  X   |
|             **Excess of mean**              |   X    |        X         |    X    |  X   |
|       **Moment Generating Function**        |   X    |        X         |         |      |
|      **Probability Density Function**       |   X    |                  |         |      |
| **Cumulative Probability Density Function** |   X    |                  |         |      |
|              **Value-at-Risk**              |   X    |        X         |    X    |  X   |
|           **Tail Value-at-Risk**            |   X    |        X         |    X    |  X   |

Table continues below

|                                             | Log-logistic | Beta | Gamma | Pareto |
|:-------------------------------------------:|:------------:|:----:|:-----:|:------:|
|                  **Mean**                   |      X       |  X   |   X   |   X    |
|               **kth moment**                |      X       |  X   |   X   |   X    |
|                **Variance**                 |      X       |  X   |   X   |   X    |
|             **Truncated mean**              |      X       |  X   |   X   |   X    |
|              **Limited mean**               |      X       |  X   |   X   |   X    |
|                **Stop-loss**                |      X       |  X   |   X   |   X    |
|             **Excess of mean**              |      X       |  X   |   X   |   X    |
|       **Moment Generating Function**        |              |  X   |   X   |        |
|      **Probability Density Function**       |      X       |      |       |   X    |
| **Cumulative Probability Density Function** |      X       |      |       |   X    |
|              **Value-at-Risk**              |      X       |  X   |   X   |   X    |
|           **Tail Value-at-Risk**            |      X       |  X   |   X   |   X    |

Table continues below

|                                             | Lognormal | Exponential | Uniform | Normal |
|:-------------------------------------------:|:---------:|:-----------:|:-------:|:------:|
|                  **Mean**                   |     X     |      X      |    X    |   X    |
|               **kth moment**                |     X     |      X      |    X    |        |
|                **Variance**                 |     X     |      X      |    X    |   X    |
|             **Truncated mean**              |     X     |      X      |    X    |   X    |
|              **Limited mean**               |     X     |      X      |    X    |   X    |
|                **Stop-loss**                |     X     |      X      |    X    |   X    |
|             **Excess of mean**              |     X     |      X      |    X    |   X    |
|       **Moment Generating Function**        |           |      X      |    X    |   X    |
|      **Probability Density Function**       |           |             |         |        |
| **Cumulative Probability Density Function** |           |             |         |        |
|              **Value-at-Risk**              |     X     |      X      |    X    |   X    |
|           **Tail Value-at-Risk**            |     X     |      X      |    X    |   X    |

## Discrete distributions

|                                             | Binomial | Negative Binomial | Poisson |
|:-------------------------------------------:|:--------:|:-----------------:|:-------:|
|                  **Mean**                   |    X     |         X         |    X    |
|               **kth moment**                |          |                   |         |
|                **Variance**                 |    X     |         X         |    X    |
|             **Truncated mean**              |    X     |         X         |    X    |
|              **Limited mean**               |          |                   |         |
|                **Stop-loss**                |          |                   |         |
|             **Excess of mean**              |          |                   |         |
|       **Moment Generating Function**        |    X     |         X         |    X    |
|     **Probability Generating Function**     |    X     |         X         |    X    |
|      **Probability Density Function**       |          |                   |         |
| **Cumulative Probability Density Function** |          |                   |         |
|              **Value-at-Risk**              |    X     |                   |         |
|           **Tail Value-at-Risk**            |    X     |         X         |    X    |

Table continues below

|                                             | Uniform | Logarithmic | Hypergeometric |
|:-------------------------------------------:|:-------:|:-----------:|:--------------:|
|                  **Mean**                   |    X    |      X      |       X        |
|               **kth moment**                |         |             |                |
|                **Variance**                 |    X    |      X      |       X        |
|             **Truncated mean**              |         |             |                |
|              **Limited mean**               |         |             |                |
|                **Stop-loss**                |         |             |                |
|             **Excess of mean**              |         |             |                |
|       **Moment Generating Function**        |         |      X      |                |
|     **Probability Generating Function**     |         |      X      |                |
|      **Probability Density Function**       |    X    |             |                |
| **Cumulative Probability Density Function** |    X    |             |                |
|              **Value-at-Risk**              |         |      X      |                |
|           **Tail Value-at-Risk**            |         |             |                |

## Copulas

1.  Independence Copula
2.  Fréchet Lower Bound Copula
3.  Fréchet Upper Bound Copula
4.  Fréchet Copula
5.  Bivariate Gumbel Copula
6.  Bivariate Clayton Copula
7.  Bivariate Ali-Mikhail-Haq Copula
8.  Bivariate Cuadras-Augé Copula
9.  Bivariate Marshall-Olkin Copula
10. Bivariate Frank Copula
11. Bivariate Eyraud-Farlie-Gumbel-Morgenstern (EFGM) Copula

# Updates

|    Date    |                            Modifications                            |
|:----------:|:-------------------------------------------------------------------:|
| 26/07/2019 |                     Initial creation of package                     |
| 12/09/2019 |       Completion of creation of all necessary function files        |
| 17/11/2019 |     Merger of tvarPackage, beginning of documentation creation.     |
| 20/05/2020 |                    Addition of Shiny component.                     |
| 02/06/2020 | Completion of documentation, first attempt of a submission to CRAN. |
| 02/07/2020 |       Modifications according to CRAN’s notes, version 0.2.0.       |
| 02/13/2020 |                     Small fixes, version 0.2.2.                     |
| 31/08/2020 |          Significant changes and additions, version 0.3.0.          |
| 31/12/2023 |      Removal of shiny components and vignettes, version 0.4.0.      |
