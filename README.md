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
E_beta(shape1 = 2, shape2 = 4) # E[X]
#> [1] 0.3333333
V_beta(shape1 = 2, shape2 = 4) # V(X)
#> [1] 0.03174603
kthmoment_beta(k = 3, shape1 = 2, shape2 = 4) # E[X^k]
#> [1] 0.07142857

##  Expected value of functions:
Elim_beta(d = 0.3, shape1 = 2, shape2 = 4) # E[min(X; d)]
#> [1] 2.19811
Etronq_beta(d = .3, shape1 = 2, shape2 = 4, less.than.d = TRUE) # E[X * 1_{X <= d}]
#> [1] 0.08523
Etronq_beta(d = .3, shape1 = 2, shape2 = 4, less.than.d = FALSE) # E[X * 1_{X > d}]
#> [1] 0.2481033
Mexcess_beta(d = .3, shape1 = 2, shape2 = 4) # E[(X - d | X > d)]
#> [1] 0.169697
SL_beta(d = .3, shape1 = 2, shape2 = 4) # E[max(X - d, 0)]
#> [1] 0.4065693

##  Risk measures:
TVaR_beta(kap = 0.99, shape1 = 2, shape2 = 4) # TVaR_{k}(X)
#> [1] 0.8239414
VaR_beta(kap = 0.99, shape1 = 2, shape2 = 4) # VaR_{k}(X) = F_X^(-1)(k)
#> [1] 0.7779277
```

## Syntax:

| Function                        | Syntax                  |
| ------------------------------- | ----------------------- |
| Mean                            | E\_distribution         |
| K-th moment                     | kthmoment\_distribution |
| Truncated mean                  | Etronq\_distribution    |
| Limited expected value          | Elim\_distribution      |
| Variance                        | V\_distribution         |
| Stop-loss                       | SL\_distribution        |
| Excess of mean                  | Mexcess\_distribution   |
| Moment Generating Function      | MGF\_distribution       |
| Probability Generating Function | PGF\_distribution       |
| Density                         | ddistribution           |
| Cumulative density function     | pdistribution           |
| Value-at-Risk (percentile)      | TVaR\_distribution      |
| Tail Value-at-Risk              | VaR\_distribution       |

# Included distributions and functions

## Continuous distributions

|                                             | Erlang | Inverse Gaussian | Weibull | Burr |
| :-----------------------------------------: | :----: | :--------------: | :-----: | :--: |
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

Table continues
below

|                                             | Log-logistic | Beta | Gamma | Pareto |
| :-----------------------------------------: | :----------: | :--: | :---: | :----: |
|                  **Mean**                   |      X       |  X   |   X   |   X    |
|               **kth moment**                |      X       |  X   |   X   |   X    |
|                **Variance**                 |      X       |  X   |   X   |   X    |
|             **Truncated mean**              |      X       |  X   |   X   |   X    |
|              **Limited mean**               |      X       |  X   |   X   |   X    |
|                **Stop-loss**                |      X       |  X   |   X   |   X    |
|             **Excess of mean**              |      X       |  X   |   X   |   X    |
|       **Moment Generating Function**        |              |  X   |   X   |        |
|      **Probability Density Function**       |              |      |       |        |
| **Cumulative Probability Density Function** |              |      |       |        |
|              **Value-at-Risk**              |      X       |  X   |   X   |   X    |
|           **Tail Value-at-Risk**            |      X       |  X   |   X   |   X    |

Table continues
below

|                                             | Lognormal | Exponential | Uniform | Normal |
| :-----------------------------------------: | :-------: | :---------: | :-----: | :----: |
|                  **Mean**                   |     X     |      X      |    X    |   X    |
|               **kth moment**                |     X     |      X      |    X    |        |
|                **Variance**                 |     X     |      X      |    X    |   X    |
|             **Truncated mean**              |     X     |      X      |    X    |   X    |
|              **Limited mean**               |     X     |      X      |    X    |   X    |
|                **Stop-loss**                |     X     |      X      |    X    |   X    |
|             **Excess of mean**              |     X     |      X      |    X    |   X    |
|       **Moment Generating Function**        |           |      X      |         |   X    |
|      **Probability Density Function**       |           |             |         |        |
| **Cumulative Probability Density Function** |           |             |         |        |
|              **Value-at-Risk**              |     X     |      X      |    X    |   X    |
|           **Tail Value-at-Risk**            |     X     |      X      |    X    |   X    |

## Discrete distributions

|                                             | Binomial | Negative Binomial | Poisson |
| :-----------------------------------------: | :------: | :---------------: | :-----: |
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

Table continues
below

|                                             | Uniform | Logarithmic | Hypergeometric |
| :-----------------------------------------: | :-----: | :---------: | :------------: |
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
|              **Value-at-Risk**              |         |             |                |
|           **Tail Value-at-Risk**            |         |             |                |

# Updates

|    Date    |                            Modifications                            |
| :--------: | :-----------------------------------------------------------------: |
| 26/07/2019 |                     Initial creation of package                     |
| 12/09/2019 |       Completion of creation of all necessary function files        |
| 17/11/2019 |     Merger of tvarPackage, beginning of documentation creation.     |
| 20/05/2020 |                    Addition of Shiny component.                     |
| 02/06/2020 | Completion of documentation, first attempt of a submission to CRAN. |
| 02/07/2020 |       Modifications according to CRAN’s notes, version 0.2.0.       |
