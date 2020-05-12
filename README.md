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

This package depends primarily on both the stats package and the actuar
package (for the Pareto
distribution).

## Installation

<!-- You can install the released version of Distributacalcul from [CRAN](https://CRAN.R-project.org) with: -->

<!-- ``` r -->

<!-- install.packages("Distributacalcul") -->

<!-- ``` -->

<!-- And the development version from [GitHub](https://github.com/) with: -->

You can install the most recent development version of Distributacalcul
from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("alec42/Distributacalcul_Package")
```

## Example

This is a basic example which shows you how the typical use of the
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
#> [1] 0.0216053
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

| Erlang | Inverse Gaussian | Weibull | Burr | Log-logistic | Beta | Gamma |
| :----: | :--------------: | :-----: | :--: | :----------: | :--: | :---: |
|   X    |        X         |    X    |  X   |      X       |  X   |   X   |
|   X    |                  |    X    |  X   |      X       |  X   |   X   |
|   X    |        X         |    X    |  X   |      X       |  X   |   X   |
|   X    |        X         |    X    |  X   |      X       |  X   |   X   |
|   X    |        X         |    X    |  X   |      X       |  X   |   X   |
|   X    |        X         |    X    |  X   |      X       |  X   |   X   |
|   X    |        X         |    X    |  X   |      X       |  X   |   X   |
|        |        X         |         |      |              |      |       |
|   X    |        X         |         |      |              |      |       |
|   X    |        X         |         |      |              |      |       |
|        |        X         |    X    |  X   |      X       |  X   |   X   |
|   X    |        X         |    X    |  X   |      X       |  X   |   X   |

Table continues
below

| Pareto | Generalized F-distribution | Lognormal | Exponential | Uniform | Normal |
| :----: | :------------------------: | :-------: | :---------: | :-----: | :----: |
|   X    |                            |     X     |      X      |    X    |        |
|   X    |                            |     X     |      X      |    X    |        |
|   X    |                            |     X     |      X      |    X    |        |
|   X    |                            |     X     |      X      |    X    |   X    |
|   X    |                            |     X     |      X      |    X    |   X    |
|   X    |                            |     X     |      X      |    X    |   X    |
|   X    |                            |     X     |      X      |    X    |   X    |
|        |             X              |           |             |         |        |
|   X    |             X              |     X     |      X      |    X    |   X    |
|   X    |                            |     X     |      X      |    X    |   X    |

## Discrete distributions

| Binomial | Negative Binomial | Poisson | Uniform | Logarithmic | Hypergeometric |
| :------: | :---------------: | :-----: | :-----: | :---------: | :------------: |
|    X     |                   |    X    |    X    |      X      |       X        |
|    X     |                   |    X    |    X    |      X      |       X        |
|    X     |         X         |    X    |         |             |                |
|    X     |                   |    X    |         |             |                |
|    X     |                   |    X    |         |             |                |
|          |                   |         |    X    |             |                |
|          |                   |         |    X    |             |                |
|    X     |                   |    X    |         |             |                |
|    X     |         X         |    X    |         |             |                |

# Updates

|    Date    |                        Modifications                        |
| :--------: | :---------------------------------------------------------: |
| 26/07/2019 |                 Initial creation of package                 |
| 12/09/2019 |   Completion of creation of all necessary function files    |
| 17/11/2019 | Merger of tvarPackage, beginning of documentation creation. |
