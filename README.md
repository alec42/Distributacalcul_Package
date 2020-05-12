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

``` r
cont.distr.support %>% 
    pander::pander()
```

|                                             | Erlang | Inverse Gaussian | Weibull | Burr  |
| :-----------------------------------------: | :----: | :--------------: | :-----: | :---: |
|                  **Mean**                   |  TRUE  |       TRUE       |  TRUE   | TRUE  |
|               **kth moment**                |  TRUE  |      FALSE       |  TRUE   | TRUE  |
|                **Variance**                 |  TRUE  |       TRUE       |  TRUE   | TRUE  |
|             **Truncated mean**              |  TRUE  |       TRUE       |  TRUE   | TRUE  |
|              **Limited mean**               |  TRUE  |       TRUE       |  TRUE   | TRUE  |
|                **Stop-loss**                |  TRUE  |       TRUE       |  TRUE   | TRUE  |
|             **Excess of mean**              |  TRUE  |       TRUE       |  TRUE   | TRUE  |
|       **Moment Generating Function**        | FALSE  |       TRUE       |  FALSE  | FALSE |
|      **Probability Density Function**       |  TRUE  |       TRUE       |  FALSE  | FALSE |
| **Cumulative Probability Density Function** |  TRUE  |       TRUE       |  FALSE  | FALSE |
|              **Value-at-Risk**              | FALSE  |       TRUE       |  TRUE   | TRUE  |
|           **Tail Value-at-Risk**            |  TRUE  |       TRUE       |  TRUE   | TRUE  |

Table continues
below

|                                             | Log-logistic | Beta  | Gamma | Pareto |
| :-----------------------------------------: | :----------: | :---: | :---: | :----: |
|                  **Mean**                   |     TRUE     | TRUE  | TRUE  |  TRUE  |
|               **kth moment**                |     TRUE     | TRUE  | TRUE  |  TRUE  |
|                **Variance**                 |     TRUE     | TRUE  | TRUE  |  TRUE  |
|             **Truncated mean**              |     TRUE     | TRUE  | TRUE  |  TRUE  |
|              **Limited mean**               |     TRUE     | TRUE  | TRUE  |  TRUE  |
|                **Stop-loss**                |     TRUE     | TRUE  | TRUE  |  TRUE  |
|             **Excess of mean**              |     TRUE     | TRUE  | TRUE  |  TRUE  |
|       **Moment Generating Function**        |    FALSE     | FALSE | FALSE | FALSE  |
|      **Probability Density Function**       |    FALSE     | FALSE | FALSE | FALSE  |
| **Cumulative Probability Density Function** |    FALSE     | FALSE | FALSE | FALSE  |
|              **Value-at-Risk**              |     TRUE     | TRUE  | TRUE  |  TRUE  |
|           **Tail Value-at-Risk**            |     TRUE     | TRUE  | TRUE  |  TRUE  |

Table continues
below

|                                             | Generalized F-distribution | Lognormal |
| :-----------------------------------------: | :------------------------: | :-------: |
|                  **Mean**                   |           FALSE            |   TRUE    |
|               **kth moment**                |           FALSE            |   TRUE    |
|                **Variance**                 |           FALSE            |   TRUE    |
|             **Truncated mean**              |           FALSE            |   TRUE    |
|              **Limited mean**               |           FALSE            |   TRUE    |
|                **Stop-loss**                |           FALSE            |   TRUE    |
|             **Excess of mean**              |           FALSE            |   TRUE    |
|       **Moment Generating Function**        |           FALSE            |   FALSE   |
|      **Probability Density Function**       |           FALSE            |   FALSE   |
| **Cumulative Probability Density Function** |            TRUE            |   FALSE   |
|              **Value-at-Risk**              |            TRUE            |   TRUE    |
|           **Tail Value-at-Risk**            |           FALSE            |   TRUE    |

Table continues
below

|                                             | Exponential | Uniform | Normal |
| :-----------------------------------------: | :---------: | :-----: | :----: |
|                  **Mean**                   |    TRUE     |  TRUE   | FALSE  |
|               **kth moment**                |    TRUE     |  TRUE   | FALSE  |
|                **Variance**                 |    TRUE     |  TRUE   | FALSE  |
|             **Truncated mean**              |    TRUE     |  TRUE   |  TRUE  |
|              **Limited mean**               |    TRUE     |  TRUE   |  TRUE  |
|                **Stop-loss**                |    TRUE     |  TRUE   |  TRUE  |
|             **Excess of mean**              |    TRUE     |  TRUE   |  TRUE  |
|       **Moment Generating Function**        |    FALSE    |  FALSE  | FALSE  |
|      **Probability Density Function**       |    FALSE    |  FALSE  | FALSE  |
| **Cumulative Probability Density Function** |    FALSE    |  FALSE  | FALSE  |
|              **Value-at-Risk**              |    TRUE     |  TRUE   |  TRUE  |
|           **Tail Value-at-Risk**            |    TRUE     |  TRUE   |  TRUE  |

## Discrete distributions

``` r
discr.distr.support %>% 
    pander::pander()
```

|                                             | Binomial | Negative Binomial | Poisson |
| :-----------------------------------------: | :------: | :---------------: | :-----: |
|                  **Mean**                   |   TRUE   |       FALSE       |  TRUE   |
|               **kth moment**                |  FALSE   |       FALSE       |  FALSE  |
|                **Variance**                 |   TRUE   |       FALSE       |  TRUE   |
|             **Truncated mean**              |   TRUE   |       TRUE        |  TRUE   |
|              **Limited mean**               |  FALSE   |       FALSE       |  FALSE  |
|                **Stop-loss**                |  FALSE   |       FALSE       |  FALSE  |
|             **Excess of mean**              |  FALSE   |       FALSE       |  FALSE  |
|       **Moment Generating Function**        |   TRUE   |       FALSE       |  TRUE   |
|     **Probability Generating Function**     |   TRUE   |       FALSE       |  TRUE   |
|      **Probability Density Function**       |  FALSE   |       FALSE       |  FALSE  |
| **Cumulative Probability Density Function** |  FALSE   |       FALSE       |  FALSE  |
|              **Value-at-Risk**              |   TRUE   |       FALSE       |  TRUE   |
|           **Tail Value-at-Risk**            |   TRUE   |       TRUE        |  TRUE   |

Table continues
below

|                                             | Uniform | Logarithmic | Hypergeometric |
| :-----------------------------------------: | :-----: | :---------: | :------------: |
|                  **Mean**                   |  TRUE   |    TRUE     |      TRUE      |
|               **kth moment**                |  FALSE  |    FALSE    |     FALSE      |
|                **Variance**                 |  TRUE   |    TRUE     |      TRUE      |
|             **Truncated mean**              |  FALSE  |    FALSE    |     FALSE      |
|              **Limited mean**               |  FALSE  |    FALSE    |     FALSE      |
|                **Stop-loss**                |  FALSE  |    FALSE    |     FALSE      |
|             **Excess of mean**              |  FALSE  |    FALSE    |     FALSE      |
|       **Moment Generating Function**        |  FALSE  |    FALSE    |     FALSE      |
|     **Probability Generating Function**     |  FALSE  |    FALSE    |     FALSE      |
|      **Probability Density Function**       |  TRUE   |    FALSE    |     FALSE      |
| **Cumulative Probability Density Function** |  TRUE   |    FALSE    |     FALSE      |
|              **Value-at-Risk**              |  FALSE  |    FALSE    |     FALSE      |
|           **Tail Value-at-Risk**            |  FALSE  |    FALSE    |     FALSE      |

# Updates

|    Date    |                        Modifications                        |
| :--------: | :---------------------------------------------------------: |
| 26/07/2019 |                 Initial creation of package                 |
| 12/09/2019 |   Completion of creation of all necessary function files    |
| 17/11/2019 | Merger of tvarPackage, beginning of documentation creation. |
