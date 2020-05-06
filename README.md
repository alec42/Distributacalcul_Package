To instal package from RStudio: `devtools::install_github("alec42/Distributacalcul_Package")`

# Distributacalcul_Package

Package containing functions used in the R Shiny application [distributacalcul](https://alec42.shinyapps.io/distributacalcul/).

Functions included in the package are marked by an x, those included in the [TVaR package](https://github.com/gabrielcrepeault/tvarPackage) are marked by a y.

## Continuous Distributions

| Function                    | Erlang | Inverse Gaussian | Weibull | Burr | Log-logistic | Beta | Gamma | Pareto | Generalized F-distribution | Lognormal | Exponential | Uniform | Normal |
| --------------------------- |:------:|:----------------:|:-------:|:----:|:------------:|:----:|:-----:|:------:|:--------------------------:|:---------:|:-----------:|:-------:|:------:|
| Mean                        | x      | x                | x       | x    |              | x    | x     | x      |                            | x         | x           | x       |        |
| K-th moment                 | x      |                  | x       | x    | x            | x    | x     | x      |                            | x         |             |         |        |
| Truncated mean              | x      | x                | x       | x    | x            | x    | x     | x      |                            | x         | x           | x       | x      |
| Limited expected value      | x      | x                | x       | x    | x            | x    | x     | x      |                            | x         | x           | x       | x      |
| Variance                    | x      | x                | x       | x    | x            | x    | x     | x      |                            | x         | x           | x       |        |
| Stop-loss                   | x      | x                | x       | x    | x            | x    | x     | x      |                            | x         | x           | x       | x      |
| Excess of mean              | x      | x                | x       | x    | x            | x    | x     | x      |                            | x         | x           | x       | x      |
| Moment Generating Function  |        | x                |         |      |              |      |       |        |                            |           |             |         |        |
| Density                     | x      | x                |         |      |              |      |       |        |                            |           |             |         |        |
| Cumulative density function | x      | x                |         |      |              |      |       |        | x                          |           |             |         |        |
| Value-at-Risk (percentile)  |        | x                | x       | x    | x            | x    | x     | x      | x                          | x         | x           | x       | x      |
| Tail Value-at-Risk          | x      | x                | x       | x    | x            | x    | x     | x      |                            | x         | x           |         | x      |

## Discrete Distributions

| Function                    | Binomial | Bernoulli | Negative Binomial | Geometric | Poisson | Uniform | Logarithmic | Hypergeometric |
| --------------------------- |:--------:|:---------:|:-----------------:|:---------:|:-------:|:-------:|:-----------:|:--------------:|
| Mean                        | x        |           | x/x               |           |         | x       | x           | x              |
| K-th moment                 |          |           |                   |           |         |         |             |                |
| Truncated mean              | x        |           |                   |           |  x      |         |             |                |
| Limited expected value      |          |           |                   |           |         |         |             |                |
| Variance                    | x        |           | x/x               |           |         | x       | x           | x              |
| Stop-loss                   |          |           |                   |           |         |         |             |                |
| Excess of mean              |          |           |                   |           |         |         |             |                |
| Moment Generating Function  | x        |           |                   |           |         |         |             |                |
| Probability Generating Function | x        |           |                   |           |         |         |             |                |
| Density                     |          |           | x                 |           |         | x       |             |                |
| Cumulative density function |          |           | x                 |           |         | x       |             |                |
| Value-at-Risk (percentile)  | x        |           |                   |           |         |         |             |                |
| Tail Value-at-Risk          | x        |           |                   |           | x       |         |             |                |

## Compound Distributions:

| Function                    | Compound Negative Binomial | Compound Binomial | Compound Poisson |
| --------------------------- |:--------------------------:|:-----------------:|:----------------:|
| Mean                        | x                          | x                 | x                |
| K-th moment                 |                            |                   |                  |
| Truncated mean              |                            |                   |                  |
| Limited expected value      |                            |                   |                  |
| Variance                    | x                          | x                 | x                |
| Stop-loss                   |                            |                   |                  |
| Excess of mean              |                            |                   |                  |
| Moment Generating Function  |                            |                   |                  |
| Density                     |                            |                   |                  |
| Cumulative density function | x                          | x                 | x                |
| Value-at-Risk (percentile)  | x                          | x                 | x                |
| Tail Value-at-Risk          | x                          | x                 | x                |

# Syntax:

| Function                    | Syntax                 |
| --------------------------- | ---------------------- |
| Mean                        | E_distribution         |
| K-th moment                 | kthmoment_distribution |
| Truncated mean              | Etronq_distribution    |
| Limited expected value      | Elim_distribution      |
| Variance                    | V_distribution         |
| Stop-loss                   | SL_distribution        |
| Excess of mean              | Mexcess_distribution   |
| Moment Generating Function  | MGF_distribution       |
| Probability Generating Function  | PGF_distribution       |
| Density                     | ddistribution          |
| Cumulative density function | pdistribution          |
| Value-at-Risk (percentile)  | TVaR_distribution      |
| Tail Value-at-Risk          | VaR_distribution       |

# Updates

| Date       | Modifications                                               |
|:----------:|:-----------------------------------------------------------:|
| 26/07/2019 | Initial creation of package                                 |
| 12/09/2019 | Completion of creation of all necessary function files      |
| 17/11/2019 | Merger of tvarPackage, beginning of documentation creation. |
