# Introduction
This R package was created in the first place to calculate more quickly and easily a lot of formulas (mean, variance, VaR, TVaR, stop-loss function, etc.) related to popular probability distributions, such as :  

- Uniform 
- Binomial
- Exponential
- Gamma
- Pareto
- 



# Formulas available
I've tried to make the package pretty simple :

|Formulas  | `code`|
|:-----------:|:-----------------:|
|Mean         |`E_<distribution>` |
|Variance     |`V_<distribution>` |

## Mean
if you want to get the mean of a specific function, you just have to enter  `E_*<name of the distribution>`. For example, you can calculate the mean for a $Gamma(\alpha, \beta)$ distribution with `E_gamma(alph, beta)`.

# Updates
|Date   | modification|
|:-----------:|:---------:|
|24/02/2018 | Intial commit of the package|

