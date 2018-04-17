# tvarPackage
To download this R package on your R session, copy-paste the next code line :

```
install_github("gabrielcrepeault/tvarPackage")
```

# Introduction
This R package was created in the first place to calculate more quickly and easily a lot of formulas (mean, variance, VaR, TVaR, stop-loss function, etc.) related to popular probability distributions, such as :  

- Uniform 
- Binomial
- Poisson
- Exponential
- Gamma
- Pareto
- Beta
- Lognormal
- Pareto
- Normal

And also some Mortality laws, like

- Gompertz



# Formulas convention
I've tried to make the package pretty simple to use, the syntax is based on quite the same as some popular R package, like the [`stats` ](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/00Index.html) or [`actuar`](https://cran.r-project.org/web/packages/actuar/index.html) packages :

|Formulas  | `code`|
|:-----------:|-----------------|
|Mean         |`E_<distribution>` |
|Variance     |`V_<distribution>` |
|stop loss     |`SL_<distribution>` |
|Limited expected value     |`Elim_<distribution>` |
|truncated mean     |`Etronq_<distribution>` |
|Value-at-Risk     |`VaR_<distribution>` |
|Tail Value-at-Risk     |`TVaR_<distribution>` |
|Mean Excess-loss    |`Mexcess_<distribution>` |

## Specifications for Life insurance
For probability laws used in life insurance, there is a special annotation (not much different from the one above) : 

* first, you need to specify if you want to have the probability related to $X$ or $T_x$ (`Tx = T` si setup by default)
* The Mean, Variance, VaR and TVaR expressions are given as a result of the `integrate` function or with floor equivalent, because there are no *closed* (explicit) expressions. You can call them with a slightly different syntax, `Etx_<distribution>`, `Vtx_<distribution>` and `TVaRtx_<distribution>`.
* In life insurance , we work with survival functions most of the time. So I have also defined a survival function (it's gonna save coding time and space). As the section above, the syntax is based on what's already in the most popular packages.
* There is less practical application for VaR and TVaR for life duration, so I didn't create VaR functions, since the quantilee fonction makes more sense.

|Formulas  | `code`|
|:-----------:|-----------------|
|Mortality force     |`h<distribution>` |
|density function     |`d<distribution>` |
|cumulative density function (cdf)    |`p<distribution>` |
|survival function (cdf)    |`s<distribution>` |




# Updates
|Date   | Modifications|
|:-----------:|:---------:|
|24/02/2018 | Intial commit of the package|
|27/02/2018 | adding binomial, uniform and exponential laws|
|04/03/2018 | adding beta distribution|
|06/03/2018 | adding Lognormal distribution|
|06/04/2018 | adding Pareto, Burr and Normal (E[X] & Var(X)) distribution|
|16/04/2018 | adding Gompertz probability law & README update.|


