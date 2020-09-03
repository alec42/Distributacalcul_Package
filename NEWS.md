#   Distributacalcul 0.3.0
##  Significant structural change
+   Changed syntax of all package functions (see README for summary of new syntax).
    +   As per comments of peers, the syntax was inconsistent.
    +   Changed all functions names to follow lowerCamelCase.
    +   E_d -> expValD; V_d -> varD; Elim_d -> expValLimD; Etrunc_d -> expValTruncD; Mexcess_d -> meanExcessD; SL_d -> stopLossD; VaR_d -> VatRD; TVaR_d -> TVatRD; kthmoment_d -> kthMomentD; MGF_d -> mgfD; PGF_d -> pgfD; p_d -> pD; d_d -> dD.
+   Added MGF for the Uniform distribution.
+   Combined all separate function files to files containing the entire distribution.
    +   Amongst other things, it simplifies working with the package.
+   Added template files for the parameters which didn't already have some.
    +   dispersion parameter; mean parameters for the Normal (mu), lognormal, and Inverse Gaussian (location) distributions; min/max parameters for the Uniform distributions; number of tries parameter for the Negative Binomial; prob parameter of the logarithmic distribution; rate as the normal beta, as an alternative parameter and as lambda; scale parameter as normal and as lambda; standard deviation parameter for both the normal and lognormal distributions; shape parameter as the normal alpha, the n for the Erlang, and as tau; shape1 and shape2 for the Beta.
+   Changed name of compound distributions as well as the others.
+   Changed negative binomial functions names from negbinom to nbinom.

##  Significant New Features
+   Added copulas ! 
    +   copula functions `cCopula`, density functions associated with the copulas `cdCopula`, and simulation functions `crCopula`.
+   Added copulas are : 
    1.  Independence Copula
    1.  Fréchet Lower Bound Copula
    1.  Fréchet Upper Bound Copula
    1.  Fréchet Copula
    1.  Bivariate Gumbel Copula
    1.  Bivariate Clayton Copula
    1.  Bivariate Ali-Mikhail-Haq Copula
    1.  Bivariate Cuadras-Augé Copula
    1.  Bivariate Marshall-Olkin Copula
    1.  Bivariate Frank Copula
    1.  Bivariate Eyraud-Farlie-Gumbel-Morgenstern (EFGM) Copula
+   Added visualisation function `copulaVis` for Copulas.
    +   Internal functions are distributionPlotBoxCopulas (3D plot of distribution), simulationPlotBoxCopulas (hexagon plot of simulated observations) and parametersBoxCopulas (parameters box).
    
+

## New Features
+   Added `pllogis()` and `dllogis()` functions.
    +   Added the functions to the return field of the `loglogistic-template` file.
+   Added `plogarithmic()`, `dlogarithmic()`, and `VaR_logarithmic()`functions.
    +   Added the functions to the return field of the `logarithmic-template` file.
+   Created vignette `distributionParameters`:
    +   Explains the various parameters of distributions (scale, rate, location).
    +   Still need to add shape and dispersion.
    +   Still need to fix viewing of vignette.
+   Added functionality to `MGF_negbinom()` and `PGF_negbinom()` functions:
    +   Now have both definitions of the Negative Binomial (number of trials or number of failures).
+   Replaced Shiny module `Distributacalcul_vis()` by `distributacalculVis()` :
    +   The new visualisation function can choose which modules to execute.
    +   There are 4 different modules for :
        1.  The input parameters `parametersBox()`.
        2.  The PDF and CDF `functionsBox()`.
        3.  The risk measures (VaR and TVaR) `riskMeasuresBox()`.
        4.  Various moments `momentsBox()`.
    +   Added LaTeX formulas for all functions (VaR and TVaR, moments, PDF and CDF).
    +   Added language translation with package shiny.i18n.
        +   English and French translation.
        +   Added shiny.i18n to the imports section of the `DESCRIPTION` file.
        +   Changed titles and user inputs to be bilingual.
        +   Added (bilingual) labels to the plots.
        +   Added language input at the top right of the screen.
        +   Added a json file containing the translations under man-roxygen/translations.
    +   Removed old function.
+   Updated Shiny functions with new syntax.
+   Updated vignette with new syntax.
+   Updated README with new syntax.
+   Added structure for tests.
    +   Included basic tests to learn how it works.
+   Added function to generate plot of simulation observations from a copula.

##  Bug Fixes and Changes
+   Added `pPareto()` and `dPareto()` functions. 
    +   Fixed environment error in the quantile function plot of the Shiny app.
+   Changed the name of file `lowerthan-template` to `lower.tail-template` :
    +   Typo, it contains the template for the `lower.tail` parameter.
    +   Updated binomial distribution template file reference to the new one.
+   Fixed the documentation of the compound distributions.
    +   Now one input under BINCOMP / BNCOMP / PCOMP for the family of distributions.
    +   All functions appear there instead of having their individual entries in the documentation.
+   Improved title case and style according to the Tidyverse style guide.
+   Changed filename of `riskMeasuresBoxVis` to `riskMeasuresBox`
+   Fixed mistake in discrete uniform template file which linked the PDF instead of the CDF in the return field of the description.
+   Added rmarkdown to the vignette builder section of the `DESCRIPTION` file as per [this rubric](https://cran.r-project.org/doc/manuals/R-exts.html#The-DESCRIPTION-file) of the writing R extensions guide.
+   Added `CITATION` file for custom citation.
+   Converted parameters into templates for both Shiny applications.
+   Added more translations for the second Shiny app.

##  Minor Changes
+   Changed `Etronq_` functions to `Etrunc_` functions.
    +   French spelling to English spelling.
    +   Later on, with change of all syntax, changed to expValTrunc
+   Changed Pareto PDF and CDF from `p_` and `d_` to `p` and `d`: 
    +   To be consistent with other packages.
    +   Also updated return field of the `pareto-template` file.
+   Optimized compound distributions' `p_` (CDF) and `TVaR_` codes 
    +   Removed `sapply` and used R's vector operations instead
+   Merged documentation of families under one file.
    +   The procedure is to : 
        1.  Create a new file under the distribution's name 
        2.  Copy all functions to it.
        3.  Remove all individual function files.
        4.  Remove the template file from `man-roxygen/`.
    +   This helps simplify the package and the documentation.
    +   The same procedure is used by other stats packages.
    +   Distributions merged : 
        1.  Binomial (06/16/2020).
        2.  Compound distributions (06/16/2020).
        3.  All of them (08/17/2020)
+   Replaced(ing) `@templateVar` calls for `@template` calls with files for each variable.
    +   Makes it so each distribution's template file doesn't individually define the parameters and it's easier to change the definition of a parameter for all the distributions.
    +   Removed(ing) the conditional calls for the variables from the distributions' template files.
    +   Done: beta, burr, uniform (both), exp, gamma, hyper, IG.
+   Pulled x, q, min and max, and lower.tail parameter descriptions directly from functions of the stats package with `@inheritParams` call.
+   Added notes for `VatR` functions which are just wrappers and added direct links to the underlying functions.
+   Removed special symbols in LaTeX formulas.
+   Added `distributacalcul-package.R` file which describes the package ! 
+   Added author, and did other minor changes, to the vignette.
+   Added `%>%` from dplyr to NAMESPACE file.
+   Fixed documentation of Copula functions.

#   Distributacalcul 0.2.2
## New Features
+   Added Pareto PDF and CDF.
+   Updated IG VaR from 'statmod' package.

## Bug Fixes
+   Updated URL in description file to GitHub.io site.

#   Distributacalcul 0.2.1
Unremarkable release which updates the README.

## Bug Fixes
+   Updated table of functions.
+   Fixed English grammar mistake in README example.
+   Updated instructions for downloading the package.

#   Distributacalcul 0.2.0
##  New Features
+   Added the expected value and variance of the Poisson distribution as functions for eventual addition to Shiny application.
+   Added the MGF of the Erlang distribution to ensure all distributions which have a MGF have the function defined.
+   Added NEWS.md file.

##  Bug Fixes
+   Added @return field to function documentation to describe what each functions returns.
+   Changed the filenames for d_negbinom and p_negbinom to dnegbinom and pnegbinom as per function name.
+   Changed function names for E_logarithmique and V_logarithmique to E_logarithmic and V_logarithmic as per other english function names.
+   Changed all 'F' and 'T' to 'FALSE' and 'TRUE'.
+   Fixed PGF to MGF in description of MGF_pois function.
+   Removed 'dontrun' from the TVaR_BNCOMP example.
+   Removed d_negbinom and p_negbinom (instead of not exporting the function) until I can fix the mistake in the function.
+   Removed the notes on Distributacalcul_vis module functions and replaced with 'return' field describing their use.
+   Added domain restrictions for the Poisson distribution.


#   Distributacalcul 0.1.0
+   initial release.
+   Fixed original CRAN comment on license by changing the file license to be MIT.
