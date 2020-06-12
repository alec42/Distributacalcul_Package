#   Distributacalcul 0.2.3
## New features
+   Added loglogistic PDF and CDF.
+   Added LaTeX formulas for all functions (VaR, TVaR, moments, PDF and CDF) in the Shiny application.
+   Created vignette explaining the various parameters of distributions (scale, rate, location).
    +   Still need to add shape and dispersion.
    +   Still need to fix viewing of vignette.
+   Seperated module into 4 different modules for the parameters, functions, risk measures and moments.
+   Created new visualisation function which can choose which modules to execute.
+   Added both definitions of the PGF and MGF for the negative binomial (number of trials or number of failures).

##  Bug  Fixes
+   By adding the PDF and CDF, fixed an error in the quantile function plot in the Shiny application.
+   Fixed name of file for lower.tail variable from 'lowerthan-template' to 'lower.tail-template'.

##  Minor changes
+   Changed the spelling of the truncated mean functions from french spelling 'Etronq' to english spelling 'Etrunc'.
+   Changed function name of pareto distribution functions from 'p_' and 'd_' to 'p' and 'd' to be consistent with other packages (also updated return field of the 'pareto-template' file.
+   Added loglogistic density functions to the return field of the 'loglogistic-template' file.
+   Optimized code for the CDF and TVaR of the compound distributions by removing the sapplies and using R's vector operations.

#   Distributacalcul 0.2.2
## New features
+   Added Pareto PDF and CDF.
+   Updated IG VaR from 'statmod' package.

## Bug fixed
+   Updated URL in description file to github.io site.

#   Distributacalcul 0.2.1
Unremarkable release which updates the readme.

## Bug fixes
+   Updated table of functions.
+   Fixed English grammar mistake in readme example.
+   Updated instructions for downloading the package.

#   Distributacalcul 0.2.0
##  New features
+   Added the expected value and variance of the Poisson distribution as functions for eventual addition to Shiny application.
+   Added the MGF of the Erlang distribution to ensure all distributions which have a MGF have the function defined.
+   Added NEWS.md file.

##  Bug fixes
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
