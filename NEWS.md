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
