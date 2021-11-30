# semoutput 1.0.2

* added `sem_tables()` as a wrapper around the other functions. Can now use this single function to print out all summary output tables

* the R Markdown template has been updated to reflect this

# semoutput 1.0.1

* `sem_factorcor()` now automatically detects latent factor labels. The argument factors = c() is no deprecated.

* `sem_factorvar()` now automatically detects latent factor labels. The argument factors = c() is no deprecated.

* `sem_rsquared()` now only shows the R-squared values for latent factors.

* The R Markdown template has been updated:

    * Parameters are no longer set in YAML
    
    * The header levels have been changed for easier use
    
    * Fewer commented code, more streamlined to reduce clutter

# semoutput 1.0.0

Updated: 5/11/2020

* Added EFA output functions

* Added parameter `estimator` to YAML

# semoutput 0.1.3

Updated: 11/29/2018

* Added an updated and improved Rmarkdown template

* Added actual p-values to all tables not just significant stars

* Renamed `sem_anova()` to `sem_modelcomp()`

# semoutput 0.1.2

Updated: 11/29/2018

* Added `sem_anova()` function to print a model comparison table

# semoutput 0.1.1

Updated: 11/13/2018

* Deprecrated functions with "." in name. Replaced with "_"
