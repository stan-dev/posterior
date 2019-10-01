# posterior

The **posterior** R package will provide various tools for working with posterior distributions that are represented by samples.

Goals: 

* provide efficient methods for converting between different representations of posterior samples.
* provide summaries of posterior samples with names that can be adopted to standardize conventions in the bayesian R package world
* provide R implementations of the most useful MCMC diagnostics without depending on RStan

### Possible representations of posterior samples to support: 

* matrix (chains merged)
* array (with chain dimension, maybe use new [rray](https://cran.r-project.org/web/packages/rray/index.html) types?)
* list (chains merged, in the style of rstan::extract() when it returns a list)
* list of lists style 1 (separate chains, i.e., same as list but with separate sub-lists for each chain)
* maybe a list of lists style 2 (nested lists corresponding to grouping structure of params, e.g. x$group_var$slope$level)
* tidy data frames (or maybe just tibbles?, coordinate with tidybayes)
* what else?
