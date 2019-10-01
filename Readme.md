Goals: 

* provide efficient methods for converting between different representations
of posterior samples.
* standardize output of summaries of MCMCs
* MCMC diagnostics

Possible representations to support: 

* matrix (no chain)
* array (maybe use new rray types?)
* list (like rstan::extract())
* list of lists (if we care about chains)
* tidy data frames (or maybe just tibbles?)
* draws$group$slope$level

