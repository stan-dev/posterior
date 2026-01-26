---
title: 'posterior: Tools for Working with Posterior Distributions in R'
tags:
  - R
  - Bayesian Inference
  - MCMC
  - Diagnostics
authors:
  - name: Paul-Christian Bürkner
    orcid: 0000-0001-5765-8995
    affiliation: 1
  - name: Jonah Gabry
    orcid: 0000-0001-6437-6144
    affiliation: 2
  - name: Matthew Kay
    orcid: 0000-0001-9446-0419
    affiliation: 3
  - name: Aki Vehtari
    orcid: 0000-0003-2164-9469
    affiliation: 4
affiliations:
 - name: TU Dortmund University, Germany
   index: 1
 - name: Columbia University, USA
   index: 2
 - name: Northwestern University, USA
   index: 3
 - name: Aalto University, Finland
   index: 4
date: 26 January 2026
bibliography: paper.bib

# Optional fields if submitting to a AAS journal too, see this blog post:
# https://blog.joss.theoj.org/2018/12/a-new-collaboration-with-aas-publishing
aas-doi: 10.3847/xxxxx <- update this with the DOI from AAS once you know it.
aas-journal: Astrophysical Journal <- The name of the AAS journal.
---

# Summary

Modern Bayesian Inference is often performed via sampling algorithms that produce draws (samples) from the model's posterior distribution. The most important class of such algorithms is Markov chain Monte Carlo (MCMC), but also other algorithm classes such as variational inference and neural posterior estimation rely on posterior draws as their primary output representation. Regardless of their specific origin, these draws have to be stored and post-processed to obtain insights into the obtained Bayesian inference results. In this context, relevant questions for the Bayesian modeler include in which format to store the posterior draws, which diagnostics to run to assess the trustworthiness of the obtained draws, and how to best summarize the draws for inference and decision making. Due to the widespread use of sampling algorithms in Bayesian inference, essentially all Bayesian modelers face these questions during their data analyses, thus strongly benefitting from modern, efficient, and easy-to-use solutions.


# Statement of need

The `posterior` R package is intended to provide useful tools for both users
and developers of packages for fitting Bayesian models or working with output
from Bayesian models. The primary goals of `posterior` are three-fold:

(a\) Efficiently convert between many different useful formats of draws. Existing packages for storing posterior draws, most notably `coda` [@coda], support only a single format. However, in practice, different analyses and workflow require different formats. For example, when MCMC chain information is required, say, for convergence diagnostics, storing chains in an extra dimension is beneficial. In contrast, once convergence has been established, chain information becomes irrelevant and so dropping this information simplifies any subsequent operations. To accomodate users' and developers' practical needs, `posterior` natively supports several formats, with the ability to seamlessly switch between them.

(b\) Provide consistent methods for operations commonly performed on draws, for example, subsetting, binding, mutating, or summarizing draws. These operations work consistently across all draws formats, thus providing a single interface regardless of format.

(c\) Provide lightweight implementations of state-of-the-art posterior inference diagnostics [e.g., @vehtari_rhat_2021]. Especially for draws obtained via MCMC, convergence diagnostics are essential to assess the trustworthiness of the conclusions obtained from the draws. By providing a consistent and extensible interface for all these diagnostics, we not only ensure safe and easy applications, but also easy extensibility whenever new, promising diagnostics are being proposed.

# Software design

`posterior`'s design follows the central idea of providing consistent and safe interfaces between all draws formats, operations, and diagnostics. As such, users and developers alike can rely on predictable output structures and error handling regardless of their draws format of choice. All draws formats all contain information on (a) the *variables* (parameters) from whose distributions draws have been sampled and (b) the *draw indices* to safely map draws across variables, thus preserving their dependence structure. Some formats also contain information on (c) the *chain indices*, primarily useful for MCMC, to ensure the correct behavior of convergence diagnostics [@vehtari_rhat_2021]. 

Except for one special format (see [https://mc-stan.org/posterior/articles/rvar.html](https://mc-stan.org/posterior/articles/rvar.html)), all formats are directly built on R base formats (matrices, arrays, lists, and data.frames) such that the objects are easily usable also outside of the methods that `posterior` itself provides. As such, users can easily move in and out of the `posterior` framework without facing major difficulties of converting their draws into any special format. The deliberate choice to supporting multiple formats required extra work on the internal interfaces and introduced the requirements for format-transforming methods. Nevertheless, we believe that the practical benefit of the multi-format approach justifies the internal code overhead necessary for its support. 

# Research impact statement

The `posterior` package is part of the Stan ecosystem of packages ([https://mc-stan.org/tools](https://mc-stan.org/tools/)), being tightly integrated in the post-processing methods of widely-used Stan-based packages such as `brms` [@brms1], `rstanarm` [@rstanarm], and `cmdstanr` [@cmdstanr], among others. 
These packages have together been cited well over 10,000 times (according to GoogleScholar) since they started to feature `posterior` for the handling of posterior draws. While not being cited directly in most of these applications, `posterior` is sure to have been used in almost all of them in terms of posterior summaries and convergence diagnostics, which are essential within the Bayesian workflow [@gelman_workflow_2020]. `posterior` itself has been cited directly over 100 times (according to GoogleScholar) by methods, software, and application papers alike.

Despite its close connection to Stan, its modules and methods are generic, thus being compatible with essentially all implementations of sampling algorithms available in R. It is currently imported or suggested by a growing list of currently over 60 other packages on [CRAN](https://cran.r-project.org/web/packages/posterior/index.html), many of which building on sampling backends other than Stan, for example, `BayesMultiMode` [@BayesMultiMode], `bayesQRsurvey` [@bayesQRsurvey], or `jagstargets` [@jagstargets]. From the Posit CRAN mirror alone, `posterior` have been downloaded over 3,7 million times since its first release in August 2021. In 2025, it has been downloaded over 1 million times, which corresponds to roughly 20,000 weekly downloads on average. Together, this showcases the importance of `posterior` for the area of sampling-based Bayesian inference. 

# AI usage disclosure

No generative AI tools were used in the development of this software, the writing
of this manuscript, or the preparation of supporting materials.

# Acknowledgements

We acknowledge contributions from Måns Magnusson, Rok Češnovar, Ben Lambert, Ozan Adıgüzel, Jacob Socolar, Noa Kallioinen, and Teemu Säilynoja.

# References