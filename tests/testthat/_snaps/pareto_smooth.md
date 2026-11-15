# pareto_khat diagnostics messages are as expected

    Code
      pareto_k_diagmsg(diags)
    Message
      Pareto k-hat = 0.5.
    Code
      diags$khat <- 0.6
      pareto_k_diagmsg(diags)
    Message
      Pareto k-hat = 0.6. Sample size is too small, for given Pareto k-hat. Sample size larger than 10 is needed for reliable results.
      
    Code
      diags$khat <- 0.71
      diags$khat_threshold <- 0.8
      pareto_k_diagmsg(diags)
    Message
      Pareto k-hat = 0.71. Bias dominates when k-hat > 0.7, making empirical mean estimate of the Pareto-smoothed draws unreliable.
      
    Code
      diags$khat_threshold <- 0.55
      pareto_k_diagmsg(diags)
    Message
      Pareto k-hat = 0.71. Sample size is too small, for given Pareto k-hat. Sample size larger than 10 is needed for reliable results.
       Bias dominates when k-hat > 0.7, making empirical mean estimate of the Pareto-smoothed draws unreliable.
      
    Code
      diags$khat <- 1.1
      pareto_k_diagmsg(diags)
    Message
      Pareto k-hat = 1.1. Mean does not exist, making empirical mean estimate of the draws not applicable.
    Code
      diags$khat <- 0.8
      pareto_k_diagmsg(diags, are_weights = TRUE)
    Message
      Pareto k-hat = 0.8. Pareto khat for weights is high (0.8). This indicates a single or few weights dominate.
      Inference based on weighted draws will be unreliable.
      
    Code
      diags$khat <- 0.5
      pareto_k_diagmsg(diags, are_weights = TRUE)
    Message
      Pareto k-hat = 0.5.

