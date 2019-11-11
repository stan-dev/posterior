.onAttach <- function(...) {
  ver <- utils::packageVersion("posterior")
  packageStartupMessage("This is posterior version ", ver)
  # packageStartupMessage("- Online documentation and vignettes at mc-stan.org/posterior")
}


