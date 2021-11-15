.onAttach <- function(...) {
  ver <- utils::packageVersion("posterior")
  packageStartupMessage("This is posterior version ", ver)
  # packageStartupMessage("- Online documentation and vignettes at mc-stan.org/posterior")
}

.onLoad <- function(...) {
  # S3 generics for packages in Suggests, for compatibility with R < 3.6.
  vctrs::s3_register("dplyr::dplyr_reconstruct", "draws_df")
}
