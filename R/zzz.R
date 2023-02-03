.onAttach <- function(...) {
  ver <- utils::packageVersion("posterior")
  packageStartupMessage("This is posterior version ", ver)
  # packageStartupMessage("- Online documentation and vignettes at mc-stan.org/posterior")
}

.onLoad <- function(...) {
  # S3 generics for packages in Suggests, for compatibility with R < 3.6.
  # See help("s3_register", package = "vctrs") for more information.
  vctrs::s3_register("dplyr::dplyr_reconstruct", "draws_df")
  vctrs::s3_register("ggplot2::scale_type", "rvar")
}
