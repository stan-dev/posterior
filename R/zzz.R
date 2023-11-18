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

  # S3 methods for matrixOps, which is a group generic that didn't exist
  # until R 4.3, so we can't register it in NAMESPACE
  if (getRversion() >= "4.3") {
    registerS3method("matrixOps", "rvar", matrixOps.rvar, asNamespace("base"))
  }
}
