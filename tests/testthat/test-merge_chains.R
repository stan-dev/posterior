test_that("merge_chains works correctly", {
   x <- example_draws()

   xm <- merge_chains(as_draws_matrix(x))
   expect_equal(nchains(xm), 1)
   expect_equal(ndraws(xm), ndraws(x))
   expect_equal(variables(xm), variables(x))

   xm <- merge_chains(as_draws_array(x))
   expect_equal(nchains(xm), 1)
   expect_equal(ndraws(xm), ndraws(x))
   expect_equal(variables(xm), variables(x))

   xm <- merge_chains(as_draws_df(x))
   expect_equal(nchains(xm), 1)
   expect_equal(ndraws(xm), ndraws(x))
   expect_equal(variables(xm), variables(x))

   xm <- merge_chains(as_draws_list(x))
   expect_equal(nchains(xm), 1)
   expect_equal(ndraws(xm), ndraws(x))
   expect_equal(variables(xm), variables(x))
})
