test_that("testing the model Proba_pH", {
  expect_equal(Proba_pH(4,4.34,5.93), 0)
  expect_equal(Proba_pH(6,4.34,5.93),1)
})
