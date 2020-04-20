test_that("testing the model Gamma_aw_3p", {
  expect_equal ( Gamma_aw_3p(0.98,0.90, 0.98, 0.5), 0.5)
  expect_equal(Gamma_aw_3p(0.92,0.90, 0.98, 1),0.1)
  expect_equal(Gamma_aw_3p(0.91,0.90,0.98,0.04),0.001022727)
})
