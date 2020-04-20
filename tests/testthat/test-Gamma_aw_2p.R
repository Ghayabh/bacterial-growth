test_that("Testing the model Gamma_aw_2p", {
  expect_equal(Gamma_aw_2p(0.92,0.90,0.097), 0.00388)
  expect_equal(Gamma_aw_2p(0.90,0.90,0.097),0)
  expect_equal(Gamma_aw_2p(0.98,0.90,0.097),0.06208)
})
