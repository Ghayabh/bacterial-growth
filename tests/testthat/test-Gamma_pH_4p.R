test_that("Cheking the model Gamma_pH_4p", {
  expect_equal(Gamma_pH_4p(5,4,9,7,0.5),0.25)
  expect_equal(Gamma_pH_4p(4,4,9,7,0.5),0)
  expect_equal(Gamma_pH_4p(7,4,9,7,0.5),0.5)
})
