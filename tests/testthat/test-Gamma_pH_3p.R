test_that("Cheking the model Gamma_pH_3p", {
  expect_equal(Gamma_pH_3p(7,4,2,7),2)
  expect_equal(Gamma_pH_3p(4,4,0.2,7),0)
  expect_equal(Gamma_pH_3p(5,3,1,7),0,75)
})
