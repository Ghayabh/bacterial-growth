test_that("Cheking the model Gamma_pH_3p", {
  expect_equal(Gamma_pH_3p(7,2,7,2),2)
  expect_equal(Gamma_pH_3p(4,4,7,0.2),0)
  expect_equal(Gamma_pH_3p(5,3,7,1),0,75)
})
