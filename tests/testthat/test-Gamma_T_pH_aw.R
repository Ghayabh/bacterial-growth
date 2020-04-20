test_that("Cheking the model Gamma_pH_4p", {
  expect_equal(Gamma_T_pH_aw(10,4,45,37,5,4,9,7,0.92,0.90,0.98,0.97,0.5),0.00232133)
  expect_equal(Gamma_T_pH_aw(37,4,45,37,7,5,9,7,0.97,0.90,0.98,0.97,0.5),0.5)
  expect_equal(Gamma_T_pH_aw(3,4,45,37,7,5,9,7,0.97,0.90,0.98,0.97,0.5),0)
  })
