test_that("Testing the model", {
  expect_equal(modrosso(temp=0,mumax=0.37,lag=4,log10N0=1,log10Nmax =5), 1)
  expect_equal(modrosso(temp=15,mumax=0.37,lag=4,log10N0=2,log10Nmax= 5), 3.74327477251137)
  expect_equal(modrosso(temp=1000,mumax=0.37,lag=4,log10N0=2,log10Nmax = 5),5)
})
