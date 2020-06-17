test_that("Testing the model", {
  expect_equal(modrosso(temp=0,mumax=0.37,lag=4,N0=100,Nmax = 10^5), 2)
  expect_equal(modrosso(temp=15,mumax=0.37,lag=4,N0=100,Nmax = 10^5), 3.74327477251137)
  expect_equal(modrosso(temp=1000,mumax=0.37,lag=4,N0=100,Nmax = 10^5),5)
})
