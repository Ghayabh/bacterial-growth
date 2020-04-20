test_that("Testing the model", {
  expect_equal(modrosso(temp=0,mumax=0.37,lag=4,Nmax = 10^5,N0=100), 2)
  expect_equal(modrosso(temp=15,mumax=0.37,lag=4,Nmax = 10^5,N0=100), 3.74327477251137)
  expect_equal(modrosso(temp=10,mumax=0.37,lag=4,Nmax = 10^5,N0=100),2.96058389887442 )
})
