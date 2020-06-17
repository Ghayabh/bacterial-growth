test_that("Checking the jameson model", {
  expect_equal(length(ode(y = c(Q1=1/((1/exp(-2))-1),Q2=1/((1/exp(-10))-1),y1=1,y2=10),times = seq(from=0, to=200, by = 1), func = jameson, parms = c(mumax1 = 0.14, mumax2=0.3, ymax1=100000,ymax2=10000000))), 1005)
})
