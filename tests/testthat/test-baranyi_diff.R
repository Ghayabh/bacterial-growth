library(deSolve)
test_that("checking the differential form of the baranyi model", {
  expect_equal(length(ode(y = c(Q=(1/exp(-9.21)-1), y=10), times = seq(0,24,1), func = baranyi_diff, parms =c(mumax=1, ymax=1000000000))),75)
})

