test_that("checking the differential form of the baranyi_without_Nmax model", {
  expect_equal(length(ode(y = c(Q=1/((1/exp(-9.21))-1),y=15), times = seq(0,70, 1), func = baranyi_diff_without_Nmax, parms = c(mumax=1))), 213)
})
