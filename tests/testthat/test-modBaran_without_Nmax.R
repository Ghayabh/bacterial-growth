test_that("Testing the model modBaran_without_Nmax", {
  expect_equal(modBaran_without_Nmax(temp=1, 0.035, 4, 3),3.0132444639956)
  expect_equal(modBaran_without_Nmax(temp=90, 0.035, 4, 3),4.31001408622889)
  expect_equal(length(modBaran_without_Nmax(seq(0,48,0.5), 0.035, 4, 3)), 97)
})
