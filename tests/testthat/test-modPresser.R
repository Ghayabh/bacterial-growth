test_that("Testing the Presser's model", {
               expect_equal(modPresser(6,4.5,0.5), 0.984188611699158)
               expect_equal(modPresser(3,4.5,0.5),0)
               expect_equal(modPresser(7,4,0.003),0.999997)
})
