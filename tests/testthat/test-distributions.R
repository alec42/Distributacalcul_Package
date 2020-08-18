test_that("Domains of distribution parameters are valid", {
    expect_error(expValPois(-2), "lambda > 0 is not TRUE")
    expect_error(expValGamma(-2, 2), "shape > 0 is not TRUE")
})

test_that("Means of distributions are as expected", {
    expect_equal(expValPois(5), 5)
    expect_equal(expValNorm(5, 2), 5)
})
