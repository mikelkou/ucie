df <- data.frame(V1=runif(2,  0,1), V2=runif(2,  0,5))

test_that("data2cielab works", {
  suppressWarnings(
    expect_that(data2cielab(df), gives_warning())
  )
})

