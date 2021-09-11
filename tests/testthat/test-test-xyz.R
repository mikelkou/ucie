df <- data.frame(V1=runif(2,  0,1), V2=runif(2,  0,5), V3=runif(2,  0,30), V4=runif(2,  0,30))

test_that("data2cielab works", {
  suppressWarnings(
    expect_error(data2cielab(df))
  )
})

