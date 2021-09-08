# source("Data_transformations.R")
# library(ucie)
# options(rgl.useNULL=TRUE)

df <- data.frame(V1=runif(2,  0,1), V2=runif(2,  0,5), V3=runif(2,  0,30))

test_that("data2cielab works", {

  data_with_colors <- data2cielab(df, Wb=1.2, S=1.6)
  suppressWarnings(
  expect_that( ncol(data_with_colors), equals(2) )
  )
})

