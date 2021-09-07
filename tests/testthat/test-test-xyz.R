# source("Data_transformations.R")
# library(ucie)
options(rgl.useNULL=TRUE)

df <- data.frame(V1=runif(100,  0,1), V2=runif(100,  0,5), V3=runif(100,  0,30))

test_that("data2cielab works", {

  data_with_colors <- data2cielab(df, Wb=1.2, S=1.6)
  suppressWarnings(
  expect_that( data_with_colors, is_a("data.frame") ) )
  suppressWarnings(
  expect_that( ncol(data_with_colors), equals(2) )
  )
})

test_that("data2cielab with LAB coords works", {

  data_with_colors_with_LAB <- data2cielab(df, Wb=1.2, S=1.6, LAB_coordinates = T)
  suppressWarnings(
  expect_that( data_with_colors_with_LAB, is_a("data.frame") ) )
  suppressWarnings(
  expect_that( ncol(data_with_colors_with_LAB), equals(4) ) )
})
