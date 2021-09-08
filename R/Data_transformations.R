# Sys.setenv(RGL_USE_NULL="TRUE", R_INTERACTIVE_DEVICE="TRUE", DISPLAY="paxbox1.paxco.com:0.0")
Sys.setenv("DISPLAY"=":0.0")

#' @import dplyr
#' @import remotes
#' @import colorspace
#' @importFrom geometry convhulln
#' @importFrom pracma distmat
#' @importFrom ptinpoly pip3d
NULL

# RGL_USE_NULL=TRUE

RGB_space <- data.frame("R"= c(seq(0, 255, by=32),255, # K -> R
                               seq(0, 255, by=32),255, # G -> Y
                               rep(255, 9), # Y -> W
                               rep(255, 9), # R -> Y
                               rep(255, 9), # R -> M
                               seq(0, 255, by=32),255, # B -> M
                               rep(0, 9), # B -> C
                               seq(0, 255, by=32),255, # C -> W
                               rep(0, 9), # G -> C
                               rep(0, 9), # K -> G
                               rep(0, 9), # K -> B
                               rep(255, 9)), # M -> W

                        "G"= c(rep(0, 9), # K -> R
                               rep(255, 9), # G -> Y
                               rep(255, 9), # Y -> W
                               seq(0, 255, by=32),255, # R -> Y
                               rep(0, 9),  # R -> M
                               rep(0, 9),  # B -> M
                               seq(0, 255, by=32),255, # B -> C
                               rep(255, 9), # C -> W
                               rep(255, 9), # G -> C
                               seq(0, 255, by=32),255, # K -> G
                               rep(0, 9), # K -> B
                               seq(0, 255, by=32),255), # M -> W

                        "B"= c(rep(0, 9), # K -> R
                               rep(0, 9), # G -> Y
                               seq(0, 255, by=32),255, # Y -> W
                               rep(0, 9), # R -> Y
                               seq(0, 255, by=32),255, # R -> M
                               rep(255, 9),  # B -> M
                               rep(255, 9), # B -> C
                               rep(255, 9), # C -> W
                               seq(0, 255, by=32),255, # G -> C
                               rep(0, 9), # K -> G
                               seq(0, 255, by=32),255, # K -> B
                               rep(255, 9))# M -> W

)

ColorSpacePolygon <- function(ColorSpace){
  RGB2Lab <- function(mat) {
    # cast mat in case of a single numeric vector
    mat <- matrix(mat, ncol=3)
    # input should be a matrix with R,G and B as columns in [0,1], and columnwise pixels as rows.
    # RGB -> XYZ
    thres1 <- 0.04045
    M <- c(0.412453, 0.357580, 0.180423,
           0.212671, 0.715160, 0.072169,
           0.019334, 0.119193, 0.950227)
    M <- matrix(M, nrow=3, byrow=TRUE)
    matthres <- mat > thres1
    mat <- matthres * ((mat + 0.055) / 1.055) ^ 2.2 + (!matthres) * mat / 12.92
    xyz <- mat %*% t(M)

    # XYZ -> Lab
    thres2 <- 0.008856
    xyz <- sweep(xyz, 2, c(0.950456,1,1.088754), "/")
    #yalone <- xyz[,2]
    #y3 <- yalone^(1/3)
    xyzthres <- xyz > thres2
    xyz <- xyzthres * xyz^(1/3) + (!xyzthres) * (7.787*xyz+16/116)
    #L <- xyzthres[,2] * (116*y3-16) + (!xyzthres[,2]) * (903.3*yalone)
    L <- 116 * xyz[,2] - 16
    a <- 500 * (xyz[,1] - xyz[,2])
    b <- 200 * (xyz[,2] - xyz[,3])
    return(cbind(L,a,b))
  }
  RGBtoLabCoords <- as.data.frame(RGB2Lab(as.matrix(ColorSpace)/255)) # RGB space
  ch <- grDevices::chull(as.matrix(RGBtoLabCoords))
  polygon <- as.matrix(RGBtoLabCoords)[c(ch, ch[1]), ] # Convex that cloud should fit in
  return(polygon)
} # Switch colorspaces -- e.g. Fit sRGB (box) into the CIELab color space

DataConvex <- function(Query){
  ch_cloud <- grDevices::chull(as.matrix(Query))
  ConvexCloud <- as.matrix(Query)[c(ch_cloud, ch_cloud[1]), ] # Convex of cloud
  return(ConvexCloud)
} # Create a Convex Hull from the UMAP

Rotation <- function(ConvexCloud, RotL, Rota, Rotb){
  ConvexCloud <-  suppressWarnings(rgl::rotate3d(obj = ConvexCloud, angle = RotL, x = 1, y = 0, z = 0))
  ConvexCloud <-  suppressWarnings(rgl::rotate3d(obj = ConvexCloud, angle = Rota, x = 0, y = 1, z = 0))
  ConvexCloud <-  suppressWarnings(rgl::rotate3d(obj = ConvexCloud, angle = Rotb, x = 0, y = 0, z = 1))
  return(ConvexCloud)
}

Translation <- function(ConvexCloud, TrL, Tra, Trb){
  ConvexCloud <-  suppressWarnings(rgl::translate3d(ConvexCloud, TrL, Tra, Trb))
  return(ConvexCloud)
}

Scaling <- function(ConvexCloud, S){
  ConvexCloud <-  suppressWarnings(rgl::scale3d(ConvexCloud, S, S, S))
  return(ConvexCloud)
}

TransformedConvexCloud <- function(S, RotL, Rota, Rotb,  TrL, Tra, Trb, WL, Wa, Wb, Query){
  ConvexCloud <- Rotation(Query, RotL, Rota, Rotb)
  ConvexCloud <- Scaling(ConvexCloud, S)
  ConvexCloud <- Translation(ConvexCloud, TrL, Tra, Trb)

  L <- (max(ConvexCloud[,1]) - min(ConvexCloud[,1]))
  a <- (max(ConvexCloud[,2]) - min(ConvexCloud[,2]))
  b <- (max(ConvexCloud[,3]) - min(ConvexCloud[,3]))

  if(b > L & b > a){
    ConvexCloud <- ConvexCloud
  } else {
    # print(S)
    ConvexCloud[,1] <- WL*ConvexCloud[,1]
    ConvexCloud[,2] <- Wa*ConvexCloud[,2]
    ConvexCloud[,3] <- Wb*ConvexCloud[,3]
  }
  return(ConvexCloud)
} # Transform the UMAP Convex to otpimize it

Distance <- function(S, RotL, Rota, Rotb,  TrL, Tra, Trb, WL, Wa, Wb, Query, polygon, faces){
  ConvexCloud <- TransformedConvexCloud(S, RotL, Rota, Rotb,  TrL, Tra, Trb, WL, Wa, Wb, Query)
  point_in_space <- ptinpoly::pip3d(polygon, faces, ConvexCloud)
  outside <- ConvexCloud[which(point_in_space==-1), ]
  # print(outside)
  if(length(outside) == 0){
    dist <- 0
  } else {
    dist_mat <- pracma::distmat(polygon, outside)
    dist <- as.data.frame(apply(dist_mat,2,min))
  }
  return(dist)
} # Gives me the distance of the points from the Polygon

ObjectiveFunction <- function(param, WL, Wa, Wb, data, polygon, faces){
  X <- Distance(param[1],param[2],param[3],param[4],param[5],param[6],param[7], WL, Wa, Wb, data, polygon, faces) # S, RotL, Rota, Rotb,  TrL, Tra, Trb
  a <- 1
  f <- (a*param[1]) - sum(X^2)
  return(f)
}

FitColorsFunction <- function(dataset, WL, Wa, Wb){
  polygon = ColorSpacePolygon(RGB_space)
  dat <- DataConvex(dataset)
  faces <- geometry::convhulln(polygon, return.non.triangulated.facets = T)

  #------ Initial Guess ---------------------------------------------------------#
  #--- Translation ---#
  centroidval_color_space <- colMeans(polygon) # centroid of color space
  centroidval_cloud <- colMeans(DataConvex(dataset)) # centroid of cloud

  TrL <- (centroidval_color_space - centroidval_cloud)[1]
  Tra <- (centroidval_color_space - centroidval_cloud)[2]
  Trb <- (centroidval_color_space - centroidval_cloud)[3]

  #--- Scaling factor ---#
  ConvexCloud <- (DataConvex(dataset))

  Cloud1Size <- max(ConvexCloud[, 1]) - min(ConvexCloud[, 1])
  Cloud2Size <- max(ConvexCloud[, 2]) - min(ConvexCloud[, 2])
  Cloud3Size <- max(ConvexCloud[, 3]) - min(ConvexCloud[, 3])

  # Find the smallest and use it as L

  Cloud_scaled <- matrix(nrow = nrow(ConvexCloud), ncol = ncol(ConvexCloud))
  if (Cloud1Size < Cloud2Size & Cloud1Size < Cloud3Size) {
    MaxScalingFactor_1 <- max(polygon[,1]) / Cloud1Size
    MaxScalingFactor_2 <- max(polygon[,2]) / Cloud2Size
    MaxScalingFactor_3 <- max(polygon[,3]) / Cloud3Size

    MaxScalingFactor <- ifelse(MaxScalingFactor_1 < MaxScalingFactor_2,MaxScalingFactor_1, MaxScalingFactor_2)
    MaxScalingFactor <- ifelse(MaxScalingFactor < MaxScalingFactor_3, MaxScalingFactor, MaxScalingFactor_3)

    #offset
    Cloud1offset <- (max(ConvexCloud[, 1]) + min(ConvexCloud[, 1])) / 2
    Cloud2offset <- (max(ConvexCloud[, 2]) + min(ConvexCloud[, 2])) / 2
    Cloud3offset <- (max(ConvexCloud[, 3]) + min(ConvexCloud[, 3])) / 2

    Cloud_scaled[, 1] <- (ConvexCloud[, 1] - Cloud1offset) * MaxScalingFactor + 50
    Cloud_scaled[, 2] <- (ConvexCloud[, 2] - Cloud2offset) * MaxScalingFactor
    Cloud_scaled[, 3] <- (ConvexCloud[, 3] - Cloud3offset) * MaxScalingFactor
  }
  if (Cloud2Size < Cloud1Size & Cloud2Size < Cloud3Size) {
    MaxScalingFactor_1 <- max(polygon[,2]) / Cloud1Size
    MaxScalingFactor_2 <- max(polygon[,1]) / Cloud2Size
    MaxScalingFactor_3 <- max(polygon[,3]) / Cloud3Size

    MaxScalingFactor <- ifelse(MaxScalingFactor_1 < MaxScalingFactor_2, MaxScalingFactor_1, MaxScalingFactor_2)
    MaxScalingFactor <- ifelse(MaxScalingFactor < MaxScalingFactor_3, MaxScalingFactor, MaxScalingFactor_3)

    #offset
    Cloud1offset <- (max(ConvexCloud[, 1]) + min(ConvexCloud[, 1])) / 2
    Cloud2offset <- (max(ConvexCloud[, 2]) + min(ConvexCloud[, 2])) / 2
    Cloud3offset <- (max(ConvexCloud[, 3]) + min(ConvexCloud[, 3])) / 2

    Cloud_scaled[, 1] <- (ConvexCloud[, 2] - Cloud2offset) * MaxScalingFactor + 50
    Cloud_scaled[, 2] <- (ConvexCloud[, 1] - Cloud1offset) * MaxScalingFactor
    Cloud_scaled[, 3] <- (ConvexCloud[, 3] - Cloud3offset) * MaxScalingFactor
  }
  if (Cloud3Size < Cloud1Size & Cloud3Size < Cloud2Size) {
    MaxScalingFactor_1 <- max(polygon[,3]) / Cloud1Size
    MaxScalingFactor_2 <- max(polygon[,2]) / Cloud2Size
    MaxScalingFactor_3 <- max(polygon[,1]) / Cloud3Size

    MaxScalingFactor <- ifelse( MaxScalingFactor_1 < MaxScalingFactor_2, MaxScalingFactor_1, MaxScalingFactor_2)
    MaxScalingFactor <- ifelse(MaxScalingFactor < MaxScalingFactor_3, MaxScalingFactor, MaxScalingFactor_3)

    #offset
    Cloud1offset <- (max(ConvexCloud[, 1]) + min(ConvexCloud[, 1])) / 2
    Cloud2offset <- (max(ConvexCloud[, 2]) + min(ConvexCloud[, 2])) / 2
    Cloud3offset <- (max(ConvexCloud[, 3]) + min(ConvexCloud[, 3])) / 2

    Cloud_scaled[, 1] <- (ConvexCloud[, 3] - Cloud3offset) * MaxScalingFactor  + 50
    Cloud_scaled[, 2] <- (ConvexCloud[, 1] - Cloud1offset) * MaxScalingFactor
    Cloud_scaled[, 3] <- (ConvexCloud[, 2] - Cloud2offset) * MaxScalingFactor
  }

  S <- MaxScalingFactor

  # Simplex optimizer
  set.seed(123)
  simplex_vectors <- c()
  angle <- 1
  start.values <- c(S , pi/4 ,pi/4, pi/4, TrL, Tra , Trb) # S, RotL, Rota, Rotb,  TrL, Tra, Trb
  k <- c()

  for(i in 1:25){
    Simplex_optim <- stats::optim(par = start.values,
                           method = "Nelder-Mead",
                           ObjectiveFunction,
                           WL = WL,
                           Wa = Wa,
                           Wb = Wb,
                           data = dat,
                           polygon = polygon,
                           faces = faces,
                           control=list(fnscale=-1, maxit=1000)) #, trace=T
    k[[i]] <- Simplex_optim
    angle <- angle + 0.5
    start.values <- c(start.values[1], start.values[2]+(pi/angle), start.values[3], start.values[4], start.values[5], start.values[6], start.values[7]) # mirror rotation in x
    simplex_vectors <- rbind(simplex_vectors, k[[i]]$par)
  }

  result <- simplex_vectors[which(simplex_vectors[,1] == max(simplex_vectors[,1]))[1], ]

  return(result)
}


# S, RotL, Rota, Rotb,  TrL, Tra, Trb

#' @export
data2cielab <- function(dataset, WL = 1, Wa = 1, Wb = 1, S = 1, LAB_coordinates = F){

  # install.packages("rgl", repos = "https://dmurdoch.github.io/drat",
  #                  type = "binary")

  if(class(dataset)[1]!="data.frame"){
    warning("The dataset has been transformed into a data frame.")
    if(is.na(as.numeric(dataset[,1]))){
      val <- dataset[,1]
      dataset <- dataset[,2:ncol(dataset)]
      dataset <- matrix(as.numeric(unlist(dataset)),nrow=nrow(dataset))
      rownames(dataset) <- val
    } else{
      dataset <- matrix(as.numeric(unlist(dataset)),nrow=nrow(dataset))
    }
    dataset <- as.data.frame(dataset)
  }

  if(is.character(dataset[,1])){
    rownames(dataset) <- dataset[,1]
    dataset <- dataset[,2:ncol(dataset)]
  }

  if(ncol(dataset)==2){
    warning("Data expanded to 3D!")
    dataset <- cbind(dataset, rep(1,nrow(dataset)))
  }

  if(ncol(dataset)>3){
    stop("The dataset should have 3 numeric columns!")
    dataset <- cbind(dataset, rep(1,nrow(dataset)))
  }

  if(any(is.na(dataset))){
    stop("The dataset has missing values. Check again!")
  }

  dataset <- Scaling(dataset, FitColorsFunction(dataset, WL, Wa, Wb)[1]*S)
  dataset <- Rotation(as.matrix(dataset), FitColorsFunction(dataset, WL, Wa, Wb)[2], FitColorsFunction(dataset, WL, Wa, Wb)[3], FitColorsFunction(dataset, WL, Wa, Wb)[4])
  dataset <- Translation(as.matrix(dataset), FitColorsFunction(dataset, WL, Wa, Wb)[5], FitColorsFunction(dataset, WL, Wa, Wb)[6], FitColorsFunction(dataset, WL, Wa, Wb)[7])

  Lab <- dataset
  Lab <- round(Lab, 2)
  rawdata = structure(
    list(
      Lstar = c(Lab[, 1]),
      Astar = c(Lab[, 2]),
      Bstar = c(Lab[, 3])
    ),
    .Names = c("Lstar", "Astar", "Bstar"),
    row.names = c(rownames(dataset)),
    class = "data.frame"
  )

  LABdata <- with(rawdata, colorspace::LAB(Lstar, Astar, Bstar))

  if(LAB_coordinates==F){
    colors <- as.data.frame(cbind(rownames(dataset),colorspace::hex(LABdata, fix = TRUE)))
  } else {
    colors <- as.data.frame(cbind(rownames(dataset),as.data.frame(LABdata@coords)))
    colnames(colors) <- c("names", "L", "a", "b")
  }

  return(colors)
}








