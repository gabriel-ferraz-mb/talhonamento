### Script for mapping clusters using raster maps
library(RPostgreSQL)
library(raster)
library(cluster)
library(rgdal)
library(tmap)
library(hash)
library(psych)
library(stringr)
library(factoextra)
library(paran)
library(hopkins)
library(glue)
library(sf)
library(DBI)
library(terra)
library(smoothr)
library(raster)
## Set directory

execute <- function(r.file, v.file, cod){
  
  # r.file <- "input/TO-1700707-4B994681854F4341899F4D877E127609_2022-08-10_stack.tif"
  # v.file <- "input/vector.shp"
  
  
  #doa <- car_split[3]
  
  # Load raster stack 
  s2terrain <- stack(r.file)
  
  crs = crs(s2terrain)
  aoi <- readOGR(v.file)
  aoi <-  spTransform(aoi, crs(crs))
  aoi <- aggregate(aoi, dissolve = TRUE)
  
  
  s2terrain <- mask(s2terrain, aoi)
  #plot(s2terrain)
  #s2terrain <- crop(s2terrain, r)
  names(s2terrain) <- c('B1','B2','B3','B4','B5','B6','B7','B8','B8A','B9','B11','B12', 'NDVI', 'ELEVATION', 'SLOPE', 'CLAY', 'SAND')
  # Get the values of the raster stack
  #stack_vals <- getValues(s2terrain)
  stack_vals <- getValues(s2terrain)
  
  
  data <- (na.omit(scale(stack_vals))) #scale and exclude NA's
  
  #Generate PCA dataset
  xxx.pca1<-prcomp(data, center=FALSE, scale.=FALSE, rank. = 3) # stats::
  results <- xxx.pca1$x
  
  ### Clustering
  
  clus5PCA <- cluster::clara(results, k=5, metric = "euclidean", stand = TRUE, samples = 1000, pamLike = T)
  
  # Create am index
  idx <- 1:ncell(s2terrain)
  idx1 <- idx[-unique(which(is.na(stack_vals), arr.ind=TRUE)[,1])] #The which() function in R returns the position or the index of the value which satisfies the given condition. The Which() function in R gives you the position of the value in a logical vector. The position can be of anything like rows, columns and even vector as well
  #str(idx1)
  
  # Create an empty raster using the first raster of your stack
  clust_raster <- s2terrain[[1]]
  clust_raster[] <- NA
  clust_raster_PCA_5 <- clust_raster
  
  # Transfer the clustering results to your empty raster and save it
  clust_raster_PCA_5[idx1] <- clus5PCA$clustering
  
  rData <- rast(clust_raster_PCA_5)
  
  pol <- terra::as.polygons(rData, dissolve = T)
  threshold  <- expanse(aggregate(pol, dissolve = T), unit = 'ha')*0.01
  n_pol <- smoothr::drop_crumbs(pol, units::set_units(threshold, hectare))
  n_pol <- fill_holes(n_pol, units::set_units(threshold, hectare))
  r_poly_smooth <- smooth(n_pol, method = "ksmooth", smoothness = 5)
  
  
  talhonamento <- sf::st_as_sf(r_poly_smooth)

  st_write(talhonamento, pste0(cod, '.GeoJSON'))

} 

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

file.raster <- dir('input/', pattern = "stack.tif$", full.names = TRUE, ignore.case = TRUE)
vector <- dir('input/', pattern = "vector.shp$", full.names = TRUE, ignore.case = TRUE)
car_split <- strsplit(file.raster, split = "/")[[1]]

cod <- car_split[2]

fname <- str_replace(cod, "_stack.tif", '')

execute(file.raster, vector, fname)

full_path <- str_replace(normalizePath(file.raster), cod, '')
new_path <-  str_replace(full_path, 'input', fname)


quit("no")
