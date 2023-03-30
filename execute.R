#! /usr/bin/Rscript

## 1. Pacotes a serem utilizados ----

x <- c('RPostgreSQL','raster','cluster','rgdal','tmap','hash','gstat',
       'psych','stringr','factoextra', 'paran','hopkins','glue',
       'sf','DBI', "terra",'rmapshaper','smoothr')


# Funcao para carregar todos os pacotes, ou instalar e carregar caso nao nao tenha algum pacote
lapply(x, function(i){
  if(i %in% rownames(installed.packages())){
    library(i,character.only = T)
  }else{
    install.packages(i)
    library(i,character.only = T)
  }
}
)

### Script for mapping clusters using raster maps
# library(RPostgreSQL)
# library(raster)
# library(cluster)
# library(rgdal)
# library(tmap)
# library(hash)
# library(psych)
# library(stringr)
# library(factoextra)
# library(paran)
# library(hopkins)
# library(glue)
# library(sf)
# library(DBI)
# library(terra)
# library(rmapshaper)
# library(smoothr)
## Set directory

execute <- function(r.file, v.file, cod){
  
  # r.file <- "input/TO-1700707-0D678788927E4C55B7706E3A718B9912_2022-08-10_stack.tif"
  # v.file <- "input/vector.shp"
  # cod <- 'TO-1700707-0D678788927E4C55B7706E3A718B9912_2022-08-10'

  #doa <- car_split[3]
  
  # Load raster stack 
  s2terrain <- stack(r.file)
  #names(s2terrain) <- c('B1','B2','B3','B4','B5','B6','B7','B8','B8A','B9','B11','B12', 'NDVI', 'ELEVATION', 'SLOPE', 'CLAY', 'SAND')
  #as.numeric(cellStats(s2terrain[[17]], stat='mean', na.rm=TRUE, asSample=TRUE))
  #plot(s2terrain)
  crs = crs(s2terrain)
  aoi <- readOGR(v.file)
  aoi <-  spTransform(aoi, crs(crs))
  aoi <- aggregate(aoi, dissolve = TRUE)
  # 
  # s2terrain <- crop(s2terrain, aoi)
  s2terrain <- mask(s2terrain, aoi)
  #plot(s2terrain)
  
  #names(s2terrain) <- c('B1','B2','B3','B4','B5','B6','B7','B8','B8A','B9','B11','B12', 'NDVI', 'ELEVATION', 'SLOPE', 'CLAY', 'SAND')
  # Get the values of the raster stack
  #stack_vals <- getValues(s2terrain)
  # options(max.print=10243)
  stack_vals <- getValues(s2terrain)
  
  data <- (na.omit(scale(stack_vals))) #scale and exclude NA's
  
  #Generate PCA dataset
  xxx.pca1<-prcomp(data, center=FALSE, scale.=FALSE, rank. = 3) # stats::
  results <- xxx.pca1$x
  
  ### Clustering
  
  clus5PCA <- cluster::clara(results, k=3, metric = "euclidean", stand = TRUE, samples = 1000, pamLike = T)
  
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
  projected_raster <- projectRaster(clust_raster_PCA_5, crs = "+proj=longlat +datum=WGS84")
  
  rData <- rast(projected_raster)
  
  pol <- terra::as.polygons(rData, dissolve = T)
  threshold  <- expanse(aggregate(pol, dissolve = T), unit = 'ha')*0.01
  n_pol <- smoothr::drop_crumbs(pol, units::set_units(threshold, hectare))
  n_pol <- fill_holes(n_pol, units::set_units(threshold, hectare))
  r_poly_smooth <- smooth(n_pol, method = "ksmooth", smoothness = 5)
  
  
  talhonamento <- sf::st_as_sf(r_poly_smooth)
  result <- ms_explode(talhonamento)
  
  for (i in 1:nrow(result)){
    result$id_talhao[i] = i
  }
  colnames(result)[1] = "class"
  #result <- talhonamento
  
  n = strsplit(cod, split = '_')[[1]][1]
  
  product <- paste0(n, '.GeoJSON')
  if (!file.exists(product)){
    st_write(result, product)
  }
} 

#this.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

file.raster <- dir('input/', pattern = "stack.tif$", full.names = TRUE, ignore.case = TRUE)
vector <- dir('input/', pattern = "vector.shp$", full.names = TRUE, ignore.case = TRUE)

car_split <- strsplit(file.raster, split = "/")[[1]]
cod <- car_split[2]

fname <- str_replace(cod, "_stack.tif", '')

execute(file.raster, vector, fname)
do.call(file.remove, list(list.files(paste0(this.dir, '/input'), full.names = TRUE)))

quit("no")
