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
## Set directory

execute <- function(r.file, v.file, talhao){
  r.file <- 'C:\\Projetos\\talhonamento\\input\\TO-1700707-870A02A48FF8412A9E4762F3488B6CFA\\TO-1700707-870A02A48FF8412A9E4762F3488B6CFA_2022-08-10_multiband.tif'
  car_split <- strsplit(r.file, split = "/")[[1]]
  cod <- car_split[2]
  #car <- paste0(car_split[1], '_',car_split[2]) 
  
  # Load raster stack 
  s2 <- stack(r.file)
  clay <- stack(str_replace(r.file, 'multiband', 'clay'))
  dsm <- stack(str_replace(r.file, 'multiband', 'dsm'))
  sand <- stack(str_replace(r.file, 'multiband', 'sand'))
  slope <- stack(str_replace(r.file, 'multiband', 'slope'))
  s2terrain <- stack(s2, clay, dsm, sand, slope)
  
  crs = crs(s2terrain)
  aoi <- readOGR(v.file)
  aoi <-  spTransform(aoi, crs(crs))
  aoi <- aggregate(aoi, dissolve = TRUE)
 
  t <- readOGR(talhao)
  t <-  spTransform(t, crs(crs))
  
  s2terrain <- mask(s2terrain, aoi)
  #plot(s2terrain)
  #s2terrain <- crop(s2terrain, r)
  names(s2terrain) <- c('B1','B2','B3','B4','B5','B6','B7','B8','B8A','B9','B11','B12', 'NDVI', 'ELEVATION', 'SLOPE', 'CLAY', 'SAND')
  # Get the values of the raster stack
  #stack_vals <- getValues(s2terrain)
  stack_vals <- getValues(s2)
  
  
  data <- (na.omit(scale(stack_vals))) #scale and exclude NA's
  
  #Generate PCA dataset
  xxx.pca1<-prcomp(data, center=FALSE, scale.=FALSE, rank. = 3) # stats::
  results <- xxx.pca1$x
  
  ### Clustering
  # Use the clara function from cluster package. In this code, a sample of 1000 and the euclidean distance are used.
  # I used k=10, as example
  # clus <- cluster::clara(data, k=3, metric = "euclidean", stand = TRUE, samples = 1000, pamLike = T)  
  # clus5 <- cluster::clara(data, k=5, metric = "euclidean", stand = TRUE, samples = 1000, pamLike = T) 
  # clusPCA <- cluster::clara(results, k=3, metric = "euclidean", stand = TRUE, samples = 1000, pamLike = T)
  clus5PCA <- cluster::clara(results, k=5, metric = "euclidean", stand = TRUE, samples = 1000, pamLike = T)
  
  # Create am index
  idx <- 1:ncell(s2terrain)
  idx1 <- idx[-unique(which(is.na(stack_vals), arr.ind=TRUE)[,1])] #The which() function in R returns the position or the index of the value which satisfies the given condition. The Which() function in R gives you the position of the value in a logical vector. The position can be of anything like rows, columns and even vector as well
  #str(idx1)
  
  # Create an empty raster using the first raster of your stack
  clust_raster <- s2terrain[[1]]
  clust_raster[] <- NA
  # clust_raster_5 <- clust_raster
  # clust_raster_PCA <- clust_raster
  clust_raster_PCA_5 <- clust_raster
  
  # Transfer the clustering results to your empty raster and save it
  # clust_raster[idx1] <- clus$clustering
  # clust_raster_5[idx1] <- clus5$clustering
  # clust_raster_PCA[idx1] <- clusPCA$clustering
  clust_raster_PCA_5[idx1] <- clus5PCA$clustering
  
  rData <- rast(clust_raster_PCA_5)
  
  pol <- terra::as.polygons(rData, dissolve = T)
  threshold  <- expanse(aggregate(pol, dissolve = T), unit = 'ha')*0.01
  n_pol <- smoothr::drop_crumbs(pol, units::set_units(threshold, hectare))
  n_pol <- fill_holes(n_pol, units::set_units(threshold, hectare))
  r_poly_smooth <- smooth(n_pol, method = "ksmooth", smoothness = 5)
  #plot(r_poly_smooth)
  
  talhonamento <- sf::st_as_sf(r_poly_smooth)
  # talhonamento_geojson <- geojsonsf::sf_geojson(
  #   talhonamento,
  #   atomise = FALSE,
  #   simplify = TRUE,
  #   digits = NULL,
  #   factors_as_string = TRUE
  # )
  #writeOGR(r_poly_smooth, dsn="C:/Projetos/talhonamento/test_geojson.GeoJSON", driver="GeoJSON")
  
  st_write(talhonamento, paste0('C:/Projetos/talhonamento/result3/', cod, '.GeoJSON'))
  
  #generate_map(clust_raster, clust_raster_5, clust_raster_PCA, clust_raster_PCA_5, s2terrain, cod, t)
  #cluster_map(clust_raster_PCA_5, s2terrain, cod, t, talhonamento)
  #writeRaster(clust_raster,paste0('result/',str_replace(r.file,'.tif', 'class_5.tif')))
  #writeRaster(s2terrainViz,paste0('result/',str_replace(r.file,'.tif', 'rgb.tif')))
} 

generate_map <- function(clust, clust_5, clust_PCA ,clust_PCA_5, viz, car.code, aoi){
  
  mapa_class <- tm_shape(clust)+tm_raster(title=paste0('cluster'), style = 'fixed', breaks = seq(1,4, by = 1), palette ='RdYlGn' )+
    tm_layout(legend.outside = T)+
    #tm_shape(aoi)+tm_borders(lwd = 2, col = 'red')+
    #tm_shape(aoi)+tm_borders(lwd = 2, col = 'blue')+
    tm_scale_bar(position=c(0.08, 0.90),width = 0.1)+ # Barra de escala
    tm_compass(type="arrow", position=c(0.10,0.07), show.labels = 1,size=2.5,fontsize = 0.6)#+ # Norte
  
  mapa_class_5 <- tm_shape(clust_5)+tm_raster(title=paste0('cluster 5'), style = 'fixed', breaks = seq(1,6, by = 1), palette ='RdYlGn' )+
    tm_layout(legend.outside = T)+
    #tm_shape(aoi)+tm_borders(lwd = 2, col = 'red')+
    #tm_shape(aoi)+tm_borders(lwd = 2, col = 'blue')+
    tm_scale_bar(position=c(0.08, 0.90),width = 0.1)+ # Barra de escala
    tm_compass(type="arrow", position=c(0.10,0.07), show.labels = 1,size=2.5,fontsize = 0.6)#+ # Norte
  #tmap_save(mapa_class, paste0(car.code, "_class.png"))
  
  mapa_class_PCA <- tm_shape(clust_PCA)+tm_raster(title=paste0('cluster PCA'), style = 'fixed', breaks = seq(1,4, by = 1), palette ='RdYlGn' )+
    tm_layout(legend.outside = T)+
    #tm_shape(aoi)+tm_borders(lwd = 2, col = 'red')+
    #tm_shape(aoi)+tm_borders(lwd = 2, col = 'blue')+
    tm_scale_bar(position=c(0.08, 0.90),width = 0.1)+ # Barra de escala
    tm_compass(type="arrow", position=c(0.10,0.07), show.labels = 1,size=2.5,fontsize = 0.6)#+ # Norte
  
  mapa_class_PCA_5 <- tm_shape(clust_PCA_5)+tm_raster(title=paste0('cluster PCA 5'), style = 'fixed', breaks = seq(1,6, by = 1), palette ='RdYlGn' )+
    tm_layout(legend.outside = T)+
    #tm_shape(aoi)+tm_borders(lwd = 2, col = 'red')+
    #tm_shape(aoi)+tm_borders(lwd = 2, col = 'blue')+
    tm_scale_bar(position=c(0.08, 0.90),width = 0.1)+ # Barra de escala
    tm_compass(type="arrow", position=c(0.10,0.07), show.labels = 1,size=2.5,fontsize = 0.6)#+ # Norte
  #tmap_save(mapa_class, paste0(car.code, "_class.png"))
  
  sel <- subset(viz, 2:4)
  
  for (i in  1:3) {
    sel[[i]] <- stretch(sel[[i]], maxv = 255, minv = 0, minq = 0.1, maxq = 0.9)  
  } 
  
  #talhoes <- readOGR('TO-1720978-17E04363D37E49768CEAE0BC47F196BE_talhoes_CarlosGuilherme.shp')
  
  mapa_rgb <- tm_shape(sel)+tm_rgb(r=1, g=2, b=3, max.value = max(maxValue(sel)))+
    #tm_shape(talhoes)+tm_borders(lwd = 5, col = 'red')+
    tm_shape(aoi)+tm_borders(lwd = 1, col = 'blue')+
    # tm_shape(talhoes[2,])+tm_borders(lwd = 5, col = 'blue')+
    # tm_shape(talhoes[3,])+tm_borders(lwd = 5, col = 'blue')+
    tm_scale_bar(position=c(0.08, 0.90),width = 0.1)+ # Barra de escala
    tm_compass(type="arrow", position=c(0.10,0.07), show.labels = 1,size=2.5,fontsize = 0.6)+
    tm_grid(labels.margin.y = -0.3,alpha=0.1,n.x=5,n.y=5)+ # Grid
    tm_layout(main.title = paste0("RGB"), main.title.position = 'center')
  
  #mapa_rgb
  
  current.mode <- tmap_mode("plot")
  result <- tmap_arrange(mapa_class, mapa_class_5, mapa_class_PCA, mapa_class_PCA_5, mapa_rgb)
  tmap_mode(current.mode)
  
  tmap_save(result, paste0('C:/Projetos/talhonamento/result2/', car.code, ".png"))
}

cluster_map <- function(clust_PCA_5, viz, car.code, aoi, smoothPoly){
  
  mapa_class_PCA_5 <- tm_shape(clust_PCA_5)+tm_raster(title=paste0('cluster PCA 5'), style = 'fixed', breaks = seq(1,6, by = 1), palette ='RdYlGn' )+
    tm_layout(legend.outside = T)+
    #tm_shape(aoi)+tm_borders(lwd = 2, col = 'red')+
    #tm_shape(aoi)+tm_borders(lwd = 2, col = 'blue')+
    tm_scale_bar(position=c(0.08, 0.90),width = 0.1)+ # Barra de escala
    tm_compass(type="arrow", position=c(0.10,0.07), show.labels = 1,size=2.5,fontsize = 0.6)#+ # Norte
  #tmap_save(mapa_class, paste0(car.code, "_class.png"))
  
  sel <- subset(viz, 2:4)
  
  for (i in  1:3) {
    sel[[i]] <- stretch(sel[[i]], maxv = 255, minv = 0, minq = 0.1, maxq = 0.9)  
  } 
  
  #talhoes <- readOGR('TO-1720978-17E04363D37E49768CEAE0BC47F196BE_talhoes_CarlosGuilherme.shp')
  
  mapa_rgb <- tm_shape(sel)+tm_rgb(r=1, g=2, b=3, max.value = max(maxValue(sel)))+
    #tm_shape(talhoes)+tm_borders(lwd = 5, col = 'red')+
    tm_shape(aoi)+tm_borders(lwd = 2, col = 'blue')+
    # tm_shape(talhoes[2,])+tm_borders(lwd = 5, col = 'blue')+
    # tm_shape(talhoes[3,])+tm_borders(lwd = 5, col = 'blue')+
    tm_scale_bar(position=c(0.08, 0.90),width = 0.1)+ # Barra de escala
    tm_compass(type="arrow", position=c(0.10,0.07), show.labels = 1,size=2.5,fontsize = 0.6)+
    tm_grid(labels.margin.y = -0.3,alpha=0.1,n.x=5,n.y=5)+ # Grid
    tm_layout(main.title = paste0("RGB"), main.title.position = 'center')
  
  mapa_cluster <- tm_shape(sel)+tm_rgb(r=1, g=2, b=3, max.value = max(maxValue(sel)))+
    #tm_shape(talhoes)+tm_borders(lwd = 5, col = 'red')+
    tm_shape(smoothPoly)+tm_borders(lwd = 2, col = 'red')+
    # tm_shape(talhoes[2,])+tm_borders(lwd = 5, col = 'blue')+
    # tm_shape(talhoes[3,])+tm_borders(lwd = 5, col = 'blue')+
    tm_scale_bar(position=c(0.08, 0.90),width = 0.1)+ # Barra de escala
    tm_compass(type="arrow", position=c(0.10,0.07), show.labels = 1,size=2.5,fontsize = 0.6)+
    tm_grid(labels.margin.y = -0.3,alpha=0.1,n.x=5,n.y=5)+ # Grid
    tm_layout(main.title = paste0("RGB"), main.title.position = 'center')
  
  #mapa_rgb
  
  current.mode <- tmap_mode("plot")
  result <- tmap_arrange(mapa_class_PCA_5, mapa_rgb, mapa_cluster)
  tmap_mode(current.mode)
  
  tmap_save(result, paste0('C:/Projetos/talhonamento/result3/', car.code, ".png"))
}

getAc <- function(cod){
  cod <- 'TO-1720978-52D67EB87AB5455DB2AC7E5787C96118'
  #Connect to Postgres
  db <- 'carbon'  #provide the name of your db
  host_db <- '186.202.136.178'    
  db_port <- '5432'  # or any other port specified by the DBA
  db_user <- 'ferraz' 
  db_password <- '$14EcZy2snd3'
  con <- dbConnect(RPostgres::Postgres(), dbname = db, host=host_db, port=db_port, user=db_user, password=db_password)  
  
  #Build query
  query = "select ac.geom as geom from car_layer.area_consolidada_to as ac, car.area_imovel_to as ai where st_intersects(st_makevalid(ac.geom), st_makevalid(ai.geom)) = true and cod_imovel = '{cod}';"
  q <- glue(query)
  
  r <- st_read(con, query = q)
  
  #rs = dbSendQuery(con,q)
  #df = fetch(rs,n=-1)
  #dbClearResult(rs) 
  dbDisconnect(con)
  
  return(r)
}

setwd("C:/Projetos/talhonamento/input")

dirs <- list.dirs(full.names = TRUE, recursive = TRUE)
dirs <- dirs[-1]
#d <- "./TO-1700707-0D678788927E4C55B7706E3A718B9912"

for (d in dirs){
#  getwd()
  #setwd(d)
  file.raster <- dir(d, pattern = "multiband.tif$", full.names = TRUE, ignore.case = TRUE)
  vector <- dir(d, pattern = "vector.shp$", full.names = TRUE, ignore.case = TRUE)
  talhao <- dir(d, pattern = "talhoes.shp$", full.names = TRUE, ignore.case = TRUE)
  
  execute(file.raster, vector, talhao)
} 


# h <- hash()
# 
# for (r in files.raster){
#   key <- strsplit(r, split = "_")[1]
#   for (v in files.vector){
#     value <- strsplit(v, split = "_")[1]
#     if(key[[1]][1] == value[[1]][1]) {
#       h[[r]] <- v
#     }
#   }
# }

 
