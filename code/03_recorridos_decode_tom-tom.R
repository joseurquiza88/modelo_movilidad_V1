# Union de los puntos con lineas

points_to_line <- function(data, long, lat, id_field = NULL, sort_field = NULL) {
  #https://rpubs.com/walkerke/points_to_line 
  # Convert to SpatialPointsDataFrame
  coordinates(data) <- c(long, lat)
  
  # If there is a sort field...
  if (!is.null(sort_field)) {
    if (!is.null(id_field)) {
      data <- data[order(data[[id_field]], data[[sort_field]]), ]
    } else {
      data <- data[order(data[[sort_field]]), ]
    }
  }
  
  # If there is only one path...
  if (is.null(id_field)) {
    
    lines <- SpatialLines(list(Lines(list(Line(data)), "id")))
    
    return(lines)
    
    # Now, if we have multiple lines...
  } else if (!is.null(id_field)) {  
    
    # Split into a list by ID field
    paths <- sp::split(data, data[[id_field]])
    
    sp_lines <- SpatialLines(list(Lines(list(Line(paths[[1]])), "line1")))
    #sp_lines2 <- SpatialLines(list(Lines(list(Line(paths[[1]])), "line1")))
    
    
    #plot(mypoints, axes=TRUE)
    
    # I like for loops, what can I say...
    for (p in 2:length(paths)) {
      id <- paste0("line", as.character(p))
      print(id)
      
      l <- SpatialLines(list(Lines(list(Line(paths[[p]])), id)))
      
      sp_lines <- spRbind(sp_lines, l)
    }
    
    return(sp_lines)
  }
}
archivo <- "auto_prueba"
name_archivo <- paste("D:/Josefina/Proyectos/salud/movilidad_6/procesamiento/recorridos/auto/",archivo, ".csv",sep="")
datos<- read.csv(name_archivo)

v_lines <- points_to_line(data = datos, 
                          long = "long", 
                          lat = "lat", 
                          id_field = "alternativa",#"id_trayecto",#"id_trayecto", 
                          sort_field = "ID")
datos$name_trayecto<- paste(datos$nombre_origen,"-",datos$nombre_destino,sep="")

names(datos)<- c("X","ID","tiempo_tot" ,"distancia_tot",
                 "long","lat","departureTime","arrivalTime",
                 "trafficDelayInSeconds","trafficLengthInMeters", "travelMode",
                 "num",  "nombre_origen" ,"nombre_destino" ,"id_origen" ,
                 "id_destino","id_trayecto","historicTrafficTravelTimeInSeconds","name_trayecto" )

id_df <- data.frame()
datos%>%
  group_by(id_trayecto) %>%  #(id_trayecto) %>%  
  group_split() -> dat_agrupado

length(dat_agrupado)

for (x in 1:length(v_lines@lines)){
  id <- v_lines@lines[[x]]@ID
  name_trayecto<-  dat_agrupado[[x]][["name_trayecto"]][1]
  nombre_origen<- dat_agrupado[[x]][["nombre_origen"]][1]
  nombre_destino<- dat_agrupado[[x]][["nombre_destino"]][1]
  id_origen<- dat_agrupado[[x]][["id_origen"]][1]
  id_destino<- dat_agrupado[[x]][["id_destino"]][1]
  distancia_tot<- dat_agrupado[[x]][["distancia_tot"]][1]
  tiempo_tot <- dat_agrupado[[x]][["tiempo_tot"]][1]
  id_trayecto<- dat_agrupado[[x]][["id_trayecto"]][1]
  historicTrafficTravelTimeInSeconds<- dat_agrupado[[x]][["historicTrafficTravelTimeInSeconds"]][1]
  data_frame_1 <- data.frame(id,nombre_origen,nombre_destino,id_origen,
                             id_destino,distancia_tot,tiempo_tot,id_trayecto,historicTrafficTravelTimeInSeconds)
  names (data_frame_1)<- c("id","nombre_origen","nombre_destino",
                           "id_origen","id_destino","distancia_tot","tiempo_tot","id_trayecto","historicTrafficTravelTimeInSeconds")
  
  id_df <- rbind(id_df,data_frame_1)
}


df2<-SpatialLinesDataFrame(v_lines, id_df , match.ID = F)
proj4string(df2) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
nombre_guardar <-  paste(archivo,sep="")
nombre_guardar
#Guardamos
#iteOGR(df2,"D:/Josefina/Proyectos/salud/movilidad_5/procesamiento/recorridos/final","colectivo_v3_decode", driver="ESRI Shapefile")
writeOGR(df2,"D:/Josefina/Proyectos/salud/movilidad_6/procesamiento/recorridos/auto/shape",nombre_guardar, driver="ESRI Shapefile")
writeOGR(v_lines,"D:/Josefina/Proyectos/salud/movilidad_6/","nombre_guardar", driver="ESRI Shapefile")


########################################################################3
##########################################################################
# A esto lo hacemos cuando soy muy grandes los archivos
# Por ejemplo en el de dpto no es necesario
# COMBINAMOS .SHP DE UNA CARPETA Y LO PONEMOS EN UNO SOLO
unir_SPDF <- function(directorio) {
  setwd(directorio)
  files <- list.files(path = directorio, pattern = "*.shp", recursive = T)# a partir de esto construyo un bucle para leer los archivos:
  files_total<-list()
  for (i in files) {
    print(i)
    files_total[[i]]<- sf::st_read (paste(sep="","",i))
  }
  lista_de_shp<- files_total
  #stopifnot(!any(sapply(lista_de_shp, class)=="SpatialPolygonsDataFrame"))        
  shp <- lista_de_shp[[1]]                                                        
  for (i in 2:length(lista_de_shp)) {     
    shp <- union(shp, lista_de_shp[[i]])   
  }
  return(shp)                               
}

#directorio_shape2 <- ("D:/Josefina/Proyectos/salud/movilidad_3/procesamiento/colectivos/recorridos/final/tot")
directorio_shape3 <- ("D:/Josefina/Proyectos/salud/movilidad_6/procesamiento/recorridos/auto/shape")
directorio <- directorio_shape3
prueba2 <- unir_SPDF(directorio_shape2)
prueba3 <- unir_SPDF(directorio_shape3)
# Eliminamos las geometrias duplicadas
prueba_2 <- prueba[!duplicated(prueba$id_tryc), ]
prueba_3a <- prueba3[!duplicated(prueba3$id_tryc), ]

# Guardamos
setwd("D:/Josefina/Proyectos/salud/movilidad_3/procesamiento/colectivos/recorridos/recorridos_tot/shape/tot")
setwd("D:/Josefina/Proyectos/salud/movilidad_6/procesamiento/recorridos/auto/shape/")

proj4string(prueba_2) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
proj4string(prueba3) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

st_write(prueba_2, "proc_proc_combinado.shp", delete_layer = TRUE,crs)
st_write(prueba_2, "pie_06_combinado.shp", delete_layer = TRUE)
st_write(prueba3, "auto_combinado.shp", delete_layer = TRUE)
write.csv(prueba3, "colectivo_combinado.csv")
prueba_3b <- prueba3[,4:9]
