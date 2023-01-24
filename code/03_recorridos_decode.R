# Este codigo permite decodificar los resultados de la request de google maps
# Los recorridos de google mapas estan puntos pero codificados con JS
# Usamos la libreria googlepolylines para transformar esos codigos en puntos

# Generalmente partimos de una carpeta donde tenemos una serie de archivos
# Porque genermos archivos muy pesados

                          # ---   FUNCION DECODE   ---   #

funcion_decode <- function(dir_entrada){
  ################################## ARCHIVO   ##################################

    df_tot <- data.frame()
    df_rb <- data.frame()
    df_salida <- data.frame()
    df_salida_2<- data.frame()
    setwd(dir_entrada)
    archivo <- dir(dir_entrada, pattern = ".csv")
    data_f_2 <- data.frame()
    #p<-3
    for(p in 1:length(archivo)){
      df_tot <- data.frame()
      df_rb <- data.frame()
      df_salida <- data.frame()
      df_salida_2<- data.frame()
      data_f_2 <- data.frame()
      print(c("Esto es p =",p))
      #df <- read.csv(archivo[1],  header=TRUE, sep=",", dec=".", stringsAsFactors = FALSE, na.strings = "NA")
      df <- read.csv(archivo [p],  header=TRUE, sep=",", dec=".", stringsAsFactors = FALSE, na.strings = "NA")
      
      name_archivo<- gsub('.csv', "_decode",archivo[p])
      tipo_transporte <- name_archivo
      print(tipo_transporte)
      df$name_archivo<- name_archivo
      df$nombre_trayecto <- paste(df$nombre_origen,"-",df$nombre_destino,sep = "")
      ################################## TRAYECTO  ##################################
      df %>%
        group_by(nombre_trayecto) %>%  #tipo_transp
        group_split() -> data_ingreso
      
      df_ingreso_2 <- data.frame()
      
      for (k in 1:length(data_ingreso)){
        # if(i %%100==0){
        #   print(i)
        # }
        #print(c("Esto es k =",k))
        df_ingreso_1 <- data.frame(data_ingreso[[k]])
        # if(tipo_transporte=="colectivo_v2_decode"){
        # 
        #   # SI ES COLECTIVO
        #   names(df_ingreso_1) <- c( "null","null","ID",  "distance.text", "distance.value"  ,
        #                             "duration.text","duration.value" , "start_location.lat",
        #                             "start_location.lng", "end_location.lat","end_location.lng",
        #                             "travel_mode", "num","polyline" ,
        #                              "nombre_origen",
        #                             "nombre_destino","id_origen","id_destino", "id_origen2", "id_destino2",       
        #                             "id_trayecto","name_archivo", "nombre_trayecto")
        # }else{
          
          #SI NO ES COLECTIVO
          names(df_ingreso_1) <- c( "null","null","ID",  "distance.text", "distance.value"  ,  
                                    "duration.text","duration.value" , "start_location.lat",
                                    "start_location.lng", "end_location.lat","end_location.lng",  
                                    "travel_mode",  #"linea" , "parada"  ,          
                                    "num","polyline" , "nombre_origen",     
                                    "nombre_destino","id_origen","id_destino", "id_origen2", "id_destino2",       
                                    "id_trayecto","name_archivo", "nombre_trayecto")
        #}
        
        
        name_trayecto <- df_ingreso_1$nombre_trayecto[1]
        decode_df <- decode(df_ingreso_1$polyline)
        
        distancia_tot <- sum( df_ingreso_1$distance.value)
        tiempo_tot <- sum( df_ingreso_1$duration.value)
        ################################## LEG  ##################################
        for (i in 1:length(decode_df)){
          df_decode_df <- decode_df[[i]]
          
          data_f_2 <- data.frame()
          
          
          ################################## LEG-LEG  ##################################    
          for (j in  1:nrow(df_decode_df)){
            if (j %% 10 == 0) {
              
              
              data_f <- data.frame(df_decode_df[j,])
              data_f_2<- rbind(data_f_2,data_f)
              
            }
            else if (j %% 1== 0) {
              
              
              data_f <- data.frame(df_decode_df[j,])
              data_f_2<- rbind(data_f_2,data_f)
              
              
            }
          }
          
          data_f_2$tiempo <- df_ingreso_1$duration.value[i]
          data_f_2$distancia <- df_ingreso_1$distance.value[i]
          data_f_2$id_origen <- df_ingreso_1$id_origen[i]
          data_f_2$id_destino <- df_ingreso_1$id_destino[i]
          data_f_2$id_trayecto <- df_ingreso_1$id_trayecto[i]
          data_f_2$nombre_origen<- df_ingreso_1$nombre_origen[i]
          data_f_2$nombre_destino<- df_ingreso_1$nombre_destino[i]
          
          data_f_2$name_trayecto <- name_trayecto
          #data_f_2$name_archivo<- name_archivo
          df_salida<- rbind(df_salida,data_f_2)
          
        }
        
        num_rows<-   nrow(df_salida)
        ID <- c(1:num_rows)
        data_frame_resp <- cbind(ID, df_salida)
        data_frame_resp$distancia_tot <- distancia_tot
        data_frame_resp$tiempo_tot <- tiempo_tot
        #data_frame_resp$name_archivo <- archivo[p]
        df_salida_2<- rbind(df_salida_2,data_frame_resp)
        #nombre_archivo_guardar <-paste(directorio_,"final/" ,name_archivo,"_proc.csv",sep = "")

      }
      

      nombre_archivo_guardar <-paste(dir_entrada,"decode/",name_archivo,".csv",sep = "")
      
      # Estamos gurdando puntos de los recorridos
      write.csv(df_salida_2,nombre_archivo_guardar)
    }
    
    
    
    
}
###############################################################################
# CORREMOS LA FUNCION

###         BICICLETAS 
directorio <-"D:/Josefina/Proyectos/salud/movilidad_3/procesamiento/colectivos/recorridos/"
setwd(directorio)
archivo <- dir(directorio, pattern = ".csv")
###         COLECTIVOS     
directorio <-"D:/Josefina/Proyectos/salud/movilidad_3/procesamiento/colectivos/recorridos/"
setwd(directorio)
archivo <- dir(directorio, pattern = ".csv")

decode_auto <- funcion_decode(dir_entrada=entrada_dir)
# Estamos gurdando puntos de los recorridos
entrada_dir <-"D:/Josefina/Proyectos/salud/movilidad_5/procesamiento/recorridos/id/"

#########################################################################
# Union de los puntos con lineas

points_to_line <- function(data=dat, long, lat, id_field = NULL, sort_field = NULL) {
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
      
      l <- SpatialLines(list(Lines(list(Line(paths[[p]])), id)))
      
      sp_lines <- spRbind(sp_lines, l)
    }
    
    return(sp_lines)
  }
}

# directorio <-"D:/Josefina/Proyectos/salud/movilidad_3/procesamiento/bicicletas/recorridos/final/tot/"
# 
# setwd(directorio)
# directorio_archivo <- dir(directorio, pattern = ".csv")
datos<- df#directorio_archivo[1]

# Prueba departamentos
datos <- read.csv("D:/Josefina/Proyectos/salud/movilidad_5/procesamiento/recorridos/id/decode/colectivo_v2_decode.csv")
v_lines <- points_to_line(data = datos, 
                          long = "lon", 
                          lat = "lat", 
                          id_field = "id_trayecto", 
                          sort_field = "ID")

id_df <- data.frame()
datos%>%
  group_by(id_trayecto) %>%  
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
  data_frame_1 <- data.frame(id,nombre_origen,nombre_destino,id_origen,
                             id_destino,distancia_tot,tiempo_tot,id_trayecto)
  names (data_frame_1)<- c("id","nombre_origen","nombre_destino",
                           "id_origen","id_destino","distancia_tot","tiempo_tot","id_trayecto")
  
  id_df <- rbind(id_df,data_frame_1)
}


df2<-SpatialLinesDataFrame(v_lines, id_df , match.ID = F)
proj4string(df2) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
#Guardamos
writeOGR(df2,"D:/Josefina/Proyectos/salud/movilidad_5/procesamiento/recorridos/final","colectivo_v3_decode", driver="ESRI Shapefile")


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

directorio_shape <- ("D:/Josefina/Proyectos/salud/movilidad_3/procesamiento/colectivos/recorridos/final/tot")

prueba <- unir_SPDF(directorio_shape)
# Eliminamos las geometrias duplicadas
prueba_2 <- prueba[!duplicated(prueba$id_tryc), ]

# Guardamos
setwd("D:/Josefina/Proyectos/salud/movilidad_3/procesamiento/colectivos/recorridos/recorridos_tot/shape/tot")

proj4string(prueba_2) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

st_write(prueba_2, "proc_proc_combinado.shp", delete_layer = TRUE,crs)



