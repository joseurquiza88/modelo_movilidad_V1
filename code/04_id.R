
## -------- INGRESO DE DATOS DE ORIGEN - DESTINO -------- #####

#Le ponemos un ID para luego generar las grillas
# archivo con ID

funcion_id <- function(df_recorridos,puntos,modo){
  
  # Archivo con nombres de los puntos origen destino
  # Les pongo ID
  mapas<- puntos
  for (x in 1:nrow(mapas)){
    num_rows<-   nrow(mapas)
    ID <- c(1:num_rows)
    mapas_id <- cbind(ID, mapas)
  }
  #write.csv(mapas_id,"D:/Josefina/Proyectos/salud/movilidad_5/procesamiento/coordenadas/puntos.csv")
  
  
  ## ------  Les pongo el ID  al archivo de los recorridos    ------ ##
  df_salida<- data.frame()
  nombre_recorridos <- paste(df_recorridos,".csv",sep = "")
  df_recorridos <- read.csv(nombre_recorridos)
  for (i in 1: nrow(mapas_id)){ 
    # if (i %% 50 == 0) {
    #   print (i)
    # }
    
    tabla <- df_recorridos
    eq_origen <- which((tabla$nombre_origen) == (mapas_id[i,]$punto))
    
    tabla <- tabla[eq_origen,] 
    dim_tabla <- dim(tabla)
    if(dim_tabla[1] == 0){
      salida <- data.frame(NA, NA, NA, NA,NA,NA,NA,NA,
                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)   
      
    }else{
      print("a1")
        salida <- data.frame(tabla,mapas_id[i,]$ID)
        # names(salida) <- c("X", "ID","distance.text" ,    
        #                    "distance.value","duration.text","duration.value",    
        #                    "start_location.lat" ,"start_location.lng", "end_location.lat",  
        #                    "end_location.lng","travel_mode","num" ,              
        #                    "polyline","nombre_origen","nombre_destino",    
        #                    "id_origen")
        
        names(salida) <- c("X", "ID","distance.text" ,    
                           "distance.value","duration.text","duration.value",    
                           "start_location.lat" ,"start_location.lng", "end_location.lat",  
                           "end_location.lng","travel_mode","num" ,              
                           "polyline","nombre_origen","nombre_destino",    
                           "id_origen")
        }
    df_salida <- rbind(df_salida, salida)
    print("a")
    names(df_salida) <- c("X", "ID","distance.text" ,    
                         "distance.value","duration.text","duration.value",    
                         "start_location.lat" ,"start_location.lng", "end_location.lat",  
                         "end_location.lng","travel_mode","num" ,              
                         "polyline","nombre_origen","nombre_destino",    
                         "id_origen")    
    }
  df_salida2<- data.frame()
  
  for (i in 1: nrow(mapas_id)){ 
    # if (i %% 50 == 0) {
    #   print (i)
    # }
    
    tabla2 <- df_salida 
    eq_destino <- which((tabla2$nombre_destino) == (mapas_id[i,]$punto))

    
    tabla2 <- tabla2[eq_destino,] 
    dim_tabla <- dim(tabla2)
    if(dim_tabla[1] == 0){
      salida <- data.frame(NA, NA, NA, NA,NA,NA,NA,NA,
                           NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)   
      
    }else{ 
    salida <- data.frame(tabla2,mapas_id[i,]$ID)
    names(salida) <- c("X", "ID","distance.text" ,    
                         "distance.value","duration.text","duration.value",    
                         "start_location.lat" ,"start_location.lng", "end_location.lat",  
                         "end_location.lng","travel_mode","num" ,              
                         "polyline","nombre_origen","nombre_destino",    
                         "id_origen","id_destino")
    }
    print("b")
    df_salida2 <- rbind(df_salida2, salida)
    names(df_salida2) <- c("X", "ID","distance.text" ,    
                          "distance.value","duration.text","duration.value",    
                          "start_location.lat" ,"start_location.lng", "end_location.lat",  
                          "end_location.lng","travel_mode","num" ,              
                          "polyline","nombre_origen","nombre_destino",    
                          "id_origen","id_destino")    
  }
  
  ################### Ponemos el 0 
  for (i in 1:nrow(df_salida2)){
    # if (i %% 1000 == 0) {
    #   print (i)
    # }
    if (df_salida2$id_origen[i] < 10){
      df_salida2$id_origen2[i]<- paste("0",df_salida2$id_origen[i],sep="")
    }else{
      df_salida2$id_origen2[i] <- df_salida2$id_origen[i]
    }
    
  }
  # 
  for (i in 1:nrow(df_salida2)){
    if (i %% 1000 == 0) {
      print (i)
    }
    if (df_salida2$id_destino[i] < 10){
      df_salida2$id_destino2[i] <- paste("0",df_salida2$id_destino[i],sep="")
    }else{
      df_salida2$id_destino2[i] <- df_salida2$id_destino[i]
    }
    
  }
  df_salida2$id_trayecto <- paste(df_salida2$id_origen2,"-",df_salida2$id_destino2,sep="")
  return( df_salida2)
}

df_recorridos <-"D:/Josefina/Proyectos/salud/movilidad_5/procesamiento/recorridos/auto"
puntos <- read.csv("D:/Josefina/Proyectos/salud/movilidad_5/procesamiento/datos_entrada/coordenadas/puntos.csv", sep=",")


prueba<-funcion_id(df_recorridos=df_recorridos,puntos=puntos)
nombre_salida <- paste(df_recorridos,"_v2.csv",sep="")
nombre_salida 
write.csv(prueba_bici, nombre_salida)


##########################################################################3


#names(trayectos) <- "sitios"
puntos <- read.csv("D:/Josefina/Proyectos/salud/movilidad_5/procesamiento/datos_entrada/coordenadas/puntos.csv", sep=",")


mapas<- puntos
for (x in 1:nrow(puntos)){
  num_rows<-   nrow(puntos)
  ID <- c(1:num_rows)
  mapas_id <- cbind(ID, puntos)
}

################### Ponemos el 0 
for (i in 1:nrow(mapas_id)){
  # if (i %% 1000 == 0) {
  #   print (i)
  # }
  if (mapas_id$ID[i] < 10){
    mapas_id$ID2[i]<- paste("0",mapas_id$ID[i],sep="")
  }else{
    mapas_id$ID2[i] <- mapas_id$ID[i]
  }
  
}
trayectos<- (sort(unique(prueba$id_trayecto)))
grilla<-  data.frame(matrix(ncol = nrow(mapas_id)+1, nrow = length(trayectos)))
grilla$X1 <- trayectos
names_cols <- mapas_id$ID2
names(grilla) <- c("distritos",names_cols)

write.csv(grilla,"D:/Josefina/Proyectos/salud/movilidad_5/procesamiento/grilla/grilla.csv")






