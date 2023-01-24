# Con este codigo utilizamos la api de google para conocer los recorridos
# entre puntos de origen-destino
#link de la api
#https://developers.google.com/maps/documentation/directions/get-directions

# --------   Modes -------- #
#&mode=transit&modes=bus
#&mode=walking
#&mode=DRIVING
#&mode=bicycling
#&mode=WALKING

                             # --- FUNCION RECORRIDOS --- #

recorridos <- function(modo, coordenadas,key){
  coordinates <- coordenadas
  resp_df_competo <- data.frame()
  url_part_1 <- "https://maps.googleapis.com/maps/api/directions/json?&origin=" 
  
  url_part_2 <- "&destination="
  mode <- modo
  if (modo=="colectivo"){
    api_modo <- "&mode=transit&modes=bus"
  }
  
  if (modo=="pie"){
    api_modo <- "&mode=walking"
  }
  
  if (modo=="auto"){
    api_modo <- "&mode=driving"#DRIVING"
  }
  
  if (modo=="bicicleta"){
    api_modo <- "&mode=bicycling"
  }
  # vER LA KEY
  api_key<- paste(api_modo,"&key=",key,sep="")
  
  for(i in 1:nrow(coordinates )){
  
    if (i %% 100 == 0) {
      print (i)
    }
    
    destination <-coordinates$COORD_DESTINO[i]#coordinates[!coordinates$ID_distrit == origin_id,]
    origin <- coordinates$COORD_ORIGEN[i]#coordinates[coordinates$ID_distrit == origin_id,]
    nombre_origen <- mapas$DPTO_ORIGEN[i]
    nombre_destino <- mapas$DPTO_DESTINO[i]
    url <- paste0(url_part_1,
                  origin,
                  url_part_2,
                  paste0(destination,collapse = "|"),
                  api_key,collapse = "")
    
    response <- GET(url)
    
    resp_json <- fromJSON(content(response, as = "text"))
   
  
    resp <- data.frame(resp_json[["routes"]][["legs"]][[1]][["steps"]])
    if( modo=="colectivo"){
    # SI ES COLECTIVO AGREGAMOS MAS INFO, COMO LAS PARADAS SINO NO!
      if (length(resp$travel_mode)==0){
        
        resp_df <- data.frame(ID = NA,
                              distance.text=NA,
                              distance.value=NA,
                              duration.text=NA,
                              duration.value=NA,
                              distance=NA,
                              duration=NA,
                              start_location=NA,
                              end_location=NA,
                              travel_mode=NA,
                              linea = NA,#resp$transit_details$line$short_name,
                              parada = NA ,#NA,#,resp$transit_details$num_stops,
                              num = NA,
                              polyline = NA,
                              
                              nombre_origen=nombre_origen,
                              nombre_destino=nombre_destino)
        
        resp_df_competo <- resp_df
        #SI ES COLECTIVO SON OTROS NOMBRES
        names(resp_df_competo) <-c("ID","distance.text",
                                   "distance.value","duration.text",
                                   "duration.value","start_location.lat",
                                   "start_location.lng", "end_location.lat",
                                   "end_location.lng","travel_mode",
                                   "linea" , "parada",
                                   "num","polyline",
                                   "nombre_origen","nombre_destino" )
        
      }else{
      df_rbind <- data.frame()
      
      # for(x in 1:length(resp$travel_mode)){ 
      #   
      #   if (resp$travel_mode[x]== "TRANSIT"){
      #     df <- data.frame("TRUE")
      #     names(df)<- "tipo"
      #     df_rbind<- rbind(df_rbind,df)
      #     names(df_rbind)<- "tipo"
      #   }
      #   
      # }
      # if (dim(df_rbind)[1]>0){
       
        
        resp_df <- data.frame(distance=resp$distance,
                              duration=resp$duration,
                              start_location=resp$start_location,
                              end_location=resp$end_location,
                              travel_mode=resp$travel_mode,
                              linea = resp$transit_details$line$short_name,
                              parada = resp$transit_details$num_stops,
                              num = i,
                              polyline = resp$polyline$points,
                              
                              nombre_origen=nombre_origen,
                              nombre_destino=nombre_destino)
        num_rows<-  nrow(resp_df)
        ID <- c(1:num_rows)
        data_frame_resp <- cbind(ID , resp_df)
        resp_df_competo <- rbind (resp_df_competo,data_frame_resp)
        #SI ES COLECTIVO SON OTROS NOMBRES
        names(resp_df_competo) <- c("ID","distance.text","distance.value","duration.text",
                                    "duration.value","start_location.lat",
                                    "start_location.lng", "end_location.lat",
                                    "end_location.lng","travel_mode","linea" , "parada","num","polyline","nombre_origen","nombre_destino" )
      } 
      # else{
      #   
      #   resp_df <- data.frame(distance=resp$distance,
      #                         duration=resp$duration,
      #                         start_location=resp$start_location,
      #                         end_location=resp$end_location,
      #                         travel_mode=resp$travel_mode,
      #                         linea = "NO-BUS",#resp$transit_details$line$short_name,
      #                         parada = "NO-BUS",#NA,#,resp$transit_details$num_stops,
      #                         num = i,
      #                         polyline = resp$polyline$points,
      #                         
      #                         nombre_origen=nombre_origen,
      #                         nombre_destino=nombre_destino)
      #   num_rows<-  nrow(resp_df)
      #   ID <- c(1:num_rows)
      #   data_frame_resp <- cbind(ID , resp_df)
      #   resp_df_competo <- rbind (resp_df_competo,data_frame_resp)
      #   #SI ES COLECTIVO SON OTROS NOMBRES
      #   names(resp_df_competo) <- c("ID","distance.text",
      #                               "distance.value","duration.text",
      #                               "duration.value","start_location.lat",
      #                               "start_location.lng", "end_location.lat",
      #                               "end_location.lng","travel_mode",
      #                               "linea" , "parada",
      #                               "num","polyline",
      #                               "nombre_origen","nombre_destino" )
      # }
      #}
      }else{
            # SI NO ES COLECTIVO
            
            if (length(resp$travel_mode)==0){
              
              resp_df <- data.frame(ID = NA,
                                    distance.text=NA,
                                    distance.value=NA,
                                    duration.text=NA,
                                    duration.value=NA,
                                    distance=NA,
                                    duration=NA,
                                    start_location=NA,
                                    end_location=NA,
                                    travel_mode=NA,
                                    linea = NA,#resp$transit_details$line$short_name,
                                    parada = NA ,#NA,#,resp$transit_details$num_stops,
                                    num = NA,
                                    polyline = NA,
                                    
                                    nombre_origen=nombre_origen,
                                    nombre_destino=nombre_destino)
              
              resp_df_competo <- resp_df
              #SI ES COLECTIVO SON OTROS NOMBRES
              names(resp_df_competo) <-c("ID","distance.text",
                                         "distance.value","duration.text",
                                         "duration.value","start_location.lat",
                                         "start_location.lng", "end_location.lat",
                                         "end_location.lng","travel_mode",
                                         "linea" , "parada",
                                         "num","polyline",
                                         "nombre_origen","nombre_destino" )
              
            }else{ 
              resp_df <- data.frame(distance=resp$distance,
                          duration=resp$duration,
                          start_location=resp$start_location,
                          end_location=resp$end_location,
                          travel_mode=resp$travel_mode,
                         
                          num = i,
                          polyline = resp$polyline$points,
  
                          nombre_origen=nombre_origen,
                          nombre_destino=nombre_destino)
      num_rows<-  nrow(resp_df)
      ID <- c(1:num_rows)
      data_frame_resp <- cbind(ID , resp_df)
      resp_df_competo <- rbind (resp_df_competo,data_frame_resp)
      names(resp_df_competo) <- c("ID","distance.text","distance.value","duration.text",
                                "duration.value","start_location.lat",
                                "start_location.lng", "end_location.lat",
                                "end_location.lng","travel_mode","num","polyline",
                                "nombre_origen","nombre_destino" )
            }
      }
  }
  return(resp_df_competo)
}
   
######################################################################################
# Carpeta donde estamos trabajando
setwd("D:/Josefina/Proyectos/salud/modelo_movilidad_V1/")
# -- Corremos la funcion
# Atencion con los colectivos!!
mapas <- read.csv("./procesamiento/datos_entrada/mapas_sd.csv", sep=",")

api_key <- "AIzaSyB1v7tsUDZZaqRPqKwHZefNWP0OtwIau-Q"
prueba_auto<- recorridos(modo="auto",coordenadas = mapas,key=api_key )
prueba_pie<- recorridos(modo="pie",coordenadas = mapas,key=api_key )
prueba_bicicleta<- recorridos(modo="bicicleta",coordenadas = mapas,key=api_key )
prueba_colectivo<- recorridos(modo="colectivo",coordenadas = mapas,key=api_key)

######################################################################################3
# ----- GUARDAMOS ------
modo
vuelta <- "colectivo"
nombre_recorrido <- paste("D:/Josefina/Proyectos/salud/movilidad_5/procesamiento/recorridos/",vuelta,".csv",sep="")
write.csv(resp_df_competo,nombre_recorrido)
