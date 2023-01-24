#fastest
#shortest
key_1 <- "L4P6mCEdDjNejIszYS44dMMlW1n9Imzj" #josefina.backup ok
key_2 <-"XB3nUS9mmbqwtAoyyPFh0jDAKf20cMOL" # josefina.urquizap OKp
key_3 <-"TsDPqIWPvjafpmmZMAh5255bziGL1tEA"#jurquicha@gmail.com 
key_4 <-"2uZZkn5R9YGXTznHS2NPla5ZSJ1NcWbd" #"joseurquiza88"
key=key_1
mapas <- read.csv("D:/Josefina/Proyectos/salud/movilidad_6/procesamiento/datos_entrada/coordenadas/prueba.csv", sep=",")
modo="car"
coordinates  <- mapas
#recorridos_tomtom <- function(coordenadas,key){
  resp_df_competo <- data.frame()
  url_part_1 <- "https://api.tomtom.com/routing/1/calculateRoute/" 
  
  #url_part_2 <- "/json?routeType=fastest&traffic=true&travelMode="
  url_part_2 <- "/json?routeRepresentation=polyline&computeTravelTimeFor=all&routeType=fastest&traffic=true&travelMode=" 
  mode= modo
  url_part_3="&vehicleEngineType=combustion&key="
  
  df_rbind <- data.frame()
  resp_df_competo <- data.frame()
  #coordinates <- coordenadas
  for(i in 1:nrow(coordinates )){
    #print(i)
    if (i %% 50 == 0) {
      print (i)
    }
    
    destination_lat <-coordinates$LAT_DESTINO[i]#coordinates[!coordinates$ID_distrit == origin_id,]
    origin_lat <- coordinates$LAT_ORIGEN[i]#coordinates[coordinates$ID_distrit == origin_id,]
    destination_long <-coordinates$LONG_DESTINO[i]#coordinates[!coordinates$ID_distrit == origin_id,]
    origin_long <- coordinates$LONG_ORIGEN[i]
    nombre_origen <- coordinates$DPTO_ORIGEN[i]
    nombre_destino <- coordinates$DPTO_DESTINO[i]
    id_origen <- coordinates$id_origen[i]
    id_destino <- coordinates$id_destino[i]
    
    if (id_origen < 10){
      id_origen <- paste("0",id_origen ,sep="")
    }else{
      id_origen  <- id_origen 
    }
    
    if (id_destino < 10){
      id_destino <- paste("0",id_destino ,sep="")
    }else{
      id_destino  <- id_destino
    }
    id_trayecto <- paste (id_origen,"-",id_destino,sep="")
    
  # ACA CAMBIA KEY 1 - 2
    url<- paste0(url_part_1,origin_lat,"%2C",origin_long,"%3A",destination_lat,"%2C",destination_long,url_part_2,modo,url_part_3,key)
    
    response <- GET(url)
    
    resp_json <- fromJSON(content(response, as = "text"))
    
    
    
    resp <- data.frame(travelTimeInSeconds = resp_json[["routes"]][["summary"]][["travelTimeInSeconds"]],
                      lengthInMeters = resp_json[["routes"]][["summary"]][["lengthInMeters"]],
                      long = resp_json[["routes"]][["legs"]][[1]][["points"]][[1]][["longitude"]],
                      lat = resp_json[["routes"]][["legs"]][[1]][["points"]][[1]][["latitude"]],
                      departureTime= resp_json[["routes"]][["summary"]][["departureTime"]],
                      arrivalTime= resp_json[["routes"]][["summary"]][["arrivalTime"]],
                      trafficDelayInSeconds=resp_json[["routes"]][["summary"]][["trafficDelayInSeconds"]],
                      trafficLengthInMeters=resp_json[["routes"]][["summary"]][["trafficLengthInMeters"]],
                      travelMode=resp_json[["routes"]][["sections"]][[1]][["travelMode"]][1],
                      historicTrafficTravelTimeInSeconds=resp_json[["routes"]][["summary"]][["historicTrafficTravelTimeInSeconds"]])
  
  
    resp_df <- data.frame(travelTimeInSeconds = resp$travelTimeInSeconds,
                          lengthInMeters=resp$lengthInMeters,
                          long =resp$long,
                          lat =resp$lat,
                          departureTime = resp$departureTime,
                          arrivalTime = resp$arrivalTime,
                          trafficDelayInSeconds = resp$trafficDelayInSeconds,
                          trafficLengthInMeters = resp$trafficLengthInMeters,
                          travelMode= resp$travelMode,
                          num = i,
                          nombre_origen=nombre_origen,
                          nombre_destino=nombre_destino,
                          id_origen = id_origen,
                          id_destino = id_destino,
                          id_trayecto = id_trayecto,
                          historicTrafficTravelTimeInSeconds= resp$historicTrafficTravelTimeInSeconds
                          )
        num_rows<-  nrow(resp_df)
        ID <- c(1:num_rows)
        data_frame_resp <- cbind(ID , resp_df)
        resp_df_competo <- rbind (resp_df_competo,data_frame_resp)
        #SI ES COLECTIVO SON OTROS NOMBRES
        names(resp_df_competo) <- c("ID","travelTimeInSeconds","lengthInMeters",       
                                    "long","lat","departureTime",        
                                    "arrivalTime","trafficDelayInSeconds", "trafficLengthInMeters",
                                    "travelMode","num","nombre_origen",        
                                    "nombre_destino","id_origen","id_destino",           
                                    "id_trayecto","historicTrafficTravelTimeInSeconds")
  }
  
      
  #return(resp_df_competo)
#}

recorrido_pie<- recorridos_tomtom (coordenadas=mapas,key=key_4)
setwd("D:/Josefina/Proyectos/salud/movilidad_6/procesamiento/recorridos/auto")
write.csv(resp_df_competo,"auto_prueba.csv")

