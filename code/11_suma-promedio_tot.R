

           ## --- Suma-promedio segun el punto de origen --- ##

# Ejemplos 
################################################################################
#                                        PIE
################################################################################

setwd("D:/Josefina/Proyectos/salud/movilidad_3/procesamiento/pie/salidas")
dato_origen<- read.csv("salida_origen.csv")
dato_origen<- dato_origen[complete.cases(dato_origen$nombre_origen),]


################################################################################
#                                        AUTO
################################################################################

setwd("D:/Josefina/Proyectos/salud/movilidad_3/procesamiento/auto/salida")
dato_origen<- read.csv("salida_origen.csv")
dato_origen<- dato_origen[complete.cases(dato_origen$nombre_origen),]


################################################################################
#                                       BICICLETAS
################################################################################

setwd("D:/Josefina/Proyectos/salud/movilidad_3/procesamiento/bicicletas/salidas")
dato_origen<- read.csv("salida_origen.csv")
dato_origen<- dato_origen[complete.cases(dato_origen$nombre_origen),]


################################################################################
#                                       COLECTIVOS
################################################################################

setwd("D:/Josefina/Proyectos/salud/movilidad_3/procesamiento/colectivos/salidas")
dato_origen<- read.csv("salida_origen.csv")
dato_origen<- dato_origen[complete.cases(dato_origen$nombre_origen),]


################################################################################
#                                       DEPARTMANTOS
################################################################################

setwd("D:/Josefina/Proyectos/salud/movilidad_5/salidas/pie")
dato_origen<- read.csv("salida_origen.csv")
dato_origen<- dato_origen[complete.cases(dato_origen$nombre_origen),]

## Agrupamos los datos obtenidos segun el punto de origen y calculamos las
# horas que pasan en el hogar.
# Hs en el hogar = 24 hs - (hs en trayecto+hs realizando alguna actividad)

dato_origen %>%
  group_by(nombre_origen) %>%  
  group_split() -> data_total

df_total <- data.frame()

for (i in 1:length(data_total)){
  # if(i %%100==0){
  #   print(i)
  # }
  print(i)
  # Nombre deptos interes

  dpto_origen<- data_total[[i]][["nombre_origen"]][1]
  exp_tray <- mean(data_total[[i]][["exp_tray"]],na.rm=T)
  exp_destino <- mean(data_total[[i]][["exp_destino"]],na.rm=T)
  exp_origen_pixel <- mean(data_total[[i]][["exp_origen_pixel"]],na.rm=T)
  exp_origen_depto <- mean(data_total[[i]][["exp_origen_dpto"]],na.rm=T)
  
  exp_total_pixel <- mean(data_total[[i]][["exp_total_pixel"]],na.rm=T)
  exp_total_depto <- mean(data_total[[i]][["exp_total_depto"]],na.rm=T)
  exp_total_hora_pixel <- mean(data_total[[i]][["exp_total_hs_pixel"]],na.rm=T)
  exp_total_hora_depto <- mean(data_total[[i]][["exp_total_hs_depto"]],na.rm=T)
  
  df <- data.frame(dpto_origen,exp_tray,exp_destino,exp_origen_pixel,exp_origen_depto ,
                   exp_total_pixel,exp_total_depto,exp_total_hora_pixel,exp_total_hora_depto)
  
  names(df) <- c("dpto_origen","exp_tray","exp_destino","exp_origen_pixel","exp_origen_depto",
                 "exp_total_pixel","exp_total_depto","exp_total_hora_pixel","exp_total_hora_depto")        
  df_total <- rbind(df_total ,df)
  names(df_total ) <- c("dpto_origen","exp_tray","exp_destino","exp_origen_pixel","exp_origen_depto",
                        "exp_total_pixel","exp_total_depto","exp_total_hora_pixel","exp_total_hora_depto")        
}

############################################################################

#Unimos datos con los de direcciones

#dato_direcciones <- read.csv("D:/Josefina/Proyectos/salud/movilidad_3/procesamiento/auto/productos_final/direcciones_comp.csv")
dato_direcciones<- read.csv("D:/Josefina/Proyectos/salud/movilidad_5/procesamiento/datos_entrada/coordenadas/puntos.csv", sep=",")
dato_direcciones$id <- dato_direcciones$ID
dato_origen_salida<- data.frame()
for (i in 1: nrow(dato_direcciones)){ 
  if (i %% 50 == 0) {
    print (i)
  }
  
  tabla_trayecto <- df_total 
  eq_trayecto <- which( tabla_trayecto$dpto_origen == dato_direcciones[i,]$id)
  
  tabla_trayecto <- tabla_trayecto[eq_trayecto,] 
  dpto<- dato_direcciones[i,]$dept
  #distrito<- dato_direcciones[i,]$dis
  distrito<- dato_direcciones[i,]$punto
  lat<- dato_direcciones[i,]$LAT
  long<- dato_direcciones[i,]$LONG
  
  dim_tabla <- dim(tabla_trayecto )
  
  if(dim_tabla[1] == 0){
    salida <- data.frame(NA, NA, NA, NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)   
    names(salida) <- c( "dpto_origen","exp_tray","exp_destino","exp_origen_pixel",
                        "exp_origen_depto","exp_total_pixel" ,"exp_total_depto",
                        "exp_total_hora_pixel",
                        "exp_total_hora_depto", "dpto","distrito", "lat" ,"long")
    
  }else{ 
  
  salida <- data.frame(tabla_trayecto, dpto,distrito,lat,long)
  # names(salida) <- c( "dpto_origen","exp_tray" ,"exp_destino" ,"exp_origen" ,   
  #                     "exp_total","exp_total_hora" ,"dpto", "distrito" ,  
  #                     "lat","long")
  names(salida) <- c( "dpto_origen","exp_tray","exp_destino","exp_origen_pixel",
                       "exp_origen_depto","exp_total_pixel" ,"exp_total_depto",
                       "exp_total_hora_pixel",
                       "exp_total_hora_depto", "dpto","distrito", "lat" ,"long")
                      }
  dato_origen_salida <- rbind(dato_origen_salida,salida)
  names(dato_origen_salida) <- c( "dpto_origen","exp_tray","exp_destino","exp_origen_pixel",
                                  "exp_origen_depto","exp_total_pixel" ,"exp_total_depto",
                                  "exp_total_hora_pixel",
                                  "exp_total_hora_depto", "dpto","distrito", "lat" ,"long")  
}


############################################################################

#Unimos datos con las concentraciones de origen

#dato_concentraciones <- read.csv("D:/Josefina/Proyectos/salud/movilidad_3/procesamiento/auto/variables_ingreso_modelo/concentracion_destino.csv")
dato_concentraciones <- read.csv("D:/Josefina/Proyectos/salud/movilidad_5/procesamiento/concentraciones/concentracion_destino_tot.csv")

dato_origen_concentraciones<- data.frame()
for (i in 1: nrow(dato_concentraciones)){ 
  if (i %% 50 == 0) {
    print (i)
  }
  
  tabla_trayecto <- dato_origen_salida 
  eq_trayecto <- which( tabla_trayecto$dpto_origen == dato_concentraciones[i,]$id)
  
  tabla_trayecto <- tabla_trayecto[eq_trayecto,] 
  #concentraciones<- dato_concentraciones[i,]$PMDIARIO
  PMDIARIO_pixel<- dato_concentraciones[i,]$PMDIARIO_pixel
  PMDIARIO_depto<- dato_concentraciones[i,]$PMDIARIO_depto
  POBLXGRI_sum_dept<- dato_concentraciones[i,]$POBLXGRI_sum_dept
  POBLXGRI_pixel<- dato_concentraciones[i,]$POBLXGRI_pixel
  
  
  dim_tabla <- dim(tabla_trayecto )
  
  if(dim_tabla[1] == 0){
    salida <- data.frame(NA, NA, NA, NA,NA,NA,NA,NA,NA,NA,NA)   
    names(salida) <- c( "dpto_origen","exp_tray" ,"exp_destino" ,"exp_origen" ,   
                        "exp_total","exp_total_hora" ,"dpto", "distrito" ,  
                        "lat","long","concentraciones")
    
  }else{ 

  
  salida <- data.frame(tabla_trayecto, PMDIARIO_pixel,PMDIARIO_depto,POBLXGRI_sum_dept,POBLXGRI_pixel)
  names(salida) <- c( "dpto_origen","exp_tray" ,"exp_destino" ,"exp_origen_pixel",
                      "exp_origen_depto","exp_total_pixel", "exp_total_depto","exp_total_hora_pixel",
                      "exp_total_hora_depto","dpto", "distrito","lat" ,"long","PMDIARIO_pixel", "PMDIARIO_depto",      
                       "POBLXGRI_sum_dept","POBLXGRI_pixel" )
  }
  dato_origen_concentraciones<- rbind(dato_origen_concentraciones,salida)
  names(dato_origen_concentraciones) <- c( "dpto_origen","exp_tray" ,"exp_destino" ,"exp_origen_pixel",
                                           "exp_origen_depto","exp_total_pixel", "exp_total_depto","exp_total_hora_pixel",
                                           "exp_total_hora_depto","dpto", "distrito","lat" ,"long","PMDIARIO_pixel", "PMDIARIO_depto",      
                                           "POBLXGRI_sum_dept","POBLXGRI_pixel" ) 
}
############################################################################

##########################################################################
# Guardamos
getwd()

# #pie
# write.csv(dato_origen_concentraciones ,"salida_final_pie.csv")


#AUTO
#write.csv(dato_origen_concentraciones ,"salida_final_auto.csv")

#BICICLETAS
# write.csv(dato_origen_concentraciones ,"salida_final_bicicletas.csv")

#COLECTIVOS
#write.csv(dato_origen_concentraciones ,"salida_final_colectivos.csv")

write.csv(dato_origen_concentraciones ,"salida_final_pie.csv")
