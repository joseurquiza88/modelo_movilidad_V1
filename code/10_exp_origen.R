# Variables


# codigo de exp origen modificado

##################################################################
# ####################          PIE      ########################
##################################################################
setwd("D:/Josefina/Proyectos/salud/movilidad_3/procesamiento/pie/salidas")

dato_trayecto <- read.csv("prueba_trayecto.csv")
dato_destino <- read.csv("salida_destino.csv")
dato_destino <-dato_destino[2:10]
#dato_trayecto<-dato_trayecto[2:6]
dato_trayecto <- data.frame(dato_trayecto[2:3],dato_trayecto[5:7])


##################################################################
# ####################          AUTO      ########################
##################################################################
setwd("D:/Josefina/Proyectos/salud/movilidad_3/procesamiento/auto/salida")

dato_trayecto <- read.csv("prueba_trayecto.csv")
dato_destino <- read.csv("salida_destino.csv")
dato_destino <-dato_destino[2:10]
#dato_trayecto<-dato_trayecto[2:6]
dato_trayecto <- data.frame(dato_trayecto[2:3],dato_trayecto[5:7])



##################################################################
# ####################           BICICLETA     ########################
##################################################################
setwd("D:/Josefina/Proyectos/salud/movilidad_3/procesamiento/bicicletas/salidas")

dato_trayecto <- read.csv("prueba_trayecto.csv")
dato_destino <- read.csv("salida_destino.csv")
dato_destino <-dato_destino[2:10]
#dato_trayecto<-dato_trayecto[2:6]
dato_trayecto <- data.frame(dato_trayecto[2:3],dato_trayecto[5:7])



##################################################################
# ####################           COLECTIVOS     ########################
##################################################################
setwd("D:/Josefina/Proyectos/salud/movilidad_3/procesamiento/colectivos/salidas")

dato_trayecto <- read.csv("prueba_trayecto.csv")
dato_destino <- read.csv("salida_destino.csv")
dato_destino <-dato_destino[2:10]
#dato_trayecto<-dato_trayecto[2:6]
dato_trayecto <- data.frame(dato_trayecto[2:3],dato_trayecto[5:7])
# Al tryecto le tenemos que sacar las horas de min - seg
#lo dejamos en mins

##################################################################
# ####################           DEPARTAMENTO     ########################
##################################################################
setwd("D:/Josefina/Proyectos/salud/movilidad_5/salidas/pie")

dato_trayecto <- read.csv("prueba_trayecto.csv")
dato_destino <- read.csv("salida_destino.csv")
dato_destino <-dato_destino[2:8]
#dato_trayecto<-dato_trayecto[2:6]
dato_trayecto <- data.frame(dato_trayecto[2:3],dato_trayecto[5:7])
# Al tryecto le tenemos que sacar las horas de min - seg
#lo dejamos en mins

dato_origen <- data.frame()

for (i in 1: nrow(dato_trayecto)){ 
  if (i %% 100 == 0) {
    print (i)
  }
  
  tabla_trayecto <- dato_destino
  eq_trayecto <- which( dato_destino$trayecto == dato_trayecto[i,]$trayecto)
  
  tabla_trayecto <- tabla_trayecto[eq_trayecto,] 
  salida <- data.frame(tabla_trayecto,dato_trayecto[i,]) #"actividad_p" "transporte_p",
  names(salida) <- c("nombre_origen","nombre_dpto_destino" ,"indice_dpto","concentracion_destino",
                     "hora_actividad","trayecto" ,  "actividad_hs",
                     "trayecto","exp_prom_tray", "tiempo_viaje","tipo_movilidad","num_distritos")
  dato_origen <- rbind(dato_origen,salida)
  names(dato_origen ) <- c("nombre_origen","nombre_dpto_destino" ,"indice_dpto","concentracion_destino",
                     "hora_actividad","trayecto" ,  "actividad_hs",
                     "trayecto","exp_prom_tray", "tiempo_viaje","tipo_movilidad","num_distritos")
  
}
dato_origen$exp_prom_tray_tot <- dato_origen$exp_prom_tray#dato_origen$transporte_p * dato_origen$exp_prom_tray

############################################################################
# ponemos las concentraciones de origen SON SIEMPRE LAS MISMAS PEERO
# LAS DIVIDIMOS EN TIPO DE VEH

# PIE
#concentracion_origen <- read.csv("D:/Josefina/Proyectos/salud/movilidad_3/procesamiento/pie/variables_ingreso_model/concentracion_destino.csv")
# DEPARTAMENTO
concentracion_origen <- read.csv("D:/Josefina/Proyectos/salud/movilidad_5/procesamiento/concentraciones/concentracion_destino_tot.csv")

dato_origen_salida<- data.frame()
#for (i in 1: nrow(concentracion_origen)){ 
for (i in 1: nrow(dato_origen)){ 
  if (i %% 50 == 0) {
    print (i)
  }
  
  #tabla_trayecto <- dato_origen
  tabla_trayecto <- concentracion_origen 
  #eq_trayecto <- which( tabla_trayecto$nombre_origen == concentracion_origen[i,]$id)
  eq_trayecto <- which(tabla_trayecto$id == dato_origen[i,]$nombre_origen )
  tabla_trayecto <- tabla_trayecto[eq_trayecto,] 
  PMDIARIO_pixel <- concentracion_origen[i,]$PMDIARIO_pixel
  PMDIARIO_depto <- concentracion_origen[i,]$PMDIARIO_depto
  POBLXGRI_sum_dept <- concentracion_origen[i,]$POBLXGRI_sum_dept
  POBLXGRI_pixel <- concentracion_origen[i,]$POBLXGRI_pixel
  
  
  hs_origen <- 1440 
  dim_trayecto <- dim(tabla_trayecto)
  if (dim_trayecto[1] == 0){
    salida <- data.frame(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
    
    # names(salida) <- c( "nombre_origen", "nombre_dpto_destino","indice_dpto" ,         
    #                     "concentracion_destino" ,"hora_actividad", "trayecto" ,            
    #                    "actividad_p" , "actividad_hs","transporte_p" , "trayecto" ,            
    #                    "exp_tray" ,"tiempo_viaje", "tipo_movilidad"    ,   
    #                    "num_distritos","exp_prom_tray_tot","conc_origen" )
    names(salida) <- c( "nombre_origen", "nombre_dpto_destino","indice_dpto" ,         
                        "concentracion_destino" ,"hora_actividad", "trayecto" ,            
                        "actividad_hs","trayecto" ,            
                        "exp_tray" ,"tiempo_viaje", "tipo_movilidad"    ,   
                        "num_distritos","exp_prom_tray_tot" ,"PMDIARIO_pixel","PMDIARIO_depto",       
                        "POBLXGRI_pixel","POBLXGRI_sum_dept")
   
  }else{
    # salida <- data.frame(tabla_trayecto, concentraciones)
    # names(salida) <- c( "nombre_origen", "nombre_dpto_destino","indice_dpto" ,         
    #                     "concentracion_destino" ,"hora_actividad", "trayecto" ,            
    #                     "actividad_p" , "actividad_hs","transporte_p" , "trayecto" ,            
    #                     "exp_tray" ,"tiempo_viaje", "tipo_movilidad"    ,   
    #                     "num_distritos","exp_prom_tray_tot" ,"conc_origen")
    #salida <- data.frame(tabla_trayecto, PMDIARIO_pixel,PMDIARIO_depto,POBLXGRI_pixel,POBLXGRI_sum_dept)
    salida <- data.frame( dato_origen[i,],tabla_trayecto[,4:7])
    
    names(salida) <- c( "nombre_origen", "nombre_dpto_destino","indice_dpto" ,         
                        "concentracion_destino" ,"hora_actividad", "trayecto" ,            
                        "actividad_hs","trayecto" ,            
                        "exp_tray" ,"tiempo_viaje", "tipo_movilidad"    ,   
                        "num_distritos","exp_prom_tray_tot" ,"PMDIARIO_pixel","PMDIARIO_depto",       
                         "POBLXGRI_pixel","POBLXGRI_sum_dept")
      
    }
  dato_origen_salida <- rbind(dato_origen_salida,salida)

  names(dato_origen_salida) <- c( "nombre_origen", "nombre_dpto_destino","indice_dpto" ,         
                                  "concentracion_destino" ,"hora_actividad", "trayecto" ,            
                                  "actividad_hs","trayecto" ,            
                                  "exp_tray" ,"tiempo_viaje", "tipo_movilidad"    ,   
                                  "num_distritos","exp_prom_tray_tot" ,"PMDIARIO_pixel","PMDIARIO_depto",       
                                  "POBLXGRI_pixel","POBLXGRI_sum_dept")
  
}

####################################################################################
# TEMA HORAS-MINUNTOS
# LOS RECORRIDOS ESTABAN EN SEG Y LOS PASAMOS EN MINS
# ojo horas actividad esta en horas y traeycto en mins. Transformamos a mins

dato_origen_salida$hora_actividad <- dato_origen_salida$hora_actividad * 60

# EN EL DIA HAY 1440 MINS
dato_origen_salida$hs_origen <- 1440-(dato_origen_salida$hora_actividad+dato_origen_salida$tiempo_viaje)
# lo pasamos a hora /60
dato_origen_salida$exp_destino <- (dato_origen_salida$indice_dpto *dato_origen_salida$hora_actividad * dato_origen_salida$concentracion_destino)/60
#dato_origen_salida$exp_origen <- (dato_origen_salida$hs_origen * dato_origen_salida$conc_origen)/60
dato_origen_salida$exp_origen_pixel <- (dato_origen_salida$hs_origen * dato_origen_salida$PMDIARIO_pixel)/60
dato_origen_salida$exp_origen_dpto <- (dato_origen_salida$hs_origen * dato_origen_salida$PMDIARIO_depto)/60


dato_origen_salida$exp_tray <- dato_origen_salida$exp_tray/60
# EXPOSICION POR MINUTO
#dato_origen_salida$exp_total <- dato_origen_salida$exp_destino + dato_origen_salida$exp_origen +dato_origen_salida$exp_tray 
dato_origen_salida$exp_total_pixel <- dato_origen_salida$exp_destino + dato_origen_salida$exp_origen_pixel +dato_origen_salida$exp_tray 
dato_origen_salida$exp_total_depto <- dato_origen_salida$exp_destino + dato_origen_salida$exp_origen_dpto +dato_origen_salida$exp_tray 

# EXPOSICION TOTAL MEDIA HORARIA
#dato_origen_salida$exp_total_hs <- (dato_origen_salida$exp_total/24)#(dato_origen_salida$exp_total*60)/1440
dato_origen_salida$exp_total_hs_pixel <- (dato_origen_salida$exp_total_pixel/24)
dato_origen_salida$exp_total_hs_depto <- (dato_origen_salida$exp_total_depto/24)

####################################################################################
#GUARDAMOS
# # PIE
# setwd("D:/Josefina/Proyectos/salud/movilidad_3/procesamiento/pie/salidas")
# 
# write.csv(dato_origen_salida,"salida_origen.csv")


# AUTO
# setwd("D:/Josefina/Proyectos/salud/movilidad_3/procesamiento/auto/salida")
# 
# write.csv(dato_origen_salida,"salida_origen.csv")


# BICICLETA
setwd("D:/Josefina/Proyectos/salud/movilidad_3/procesamiento/pie/salidas")

write.csv(dato_origen_salida,"salida_origen.csv")




# COLECTIVO
setwd("D:/Josefina/Proyectos/salud/movilidad_3/procesamiento/colectivos/salidas")

write.csv(dato_origen_salida,"salida_origen.csv")


# DEPARTAMENTO
setwd("D:/Josefina/Proyectos/salud/movilidad_5/salidas/pie")

write.csv(dato_origen_salida,"salida_origen.csv")

