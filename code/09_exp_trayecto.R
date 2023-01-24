# Calculo de exposicion trayecto
# La funcion calcula: las horas
# Conc = es fija por dpto
# Tiempo de viaje depende del trayecto y del tipo de vehiculo
# Distancia del viaje depende del trayecto y del tipo de vehiculo
# La salida es un df con todos los trayectos

func_trayecto<- function(hs_viaje,concentracion,num_distrito){
  df_tot_dptos <- data.frame()
  # Hago un for 1-6 que es el n de los dptos
  for (p in 1:num_distrito){#67){
    print(p)
    df_tot <- data.frame()
    # Recorro el archivo de hs de viaje
    for (i in 1:nrow(hs_viaje)){
      if (i %% 200 == 0) {
        print (i)
      }
      
      # Nombre del dpto del trayecto (puede o no coicidir con dpto de origen/destino)
      # Nombre del dpto donde se encuentra actualmente
      nombre_dpto_tray <-  names(hs_viaje)[p+1]
      nombre_dpto_conc <-  concentracion[i,1]
      # Nombre del recorrido total origen - destino. Del trayecto original
      nombre_tray <-  hs_viaje[i,1] # [fila,ciolumna]
      # Concentracion de la concentracion del dpto del trayecto 
      # (puede o no coicidir con dpto de origen/destino)
      # p ==> 1 - 6 dptos filas 
      # contamianante es el numero de columna
      #conc_dpto_tray <- concentracion[p,contaminante+1]
      #Ahora p es el numero de columna que es el dpto
      conc_dpto_tray <- concentracion[i,p+1] 
      # Tiempo de viaje en el trayecto 
      
      # Lo multiplico x2 porque es la ida y la vuelta
      tiempo_viaje <- (hs_viaje[i,p+1]) * 2 #/60 por si lo pongo en hs

      
      # variables de control:
      # Coincidencia del dpto
      departamento_conc <- names(concentracion)[p+1]
      departamento_hs <- names(hs_viaje)[p+1]
      tipo_movilidad <- hs_viaje[i,ncol(hs_viaje)]#8]
      tipo_movilidad2 <- concentracion[i,ncol(hs_viaje)]
      # Calculo final
      #exp_trayecto <- conc_dpto_tray *  tiempo_viaje#* distancia_viaje 

      # Armo df de interes,

      df <- data.frame(nombre_dpto_tray,  nombre_tray,  conc_dpto_tray,   tiempo_viaje, departamento_conc,departamento_hs,tipo_movilidad,nombre_dpto_conc )
      names(df) <- c( "nombre_dpto_tray","nombre_tray", "conc_dpto_tray","tiempo_viaje", "dpto_conc_control","dpto_hs_control", "tipo_movilidad","nombre_dpto_conc")
      df_tot <- rbind(df_tot,df)
      names(df_tot) <- c("nombre_dpto_tray","nombre_tray","conc_dpto_tray","tiempo_viaje", "dpto_conc_control","dpto_hs_control","tipo_movilidad","nombre_dpto_conc" )
      
    }
    # Tengo NA de las combinaciones donde no hay datos
    # Elimino esos NA
    exp_tray <-  df_tot[complete.cases(df_tot$tiempo_viaje),]
    df_tot_dptos <- rbind(df_tot_dptos,exp_tray)
    names(df_tot_dptos) <- c( "nombre_dpto_tray","nombre_tray", "conc_dpto_tray","tiempo_viaje", "dpto_conc_control","dpto_hs_control","tipo_movilidad","nombre_dpto_conc" )
    
    
    }
  
  
  return(df_tot_dptos)
    
}

#########################################################3
#VARIABLES
conc_trayecto<- conc_trayecto
conc_trayecto$tipo <- "pie"
concentracion
#concentracion <- conc_trayecto
hs_viaje <- hs_viaje
prueba_tray <- func_trayecto(hs_viaje,concentracion=conc_trayecto,num_distrito = 6)

# ################# GUARDAMOS
# # PIE
# write.csv(prueba_tray,"D:/Josefina/Proyectos/salud/movilidad_3/procesamiento/pie/salidas/salida_trayecto.csv")

################# GUARDAMOS
# AUTO
#write.csv(prueba_tray,"D:/Josefina/Proyectos/salud/movilidad_3/procesamiento/auto/salida/salida_trayecto.csv")

################# GUARDAMOS
# BICICLETA
#write.csv(prueba_tray,"D:/Josefina/Proyectos/salud/movilidad_3/procesamiento/bicicletas/salidas/salida_trayecto.csv")


################# GUARDAMOS
# CO¿OLECTIVOS
write.csv(prueba_tray,"D:/Josefina/Proyectos/salud/movilidad_3/procesamiento/colectivos/salidas/salida_trayecto.csv")


################# GUARDAMOS
# DEPARTAMENTOS
write.csv(prueba_tray,"D:/Josefina/Proyectos/salud/movilidad_5/salidas/pie/salida_trayecto.csv")

########################################################################
#procesamiento previo de Trayecto para que no sea tan largo el archivo!
# EL TIEMPO ESTA EN SEGUNDOS Y LA DISTANCIA EN METROS
# al tiempo lo pasamos en minutos
# # PIE
# dato_trayecto_p <- read.csv("D:/Josefina/Proyectos/salud/movilidad_3/procesamiento/pie/salidas/salida_trayecto.csv")
# ########################################################################
# AUTO
#dato_trayecto_p <- read.csv("D:/Josefina/Proyectos/salud/movilidad_3/procesamiento/auto/salida/salida_trayecto.csv")

# BICICLETA
#dato_trayecto_p <- read.csv("D:/Josefina/Proyectos/salud/movilidad_3/procesamiento/bicicletas/salidas/salida_trayecto.csv")
# BICICLETA
dato_trayecto_p <- read.csv("D:/Josefina/Proyectos/salud/movilidad_3/procesamiento/colectivos/salidas/salida_trayecto.csv")

dato_trayecto_p <- prueba_tray


dato_trayecto_p $tiempo_viaje_min <- dato_trayecto_p $tiempo_viaje / 60
dato_trayecto_p$exp_trayecto <- dato_trayecto_p$tiempo_viaje_min*dato_trayecto_p$conc_dpto_tray
dato_trayecto_p %>%
  group_by(nombre_tray) %>%  #tipo_transp
  group_split() -> data_total
exp_total <- data.frame()

for (i in 1:length(data_total)){
  if(i %%100==0){
    print(i)
  }
  
  # Nombre deptos interes
  trayecto <- data_total[[i]][["nombre_tray"]][1]
  conc_prom <- mean(data_total[[i]][["conc_dpto_tray"]])
  exp_prom <- mean(data_total[[i]][["exp_trayecto"]]) #por min
  tiempo_viaje_seg <- sum(data_total[[i]][["tiempo_viaje"]])
  tiempo_viaje_min <- sum(data_total[[i]][["tiempo_viaje_min"]])
  tipo_movilidad<- data_total[[i]][["tipo_movilidad"]][1]
  num_distritos <- length(data_total[[i]][["nombre_dpto_tray"]])

  df <- data.frame(trayecto,  exp_prom,tiempo_viaje_seg, tiempo_viaje_min,tipo_movilidad,num_distritos,conc_prom)
  names(df) <- c("trayecto",  "exp_prom","tiempo_viaje_seg", "tiempo_viaje_min","tipo_movilidad","num_distritos","conc_prom")
  exp_total<- rbind(exp_total,df)
  names(exp_total) <- c("trayecto",  "exp_prom","tiempo_viaje_seg", "tiempo_viaje_min","tipo_movilidad","num_distritos","conc_prom")
}


# #PIE
# setwd("D:/Josefina/Proyectos/salud/movilidad_3/procesamiento/pie/salidas")
# write.csv(exp_total,"prueba_trayecto.csv")

# #AUTO
# setwd("D:/Josefina/Proyectos/salud/movilidad_3/procesamiento/auto/salida")
# write.csv(exp_total,"prueba_trayecto.csv")
# 
# 
# #BICICLETA
# setwd("D:/Josefina/Proyectos/salud/movilidad_3/procesamiento/bicicletas/salidas")
# write.csv(exp_total,"prueba_trayecto.csv")


#COLECTIVO
setwd("D:/Josefina/Proyectos/salud/movilidad_3/procesamiento/colectivo/salidas")
write.csv(exp_total,"prueba_trayecto.csv")



#DEPARTAMENTO
setwd("D:/Josefina/Proyectos/salud/movilidad_5/salidas/pie")
write.csv(exp_total,"prueba_trayecto.csv")

