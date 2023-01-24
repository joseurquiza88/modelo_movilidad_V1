# CALCULO EXPOSICION DESTINO: 
# En funcion de la actividad y del numero de personas de otros dptos

# Esta funcion calcula la exposicion de destin
# Las variables de ingreso son:
# - dpto: Depto de destino CAP = 1
# - Indice: Cuantas personas van desde cualquier dpto hacia el dpto de destino
# (Ver si esto hay que hacerlo segun actividades)
# - Conc: Concentracion del dpto original que se va a re-asignar al dpto destino
# - hs_actividades: hs que le asignamos a cada actividad


func_destino<- function(indice,concentracion,hs_actividades,contaminante=3){
  df_tot_dptos <- data.frame()
  # Hago un for 1-6 que es el n de los dptos
  # Hago un for 1-67 que es el n de los distritos
  for (p in 1:nrow(concentracion)){#67
    print(p)
    df_tot <- data.frame()
    # Hago un for que recorra todo el indice p 
    #(relacion de cuantas personas llegan al dpto de destino segun actividad)
    
    for (i in 1:(length(indice)-1)){
      
      # Recorro el archivo de las hs de actividades (en min)
      # son 4 datos: Recreac, Trabajo, Escuela, Otros
      for (j in 1:length(hs_actividades[,1])){ 
        
        # Tomo del indice p todos los valores que corresponda a la actividad en cuestion j
        # por ejemplo j=1 es trabajo
        dato <- indice[(indice$actividades == j),]#[(indice$num_act == j),]
        #dato_transp <- indice_transp[(indice_transp$num_act == 2),]

        # Del DF anterior tomo el dato corresponden al dpto de origen - destino de interes
        indice_dpto <- dato[i,(p+1)]
        #indice_dpto_transp <- dato_transp[i,(p+1)]
        
        # La concentracion es del depto destino

        conc<- concentracion[p,contaminante+1]
        # Nombres de los dptos origen destino
        nombre_dpto_destino <- names(dato)[p+1]
        nombre_origen <- dato[i,1]
        
        #Nombre del trayecto
        trayecto <- paste(nombre_origen ,"-",nombre_dpto_destino,sep = "")
     
        # Actividad
        #[filas,columnas]
        actividad_p <- dato[i,ncol(dato)]
        #transporte_p <- dato_transp[i,ncol(dato_transp)]
        actividad_hs <- hs_actividades[j,2]
        
        # Hago un df con los datos de interes
        df <- data.frame(nombre_origen, nombre_dpto_destino,indice_dpto ,conc, (hs_actividades[j,2]/60),trayecto,actividad_hs)#,indice_dpto_transp )actividad_p,
        names(df) <- c("nombre_origen", "nombre_dpto_destino","indice_dpto", "concentracion_destino","hora_actividad","trayecto","actividad_hs")#,"transporte_p")"actividad_p",
        # Junto todo
        
        df_tot <- rbind(df_tot,df)
        names(df_tot) <- c("nombre_origen", "nombre_dpto_destino","indice_dpto", "concentracion_destino","hora_actividad","trayecto","actividad_hs")#,"transporte_p")"actividad_p",
        df_tot <- df_tot[complete.cases(df_tot$indice_dpto),]
        
        
        
        
      }
    }
    df_tot_dptos <- rbind(df_tot_dptos,df_tot)
    names(df_tot_dptos) <- c("nombre_origen","nombre_dpto_destino","indice_dpto","concentracion_destino","hora_actividad","trayecto","actividad_hs")#,"transporte_p")"actividad_p",
    
    
  }
  return(df_tot_dptos)
}
################################################################################
# VARIABLES 
indice_act <- indice
indice_transp <- indice_transp
concentracion_trayecto <- conc_trayecto
concentracion <- concentracion
hs_actividades <- read.csv("tiempo_actividades.csv")
prueba <- func_destino(indice_act,concentracion,hs_actividades,contaminante=3)

# ##################################
# #GUARDAMOS
# #PIE
# write.csv(prueba,"D:/Josefina/Proyectos/salud/movilidad_3/procesamiento/pie/salidas/salida_destino.csv")


# ##################################
# #GUARDAMOSB
# #AUTO
# write.csv(prueba,"D:/Josefina/Proyectos/salud/movilidad_3/procesamiento/auto/salida/salida_destino.csv")
# 
# 
##################################
#BICICLTAGUARDAMOS
#AUTO
#write.csv(prueba,"D:/Josefina/Proyectos/salud/movilidad_3/procesamiento/bicicletas/salidas/salida_destino.csv")


##################################
# COLECTIVOS
write.csv(prueba,"D:/Josefina/Proyectos/salud/movilidad_3/procesamiento/colectivos/salidas/salida_destino.csv")



###########
#######################
# DEPARTAMENTOS - AUTO
write.csv(prueba,"D:/Josefina/Proyectos/salud/movilidad_5/salidas/pie/salida_destino.csv")

