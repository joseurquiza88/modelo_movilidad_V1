#Con este codigo la idea es sacar los promedios y suma de las concentraciones
# a partir de los segmentos de la grilla
# CONCENTRACONES TRAYECTO
#################################################################################
#                           CONCENTRACONES TRAYECTO
#                                     PIE
################################################################################
setwd("D:/Josefina/Proyectos/salud/movilidad_3/procesamiento/pie/recorridos/recorrido_tot/shape/")

#pie
nombre_archivo <-"interseccion_grilla_pie_v2.csv"

interseccion_grilla_1 <- read.csv(nombre_archivo)

#################################################################################
#                           CONCENTRACONES TRAYECTO
#                                     AUTO
################################################################################
setwd("D:/Josefina/Proyectos/salud/movilidad_3/procesamiento/auto/recorridos/recorridos_tot/shape/tot")

#pie
nombre_archivo <-"interseccion_grilla_auto.csv"

interseccion_grilla  <- read.csv(nombre_archivo)



#################################################################################
#                           CONCENTRACONES TRAYECTO
#                                     BICICLETA
################################################################################
setwd("D:/Josefina/Proyectos/salud/movilidad_3/procesamiento/bicicletas/recorridos/recorridos_tot/shape/tot")

#pie
nombre_archivo <-"interseccion_grilla_bicicleta.shp"

interseccion_grilla  <- read.csv(nombre_archivo)

#################################################################################
#                           CONCENTRACONES TRAYECTO
#                                     COLECTIVO
################################################################################
setwd("D:/Josefina/Proyectos/salud/movilidad_3/procesamiento/colectivos/recorridos/recorridos_tot/shape/tot")
nombre_archivo <-"interseccion_grilla_colectivos.csv"


#################################################################################
#                           CONCENTRACONES TRAYECTO
#                                     DEPARTAMENTOS
################################################################################
setwd("D:/Josefina/Proyectos/salud/movilidad_5/procesamiento/recorridos/final/")
nombre_archivo <-"pie_grilla.shp"

interseccion_grilla  <- st_read(nombre_archivo)



# lo agrupo por el id del trayecto y por el id de distrito actual
interseccion_grilla%>%
  group_by(id_tryc,id_dstr) %>% 
  group_split() -> dataSplit

# Hago promedios y sumas de las concentraciones
# por los pixeles donde pasan los recorridos
# es decir donde interesecta la linea y el pixel

# OJO TARDA BASTANTE!!
suma_df <- data.frame()
for (i in 1:length(dataSplit)){
  if (i %% 1000==0){
    print (i)
  }
  
  id_tryc <- dataSplit[[i]][["id_tryc"]][1]
  len_tot <- dataSplit[[i]][["len_tot"]][1]
  
  DISTRIT <- dataSplit[[i]][["DISTRIT"]][1]
  id_dstr <- dataSplit[[i]][["id_dstr"]][1]
  # Del trayecto total
  ln_dstr<- dataSplit[[i]][["ln_dstr"]][1]
  #distancia total
  dst_tt_<- dataSplit[[i]][["dstnc_t"]][1]
  drtn_tt<- dataSplit[[i]][["drtn_tt"]][1]
  # dentro de cada distrito
  tiempo<- dataSplit[[i]][["duratin"]][1]
  distanc<- dataSplit[[i]][["distanc"]][1]

  POBLXGRI_sum<-sum(dataSplit[[i]][["POBLXGRI"]],na.rm = T)
  POBLXGRI_prom<-mean(dataSplit[[i]][["POBLXGRI"]],na.rm = T)
  PMDIARIO_sum <-sum(dataSplit[[i]][["PMDIARIO"]],na.rm = T)
  PMDIARIO_prom <-mean(dataSplit[[i]][["PMDIARIO"]],na.rm = T)
  len_pixeles <- length(dataSplit[[i]][["PMDIARIO"]])
  #df <- data.frame(id_tryc,len_tot,DISTRIT,id_dstr ,ln_dstr,dst_tt_,tiempo,dstnc_k,drtn_hs,distanc,POBLXGRI_sum,POBLXGRI_prom,PMDIARIO_sum,PMDIARIO_prom,len_pixeles )
  df <- data.frame(id_tryc,len_tot,DISTRIT,id_dstr ,ln_dstr,dst_tt_,drtn_tt,tiempo,distanc,POBLXGRI_sum,POBLXGRI_prom,PMDIARIO_sum,PMDIARIO_prom,len_pixeles )
  
  #names(df) <- c("id_tryc","len_tot","DISTRIT","id_dstr" ,"ln_dst","dst_tt_","tiempo","dstnc_k","drtn_hs","distanc","POBLXGRI_sum","POBLXGRI_prom","PMDIARIO_sum","PMDIARIO_prom","len_pixeles")
  names(df) <- c("id_tryc","len_tot","DISTRIT","id_dstr" ,"ln_dst","dst_tt_","drtn_tt","tiempo","distanc","POBLXGRI_sum","POBLXGRI_prom","PMDIARIO_sum","PMDIARIO_prom","len_pixeles")
  
  suma_df<- rbind(suma_df,df)
  #names(suma_df) <- c("id_tryc","len_tot","DISTRIT","id_dstr" ,"ln_dst","dst_tt_","tiempo","dstnc_k","drtn_hs","distanc","POBLXGRI_sum","POBLXGRI_prom","PMDIARIO_sum","PMDIARIO_prom","len_pixeles")
  names(suma_df) <- c("id_tryc","len_tot","DISTRIT","id_dstr" ,"ln_dst","dst_tt_","drtn_tt","tiempo","distanc","POBLXGRI_sum","POBLXGRI_prom","PMDIARIO_sum","PMDIARIO_prom","len_pixeles")
  
}

getwd()

############################################################
#ponerle el id a la columna distrito
suma_df$id_dstr<- as.numeric(suma_df$id_dstr)
for (i in 1:nrow(suma_df)){
  if (suma_df$id_dstr[i] < 10){
    suma_df$id_dstr2[i] <- paste("0",suma_df$id_dstr[i],sep="")
  }else{
    suma_df$id_dstr2[i] <- suma_df$id_dstr[i]
  }
  
}

#################################################################################
#                           CONCENTRACONES TRAYECTO
#                                     PIE
################################################################################
# GUARDAR
# # PIE
# write.csv(suma_df,"D:/Josefina/Proyectos/salud/movilidad_3/procesamiento/pie/concentraciones/concentracion_pm_pie_trayecto.csv")


#################################################################################
#                           CONCENTRACONES TRAYECTO
#                                     AUTO
################################################################################
# GUARDAR
# auto
#write.csv(suma_df,"D:/Josefina/Proyectos/salud/movilidad_3/procesamiento/auto/concentraciones/concentracion_pm_auto_trayecto.csv")


# #################################################################################
# #                           CONCENTRACONES TRAYECTO
# #                                     BICI
# ################################################################################
# # GUARDAR
# # auto
# write.csv(suma_df,"D:/Josefina/Proyectos/salud/movilidad_3/procesamiento/bicicletas/concentraciones/concentracion_pm_bicicleta_trayecto.csv")

#################################################################################
#                           CONCENTRACONES TRAYECTO
#                                     COLECTIVO
################################################################################
# GUARDAR
# auto
write.csv(suma_df,"D:/Josefina/Proyectos/salud/movilidad_3/procesamiento/colectivos/concentraciones/concentracion_pm_colectivos_trayecto.csv")

#################################################################################
#                           CONCENTRACONES TRAYECTO
#                                     departamento
################################################################################
# GUARDAR
#
nombre_archivo
write.csv(suma_df,"D:/Josefina/Proyectos/salud/movilidad_5/procesamiento/concentraciones/concentracion_pm_pie_trayecto.csv")

suma_df_completo<- read.csv("D:/Josefina/Proyectos/salud/movilidad_5/procesamiento/concentraciones/concentracion_pm_colectivos_trayecto.csv")
# pie menor 60 mins
#bici menor 90 mins
# colectivo mayor a 5mins y menor a 2 hs
suma_df <- suma_df_completo[suma_df_completo$drtn_tt<=7200,]
suma_df <- suma_df[suma_df$drtn_tt>=300,]

###########################################################################3###
#                             GRILLA DE  CONCENTRACIONES
#La idea es partir de una grilla vacia y despues se va pisando
###########################################################################3###
# matriz vacia
distritos <- read.csv("D:/Josefina/Proyectos/salud/movilidad_3/procesamiento/auto/productos_final/salida_pm.csv")

#PIE
# suma_df_completo <- read.csv("D:/Josefina/Proyectos/salud/movilidad_3/procesamiento/pie/concentraciones/concentracion_pm_pie_trayecto.csv")
# 
# # PARA LOS RECORRIDOS A PIE ELIMINAMOS LOS TRAYECTOS MAYORES A 1hs
# suma_df <- suma_df_completo[suma_df_completo$drtn_tt<=3600,]


# --------    AUTO     --------
#suma_df<- read.csv("D:/Josefina/Proyectos/salud/movilidad_3/procesamiento/auto/concentraciones/concentracion_pm_auto_trayecto.csv")
# --------    BICICLETA     --------
# suma_df_completo<- read.csv("D:/Josefina/Proyectos/salud/movilidad_3/procesamiento/bicicletas/concentraciones/concentracion_pm_bicicleta_trayecto.csv")
# #PARA LOS RECORRIDOS A PIE ELIMINAMOS LOS TRAYECTOS MAYORES A 1.30hs
# suma_df <- suma_df_completo[suma_df_completo$drtn_tt<=5400,]


# --------    COLECTIVOS     --------
suma_df_completo<- read.csv("D:/Josefina/Proyectos/salud/movilidad_3/procesamiento/colectivos/concentraciones/concentracion_pm_colectivos_trayecto.csv")


#PARA LOS RECORRIDOS A PIE ELIMINAMOS LOS TRAYECTOS MENORES A 05 MINS
suma_df <- suma_df_completo[suma_df_completo$drtn_tt>=300,]
#PARA LOS RECORRIDOS A PIE ELIMINAMOS LOS TRAYECTOS MAYORES A 2HS- 120MINS-
suma_df <- suma_df[suma_df$drtn_tt<=7200,]

#-- GRILLA vacia DPTOS
# corremos el 02.id la grilla asi quedan bien las columas
distritos <- grilla#read.csv("D:/Josefina/Proyectos/salud/movilidad_5/procesamiento/grilla/grilla.csv")
#suma_df <-read.csv("D:/Josefina/Proyectos/salud/movilidad_5/procesamiento/concentraciones/concentracion_pm_colectivos_trayecto.csv")

#ponerle el id a la columna distrito

for (i in 1:nrow(suma_df)){
  if (suma_df$id_dstr[i] < 10){
    suma_df$id_dstr2[i] <- paste("0",suma_df$id_dstr[i],sep="")
  }else{
    suma_df$id_dstr2[i] <- suma_df$id_dstr[i]
  }
  
}
suma_df$id_dstr <- suma_df$id_dstr2
# REVISAR LAS COLUMNAS ANTES DE HACER ESTO
names(distritos) <- c("distritos","01",	"02",	"03",	"04","05",	"06",	"07",	"08",	"09",	"10",
                      "11",	"12",	"13",	"14",	"15",	"16",	"17",	"18",	"19",	"20",	
                      "21",	"22",	"23",	"24",	"25",	"26",	"27",	"28",	"29",	"30",	
                      "31",	"32",	"33",	"34",	"35",	"36",	"37",	"38",	"39",	"40",
                      "41",	"42",	"43",	"44",	"45",	"46",	"47",	"48",	"49",	"50",	
                      "51",	"52",	"53",	"54",	"55",	"56",	"57",	"58",	"59",	"60",	
                      "61",	"62",	"63",	"64",	"65"	,"66","67")#,"tipo")

for (o in 1:nrow(distritos)){
  vec_origen<- which((suma_df$id_tryc) == (distritos$distrito[o]))
  if ((length(vec_origen))!=0){
    for (pos in 1:length(vec_origen)){
      
      pos_destino<-which(names(distritos)==suma_df$id_dstr2[vec_origen[pos]])
      distritos[o,pos_destino]<-suma_df$PMDIARIO_prom[vec_origen[pos]]
      #[filas,columnas]
    }
  }
}


#distritos$tipo <- "pie"
distritos$tipo <- "colectivo"
getwd()

# #pie
#write.csv(distritos,"D:/Josefina/Proyectos/salud/movilidad_3/procesamiento/pie/concentraciones/concentracion_pm_pie_grilla.csv")
# #AUTO
#write.csv(distritos,"D:/Josefina/Proyectos/salud/movilidad_3/procesamiento/auto/concentraciones/concentracion_pm_auto_grilla.csv")
# BICICLETAS
#write.csv(distritos,"D:/Josefina/Proyectos/salud/movilidad_3/procesamiento/bicicletas/concentraciones/concentracion_pm_bicicletas_grilla_cortado.csv")

# COLECTIVOS
write.csv(distritos,"D:/Josefina/Proyectos/salud/movilidad_3/procesamiento/colectivos/concentraciones/concentracion_pm_colectivos_grilla_cortado.csv")

# 
# #CARPETA VARIABLES A USAR####################################################3
#PIE
#write.csv(distritos,"D:/Josefina/Proyectos/salud/movilidad_3/procesamiento/pie/variables_ingreso_model/PMDIARIO_prom_trayecto_pie.csv")
# AUTO
#write.csv(distritos,"D:/Josefina/Proyectos/salud/movilidad_3/procesamiento/auto/variables_ingreso_modelo/PMDIARIO_prom_trayecto_auto.csv")

# BICICLETAS
#write.csv(distritos,"D:/Josefina/Proyectos/salud/movilidad_3/procesamiento/bicicletas/variables_ingreso_model/PMDIARIO_prom_trayecto_bicicletas.csv")


# COLECTIVOS
write.csv(distritos,"D:/Josefina/Proyectos/salud/movilidad_3/procesamiento/colectivos/variables_ingreso_model/PMDIARIO_prom_trayecto_colectivos.csv")

# PEURBA DEPARTAMENTO
nombre_archivo
write.csv(distritos,"D:/Josefina/Proyectos/salud/movilidad_5/procesamiento/concentraciones/PMDIARIO_prom_trayecto_colectivos.csv")

###########################################################################3###
#                             GRILLA DE  TIEMPO
#La idea es partir de una grilla vacia y despues se va pisando
###########################################################################3###
# matriz vacia
distritos <- read.csv("D:/Josefina/Proyectos/salud/movilidad_3/procesamiento/auto/productos_final/salida_pm.csv")
 
#########                              PIE          ###########################
# suma_df_completo <- read.csv("D:/Josefina/Proyectos/salud/movilidad_3/procesamiento/pie/concentraciones/concentracion_pm_pie_trayecto.csv")
# 
# # PARA LOS RECORRIDOS A PIE ELIMINAMOS LOS TRAYECTOS MAYORES A 1hs
# suma_df <- suma_df_completo[suma_df_completo$drtn_tt<=3600,]
# REVISAR LAS COLUMNAS ANTES DE HACER ESTO

# ##############                     AUTO          ###########################
# suma_df<- read.csv("D:/Josefina/Proyectos/salud/movilidad_3/procesamiento/auto/concentraciones/concentracion_pm_auto_trayecto.csv")

##############                     BICICLETA          ###########################
#suma_df2<- read.csv("D:/Josefina/Proyectos/salud/movilidad_3/procesamiento/bicicletas/concentraciones/concentracion_pm_bicicleta_trayecto_cortado.csv")
distritos<- grilla

for (i in 1:nrow(suma_df)){
  if (suma_df$id_dstr[i] < 10){
    suma_df$id_dstr2[i] <- paste("0",suma_df$id_dstr[i],sep="")
  }else{
    suma_df$id_dstr2[i] <- suma_df$id_dstr[i]
  }
  
}
suma_df$id_dstr <- suma_df$id_dstr2
names(distritos) <- c("distritos","01",	"02",	"03",	"04","05",	"06",	"07",	"08",	"09",	"10",
                      "11",	"12",	"13",	"14",	"15",	"16",	"17",	"18",	"19",	"20",	
                      "21",	"22",	"23",	"24",	"25",	"26",	"27",	"28",	"29",	"30",	
                      "31",	"32",	"33",	"34",	"35",	"36",	"37",	"38",	"39",	"40",
                      "41",	"42",	"43",	"44",	"45",	"46",	"47",	"48",	"49",	"50",	
                      "51",	"52",	"53",	"54",	"55",	"56",	"57",	"58",	"59",	"60",	
                      "61",	"62",	"63",	"64",	"65"	,"66","67")#,"tipo")

for (o in 1:nrow(distritos)){
  vec_origen<- which((suma_df$id_tryc) == (distritos$distrito[o]))
  if ((length(vec_origen))!=0){
    for (pos in 1:length(vec_origen)){
      
      pos_destino<-which(names(distritos)==suma_df$id_dstr2[vec_origen[pos]])
      distritos[o,pos_destino]<-suma_df$tiempo[vec_origen[pos]]
      #[filas,columnas]
    }
  }
}


#distritos$tipo <- "pie"
distritos$tipo <- "colectivo"
getwd()

# # #pie
# write.csv(distritos,"D:/Josefina/Proyectos/salud/movilidad_3/procesamiento/pie/tiempo/tiempo_pie.csv")
# # 
# # #CARPETA VARIABLES A USAR
# write.csv(distritos,"D:/Josefina/Proyectos/salud/movilidad_3/procesamiento/pie/variables_ingreso_model/PMDIARIO_prom_trayecto_pie.csv")

# ################ AUTO
# # # AUTO
# write.csv(distritos,"D:/Josefina/Proyectos/salud/movilidad_3/procesamiento/auto/tiempo/tiempo_auto.csv")
# # 
# # #CARPETA VARIABLES A USAR
# write.csv(distritos,"D:/Josefina/Proyectos/salud/movilidad_3/procesamiento/auto/variables_ingreso_modelo/tiempo_auto.csv")


# ################ BICICLETAS
# # # BICICLETAS
# write.csv(distritos,"D:/Josefina/Proyectos/salud/movilidad_3/procesamiento/bicicletas/tiempo/tiempo_bicicletas.csv")
# # 
# # #CARPETA VARIABLES A USAR
# write.csv(distritos,"D:/Josefina/Proyectos/salud/movilidad_3/procesamiento/bicicletas/variables_ingreso_model/tiempo_bicicletas.csv")

################ COLECTIVOS
# # COLECTIVOS
write.csv(distritos,"D:/Josefina/Proyectos/salud/movilidad_3/procesamiento/colectivos/tiempo/tiempo_colectivos.csv")
# 
# #CARPETA VARIABLES A USAR
write.csv(distritos,"D:/Josefina/Proyectos/salud/movilidad_3/procesamiento/colectivos/variables_ingreso_model/tiempo_colectivos.csv")


# #PRUEBA DEPTARTAMENTOS
nombre_archivo
write.csv(distritos,"D:/Josefina/Proyectos/salud/movilidad_5/procesamiento/tiempo/tiempo_colectivo.csv")

###########################################################################3###
#                             GRILLA DE  DISTANCIA
#La idea es partir de una grilla vacia y despues se va pisando
###########################################################################3###
# matriz vacia
distritos <- read.csv("D:/Josefina/Proyectos/salud/movilidad_3/procesamiento/auto/productos_final/salida_pm.csv")
suma_df_completo <- read.csv("D:/Josefina/Proyectos/salud/movilidad_3/procesamiento/pie/concentraciones/concentracion_pm_pie_trayecto.csv")

# PARA LOS RECORRIDOS A PIE ELIMINAMOS LOS TRAYECTOS MAYORES A 1hs
suma_df <- suma_df_completo[suma_df_completo$drtn_tt<=3600,]
# REVISAR LAS COLUMNAS ANTES DE HACER ESTO

names(distritos) <- c("distritos","01",	"02",	"03",	"04","05",	"06",	"07",	"08",	"09",	"10",
                      "11",	"12",	"13",	"14",	"15",	"16",	"17",	"18",	"19",	"20",	
                      "21",	"22",	"23",	"24",	"25",	"26",	"27",	"28",	"29",	"30",	
                      "31",	"32",	"33",	"34",	"35",	"36",	"37",	"38",	"39",	"40",
                      "41",	"42",	"43",	"44",	"45",	"46",	"47",	"48",	"49",	"50",	
                      "51",	"52",	"53",	"54",	"55",	"56",	"57",	"58",	"59",	"60",	
                      "61",	"62",	"63",	"64",	"65"	,"66","67")#,"tipo")

## PRUEBA DEPTARTAMENTO
distritos<- grilla
for (o in 1:nrow(distritos)){
  vec_origen<- which((suma_df$id_tryc) == (distritos$distrito[o]))
  if ((length(vec_origen))!=0){
    for (pos in 1:length(vec_origen)){
      
      pos_destino<-which(names(distritos)==suma_df$id_dstr2[vec_origen[pos]])
      distritos[o,pos_destino]<-suma_df$distanc[vec_origen[pos]]
      #[filas,columnas]
    }
  }
}


#distritos$tipo <- "pie"
distritos$tipo <- "colectivos"
getwd()

# # #pie
# write.csv(distritos,"D:/Josefina/Proyectos/salud/movilidad_3/procesamiento/pie/distancia/distancia_pie.csv")
# # 
# # #CARPETA VARIABLES A USAR
# write.csv(distritos,"D:/Josefina/Proyectos/salud/movilidad_3/procesamiento/pie/variables_ingreso_model/distancia_pie.csv")

# # AUTO
# 
# write.csv(distritos,"D:/Josefina/Proyectos/salud/movilidad_3/procesamiento/auto/distancia/distancia_auto.csv")
# # 
# # #CARPETA VARIABLES A USAR
# write.csv(distritos,"D:/Josefina/Proyectos/salud/movilidad_3/procesamiento/auto/variables_ingreso_modelo/distancia_auto.csv")



# # BICICLETAS
# 
# write.csv(distritos,"D:/Josefina/Proyectos/salud/movilidad_3/procesamiento/bicicletas/distancia/distancia_bicicletas.csv")
# # 
# # #CARPETA VARIABLES A USAR
# write.csv(distritos,"D:/Josefina/Proyectos/salud/movilidad_3/procesamiento/bicicletas/variables_ingreso_model/distancia_bicicletas.csv")


# COLECTIVOS

write.csv(distritos,"D:/Josefina/Proyectos/salud/movilidad_3/procesamiento/colectivos/distancia/distancia_colectivos.csv")
# 
# #CARPETA VARIABLES A USAR
write.csv(distritos,"D:/Josefina/Proyectos/salud/movilidad_3/procesamiento/colectivos/variables_ingreso_model/distancia_colectivos.csv")



# #PRUEBA DEPTARTAMENTOS
nombre_archivo
write.csv(distritos,"D:/Josefina/Proyectos/salud/movilidad_5/procesamiento/distancia/distancia_colectivos.csv")
