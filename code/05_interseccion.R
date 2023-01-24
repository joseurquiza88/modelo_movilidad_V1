# Con este programa hago la interseccion entre los distritos
#y las lineas de los trayectos
# Leo los archivos en formato .shp
#archivo de recorridos
#

#################################################################################
#                                     PIE
################################################################################

setwd("D:/Josefina/Proyectos/salud/movilidad_3/procesamiento/pie/recorridos/recorrido_tot/shape")


nombre_recorrido <- "recorridos_8_tot"
recorridos <- st_read(paste(nombre_recorrido,".shp",sep = ""))
prueba_pie <- st_read("D:/Josefina/Proyectos/salud/movilidad_3/procesamiento/pie/recorridos/recorrido_tot/shape/recorridos_8_tot.shp")


#################################################################################
#                                     AUTO
################################################################################
#AUTOS V2
setwd("D:/Josefina/Proyectos/salud/movilidad_3/procesamiento/auto/recorridos/recorridos_tot/shape/tot")



nombre_recorrido <- "proc_proc_combinado"
recorridos <- st_read(paste(nombre_recorrido,".shp",sep = ""))


#################################################################################
#                                     BICICLETAS
################################################################################
setwd("D:/Josefina/Proyectos/salud/movilidad_3/procesamiento/bicicletas/recorridos/recorridos_tot/shape/tot")



nombre_recorrido <- "proc_proc_combinado"
recorridos <- st_read(paste(nombre_recorrido,".shp",sep = ""))


#################################################################################
#                                     COLECTIVOS
################################################################################
setwd("D:/Josefina/Proyectos/salud/movilidad_3/procesamiento/colectivos/recorridos/recorridos_tot/shape/tot")
nombre_recorrido <- "proc_proc_combinado"
recorridos <- st_read(paste(nombre_recorrido,".shp",sep = ""),crs=4326)



#################################################################################
#                                     Prueba departamentos
################################################################################
setwd("D:/Josefina/Proyectos/salud/movilidad_5/procesamiento/recorridos/final")
nombre_recorrido <- "auto_v3_decode"
recorridos <- st_read(paste(nombre_recorrido,".shp",sep = ""),crs=4326)


# archivo de distritos
#distritos <- st_read("D:/Josefina/Proyectos/salud/movilidad_3/procesamiento/auto/recorridos/shape/distritos_interes.shp")
# archivo de departamento
distritos <- st_read("D:/Josefina/Proyectos/salud/movilidad_5/procesamiento/shape/departamento.shp")

recorridos$len_tot_2 <-st_length(recorridos)
# Hago la interseccion entre los archivos
interseccion <- st_intersection(recorridos,distritos)
#plot(interseccion,lwd=3)
# En teoria con "st_intersection" me quedan ambas geometrias
#Puntos y  lineas. Con esto solo me quedo con las lineas
interseccion_l <- st_collection_extract(interseccion,type="LINESTRING")

# Calculo la linea de de cada corte realizado a parte
longitud <- st_length(interseccion_l) 
# Lo agrego como columna
interseccion_l$len_distrito <- longitud
# Calculo la relacion del total esto me va a servir para calcular los tiempos
interseccion_l$dist_tot_rec <- interseccion_l$len_distrito/interseccion_l$len_tot

# ESTE ES EL TIEMPO QUE PASA EN CADA DISTRITO
#interseccion_l$tiempo_distrito <- interseccion_l$X_un_tiem_1*interseccion_l$dist_tot_rec
interseccion_l$tiempo_distrito <- interseccion_l$timp_tt*interseccion_l$dist_tot_rec

# interseccion_l_2 <- data.frame(id_trayecto=interseccion_l$id_trayect,
#                                len_tot=interseccion_l$len_tot_2,
#                                DISTRITOS= interseccion_l$dist_nom,
#                                id_distrito=interseccion_l$id_text,
#                                len_distrito=interseccion_l$len_distrito,
#                                dist_tot_rec=interseccion_l$dist_tot_rec,
#                                distanc_tot = interseccion_l$X_un_dist_1,
#                                duration_tot = interseccion_l$X_un_tiem_1,
#                                distanc = interseccion_l$len_distrito,
#                                duration = interseccion_l$tiempo_distrito,##X_un_tiempo,
#                                id_orgn = interseccion_l$X_un_id_ori,#X_num_id_or
#                                id_dst = interseccion_l$X_un_id_des,#X_num_id_de,#X_un_id_des,
#                                geometry=interseccion_l$geometry
# )

interseccion_l_2 <- data.frame(id_trayecto=interseccion_l$id_tryc,
                               len_tot=interseccion_l$len_tot_2,
                               DISTRITOS= interseccion_l$dist_nom,
                               id_distrito=interseccion_l$id_text,
                               len_distrito=interseccion_l$len_distrito,
                               dist_tot_rec=interseccion_l$dist_tot_rec,
                               distanc_tot = interseccion_l$dstnc_t,
                               duration_tot = interseccion_l$timp_tt,
                               distanc = interseccion_l$len_distrito,
                               duration = interseccion_l$tiempo_distrito,##X_un_tiempo,
                               id_orgn = interseccion_l$id_orgn,#X_num_id_or
                               id_dst = interseccion_l$id_dstn,#X_num_id_de,#X_un_id_des,
                               geometry=interseccion_l$geometry)

nombre_guardado <- paste(nombre_recorrido,"_v2.shp", sep="")

st_write(interseccion_l_2 ,nombre_guardado ,delete_layer = TRUE)





####################################
# ahora tenemos que hacer un intereect con la grilla
# en wgs la coordenadas

grilla <- st_read("D:/Josefina/Proyectos/salud/movilidad_3/procesamiento/auto/grilla/buffer_wgs.shp")
interseccion_l_2 <- st_read("D:/Josefina/Proyectos/salud/movilidad_5/procesamiento/recorridos/final/bicicleta_v3_decode_v2.shp")


# Hago la interseccion entre los archivos
# muy pesado hacerlo en el qgs!!!!!!!
# capa entrada recorridos
# capa de superposicion grilla
interseccion_grilla <- st_intersection(interseccion_l_2,grilla)
interseccion_l_grilla <- st_collection_extract(interseccion_grilla,type="LINESTRING")
nombre_guardado<- "bicicleta_grilla.shp"

#write.csv(interseccion_l_grilla ,"pie_grilla.csv")
st_write(interseccion_l_grilla ,nombre_guardado ,delete_layer = TRUE)

interseccion_grilla  <- read.csv("D:/Josefina/Proyectos/salud/movilidad_3//procesamiento/recorridos/shape/interseccion_grilla.csv")
