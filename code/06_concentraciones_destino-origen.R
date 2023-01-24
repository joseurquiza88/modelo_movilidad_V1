# concentracion destino
# sto es porque en el origen y en el destino solo voy a tomar
#las concentraciones del pixel donde comienza o  termina el recorrido
# a diferncia del otro codigo que toma el promedio/suma de todo el recorrido

# ESTO ES LO  MISMO PARA TODOS LOS TIPOS DE TRANSPORTE

grilla <- st_read("D:/Josefina/Proyectos/salud/movilidad_3/procesamiento/auto/grilla/buffer_wgs.shp")
#todoslasmovilidaes
#puntos <- st_read("D:/Josefina/Proyectos/salud/movilidad_3/procesamiento/auto/recorridos/shape/puntos_origen_destino.shp")
#todoslasmovilidaes
puntos <- st_read("D:/Josefina/Proyectos/salud/movilidad_5/procesamiento/shape/puntos/puntos.shp")

#setwd("D:/Josefina/Proyectos/salud/movilidad_3/procesamiento/concentraciones")
setwd("D:/Josefina/Proyectos/salud/movilidad_5/procesamiento/shape/conc_origen_destino")
# Hago la interseccion entre los archivos

interseccion <- st_intersection(puntos ,grilla )
interseccion_l <- st_collection_extract(interseccion,type="POINT")
#st_write(interseccion_l,"concentracion_origen_destino.shp",delete_layer = TRUE)


st_write(interseccion,"concentracion_origen_destino.shp",delete_layer = TRUE)
########
#lo pongo en formato 
df <- data.frame(id = interseccion_l$id,
                 distrito = interseccion_l$dis,
                 dpto = interseccion_l$dept,
                 PMBICIS = interseccion_l$PMBICIS,
                 PMDIARIO = interseccion_l$PMDIARIO,
                 EMI_NOX = interseccion_l$EMI_NOX,
                 interseccion_l$POBLXGRI,)


df <- data.frame(id = interseccion$ID,
                 distrito = interseccion$punto,
                 #dpto = interseccion$dept,
                 PMBICIS = interseccion$PMBICIS,
                 PMDIARIO = interseccion$PMDIARIO,
                 EMI_NOX = interseccion$EMI_NOX,
                 POBLXGRI=interseccion$POBLXGRI)
#write.csv(df,"D:/Josefina/Proyectos/salud/movilidad_3/variables_ingreso_modelo/concentracion_destino.csv")                 
write.csv(df,"D:/Josefina/Proyectos/salud/movilidad_5/procesamiento/concentraciones/concentracion_destino.csv")                 

################################################################################                 
# Concentrciones medias departamentales
grilla <- st_read("D:/Josefina/Proyectos/salud/movilidad_3/procesamiento/auto/grilla/buffer_wgs.shp")
departamento <- st_read("D:/Josefina/Proyectos/salud/movilidad_5/procesamiento/shape/departamento/departamento.shp")
interseccion <- st_intersection(departamento ,grilla )


interseccion%>%
  group_by(ID) %>% 
  group_split() -> dataSplit
suma_df <- data.frame()
for (i in 1:length(dataSplit)){
  if (i %% 1000==0){
    print (i)
  }
  
  departamento <- dataSplit[[i]][["NAM"]][1]
  id <- dataSplit[[i]][["ID"]][1]

  POBLXGRI_sum<-sum(dataSplit[[i]][["POBLXGRI"]],na.rm = T)
  POBLXGRI_prom<-mean(dataSplit[[i]][["POBLXGRI"]],na.rm = T)
  PMDIARIO_sum <-sum(dataSplit[[i]][["PMDIARIO"]],na.rm = T)
  PMDIARIO_prom <-mean(dataSplit[[i]][["PMDIARIO"]],na.rm = T)
  len_pixeles <- length(dataSplit[[i]][["PMDIARIO"]])
  #df <- data.frame(id_tryc,len_tot,DISTRIT,id_dstr ,ln_dstr,dst_tt_,tiempo,dstnc_k,drtn_hs,distanc,POBLXGRI_sum,POBLXGRI_prom,PMDIARIO_sum,PMDIARIO_prom,len_pixeles )
  df <- data.frame(departamento,id,POBLXGRI_sum,POBLXGRI_prom,PMDIARIO_sum,PMDIARIO_prom,len_pixeles )
  
  #names(df) <- c("id_tryc","len_tot","DISTRIT","id_dstr" ,"ln_dst","dst_tt_","tiempo","dstnc_k","drtn_hs","distanc","POBLXGRI_sum","POBLXGRI_prom","PMDIARIO_sum","PMDIARIO_prom","len_pixeles")
  names(df) <- c("departamento","id","POBLXGRI_sum_dept","POBLXGRI_prom_dept","PMDIARIO_sum_dept","PMDIARIO_prom_dept","len_pixeles")
  
  suma_df<- rbind(suma_df,df)
  #names(suma_df) <- c("id_tryc","len_tot","DISTRIT","id_dstr" ,"ln_dst","dst_tt_","tiempo","dstnc_k","drtn_hs","distanc","POBLXGRI_sum","POBLXGRI_prom","PMDIARIO_sum","PMDIARIO_prom","len_pixeles")
  names(suma_df) <- c("departamento","id","POBLXGRI_sum_dept","POBLXGRI_prom_dept","PMDIARIO_sum_dept","PMDIARIO_prom_dept","len_pixeles")
  
}
                
                 
                 
concentraciones<- read.csv("D:/Josefina/Proyectos/salud/movilidad_5/procesamiento/concentraciones/concentracion_destino.csv")                  
concentraciones_total <- merge(x = concentraciones, y = suma_df, by ="id") # Equivalente                 

df <- data.frame(id = concentraciones_total$id,
                 dpto = concentraciones_total$distrito,
                 PMDIARIO_pixel = concentraciones_total$PMDIARIO,
                 PMDIARIO_depto = concentraciones_total$PMDIARIO_prom_dept,
                 POBLXGRI_sum_dept= concentraciones_total$POBLXGRI_sum_dept,
                 POBLXGRI_pixel = concentraciones_total$POBLXGRI)
write.csv(df,"D:/Josefina/Proyectos/salud/movilidad_5/procesamiento/concentraciones/concentracion_destino_tot.csv")                  

