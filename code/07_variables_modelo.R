# variables del modelo puesta a punto AUTO
#


#################################################################################
#                           VARIABLES: PIE
################################################################################

setwd("D:/Josefina/Proyectos/salud/movilidad_3/procesamiento/pie/variables_ingreso_model")

####################################################
# CONCENTRACIONES TRAYECTO
concentracion_trayecto  <- read.csv("PMDIARIO_prom_trayecto_pie.csv",sep = ",",header = T)
names(concentracion_trayecto ) <- c("x","distritos","01",	"02",	"03",	"04","05",	"06",	"07",	"08",	"09",	"10",
                      "11",	"12",	"13",	"14",	"15",	"16",	"17",	"18",	"19",	"20",	
                      "21",	"22",	"23",	"24",	"25",	"26",	"27",	"28",	"29",	"30",	
                      "31",	"32",	"33",	"34",	"35",	"36",	"37",	"38",	"39",	"40",
                      "41",	"42",	"43",	"44",	"45",	"46",	"47",	"48",	"49",	"50",	
                      "51",	"52",	"53",	"54",	"55",	"56",	"57",	"58",	"59",	"60",	
                      "61",	"62",	"63",	"64",	"65"	,"66","67","tipo")

conc_trayecto<- concentracion_trayecto[2:70]

####################################################
# COEFICIENTE P
indice <- read.csv("grilla_coef_p.csv")
names(indice) <- c("distritos","01",	"02",	"03",	"04","05",	"06",	"07",	"08",	"09",	"10",
                                 "11",	"12",	"13",	"14",	"15",	"16",	"17",	"18",	"19",	"20",	
                                 "21",	"22",	"23",	"24",	"25",	"26",	"27",	"28",	"29",	"30",	
                                 "31",	"32",	"33",	"34",	"35",	"36",	"37",	"38",	"39",	"40",
                                 "41",	"42",	"43",	"44",	"45",	"46",	"47",	"48",	"49",	"50",	
                                 "51",	"52",	"53",	"54",	"55",	"56",	"57",	"58",	"59",	"60",	
                                 "61",	"62",	"63",	"64",	"65"	,"66","67","num_actividad","actividades")


#le ponemos 0 al distrito
for (i in 1:nrow(indice)){
  if (indice$distritos[i] < 10){
    indice$distritos2[i] <- paste("0",indice$distritos[i],sep="")
  }else{
    indice$distritos2[i] <- indice$distritos[i]
  }
  
}
indice$distritos <- indice$distritos2
indice <- indice[1:70]
#################################################
#INDICE P TRANSPORTE
indice_transp <- read.csv("grilla_coef_p_transporte.csv")
names(indice_transp) <- c("distritos","01",	"02",	"03",	"04","05",	"06",	"07",	"08",	"09",	"10",
                   "11",	"12",	"13",	"14",	"15",	"16",	"17",	"18",	"19",	"20",	
                   "21",	"22",	"23",	"24",	"25",	"26",	"27",	"28",	"29",	"30",	
                   "31",	"32",	"33",	"34",	"35",	"36",	"37",	"38",	"39",	"40",
                   "41",	"42",	"43",	"44",	"45",	"46",	"47",	"48",	"49",	"50",	
                   "51",	"52",	"53",	"54",	"55",	"56",	"57",	"58",	"59",	"60",	
                   "61",	"62",	"63",	"64",	"65"	,"66","67","num_actividad","actividades")


#le ponemos 0 al distrito
for (i in 1:nrow(indice_transp)){
  if (indice_transp$distritos[i] < 10){
    indice_transp$distritos2[i] <- paste("0",indice_transp$distritos[i],sep="")
  }else{
    indice_transp$distritos2[i] <- indice_transp$distritos[i]
  }
  
}
indice_transp$distritos <- indice_transp$distritos2
indice_transp <- indice_transp[1:70]
##################################################
#CONCENTRACIONES ORIGEN - DESTINO
concentracion <- read.csv("concentracion_destino.csv")


#le ponemos 0 al distrito
for (i in 1:nrow(concentracion)){
  if (concentracion$id[i] < 10){
    concentracion$id2[i] <- paste("0",concentracion$id[i],sep="")
  }else{
    concentracion$id2[i] <- concentracion$id[i]
  }
  
}
concentracion$id <- concentracion$id2
concentracion <- concentracion[1:6]


#######################################
# TIEMPO EN CADA DISTRITO SEGUN TRAYECTO
hs_viaje  <- read.csv("tiempo_pie.csv")
hs_viaje <- hs_viaje[2:69]
#hs_viaje$tipo <- "colectivo"
hs_viaje$tipo <- "pie"
names(hs_viaje) <- c("distritos","01",	"02",	"03",	"04","05",	"06",	"07",	"08",	"09",	"10",
                   "11",	"12",	"13",	"14",	"15",	"16",	"17",	"18",	"19",	"20",	
                   "21",	"22",	"23",	"24",	"25",	"26",	"27",	"28",	"29",	"30",	
                   "31",	"32",	"33",	"34",	"35",	"36",	"37",	"38",	"39",	"40",
                   "41",	"42",	"43",	"44",	"45",	"46",	"47",	"48",	"49",	"50",	
                   "51",	"52",	"53",	"54",	"55",	"56",	"57",	"58",	"59",	"60",	
                   "61",	"62",	"63",	"64",	"65"	,"66","67","tipo")
#################################################################################
#################################################################################
#################################################################################
#################################################################################
#                           VARIABLES: AUTO
################################################################################

setwd("D:/Josefina/Proyectos/salud/movilidad_3/procesamiento/auto/variables_ingreso_modelo")

####################################################
# CONCENTRACIONES TRAYECTO
concentracion_trayecto  <- read.csv("PMDIARIO_prom_trayecto_auto.csv",sep = ",",header = T)
names(concentracion_trayecto ) <- c("x","distritos","01",	"02",	"03",	"04","05",	"06",	"07",	"08",	"09",	"10",
                                    "11",	"12",	"13",	"14",	"15",	"16",	"17",	"18",	"19",	"20",	
                                    "21",	"22",	"23",	"24",	"25",	"26",	"27",	"28",	"29",	"30",	
                                    "31",	"32",	"33",	"34",	"35",	"36",	"37",	"38",	"39",	"40",
                                    "41",	"42",	"43",	"44",	"45",	"46",	"47",	"48",	"49",	"50",	
                                    "51",	"52",	"53",	"54",	"55",	"56",	"57",	"58",	"59",	"60",	
                                    "61",	"62",	"63",	"64",	"65"	,"66","67","tipo")

conc_trayecto<- concentracion_trayecto[2:70]

####################################################
# COEFICIENTE P
indice <- read.csv("grilla_coef_p.csv")
names(indice) <- c("distritos","01",	"02",	"03",	"04","05",	"06",	"07",	"08",	"09",	"10",
                   "11",	"12",	"13",	"14",	"15",	"16",	"17",	"18",	"19",	"20",	
                   "21",	"22",	"23",	"24",	"25",	"26",	"27",	"28",	"29",	"30",	
                   "31",	"32",	"33",	"34",	"35",	"36",	"37",	"38",	"39",	"40",
                   "41",	"42",	"43",	"44",	"45",	"46",	"47",	"48",	"49",	"50",	
                   "51",	"52",	"53",	"54",	"55",	"56",	"57",	"58",	"59",	"60",	
                   "61",	"62",	"63",	"64",	"65"	,"66","67","num_actividad","actividades")


#le ponemos 0 al distrito
for (i in 1:nrow(indice)){
  if (indice$distritos[i] < 10){
    indice$distritos2[i] <- paste("0",indice$distritos[i],sep="")
  }else{
    indice$distritos2[i] <- indice$distritos[i]
  }
  
}
indice$distritos <- indice$distritos2
indice <- indice[1:70]
#################################################
#INDICE P TRANSPORTE
indice_transp <- read.csv("grilla_coef_p_transporte.csv")
names(indice_transp) <- c("distritos","01",	"02",	"03",	"04","05",	"06",	"07",	"08",	"09",	"10",
                          "11",	"12",	"13",	"14",	"15",	"16",	"17",	"18",	"19",	"20",	
                          "21",	"22",	"23",	"24",	"25",	"26",	"27",	"28",	"29",	"30",	
                          "31",	"32",	"33",	"34",	"35",	"36",	"37",	"38",	"39",	"40",
                          "41",	"42",	"43",	"44",	"45",	"46",	"47",	"48",	"49",	"50",	
                          "51",	"52",	"53",	"54",	"55",	"56",	"57",	"58",	"59",	"60",	
                          "61",	"62",	"63",	"64",	"65"	,"66","67","num_actividad","actividades")


#le ponemos 0 al distrito
for (i in 1:nrow(indice_transp)){
  if (indice_transp$distritos[i] < 10){
    indice_transp$distritos2[i] <- paste("0",indice_transp$distritos[i],sep="")
  }else{
    indice_transp$distritos2[i] <- indice_transp$distritos[i]
  }
  
}
indice_transp$distritos <- indice_transp$distritos2
indice_transp <- indice_transp[1:70]
##################################################
#CONCENTRACIONES ORIGEN - DESTINO
concentracion <- read.csv("concentracion_destino.csv")


#le ponemos 0 al distrito
for (i in 1:nrow(concentracion)){
  if (concentracion$id[i] < 10){
    concentracion$id2[i] <- paste("0",concentracion$id[i],sep="")
  }else{
    concentracion$id2[i] <- concentracion$id[i]
  }
  
}
concentracion$id <- concentracion$id2
concentracion <- concentracion[1:6]


#######################################
# TIEMPO EN CADA DISTRITO SEGUN TRAYECTO
hs_viaje  <- read.csv("tiempo_auto.csv")
hs_viaje <- hs_viaje[2:69]
#hs_viaje$tipo <- "colectivo"
hs_viaje$tipo <- "auto"
names(hs_viaje) <- c("distritos","01",	"02",	"03",	"04","05",	"06",	"07",	"08",	"09",	"10",
                     "11",	"12",	"13",	"14",	"15",	"16",	"17",	"18",	"19",	"20",	
                     "21",	"22",	"23",	"24",	"25",	"26",	"27",	"28",	"29",	"30",	
                     "31",	"32",	"33",	"34",	"35",	"36",	"37",	"38",	"39",	"40",
                     "41",	"42",	"43",	"44",	"45",	"46",	"47",	"48",	"49",	"50",	
                     "51",	"52",	"53",	"54",	"55",	"56",	"57",	"58",	"59",	"60",	
                     "61",	"62",	"63",	"64",	"65"	,"66","67","tipo")



###############################################################################
####################################################################################

#################################################################################
#                           VARIABLES: BICICLETAS
################################################################################

setwd("D:/Josefina/Proyectos/salud/movilidad_3/procesamiento/bicicletas/variables_ingreso_model")

####################################################
# CONCENTRACIONES TRAYECTO
concentracion_trayecto  <- read.csv("PMDIARIO_prom_trayecto_bicicletas.csv",sep = ",",header = T)
names(concentracion_trayecto ) <- c("x","distritos","01",	"02",	"03",	"04","05",	"06",	"07",	"08",	"09",	"10",
                                    "11",	"12",	"13",	"14",	"15",	"16",	"17",	"18",	"19",	"20",	
                                    "21",	"22",	"23",	"24",	"25",	"26",	"27",	"28",	"29",	"30",	
                                    "31",	"32",	"33",	"34",	"35",	"36",	"37",	"38",	"39",	"40",
                                    "41",	"42",	"43",	"44",	"45",	"46",	"47",	"48",	"49",	"50",	
                                    "51",	"52",	"53",	"54",	"55",	"56",	"57",	"58",	"59",	"60",	
                                    "61",	"62",	"63",	"64",	"65"	,"66","67","tipo")

conc_trayecto<- concentracion_trayecto[2:70]

####################################################
# COEFICIENTE P
indice <- read.csv("grilla_coef_p.csv")
names(indice) <- c("distritos","01",	"02",	"03",	"04","05",	"06",	"07",	"08",	"09",	"10",
                   "11",	"12",	"13",	"14",	"15",	"16",	"17",	"18",	"19",	"20",	
                   "21",	"22",	"23",	"24",	"25",	"26",	"27",	"28",	"29",	"30",	
                   "31",	"32",	"33",	"34",	"35",	"36",	"37",	"38",	"39",	"40",
                   "41",	"42",	"43",	"44",	"45",	"46",	"47",	"48",	"49",	"50",	
                   "51",	"52",	"53",	"54",	"55",	"56",	"57",	"58",	"59",	"60",	
                   "61",	"62",	"63",	"64",	"65"	,"66","67","num_actividad","actividades")


#le ponemos 0 al distrito
for (i in 1:nrow(indice)){
  if (indice$distritos[i] < 10){
    indice$distritos2[i] <- paste("0",indice$distritos[i],sep="")
  }else{
    indice$distritos2[i] <- indice$distritos[i]
  }
  
}
indice$distritos <- indice$distritos2
indice <- indice[1:70]
#################################################
#INDICE P TRANSPORTE
indice_transp <- read.csv("grilla_coef_p_transporte.csv")
names(indice_transp) <- c("distritos","01",	"02",	"03",	"04","05",	"06",	"07",	"08",	"09",	"10",
                          "11",	"12",	"13",	"14",	"15",	"16",	"17",	"18",	"19",	"20",	
                          "21",	"22",	"23",	"24",	"25",	"26",	"27",	"28",	"29",	"30",	
                          "31",	"32",	"33",	"34",	"35",	"36",	"37",	"38",	"39",	"40",
                          "41",	"42",	"43",	"44",	"45",	"46",	"47",	"48",	"49",	"50",	
                          "51",	"52",	"53",	"54",	"55",	"56",	"57",	"58",	"59",	"60",	
                          "61",	"62",	"63",	"64",	"65"	,"66","67","num_actividad","actividades")


#le ponemos 0 al distrito
for (i in 1:nrow(indice_transp)){
  if (indice_transp$distritos[i] < 10){
    indice_transp$distritos2[i] <- paste("0",indice_transp$distritos[i],sep="")
  }else{
    indice_transp$distritos2[i] <- indice_transp$distritos[i]
  }
  
}
indice_transp$distritos <- indice_transp$distritos2
indice_transp <- indice_transp[1:70]
##################################################
#CONCENTRACIONES ORIGEN - DESTINO
concentracion <- read.csv("concentracion_destino.csv")


#le ponemos 0 al distrito
for (i in 1:nrow(concentracion)){
  if (concentracion$id[i] < 10){
    concentracion$id2[i] <- paste("0",concentracion$id[i],sep="")
  }else{
    concentracion$id2[i] <- concentracion$id[i]
  }
  
}
concentracion$id <- concentracion$id2
concentracion <- concentracion[1:6]


#######################################
# TIEMPO EN CADA DISTRITO SEGUN TRAYECTO
hs_viaje  <- read.csv("tiempo_bicicletas.csv")
hs_viaje <- hs_viaje[2:69]
#hs_viaje$tipo <- "colectivo"
hs_viaje$tipo <- "bicicleta"
names(hs_viaje) <- c("distritos","01",	"02",	"03",	"04","05",	"06",	"07",	"08",	"09",	"10",
                     "11",	"12",	"13",	"14",	"15",	"16",	"17",	"18",	"19",	"20",	
                     "21",	"22",	"23",	"24",	"25",	"26",	"27",	"28",	"29",	"30",	
                     "31",	"32",	"33",	"34",	"35",	"36",	"37",	"38",	"39",	"40",
                     "41",	"42",	"43",	"44",	"45",	"46",	"47",	"48",	"49",	"50",	
                     "51",	"52",	"53",	"54",	"55",	"56",	"57",	"58",	"59",	"60",	
                     "61",	"62",	"63",	"64",	"65"	,"66","67","tipo")

###############################################################################
####################################################################################

#################################################################################
#                           VARIABLES: COLECTIVOS
################################################################################

setwd("D:/Josefina/Proyectos/salud/movilidad_3/procesamiento/colectivos/variables_ingreso_model")

####################################################
# CONCENTRACIONES TRAYECTO
concentracion_trayecto  <- read.csv("PMDIARIO_prom_trayecto_colectivos.csv",sep = ",",header = T)
names(concentracion_trayecto ) <- c("x","distritos","01",	"02",	"03",	"04","05",	"06",	"07",	"08",	"09",	"10",
                                    "11",	"12",	"13",	"14",	"15",	"16",	"17",	"18",	"19",	"20",	
                                    "21",	"22",	"23",	"24",	"25",	"26",	"27",	"28",	"29",	"30",	
                                    "31",	"32",	"33",	"34",	"35",	"36",	"37",	"38",	"39",	"40",
                                    "41",	"42",	"43",	"44",	"45",	"46",	"47",	"48",	"49",	"50",	
                                    "51",	"52",	"53",	"54",	"55",	"56",	"57",	"58",	"59",	"60",	
                                    "61",	"62",	"63",	"64",	"65"	,"66","67","tipo")

conc_trayecto<- concentracion_trayecto[2:70]

####################################################
# COEFICIENTE P
indice <- read.csv("grilla_coef_p.csv")
names(indice) <- c("distritos","01",	"02",	"03",	"04","05",	"06",	"07",	"08",	"09",	"10",
                   "11",	"12",	"13",	"14",	"15",	"16",	"17",	"18",	"19",	"20",	
                   "21",	"22",	"23",	"24",	"25",	"26",	"27",	"28",	"29",	"30",	
                   "31",	"32",	"33",	"34",	"35",	"36",	"37",	"38",	"39",	"40",
                   "41",	"42",	"43",	"44",	"45",	"46",	"47",	"48",	"49",	"50",	
                   "51",	"52",	"53",	"54",	"55",	"56",	"57",	"58",	"59",	"60",	
                   "61",	"62",	"63",	"64",	"65"	,"66","67","num_actividad","actividades")


#le ponemos 0 al distrito
for (i in 1:nrow(indice)){
  if (indice$distritos[i] < 10){
    indice$distritos2[i] <- paste("0",indice$distritos[i],sep="")
  }else{
    indice$distritos2[i] <- indice$distritos[i]
  }
  
}
indice$distritos <- indice$distritos2
indice <- indice[1:70]
#################################################
#INDICE P TRANSPORTE
indice_transp <- read.csv("grilla_coef_p_transporte.csv")
names(indice_transp) <- c("distritos","01",	"02",	"03",	"04","05",	"06",	"07",	"08",	"09",	"10",
                          "11",	"12",	"13",	"14",	"15",	"16",	"17",	"18",	"19",	"20",	
                          "21",	"22",	"23",	"24",	"25",	"26",	"27",	"28",	"29",	"30",	
                          "31",	"32",	"33",	"34",	"35",	"36",	"37",	"38",	"39",	"40",
                          "41",	"42",	"43",	"44",	"45",	"46",	"47",	"48",	"49",	"50",	
                          "51",	"52",	"53",	"54",	"55",	"56",	"57",	"58",	"59",	"60",	
                          "61",	"62",	"63",	"64",	"65"	,"66","67","num_actividad","actividades")


#le ponemos 0 al distrito
for (i in 1:nrow(indice_transp)){
  if (indice_transp$distritos[i] < 10){
    indice_transp$distritos2[i] <- paste("0",indice_transp$distritos[i],sep="")
  }else{
    indice_transp$distritos2[i] <- indice_transp$distritos[i]
  }
  
}
indice_transp$distritos <- indice_transp$distritos2
indice_transp <- indice_transp[1:70]
##################################################
#CONCENTRACIONES ORIGEN - DESTINO
concentracion <- read.csv("concentracion_destino.csv")


#le ponemos 0 al distrito
for (i in 1:nrow(concentracion)){
  if (concentracion$id[i] < 10){
    concentracion$id2[i] <- paste("0",concentracion$id[i],sep="")
  }else{
    concentracion$id2[i] <- concentracion$id[i]
  }
  
}
concentracion$id <- concentracion$id2
concentracion <- concentracion[1:6]


#######################################
# TIEMPO EN CADA DISTRITO SEGUN TRAYECTO
hs_viaje  <- read.csv("tiempo_colectivos.csv")
hs_viaje <- hs_viaje[2:69]
#hs_viaje$tipo <- "colectivo"
hs_viaje$tipo <- "colectivos"
names(hs_viaje) <- c("distritos","01",	"02",	"03",	"04","05",	"06",	"07",	"08",	"09",	"10",
                     "11",	"12",	"13",	"14",	"15",	"16",	"17",	"18",	"19",	"20",	
                     "21",	"22",	"23",	"24",	"25",	"26",	"27",	"28",	"29",	"30",	
                     "31",	"32",	"33",	"34",	"35",	"36",	"37",	"38",	"39",	"40",
                     "41",	"42",	"43",	"44",	"45",	"46",	"47",	"48",	"49",	"50",	
                     "51",	"52",	"53",	"54",	"55",	"56",	"57",	"58",	"59",	"60",	
                     "61",	"62",	"63",	"64",	"65"	,"66","67","tipo")




#################################################################################
#                           VARIABLES: DEPARTAMENTO -AUTO
################################################################################

setwd("D:/Josefina/Proyectos/salud/movilidad_5/variables_ingreso_modelo/pie")

####################################################
# CONCENTRACIONES TRAYECTO
concentracion_trayecto  <- read.csv("PMDIARIO_prom_trayecto_pie_v2.csv",sep = ",",header = T)
names(concentracion_trayecto ) <- c("x","distritos","01",	"02",	"03",	"04","05",	"06","tipo")

conc_trayecto<- concentracion_trayecto[2:8]

####################################################
# COEFICIENTE P
indice <- read.csv("grilla_coef_p.csv")
names(indice) <- c("distritos","01",	"02",	"03",	"04","05",	"06",	"num_actividad","actividades")


#le ponemos 0 al distrito
for (i in 1:nrow(indice)){
  if (indice$distritos[i] < 10){
    indice$distritos2[i] <- paste("0",indice$distritos[i],sep="")
  }else{
    indice$distritos2[i] <- indice$distritos[i]
  }
  
}
indice$distritos <- indice$distritos2
indice <- indice[1:9]
##################################################
#CONCENTRACIONES ORIGEN - DESTINO
concentracion <- read.csv("concentracion_destino_pie.csv")


#le ponemos 0 al distrito
for (i in 1:nrow(concentracion)){
  if (concentracion$id[i] < 10){
    concentracion$id2[i] <- paste("0",concentracion$id[i],sep="")
  }else{
    concentracion$id2[i] <- concentracion$id[i]
  }
  
}
concentracion$id <- concentracion$id2
concentracion <- concentracion[2:7]


#######################################
# TIEMPO EN CADA DISTRITO SEGUN TRAYECTO
hs_viaje  <- read.csv("tiempo_pie_v2.csv")
hs_viaje <- hs_viaje[2:9]
#hs_viaje$tipo <- "colectivo"
hs_viaje$tipo <- "pie"
names(hs_viaje) <- c("distritos","01",	"02",	"03",	"04","05",	"06","tipo")



