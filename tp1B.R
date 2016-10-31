library(sampling)
library(SamplingStrata)
library(dplyr)
# Cargo dataframe
load("data/Marco.Agropecuario.RData")

# Creo las categorizazciones de CANTEMP y SUPERFICIE
# Para ello, comienzo asignando la categoria "superior" a todas las observaciones del Marco,
# y luego voy "pisando" este valor por default con las categorias "inferiores"
Marco.Agropecuario$catCANTEMP <- 5
Marco.Agropecuario$catCANTEMP <- ifelse(Marco.Agropecuario$CANTEMP <= 50, 4, Marco.Agropecuario$catCANTEMP)
Marco.Agropecuario$catCANTEMP <- ifelse(Marco.Agropecuario$CANTEMP <= 40, 3, Marco.Agropecuario$catCANTEMP)
Marco.Agropecuario$catCANTEMP <- ifelse(Marco.Agropecuario$CANTEMP <= 30, 2, Marco.Agropecuario$catCANTEMP)
Marco.Agropecuario$catCANTEMP <- ifelse(Marco.Agropecuario$CANTEMP <= 20, 1, Marco.Agropecuario$catCANTEMP)

Marco.Agropecuario$catSUPERFICIE <- 5
Marco.Agropecuario$catSUPERFICIE <- ifelse(Marco.Agropecuario$SUPERFICIE <= 2000, 4, Marco.Agropecuario$catSUPERFICIE)
Marco.Agropecuario$catSUPERFICIE <- ifelse(Marco.Agropecuario$SUPERFICIE <= 1500, 3, Marco.Agropecuario$catSUPERFICIE)
Marco.Agropecuario$catSUPERFICIE <- ifelse(Marco.Agropecuario$SUPERFICIE <= 1000, 2, Marco.Agropecuario$catSUPERFICIE)
Marco.Agropecuario$catSUPERFICIE <- ifelse(Marco.Agropecuario$SUPERFICIE <= 500, 1, Marco.Agropecuario$catSUPERFICIE)

# Compruebo que la categorizacion haya sido correcta:
# sample_n(Marco.Agropecuario, 20)[,c("SUPERFICIE","catSUPERFICIE")]
# sample_n(Marco.Agropecuario, 20)[,c("CANTEMP","catCANTEMP")]

# Copio y renombro las variables estratificadores
Marco.Agropecuario$X1 <- Marco.Agropecuario$catCANTEMP
Marco.Agropecuario$X2 <- Marco.Agropecuario$catSUPERFICIE

# Copio y renombro las variables objetivo
Marco.Agropecuario$Y1 <- Marco.Agropecuario$SOJA
Marco.Agropecuario$Y2 <- Marco.Agropecuario$TRIGO
Marco.Agropecuario$Y3 <- Marco.Agropecuario$BOVINOS

# Variable de control necesara para la optimizacion de estratos
Marco.Agropecuario$domainvalue = 1

# Separo Marco Agropecuario en 3 dataframes con igual cantidad de variables,
# cada uno representando un dominio
Norte <- Marco.Agropecuario[Marco.Agropecuario$REGION==1,]
Centro <- Marco.Agropecuario[Marco.Agropecuario$REGION==2,]
Sur <- Marco.Agropecuario[Marco.Agropecuario$REGION==3,]

###################################
# Creacion del dataframe con las
# CV deseados para las estimaciones
###################################

DOM <- "DOM1"
CV1 <- 0.02
CV2 <- 0.01
CV3 <- 0.02
domainvalue <- 1
CVNorte <- data.frame(DOM,CV1,CV2,CV3,domainvalue)

CV1 <- 0.01
CV2 <- 0.01
CV3 <- 0.02
CVCentro <- data.frame(DOM,CV1,CV2,CV3,domainvalue)

CV1 <- 0.05
CV2 <- 0.03
CV3 <- 0.05
CVSur <- data.frame(DOM,CV1,CV2,CV3,domainvalue)

resumenNorte <- buildStrataDF(Norte)
resumenCentro <- buildStrataDF(Centro)
resumenSur <- buildStrataDF(Sur)

# Busqueda de soluciones por algo Bethel por dominio
soluNorte <- bethel(resumenNorte,CVNorte,printa=TRUE)
soluCentro <- bethel(resumenCentro,CVCentro,printa=TRUE)
soluSur <- bethel(resumenSur,CVSur,printa=TRUE)

tablaCV <- function (solucion) {
  tempResult <- data.frame(attr(soluSur,"outcv")[,c(2,3,4)])
  
  tempResult$variable <- c("SOJA","TRIGO","BOVINOS")
  tempResult$plannedCV <- as.numeric(as.character(tempResult$PLANNED.CV))
  tempResult$actualCV <- as.numeric(as.character(tempResult$ACTUAL.CV))
  
  result <- tempResult[,c("variable","plannedCV","actualCV")]
  return(result)
}

tablaCVNorte <- tablaCV(soluNorte)
tablaCVCentro <- tablaCV(soluCentro)
tablaCVSur <- tablaCV(soluSur)

tablaEstratos <- function(solucion) {
  tempResult <- data.frame(attr(solucion,"confr")[,c(1,2,3)])
  
  tempResult$Nh <- as.numeric(as.character(tempResult$POPULATION))
  tempResult$nh <- as.numeric(as.character(tempResult$BETHEL))
  tempResult$Pik <- tempResult$nh/tempResult$Nh
  
  result <- tempResult[,c("STRATUM","Nh","nh","Pik")]
  return(result)
}

tablaEstratosNorte <- tablaEstratos(soluNorte)
tablaEstratosCentro <- tablaEstratos(soluCentro)
tablaEstratosSur <- tablaEstratos(soluSur)

# Reviso las condiciones de inestabilidad
malosEstratos <- function(tablaEstratos) {
  result <- tablaEstratos[tablaEstratos$Nh<=10 | 
                            tablaEstratos$nh<=4 | 
                            tablaEstratos$Pik==1,]
  return(result)
}
malosEstratos(tablaEstratosNorte)
malosEstratos(tablaEstratosCentro)
malosEstratos(tablaEstratosSur)


