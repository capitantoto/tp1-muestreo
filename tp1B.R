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
Marco.Agropecuario$X1 <- as.factor(Marco.Agropecuario$catCANTEMP)
Marco.Agropecuario$X2 <- as.factor(Marco.Agropecuario$catSUPERFICIE)

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

################################################################################
#                            Solucion por algoritmos geneticos
################################################################################

# Agrego REGION como variable estratificadora y definidora de dominios
Marco.Agropecuario$X3 <- as.factor(Marco.Agropecuario$REGION)
Marco.Agropecuario$domainvalue <- Marco.Agropecuario$REGION

##########################################
# Efectivos, Promedios y STD en cada
# Micro Estrato para pasar a la Optimizacion
##########################################
resumenMarco <- buildStrataDF(Marco.Agropecuario)


########################
# Precisiones deseadas 
# sobre las variables Y
# en cada dominio
########################
DOM <- rep("DOM1",3)
CV1 <- c(0.02, 0.01, 0.05)
CV2 <- c(0.01, 0.01, 0.03)
CV3 <- c(0.02, 0.02, 0.05)
domainvalue <- c(1, 2, 3)
CVobjetivo <- data.frame(DOM, CV1, CV2, CV3, domainvalue)
CVobjetivo

##############################
#    Ajuste de parametros    #
##############################

# Considero seis juegos de parametros a probar, combinando 3 variantes de mut_chance con 2 de elitism_rate
# Los manoseo un poco para ubicarlos en un data frame
prueba1 <- c("prueba1", 0.05, 0.1)
prueba2 <- c("prueba2", 0.05, 0.2)
prueba3 <- c("prueba3", 0.10, 0.1)
prueba4 <- c("prueba4", 0.10, 0.2)
prueba5 <- c("prueba5", 0.20, 0.1)
prueba6 <- c("prueba6", 0.20, 0.2)

parametrosAProbar <- array(t(c(prueba1,
                             prueba2,
                             prueba3,
                             prueba4,
                             prueba5,
                             prueba6)),
                           c(3,6))
parametrosAProbar <- as.data.frame(t(parametrosAProbar))
colnames(parametrosAProbar) <- c("dirname","mut_chance","elitism_rate")
parametrosAProbar$mut_chance <- as.numeric(as.character(parametrosAProbar$mut_chance))
parametrosAProbar$elitism_rate <- as.numeric(as.character(parametrosAProbar$elitism_rate))

# La siguiente funcion intenta obtener agrupamientos optimos de estratos con poblaciones de 200 individuos, 
# durante 100 iteraciones. Luego, actualiza el marco original con los nuevos estratos, provee un resumen de las
# cantidades mas relevantes, y finalmente evalua la solucion en 100 muestras

probarParametros <- function(mut_chance, elitism_rate) {
  
  prueba <- list()
  prueba$solu <- optimizeStrata(strata=resumenMarco,
                                errors=CVobjetivo,                        
                                iter=200,
                                pops=100,
                                initialStrata=5,
                                mut_chance=mut_chance,
                                elitism_rate=elitism_rate,
                                minnumstr=5,
                                writeFiles=TRUE)
  
  prueba$nuevosEstratos <- updateStrata(resumenMarco, prueba$solu, writeFile=TRUE)
  
  prueba$nuevoMarco <- updateFrame(Marco.Agropecuario,prueba$nuevosEstratos)
  
  # Resumen aspectos centrales
  prueba$resumen <- data.frame(mut_chance,elitism_rate)
  prueba$resumen <- cbind(prueba$resumen,t(tapply(prueba$solu$aggr_strata$STRATO,prueba$solu$aggr_strata$DOM1,function(x) length(unique(x)))))
  prueba$resumen <- cbind(prueba$resumen,t(tapply(prueba$solu$aggr_strata$SOLUZ,prueba$solu$aggr_strata$DOM1,sum)))
  
  colnames(prueba$resumen) <- c("mut_chance","elitism_rate","hNorte","hCentro","hSur","nNorte","nCentro","nSur")
  
  prueba$resumen$H <- prueba$resumen$hNorte + prueba$resumen$hCentro + prueba$resumen$hSur
  prueba$resumen$N <- prueba$resumen$nNorte + prueba$resumen$nCentro + prueba$resumen$nSur
  
  prueba$eval <- evalSolution(prueba$nuevoMarco,prueba$solu$aggr_strata,nsampl=100,writeFiles=TRUE)
  
  prueba$CVesperado <-read.csv("expected_cv.csv")
  prueba$CVesperado <- cbind(prueba$CVesperado,CVobjetivo)
  
  return(prueba)
}


pruebas <- list()

for (i in 1:6) {
  row <- parametrosAProbar[i,]
  dirname <- as.character(row$dirname)
  # Creo un directorio para los archivos de trabajo si aun no existe
  dir.exists(dirname) || dir.create(dirname)
  setwd(dirname)

    pruebas[[i]] <- probarParametros(mut_chance = row$mut_chance,
                                 elitism_rate = row$elitism_rate)
  setwd("..")
}

### Seleccion de la muestra bajo MSA
# muestra <- selectSample(nuevoMarco1,soluGen1$aggr_strata)
## Resumen
# Total Pob
# sum(muestra$WEIGHTS)
# Total Pob por dominio
# tapply(muestra$WEIGHTS,muestra$DOMAINVALUE,sum)
# Total de muestra por dominio
# table(muestra$DOMAINVALUE)

setwd("..")

### Seleccion de la muestra bajo MSA
# muestra <- selectSample(nuevoMarco1,prueba$solu$aggr_strata)
## Resumen
# Total Pob
# sum(muestra$WEIGHTS)
# Total Pob por dominio
# tapply(muestra$WEIGHTS,muestra$DOMAINVALUE,sum)
# Total de muestra por dominio
# table(muestra$DOMAINVALUE)

