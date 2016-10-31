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

#############################
#       Optimizacion        #
#############################
dirname  <-  "soluGen1"
# Creo un directorio para los archivos de trabajo si aun no existe
dir.exists(dirname) || dir.create(dirname)
setwd(dirname)

soluGen1 <- optimizeStrata(strata=resumenMarco,
                    errors=CVobjetivo,                        
                    iter=200,
                    pops=50,
                    initialStrata=10,
                    mut_chance=0.05,
                    elitism_rate=0.10,
                    minnumstr=5,
                    writeFiles=TRUE)


nuevosEstratosGen1 <- updateStrata(resumenMarco,soluGen1,writeFile=TRUE)

nuevoMarco1 <- updateFrame(Marco.Agropecuario,nuevosEstratosGen1)

nuevosEstratosGen1Ordenado <- nuevosEstratosGen1[order(DOM1,LABEL),]

# Muestra por Estrato
tapply(soluGen1$aggr_strata$SOLUZ,soluGen1$aggr_strata$DOM1,sum)

# Evaluacion de la solucion sobre un conjunto de muestras

evalSolution(nuevoMarco1,soluGen1$aggr_strata,nsampl=1000,writeFiles=TRUE)

CV_esperados1 <- read.csv("expected_cv.csv")

CV_esperados1 <- cbind(CV_esperados1,CVobjetivo)

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

dirname  <-  "soluGen2"
# Creo un directorio para los archivos de trabajo si aun no existe
dir.exists(dirname) || dir.create(dirname)
setwd(dirname)

soluGen2 <- optimizeStrata(strata=resumenMarco,
                    errors=CVobjetivo,                        
                    iter=200,
                    pops=50,
                    initialStrata=10,
                    mut_chance=0.05,
                    elitism_rate=0.20,
                    minnumstr=5,
                    writeFiles=TRUE)


nuevosEstratosGen2 <- updateStrata(resumenMarco,soluGen2,writeFile=TRUE)

nuevoMarco2 <- updateFrame(Marco.Agropecuario,nuevosEstratosGen2)

nuevosEstratosGen2Ordenado <- nuevosEstratosGen2[order(DOM1,LABEL),]

# Muestra por Estrato
tapply(soluGen2$aggr_strata$SOLUZ,soluGen2$aggr_strata$DOM1,sum)

# Evaluacion de la solucion sobre un conjunto de muestras

evalSolution(nuevoMarco2,soluGen2$aggr_strata,nsampl=1000,writeFiles=TRUE)

CV_esperados2 <- read.csv("expected_cv.csv")

CV_esperados2 <- cbind(CV_esperados2,CVobjetivo)

### Seleccion de la muestra bajo MSA
# muestra <- selectSample(nuevoMarco2,soluGen2$aggr_strata)
## Resumen
# Total Pob
# sum(muestra$WEIGHTS)
# Total Pob por dominio
# tapply(muestra$WEIGHTS,muestra$DOMAINVALUE,sum)
# Total de muestra por dominio
# table(muestra$DOMAINVALUE)

setwd("..")

dirname  <-  "soluGen3"
# Creo un directorio para los archivos de trabajo si aun no existe
dir.exists(dirname) || dir.create(dirname)
setwd(dirname)

soluGen3 <- optimizeStrata(strata=resumenMarco,
                    errors=CVobjetivo,                        
                    iter=200,
                    pops=50,
                    initialStrata=10,
                    mut_chance=0.05,
                    elitism_rate=0.20,
                    minnumstr=5,
                    writeFiles=TRUE)


nuevosEstratosGen3 <- updateStrata(resumenMarco,soluGen3,writeFile=TRUE)

nuevoMarco3 <- updateFrame(Marco.Agropecuario,nuevosEstratosGen3)

nuevosEstratosGen3Ordenado <- nuevosEstratosGen3[order(DOM1,LABEL),]

# Muestra por Estrato
tapply(soluGen3$aggr_strata$SOLUZ,soluGen3$aggr_strata$DOM1,sum)

# Evaluacion de la solucion sobre un conjunto de muestras

evalSolution(nuevoMarco3,soluGen3$aggr_strata,nsampl=1000,writeFiles=TRUE)

CV_esperados3 <- read.csv("expected_cv.csv")

CV_esperados3 <- cbind(CV_esperados3,CVobjetivo)

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

dirname  <-  "soluGen4"
# Creo un directorio para los archivos de trabajo si aun no existe
dir.exists(dirname) || dir.create(dirname)
setwd(dirname)

soluGen4 <- optimizeStrata(strata=resumenMarco,
                    errors=CVobjetivo,                        
                    iter=200,
                    pops=50,
                    initialStrata=10,
                    mut_chance=0.05,
                    elitism_rate=0.20,
                    minnumstr=5,
                    writeFiles=TRUE)


nuevosEstratosGen4 <- updateStrata(resumenMarco,soluGen4,writeFile=TRUE)

nuevoMarco4 <- updateFrame(Marco.Agropecuario,nuevosEstratosGen4)

nuevosEstratosGen4Ordenado <- nuevosEstratosGen4[order(DOM1,LABEL),]

# Muestra por Estrato
tapply(soluGen4$aggr_strata$SOLUZ,soluGen4$aggr_strata$DOM1,sum)

# Evaluacion de la solucion sobre un conjunto de muestras

evalSolution(nuevoMarco4,soluGen4$aggr_strata,nsampl=1000,writeFiles=TRUE)

CV_esperados4 <- read.csv("expected_cv.csv")

CV_esperados4 <- cbind(CV_esperados4,CVobjetivo)

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

dirname  <-  "soluGen5"
# Creo un directorio para los archivos de trabajo si aun no existe
dir.exists(dirname) || dir.create(dirname)
setwd(dirname)

soluGen5 <- optimizeStrata(strata=resumenMarco,
                    errors=CVobjetivo,                        
                    iter=200,
                    pops=50,
                    initialStrata=10,
                    mut_chance=0.05,
                    elitism_rate=0.20,
                    minnumstr=5,
                    writeFiles=TRUE)


nuevosEstratosGen5 <- updateStrata(resumenMarco,soluGen5,writeFile=TRUE)

nuevoMarco5 <- updateFrame(Marco.Agropecuario,nuevosEstratosGen5)

nuevosEstratosGen5Ordenado <- nuevosEstratosGen5[order(DOM1,LABEL),]

# Muestra por Estrato
tapply(soluGen5$aggr_strata$SOLUZ,soluGen5$aggr_strata$DOM1,sum)

# Evaluacion de la solucion sobre un conjunto de muestras

evalSolution(nuevoMarco5,soluGen5$aggr_strata,nsampl=1000,writeFiles=TRUE)

CV_esperados5 <- read.csv("expected_cv.csv")

CV_esperados5 <- cbind(CV_esperados5,CVobjetivo)

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

dirname  <-  "soluGen6"
# Creo un directorio para los archivos de trabajo si aun no existe
dir.exists(dirname) || dir.create(dirname)
setwd(dirname)

soluGen6 <- optimizeStrata(strata=resumenMarco,
                    errors=CVobjetivo,                        
                    iter=200,
                    pops=50,
                    initialStrata=10,
                    mut_chance=0.05,
                    elitism_rate=0.20,
                    minnumstr=5,
                    writeFiles=TRUE)


nuevosEstratosGen6 <- updateStrata(resumenMarco,soluGen6,writeFile=TRUE)

nuevoMarco6 <- updateFrame(Marco.Agropecuario,nuevosEstratosGen6)

nuevosEstratosGen6Ordenado <- nuevosEstratosGen6[order(DOM1,LABEL),]

# Muestra por Estrato
tapply(soluGen6$aggr_strata$SOLUZ,soluGen6$aggr_strata$DOM1,sum)

# Evaluacion de la solucion sobre un conjunto de muestras

evalSolution(nuevoMarco6,soluGen6$aggr_strata,nsampl=1000,writeFiles=TRUE)

CV_esperados6 <- read.csv("expected_cv.csv")

CV_esperados6 <- cbind(CV_esperados6,CVobjetivo)

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

probarParametros <- function(mut_chance, elitism_rate) {

  prueba <- list()
  prueba$solu <- optimizeStrata(strata=resumenMarco,
                    errors=CVobjetivo,                        
                    iter=20,
                    pops=10,
                    initialStrata=10,
                    mut_chance=mut_chance,
                    elitism_rate=elitism_rate,
                    minnumstr=5,
                    writeFiles=TRUE)

  prueba$nuevosEstratos <- updateStrata(resumenMarco, prueba$solu, writeFile=TRUE)

  prueba$nuevoMarco <- updateFrame(Marco.Agropecuario,prueba$nuevosEstratos)

# Muestra por Estrato
  prueba$resumen <- data.frame(mut_chance,elitism_rate)
  prueba$resumen <- cbind(resumen,t(tapply(soluGen1$aggr_strata$STRATO,soluGen1$aggr_strata$DOM1,function(x) length(unique(x)))))
  prueba$resumen <- cbind(resumen,t(tapply(soluGen1$aggr_strata$SOLUZ,soluGen1$aggr_strata$DOM1,sum)))

  colnames(prueba$resumen) <- c("mut_chance","elitism_rate","hNorte","hCentro","hSur","nNorte","nCentro","nSur")
  
  prueba$resumen$H <- resumen$hNorte + resumen$hCentro + resumen$hSur
  prueba$resumen$N <- resumen$nNorte + resumen$nCentro + resumen$nSur

  prueba$eval <- evalSolution(prueba$nuevoMarco,prueba$solu$aggr_strata,nsampl=100,writeFiles=TRUE)

  prueba$CVesperado <-read.csv("expected_cv.csv")
  prueba$CVesperado <- cbind(CVesperado,CVobjetivo)

  return(prueba)
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

