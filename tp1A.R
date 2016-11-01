# Este script utiliza los siguientes packages:
library(moments)
library(stratification)
library(car)
library(PracTools)
library(sampling)

# Cargo la data de prueba
load("data/Prueba.Piloto.RData")
# attach(Prueba.Piloto)

# Doy nombres mas descriptivos a las variables del Piloto
# PO VARIABLE X
POpiloto = Prueba.Piloto$x
# VMP VARIABLE Y
VMPpiloto = Prueba.Piloto$y

# Cargo el Marco completo
load("data/Marco.PO.RData")
# attach(Marco.PO)

# Doy nombres mas descriptivos a las variables del Marco
POmarco = Marco.PO$PO



# Observo un poco que hay
# summary(Prueba.Piloto)
# head(Prueba.Piloto)
# tail(Prueba.Piloto)

# Histograma de PO con alfombra. Aun no veo la asimetria. TODO
hist(POmarco, density = 20, col = 'blue')

plot(density(POmarco))
rug(POmarco, col= 'orange', ticksize = 0.04, lwd = 1.2)
summary(POmarco)
asimPOmarco <- moments::skewness(POmarco)


# Ploteo 
# La siguiente linea es identica a `plot(Prueba.Piloto)`
plot(POpiloto, VMPpiloto)


# Las siguientes tres formulas devuelven resultado identicos.
# Me quedo con la primera porque me resulta mas exprsiva. 
sinOrdenada <- lm(VMPpiloto ~ POpiloto + 0)
#sinOrdenada <- lm(formula = Prueba.Piloto$y ~ Prueba.Piloto$x + 0)
#sinOrdenada <- lm(Prueba.Piloto$y ~ Prueba.Piloto$x - 1)

# Compruebo que entiendo como se calculan los residuos y fitted values del ML:
# (x1 - x2)^2 < 1e-6 pretende proteger la comparacion contra errores de precision de float
beta = sinOrdenada$coefficients["POpiloto"]
VMPhat = POpiloto * beta # 'hat' por VMP 'sombrero'
residuos = VMPpiloto - POpiloto * beta
all((sinOrdenada$fitted.values - VMPhat)^2 < 0.000001)
all((sinOrdenada$residuals - residuos)^2 < 0.000001)

# Plots indicativos de heterocedasticidad en el modelo lineal VMP ~ PO + 0
# el cuadrado de los residuos contra PO
residuosCuadrados = sinOrdenada$residuals ^ 2
plot(POpiloto, residuosCuadrados)
# Plot de los residuos del modelo lineal ofrecido por `car`
# Se aprecia claramente como el modulo de los residuos aumenta con PO
# Es identico a `plot(PO, residuals(sinOrdenada))` pero mejor editado
residualPlot(sinOrdenada)


# Computo el test de Breusch-Pagan de heterocedasticidad en el modelo:
BPTest <- ncvTest(sinOrdenada)

# BPTest$p es infimo, por lo cual debemos rechazar la hipotesis nula de homocedasticidad

# Estimacion de parametros para el algoritmo de Kozak
# Modo A: Utilizando PracTools::gammaFit
gammaA <- gammaFit(POpiloto, POpiloto, VMPpiloto)$g.hat

# Modo B: Asumiendo un modelo lineal entre log(residuosCuadrados) ~ log(x)
modeloResiduos <- lm(log(residuosCuadrados) ~ log(POpiloto))
gammaB <- modeloResiduos$coefficients["log(POpiloto)"]
sig2B <- exp(modeloResiduos$coefficients["(Intercept)"])

# Elijo estratos con Kozak
asigNeyman <- c(0.5, 0, 0.5)

# Armo vectores de no-respuesta para 2-5 estratos
rh2 <- c(rep(0.8, 1), 1)
rh3 <- c(rep(0.8, 2), 1)
rh4 <- c(rep(0.8, 3), 1)
rh5 <- c(rep(0.8, 4), 1)

### KOZAK CON MODELO LINEAR
# Tengo que elegir una de las dos estimaciones de gama. 
# Decido ser precavido y usar gammaA, que es un poco mayor y por
# ende obligara a tomar muestras un poco mas grandes para garantizar el CV objetivo

parametrosKozak <- list(beta = beta, sig2 = sig2B, gamma = gammaA)

# K{N} sera la variable que contenga los resultados de la estratificacion por Kozak para N estratos

K2 <- strata.LH(x = POmarco,
               CV = 0.02,
               Ls = 2,
               alloc = asigNeyman,
               takeall = 1,
               rh = rh2,
               model = "linear",
               model.control = parametrosKozak)

K3 <- strata.LH(x = POmarco,
               CV = 0.02,
               Ls = 3,
               alloc = asigNeyman,
               takeall = 1,
               rh = rh3,
               model = "linear",
               model.control = parametrosKozak)

K4 <- strata.LH(x = POmarco,
               CV = 0.02,
               Ls = 4,
               alloc = asigNeyman,
               takeall = 1,
               rh = rh4,
               model = "linear",
               model.control = parametrosKozak)

K5 <- strata.LH(x = POmarco,
               CV = 0.02,
               Ls = 5,
               alloc = asigNeyman,
               takeall = 1,
               rh = rh5,
               model = "linear",
               model.control = parametrosKozak)

CVestratos <- function(estratificacion) {
  CV <- sqrt(estratificacion$varh * (1/estratificacion$nh - 1/estratificacion$Nh)) / estratificacion$mean
  return(CV)
}

K2$CV <- CVestratos(K2)
K3$CV <- CVestratos(K3)
K4$CV <- CVestratos(K4)
K5$CV <- CVestratos(K5)

# Elijo 5 estratos, K5
# Asigno cada empresa al estrato que le corresponde

Estrato <- K5$stratumID
MarcoAsignado <- cbind(Marco.PO,Estrato)

Pik <- K5$nh / K5$Nh
idEstratos <- c(1,2,3,4,5)

PikPorEstrato <- cbind.data.frame(idEstratos, Pik)
colnames(PikPorEstrato) <- c("Estrato","Pik")

MarcoAsignado <- merge(MarcoAsignado, PikPorEstrato)

seleccion <- strata(MarcoAsignado,
                    stratanames="Estrato",
                    size=K5$nh,
                    method="srswor",
                    pik=MarcoAsignado$Pik)

muestra <- getdata(MarcoAsignado,seleccion)

write.csv(muestra, file = 'data/muestraA.csv')

save.image(file = ".RDataParteA")
