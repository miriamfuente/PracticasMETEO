options(java.parameters = "-Xmx75g") 
library(loadeR)
library(transformeR)
library(visualizeR)
library(climate4R.UDG)
library(dplyr)
library(climate4R.value)
library(VALUE)
library(downscaleR) # para bias correction (función biasCorrection)
library(climate4R.indices) # para calcular índices


# Leer el archivo CSV
df <- read.csv("https://data.meteo.unican.es/inventory.csv") # inventario con muchos datasets
# Filtrar los enlaces que necesitas
#############################
# *** Leer observaciones ****
#############################
obs.subset <- subset(df, product == 'AEMET-5KM-regular')
obs.loc <- as.character(obs.subset$location)
di.obs <- dataInventory(obs.loc)
obs.data <- loadGridData(obs.loc, var="tasmax", years= 1986:2005,lonLim =c(-10, 5), latLim = c(35,44))

png("obs.png")
spatialPlot(climatology(obs.data), backdrop.theme = "coastline", rev.colors = TRUE, at= seq(0,25), 
            main= "Temperatura maxima observada")
dev.off()

#########################
# *** Leer un modelo ****
#########################
rcm.hist.subset <- subset(df, activity == 'CORDEX' & domain=='EUR-11' & experiment == 'historical' & variable=='tasmax' & rcm =='RACMO22E' & model == 'ICHEC-EC-EARTH' & ensemble == 'r1i1p1')
# rcm.hist.subset <- subset(df, activity == 'CORDEX' & domain=='EUR-44' & experiment == 'historical' & variable=='tasmax' & rcm =='RACMO22E' & model == 'ICHEC-EC-EARTH' & ensemble == 'r1i1p1') 
rcm.hist.loc <- as.character(rcm.hist.subset $location)
hist.data <- loadGridData(rcm.hist.loc, var="tasmax", years= 1986:2005,lonLim =c(-10, 5), latLim = c(35,44))
# Comprobar las unidades de los datos
message("model units ",getGridUnits(hist.data)) # degC para temperaturas, mm ó kg*m-2 para precipitación
# Pasar a grados Celsius
hist.data <- gridArithmetics(hist.data, 273.15, operator="-")

png("hist.model.png")
spatialPlot(climatology(hist.data) , backdrop.theme = "coastline", rev.colors = TRUE,  at= seq(0,25),
            main= "Temperatura máxima de Iberia simulada") # lo pinta en el Ecuador, donde está centrada la malla regular del modelo
dev.off()

##############################################
# *** Aplicar máscara de tierra al modelo ****
##############################################
rcm.mask <- subset(df, activity == 'CORDEX' & domain=='EUR-44' & experiment == 'historical' & variable=='sftlf' & rcm =='RACMO22E' & model == 'ICHEC-EC-EARTH' & ensemble == 'r1i1p1') 
rcm.mask.loc <- as.character(rcm.mask$location)
rcm.mask <- loadGridData(rcm.mask.loc, var="sftlf", lonLim =c(-10, 5), latLim = c(35,44))
# En este modelo, valores entre 0 y 100 de land area fraction, establecemos un umbral para decir qué es tierra y qué es mar

png("tierra.png")
spatialPlot(rcm.mask) # porcentaje de tierra de cada pixel
dev.off()

rcm.mask$Data[rcm.mask$Data < 40] <- NA
rcm.mask$Data[rcm.mask$Data >= 40] <- 1 # OJO! puede que en el algún modelo venga en tanto por 1 en lugar de %. Adaptar.

png("tierra2.png")
spatialPlot(rcm.mask) # 1's en tierra, NA's en mar
dev.off()

# Multiplicamos los datos del modelo por su máscara tierra-mar. Pondrá NA en los puntos de mar.
# Se hace una multiplicación día a día de los datos del modelo por la máscara.
time <- getRefDates(hist.data)
nt <- length(time)
ls <-lapply(1:nt, function(i){
  timei <-subsetDimension(hist.data, dimension = "time", indices=i) # separa día a día 
  gridArithmetics(timei, rcm.mask, operator = "*") # multiplica los dos grids
}
)
rcm.hist.masked <- bindGrid(ls, dimension = "time") # bindGrid es una función de transforeR que vuelve a unir todos los días en un grid de climate4R
rm(hist.data, ls, rcm.mask)

png("tierra3.png")
spatialPlot(climatology(rcm.hist.masked) , backdrop.theme = "coastline", rev.colors = TRUE,  
            at= seq(0,25), main= "Temperatura máxima de Iberia (solo tierra) simulada")# lo pinta en el Ecuador, donde está centrada la malla regular del modelo
dev.off()

############################################################
# *** Interpolar el modelo a la malla de la observación ****
############################################################
# Este paso lo hace automáticamente biasCorrection, pero lo hago aquí para representar el modelo en la misma malla que la observación y calcular el sesgo (bias)
rcm.hist.interp <- interpGrid(rcm.hist.masked, new.coordinates = getGrid(obs.data))

png("interpolacion.png")
spatialPlot(makeMultiGrid(climatology(obs.data),climatology(rcm.hist.interp)) , backdrop.theme = "coastline", 
            rev.colors = TRUE,  at= seq(0,25), main= "Temperatura máxima en Iberia", as.table=TRUE, 
            names.attr=c("OBS", "RCM interpolado"), layout=c(2,1)) # pinta ambos en las latitudes que tocan
dev.off()

##########################
# *** Bias correction ****
##########################
bc.scaling.hist <- biasCorrection(y=obs.data, x=rcm.hist.interp, newdata= rcm.hist.interp, precipitation = FALSE, 
                             method="scaling", scaling.type="additive" ) # correción solo de la media (aditiva para temperatura)
bc.eqm.hist <- biasCorrection(y=obs.data, x=rcm.hist.interp, newdata= rcm.hist.interp, precipitation = FALSE, 
                         method="eqm", extrapolation = "constant", n.quantiles=99 )  # corrección de 99 percentiles

# Calcular el sesgo de los datos sin corregir (raw) y de las dos correcciones en la media
bias.raw.mean.hist <- gridArithmetics(climatology(rcm.hist.interp), climatology(obs.data), operator="-")
bias.scaling.mean.hist <- gridArithmetics(climatology(bc.scaling.hist), climatology(obs.data), operator="-")
bias.eqm.mean.hist <- gridArithmetics(climatology(bc.eqm.hist), climatology(obs.data), operator="-")

png("sesgo3.png", width=1000, height=500)
spatialPlot(makeMultiGrid(bias.raw.mean.hist, bias.scaling.mean.hist, bias.eqm.mean.hist), backdrop.theme = "coastline", 
            rev.colors = TRUE,  at= seq(-6,6), main= "Sesgo en la temperatura máxima (promedio)", layout=c(3,1),
            as.table=TRUE, names.attr=c("RAW", "BC (scaling)", "BC (eqm)"))
dev.off()

###############################################
# *** Bias correction del escenario futuro ****
###############################################
# TAREA: Hacer lo mismo, pero esta vez aplicar la correción a las simulaciones de futuro (argumento newdata)
rcm.rcp85.subset <- subset(df, activity == 'CORDEX' & domain=='EUR-44' & experiment == 'rcp85' & variable=='tasmax' & rcm =='RACMO22E' & model == 'ICHEC-EC-EARTH' & ensemble == 'r1i1p1') 
rcm.rcp85.loc <- as.character(rcm.rcp85.subset $location)

rcp85.data <- loadGridData(rcm.rcp85.loc, var="tasmax", years= 2050:2080,lonLim =c(-10, 5), latLim = c(35,44))
# Comprobar las unidades de los datos
message("model units ",getGridUnits(rcp85.data)) # degC para temperaturas, mm ó kg*m-2 para precipitación
# Pasar a grados Celsius
rcp85.data <- gridArithmetics(rcp85.data, 273.15, operator="-")

png("futuro.png")
spatialPlot(climatology(rcp85.data) , backdrop.theme = "coastline", rev.colors = TRUE,  at= seq(0,25),
            main= "Temperatura máxima de Iberia simulada") # lo pinta en el Ecuador, donde está centrada la malla regular del modelo
dev.off()

##############################################
# *** Aplicar máscara de tierra al modelo ****
##############################################
rcm.mask <- subset(df, activity == 'CORDEX' & domain=='EUR-44' & experiment == 'rcp85' & variable=='sftlf' & rcm =='RACMO22E' & model == 'ICHEC-EC-EARTH' & ensemble == 'r1i1p1') 
rcm.mask.loc <- as.character(rcm.mask$location)
rcm.mask <- loadGridData(rcm.mask.loc, var="sftlf", lonLim =c(-10, 5), latLim = c(35,44))
# En este modelo, valores entre 0 y 100 de land area fraction, establecemos un umbral para decir qué es tierra y qué es mar

spatialPlot(rcm.mask) # porcentaje de tierra de cada pixel

rcm.mask$Data[rcm.mask$Data < 40] <- NA
rcm.mask$Data[rcm.mask$Data >= 40] <- 1 # OJO! puede que en el algún modelo venga en tanto por 1 en lugar de %. Adaptar.
spatialPlot(rcm.mask) # 1's en tierra, NA's en mar

# Multiplicamos los datos del modelo por su máscara tierra-mar. Pondrá NA en los puntos de mar.
# Se hace una multiplicación día a día de los datos del modelo por la máscara.
time <- getRefDates(rcp85.data)
nt <- length(time)
ls <-lapply(1:nt, function(i){
  timei <-subsetDimension(rcp85.data, dimension = "time", indices=i) # separa día a día 
  gridArithmetics(timei, rcm.mask, operator = "*") # multiplica los dos grids
}
)
rcm.rcp85.masked <- bindGrid(ls, dimension = "time") # bindGrid es una función de transforeR que vuelve a unir todos los días en un grid de climate4R
rm(rcp85.data, ls, rcm.mask)

png("futuro2.png")
spatialPlot(climatology(rcm.rcp85.masked) , backdrop.theme = "coastline", rev.colors = TRUE,  
            at= seq(0,25), main= "Temperatura máxima de Iberia (solo tierra) simulada")# lo pinta en el Ecuador, donde está centrada la malla regular del modelo
dev.off()

############################################################
# *** Interpolar el modelo a la malla de la observación ****
############################################################
# Este paso lo hace automáticamente biasCorrection, pero lo hago aquí para representar el modelo en la misma malla que la observación y calcular el sesgo (bias)
rcm.rcp85.interp <- interpGrid(rcm.rcp85.masked, new.coordinates = getGrid(obs.data))

png("futuro3.png")
spatialPlot(makeMultiGrid(climatology(obs.data),climatology(rcm.rcp85.interp), skip.temporal.check = TRUE) , backdrop.theme = "coastline", 
            rev.colors = TRUE,  at= seq(0,25), main= "Temperatura máxima en Iberia", as.table=TRUE, 
            names.attr=c("OBS", "RCM interpolado"), layout=c(2,1)) # pinta ambos en las latitudes que tocan
dev.off()


#########################
# *** PRUEBA MIRIAM *** #
png("interpolacion_obs_hist_future.png", width=1300, height=800)
spatialPlot(makeMultiGrid(climatology(obs.data),climatology(rcm.hist.interp),climatology(rcm.rcp85.interp), skip.temporal.check = TRUE) , backdrop.theme = "coastline", 
            rev.colors = TRUE,  at= seq(0,25), main= "Temperatura máxima en Iberia", as.table=TRUE, 
            names.attr=c("OBS", "RCM hist interpolado", "RCM rcp85 interpolado"), layout=c(3,1)) # pinta ambos en las latitudes que tocan
dev.off()


##########################
# *** Bias correction ****
##########################
bc.scaling.fut <- biasCorrection(y=obs.data, x=rcm.hist.interp, newdata= rcm.rcp85.interp, precipitation = FALSE, 
                             method="scaling", scaling.type="additive" ) # correción solo de la media (aditiva para temperatura)
bc.eqm.fut <- biasCorrection(y=obs.data, x=rcm.hist.interp, newdata= rcm.rcp85.interp, precipitation = FALSE, 
                         method="eqm", extrapolation = "constant", n.quantiles=99 )  # corrección de 99 percentiles


bias.raw.mean.fut <- gridArithmetics(climatology(rcm.rcp85.interp), climatology(rcm.hist.interp), operator="-")
bias.scaling.mean.fut <- gridArithmetics(climatology(bc.scaling.fut), climatology(bc.scaling.hist), operator="-")
bias.eqm.mean.fut <- gridArithmetics(climatology(bc.eqm.fut), climatology(bc.eqm.hist), operator="-")

png("sesgo_future.png", width=1300, height=800)
spatialPlot(makeMultiGrid(bias.raw.mean.fut, bias.scaling.mean.fut, bias.eqm.mean.fut), backdrop.theme = "coastline", 
            rev.colors = TRUE,  at= seq(-6,6), main= "Sesgo en la temperatura máxima (promedio)", layout=c(3,2),
            as.table=TRUE, names.attr=c("RAW", "BC (scaling)", "BC (eqm)"))
dev.off()