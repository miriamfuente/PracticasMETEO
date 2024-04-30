options(java.parameters = "-Xmx32g") 
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

################################
#### FUNCIÓN COMPOUND EVENTS ###
################################

source(functionCE.R)


##############
## OBS DATA ##
##############
# obs.subset <- subset(df, product == 'AEMET-5KM-regular')
# obs.loc <- as.character(obs.subset$location)
# di.obs <- dataInventory(obs.loc)
# tmax <- loadGridData(obs.loc, var="tasmax", years= 1986:2005,lonLim =c(-10, 5), latLim = c(35,44))
# pr <- loadGridData(obs.loc, var="pr", years= 1986:2005,lonLim =c(-10, 5), latLim = c(35,44))

# # Reescalo
# pr <- upscaleGrid(pr, times = 2, aggr.fun = list(FUN = mean, na.rm = TRUE))
# tmax <- upscaleGrid(tmax, times = 2, aggr.fun = list(FUN = mean, na.rm = TRUE))

obs.pr <- readRDS("obs.pr.rds")

# # Compound Events Observations
# obs.CE <- calculoCE(pr, tmax, years = seq(1986, 2005, by = 1))



png("obs.CE.png")
spatialPlot(climatology(obs.CE, clim.fun = list(FUN = "sum", na.rm = TRUE)), backdrop.theme = "coastline")
dev.off()


###########################
## MODELO HISTORICO DATA ##
###########################
rcm.hist.subset <- subset(df, activity == 'CORDEX' & domain=='EUR-11' & experiment == 'historical' & variable=='tasmax' & rcm =='RACMO22E' & model == 'ICHEC-EC-EARTH' & ensemble == 'r1i1p1') 
rcm.hist.loc <- as.character(rcm.hist.subset $location)
tmax.hist <- loadGridData(rcm.hist.loc, var="tasmax", years= 1986:2005,lonLim =c(-10, 5), latLim = c(35,44))
# Comprobar las unidades de los datos
message("model units ",getGridUnits(tmax.hist)) # degC para temperaturas, mm ó kg*m-2 para precipitación
# Pasar a grados Celsius
tmax.hist <- gridArithmetics(tmax.hist, 273.15, operator="-")

rcm.hist.subset <- subset(df, activity == 'CORDEX' & domain=='EUR-11' & experiment == 'historical' & variable=='pr' & rcm =='RACMO22E' & model == 'ICHEC-EC-EARTH' & ensemble == 'r1i1p1') 
rcm.hist.loc <- as.character(rcm.hist.subset $location)
pr.hist <- loadGridData(rcm.hist.loc, var="pr", years= 1986:2005,lonLim =c(-10, 5), latLim = c(35,44))
# Compound Events Historical
hist.CE <- calculoCE(pr.hist, tmax.hist, years = seq(1986, 2005, by = 1))

png("hist.CE.png")
spatialPlot(climatology(hist.CE, clim.fun = list(FUN = "sum", na.rm = TRUE)), backdrop.theme = "coastline")
dev.off()

        ###########################
        ## APLICO MÁSCARA TIERRA ##
        ###########################
rcm.mask <- subset(df, activity == 'CORDEX' & domain=='EUR-11' & experiment == 'historical' & variable=='sftlf' & rcm =='RACMO22E' & model == 'ICHEC-EC-EARTH' & ensemble == 'r1i1p1') 
rcm.mask.loc <- as.character(rcm.mask$location)
rcm.mask <- loadGridData(rcm.mask.loc, var="sftlf", lonLim =c(-10, 5), latLim = c(35,44))
# En este modelo, valores entre 0 y 100 de land area fraction, establecemos un umbral para decir qué es tierra y qué es mar

rcm.mask$Data[rcm.mask$Data < 40] <- NA
rcm.mask$Data[rcm.mask$Data >= 40] <- 1 # OJO! puede que en el algún modelo venga en tanto por 1 en lugar de %. Adaptar.

# Multiplicamos los datos del modelo por su máscara tierra-mar. Pondrá NA en los puntos de mar.
# Se hace una multiplicación día a día de los datos del modelo por la máscara.
time <- getRefDates(pr.hist)
nt <- length(time)
ls <-lapply(1:nt, function(i){
  timei <-subsetDimension(pr.hist, dimension = "time", indices=i) # separa día a día 
  gridArithmetics(timei, rcm.mask, operator = "*") # multiplica los dos grids
}
)
rcm.hist.masked <- bindGrid(ls, dimension = "time") # bindGrid es una función de transforeR que vuelve a unir todos los días en un grid de climate4R
rm(hist.data, ls, rcm.mask)

png("hist.CE.tierra.png")
spatialPlot(climatology(rcm.hist.masked,  clim.fun = list(FUN = "sum")) , backdrop.theme = "coastline", main= "CE de Iberia (solo tierra) simulada")# lo pinta en el Ecuador, donde está centrada la malla regular del modelo
dev.off()

        ############################################################
        # *** Interpolar el modelo a la malla de la observación ****
        ############################################################
# Este paso lo hace automáticamente biasCorrection, pero lo hago aquí para representar el modelo en la misma malla que la observación y calcular el sesgo (bias)
rcm.hist.interp <- interpGrid(rcm.hist.masked, new.coordinates = getGrid(obs.pr))

png("interpolacion_EUR_11.png")
spatialPlot(makeMultiGrid(climatology(obs.pr),climatology(rcm.hist.interp)) , backdrop.theme = "coastline", 
            rev.colors = TRUE,  at= seq(0,25), main= "CE en Iberia", as.table=TRUE, 
            names.attr=c("OBS", "RCM interpolado"), layout=c(2,1)) # pinta ambos en las latitudes que tocan
dev.off()

########################
### BIAS CORRECTION ####
########################
