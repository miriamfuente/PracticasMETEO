options(java.parameters = "-Xmx64g")
library(loadeR)
library(transformeR)
library(visualizeR)
library(climate4R.UDG)
library(dplyr)
library(climate4R.value)
library(VALUE)
library(downscaleR) # para bias correction (función biasCorrection)
library(climate4R.indices) # para calcular índices

# Read the inventory file
df <- read.csv("https://data.meteo.unican.es/inventory.csv")

# Conditions for the analysis
var.to.study <- "tasmax" # The variable to be analyzed. It could be "tasmin", "tasmax" or "pr".
years <- 1951:2021 # The years to be analyzed.
method <- 1 # It could be 1 or 2. The first one to select the 5 previous months to the current month, the second one to select the 3 previous and 3 next months to the current month.
months.names <- c("january", "february", "march", "april", "may", "june", "july", "august", "september", "october", "november", "december")

name.path.obs <- paste("/oceano/gmeteo/users/fuentesm/AEMET/TFM/", var.to.study, ".obs.DD.", min(years), "-", max(years), ".rds", sep="")
name.path.raw <- paste("/oceano/gmeteo/users/fuentesm/AEMET/TFM/", var.to.study, ".rcm.raw.DD.", min(years), "-", max(years), ".rds", sep="")
name.path.bc <- paste("/oceano/gmeteo/users/fuentesm/AEMET/TFM/", var.to.study, ".rcm.eqm.DD.", min(years), "-", max(years), ".rds", sep="")

if (var.to.study == "tasmin" | var.to.study == "tasmax") {
  precipitation <- FALSE
} else if (var.to.study == "pr") {
  precipitation <- TRUE
}

################################
## Read the OBSERVATIONS Data ##
################################
obs.subset <- subset(df, product == 'AEMET-5KM-regular')
obs.loc <- as.character(obs.subset$location)
di.obs <- dataInventory(obs.loc)
obs.data <- loadGridData(obs.loc, var=var.to.study, years= years, lonLim =c(-10, 5), latLim = c(35,44))
obs.data <- upscaleGrid(obs.data, times = 2, aggr.fun=list(FUN=mean, na.rm=TRUE))

rm(obs.subset, obs.loc, di.obs)

saveRDS(obs.data, name.path.obs, compress="xz")

#######################
## Read the RCM Data ##
#######################
rcm.hist.subset <- subset(df, activity == 'CORDEX' & domain=='EUR-11' & experiment == 'historical' & variable==var.to.study & rcm =='RACMO22E' & model == 'ICHEC-EC-EARTH' & ensemble == 'r1i1p1') 
rcm.hist.loc <- as.character(rcm.hist.subset $location)

hist.data <- loadGridData(rcm.hist.loc, var=var.to.study, years= years,lonLim =c(-10, 5), latLim = c(35,44))

# Units of the model data
message("model units ",getGridUnits(hist.data)) 

if (var.to.study == "tasmin" | var.to.study == "tasmax") {
  hist.data <- gridArithmetics(hist.data, 273.15, operator="-")
}else if (var.to.study == "pr") {
  hist.data <- gridArithmetics(hist.data, 86400, operator="*")
}
rm(rcm.hist.subset, rcm.hist.loc)

############################################
## Apply the EARTH MASK to the model data ##
############################################
rcm.mask <- subset(df, activity == 'CORDEX' & domain=='EUR-11' & experiment == 'historical' & variable=='sftlf' & rcm =='RACMO22E' & model == 'ICHEC-EC-EARTH' & ensemble == 'r1i1p1') 
rcm.mask.loc <- as.character(rcm.mask$location)
rcm.mask <- loadGridData(rcm.mask.loc, var="sftlf", lonLim =c(-10, 5), latLim = c(35,44))

rcm.mask$Data[rcm.mask$Data < 40] <- NA 
rcm.mask$Data[rcm.mask$Data >= 40] <- 1 # In some models the mask is in %, in others in fraction. Adapt.

time <- getRefDates(hist.data)
nt <- length(time)
ls <-lapply(1:nt, function(i){
  timei <-subsetDimension(hist.data, dimension = "time", indices=i) # split day by day
  gridArithmetics(timei, rcm.mask, operator = "*") # multiply the two grids
}
)
rcm.hist.masked <- bindGrid(ls, dimension = "time") # bindGrid es una función de transforeR que vuelve a unir todos los días en un grid de climate4R
rm(hist.data, ls, rcm.mask)


#############################################################
## Interpolation of the model data to the observation grid ##
#############################################################
rcm.hist.interp <- interpGrid(rcm.hist.masked, new.coordinates = getGrid(obs.data))
# Apply the mask of the observation grid data.
mask.obs <- climatology(obs.data)
mask.obs$Data[!is.na(mask.obs$Data)] <- 1

time <- getRefDates(rcm.hist.interp)
nt <- length(time)
ls <-lapply(1:nt, function(i){
  timei <-subsetDimension(rcm.hist.interp, dimension = "time", indices=i) # split day by day
  gridArithmetics(timei, mask.obs, operator = "*") # multiply the two grids
}
)
rcm.hist.interp <- bindGrid(ls, dimension = "time")
rm(rcm.hist.masked, mask.obs, ls)

saveRDS(rcm.hist.interp, name.path.raw, compress="xz")

##########################
##    Bias correction   ##
##########################
obs.data <- readRDS("/oceano/gmeteo/users/fuentesm/AEMET/TFM/tasmin.obs.DD.1976-2005.rds")
rcm.hist.interp <- readRDS("/oceano/gmeteo/users/fuentesm/AEMET/TFM/tasmin.rcm.raw.DD.1976-2005.rds")

months <- function(month, method) {
  if (method == 1) {
    if (month > 5) {
      return(sort(c((month-5):(month-1), month)))
    } else if (month == 1) {
      return(sort(c((12-(5-month)):12, month)))
    } else {
      return(sort(c((12-(5-month)):12, 1:(month-1), month)))
    }
  } else if (method == 2) {
    monthes <- c((month-3)%%12, (month-2)%%12, (month-1)%%12, month%%12, (month+1)%%12, (month+2)%%12, (month+3)%%12)
    monthes[monthes == 0] <- 12
    return(sort(monthes))
  } else {
    stop("Method not found.")
  }
}

subset_months <- function(obs, rcm, month, method, precipitation) {
  months_relacion <- months(month, method)
  # month.rcm <- subsetGrid(rcm, season = month)
  months.rcm <- subsetGrid(rcm, season = months_relacion)
  months.obs <- subsetGrid(obs, season = months_relacion)
  bc1 <- biasCorrection(y=months.obs, x=months.rcm, newdata=months.rcm, precipitation=precipitation,
                        method="eqm", extrapolation="constant", n.quantiles=99)
  bc.month <- subsetGrid(bc1, season = month)
  bc.month$Dates$start <- as.character(bc.month$Dates$start)
  bc.month$Dates$end <- as.character(bc.month$Dates$end)
  return(bc.month)
}

var.study <- list()
for (i in 1:12) {
  var.study[[months.names[i]]] <- subset_months(obs.data, rcm.hist.interp, i, method, precipitation)
}

bc <- bindGrid(var.study$january, var.study$february, var.study$march, var.study$april, var.study$may, var.study$june, var.study$july, var.study$august, var.study$september, var.study$october, var.study$november, var.study$december, dimension = "time")

saveRDS(bc, name.path.bc, compress="xz")