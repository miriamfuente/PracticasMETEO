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

pr.obs <- readRDS("/lustre/gmeteo/WORK/fuentesm/TFM/section4.2/pr.obs.DD.1976-2005.subset.rds")
pr.rcm <- readRDS("/lustre/gmeteo/WORK/fuentesm/TFM/section4.2/pr.rcm.raw.DD.1976-2005.subset.rds")


years <- 1976:2005
set.seed(12) 
mix <- sample(years)
size.fold <- length(mix) / 2
fold1 <- mix[1:size.fold]
fold2 <- mix[(size.fold + 1):length(mix)]
cat("Fold 1:", fold1, "\n")
cat("Fold 2:", fold2, "\n")


meses <- function(mes, metodo) {
  if (metodo == 1) {
    if (mes > 5) {
      return(sort(c((mes-5):(mes-1), mes)))
    } else if (mes == 1) {
      return(sort(c((12-(5-mes)):12, mes)))
    } else {
      return(sort(c((12-(5-mes)):12, 1:(mes-1), mes)))
    }
  } else if (metodo == 2) {
    meses <- c((mes-3)%%12, (mes-2)%%12, (mes-1)%%12, mes%%12, (mes+1)%%12, (mes+2)%%12, (mes+3)%%12)
    meses[meses == 0] <- 12
    return(sort(meses))
  } else {
    stop("Método no reconocido")
  }
}

mes <- 5
metodo <-2
meses_relacion <- meses(mes, metodo) # 2 3 4 5 6 7 8
years <- list(fold1, fold2)
target.year <- years[[1]]
rest.years <- setdiff(unlist(years), target.year)

#RCM
rcm <- pr.rcm # tus datos subset del rcm
meses.rcm <- subsetGrid(rcm, season = meses_relacion) # empieza en febrero de 1976
meses.rcm2 <- fillGrid(meses.rcm, lonLim = NULL, latLim = NULL) # empieza en febrero de 1976 y rellena NA intermedios

meses.rcm3<- subsetGrid(meses.rcm2, years = rest.years, drop = FALSE) # no introduce NA, getSeason y sort(getSeason) devuelven lo mismo

#OBS
obs <- pr.obs # tus datos subset del obs
meses.obs <- subsetGrid(obs, season = meses_relacion) # empieza en febrero de 1976
meses.obs2 <- fillGrid(meses.obs, lonLim = NULL, latLim = NULL) # empieza en febrero de 1976 y rellena NA intermedios

meses.obs3 <- subsetGrid(meses.obs2, years = rest.years, drop = FALSE) # no introduce NA, getSeason y sort(getSeason) devuelven lo mismo

bc <- biasCorrection(y=meses.obs, x=meses.rcm, newdata=meses.rcm, precipitation=TRUE,
                     method="eqm", extrapolation="constant", n.quantiles=99, cross.val="kfold", folds=list(fold1, fold2))
