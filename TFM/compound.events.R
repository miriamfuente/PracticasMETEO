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
source("/oceano/gmeteo/users/fuentesm/AEMET/fun.aux.trends.R")

quantile.pr <- 0.1
quantile.tmax <- 30
years <- 1976:2005

# Load the data OBS
tmax.obs <- readRDS("/lustre/gmeteo/WORK/fuentesm/TFM/section4.2/ventana.movil/tmax/tasmax.obs.DD.1976-2005.rds")
pr.obs <- readRDS("/lustre/gmeteo/WORK/fuentesm/TFM/section4.2/ventana.movil/pr/pr.obs.DD.1976-2005.rds")

ce.obs <- calculoCE(pr.obs, tmax.obs, quantile.pr, quantile.tmax, years)

saveRDS(ce.obs, "/lustre/gmeteo/WORK/fuentesm/TFM/section4.2/ventana.movil/compound/ce.obs.DD.1976-2005.rds")

# Load the data RCM RAW
tmax.rcm <- readRDS("/lustre/gmeteo/WORK/fuentesm/TFM/section4.2/ventana.movil/tmax/tasmax.rcm.raw.DD.1976-2005.rds")
pr.rcm <- readRDS("/lustre/gmeteo/WORK/fuentesm/TFM/section4.2/ventana.movil/pr/pr.rcm.raw.DD.1976-2005.rds")

ce.rcm <- calculoCE(pr.rcm, tmax.rcm, quantile.pr, quantile.tmax, years)

saveRDS(ce.rcm, "/lustre/gmeteo/WORK/fuentesm/TFM/section4.2/ventana.movil/compound/ce.rcm.raw.DD.1976-2005.rds")

# Load the data RCM EQM
tmax.rcm.eqm <- readRDS("/lustre/gmeteo/WORK/fuentesm/TFM/section4.2/ventana.movil/tmax/tmax.rcm.eqm.DD.1976-2005.rds")
pr.rcm.eqm <- readRDS("/lustre/gmeteo/WORK/fuentesm/TFM/section4.2/ventana.movil/pr/pr.rcm.eqm.DD.1976-2005.rds")

ce.rcm.eqm <- calculoCE(pr.rcm.eqm, tmax.rcm.eqm, quantile.pr, quantile.tmax, years)

saveRDS(ce.rcm.eqm, "/lustre/gmeteo/WORK/fuentesm/TFM/section4.2/ventana.movil/compound/ce.rcm.eqm.DD.1976-2005.rds")

# Load the data RCP85 RAW
tmax.rcp.raw <- readRDS("/lustre/gmeteo/WORK/fuentesm/TFM/section4.2/ventana.movil/tmax/tmax.rcp85.raw.DD.2041-2070.rds")
pr.rcp.raw <- readRDS("/lustre/gmeteo/WORK/fuentesm/TFM/section4.2/ventana.movil/pr/pr.rcp85.raw.DD.2041-2070.rds")
years <- 2041:2070

ce.rcp.raw <- calculoCE(pr.rcp.raw, tmax.rcp.raw, quantile.pr, quantile.tmax, years)

saveRDS(ce.rcp.raw, "/lustre/gmeteo/WORK/fuentesm/TFM/section4.2/ventana.movil/compound/ce.rcp85.raw.DD.2041-2070.rds")

# Load the data RCP85 EQM
tmax.rcp.eqm <- readRDS("/lustre/gmeteo/WORK/fuentesm/TFM/section4.2/ventana.movil/tmax/tmax.rcp85.eqm.DD.2041-2070.rds")
pr.rcp.eqm <- readRDS("/lustre/gmeteo/WORK/fuentesm/TFM/section4.2/ventana.movil/pr/pr.rcp85.eqm.DD.2041-2070.rds")
years <- 2041:2070

ce.rcp.eqm <- calculoCE(pr.rcp.eqm, tmax.rcp.eqm, quantile.pr, quantile.tmax, years)

saveRDS(ce.rcp.eqm, "/lustre/gmeteo/WORK/fuentesm/TFM/section4.2/ventana.movil/compound/ce.rcp85.eqm.DD.2041-2070.rds")

# Load the data RCP85 RAW
tmax.rcp.raw <- readRDS("/lustre/gmeteo/WORK/fuentesm/TFM/section4.2/ventana.movil/tmax/tmax.rcp85.raw.DD.2071-2100.rds")
pr.rcp.raw <- readRDS("/lustre/gmeteo/WORK/fuentesm/TFM/section4.2/ventana.movil/pr/pr.rcp85.raw.DD.2071-2100.rds")
years <- 2071:2100

ce.rcp.raw <- calculoCE(pr.rcp.raw, tmax.rcp.raw, quantile.pr, quantile.tmax, years)

saveRDS(ce.rcp.raw, "/lustre/gmeteo/WORK/fuentesm/TFM/section4.2/ventana.movil/compound/ce.rcp85.raw.DD.2071-2100.rds")

# Load the data RCP85 EQM
tmax.rcp.eqm <- readRDS("/lustre/gmeteo/WORK/fuentesm/TFM/section4.2/ventana.movil/tmax/tmax.rcp85.eqm.DD.2071-2100.rds")
pr.rcp.eqm <- readRDS("/lustre/gmeteo/WORK/fuentesm/TFM/section4.2/ventana.movil/pr/pr.rcp85.eqm.DD.2071-2100.rds")
years <- 2071:2100

ce.rcp.eqm <- calculoCE(pr.rcp.eqm, tmax.rcp.eqm, quantile.pr, quantile.tmax, years)

saveRDS(ce.rcp.eqm, "/lustre/gmeteo/WORK/fuentesm/TFM/section4.2/ventana.movil/compound/ce.rcp85.eqm.DD.2071-2100.rds")

# Load compound events data
ce.obs <- readRDS("/lustre/gmeteo/WORK/fuentesm/TFM/section4.2/ventana.movil/compound/ce.obs.DD.1976-2005.rds")
ce.rcm <- readRDS("/lustre/gmeteo/WORK/fuentesm/TFM/section4.2/ventana.movil/compound/ce.rcm.raw.DD.1976-2005.rds")
ce.rcm.eqm <- readRDS("/lustre/gmeteo/WORK/fuentesm/TFM/section4.2/ventana.movil/compound/ce.rcm.eqm.DD.1976-2005.rds")
ce.rcp.raw <- readRDS("/lustre/gmeteo/WORK/fuentesm/TFM/section4.2/ventana.movil/compound/ce.rcp85.raw.DD.2041-2070.rds")
ce.rcp.eqm <- readRDS("/lustre/gmeteo/WORK/fuentesm/TFM/section4.2/ventana.movil/compound/ce.rcp85.eqm.DD.2041-2070.rds")
ce.rcp.raw2 <- readRDS("/lustre/gmeteo/WORK/fuentesm/TFM/section4.2/ventana.movil/compound/ce.rcp85.raw.DD.2071-2100.rds")
ce.rcp.eqm2 <- readRDS("/lustre/gmeteo/WORK/fuentesm/TFM/section4.2/ventana.movil/compound/ce.rcp85.eqm.DD.2071-2100.rds")

tmax <- readRDS("/oceano/gmeteo/users/fuentesm/AEMET/TFM/section4.1/tasmax.obs.DD.1971-2020.rds")

percentage <- function(CE, tmax,years) {
    earth.layer <- climatology(tmax)
    earth.layer$Data[!is.na(earth.layer$Data)] <- 1

    # years <- 1971:2020

    # Bind the data of a specific station for several years
    bindGrid_stations <- function(data, season) {
        # Extract the years available in the data
        years <- names(data)
        # Create a list to store the grids of each year
        list_grids <- list()
        # Iterate over the years
        for (year in years) {
            # Get the grid of the current year and station
            grid <- data[[year]][[season]]
            # Load the grid in the list
            list_grids[[year]] <- grid
        }
        # Perform the bindGrid with all the grids in the list
        result <- bindGrid(list_grids, dimension = "time", skip.temporal.check = TRUE)
        return(result)
    }

    winter <- bindGrid_stations(CE, season = "Winter")
    n.winter <- dim(winter$Data)[1]
    winter <- climatology(winter, clim.fun = list(FUN = "sum", na.rm = TRUE))
    winter <- gridArithmetics(winter, n.winter, operator = "/")
    winter <- gridArithmetics(winter, 100, operator = "*")
    winter <- gridArithmetics(winter, earth.layer, operator = "*")


    spring <- bindGrid_stations(CE, season = "Spring")
    n.spring <- dim(spring$Data)[1]
    spring <- climatology(spring, clim.fun = list(FUN = "sum", na.rm = TRUE))
    spring <- gridArithmetics(spring, n.spring, operator = "/")
    spring <- gridArithmetics(spring, 100, operator = "*")
    spring <- gridArithmetics(spring, earth.layer, operator = "*")

    summer <- bindGrid_stations(CE, season = "Summer")
    n.summer <- dim(summer$Data)[1]
    summer <- climatology(summer, clim.fun = list(FUN = "sum", na.rm = TRUE))
    summer <- gridArithmetics(summer, n.summer, operator = "/")
    summer <- gridArithmetics(summer, 100, operator = "*")
    summer <- gridArithmetics(summer, earth.layer, operator = "*")

    autumn <- bindGrid_stations(CE, season = "Autumn")
    n.autumn <- dim(autumn$Data)[1]
    autumn <- climatology(autumn, clim.fun = list(FUN = "sum", na.rm = TRUE))
    autumn <- gridArithmetics(autumn, n.autumn, operator = "/")
    autumn <- gridArithmetics(autumn, 100, operator = "*")
    autumn <- gridArithmetics(autumn, earth.layer, operator = "*")
    
    result <- list(winter, spring, summer, autumn)
    # doy nombre a los elementos de la lista
    names(result) <- c("Winter", "Spring", "Summer", "Autumn")
    return(result)
}

ce.per.obs <- percentage(ce.obs, tmax, 1976:2005)
ce.per.rcm.raw <- percentage(ce.rcm, tmax, 1976:2005)
ce.per.rcm.eqm <- percentage(ce.rcm.eqm, tmax, 1976:2005)
ce.per.rcp.raw <- percentage(ce.rcp.raw, tmax, 2041:2070)
ce.per.rcp.eqm <- percentage(ce.rcp.eqm, tmax, 2041:2070)
ce.per.rcp.raw2 <- percentage(ce.rcp.raw2, tmax, 2071:2100)
ce.per.rcp.eqm2 <- percentage(ce.rcp.eqm2, tmax, 2071:2100)

prueba <- spatialPlot(makeMultiGrid(ce.per.obs$Winter, ce.per.obs$Spring, ce.per.obs$Summer, ce.per.obs$Autumn,
                                    ce.per.rcm.raw$Winter, ce.per.rcm.raw$Spring, ce.per.rcm.raw$Summer, ce.per.rcm.raw$Autumn,
                                    ce.per.rcm.eqm$Winter, ce.per.rcm.eqm$Spring, ce.per.rcm.eqm$Summer, ce.per.rcm.eqm$Autumn,
                                    ce.per.rcp.raw$Winter, ce.per.rcp.raw$Spring, ce.per.rcp.raw$Summer, ce.per.rcp.raw$Autumn,
                                    ce.per.rcp.eqm$Winter, ce.per.rcp.eqm$Spring, ce.per.rcp.eqm$Summer, ce.per.rcp.eqm$Autumn,
                                    ce.per.rcp.raw2$Winter, ce.per.rcp.raw2$Spring, ce.per.rcp.raw2$Summer, ce.per.rcp.raw2$Autumn,
                                    ce.per.rcp.eqm2$Winter, ce.per.rcp.eqm2$Spring, ce.per.rcp.eqm2$Summer, ce.per.rcp.eqm2$Autumn,
                                    skip.temporal.check=TRUE),
                                    names.attr=c("Invierno OBS 1976-2005", "Primavera OBS 1976-2005", "Verano OBS 1976-2005", "Otoño OBS 1976-2005",
                                                 "Invierno RCM RAW 1976-2005", "Primavera RCM RAW 1976-2005", "Verano RCM RAW 1976-2005", "Otoño RCM RAW 1976-2005",
                                                 "Invierno RCM EQM 1976-2005", "Primavera RCM EQM 1976-2005", "Verano RCM EQM 1976-2005", "Otoño RCM EQM 1976-2005",
                                                 "Invierno RCP RAW 2041-2070", "Primavera RCP RAW 2041-2070", "Verano RCP RAW 2041-2070", "Otoño RCP RAW 2041-2070",
                                                 "Invierno RCP EQM 2041-2070", "Primavera RCP EQM 2041-2070", "Verano RCP EQM 2041-2070", "Otoño RCP EQM 2041-2070",
                                                 "Invierno RCP RAW 2071-2100", "Primavera RCP RAW 2071-2100", "Verano RCP RAW 2071-2100", "Otoño RCP RAW 2071-2100",
                                                 "Invierno RCP EQM 2071-2100", "Primavera RCP EQM 2071-2100", "Verano RCP EQM 2071-2100", "Otoño RCP EQM 2071-2100"),
                                    backdrop.theme = "coastline", as.table=TRUE, color.theme="Reds", rev.colors=FALSE, 
                                    main=paste0("Eventos extremos compuestos por estación"), layout=c(4,7),
                                    set.max=100, set.min=0, at=seq(0,100,5),
                                    colorkey = list(space = "right",
                                                    title = list("% de días", cex = 1)))

pdf("/oceano/gmeteo/users/fuentesm/AEMET/TFM/new.s4.1/CE.percentages.1976-2100.pdf", height=28, width=16)
prueba
dev.off()


eqm <- spatialPlot(makeMultiGrid(ce.per.obs$Winter, ce.per.obs$Spring, ce.per.obs$Summer, ce.per.obs$Autumn,
                                    ce.per.rcm.eqm$Winter, ce.per.rcm.eqm$Spring, ce.per.rcm.eqm$Summer, ce.per.rcm.eqm$Autumn,
                                    ce.per.rcp.eqm$Winter, ce.per.rcp.eqm$Spring, ce.per.rcp.eqm$Summer, ce.per.rcp.eqm$Autumn,
                                    ce.per.rcp.eqm2$Winter, ce.per.rcp.eqm2$Spring, ce.per.rcp.eqm2$Summer, ce.per.rcp.eqm2$Autumn,
                                    skip.temporal.check=TRUE),
                                    names.attr=c("Invierno OBS 1976-2005", "Primavera OBS 1976-2005", "Verano OBS 1976-2005", "Otoño OBS 1976-2005",
                                                 "Invierno RCM EQM 1976-2005", "Primavera RCM EQM 1976-2005", "Verano RCM EQM 1976-2005", "Otoño RCM EQM 1976-2005",
                                                 "Invierno RCP EQM 2041-2070", "Primavera RCP EQM 2041-2070", "Verano RCP EQM 2041-2070", "Otoño RCP EQM 2041-2070",
                                                 "Invierno RCP EQM 2071-2100", "Primavera RCP EQM 2071-2100", "Verano RCP EQM 2071-2100", "Otoño RCP EQM 2071-2100"),
                                    backdrop.theme = "coastline", as.table=TRUE, color.theme="Reds", rev.colors=FALSE, 
                                    main=paste0("Eventos extremos compuestos por estación"), layout=c(4,4),
                                    set.max=100, set.min=0, at=seq(0,100,5),
                                    colorkey = list(space = "right",
                                                    title = list("% de días", cex = 1)))

pdf("/oceano/gmeteo/users/fuentesm/AEMET/TFM/new.s4.1/CE.percentages.EQM.1976-2100.pdf", height=16, width=16)
eqm
dev.off()

# Bind the data of a specific station for several years
bindGrid_stations <- function(data, season) {
    # Extract the years available in the data
    years <- names(data)
    # Create a list to store the grids of each year
    list_grids <- list()
    # Iterate over the years
    for (year in years) {
        # Get the grid of the current year and station
        grid <- data[[year]][[season]]
        # Load the grid in the list
        list_grids[[year]] <- grid
    }
    # Perform the bindGrid with all the grids in the list
    result <- bindGrid(list_grids, dimension = "time", skip.temporal.check = TRUE)
    return(result)
}

ce.obs.seasons <- list()
ce.rcm.seasons <- list()
ce.rcm.eqm.seasons <- list()
ce.rcp.eqm.seasons <- list()   
ce.rcp.eqm.seasons2 <- list()

for (season in c("Winter", "Spring", "Summer", "Autumn")) {
    ce.obs.seasons[[season]] <- bindGrid_stations(ce.obs, season)
    ce.rcm.seasons[[season]] <- bindGrid_stations(ce.rcm, season)
    ce.rcm.eqm.seasons[[season]] <- bindGrid_stations(ce.rcm.eqm, season)
    ce.rcp.eqm.seasons[[season]] <- bindGrid_stations(ce.rcp.eqm, season)
    ce.rcp.eqm.seasons2[[season]] <- bindGrid_stations(ce.rcp.eqm2, season)
}

sesgo.raw.obs <- list()
sesgo.eqm.obs <- list()
cc.rcp.eqm <- list()
cc.rcp.eqm2 <- list()

for (season in c("Winter", "Spring", "Summer", "Autumn")) {
    sesgo.raw.obs[[season]] <- gridArithmetics(climatology(ce.rcm.seasons[[season]], clim.fun=list(FUN="sum", na.rm=FALSE)), climatology(ce.obs.seasons[[season]], clim.fun=list(FUN="sum", na.rm=FALSE)), operator = "-")
    sesgo.eqm.obs[[season]] <- gridArithmetics(climatology(ce.rcm.eqm.seasons[[season]], clim.fun=list(FUN="sum", na.rm=FALSE)), climatology(ce.obs.seasons[[season]], clim.fun=list(FUN="sum", na.rm=FALSE)), operator = "-")
    cc.rcp.eqm[[season]] <- gridArithmetics(climatology(ce.rcp.eqm.seasons[[season]], clim.fun=list(FUN="sum", na.rm=FALSE)), climatology(ce.rcm.eqm.seasons[[season]], clim.fun=list(FUN="sum", na.rm=FALSE)), operator = "-")
    cc.rcp.eqm2[[season]] <- gridArithmetics(climatology(ce.rcp.eqm.seasons2[[season]], clim.fun=list(FUN="sum", na.rm=FALSE)), climatology(ce.rcm.eqm.seasons[[season]], clim.fun=list(FUN="sum", na.rm=FALSE)), operator = "-")
}

visual.obs <- spatialPlot(makeMultiGrid(climatology(ce.obs.seasons$Winter, clim.fun=list(FUN="sum", na.rm=FALSE)), climatology(ce.obs.seasons$Spring, clim.fun=list(FUN="sum", na.rm=FALSE)), climatology(ce.obs.seasons$Summer, clim.fun=list(FUN="sum", na.rm=FALSE)), climatology(ce.obs.seasons$Autumn, clim.fun=list(FUN="sum", na.rm=FALSE)), skip.temporal.check=TRUE),
                            backdrop.theme = "coastline", as.table=TRUE, names.attr=c("Invierno", "Primavera", "Verano", "Otoño"),
                          color.theme="Reds", rev.colors=FALSE, main="Eventos extremos compuestos observados por estación (1976-2005)",
                          layout=c(4,1),
                          colorkey = list(space = "right",
                                          title = list("Nº de días", cex = 1)))

visual.rcm.eqm <- spatialPlot(makeMultiGrid(climatology(ce.rcm.eqm.seasons$Winter, clim.fun=list(FUN="sum", na.rm=FALSE)), climatology(ce.rcm.eqm.seasons$Spring, clim.fun=list(FUN="sum", na.rm=FALSE)), climatology(ce.rcm.eqm.seasons$Summer, clim.fun=list(FUN="sum", na.rm=FALSE)), climatology(ce.rcm.eqm.seasons$Autumn, clim.fun=list(FUN="sum", na.rm=FALSE)), skip.temporal.check=TRUE),
                            backdrop.theme = "coastline", as.table=TRUE, names.attr=c("Invierno", "Primavera", "Verano", "Otoño"),
                          color.theme="Reds", rev.colors=FALSE, main="Eventos extremos compuestos histórico EQM por estación (1976-2005)",
                          layout=c(4,1),
                          colorkey = list(space = "right",
                                          title = list("Nº de días", cex = 1)))

sesgo.raw.eqm.obs <- spatialPlot(makeMultiGrid(sesgo.raw.obs$Winter, sesgo.raw.obs$Spring, sesgo.raw.obs$Summer, sesgo.raw.obs$Autumn, 
                                               sesgo.eqm.obs$Winter, sesgo.eqm.obs$Spring, sesgo.eqm.obs$Summer, sesgo.eqm.obs$Autumn,
                                                  skip.temporal.check=TRUE),
                            backdrop.theme = "coastline", as.table=TRUE, names.attr=c("Invierno RAW-OBS", "Primavera RAW-OBS", "Verano RAW-OBS", "Otoño RAW-OBS",
                                                                                  "Invierno EQM-OBS", "Primavera EQM-OBS", "Verano EQM-OBS", "Otoño EQM-OBS"),
                            color.theme="RdBu", rev.colors=TRUE, main="Sesgo de los eventos extremos compuestos por estación (1976-2005)",
                            layout=c(4,2),
                            set.max=1500, set.min=-1500, at=seq(-1500,1500,100),
                            colorkey = list(space = "right",
                                            title = list("Nº de días", cex = 1)))

cc.rcp.eqm.vs <- spatialPlot(makeMultiGrid(cc.rcp.eqm$Winter, cc.rcp.eqm$Spring, cc.rcp.eqm$Summer, cc.rcp.eqm$Autumn,
                                        cc.rcp.eqm2$Winter, cc.rcp.eqm2$Spring, cc.rcp.eqm2$Summer, cc.rcp.eqm2$Autumn,
                                        skip.temporal.check=TRUE),
                            backdrop.theme = "coastline", as.table=TRUE, names.attr=c("Invierno 2041-2070", "Primavera 2041-2070", "Verano 2041-2070", "Otoño 2041-2070",
                                                                                  "Invierno 2071-2100", "Primavera 2071-2100", "Verano 2071-2100", "Otoño 2071-2100"),
                            color.theme="RdBu", rev.colors=TRUE, main="Señal de Cambio Climático de los eventos extremos compuestos por estación (2041-2100)",
                            layout=c(4,2),
                            set.max=1500, set.min=-1500, at=seq(-1500,1500,100),
                            colorkey = list(space = "right",
                                            title = list("Nº de días", cex = 1)))

library(gridExtra)
library(grid)


visual.obs.raw.eqm <- spatialPlot(makeMultiGrid(climatology(ce.obs.seasons$Winter, clim.fun=list(FUN="sum", na.rm=FALSE)), climatology(ce.obs.seasons$Spring, clim.fun=list(FUN="sum", na.rm=FALSE)), climatology(ce.obs.seasons$Summer, clim.fun=list(FUN="sum", na.rm=FALSE)), climatology(ce.obs.seasons$Autumn, clim.fun=list(FUN="sum", na.rm=FALSE)), 
                                               climatology(ce.rcm.seasons$Winter, clim.fun=list(FUN="sum", na.rm=FALSE)), climatology(ce.rcm.seasons$Spring, clim.fun=list(FUN="sum", na.rm=FALSE)), climatology(ce.rcm.seasons$Summer, clim.fun=list(FUN="sum", na.rm=FALSE)), climatology(ce.rcm.seasons$Autumn, clim.fun=list(FUN="sum", na.rm=FALSE)),
                                               climatology(ce.rcm.eqm.seasons$Winter, clim.fun=list(FUN="sum", na.rm=FALSE)), climatology(ce.rcm.eqm.seasons$Spring, clim.fun=list(FUN="sum", na.rm=FALSE)), climatology(ce.rcm.eqm.seasons$Summer, clim.fun=list(FUN="sum", na.rm=FALSE)), climatology(ce.rcm.eqm.seasons$Autumn, clim.fun=list(FUN="sum", na.rm=FALSE)),
                                               skip.temporal.check=TRUE),
                            names.attr=c("Invierno OBS", "Primavera OBS", "Verano OBS", "Otoño OBS",
                                         "Invierno RCM RAW", "Primavera RCM RAW", "Verano RCM RAW", "Otoño RCM RAW",
                                         "Invierno RCM EQM", "Primavera RCM EQM", "Verano RCM EQM", "Otoño RCM EQM"),
                            backdrop.theme = "coastline", as.table=TRUE, color.theme="Reds", rev.colors=FALSE,
                            main="Eventos extremos compuestos por estación (1976-2005)", layout=c(4,3),
                            colorkey = list(space = "right",
                                            title = list("Nº de Eventos", cex = 1)))


library(gridExtra)

pdf("ce.sesgo.pdf", width=16, height=10)

grid.arrange(
  arrangeGrob(visual.obs, ncol=1),
  arrangeGrob(sesgo.raw.eqm.obs, ncol=1),
  ncol=1,
  heights = c(1, 2) # Ajusta las proporciones, donde visual.obs ocupa menos espacio
)
dev.off()

pdf("ce.cc.pdf", width=16, height=10)
grid.arrange(
  arrangeGrob(visual.rcm.eqm, ncol=1),
  arrangeGrob(cc.rcp.eqm.vs, ncol=1),
  ncol=1,
  heights = c(1, 2) # Ajusta las proporciones, donde visual.obs ocupa menos espacio
)
dev.off()
