options(java.parameters = "-Xmx64g") 
library(loadeR)
library(transformeR)
library(visualizeR)
library(climate4R.UDG)
library(dplyr)
library(climate4R.value)
library(VALUE)
library(drought4R)
library(downscaleR) # para bias correction (función biasCorrection)
library(climate4R.indices) # para calcular índices

source("/oceano/gmeteo/users/fuentesm/AEMET/fun.aux.trends.R")

pr <- readRDS("/lustre/gmeteo/WORK/fuentesm/TFM/section4.1/pr.obs.DD.1971-2020.rds")
tmax <- readRDS("/lustre/gmeteo/WORK/fuentesm/TFM/section4.1/tasmax.obs.DD.1971-2020.rds")
tmin <- readRDS("/lustre/gmeteo/WORK/fuentesm/TFM/section4.1/tasmin.obs.DD.1971-2020.rds")

# Redim
pr <- redim(pr, drop = TRUE)
tmax <- redim(tmax, drop = TRUE)
tmin <- redim(tmin, drop = TRUE)

# Calculate the PET  (daily)
pet <-  petGrid(tasmin = tmin, tasmax = tmax, pr = pr, method = "hargreaves-samani")
pet <- redim(pet, drop = TRUE)

# Monthly agregation
pr.MM <- aggregateGrid(grid = pr, aggr.m = list(FUN = "sum", na.rm = FALSE))
pet.MM <- aggregateGrid(grid = pet, aggr.m = list(FUN = "mean", na.rm = TRUE))

# Calculate the SPEI
spei <- speiGrid(et0.grid = pet.MM, pr.grid = pr.MM, scale = 6, ref.start = c(1971,1), ref.end=c(2020,12), na.rm = TRUE)

spei.month <- list()
months <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")

for (i in 1:length(months)) {
    spei.month[[months[i]]] <- subsetGrid(spei
    , season=i)
    spei.month[[months[i]]] <- climatology(spei.month[[months[i]]])

}

pdf("pet.obs.pdf", width=16, height=12)
spatialPlot(makeMultiGrid(spei.month$Enero, spei.month$Febrero, spei.month$Marzo, spei.month$Abril,
                          spei.month$Mayo, spei.month$Junio, spei.month$Julio, spei.month$Agosto,
                          spei.month$Septiembre, spei.month$Octubre, spei.month$Noviembre, spei.month$Diciembre, skip.temporal.check=TRUE),
                          backdrop.theme="coastline", as.table=TRUE, color.theme="Reds", rev.colors=FALSE,
                          names.attr=c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre"),
                          main="PET observaciones (1971-2020)", layout=c(4,3), set.max=8, set.min=0, at=seq(0, 8, 1),
                          colorkey = list(space = "right",
                          title = list("mm/día", cex = 1)))
dev.off()

spei.winter <- subsetGrid(spei, season=c(1, 2, 12))
spei.spring <- subsetGrid(spei, season=c(3, 4, 5))
spei.summer <- subsetGrid(spei, season=c(6, 7, 8))
spei.autumn <- subsetGrid(spei, season=c(9, 10, 11))

pdf("spei.obs.seasons.pdf", width=16, height=3)
spatialPlot(makeMultiGrid(spei.winter, spei.spring, spei.summer, spei.autumn, skip.temporal.check=TRUE),
            backdrop.theme="coastline", as.table=TRUE, color.theme="RdBu", rev.colors=FALSE,
            names.attr=c("Invierno", "Primavera", "Verano", "Otoño"),
            main="SPEI 6M observaciones (1971-2020)", layout=c(4,1), set.max=0.04, set.min=-0.04, at=seq(-0.04, 0.04, 0.01),
            colorkey = list(space = "right",
            title = list(" ", cex = 1)))
dev.off()



trends <- list()
sig <- list()
visual <- list()
for (i in 1:length(months)) {
    spei.month[[months[i]]] <- subsetGrid(spei, season=i)
    trends[[months[i]]] <- climatology(spei.month[[months[i]]], clim.fun = list(FUN = "computeTrend"))
    # trends[[months[i]]] <- gridArithmetics(trends[[months[i]]], 10)
    sig[[months[i]]] = map.stippling(clim = climatology(spei.month[[months[i]]], clim.fun = list(FUN = "computeSigTrend")), 
                    threshold = 0.05, condition = "GT", 
                    pch = 19, cex = .25, col = "white")  # points exhibiting significant trends (at a 95% confidence level)

    visual[[months[i]]] <- spatialPlot(trends[[months[i]]], backdrop.theme = "coastline", color.theme = "RdBu", at = seq(-0.04, 0.04, by = 0.01), 
                set.max = 0.04, set.min = -0.04, rev.colors = FALSE, main = months[i], sp.layout = list(sig[[months[i]]]),
                colorkey=list(space = "bottom", title = list("mm/día / año ", cex = 1)))
}

library(gridExtra)
library(grid)

pdf("spei.obs.trends.pdf", width=16, height=12)
grid.arrange(
             arrangeGrob(visual$Enero, visual$Febrero, visual$Marzo, visual$Abril, ncol=4),
             arrangeGrob(visual$Mayo, visual$Junio, visual$Julio, visual$Agosto, ncol=4),
             arrangeGrob(visual$Septiembre, visual$Octubre, visual$Noviembre, visual$Diciembre, ncol=4),
             ncol=1)
dev.off()


trend.winter <- climatology(spei.winter, clim.fun = list(FUN = "computeTrend"))
trend.spring <- climatology(spei.spring, clim.fun = list(FUN = "computeTrend"))
trend.summer <- climatology(spei.summer, clim.fun = list(FUN = "computeTrend"))
trend.autumn <- climatology(spei.autumn, clim.fun = list(FUN = "computeTrend"))

sig.winter <- map.stippling(clim = climatology(spei.winter, clim.fun = list(FUN = "computeSigTrend")), 
                    threshold = 0.05, condition = "GT", 
                    pch = 19, cex = .05, col = "black")  # points exhibiting significant trends (at a 95% confidence level)
sig.spring <- map.stippling(clim = climatology(spei.spring, clim.fun = list(FUN = "computeSigTrend")),
                    threshold = 0.05, condition = "GT", 
                    pch = 19, cex = .05, col = "black")  # points exhibiting significant trends (at a 95% confidence level)
sig.summer <- map.stippling(clim = climatology(spei.summer, clim.fun = list(FUN = "computeSigTrend")),
                    threshold = 0.05, condition = "GT", 
                    pch = 19, cex = .05, col = "black")  # points exhibiting significant trends (at a 95% confidence level)
sig.autumn <- map.stippling(clim = climatology(spei.autumn, clim.fun = list(FUN = "computeSigTrend")),
                    threshold = 0.05, condition = "GT", 
                    pch = 19, cex = .05, col = "black")  # points exhibiting significant trends (at a 95% confidence level)

visual.winter <- spatialPlot(trend.winter, backdrop.theme = "coastline", color.theme = "RdBu", at = seq(-0.06, 0.06, by = 0.01), 
                set.max = 0.60, set.min = -0.06, rev.colors = FALSE, main = "Invierno", sp.layout = list(sig.winter),
                colorkey=list(space = "bottom", title = list(" ", cex = 1)))
visual.spring <- spatialPlot(trend.spring, backdrop.theme = "coastline", color.theme = "RdBu", at = seq(-0.06, 0.06, by = 0.01),
                set.max = 0.60, set.min = -0.06, rev.colors = FALSE, main = "Primavera", sp.layout = list(sig.spring),
                colorkey=list(space = "bottom", title = list(" ", cex = 1)))
visual.summer <- spatialPlot(trend.summer, backdrop.theme = "coastline", color.theme = "RdBu", at = seq(-0.06, 0.06, by = 0.01),
                set.max = 0.60, set.min = -0.06, rev.colors = FALSE, main = "Verano", sp.layout = list(sig.summer),
                colorkey=list(space = "bottom", title = list(" ", cex = 1)))
visual.autumn <- spatialPlot(trend.autumn, backdrop.theme = "coastline", color.theme = "RdBu", at = seq(-0.06, 0.06, by = 0.01),
                set.max = 0.60, set.min = -0.06, rev.colors = FALSE, main = "Otoño", sp.layout = list(sig.autumn),
                colorkey=list(space = "bottom", title = list(" ", cex = 1)))

pdf("spei.obs.seasons.trends.pdf", width=16, height=3)
grid.arrange(visual.winter, visual.spring, visual.summer, visual.autumn, ncol=4)
dev.off()

####################################################################################################################
pr.obs <- readRDS("/lustre/gmeteo/WORK/fuentesm/TFM/section4.2/ventana.movil/pr/pr.obs.DD.1976-2005.rds")
pr.rcm.raw <- readRDS("/lustre/gmeteo/WORK/fuentesm/TFM/section4.2/ventana.movil/pr/pr.rcm.raw.DD.1976-2005.rds")
pr.rcm.eqm <- readRDS("/lustre/gmeteo/WORK/fuentesm/TFM/section4.2/ventana.movil/pr/pr.rcm.eqm.DD.1976-2005.rds")
pr.rcp.raw <- readRDS("/lustre/gmeteo/WORK/fuentesm/TFM/section4.2/ventana.movil/pr/pr.rcp85.raw.DD.2041-2070.rds")
pr.rcp.eqm <- readRDS("/lustre/gmeteo/WORK/fuentesm/TFM/section4.2/ventana.movil/pr/pr.rcp85.eqm.DD.2041-2070.rds")
pr.rcp.raw2 <- readRDS("/lustre/gmeteo/WORK/fuentesm/TFM/section4.2/ventana.movil/pr/pr.rcp85.raw.DD.2071-2100.rds")
pr.rcp.eqm2 <- readRDS("/lustre/gmeteo/WORK/fuentesm/TFM/section4.2/ventana.movil/pr/pr.rcp85.eqm.DD.2071-2100.rds")

tmax.obs <- readRDS("/lustre/gmeteo/WORK/fuentesm/TFM/section4.2/ventana.movil/tmax/tasmax.obs.DD.1976-2005.rds")
tmax.rcm.raw <- readRDS("/lustre/gmeteo/WORK/fuentesm/TFM/section4.2/ventana.movil/tmax/tasmax.rcm.raw.DD.1976-2005.rds")
tmax.rcm.eqm <- readRDS("/lustre/gmeteo/WORK/fuentesm/TFM/section4.2/ventana.movil/tmax/tmax.rcm.eqm.DD.1976-2005.rds")
tmax.rcp.raw <- readRDS("/lustre/gmeteo/WORK/fuentesm/TFM/section4.2/ventana.movil/tmax/tmax.rcp85.raw.DD.2041-2070.rds")
tmax.rcp.eqm <- readRDS("/lustre/gmeteo/WORK/fuentesm/TFM/section4.2/ventana.movil/tmax/tmax.rcp85.eqm.DD.2041-2070.rds")
tmax.rcp.raw2 <- readRDS("/lustre/gmeteo/WORK/fuentesm/TFM/section4.2/ventana.movil/tmax/tmax.rcp85.raw.DD.2071-2100.rds")
tmax.rcp.eqm2 <- readRDS("/lustre/gmeteo/WORK/fuentesm/TFM/section4.2/ventana.movil/tmax/tmax.rcp85.eqm.DD.2071-2100.rds")

tmin.obs <- readRDS("/lustre/gmeteo/WORK/fuentesm/TFM/section4.2/ventana.movil/tmin/tasmin.obs.DD.1976-2005.rds")
tmin.rcm.raw <- readRDS("/lustre/gmeteo/WORK/fuentesm/TFM/section4.2/ventana.movil/tmin/tasmin.rcm.raw.DD.1976-2005.rds")
tmin.rcm.eqm <- readRDS("/lustre/gmeteo/WORK/fuentesm/TFM/section4.2/ventana.movil/tmin/tmin.rcm.eqm.DD.1976-2005.rds")
tmin.rcp.raw <- readRDS("/lustre/gmeteo/WORK/fuentesm/TFM/section4.2/ventana.movil/tmin/tmin.rcp85.raw.DD.2041-2070.rds")
tmin.rcp.eqm <- readRDS("/lustre/gmeteo/WORK/fuentesm/TFM/section4.2/ventana.movil/tmin/tmin.rcp85.eqm.DD.2041-2070.rds")
tmin.rcp.raw2 <- readRDS("/lustre/gmeteo/WORK/fuentesm/TFM/section4.2/ventana.movil/tmin/tmin.rcp85.raw.DD.2071-2100.rds")
tmin.rcp.eqm2 <- readRDS("/lustre/gmeteo/WORK/fuentesm/TFM/section4.2/ventana.movil/tmin/tmin.rcp85.eqm.DD.2071-2100.rds")

pr.obs <- redim(pr.obs, drop = TRUE)
pr.rcm.raw <- redim(pr.rcm.raw, drop = TRUE)
pr.rcm.eqm <- redim(pr.rcm.eqm, drop = TRUE)
pr.rcp.raw <- redim(pr.rcp.raw, drop = TRUE)
pr.rcp.eqm <- redim(pr.rcp.eqm, drop = TRUE)
pr.rcp.raw2 <- redim(pr.rcp.raw2, drop = TRUE)
pr.rcp.eqm2 <- redim(pr.rcp.eqm2, drop = TRUE)

tmax.obs <- redim(tmax.obs, drop = TRUE)
tmax.rcm.raw <- redim(tmax.rcm.raw, drop = TRUE)
tmax.rcm.eqm <- redim(tmax.rcm.eqm, drop = TRUE)
tmax.rcp.raw <- redim(tmax.rcp.raw, drop = TRUE)
tmax.rcp.eqm <- redim(tmax.rcp.eqm, drop = TRUE)
tmax.rcp.raw2 <- redim(tmax.rcp.raw2, drop = TRUE)
tmax.rcp.eqm2 <- redim(tmax.rcp.eqm2, drop = TRUE)

tmin.obs <- redim(tmin.obs, drop = TRUE)
tmin.rcm.raw <- redim(tmin.rcm.raw, drop = TRUE)
tmin.rcm.eqm <- redim(tmin.rcm.eqm, drop = TRUE)
tmin.rcp.raw <- redim(tmin.rcp.raw, drop = TRUE)
tmin.rcp.eqm <- redim(tmin.rcp.eqm, drop = TRUE)
tmin.rcp.raw2 <- redim(tmin.rcp.raw2, drop = TRUE)
tmin.rcp.eqm2 <- redim(tmin.rcp.eqm2, drop = TRUE)

pet.obs <- petGrid(tasmin = tmin.obs, tasmax = tmax.obs, pr = pr.obs, method = "hargreaves-samani")
pet.obs <- redim(pet.obs, drop = TRUE)
pr.obs.MM <- aggregateGrid(grid = pr.obs, aggr.m = list(FUN = "sum", na.rm = FALSE))
pet.obs.MM <- aggregateGrid(grid = pet.obs, aggr.m = list(FUN = "mean", na.rm = FALSE))
spei.obs <- speiGrid(et0.grid = pet.obs.MM, pr.grid = pr.obs.MM, scale = 6, ref.start = c(1976, 1), ref.end = c(2005, 12), na.rm = TRUE)

pet.rcm.raw <- petGrid(tasmin = tmin.rcm.raw, tasmax = tmax.rcm.raw, pr = pr.rcm.raw, method = "hargreaves-samani")
pet.rcm.raw <- redim(pet.rcm.raw, drop = TRUE)
pr.rcm.raw.MM <- aggregateGrid(grid = pr.rcm.raw, aggr.m = list(FUN = "sum", na.rm = FALSE))
pet.rcm.raw.MM <- aggregateGrid(grid = pet.rcm.raw, aggr.m = list(FUN = "mean", na.rm = FALSE))
spei.rcm.raw <- speiGrid(et0.grid = pet.rcm.raw.MM, pr.grid = pr.rcm.raw.MM, scale = 6, ref.start = c(1976, 1), ref.end = c(2005, 12), na.rm = TRUE)

pet.rcm.eqm <- petGrid(tasmin = tmin.rcm.eqm, tasmax = tmax.rcm.eqm, pr = pr.rcm.eqm, method = "hargreaves-samani")
pet.rcm.eqm <- redim(pet.rcm.eqm, drop = TRUE)
pr.rcm.eqm.MM <- aggregateGrid(grid = pr.rcm.eqm, aggr.m = list(FUN = "sum", na.rm = FALSE))
pet.rcm.eqm.MM <- aggregateGrid(grid = pet.rcm.eqm, aggr.m = list(FUN = "mean", na.rm = FALSE))
spei.rcm.eqm <- speiGrid(et0.grid = pet.rcm.eqm.MM, pr.grid = pr.rcm.eqm.MM, scale = 6, ref.start = c(1976, 1), ref.end = c(2005, 12), na.rm = TRUE)

pet.rcp.raw <- petGrid(tasmin = tmin.rcp.raw, tasmax = tmax.rcp.raw, pr = pr.rcp.raw, method = "hargreaves-samani")
pet.rcp.raw <- redim(pet.rcp.raw, drop = TRUE)
pr.rcp.raw.MM <- aggregateGrid(grid = pr.rcp.raw, aggr.m = list(FUN = "sum", na.rm = FALSE))
pet.rcp.raw.MM <- aggregateGrid(grid = pet.rcp.raw, aggr.m = list(FUN = "mean", na.rm = FALSE))
spei.rcp.raw <- speiGrid(et0.grid = pet.rcp.raw.MM, pr.grid = pr.rcp.raw.MM, scale = 6, ref.start = c(2041, 1), ref.end = c(2070, 12), na.rm = TRUE)

pet.rcp.eqm <- petGrid(tasmin = tmin.rcp.eqm, tasmax = tmax.rcp.eqm, pr = pr.rcp.eqm, method = "hargreaves-samani")
pet.rcp.eqm <- redim(pet.rcp.eqm, drop = TRUE)
pr.rcp.eqm.MM <- aggregateGrid(grid = pr.rcp.eqm, aggr.m = list(FUN = "sum", na.rm = FALSE))
pet.rcp.eqm.MM <- aggregateGrid(grid = pet.rcp.eqm, aggr.m = list(FUN = "mean", na.rm = FALSE))
spei.rcp.eqm <- speiGrid(et0.grid = pet.rcp.eqm.MM, pr.grid = pr.rcp.eqm.MM, scale = 6, ref.start = c(2041, 1), ref.end = c(2070, 12), na.rm = TRUE)

pet.rcp.raw2 <- petGrid(tasmin = tmin.rcp.raw2, tasmax = tmax.rcp.raw2, pr = pr.rcp.raw2, method = "hargreaves-samani")
pet.rcp.raw2 <- redim(pet.rcp.raw2, drop = TRUE)
pr.rcp.raw2.MM <- aggregateGrid(grid = pr.rcp.raw2, aggr.m = list(FUN = "sum", na.rm = FALSE))
pet.rcp.raw2.MM <- aggregateGrid(grid = pet.rcp.raw2, aggr.m = list(FUN = "mean", na.rm = FALSE))
spei.rcp.raw2 <- speiGrid(et0.grid = pet.rcp.raw2.MM, pr.grid = pr.rcp.raw2.MM, scale = 6, ref.start = c(2071, 1), ref.end = c(2100, 12), na.rm = TRUE)

pet.rcp.eqm2 <- petGrid(tasmin = tmin.rcp.eqm2, tasmax = tmax.rcp.eqm2, pr = pr.rcp.eqm2, method = "hargreaves-samani")
pet.rcp.eqm2 <- redim(pet.rcp.eqm2, drop = TRUE)
pr.rcp.eqm2.MM <- aggregateGrid(grid = pr.rcp.eqm2, aggr.m = list(FUN = "sum", na.rm = FALSE))
pet.rcp.eqm2.MM <- aggregateGrid(grid = pet.rcp.eqm2, aggr.m = list(FUN = "mean", na.rm = FALSE))
spei.rcp.eqm2 <- speiGrid(et0.grid = pet.rcp.eqm2.MM, pr.grid = pr.rcp.eqm2.MM, scale = 6, ref.start = c(2071, 1), ref.end = c(2100, 12), na.rm = TRUE)


# Definir estaciones del año
seasons <- list(
  Winter = c(1, 2, 12),
  Spring = c(3, 4, 5),
  Summer = c(6, 7, 8),
  Autumn = c(9, 10, 11)
)

# Función para dividir los datos por estaciones y crear una lista nombrada
split_by_season <- function(data, seasons) {
  season_data <- lapply(seasons, function(season) {
    subsetGrid(data, season = season)
  })
  names(season_data) <- names(seasons)
  return(season_data)
}

# Dividir los datos de SPEI por estaciones
spei.obs.seasons <- split_by_season(spei.obs, seasons)
spei.rcm.raw.seasons <- split_by_season(spei.rcm.raw, seasons)
spei.rcm.eqm.seasons <- split_by_season(spei.rcm.eqm, seasons)
spei.rcp.raw.seasons <- split_by_season(spei.rcp.raw, seasons)
spei.rcp.eqm.seasons <- split_by_season(spei.rcp.eqm, seasons)
spei.rcp.raw2.seasons <- split_by_season(spei.rcp.raw2, seasons)
spei.rcp.eqm2.seasons <- split_by_season(spei.rcp.eqm2, seasons)

spei <- spatialPlot(makeMultiGrid(climatology(spei.obs.seasons$Winter), climatology(spei.obs.seasons$Spring), climatology(spei.obs.seasons$Summer), climatology(spei.obs.seasons$Autumn),
                                  climatology(spei.rcm.raw.seasons$Winter), climatology(spei.rcm.raw.seasons$Spring), climatology(spei.rcm.raw.seasons$Summer), climatology(spei.rcm.raw.seasons$Autumn),
                                    climatology(spei.rcm.eqm.seasons$Winter), climatology(spei.rcm.eqm.seasons$Spring), climatology(spei.rcm.eqm.seasons$Summer), climatology(spei.rcm.eqm.seasons$Autumn),
                                    climatology(spei.rcp.raw.seasons$Winter), climatology(spei.rcp.raw.seasons$Spring), climatology(spei.rcp.raw.seasons$Summer), climatology(spei.rcp.raw.seasons$Autumn),
                                    climatology(spei.rcp.eqm.seasons$Winter), climatology(spei.rcp.eqm.seasons$Spring), climatology(spei.rcp.eqm.seasons$Summer), climatology(spei.rcp.eqm.seasons$Autumn),
                                    climatology(spei.rcp.raw2.seasons$Winter), climatology(spei.rcp.raw2.seasons$Spring), climatology(spei.rcp.raw2.seasons$Summer), climatology(spei.rcp.raw2.seasons$Autumn),
                                    climatology(spei.rcp.eqm2.seasons$Winter), climatology(spei.rcp.eqm2.seasons$Spring), climatology(spei.rcp.eqm2.seasons$Summer), climatology(spei.rcp.eqm2.seasons$Autumn),
                                    skip.temporal.check=TRUE),
                    backdrop.theme="coastline", as.table=TRUE, color.theme="RdBu", rev.colors=FALSE,
                    names.attr=c("Invierno OBS 1976-2005", "Primavera OBS 1976-2005", "Verano OBS 1976-2005", "Otoño OBS 1976-2005",
                                                 "Invierno RCM RAW 1976-2005", "Primavera RCM RAW 1976-2005", "Verano RCM RAW 1976-2005", "Otoño RCM RAW 1976-2005",
                                                 "Invierno RCM EQM 1976-2005", "Primavera RCM EQM 1976-2005", "Verano RCM EQM 1976-2005", "Otoño RCM EQM 1976-2005",
                                                 "Invierno RCP RAW 2041-2070", "Primavera RCP RAW 2041-2070", "Verano RCP RAW 2041-2070", "Otoño RCP RAW 2041-2070",
                                                 "Invierno RCP EQM 2041-2070", "Primavera RCP EQM 2041-2070", "Verano RCP EQM 2041-2070", "Otoño RCP EQM 2041-2070",
                                                 "Invierno RCP RAW 2071-2100", "Primavera RCP RAW 2071-2100", "Verano RCP RAW 2071-2100", "Otoño RCP RAW 2071-2100",
                                                 "Invierno RCP EQM 2071-2100", "Primavera RCP EQM 2071-2100", "Verano RCP EQM 2071-2100", "Otoño RCP EQM 2071-2100"),
                    main="SPEI 6M", layout=c(4,7), set.max=0.04, set.min=-0.04, at=seq(-0.04, 0.04, 0.01),
                    colorkey = list(space = "right",
                    title = list(" ", cex = 1)))

pdf("new.s4.1/spei.futuro.pdf", width=16, height=25)
spei
dev.off()

# By months
months <- list(
  January = 1, February = 2, March = 3, April = 4, May = 5, June = 6,
  July = 7, August = 8, September = 9, October = 10, November = 11, December = 12
)

# Función para dividir los datos por meses y crear una lista nombrada
split_by_month <- function(data, months) {
  month_data <- lapply(months, function(month) {
    subsetGrid(data, season = month)
  })
  names(month_data) <- names(months)
  return(month_data)
}

# Dividir los datos de SPEI por meses
spei.obs.months <- split_by_month(spei.obs, months)
spei.rcm.raw.months <- split_by_month(spei.rcm.raw, months)
spei.rcm.eqm.months <- split_by_month(spei.rcm.eqm, months)
spei.rcp.raw.months <- split_by_month(spei.rcp.raw, months)
spei.rcp.eqm.months <- split_by_month(spei.rcp.eqm, months)
spei.rcp.raw2.months <- split_by_month(spei.rcp.raw2, months)
spei.rcp.eqm2.months <- split_by_month(spei.rcp.eqm2, months)

spei.obs.months.plot <- spatialPlot(makeMultiGrid(climatology(spei.obs.months$January), climatology(spei.obs.months$February), climatology(spei.obs.months$March),
                climatology(spei.obs.months$April), climatology(spei.obs.months$May), climatology(spei.obs.months$June),
                climatology(spei.obs.months$July), climatology(spei.obs.months$August), climatology(spei.obs.months$September),
                climatology(spei.obs.months$October), climatology(spei.obs.months$November), climatology(spei.obs.months$December), skip.temporal.check=TRUE),
                backdrop.theme="coastline", as.table=TRUE, color.theme="RdBu", rev.colors=FALSE,
                names.attr=c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre"),
                main="SPEI 6M OBS", layout=c(12,1), set.max=0.04, set.min=-0.04, at=seq(-0.04, 0.04, 0.01),
                colorkey = list(space = "right",
                title = list(" ", cex = 1)))

spei.rcm.raw.months.plot <- spatialPlot(makeMultiGrid(climatology(spei.rcm.raw.months$January), climatology(spei.rcm.raw.months$February), climatology(spei.rcm.raw.months$March),
                climatology(spei.rcm.raw.months$April), climatology(spei.rcm.raw.months$May), climatology(spei.rcm.raw.months$June),
                climatology(spei.rcm.raw.months$July), climatology(spei.rcm.raw.months$August), climatology(spei.rcm.raw.months$September),
                climatology(spei.rcm.raw.months$October), climatology(spei.rcm.raw.months$November), climatology(spei.rcm.raw.months$December), skip.temporal.check=TRUE),
                backdrop.theme="coastline", as.table=TRUE, color.theme="RdBu", rev.colors=FALSE,
                names.attr=c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre"),
                main="SPEI 6M RCM RAW", layout=c(12,1), set.max=0.04, set.min=-0.04, at=seq(-0.04, 0.04, 0.01),
                colorkey = list(space = "right",
                title = list(" ", cex = 1)))

spei.rcm.eqm.months.plot <- spatialPlot(makeMultiGrid(climatology(spei.rcm.eqm.months$January), climatology(spei.rcm.eqm.months$February), climatology(spei.rcm.eqm.months$March),
                climatology(spei.rcm.eqm.months$April), climatology(spei.rcm.eqm.months$May), climatology(spei.rcm.eqm.months$June),
                climatology(spei.rcm.eqm.months$July), climatology(spei.rcm.eqm.months$August), climatology(spei.rcm.eqm.months$September),
                climatology(spei.rcm.eqm.months$October), climatology(spei.rcm.eqm.months$November), climatology(spei.rcm.eqm.months$December), skip.temporal.check=TRUE),
                backdrop.theme="coastline", as.table=TRUE, color.theme="RdBu", rev.colors=FALSE,
                names.attr=c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre"),
                main="SPEI 6M RCM EQM", layout=c(12,1), set.max=0.04, set.min=-0.04, at=seq(-0.04, 0.04, 0.01),
                colorkey = list(space = "right",
                title = list(" ", cex = 1)))

spei.rcp.raw.months.plot <- spatialPlot(makeMultiGrid(climatology(spei.rcp.raw.months$January), climatology(spei.rcp.raw.months$February), climatology(spei.rcp.raw.months$March),
                climatology(spei.rcp.raw.months$April), climatology(spei.rcp.raw.months$May), climatology(spei.rcp.raw.months$June),
                climatology(spei.rcp.raw.months$July), climatology(spei.rcp.raw.months$August), climatology(spei.rcp.raw.months$September),
                climatology(spei.rcp.raw.months$October), climatology(spei.rcp.raw.months$November), climatology(spei.rcp.raw.months$December), skip.temporal.check=TRUE),
                backdrop.theme="coastline", as.table=TRUE, color.theme="RdBu", rev.colors=FALSE,
                names.attr=c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre"),
                main="SPEI 6M RCP RAW 2041-2070", layout=c(12,1), set.max=0.04, set.min=-0.04, at=seq(-0.04, 0.04, 0.01),
                colorkey = list(space = "right",
                title = list(" ", cex = 1)))

spei.rcp.eqm.months.plot <- spatialPlot(makeMultiGrid(climatology(spei.rcp.eqm.months$January), climatology(spei.rcp.eqm.months$February), climatology(spei.rcp.eqm.months$March),
                climatology(spei.rcp.eqm.months$April), climatology(spei.rcp.eqm.months$May), climatology(spei.rcp.eqm.months$June),
                climatology(spei.rcp.eqm.months$July), climatology(spei.rcp.eqm.months$August), climatology(spei.rcp.eqm.months$September),
                climatology(spei.rcp.eqm.months$October), climatology(spei.rcp.eqm.months$November), climatology(spei.rcp.eqm.months$December), skip.temporal.check=TRUE),
                backdrop.theme="coastline", as.table=TRUE, color.theme="RdBu", rev.colors=FALSE,
                names.attr=c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre"),
                main="SPEI 6M RCP EQM 2041-2070", layout=c(12,1), set.max=0.04, set.min=-0.04, at=seq(-0.04, 0.04, 0.01),
                colorkey = list(space = "right",
                title = list(" ", cex = 1)))

spei.rcp.raw2.months.plot <- spatialPlot(makeMultiGrid(climatology(spei.rcp.raw2.months$January), climatology(spei.rcp.raw2.months$February), climatology(spei.rcp.raw2.months$March),
                climatology(spei.rcp.raw2.months$April), climatology(spei.rcp.raw2.months$May), climatology(spei.rcp.raw2.months$June),
                climatology(spei.rcp.raw2.months$July), climatology(spei.rcp.raw2.months$August), climatology(spei.rcp.raw2.months$September),
                climatology(spei.rcp.raw2.months$October), climatology(spei.rcp.raw2.months$November), climatology(spei.rcp.raw2.months$December), skip.temporal.check=TRUE),
                backdrop.theme="coastline", as.table=TRUE, color.theme="RdBu", rev.colors=FALSE,
                names.attr=c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre"),
                main="SPEI 6M RCP RAW 2071-2100", layout=c(12,1), set.max=0.04, set.min=-0.04, at=seq(-0.04, 0.04, 0.01),
                colorkey = list(space = "right",
                title = list(" ", cex = 1)))

spei.rcp.eqm2.months.plot <- spatialPlot(makeMultiGrid(climatology(spei.rcp.eqm2.months$January), climatology(spei.rcp.eqm2.months$February), climatology(spei.rcp.eqm2.months$March),
                climatology(spei.rcp.eqm2.months$April), climatology(spei.rcp.eqm2.months$May), climatology(spei.rcp.eqm2.months$June),
                climatology(spei.rcp.eqm2.months$July), climatology(spei.rcp.eqm2.months$August), climatology(spei.rcp.eqm2.months$September),
                climatology(spei.rcp.eqm2.months$October), climatology(spei.rcp.eqm2.months$November), climatology(spei.rcp.eqm2.months$December), skip.temporal.check=TRUE),
                backdrop.theme="coastline", as.table=TRUE, color.theme="RdBu", rev.colors=FALSE,
                names.attr=c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre"),
                main="SPEI 6M RCP EQM 2071-2100", layout=c(12,1), set.max=0.04, set.min=-0.04, at=seq(-0.04, 0.04, 0.01),
                colorkey = list(space = "right",
                title = list(" ", cex = 1)))

library(gridExtra)
library(grid)

pdf("new.s4.1/spei.futuro.months.pdf", width=48, height=28)
grid.arrange(spei.obs.months.plot, spei.rcm.raw.months.plot, spei.rcm.eqm.months.plot, spei.rcp.raw.months.plot, spei.rcp.eqm.months.plot, spei.rcp.raw2.months.plot, spei.rcp.eqm2.months.plot, ncol=1)
dev.off()

####################################################
############## Métricas del SPEI ###################
####################################################

# Suponiendo que spei.mon$Data tiene dimensiones [time, y, x]
spei.mon <- spei.rcp.raw
time_dim <- dim(spei.mon$Data)[1]
y_dim <- length(getCoordinates(spei.mon)$y)
x_dim <- length(getCoordinates(spei.mon)$x)

# Inicializar los arrays con NA
median.DD <- array(NA, dim = c(1, y_dim, x_dim))
median.DS <- array(NA, dim = c(1, y_dim, x_dim))
DF <- array(NA, dim = c(1, y_dim, x_dim))
mes <- 3
umbral <- -2.33
tipo <- "rcp.raw"

for(i in 1:y_dim){
    for(j in 1:x_dim){
        # Subset one pixel
        subset.spei <- spei.mon$Data[,i,j]
        subset.spei <- subset.spei[!is.na(subset.spei)]     
        
        # Duration calculations
        dry.thres <- -umbral
        dry.obj <- rle(subset.spei <= dry.thres) 
        
        all.dry.spells <- dry.obj$lengths[dry.obj$values == TRUE]
        
        DD <- all.dry.spells[all.dry.spells >= mes]
        if (length(DD) > 0) {
            median.DD[1,i,j] <- median(DD)
        } else {
            median.DD[1,i,j] <- NA
        }
        
        # Severity calculation    
        # Dry event severity
        dry.ind <- which(dry.obj$values) 
        DS <- NULL
        
        if (length(dry.ind) > 0) {
            for(k in 1:length(dry.ind)){
                if (!is.na(dry.obj$lengths[dry.ind[k]]) && dry.obj$lengths[dry.ind[k]] >= mes){ 
                    dry.indSev <- sum(dry.obj$lengths[1:(dry.ind[k]-1)]) 
                    dry.aux <- sum(subset.spei[(dry.indSev+1):(dry.indSev+all.dry.spells[k])]) 
                    DS <- c(DS, dry.aux) 
                }
            }
        }

        if (!is.null(DS) && length(DS) > 0) {
            median.DS[1,i,j] <- median(DS)
        } else {
            median.DS[1,i,j] <- NA
        }
        
        # Absolute Frequency calculations
        DF[1,i,j] <- length(DD)
    }
}


capatierra <- climatology(spei.mon)
capatierra$Data[!is.na(capatierra$Data)] <- 1

# # Duracion
# med.DD.promedio <- spei.mon 
# med.DD.promedio$Data <- median.DD
# attr(med.DD.promedio$Data,"dimensions")<- c("time" ,"lat", "lon")
# saveRDS(med.DD.promedio, "eqm1/duration.-2.33_1M.eqm1.rds", compress="xz")

# # Severidad
# med.DS.promedio <- spei.mon
# med.DS.promedio$Data <- median.DS
# attr(med.DS.promedio$Data,"dimensions")<- c("time" ,"lat", "lon")
# saveRDS(med.DS.promedio, "eqm1/severity.-2.33_1M.eqm1.rds", compress="xz")

# Frecuencia
DF.promedio <- spei.mon
DF.promedio$Data <- DF
attr(DF.promedio$Data,"dimensions")<- c("time" ,"lat", "lon")
DF.promedio <- gridArithmetics(DF.promedio, capatierra, operator = "*")
saveRDS(DF.promedio, paste0("frecuency.", umbral, "_", mes, "M.", tipo,".rds"), compress="xz")

################################
# Leer archivos

# OBS
f.1.leve.obs <- readRDS("frecuency.-0.84_1M.obs.rds")
f.1.moderado.obs <- readRDS("frecuency.-1.28_1M.obs.rds")
f.1.severo.obs <- readRDS("frecuency.-1.65_1M.obs.rds")
f.1.extremo.obs <- readRDS("frecuency.-2.33_1M.obs.rds")

f.2.leve.obs <- readRDS("frecuency.-0.84_2M.obs.rds")
f.2.moderado.obs <- readRDS("frecuency.-1.28_2M.obs.rds")
f.2.severo.obs <- readRDS("frecuency.-1.65_2M.obs.rds")
f.2.extremo.obs <- readRDS("frecuency.-2.33_2M.obs.rds")

f.3.leve.obs <- readRDS("frecuency.-0.84_3M.obs.rds")
f.3.moderado.obs <- readRDS("frecuency.-1.28_3M.obs.rds")
f.3.severo.obs <- readRDS("frecuency.-1.65_3M.obs.rds")
f.3.extremo.obs <- readRDS("frecuency.-2.33_3M.obs.rds")

pdf("frecuency.obs.pdf", width=16, height=12)
spatialPlot(makeMultiGrid(f.1.leve.obs, f.1.moderado.obs, f.1.severo.obs, f.1.extremo.obs,
                          f.2.leve.obs, f.2.moderado.obs, f.2.severo.obs, f.2.extremo.obs,
                          f.3.leve.obs, f.3.moderado.obs, f.3.severo.obs, f.3.extremo.obs,
                          skip.temporal.check=TRUE),
            backdrop.theme="coastline", as.table=TRUE, color.theme="Reds", rev.colors=FALSE,
            names.attr=c("Leve 1M", "Moderado 1M", "Severo 1M", "Extremo 1M",
                         "Leve 2M", "Moderado 2M", "Severo 2M", "Extremo 2M",
                         "Leve 3M", "Moderado 3M", "Severo 3M", "Extremo 3M"),
            main="Frecuencia de sequías OBS", layout=c(4,3), set.max=30, set.min=0, at=seq(0, 30, 1),
            colorkey = list(space = "right",
            title = list("Nº de sequías", cex = 1)))
dev.off()


# RCM RAW
f.1.leve.rcm.raw <- readRDS("frecuency.-0.84_1M.rcm.raw.rds")
f.1.moderado.rcm.raw <- readRDS("frecuency.-1.28_1M.rcm.raw.rds")
f.1.severo.rcm.raw <- readRDS("frecuency.-1.65_1M.rcm.raw.rds")
f.1.extremo.rcm.raw <- readRDS("frecuency.-2.33_1M.rcm.raw.rds")

f.2.leve.rcm.raw <- readRDS("frecuency.-0.84_2M.rcm.raw.rds")
f.2.moderado.rcm.raw <- readRDS("frecuency.-1.28_2M.rcm.raw.rds")
f.2.severo.rcm.raw <- readRDS("frecuency.-1.65_2M.rcm.raw.rds")
f.2.extremo.rcm.raw <- readRDS("frecuency.-2.33_2M.rcm.raw.rds")

f.3.leve.rcm.raw <- readRDS("frecuency.-0.84_3M.rcm.raw.rds")
f.3.moderado.rcm.raw <- readRDS("frecuency.-1.28_3M.rcm.raw.rds")
f.3.severo.rcm.raw <- readRDS("frecuency.-1.65_3M.rcm.raw.rds")
f.3.extremo.rcm.raw <- readRDS("frecuency.-2.33_3M.rcm.raw.rds")

f.1.leve.rcm.raw <- gridArithmetics(f.1.leve.rcm.raw, f.1.leve.obs, operator = "-")
f.1.moderado.rcm.raw <- gridArithmetics(f.1.moderado.rcm.raw, f.1.moderado.obs, operator = "-")
f.1.severo.rcm.raw <- gridArithmetics(f.1.severo.rcm.raw, f.1.severo.obs, operator = "-")
f.1.extremo.rcm.raw <- gridArithmetics(f.1.extremo.rcm.raw, f.1.extremo.obs, operator = "-")

f.2.leve.rcm.raw <- gridArithmetics(f.2.leve.rcm.raw, f.2.leve.obs, operator = "-")
f.2.moderado.rcm.raw <- gridArithmetics(f.2.moderado.rcm.raw, f.2.moderado.obs, operator = "-")
f.2.severo.rcm.raw <- gridArithmetics(f.2.severo.rcm.raw, f.2.severo.obs, operator = "-")
f.2.extremo.rcm.raw <- gridArithmetics(f.2.extremo.rcm.raw, f.2.extremo.obs, operator = "-")

f.3.leve.rcm.raw <- gridArithmetics(f.3.leve.rcm.raw, f.3.leve.obs, operator = "-")
f.3.moderado.rcm.raw <- gridArithmetics(f.3.moderado.rcm.raw, f.3.moderado.obs, operator = "-")
f.3.severo.rcm.raw <- gridArithmetics(f.3.severo.rcm.raw, f.3.severo.obs, operator = "-")
f.3.extremo.rcm.raw <- gridArithmetics(f.3.extremo.rcm.raw, f.3.extremo.obs, operator = "-")

pdf("frecuency.rcm.raw.pdf", width=16, height=12)
spatialPlot(makeMultiGrid(f.1.leve.rcm.raw, f.1.moderado.rcm.raw, f.1.severo.rcm.raw, f.1.extremo.rcm.raw,
                          f.2.leve.rcm.raw, f.2.moderado.rcm.raw, f.2.severo.rcm.raw, f.2.extremo.rcm.raw,
                          f.3.leve.rcm.raw, f.3.moderado.rcm.raw, f.3.severo.rcm.raw, f.3.extremo.rcm.raw,
                          skip.temporal.check=TRUE),
            backdrop.theme="coastline", as.table=TRUE, color.theme="Reds", rev.colors=FALSE,
            names.attr=c("Leve 1M", "Moderado 1M", "Severo 1M", "Extremo 1M",
                         "Leve 2M", "Moderado 2M", "Severo 2M", "Extremo 2M",
                         "Leve 3M", "Moderado 3M", "Severo 3M", "Extremo 3M"),
            main="Frecuencia de sequías histórico RAW", layout=c(4,3), set.max=30, set.min=0, at=seq(0, 30, 1),
            colorkey = list(space = "right",
            title = list("Nº de sequías", cex = 1)))
dev.off()

# RCM EQM
f.1.leve.rcm.eqm <- readRDS("frecuency.-0.84_1M.rcm.eqm.rds")
f.1.moderado.rcm.eqm <- readRDS("frecuency.-1.28_1M.rcm.eqm.rds")
f.1.severo.rcm.eqm <- readRDS("frecuency.-1.65_1M.rcm.eqm.rds")
f.1.extremo.rcm.eqm <- readRDS("frecuency.-2.33_1M.rcm.eqm.rds")

f.2.leve.rcm.eqm <- readRDS("frecuency.-0.84_2M.rcm.eqm.rds")
f.2.moderado.rcm.eqm <- readRDS("frecuency.-1.28_2M.rcm.eqm.rds")
f.2.severo.rcm.eqm <- readRDS("frecuency.-1.65_2M.rcm.eqm.rds")
f.2.extremo.rcm.eqm <- readRDS("frecuency.-2.33_2M.rcm.eqm.rds")

f.3.leve.rcm.eqm <- readRDS("frecuency.-0.84_3M.rcm.eqm.rds")
f.3.moderado.rcm.eqm <- readRDS("frecuency.-1.28_3M.rcm.eqm.rds")
f.3.severo.rcm.eqm <- readRDS("frecuency.-1.65_3M.rcm.eqm.rds")
f.3.extremo.rcm.eqm <- readRDS("frecuency.-2.33_3M.rcm.eqm.rds")

f.1.leve.rcm.eqm <- gridArithmetics(f.1.leve.rcm.eqm, f.1.leve.obs, operator = "-")
f.1.moderado.rcm.eqm <- gridArithmetics(f.1.moderado.rcm.eqm, f.1.moderado.obs, operator = "-")
f.1.severo.rcm.eqm <- gridArithmetics(f.1.severo.rcm.eqm, f.1.severo.obs, operator = "-")
f.1.extremo.rcm.eqm <- gridArithmetics(f.1.extremo.rcm.eqm, f.1.extremo.obs, operator = "-")

f.2.leve.rcm.eqm <- gridArithmetics(f.2.leve.rcm.eqm, f.2.leve.obs, operator = "-")
f.2.moderado.rcm.eqm <- gridArithmetics(f.2.moderado.rcm.eqm, f.2.moderado.obs, operator = "-")
f.2.severo.rcm.eqm <- gridArithmetics(f.2.severo.rcm.eqm, f.2.severo.obs, operator = "-")
f.2.extremo.rcm.eqm <- gridArithmetics(f.2.extremo.rcm.eqm, f.2.extremo.obs, operator = "-")

f.3.leve.rcm.eqm <- gridArithmetics(f.3.leve.rcm.eqm, f.3.leve.obs, operator = "-")
f.3.moderado.rcm.eqm <- gridArithmetics(f.3.moderado.rcm.eqm, f.3.moderado.obs, operator = "-")
f.3.severo.rcm.eqm <- gridArithmetics(f.3.severo.rcm.eqm, f.3.severo.obs, operator = "-")
f.3.extremo.rcm.eqm <- gridArithmetics(f.3.extremo.rcm.eqm, f.3.extremo.obs, operator = "-")

pdf("frecuency.rcm.eqm.pdf", width=16, height=12)
spatialPlot(makeMultiGrid(f.1.leve.rcm.eqm, f.1.moderado.rcm.eqm, f.1.severo.rcm.eqm, f.1.extremo.rcm.eqm,
                          f.2.leve.rcm.eqm, f.2.moderado.rcm.eqm, f.2.severo.rcm.eqm, f.2.extremo.rcm.eqm,
                          f.3.leve.rcm.eqm, f.3.moderado.rcm.eqm, f.3.severo.rcm.eqm, f.3.extremo.rcm.eqm,
                          skip.temporal.check=TRUE),
            backdrop.theme="coastline", as.table=TRUE, color.theme="RdBu", rev.colors=TRUE,
            names.attr=c("Leve 1M", "Moderado 1M", "Severo 1M", "Extremo 1M",
                         "Leve 2M", "Moderado 2M", "Severo 2M", "Extremo 2M",
                         "Leve 3M", "Moderado 3M", "Severo 3M", "Extremo 3M"),
            main="Sesgo de la frecuencia de sequías histórico EQM", layout=c(4,3), set.max=10, set.min=-10, at=seq(-10, 10, 1),
            colorkey = list(space = "right",
            title = list("Nº de sequías", cex = 1)))
dev.off()

# RCP eqm 1
f.1.leve.rcp.eqm <- readRDS("frecuency.-0.84_1M.rcp.eqm.1.rds")
f.1.moderado.rcp.eqm <- readRDS("frecuency.-1.28_1M.rcp.eqm.1.rds")
f.1.severo.rcp.eqm <- readRDS("frecuency.-1.65_1M.rcp.eqm.1.rds")
f.1.extremo.rcp.eqm <- readRDS("frecuency.-2.33_1M.rcp.eqm.1.rds")

f.2.leve.rcp.eqm <- readRDS("frecuency.-0.84_2M.rcp.eqm.1.rds")
f.2.moderado.rcp.eqm <- readRDS("frecuency.-1.28_2M.rcp.eqm.1.rds")
f.2.severo.rcp.eqm <- readRDS("frecuency.-1.65_2M.rcp.eqm.1.rds")
f.2.extremo.rcp.eqm <- readRDS("frecuency.-2.33_2M.rcp.eqm.1.rds")

f.3.leve.rcp.eqm <- readRDS("frecuency.-0.84_3M.rcp.eqm.1.rds")
f.3.moderado.rcp.eqm <- readRDS("frecuency.-1.28_3M.rcp.eqm.1.rds")
f.3.severo.rcp.eqm <- readRDS("frecuency.-1.65_3M.rcp.eqm.1.rds")
f.3.extremo.rcp.eqm <- readRDS("frecuency.-2.33_3M.rcp.eqm.1.rds")

f.1.leve.rcp.eqm <- gridArithmetics(f.1.leve.rcp.eqm, f.1.leve.rcm.eqm, operator = "-")
f.1.moderado.rcp.eqm <- gridArithmetics(f.1.moderado.rcp.eqm, f.1.moderado.rcm.eqm, operator = "-")
f.1.severo.rcp.eqm <- gridArithmetics(f.1.severo.rcp.eqm, f.1.severo.rcm.eqm, operator = "-")
f.1.extremo.rcp.eqm <- gridArithmetics(f.1.extremo.rcp.eqm, f.1.extremo.rcm.eqm, operator = "-")

f.2.leve.rcp.eqm <- gridArithmetics(f.2.leve.rcp.eqm, f.2.leve.rcm.eqm, operator = "-")
f.2.moderado.rcp.eqm <- gridArithmetics(f.2.moderado.rcp.eqm, f.2.moderado.rcm.eqm, operator = "-")
f.2.severo.rcp.eqm <- gridArithmetics(f.2.severo.rcp.eqm, f.2.severo.rcm.eqm, operator = "-")
f.2.extremo.rcp.eqm <- gridArithmetics(f.2.extremo.rcp.eqm, f.2.extremo.rcm.eqm, operator = "-")

f.3.leve.rcp.eqm <- gridArithmetics(f.3.leve.rcp.eqm, f.3.leve.rcm.eqm, operator = "-")
f.3.moderado.rcp.eqm <- gridArithmetics(f.3.moderado.rcp.eqm, f.3.moderado.rcm.eqm, operator = "-")
f.3.severo.rcp.eqm <- gridArithmetics(f.3.severo.rcp.eqm, f.3.severo.rcm.eqm, operator = "-")
f.3.extremo.rcp.eqm <- gridArithmetics(f.3.extremo.rcp.eqm, f.3.extremo.rcm.eqm, operator = "-")


pdf("frecuency.rcp.eqm.1.pdf", width=16, height=12)
spatialPlot(makeMultiGrid(f.1.leve.rcp.eqm, f.1.moderado.rcp.eqm, f.1.severo.rcp.eqm, f.1.extremo.rcp.eqm,
                          f.2.leve.rcp.eqm, f.2.moderado.rcp.eqm, f.2.severo.rcp.eqm, f.2.extremo.rcp.eqm,
                          f.3.leve.rcp.eqm, f.3.moderado.rcp.eqm, f.3.severo.rcp.eqm, f.3.extremo.rcp.eqm,
                          skip.temporal.check=TRUE),
            backdrop.theme="coastline", as.table=TRUE, color.theme="RdBu", rev.colors=TRUE,
            names.attr=c("Leve 1M", "Moderado 1M", "Severo 1M", "Extremo 1M",
                         "Leve 2M", "Moderado 2M", "Severo 2M", "Extremo 2M",
                         "Leve 3M", "Moderado 3M", "Severo 3M", "Extremo 3M"),
            main="Frecuencia de sequías futuro EQM (2041-2070)", layout=c(4,3), set.max=10, set.min=-10, at=seq(-10, 10, 1),
            colorkey = list(space = "right",
            title = list("Nº de eventos", cex = 1)))
dev.off()

# RCP eqm 2
f.1.leve.rcp.eqm2 <- readRDS("frecuency.-0.84_1M.rcp.eqm.2.rds")
f.1.moderado.rcp.eqm2 <- readRDS("frecuency.-1.28_1M.rcp.eqm.2.rds")
f.1.severo.rcp.eqm2 <- readRDS("frecuency.-1.65_1M.rcp.eqm.2.rds")
f.1.extremo.rcp.eqm2 <- readRDS("frecuency.-2.33_1M.rcp.eqm.2.rds")

f.2.leve.rcp.eqm2 <- readRDS("frecuency.-0.84_2M.rcp.eqm.2.rds")
f.2.moderado.rcp.eqm2 <- readRDS("frecuency.-1.28_2M.rcp.eqm.2.rds")
f.2.severo.rcp.eqm2 <- readRDS("frecuency.-1.65_2M.rcp.eqm.2.rds")
f.2.extremo.rcp.eqm2 <- readRDS("frecuency.-2.33_2M.rcp.eqm.2.rds")

f.3.leve.rcp.eqm2 <- readRDS("frecuency.-0.84_3M.rcp.eqm.2.rds")
f.3.moderado.rcp.eqm2 <- readRDS("frecuency.-1.28_3M.rcp.eqm.2.rds")
f.3.severo.rcp.eqm2 <- readRDS("frecuency.-1.65_3M.rcp.eqm.2.rds")
f.3.extremo.rcp.eqm2 <- readRDS("frecuency.-2.33_3M.rcp.eqm.2.rds")

f.1.leve.rcp.eqm2 <- gridArithmetics(f.1.leve.rcp.eqm2, f.1.leve.rcm.eqm, operator = "-")
f.1.moderado.rcp.eqm2 <- gridArithmetics(f.1.moderado.rcp.eqm2, f.1.moderado.rcm.eqm, operator = "-")
f.1.severo.rcp.eqm2 <- gridArithmetics(f.1.severo.rcp.eqm2, f.1.severo.rcm.eqm, operator = "-")
f.1.extremo.rcp.eqm2 <- gridArithmetics(f.1.extremo.rcp.eqm2, f.1.extremo.rcm.eqm, operator = "-")

f.2.leve.rcp.eqm2 <- gridArithmetics(f.2.leve.rcp.eqm2, f.2.leve.rcm.eqm, operator = "-")
f.2.moderado.rcp.eqm2 <- gridArithmetics(f.2.moderado.rcp.eqm2, f.2.moderado.rcm.eqm, operator = "-")
f.2.severo.rcp.eqm2 <- gridArithmetics(f.2.severo.rcp.eqm2, f.2.severo.rcm.eqm, operator = "-")
f.2.extremo.rcp.eqm2 <- gridArithmetics(f.2.extremo.rcp.eqm2, f.2.extremo.rcm.eqm, operator = "-")

f.3.leve.rcp.eqm2 <- gridArithmetics(f.3.leve.rcp.eqm2, f.3.leve.rcm.eqm, operator = "-")
f.3.moderado.rcp.eqm2 <- gridArithmetics(f.3.moderado.rcp.eqm2, f.3.moderado.rcm.eqm, operator = "-")
f.3.severo.rcp.eqm2 <- gridArithmetics(f.3.severo.rcp.eqm2, f.3.severo.rcm.eqm, operator = "-")
f.3.extremo.rcp.eqm2 <- gridArithmetics(f.3.extremo.rcp.eqm2, f.3.extremo.rcm.eqm, operator = "-")

pdf("frecuency.rcp.eqm2.delta.pdf", width=16, height=12)
spatialPlot(makeMultiGrid(f.1.leve.rcp.eqm2, f.1.moderado.rcp.eqm2, f.1.severo.rcp.eqm2, f.1.extremo.rcp.eqm2,
                          f.2.leve.rcp.eqm2, f.2.moderado.rcp.eqm2, f.2.severo.rcp.eqm2, f.2.extremo.rcp.eqm2,
                          f.3.leve.rcp.eqm2, f.3.moderado.rcp.eqm2, f.3.severo.rcp.eqm2, f.3.extremo.rcp.eqm2,
                          skip.temporal.check=TRUE),
            backdrop.theme="coastline", as.table=TRUE, color.theme="RdBu", rev.colors=TRUE,
            names.attr=c("Leve 1M", "Moderado 1M", "Severo 1M", "Extremo 1M",
                         "Leve 2M", "Moderado 2M", "Severo 2M", "Extremo 2M",
                         "Leve 3M", "Moderado 3M", "Severo 3M", "Extremo 3M"),
            main="Señal de cambio climático de la frecuencia de sequías futuro EQM (2071-2100)", layout=c(4,3), set.max=10, set.min=-10, at=seq(-10, 10, 1),
            colorkey = list(space = "right",
            title = list("Nº de sequías", cex = 1)))
dev.off()

# RCP raw 2 
f.1.leve.rcp.raw2 <- readRDS("frecuency.-0.84_1M.rcp.raw2.rds")
f.1.moderado.rcp.raw2 <- readRDS("frecuency.-1.28_1M.rcp.raw2.rds")
f.1.severo.rcp.raw2 <- readRDS("frecuency.-1.65_1M.rcp.raw2.rds")
f.1.extremo.rcp.raw2 <- readRDS("frecuency.-2.33_1M.rcp.raw2.rds")

f.2.leve.rcp.raw2 <- readRDS("frecuency.-0.84_2M.rcp.raw2.rds")
f.2.moderado.rcp.raw2 <- readRDS("frecuency.-1.28_2M.rcp.raw2.rds")
f.2.severo.rcp.raw2 <- readRDS("frecuency.-1.65_2M.rcp.raw2.rds")
f.2.extremo.rcp.raw2 <- readRDS("frecuency.-2.33_2M.rcp.raw2.rds")

f.3.leve.rcp.raw2 <- readRDS("frecuency.-0.84_3M.rcp.raw2.rds")
f.3.moderado.rcp.raw2 <- readRDS("frecuency.-1.28_3M.rcp.raw2.rds")
f.3.severo.rcp.raw2 <- readRDS("frecuency.-1.65_3M.rcp.raw2.rds")
f.3.extremo.rcp.raw2 <- readRDS("frecuency.-2.33_3M.rcp.raw2.rds")

f.1.leve.rcp.raw2 <- gridArithmetics(f.1.leve.rcp.raw2, f.1.leve.rcm.raw, operator = "-")
f.1.moderado.rcp.raw2 <- gridArithmetics(f.1.moderado.rcp.raw2, f.1.moderado.rcm.raw, operator = "-")
f.1.severo.rcp.raw2 <- gridArithmetics(f.1.severo.rcp.raw2, f.1.severo.rcm.raw, operator = "-")
f.1.extremo.rcp.raw2 <- gridArithmetics(f.1.extremo.rcp.raw2, f.1.extremo.rcm.raw, operator = "-")

f.2.leve.rcp.raw2 <- gridArithmetics(f.2.leve.rcp.raw2, f.2.leve.rcm.raw, operator = "-")
f.2.moderado.rcp.raw2 <- gridArithmetics(f.2.moderado.rcp.raw2, f.2.moderado.rcm.raw, operator = "-")
f.2.severo.rcp.raw2 <- gridArithmetics(f.2.severo.rcp.raw2, f.2.severo.rcm.raw, operator = "-")
f.2.extremo.rcp.raw2 <- gridArithmetics(f.2.extremo.rcp.raw2, f.2.extremo.rcm.raw, operator = "-")

f.3.leve.rcp.raw2 <- gridArithmetics(f.3.leve.rcp.raw2, f.3.leve.rcm.raw, operator = "-")
f.3.moderado.rcp.raw2 <- gridArithmetics(f.3.moderado.rcp.raw2, f.3.moderado.rcm.raw, operator = "-")
f.3.severo.rcp.raw2 <- gridArithmetics(f.3.severo.rcp.raw2, f.3.severo.rcm.raw, operator = "-")
f.3.extremo.rcp.raw2 <- gridArithmetics(f.3.extremo.rcp.raw2, f.3.extremo.rcm.raw, operator = "-")

pdf("frecuency.rcp.raw2.pdf", width=16, height=12)
spatialPlot(makeMultiGrid(f.1.leve.rcp.raw2, f.1.moderado.rcp.raw2, f.1.severo.rcp.raw2, f.1.extremo.rcp.raw2,
                          f.2.leve.rcp.raw2, f.2.moderado.rcp.raw2, f.2.severo.rcp.raw2, f.2.extremo.rcp.raw2,
                          f.3.leve.rcp.raw2, f.3.moderado.rcp.raw2, f.3.severo.rcp.raw2, f.3.extremo.rcp.raw2,
                          skip.temporal.check=TRUE),
            backdrop.theme="coastline", as.table=TRUE, color.theme="Reds", rev.colors=FALSE,
            names.attr=c("Leve 1M", "Moderado 1M", "Severo 1M", "Extremo 1M",
                         "Leve 2M", "Moderado 2M", "Severo 2M", "Extremo 2M",
                         "Leve 3M", "Moderado 3M", "Severo 3M", "Extremo 3M"),
            main="Frecuencia de sequías futuro RAW (2071-2100)", layout=c(4,3), set.max=30, set.min=0, at=seq(0, 30, 1),
            colorkey = list(space = "right",
            title = list("Nº de sequías", cex = 1)))
dev.off()

# RCP raw
f.1.leve.rcp.raw <- readRDS("frecuency.-0.84_1M.rcp.raw.rds")
f.1.moderado.rcp.raw <- readRDS("frecuency.-1.28_1M.rcp.raw.rds")
f.1.severo.rcp.raw <- readRDS("frecuency.-1.65_1M.rcp.raw.rds")
f.1.extremo.rcp.raw <- readRDS("frecuency.-2.33_1M.rcp.raw.rds")

f.2.leve.rcp.raw <- readRDS("frecuency.-0.84_2M.rcp.raw.rds")
f.2.moderado.rcp.raw <- readRDS("frecuency.-1.28_2M.rcp.raw.rds")
f.2.severo.rcp.raw <- readRDS("frecuency.-1.65_2M.rcp.raw.rds")
f.2.extremo.rcp.raw <- readRDS("frecuency.-2.33_2M.rcp.raw.rds")

f.3.leve.rcp.raw <- readRDS("frecuency.-0.84_3M.rcp.raw.rds")
f.3.moderado.rcp.raw <- readRDS("frecuency.-1.28_3M.rcp.raw.rds")
f.3.severo.rcp.raw <- readRDS("frecuency.-1.65_3M.rcp.raw.rds")
f.3.extremo.rcp.raw <- readRDS("frecuency.-2.33_3M.rcp.raw.rds")

f.1.leve.rcp.raw <- gridArithmetics(f.1.leve.rcp.raw, f.1.leve.rcm.raw, operator = "-")
f.1.moderado.rcp.raw <- gridArithmetics(f.1.moderado.rcp.raw, f.1.moderado.rcm.raw, operator = "-")
f.1.severo.rcp.raw <- gridArithmetics(f.1.severo.rcp.raw, f.1.severo.rcm.raw, operator = "-")
f.1.extremo.rcp.raw <- gridArithmetics(f.1.extremo.rcp.raw, f.1.extremo.rcm.raw, operator = "-")

f.2.leve.rcp.raw <- gridArithmetics(f.2.leve.rcp.raw, f.2.leve.rcm.raw, operator = "-")
f.2.moderado.rcp.raw <- gridArithmetics(f.2.moderado.rcp.raw, f.2.moderado.rcm.raw, operator = "-")
f.2.severo.rcp.raw <- gridArithmetics(f.2.severo.rcp.raw, f.2.severo.rcm.raw, operator = "-")
f.2.extremo.rcp.raw <- gridArithmetics(f.2.extremo.rcp.raw, f.2.extremo.rcm.raw, operator = "-")

f.3.leve.rcp.raw <- gridArithmetics(f.3.leve.rcp.raw, f.3.leve.rcm.raw, operator = "-")
f.3.moderado.rcp.raw <- gridArithmetics(f.3.moderado.rcp.raw, f.3.moderado.rcm.raw, operator = "-")
f.3.severo.rcp.raw <- gridArithmetics(f.3.severo.rcp.raw, f.3.severo.rcm.raw, operator = "-")
f.3.extremo.rcp.raw <- gridArithmetics(f.3.extremo.rcp.raw, f.3.extremo.rcm.raw, operator = "-")

pdf("frecuency.rcp.raw.pdf", width=16, height=12)
spatialPlot(makeMultiGrid(f.1.leve.rcp.raw, f.1.moderado.rcp.raw, f.1.severo.rcp.raw, f.1.extremo.rcp.raw,
                          f.2.leve.rcp.raw, f.2.moderado.rcp.raw, f.2.severo.rcp.raw, f.2.extremo.rcp.raw,
                          f.3.leve.rcp.raw, f.3.moderado.rcp.raw, f.3.severo.rcp.raw, f.3.extremo.rcp.raw,
                          skip.temporal.check=TRUE),
            backdrop.theme="coastline", as.table=TRUE, color.theme="Reds", rev.colors=FALSE,
            names.attr=c("Leve 1M", "Moderado 1M", "Severo 1M", "Extremo 1M",
                         "Leve 2M", "Moderado 2M", "Severo 2M", "Extremo 2M",
                         "Leve 3M", "Moderado 3M", "Severo 3M", "Extremo 3M"),
            main="Frecuencia de sequías futuro RAW (2071-2100)", layout=c(4,3), set.max=30, set.min=0, at=seq(0, 30, 1),
            colorkey = list(space = "right",
            title = list("Nº de sequías", cex = 1)))
dev.off()

########################################################################################
############## SPEI SERIE TEMPORAL, GRIDBOX CONCRETO ###################################
########################################################################################

####################################################################################################################
pr.obs <- readRDS("/lustre/gmeteo/WORK/fuentesm/TFM/section4.2/ventana.movil/pr/pr.obs.DD.1976-2005.rds")
tmax.obs <- readRDS("/lustre/gmeteo/WORK/fuentesm/TFM/section4.2/ventana.movil/tmax/tasmax.obs.DD.1976-2005.rds")
tmin.obs <- readRDS("/lustre/gmeteo/WORK/fuentesm/TFM/section4.2/ventana.movil/tmin/tasmin.obs.DD.1976-2005.rds")

pr.obs <- redim(pr.obs, drop = TRUE)
tmax.obs <- redim(tmax.obs, drop = TRUE)
tmin.obs <- redim(tmin.obs, drop = TRUE)

pet.obs <- petGrid(tasmin = tmin.obs, tasmax = tmax.obs, pr = pr.obs, method = "hargreaves-samani")
pet.obs <- redim(pet.obs, drop = TRUE)
pr.obs.MM <- aggregateGrid(grid = pr.obs, aggr.m = list(FUN = "sum", na.rm = FALSE))
pet.obs.MM <- aggregateGrid(grid = pet.obs, aggr.m = list(FUN = "mean", na.rm = FALSE))
spei.obs <- speiGrid(et0.grid = pet.obs.MM, pr.grid = pr.obs.MM, scale = 6, ref.start = c(1976, 1), ref.end = c(2005, 12), na.rm = TRUE)

jaen.obs <- subsetGrid(spei.obs, lonLim = -3.79, latLim = 37,77, years =1976:2005)

pdf("spei.gridbox.pdf", width=12, height=4)
plot(ts(jaen.obs$Data, start =c(1976,1), frequency=12), main="SPEI-6 OBS", ylab="SPEI-6", xlab="Meses")
grid()
abline(h=0)
dev.off()
