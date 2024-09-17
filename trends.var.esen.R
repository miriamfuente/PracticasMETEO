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


pr <- readRDS("/lustre/gmeteo/WORK/fuentesm/TFM/section4.1/pr.obs.DD.1971-2020.rds")
tmax <- readRDS("/lustre/gmeteo/WORK/fuentesm/TFM/section4.1/tasmax.obs.DD.1971-2020.rds")
tmin <- readRDS("/lustre/gmeteo/WORK/fuentesm/TFM/section4.1/tasmin.obs.DD.1971-2020.rds")

years = 1971:2020

tmax <- subsetGrid(tmax, years = years)
tmin <- subsetGrid(tmin, years = years)
pr <- subsetGrid(pr, years = years)

tmax.MM <- aggregateGrid(tmax, aggr.m = list(FUN = "mean", na.rm = FALSE))
tmin.MM <- aggregateGrid(tmin, aggr.m = list(FUN = "mean", na.rm = FALSE))
pr.MM <- aggregateGrid(pr, aggr.m = list(FUN = "sum", na.rm = FALSE))


seasons <- list(Winter = c(1,2,12), Spring = c(3, 4, 5), Summer = c(6, 7, 8), Autumn = c(9, 10, 11))

pr.list <- list()
tmax.list <- list()
tmin.list <- list()

for (season in names(seasons)) {
    season.pr <- subsetGrid(pr.MM, season = seasons[[season]])
    season.tmax <- subsetGrid(tmax.MM, season = seasons[[season]])
    season.tmin <- subsetGrid(tmin.MM, season = seasons[[season]])
    pr.list[[season]] <- aggregateGrid(season.pr, aggr.y = list(FUN = "mean", na.rm = FALSE))
    tmax.list[[season]] <- aggregateGrid(season.tmax, aggr.y = list(FUN = "mean", na.rm = FALSE))
    tmin.list[[season]] <- aggregateGrid(season.tmin, aggr.y = list(FUN = "mean", na.rm = FALSE))

}

source("/oceano/gmeteo/users/fuentesm/AEMET/fun.aux.trends.R")
winter <- pr.list$Winter
spring <- pr.list$Spring
summer <- pr.list$Summer
autumn <- pr.list$Autumn

min = -10
max = 10
step = 1

trend.winter = climatology(winter, clim.fun = list(FUN = "computeTrend"))
trend.winter = gridArithmetics(trend.winter, 10, operator = "*")
sig.winter = map.stippling(clim = climatology(winter, clim.fun = list(FUN = "computeSigTrend")), 
                    threshold = 0.05, condition = "GT", 
                    pch = 19, cex = .25, col = "white")  # points exhibiting significant trends (at a 95% confidence level)

plot.winter <- spatialPlot(trend.winter, backdrop.theme = "coastline", color.theme = "RdBu", at = seq(min, max, by = step),
                set.max = max, set.min = min, rev.colors = FALSE, main = "Invierno", sp.layout = list(sig.winter),
                colorkey=list(space = "bottom", title = list("mm/mes / 10 años", cex = 1)))


trend.spring = climatology(spring, clim.fun = list(FUN = "computeTrend"))
trend.spring=gridArithmetics(trend.spring, 10, operator="*")
sig.spring = map.stippling(clim = climatology(spring, clim.fun = list(FUN = "computeSigTrend")), 
                    threshold = 0.05, condition = "GT", 
                    pch = 19, cex = .25, col = "white")  # points exhibiting significant trends (at a 95% confidence level)

plot.spring <- spatialPlot(trend.spring, backdrop.theme = "coastline", color.theme = "RdBu", at = seq(min, max, by = step),
                set.max = max, set.min = min,  rev.colors = FALSE, main = "Primavera", sp.layout = list(sig.spring),
                 colorkey=list(space = "bottom", title = list("mm/mes / 10 años", cex = 1)))

trend.summer = climatology(summer, clim.fun = list(FUN = "computeTrend"))
trend.summer = gridArithmetics(trend.summer, 10, operator="*")
sig.summer = map.stippling(clim = climatology(summer, clim.fun = list(FUN = "computeSigTrend")), 
                    threshold = 0.05, condition = "GT", 
                    pch = 19, cex = .25, col = "white")  # points exhibiting significant trends (at a 95% confidence level)
plot.summer <- spatialPlot(trend.summer, backdrop.theme = "coastline", color.theme = "RdBu", at = seq(min, max, by = step),
                    set.max = max, set.min = min, rev.colors = FALSE, main = "Verano", sp.layout = list(sig.summer),
                    colorkey=list(space = "bottom", title = list("mm/mes / 10 años", cex = 1)))

trend.autumn = climatology(autumn, clim.fun = list(FUN = "computeTrend"))
trend.autumn = gridArithmetics(trend.autumn, 10, operator="*")
sig.autumn = map.stippling(clim = climatology(autumn, clim.fun = list(FUN = "computeSigTrend")), 
                    threshold = 0.05, condition = "GT", 
                    pch = 19, cex = .25, col = "white")  # points exhibiting significant trends (at a 95% confidence level)
plot.autumn <- spatialPlot(trend.autumn, backdrop.theme = "coastline", color.theme = "RdBu", at = seq(min, max, by = step),
                    set.max = max, set.min = min, rev.colors = FALSE, main = "Otoño", sp.layout = list(sig.autumn),
                    colorkey=list(space = "bottom", title = list("mm/mes / 10 años", cex = 1)))

plot.winter.pr <- plot.winter
plot.spring.pr <- plot.spring
plot.summer.pr <- plot.summer
plot.autumn.pr <- plot.autumn

### TMAX

winter <- tmax.list$Winter
spring <- tmax.list$Spring
summer <- tmax.list$Summer
autumn <- tmax.list$Autumn

max=1
min=-1
step=0.1

trend.winter = climatology(winter, clim.fun = list(FUN = "computeTrend"))
trend.winter = gridArithmetics(trend.winter, 10, operator = "*")
sig.winter = map.stippling(clim = climatology(winter, clim.fun = list(FUN = "computeSigTrend")), 
                    threshold = 0.05, condition = "GT", 
                    pch = 19, cex = .25, col = "white")  # points exhibiting significant trends (at a 95% confidence level)

plot.winter <- spatialPlot(trend.winter, backdrop.theme = "coastline", color.theme = "RdBu", at = seq(min, max, by = step),
                set.max = max, set.min = min, rev.colors = TRUE, main = "Invierno", sp.layout = list(sig.winter),
                colorkey=list(space = "bottom", title = list("ºC / 10 años", cex = 1)))


trend.spring = climatology(spring, clim.fun = list(FUN = "computeTrend"))
trend.spring=gridArithmetics(trend.spring, 10, operator="*")
sig.spring = map.stippling(clim = climatology(spring, clim.fun = list(FUN = "computeSigTrend")), 
                    threshold = 0.05, condition = "GT", 
                    pch = 19, cex = .25, col = "white")  # points exhibiting significant trends (at a 95% confidence level)

plot.spring <- spatialPlot(trend.spring, backdrop.theme = "coastline", color.theme = "RdBu",  at = seq(min, max, by = step),
                 set.max = max, set.min = min, rev.colors = TRUE, main = "Primavera", sp.layout = list(sig.spring),
                 colorkey=list(space = "bottom", title = list("ºC / 10 años", cex = 1)))

trend.summer = climatology(summer, clim.fun = list(FUN = "computeTrend"))
trend.summer = gridArithmetics(trend.summer, 10, operator="*")
sig.summer = map.stippling(clim = climatology(summer, clim.fun = list(FUN = "computeSigTrend")), 
                    threshold = 0.05, condition = "GT", 
                    pch = 19, cex = .25, col = "white")  # points exhibiting significant trends (at a 95% confidence level)
plot.summer <- spatialPlot(trend.summer, backdrop.theme = "coastline", color.theme = "RdBu",  at = seq(min, max, by = step),
                    set.max = max, set.min = min, rev.colors = TRUE, main = "Verano", sp.layout = list(sig.summer),
                    colorkey=list(space = "bottom", title = list("ºC / 10 años", cex = 1)))

trend.autumn = climatology(autumn, clim.fun = list(FUN = "computeTrend"))
trend.autumn = gridArithmetics(trend.autumn, 10, operator="*")
sig.autumn = map.stippling(clim = climatology(autumn, clim.fun = list(FUN = "computeSigTrend")), 
                    threshold = 0.05, condition = "GT", 
                    pch = 19, cex = .25, col = "white")  # points exhibiting significant trends (at a 95% confidence level)
plot.autumn <- spatialPlot(trend.autumn, backdrop.theme = "coastline", color.theme = "RdBu", at = seq(min, max, by = step),
                    set.max = max, set.min = min, rev.colors = TRUE, main = "Otoño", sp.layout = list(sig.autumn),
                    colorkey=list(space = "bottom", title = list("ºC / 10 años", cex = 1)))

plot.autumn.tmax <- plot.autumn
plot.spring.tmax <- plot.spring
plot.summer.tmax <- plot.summer
plot.winter.tmax <- plot.winter



### min

winter <- tmin.list$Winter
spring <- tmin.list$Spring
summer <- tmin.list$Summer
autumn <- tmin.list$Autumn

max=1
min=-1
step=0.1

trend.winter = climatology(winter, clim.fun = list(FUN = "computeTrend"))
trend.winter = gridArithmetics(trend.winter, 10, operator = "*")
sig.winter = map.stippling(clim = climatology(winter, clim.fun = list(FUN = "computeSigTrend")), 
                    threshold = 0.05, condition = "GT", 
                    pch = 19, cex = .25, col = "white")  # points exhibiting significant trends (at a 95% confidence level)

plot.winter <- spatialPlot(trend.winter, backdrop.theme = "coastline", color.theme = "RdBu", at = seq(min, max, by = step),
                set.max = max, set.min = min, rev.colors = TRUE, main = "Invierno", sp.layout = list(sig.winter),
                colorkey=list(space = "bottom", title = list("ºC / 10 años", cex = 1)))


trend.spring = climatology(spring, clim.fun = list(FUN = "computeTrend"))
trend.spring=gridArithmetics(trend.spring, 10, operator="*")
sig.spring = map.stippling(clim = climatology(spring, clim.fun = list(FUN = "computeSigTrend")), 
                    threshold = 0.05, condition = "GT", 
                    pch = 19, cex = .25, col = "white")  # points exhibiting significant trends (at a 95% confidence level)

plot.spring <- spatialPlot(trend.spring, backdrop.theme = "coastline", color.theme = "RdBu",  at = seq(min, max, by = step),
                 set.max = max, set.min = min, rev.colors = TRUE, main = "Primavera", sp.layout = list(sig.spring),
                 colorkey=list(space = "bottom", title = list("ºC / 10 años", cex = 1)))

trend.summer = climatology(summer, clim.fun = list(FUN = "computeTrend"))
trend.summer = gridArithmetics(trend.summer, 10, operator="*")
sig.summer = map.stippling(clim = climatology(summer, clim.fun = list(FUN = "computeSigTrend")), 
                    threshold = 0.05, condition = "GT", 
                    pch = 19, cex = .25, col = "white")  # points exhibiting significant trends (at a 95% confidence level)
plot.summer <- spatialPlot(trend.summer, backdrop.theme = "coastline", color.theme = "RdBu",  at = seq(min, max, by = step),
                    set.max = max, set.min = min, rev.colors = TRUE, main = "Verano", sp.layout = list(sig.summer),
                    colorkey=list(space = "bottom", title = list("ºC / 10 años", cex = 1)))

trend.autumn = climatology(autumn, clim.fun = list(FUN = "computeTrend"))
trend.autumn = gridArithmetics(trend.autumn, 10, operator="*")
sig.autumn = map.stippling(clim = climatology(autumn, clim.fun = list(FUN = "computeSigTrend")), 
                    threshold = 0.05, condition = "GT", 
                    pch = 19, cex = .25, col = "white")  # points exhibiting significant trends (at a 95% confidence level)
plot.autumn <- spatialPlot(trend.autumn, backdrop.theme = "coastline", color.theme = "RdBu", at = seq(min, max, by = step),
                    set.max = max, set.min = min, rev.colors = TRUE, main = "Otoño", sp.layout = list(sig.autumn),
                    colorkey=list(space = "bottom", title = list("ºC / 10 años", cex = 1)))

plot.winter.tmin <- plot.winter
plot.spring.tmin <- plot.spring
plot.summer.tmin <- plot.summer
plot.autumn.tmin <- plot.autumn



library(gridExtra)
library(grid)

pdf("trends.variables.esenciales.pdf", width=16, height=12)

# Crear los títulos para cada fila
title_precipitation <- textGrob("Tendencia de precipitación acumulada por estación (1971-2020)", gp=gpar(fontsize=15, fontface="bold"), just="center")
title_tmax <- textGrob("Tendencia de temperatura máxima promedio por estación (1971-2020)", gp=gpar(fontsize=15, fontface="bold"), just="center")
title_tmin <- textGrob("Tendencia de temperatura mínima promedio por estación (1971-2020)", gp=gpar(fontsize=15, fontface="bold"), just="center")

# Crear la disposición de gráficos con títulos
grid.arrange(
  arrangeGrob(title_precipitation, ncol = 1),
  arrangeGrob(plot.winter.pr, plot.spring.pr, plot.summer.pr, plot.autumn.pr, ncol = 4),
  arrangeGrob(title_tmax, ncol = 1),
  arrangeGrob(plot.winter.tmax, plot.spring.tmax, plot.summer.tmax, plot.autumn.tmax, ncol = 4),
  arrangeGrob(title_tmin, ncol = 1),
  arrangeGrob(plot.winter.tmin, plot.spring.tmin, plot.summer.tmin, plot.autumn.tmin, ncol = 4),
  ncol = 1,
  heights = c(0.5, 3, 0.5, 3, 0.5, 3) # Asegúrate de que las alturas coincidan con el número de filas
)

# Cerrar el dispositivo gráfico
dev.off()
