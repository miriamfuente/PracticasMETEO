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

seasons <- list(Winter = c(1,2,12), Spring = c(3, 4, 5), Summer = c(6, 7, 8), Autumn = c(9, 10, 11))

pr.list <- list()
tmax.list <- list()
tmin.list <- list()

for (season in names(seasons)) {
    pr.list[[season]] <- subsetGrid(pr, season = seasons[[season]])
    tmax.list[[season]] <- subsetGrid(tmax, season = seasons[[season]])
    tmin.list[[season]] <- subsetGrid(tmin, season = seasons[[season]])
}


precip <- spatialPlot(makeMultiGrid(climatology(pr.list$Winter),climatology(pr.list$Spring),climatology(pr.list$Summer),climatology(pr.list$Autumn), skip.temporal.check=TRUE),
            backdrop.theme = "coastline", as.table=TRUE, names.attr=c("Invierno", "Primavera", "Verano", "Otoño"),
            color.theme="Blues", rev.colors=FALSE, main="Precipitación por estación (1971-2020)",
            layout=c(4,1),
            colorkey = list(space = "right",
                title = list("mm/día", cex = 1)))

temp.max <- spatialPlot(makeMultiGrid(climatology(tmax.list$Winter),climatology(tmax.list$Spring),climatology(tmax.list$Summer),climatology(tmax.list$Autumn), skip.temporal.check=TRUE),
            backdrop.theme = "coastline", as.table=TRUE, names.attr=c("Invierno", "Primavera", "Verano", "Otoño"),
            color.theme="Reds", rev.colors=FALSE, main="Temperatura máxima por estación (1971-2020)",
            layout=c(4,1),
            colorkey = list(space = "right",
                title = list("ºC", cex = 1)))

temp.min <- spatialPlot(makeMultiGrid(climatology(tmin.list$Winter),climatology(tmin.list$Spring),climatology(tmin.list$Summer),climatology(tmin.list$Autumn), skip.temporal.check=TRUE),
            backdrop.theme = "coastline", as.table=TRUE, names.attr=c("Invierno", "Primavera", "Verano", "Otoño"),
            color.theme="Reds", rev.colors=FALSE, main="Temperatura mínima por estación (1971-2020)",
            layout=c(4,1),
            colorkey = list(space = "right",
                title = list("ºC", cex = 1)))   


library(gridExtra)
pdf("variables.esenciales.pdf", width = 16, height = 10)
# Arrange the plots in a grid and add the main title
grid.arrange(
    precip, temp.max, temp.min,
    ncol = 1)

dev.off()

# Tendencias
# Compute the trends

# ##############################
# # TRENDS AUXILIARY FUNCTIONS #   NOT REALLY
# ##############################
# source("/oceano/gmeteo/users/fuentesm/AEMET/fun.aux.trends.R")
# winter <- pr.list$Winter
# spring <- pr.list$Spring
# summer <- pr.list$Summer
# autumn <- pr.list$Autumn


# trend.winter = climatology(winter, clim.fun = list(FUN = "computeTrend"))
# trend.winter = gridArithmetics(trend.winter, 10, operator = "*")
# sig.winter = map.stippling(clim = climatology(winter, clim.fun = list(FUN = "computeSigTrend")), 
#                     threshold = 0.05, condition = "GT", 
#                     pch = 19, cex = .25, col = "white")  # points exhibiting significant trends (at a 95% confidence level)

# plot.winter <- spatialPlot(trend.winter, backdrop.theme = "coastline", color.theme = "RdBu", at = seq(-0.01, 0.01, by = 0.001), 
#                 set.max = 0.01, set.min = -0.01, rev.colors = FALSE, main = "Invierno", sp.layout = list(sig.winter),
#                 colorkey=list(space = "bottom", title = list("mm/día / 10 años", cex = 1)))

# trend.spring = climatology(spring, clim.fun = list(FUN = "computeTrend"))
# trend.spring=gridArithmetics(trend.spring, 10, operator="*")
# sig.spring = map.stippling(clim = climatology(spring, clim.fun = list(FUN = "computeSigTrend")), 
#                     threshold = 0.05, condition = "GT", 
#                     pch = 19, cex = .25, col = "white")  # points exhibiting significant trends (at a 95% confidence level)

# plot.spring <- spatialPlot(trend.spring, backdrop.theme = "coastline", color.theme = "RdBu", at = seq(-0.01, 0.01, by = 0.001),
#                  set.max = 0.01, set.min = -0.01, rev.colors = FALSE, main = "Primavera", sp.layout = list(sig.spring),
#                  colorkey=list(space = "bottom", title = list("mm/día / 10 años", cex = 1)))

# trend.summer = climatology(summer, clim.fun = list(FUN = "computeTrend"))
# trend.summer = gridArithmetics(trend.summer, 10, operator="*")
# sig.summer = map.stippling(clim = climatology(summer, clim.fun = list(FUN = "computeSigTrend")), 
#                     threshold = 0.05, condition = "GT", 
#                     pch = 19, cex = .25, col = "white")  # points exhibiting significant trends (at a 95% confidence level)
# plot.summer <- spatialPlot(trend.summer, backdrop.theme = "coastline", color.theme = "RdBu", at = seq(-0.01, 0.01, by = 0.001),
#                     set.max = 0.01, set.min = -0.01, rev.colors = FALSE, main = "Verano", sp.layout = list(sig.summer),
#                     colorkey=list(space = "bottom", title = list("mm/día / 10 años", cex = 1)))

# trend.autumn = climatology(autumn, clim.fun = list(FUN = "computeTrend"))
# trend.autumn = gridArithmetics(trend.autumn, 10, operator="*")
# sig.autumn = map.stippling(clim = climatology(autumn, clim.fun = list(FUN = "computeSigTrend")), 
#                     threshold = 0.05, condition = "GT", 
#                     pch = 19, cex = .25, col = "white")  # points exhibiting significant trends (at a 95% confidence level)
# plot.autumn <- spatialPlot(trend.autumn, backdrop.theme = "coastline", color.theme = "RdBu", at = seq(-0.01, 0.01, by = 0.001),
#                     set.max = 0.01, set.min = -0.01, rev.colors = FALSE, main = "Otoño", sp.layout = list(sig.autumn),
#                     colorkey=list(space = "bottom", title = list("mm/día / 10 años", cex = 1)))

# plot.winter.pr <- plot.winter
# plot.spring.pr <- plot.spring
# plot.summer.pr <- plot.summer
# plot.autumn.pr <- plot.autumn


# library(grid)
# library(gridExtra)
# # Create the PDF file
# pdf("prueba.trends.pdf")
# grid.arrange(plot.winter, plot.spring, plot.summer, plot.autumn)
# dev.off()

# pdf("tendencias.pdf", width=16, height=12)

# grid.arrange(plot.winter.pr, plot.spring.pr, plot.summer.pr, plot.autumn.pr, 
#              plot.winter.tmax, plot.spring.tmax, plot.summer.tmax, plot.autumn.tmax,
#              plot.winter.tmin, plot.spring.tmin, plot.summer.tmin, plot.autumn.tmin,
#              ncol = 4)
# dev.off()

# library(grid)
# library(gridExtra)

# # Crear el PDF
# pdf("trends.variables.esenciales.pdf", width=16, height=12)

# # Crear los títulos para cada fila
# title_precipitation <- textGrob("Tendencia de precipitación por estación (1971-2020)", gp=gpar(fontsize=15, fontface="bold"), just="center")
# title_tmax <- textGrob("Tendencia de temperatura máxima por estación (1971-2020)", gp=gpar(fontsize=15, fontface="bold"), just="center")
# title_tmin <- textGrob("Tendencia de temperatura mínima por estación (1971-2020)", gp=gpar(fontsize=15, fontface="bold"), just="center")

# # Crear la disposición de gráficos con títulos
# grid.arrange(
#   arrangeGrob(title_precipitation, ncol = 1),
#   arrangeGrob(plot.winter.pr, plot.spring.pr, plot.summer.pr, plot.autumn.pr, ncol = 4),
#   arrangeGrob(title_tmax, ncol = 1),
#   arrangeGrob(plot.winter.tmax, plot.spring.tmax, plot.summer.tmax, plot.autumn.tmax, ncol = 4),
#   arrangeGrob(title_tmin, ncol = 1),
#   arrangeGrob(plot.winter.tmin, plot.spring.tmin, plot.summer.tmin, plot.autumn.tmin, ncol = 4),
#   ncol = 1,
#   heights = c(0.5, 3, 0.5, 3, 0.5, 3) # Asegúrate de que las alturas coincidan con el número de filas
# )

# # Cerrar el dispositivo gráfico
# dev.off()

###############################
#### Bias Correction ##########
###############################
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


# pr.obs <- readRDS("/lustre/gmeteo/WORK/fuentesm/TFM/section4.2/pr.obs.DD.1976-2005.rds")
# pr.rcm <- readRDS("/lustre/gmeteo/WORK/fuentesm/TFM/section4.2/pr.rcm.raw.DD.1976-2005.rds")
# tmax.obs <- readRDS("/lustre/gmeteo/WORK/fuentesm/TFM/section4.2/tasmax.obs.DD.1976-2005.rds")
# tmax.rcm <- readRDS("/lustre/gmeteo/WORK/fuentesm/TFM/section4.2/tasmax.rcm.raw.DD.1976-2005.rds")
tmin.obs <- readRDS("/lustre/gmeteo/WORK/fuentesm/TFM/section4.2/ventana.movil/tmin/tasmin.obs.DD.1976-2005.rds")
tmin.rcm <- readRDS("/lustre/gmeteo/WORK/fuentesm/TFM/section4.2/ventana.movil/tmin/tasmin.rcm.raw.DD.1976-2005.rds")



subset_meses <- function(obs, rcm, mes, metodo, precipitation, bc) {
  meses_relacion <- meses(mes, metodo)
  # mes.rcm <- subsetGrid(rcm, season = mes)
  meses.rcm <- subsetGrid(rcm, season = meses_relacion)
  meses.obs <- subsetGrid(obs, season = meses_relacion)
  if (bc =="eqm"){
    bc1 <- biasCorrection(y=meses.obs, x=meses.rcm, newdata=meses.rcm, precipitation=precipitation,
                        method="eqm", extrapolation="constant", n.quantiles=99, cross.val="kfold", folds=list(fold1, fold2))
  }else if (bc == "scaling"){
    if(precipitation ==TRUE){
      bc1 <- biasCorrection(y=meses.obs, x=meses.rcm, newdata=meses.rcm, precipitation=precipitation,
                        method="scaling", scaling.type="multiplicative", cross.val="kfold", folds=list(fold1, fold2))
    }else if(precipitation ==FALSE){
      bc1 <- biasCorrection(y=meses.obs, x=meses.rcm, newdata=meses.rcm, precipitation=precipitation,
                        method="scaling", scaling.type="additive", cross.val="kfold", folds=list(fold1, fold2))
    }
  }
  bc.mes <- subsetGrid(bc1, season = mes)
  bc.mes$Dates$start <- as.character(bc.mes$Dates$start)
  bc.mes$Dates$end <- as.character(bc.mes$Dates$end)
  print(mes)
  return(bc.mes)
}

# pr_eqm <- list()
# tmin_1 <- list()
tmin_eqm <- list()

nombre_meses <- c("enero", "febrero", "marzo", "abril", "mayo", "junio", "julio", "agosto", "septiembre", "octubre", "noviembre", "diciembre")

for (mes in 1:12) {
  tmin_eqm[[nombre_meses[mes]]] <- subset_meses(tmin.obs, tmin.rcm, mes, 2, FALSE, "eqm")
#   tmax_1[[nombre_meses[mes]]] <- subset_meses(tmax.obs, tmax.rcm, mes, 2, FALSE)
#   tmin_1[[nombre_meses[mes]]] <- subset_meses(tmin.obs, tmin.rcm, mes, 2, FALSE)
}

tmin_eqm <- bindGrid(tmin_eqm$enero, tmin_eqm$febrero, tmin_eqm$marzo, tmin_eqm$abril, tmin_eqm$mayo, tmin_eqm$junio, 
                  tmin_eqm$julio, tmin_eqm$agosto, tmin_eqm$septiembre, tmin_eqm$octubre, tmin_eqm$noviembre, 
                  tmin_eqm$diciembre, dimension = "time")

saveRDS(tmin_eqm, "tmin.rcm.eqm.DD.1976-2005.rds")

# Pruebas
meses5 <- meses(5,2)
mayo.obs <- subsetGrid(pr.obs, season = meses5)
mayo.rcm <- subsetGrid(pr.rcm, season = meses5)
bc.mayo <- biasCorrection(y=mayo.obs, x=mayo.rcm, newdata=mayo.rcm, precipitation=TRUE,
                        method="eqm", extrapolation="constant", n.quantiles=99, cross.val="kfold", folds=list(fold1, fold2))

############################################
# Precipitation
pr.obs <- readRDS("/lustre/gmeteo/WORK/fuentesm/TFM/section4.2/ventana.movil/pr/pr.obs.DD.1976-2005.rds")
pr.rcm <- readRDS("/lustre/gmeteo/WORK/fuentesm/TFM/section4.2/ventana.movil/pr/pr.rcm.raw.DD.1976-2005.rds")
pr.sc <- readRDS("/lustre/gmeteo/WORK/fuentesm/TFM/section4.2/ventana.movil/pr/pr.rcm.scaling.DD.1976-2005.rds")
pr.eqm <- readRDS("/lustre/gmeteo/WORK/fuentesm/TFM/section4.2/ventana.movil/pr/pr.rcm.eqm.DD.1976-2005.rds")
 

pr.sc <- bindGrid(pr.sc$enero, pr.sc$febrero, pr.sc$marzo, pr.sc$abril, pr.sc$mayo, pr.sc$junio, 
                  pr.sc$julio, pr.sc$agosto, pr.sc$septiembre, pr.sc$octubre, pr.sc$noviembre, 
                  pr.sc$diciembre, dimension = "time")

pr.eqm <- bindGrid(pr.eqm$enero, pr.eqm$febrero, pr.eqm$marzo, pr.eqm$abril, pr.eqm$mayo, pr.eqm$junio, 
                  pr.eqm$julio, pr.eqm$agosto, pr.eqm$septiembre, pr.eqm$octubre, pr.eqm$noviembre, 
                  pr.eqm$diciembre, dimension = "time")

pr.sesgo.rcm <- gridArithmetics(climatology(pr.rcm), climatology(pr.obs), operator = "-")
pr.sesgo.sc <- gridArithmetics(climatology(pr.sc), climatology(pr.obs), operator = "-")
pr.sesgo.eqm <- gridArithmetics(climatology(pr.eqm), climatology(pr.obs), operator = "-")

pr.simulado <- spatialPlot(makeMultiGrid(climatology(pr.obs), climatology(pr.rcm), climatology(pr.sc), climatology(pr.eqm)),
            backdrop.theme = "coastline", as.table=TRUE, names.attr=c("Observado", "RCM", "RCM (Scaling)", "RCM (EQM)"),
            color.theme="RdBu", rev.colors=TRUE, main="Simulaciones de precipitación (1976-2005)",
            layout=c(4,1),
            colorkey = list(space = "right",
                title = list("mm/día", cex = 1)))

pr.sesgo <- spatialPlot(makeMultiGrid(pr.sesgo.rcm, pr.sesgo.sc, pr.sesgo.eqm),
            backdrop.theme = "coastline", as.table=TRUE, names.attr=c("Histórico - OBS", "Histórico (Scaling) - OBS", "Histórico (EQM) - OBS"),
            color.theme="RdBu", rev.colors=FALSE, main="Sesgo de precipitación (1976-2005)",
            layout=c(3,1), set.max = 3, set.min = -3, at = seq(-3, 3, by = 0.5),
            colorkey = list(space = "right",
                title = list("mm/día", cex = 1)))

pr.obs <- spatialPlot(climatology(pr.obs), backdrop.theme = "coastline", color.theme = "Blues", main = "Precipitación observada (1976-2005)",
            colorkey = list(space = "right", title = list("mm/día", cex = 1)))

pr.sesgo.obs <- spatialPlot(makeMultiGrid(climatology(pr.obs),pr.sesgo.rcm, pr.sesgo.sc, pr.sesgo.eqm),
            backdrop.theme = "coastline", as.table=TRUE, names.attr=c("Observado","Sesgo RCM", "Sesgo RCM (Scaling)", "Sesgo RCM (EQM)"),
            color.theme="RdBu", rev.colors=FALSE, main="Precipitación (1976-2005)",
            layout=c(4,1),
            colorkey = list(space = "right",
                title = list("mm/día", cex = 1)))

# TMAX
tmax.obs <- readRDS("/lustre/gmeteo/WORK/fuentesm/TFM/section4.2/ventana.movil/tmax/tasmax.obs.DD.1976-2005.rds")
tmax.rcm <- readRDS("/lustre/gmeteo/WORK/fuentesm/TFM/section4.2/ventana.movil/tmax/tasmax.rcm.raw.DD.1976-2005.rds")
tmax.sc <- readRDS("/lustre/gmeteo/WORK/fuentesm/TFM/section4.2/ventana.movil/tmax/tmax.rcm.scaling.DD.1976-2005.rds")
tmax.eqm <- readRDS("/lustre/gmeteo/WORK/fuentesm/TFM/section4.2/ventana.movil/tmax/tmax.rcm.eqm.DD.1976-2005.rds")

tmax.sc <- bindGrid(tmax.sc$enero, tmax.sc$febrero, tmax.sc$marzo, tmax.sc$abril, tmax.sc$mayo, tmax.sc$junio, 
                  tmax.sc$julio, tmax.sc$agosto, tmax.sc$septiembre, tmax.sc$octubre, tmax.sc$noviembre, 
                  tmax.sc$diciembre, dimension = "time")

tmax.eqm <- bindGrid(tmax.eqm$enero, tmax.eqm$febrero, tmax.eqm$marzo, tmax.eqm$abril, tmax.eqm$mayo, tmax.eqm$junio,
                  tmax.eqm$julio, tmax.eqm$agosto, tmax.eqm$septiembre, tmax.eqm$octubre, tmax.eqm$noviembre,
                  tmax.eqm$diciembre, dimension = "time")

tmax.sesgo.rcm <- gridArithmetics(climatology(tmax.rcm), climatology(tmax.obs), operator = "-")
tmax.sesgo.sc <- gridArithmetics(climatology(tmax.sc), climatology(tmax.obs), operator = "-")
tmax.sesgo.eqm <- gridArithmetics(climatology(tmax.eqm), climatology(tmax.obs), operator = "-")

tmax.simulado <- spatialPlot(makeMultiGrid(climatology(tmax.obs), climatology(tmax.rcm), climatology(tmax.sc), climatology(tmax.eqm)),
            backdrop.theme = "coastline", as.table=TRUE, names.attr=c("Observado", "RCM", "RCM (Scaling)", "RCM (EQM)"),
            color.theme="Reds", rev.colors=FALSE, main="Simulaciones de temperatura máxima (1976-2005)",
            layout=c(4,1),
            colorkey = list(space = "right",
                title = list("ºC", cex = 1)))

tmax.sesgo <- spatialPlot(makeMultiGrid(tmax.sesgo.rcm, tmax.sesgo.sc, tmax.sesgo.eqm),
            backdrop.theme = "coastline", as.table=TRUE, names.attr=c("Histórico - OBS", "Histórico (Scaling) - OBS", "Histórico (EQM) - OBS"),
            color.theme="RdBu", rev.colors=TRUE, main="Sesgo de temperatura máxima (1976-2005)",
            layout=c(3,1), set.max = 5, set.min = -5, at = seq(-5, 5, by = 0.5),
            colorkey = list(space = "right",
                title = list("ºC", cex = 1)))

tmax.obs <- spatialPlot(climatology(tmax.obs), backdrop.theme = "coastline", color.theme = "Reds", main = "Temperatura máxima observada (1976-2005)",
            colorkey = list(space = "right", title = list("ºC", cex = 1)))

tmax.sesgo.obs <- spatialPlot(makeMultiGrid(climatology(tmax.obs),tmax.sesgo.rcm, tmax.sesgo.sc, tmax.sesgo.eqm),
            backdrop.theme = "coastline", as.table=TRUE, names.attr=c("Observado","Sesgo RCM", "Sesgo RCM (Scaling)", "Sesgo RCM (EQM)"),
            color.theme="YlOrRd", rev.colors=FALSE, main="Temperatura máxima (1976-2005)",
            layout=c(4,1),
            colorkey = list(space = "right",
                title = list("ºC", cex = 1)))


# TMIN
tmin.obs <- readRDS("/lustre/gmeteo/WORK/fuentesm/TFM/section4.2/ventana.movil/tmin/tasmin.obs.DD.1976-2005.rds")
tmin.rcm <- readRDS("/lustre/gmeteo/WORK/fuentesm/TFM/section4.2/ventana.movil/tmin/tasmin.rcm.raw.DD.1976-2005.rds")
tmin.sc <- readRDS("/lustre/gmeteo/WORK/fuentesm/TFM/section4.2/ventana.movil/tmin/tmin.rcm.scaling.DD.1976-2005.rds")
tmin.eqm <- readRDS("/lustre/gmeteo/WORK/fuentesm/TFM/section4.2/ventana.movil/tmin/tmin.rcm.eqm.DD.1976-2005.rds")

tmin.sc <- bindGrid(tmin.sc$enero, tmin.sc$febrero, tmin.sc$marzo, tmin.sc$abril, tmin.sc$mayo, tmin.sc$junio, 
                  tmin.sc$julio, tmin.sc$agosto, tmin.sc$septiembre, tmin.sc$octubre, tmin.sc$noviembre, 
                  tmin.sc$diciembre, dimension = "time")

tmin.eqm <- bindGrid(tmin.eqm$enero, tmin.eqm$febrero, tmin.eqm$marzo, tmin.eqm$abril, tmin.eqm$mayo, tmin.eqm$junio,
                  tmin.eqm$julio, tmin.eqm$agosto, tmin.eqm$septiembre, tmin.eqm$octubre, tmin.eqm$noviembre,
                  tmin.eqm$diciembre, dimension = "time")

tmin.sesgo.rcm <- gridArithmetics(climatology(tmin.rcm), climatology(tmin.obs), operator = "-")
tmin.sesgo.sc <- gridArithmetics(climatology(tmin.sc), climatology(tmin.obs), operator = "-")
tmin.sesgo.eqm <- gridArithmetics(climatology(tmin.eqm), climatology(tmin.obs), operator = "-")

tmin.simulado <- spatialPlot(makeMultiGrid(climatology(tmin.obs), climatology(tmin.rcm), climatology(tmin.sc), climatology(tmin.eqm)),
            backdrop.theme = "coastline", as.table=TRUE, names.attr=c("Observado", "RCM", "RCM (Scaling)", "RCM (EQM)"),
            color.theme="Reds", rev.colors=FALSE, main="Simulaciones de temperatura mínima (1976-2005)",
            layout=c(4,1),
            colorkey = list(space = "right",
                title = list("ºC", cex = 1)))

tmin.sesgo <- spatialPlot(makeMultiGrid(tmin.sesgo.rcm, tmin.sesgo.sc, tmin.sesgo.eqm),
            backdrop.theme = "coastline", as.table=TRUE, names.attr=c("Histórico - OBS", "Histórico (Scaling) - OBS", "Histórico (EQM) - OBS"),
            color.theme="RdBu", rev.colors=TRUE, main="Sesgo de temperatura mínima (1976-2005)",
            layout=c(3,1), set.max = 5, set.min = -5, at = seq(-5, 5, by = 0.5),
            colorkey = list(space = "right",
                title = list("ºC", cex = 1)))

tmin.obs <- spatialPlot(climatology(tmin.obs), backdrop.theme = "coastline", color.theme = "Reds", main = "Temperatura mínima observada (1976-2005)",
            colorkey = list(space = "right", title = list("ºC", cex = 1)))

tmin.sesgo.obs <- spatialPlot(makeMultiGrid(climatology(tmin.obs),tmin.sesgo.rcm, tmin.sesgo.sc, tmin.sesgo.eqm),
            backdrop.theme = "coastline", as.table=TRUE, names.attr=c("Observado","Sesgo RCM", "Sesgo RCM (Scaling)", "Sesgo RCM (EQM)"),
            color.theme="YlOrRd", rev.colors=FALSE, main="Temperatura mínima (1976-2005)",
            layout=c(4,1),
            colorkey = list(space = "right",
                title = list("ºC", cex = 1)))


library(grid)
library(gridExtra)


# Configuración de los grobs, asumiendo que ya los has creado como pr.obs, pr.sesgo, etc.

# Ajuste del tamaño de los grobs
obs_width <- unit(1, "null")
sesgo_width <- unit(2, "null")  # Ajusta este valor según necesites para equilibrar el tamaño

pdf("var.esen.hist.sesgo.pdf", width=20, height=10)

grid.arrange(
  arrangeGrob(pr.obs, pr.sesgo, ncol = 2, widths = c(obs_width, sesgo_width)),
  arrangeGrob(tmax.obs, tmax.sesgo, ncol = 2, widths = c(obs_width, sesgo_width)),
  arrangeGrob(tmin.obs, tmin.sesgo, ncol = 2, widths = c(obs_width, sesgo_width)),
  ncol = 1
)

dev.off()



# Crear el PDF
pdf("var.esen.simulado.pdf", width=16, height=10)
# Crear la disposición de gráficos con títulos
grid.arrange(
  arrangeGrob(pr.simulado, ncol = 1),
  arrangeGrob(tmax.simulado, ncol = 1),
  arrangeGrob(tmin.simulado, ncol = 1),
  ncol = 1,
  heights = c(3, 3, 3) )# Asegúrate de que las alturas coincidan con el número de filas
dev.off()

pdf("var.esen.sesgo.pdf", width=16, height=10)
grid.arrange(
  arrangeGrob(pr.sesgo, ncol = 1),
  arrangeGrob(tmax.sesgo, ncol = 1),
  arrangeGrob(tmin.sesgo, ncol = 1),
  ncol = 1,
  heights = c(3, 3, 3) )# Asegúrate de que las alturas coincidan con el número de filas
dev.off()

pdf("var.esen.sesgo.obs.pdf", width=16, height=10)
grid.arrange(
  arrangeGrob(pr.sesgo.obs, ncol = 1),
  arrangeGrob(tmax.sesgo.obs, ncol = 1),
  arrangeGrob(tmin.sesgo.obs, ncol = 1),
  ncol = 1,
  heights = c(3, 3, 3) )# Asegúrate de que las alturas coincidan con el número de filas
dev.off()

############################################
######### FUTURO ###########################
############################################
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


pr.obs <- readRDS("/lustre/gmeteo/WORK/fuentesm/TFM/section4.2/ventana.movil/pr/pr.obs.DD.1976-2005.rds")
pr.rcm <- readRDS("/lustre/gmeteo/WORK/fuentesm/TFM/section4.2/ventana.movil/pr/pr.rcm.raw.DD.1976-2005.rds")
pr.rcp1 <- readRDS("/lustre/gmeteo/WORK/fuentesm/TFM/section4.2/ventana.movil/pr/pr.rcp85.raw.DD.2041-2070.rds")
pr.rcp2 <- readRDS("/lustre/gmeteo/WORK/fuentesm/TFM/section4.2/ventana.movil/pr/pr.rcp85.raw.DD.2071-2100.rds")
# tmax.obs <- readRDS("/lustre/gmeteo/WORK/fuentesm/TFM/section4.2/ventana.movil/tasmax.obs.DD.1976-2005.rds")
# tmax.rcm <- readRDS("/lustre/gmeteo/WORK/fuentesm/TFM/section4.2/ventana.movil/tasmax.rcm.raw.DD.1976-2005.rds")
# tmax.rcp1 <- readRDS("/lustre/gmeteo/WORK/fuentesm/TFM/section4.2/ventana.movil/tmax.rcp85.DD.2041.2070.rds")
# tmax.rcp2 <- readRDS("/lustre/gmeteo/WORK/fuentesm/TFM/section4.2/ventana.movil/tmax.rcp85.DD.2071.2100.rds")
# tmin.obs <- readRDS("/lustre/gmeteo/WORK/fuentesm/TFM/section4.2/ventana.movil/tasmin.obs.DD.1976-2005.rds")
# tmin.rcm <- readRDS("/lustre/gmeteo/WORK/fuentesm/TFM/section4.2/ventana.movil/tasmin.rcm.raw.DD.1976-2005.rds")
# tmin.rcp1 <- readRDS("/lustre/gmeteo/WORK/fuentesm/TFM/section4.2/ventana.movil/tmin.rcp85.DD.2041.2070.rds")
# tmin.rcp2 <- readRDS("/lustre/gmeteo/WORK/fuentesm/TFM/section4.2/ventana.movil/tmin.rcp85.DD.2071.2100.rds")

subset_meses <- function(obs, rcm, rcp, mes, metodo, precipitation, bc) {
  meses_relacion <- meses(mes, metodo)
  # mes.rcm <- subsetGrid(rcm, season = mes)
  meses.rcm <- subsetGrid(rcm, season = meses_relacion)
  meses.obs <- subsetGrid(obs, season = meses_relacion)
  meses.rcp <- subsetGrid(rcp, season = meses_relacion)
  if (bc =="eqm"){
    bc1 <- biasCorrection(y=meses.obs, x=meses.rcm, newdata=meses.rcp, precipitation=precipitation,
                        method="eqm", extrapolation="constant", n.quantiles=99)
  } 
  bc.mes <- subsetGrid(bc1, season = mes)
  bc.mes$Dates$start <- as.character(bc.mes$Dates$start)
  bc.mes$Dates$end <- as.character(bc.mes$Dates$end)
  print(mes)
  return(bc.mes)
}

pr_eqm_rcp <- list()
# tmin_1 <- list()
# tmin_eqm <- list()

nombre_meses <- c("enero", "febrero", "marzo", "abril", "mayo", "junio", "julio", "agosto", "septiembre", "octubre", "noviembre", "diciembre")

for (mes in 1:12) {
    pr_eqm_rcp[[nombre_meses[mes]]] <- subset_meses(pr.obs, pr.rcm, pr.rcp2, mes, 2, TRUE, "eqm")
#   tmax_1[[nombre_meses[mes]]] <- subset_meses(tmax.obs, tmax.rcm, mes, 2, FALSE)
#   tmin_1[[nombre_meses[mes]]] <- subset_meses(tmin.obs, tmin.rcm, mes, 2, FALSE)
}

pr_eqm_rcp <- bindGrid(pr_eqm_rcp$enero, pr_eqm_rcp$febrero, pr_eqm_rcp$marzo, pr_eqm_rcp$abril, pr_eqm_rcp$mayo, pr_eqm_rcp$junio, 
                  pr_eqm_rcp$julio, pr_eqm_rcp$agosto, pr_eqm_rcp$septiembre, pr_eqm_rcp$octubre, pr_eqm_rcp$noviembre, 
                  pr_eqm_rcp$diciembre, dimension = "time")

saveRDS(pr_eqm_rcp, "/lustre/gmeteo/WORK/fuentesm/TFM/section4.2/ventana.movil/pr/pr.rcp85.eqm.DD.2071-2100.rds")


#########################################



tmax.obs <- readRDS("/lustre/gmeteo/WORK/fuentesm/TFM/section4.2/ventana.movil/tasmax.obs.DD.1976-2005.rds")
tmax.rcm <- readRDS("/lustre/gmeteo/WORK/fuentesm/TFM/section4.2/ventana.movil/tasmax.rcm.raw.DD.1976-2005.rds")
tmax.rcp1 <- readRDS("/lustre/gmeteo/WORK/fuentesm/TFM/section4.2/ventana.movil/tmax.rcp85.DD.2041.2070.rds")
tmax.rcp2 <- readRDS("/lustre/gmeteo/WORK/fuentesm/TFM/section4.2/ventana.movil/tmax.rcp85.DD.2071.2100.rds")
tmin.obs <- readRDS("/lustre/gmeteo/WORK/fuentesm/TFM/section4.2/ventana.movil/tasmin.obs.DD.1976-2005.rds")
tmin.rcm <- readRDS("/lustre/gmeteo/WORK/fuentesm/TFM/section4.2/ventana.movil/tasmin.rcm.raw.DD.1976-2005.rds")
tmin.rcp1 <- readRDS("/lustre/gmeteo/WORK/fuentesm/TFM/section4.2/ventana.movil/tmin.rcp85.DD.2041.2070.rds")
tmin.rcp2 <- readRDS("/lustre/gmeteo/WORK/fuentesm/TFM/section4.2/ventana.movil/tmin.rcp85.DD.2071.2100.rds")

################################################################################################################
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
    climatology(subsetGrid(data, season = season))
  })
  names(season_data) <- names(seasons)
  return(season_data)
}

# Aplicar la función al conjunto de datos pr.obs
pr.obs <- split_by_season(pr.obs, seasons)
pr.rcm.raw <- split_by_season(pr.rcm.raw, seasons)
pr.rcm.eqm <- split_by_season(pr.rcm.eqm, seasons)
pr.rcp.raw <- split_by_season(pr.rcp.raw, seasons)
pr.rcp.eqm <- split_by_season(pr.rcp.eqm, seasons)
pr.rcp.raw2 <- split_by_season(pr.rcp.raw2, seasons)
pr.rcp.eqm2 <- split_by_season(pr.rcp.eqm2, seasons)

precipitacion <- spatialPlot(makeMultiGrid(climatology(pr.obs$Winter), climatology(pr.obs$Spring), climatology(pr.obs$Summer), climatology(pr.obs$Autumn),
                                           climatology(pr.rcm.raw$Winter), climatology(pr.rcm.raw$Spring), climatology(pr.rcm.raw$Summer), climatology(pr.rcm.raw$Autumn),
                                            climatology(pr.rcm.eqm$Winter), climatology(pr.rcm.eqm$Spring), climatology(pr.rcm.eqm$Summer), climatology(pr.rcm.eqm$Autumn),
                                            climatology(pr.rcp.raw$Winter), climatology(pr.rcp.raw$Spring), climatology(pr.rcp.raw$Summer), climatology(pr.rcp.raw$Autumn),
                                            climatology(pr.rcp.eqm$Winter), climatology(pr.rcp.eqm$Spring), climatology(pr.rcp.eqm$Summer), climatology(pr.rcp.eqm$Autumn),
                                            climatology(pr.rcp.raw2$Winter), climatology(pr.rcp.raw2$Spring), climatology(pr.rcp.raw2$Summer), climatology(pr.rcp.raw2$Autumn),
                                            climatology(pr.rcp.eqm2$Winter), climatology(pr.rcp.eqm2$Spring), climatology(pr.rcp.eqm2$Summer), climatology(pr.rcp.eqm2$Autumn),
                                            skip.temporal.check=TRUE), backdrop.theme="coastline", as.table=TRUE, 
                                           names.attr=c("Invierno OBS 1976-2005", "Primavera OBS 1976-2005", "Verano OBS 1976-2005", "Otoño OBS 1976-2005",
                                                        "Invierno RCM 1976-2005", "Primavera RCM 1976-2005", "Verano RCM 1976-2005", "Otoño RCM 1976-2005",
                                                        "Invierno RCM EQM 1976-2005", "Primavera RCM EQM 1976-2005", "Verano RCM EQM 1976-2005", "Otoño RCM EQM 1976-2005",
                                                        "Invierno RCP 2041-2070", "Primavera RCP 2041-2070", "Verano RCP 2041-2070", "Otoño RCP 2041-2070",
                                                        "Invierno RCP EQM 2041-2070", "Primavera RCP EQM 2041-2070", "Verano RCP EQM 2041-2070", "Otoño RCP EQM 2041-2070",
                                                        "Invierno RCP 2071-2100", "Primavera RCP 2071-2100", "Verano RCP 2071-2100", "Otoño RCP 2071-2100",
                                                        "Invierno RCP EQM 2071-2100", "Primavera RCP EQM 2071-2100", "Verano RCP EQM 2071-2100", "Otoño RCP EQM 2071-2100"),
                                            color.theme="Blues", rev.colors=FALSE, main="Precipitación", layout=c(4,7),
                                            colorkey = list(space = "right",
                                                title = list("mm/día", cex = 1)))

pdf("new.s4.1/precipitacion.futuro.pdf", width=16, height=28)
precipitacion
dev.off()


tmax.obs <- split_by_season(tmax.obs, seasons)
tmax.rcm.raw <- split_by_season(tmax.rcm.raw, seasons)
tmax.rcm.eqm <- split_by_season(tmax.rcm.eqm, seasons)
tmax.rcp.raw <- split_by_season(tmax.rcp.raw, seasons)
tmax.rcp.eqm <- split_by_season(tmax.rcp.eqm, seasons)
tmax.rcp.raw2 <- split_by_season(tmax.rcp.raw2, seasons)
tmax.rcp.eqm2 <- split_by_season(tmax.rcp.eqm2, seasons)

temperatura_maxima <- spatialPlot(makeMultiGrid(climatology(tmax.obs$Winter), climatology(tmax.obs$Spring), climatology(tmax.obs$Summer), climatology(tmax.obs$Autumn),
                                           climatology(tmax.rcm.raw$Winter), climatology(tmax.rcm.raw$Spring), climatology(tmax.rcm.raw$Summer), climatology(tmax.rcm.raw$Autumn),
                                            climatology(tmax.rcm.eqm$Winter), climatology(tmax.rcm.eqm$Spring), climatology(tmax.rcm.eqm$Summer), climatology(tmax.rcm.eqm$Autumn),
                                            climatology(tmax.rcp.raw$Winter), climatology(tmax.rcp.raw$Spring), climatology(tmax.rcp.raw$Summer), climatology(tmax.rcp.raw$Autumn),
                                            climatology(tmax.rcp.eqm$Winter), climatology(tmax.rcp.eqm$Spring), climatology(tmax.rcp.eqm$Summer), climatology(tmax.rcp.eqm$Autumn),
                                            climatology(tmax.rcp.raw2$Winter), climatology(tmax.rcp.raw2$Spring), climatology(tmax.rcp.raw2$Summer), climatology(tmax.rcp.raw2$Autumn),
                                            climatology(tmax.rcp.eqm2$Winter), climatology(tmax.rcp.eqm2$Spring), climatology(tmax.rcp.eqm2$Summer), climatology(tmax.rcp.eqm2$Autumn),
                                            skip.temporal.check=TRUE), backdrop.theme="coastline", as.table=TRUE, 
                                           names.attr=c("Invierno OBS 1976-2005", "Primavera OBS 1976-2005", "Verano OBS 1976-2005", "Otoño OBS 1976-2005",
                                                        "Invierno RCM 1976-2005", "Primavera RCM 1976-2005", "Verano RCM 1976-2005", "Otoño RCM 1976-2005",
                                                        "Invierno RCM EQM 1976-2005", "Primavera RCM EQM 1976-2005", "Verano RCM EQM 1976-2005", "Otoño RCM EQM 1976-2005",
                                                        "Invierno RCP 2041-2070", "Primavera RCP 2041-2070", "Verano RCP 2041-2070", "Otoño RCP 2041-2070",
                                                        "Invierno RCP EQM 2041-2070", "Primavera RCP EQM 2041-2070", "Verano RCP EQM 2041-2070", "Otoño RCP EQM 2041-2070",
                                                        "Invierno RCP 2071-2100", "Primavera RCP 2071-2100", "Verano RCP 2071-2100", "Otoño RCP 2071-2100",
                                                        "Invierno RCP EQM 2071-2100", "Primavera RCP EQM 2071-2100", "Verano RCP EQM 2071-2100", "Otoño RCP EQM 2071-2100"),
                                            color.theme="Reds", rev.colors=FALSE, main="Temperatura máxima", layout=c(4,7),
                                            colorkey = list(space = "right",
                                                title = list("ºC", cex = 1)))
                                                
pdf("new.s4.1/temperatura_maxima.futuro.pdf", width=16, height=28)
temperatura_maxima
dev.off()

tmin.obs <- split_by_season(tmin.obs, seasons)
tmin.rcm.raw <- split_by_season(tmin.rcm.raw, seasons)
tmin.rcm.eqm <- split_by_season(tmin.rcm.eqm, seasons)
tmin.rcp.raw <- split_by_season(tmin.rcp.raw, seasons)
tmin.rcp.eqm <- split_by_season(tmin.rcp.eqm, seasons)
tmin.rcp.raw2 <- split_by_season(tmin.rcp.raw2, seasons)
tmin.rcp.eqm2 <- split_by_season(tmin.rcp.eqm2, seasons)

temperatura_minima <- spatialPlot(makeMultiGrid(climatology(tmin.obs$Winter), climatology(tmin.obs$Spring), climatology(tmin.obs$Summer), climatology(tmin.obs$Autumn),
                                           climatology(tmin.rcm.raw$Winter), climatology(tmin.rcm.raw$Spring), climatology(tmin.rcm.raw$Summer), climatology(tmin.rcm.raw$Autumn),
                                            climatology(tmin.rcm.eqm$Winter), climatology(tmin.rcm.eqm$Spring), climatology(tmin.rcm.eqm$Summer), climatology(tmin.rcm.eqm$Autumn),
                                            climatology(tmin.rcp.raw$Winter), climatology(tmin.rcp.raw$Spring), climatology(tmin.rcp.raw$Summer), climatology(tmin.rcp.raw$Autumn),
                                            climatology(tmin.rcp.eqm$Winter), climatology(tmin.rcp.eqm$Spring), climatology(tmin.rcp.eqm$Summer), climatology(tmin.rcp.eqm$Autumn),
                                            climatology(tmin.rcp.raw2$Winter), climatology(tmin.rcp.raw2$Spring), climatology(tmin.rcp.raw2$Summer), climatology(tmin.rcp.raw2$Autumn),
                                            climatology(tmin.rcp.eqm2$Winter), climatology(tmin.rcp.eqm2$Spring), climatology(tmin.rcp.eqm2$Summer), climatology(tmin.rcp.eqm2$Autumn),
                                            skip.temporal.check=TRUE), backdrop.theme="coastline", as.table=TRUE, 
                                           names.attr=c("Invierno OBS 1976-2005", "Primavera OBS 1976-2005", "Verano OBS 1976-2005", "Otoño OBS 1976-2005",
                                                        "Invierno RCM 1976-2005", "Primavera RCM 1976-2005", "Verano RCM 1976-2005", "Otoño RCM 1976-2005",
                                                        "Invierno RCM EQM 1976-2005", "Primavera RCM EQM 1976-2005", "Verano RCM EQM 1976-2005", "Otoño RCM EQM 1976-2005",
                                                        "Invierno RCP 2041-2070", "Primavera RCP 2041-2070", "Verano RCP 2041-2070", "Otoño RCP 2041-2070",
                                                        "Invierno RCP EQM 2041-2070", "Primavera RCP EQM 2041-2070", "Verano RCP EQM 2041-2070", "Otoño RCP EQM 2041-2070",
                                                        "Invierno RCP 2071-2100", "Primavera RCP 2071-2100", "Verano RCP 2071-2100", "Otoño RCP 2071-2100",
                                                        "Invierno RCP EQM 2071-2100", "Primavera RCP EQM 2071-2100", "Verano RCP EQM 2071-2100", "Otoño RCP EQM 2071-2100"),
                                            color.theme="Reds", rev.colors=FALSE, main="Temperatura mínima", layout=c(4,7),
                                            colorkey = list(space = "right",
                                                title = list("ºC", cex = 1)))

pdf("new.s4.1/temperatura_minima.futuro.pdf", width=16, height=28)
temperatura_minima
dev.off()

# Ahora sin el modelo sin corregir

pr.sin.raw <- spatialPlot(makeMultiGrid(climatology(pr.obs$Winter), climatology(pr.obs$Spring), climatology(pr.obs$Summer), climatology(pr.obs$Autumn),
                                            climatology(pr.rcm.eqm$Winter), climatology(pr.rcm.eqm$Spring), climatology(pr.rcm.eqm$Summer), climatology(pr.rcm.eqm$Autumn),
                                            climatology(pr.rcp.eqm$Winter), climatology(pr.rcp.eqm$Spring), climatology(pr.rcp.eqm$Summer), climatology(pr.rcp.eqm$Autumn),
                                            climatology(pr.rcp.eqm2$Winter), climatology(pr.rcp.eqm2$Spring), climatology(pr.rcp.eqm2$Summer), climatology(pr.rcp.eqm2$Autumn),
                                            skip.temporal.check=TRUE), backdrop.theme="coastline", as.table=TRUE, 
                                           names.attr=c("Invierno OBS 1976-2005", "Primavera OBS 1976-2005", "Verano OBS 1976-2005", "Otoño OBS 1976-2005",
                                                        "Invierno RCM EQM 1976-2005", "Primavera RCM EQM 1976-2005", "Verano RCM EQM 1976-2005", "Otoño RCM EQM 1976-2005",
                                                        "Invierno RCP EQM 2041-2070", "Primavera RCP EQM 2041-2070", "Verano RCP EQM 2041-2070", "Otoño RCP EQM 2041-2070",
                                                        "Invierno RCP EQM 2071-2100", "Primavera RCP EQM 2071-2100", "Verano RCP EQM 2071-2100", "Otoño RCP EQM 2071-2100"),
                                            color.theme="Blues", rev.colors=FALSE, main="Precipitación", layout=c(4,4),
                                            colorkey = list(space = "right",
                                                title = list("mm/día", cex = 1)))

pdf("new.s4.1/precipitacion.futuro.sin.raw.pdf", width=16, height=16)
pr.sin.raw
dev.off()

tmax.sin.raw <- spatialPlot(makeMultiGrid(climatology(tmax.obs$Winter), climatology(tmax.obs$Spring), climatology(tmax.obs$Summer), climatology(tmax.obs$Autumn),
                                            climatology(tmax.rcm.eqm$Winter), climatology(tmax.rcm.eqm$Spring), climatology(tmax.rcm.eqm$Summer), climatology(tmax.rcm.eqm$Autumn),
                                            climatology(tmax.rcp.eqm$Winter), climatology(tmax.rcp.eqm$Spring), climatology(tmax.rcp.eqm$Summer), climatology(tmax.rcp.eqm$Autumn),
                                            climatology(tmax.rcp.eqm2$Winter), climatology(tmax.rcp.eqm2$Spring), climatology(tmax.rcp.eqm2$Summer), climatology(tmax.rcp.eqm2$Autumn),
                                            skip.temporal.check=TRUE), backdrop.theme="coastline", as.table=TRUE, 
                                           names.attr=c("Invierno OBS 1976-2005", "Primavera OBS 1976-2005", "Verano OBS 1976-2005", "Otoño OBS 1976-2005",
                                                        "Invierno RCM EQM 1976-2005", "Primavera RCM EQM 1976-2005", "Verano RCM EQM 1976-2005", "Otoño RCM EQM 1976-2005",
                                                        "Invierno RCP EQM 2041-2070", "Primavera RCP EQM 2041-2070", "Verano RCP EQM 2041-2070", "Otoño RCP EQM 2041-2070",
                                                        "Invierno RCP EQM 2071-2100", "Primavera RCP EQM 2071-2100", "Verano RCP EQM 2071-2100", "Otoño RCP EQM 2071-2100"),
                                            color.theme="Reds", rev.colors=FALSE, main="Temperatura máxima", layout=c(4,4),
                                            colorkey = list(space = "right",
                                                title = list("ºC", cex = 1)))

pdf("new.s4.1/temperatura_maxima.futuro.sin.raw.pdf", width=16, height=16)
tmax.sin.raw
dev.off()

tmin.sin.raw <- spatialPlot(makeMultiGrid(climatology(tmin.obs$Winter), climatology(tmin.obs$Spring), climatology(tmin.obs$Summer), climatology(tmin.obs$Autumn),
                                            climatology(tmin.rcm.eqm$Winter), climatology(tmin.rcm.eqm$Spring), climatology(tmin.rcm.eqm$Summer), climatology(tmin.rcm.eqm$Autumn),
                                            climatology(tmin.rcp.eqm$Winter), climatology(tmin.rcp.eqm$Spring), climatology(tmin.rcp.eqm$Summer), climatology(tmin.rcp.eqm$Autumn),
                                            climatology(tmin.rcp.eqm2$Winter), climatology(tmin.rcp.eqm2$Spring), climatology(tmin.rcp.eqm2$Summer), climatology(tmin.rcp.eqm2$Autumn),
                                            skip.temporal.check=TRUE), backdrop.theme="coastline", as.table=TRUE, 
                                           names.attr=c("Invierno OBS 1976-2005", "Primavera OBS 1976-2005", "Verano OBS 1976-2005", "Otoño OBS 1976-2005",
                                                        "Invierno RCM EQM 1976-2005", "Primavera RCM EQM 1976-2005", "Verano RCM EQM 1976-2005", "Otoño RCM EQM 1976-2005",
                                                        "Invierno RCP EQM 2041-2070", "Primavera RCP EQM 2041-2070", "Verano RCP EQM 2041-2070", "Otoño RCP EQM 2041-2070",
                                                        "Invierno RCP EQM 2071-2100", "Primavera RCP EQM 2071-2100", "Verano RCP EQM 2071-2100", "Otoño RCP EQM 2071-2100"),
                                            color.theme="Reds", rev.colors=FALSE, main="Temperatura mínima", layout=c(4,4),
                                            colorkey = list(space = "right",
                                                title = list("ºC", cex = 1)))

pdf("new.s4.1/temperatura_minima.futuro.sin.raw.pdf", width=16, height=16)
tmin.sin.raw
dev.off()


############################################
###### Señal cambio climático ##############
############################################

pr.obs <- readRDS("/lustre/gmeteo/WORK/fuentesm/TFM/section4.2/ventana.movil/pr/pr.obs.DD.1976-2005.rds")
pr.rcm.eqm <- readRDS("/lustre/gmeteo/WORK/fuentesm/TFM/section4.2/ventana.movil/pr/pr.rcm.eqm.DD.1976-2005.rds")
pr.rcp.eqm <- readRDS("/lustre/gmeteo/WORK/fuentesm/TFM/section4.2/ventana.movil/pr/pr.rcp85.eqm.DD.2041-2070.rds")
pr.rcp.eqm2 <- readRDS("/lustre/gmeteo/WORK/fuentesm/TFM/section4.2/ventana.movil/pr/pr.rcp85.eqm.DD.2071-2100.rds")

tmax.obs <- readRDS("/lustre/gmeteo/WORK/fuentesm/TFM/section4.2/ventana.movil/tmax/tasmax.obs.DD.1976-2005.rds")
tmax.rcm.eqm <- readRDS("/lustre/gmeteo/WORK/fuentesm/TFM/section4.2/ventana.movil/tmax/tmax.rcm.eqm.DD.1976-2005.rds")
tmax.rcp.eqm <- readRDS("/lustre/gmeteo/WORK/fuentesm/TFM/section4.2/ventana.movil/tmax/tmax.rcp85.eqm.DD.2041-2070.rds")
tmax.rcp.eqm2 <- readRDS("/lustre/gmeteo/WORK/fuentesm/TFM/section4.2/ventana.movil/tmax/tmax.rcp85.eqm.DD.2071-2100.rds")

tmin.obs <- readRDS("/lustre/gmeteo/WORK/fuentesm/TFM/section4.2/ventana.movil/tmin/tasmin.obs.DD.1976-2005.rds")
tmin.rcm.eqm <- readRDS("/lustre/gmeteo/WORK/fuentesm/TFM/section4.2/ventana.movil/tmin/tmin.rcm.eqm.DD.1976-2005.rds")
tmin.rcp.eqm <- readRDS("/lustre/gmeteo/WORK/fuentesm/TFM/section4.2/ventana.movil/tmin/tmin.rcp85.eqm.DD.2041-2070.rds")
tmin.rcp.eqm2 <- readRDS("/lustre/gmeteo/WORK/fuentesm/TFM/section4.2/ventana.movil/tmin/tmin.rcp85.eqm.DD.2071-2100.rds")

pr.obs <- split_by_season(pr.obs, seasons)
pr.rcm.eqm <- split_by_season(pr.rcm.eqm, seasons)
pr.rcp.eqm <- split_by_season(pr.rcp.eqm, seasons)
pr.rcp.eqm2 <- split_by_season(pr.rcp.eqm2, seasons)

tmax.obs <- split_by_season(tmax.obs, seasons)
tmax.rcm.eqm <- split_by_season(tmax.rcm.eqm, seasons)
tmax.rcp.eqm <- split_by_season(tmax.rcp.eqm, seasons)
tmax.rcp.eqm2 <- split_by_season(tmax.rcp.eqm2, seasons)

tmin.obs <- split_by_season(tmin.obs, seasons)
tmin.rcm.eqm <- split_by_season(tmin.rcm.eqm, seasons)
tmin.rcp.eqm <- split_by_season(tmin.rcp.eqm, seasons)
tmin.rcp.eqm2 <- split_by_season(tmin.rcp.eqm2, seasons)

pr.obs.hist.eqm <- spatialPlot(makeMultiGrid(pr.obs$Winter, pr.obs$Spring, pr.obs$Summer, pr.obs$Autumn,
                                            pr.rcm.eqm$Winter, pr.rcm.eqm$Spring, pr.rcm.eqm$Summer, pr.rcm.eqm$Autumn, skip.temporal.check=TRUE),
                                            names.attr=c("Invierno OBS", "Primavera OBS", "Verano OBS", "Otoño OBS", "Invierno histórico EQM",
                                            "Primavera histórico EQM", "Verano histórico EQM", "Otoño histórico EQM"), main="Precipitación 1976-2005",
                                            layout=c(4,2), color.theme="Blues", backdrop.theme="coastline", as.table=TRUE, set.max=12, set.min=0, at=seq(0,12,1),
                                            colorkey = list(space = "right", title = list("mm/día", cex = 1)))

tmax.obs.hist.eqm <- spatialPlot(makeMultiGrid(tmax.obs$Winter, tmax.obs$Spring, tmax.obs$Summer, tmax.obs$Autumn,
                                               tmax.rcm.eqm$Winter, tmax.rcm.eqm$Spring, tmax.rcm.eqm$Summer, tmax.rcm.eqm$Autumn, skip.temporal.check=TRUE),
                                 names.attr=c("Invierno OBS", "Primavera OBS", "Verano OBS", "Otoño OBS", "Invierno histórico EQM",
                                              "Primavera histórico EQM", "Verano histórico EQM", "Otoño histórico EQM"),
                                 main="Temperatura máxima 1976-2005", set.max=36, set.min=0, at=seq(0,36,2),
                                 layout=c(4,2), color.theme="Reds", backdrop.theme="coastline", as.table=TRUE, 
                                 colorkey = list(space = "right", title = list("°C", cex = 1)))

tmin.obs.hist.eqm <- spatialPlot(makeMultiGrid(tmin.obs$Winter, tmin.obs$Spring, tmin.obs$Summer, tmin.obs$Autumn,
                                               tmin.rcm.eqm$Winter, tmin.rcm.eqm$Spring, tmin.rcm.eqm$Summer, tmin.rcm.eqm$Autumn, skip.temporal.check=TRUE),
                                 names.attr=c("Invierno OBS", "Primavera OBS", "Verano OBS", "Otoño OBS", "Invierno histórico EQM",
                                              "Primavera histórico EQM", "Verano histórico EQM", "Otoño histórico EQM"),
                                 main="Temperatura mínima 1976-2005", set.max=25, set.min=0, at=seq(-0,25,2),
                                 layout=c(4,2), color.theme="Reds", backdrop.theme="coastline", as.table=TRUE, 
                                 colorkey = list(space = "right", title = list("°C", cex = 1)))

# Inicializar una lista vacía para almacenar los resultados
pr.cc.1 <- list()
pr.cc.2 <- list()
tmax.cc.1 <- list()
tmax.cc.2 <- list()
tmin.cc.1 <- list()
tmin.cc.2 <- list()
# Definir las estaciones
seasons <- c("Winter", "Spring", "Summer", "Autumn")

# Iterar sobre cada estación y aplicar la operación
for (season in seasons) {
  pr.cc.1[[season]] <- gridArithmetics(pr.rcp.eqm[[season]], pr.rcm.eqm[[season]], operator="-")
  pr.cc.2[[season]] <- gridArithmetics(pr.rcp.eqm2[[season]], pr.rcm.eqm[[season]], operator="-")
  tmax.cc.1[[season]] <- gridArithmetics(tmax.rcp.eqm[[season]], tmax.rcm.eqm[[season]], operator="-")
  tmax.cc.2[[season]] <- gridArithmetics(tmax.rcp.eqm2[[season]], tmax.rcm.eqm[[season]], operator="-")
  tmin.cc.1[[season]] <- gridArithmetics(tmin.rcp.eqm[[season]], tmin.rcm.eqm[[season]], operator="-")
  tmin.cc.2[[season]] <- gridArithmetics(tmin.rcp.eqm2[[season]], tmin.rcm.eqm[[season]], operator="-")


}

pr.cambio.c <- spatialPlot(makeMultiGrid(pr.cc.1$Winter, pr.cc.1$Spring, pr.cc.1$Summer, pr.cc.1$Autumn,
                                         pr.cc.2$Winter, pr.cc.2$Spring, pr.cc.2$Summer, pr.cc.2$Autumn, skip.temporal.check=TRUE),
                           names.attr=c("Invierno 2041-2070", "Primavera 2041-2070", "Verano 2041-2070", "Otoño 2041-2070",
                                        "Invierno 2071-2100", "Primavera 2071-2100", "Verano 2071-2100", "Otoño 2071-2100"),
                           set.max=1.5, set.min=-1.5, at=seq(-1.5,1.5,0.25),
                           main="Señal cambio climático precipitación", layout=c(4,2), color.theme="RdBu", rev.colors=FALSE, backdrop.theme="coastline", as.table=TRUE, 
                           colorkey = list(space = "right", title = list("mm/día", cex = 1)))

tmax.cambio.c <- spatialPlot(makeMultiGrid(tmax.cc.1$Winter, tmax.cc.1$Spring, tmax.cc.1$Summer, tmax.cc.1$Autumn,
                                         tmax.cc.2$Winter, tmax.cc.2$Spring, tmax.cc.2$Summer, tmax.cc.2$Autumn, skip.temporal.check=TRUE),
                            names.attr=c("Invierno 2041-2070", "Primavera 2041-2070", "Verano 2041-2070", "Otoño 2041-2070",
                                          "Invierno 2071-2100", "Primavera 2071-2100", "Verano 2071-2100", "Otoño 2071-2100"),
                            set.max=7, set.min=-7, at=seq(-7,7,1),
                            main="Señal cambio climático temperatura máxima", layout=c(4,2), color.theme="RdBu", rev.colors=TRUE, backdrop.theme="coastline", as.table=TRUE,
                            colorkey = list(space = "right", title = list("°C", cex = 1)))

tmin.cambio.c <- spatialPlot(makeMultiGrid(tmin.cc.1$Winter, tmin.cc.1$Spring, tmin.cc.1$Summer, tmin.cc.1$Autumn,
                                         tmin.cc.2$Winter, tmin.cc.2$Spring, tmin.cc.2$Summer, tmin.cc.2$Autumn, skip.temporal.check=TRUE),
                            names.attr=c("Invierno 2041-2070", "Primavera 2041-2070", "Verano 2041-2070", "Otoño 2041-2070",
                                          "Invierno 2071-2100", "Primavera 2071-2100", "Verano 2071-2100", "Otoño 2071-2100"), 
                            set.max=6, set.min=-6, at=seq(-6,6,1),  
                            main="Señal cambio climático temperatura mínima", layout=c(4,2), color.theme="RdBu", rev.colors=TRUE, backdrop.theme="coastline", as.table=TRUE,
                            colorkey = list(space = "right", title = list("°C", cex = 1)))

library(gridExtra)
library(grid)

pdf("pr.señal.cambio.climatico.pdf", width=16, height=12)
grid.arrange(
  arrangeGrob(pr.obs.hist.eqm, ncol=1),
  arrangeGrob(pr.cambio.c, ncol=1),
  ncol=1)
dev.off()

pdf("tmax.señal.cambio.climatico.pdf", width=16, height=12)
grid.arrange(
  arrangeGrob(tmax.obs.hist.eqm, ncol=1),
  arrangeGrob(tmax.cambio.c, ncol=1),
  ncol=1)
dev.off()

pdf("tmin.señal.cambio.climatico.pdf", width=16, height=12)
grid.arrange(
  arrangeGrob(tmin.obs.hist.eqm, ncol=1),
  arrangeGrob(tmin.cambio.c, ncol=1),
  ncol=1)
dev.off()

###########################################################
###### Porcentajes verificar uso EQM ########################
###########################################################
pr.obs.05 <- climatology(pr.obs, clim.fun = list(FUN = "quantile", probs = 0.05, na.rm = TRUE))
pr.obs.95 <- climatology(pr.obs, clim.fun = list(FUN = "quantile", probs = 0.95, na.rm = TRUE))

pr.rcm.05 <- climatology(pr.rcm, clim.fun = list(FUN = "quantile", probs = 0.05, na.rm = TRUE))
pr.rcm.95 <- climatology(pr.rcm, clim.fun = list(FUN = "quantile", probs = 0.95, na.rm = TRUE))

pr.eqm.05 <- climatology(pr.eqm, clim.fun = list(FUN = "quantile", probs = 0.05, na.rm = TRUE))
pr.eqm.95 <- climatology(pr.eqm, clim.fun = list(FUN = "quantile", probs = 0.95, na.rm = TRUE))

pr.sc.05 <- climatology(pr.sc, clim.fun = list(FUN = "quantile", probs = 0.05, na.rm = TRUE))
pr.sc.95 <- climatology(pr.sc, clim.fun = list(FUN = "quantile", probs = 0.95, na.rm = TRUE))

tmax.obs.05 <- climatology(tmax.obs, clim.fun = list(FUN = "quantile", probs = 0.05, na.rm = TRUE))
tmax.obs.95 <- climatology(tmax.obs, clim.fun = list(FUN = "quantile", probs = 0.95, na.rm = TRUE))

tmax.rcm.05 <- climatology(tmax.rcm, clim.fun = list(FUN = "quantile", probs = 0.05, na.rm = TRUE))
tmax.rcm.95 <- climatology(tmax.rcm, clim.fun = list(FUN = "quantile", probs = 0.95, na.rm = TRUE))

tmax.eqm.05 <- climatology(tmax.eqm, clim.fun = list(FUN = "quantile", probs = 0.05, na.rm = TRUE))
tmax.eqm.95 <- climatology(tmax.eqm, clim.fun = list(FUN = "quantile", probs = 0.95, na.rm = TRUE))

tmax.sc.05 <- climatology(tmax.sc, clim.fun = list(FUN = "quantile", probs = 0.05, na.rm = TRUE))
tmax.sc.95 <- climatology(tmax.sc, clim.fun = list(FUN = "quantile", probs = 0.95, na.rm = TRUE))

tmin.obs.05 <- climatology(tmin.obs, clim.fun = list(FUN = "quantile", probs = 0.05, na.rm = TRUE))
tmin.obs.95 <- climatology(tmin.obs, clim.fun = list(FUN = "quantile", probs = 0.95, na.rm = TRUE))

tmin.rcm.05 <- climatology(tmin.rcm, clim.fun = list(FUN = "quantile", probs = 0.05, na.rm = TRUE))
tmin.rcm.95 <- climatology(tmin.rcm, clim.fun = list(FUN = "quantile", probs = 0.95, na.rm = TRUE))

tmin.eqm.05 <- climatology(tmin.eqm, clim.fun = list(FUN = "quantile", probs = 0.05, na.rm = TRUE))
tmin.eqm.95 <- climatology(tmin.eqm, clim.fun = list(FUN = "quantile", probs = 0.95, na.rm = TRUE))

tmin.sc.05 <- climatology(tmin.sc, clim.fun = list(FUN = "quantile", probs = 0.05, na.rm = TRUE))
tmin.sc.95 <- climatology(tmin.sc, clim.fun = list(FUN = "quantile", probs = 0.95, na.rm = TRUE))

pr.rcm.05.sesgo <- gridArithmetics(pr.rcm.05, pr.obs.05, operator="-")
pr.rcm.95.sesgo <- gridArithmetics(pr.rcm.95, pr.obs.95, operator="-")
pr.sc.05.sesgo <- gridArithmetics(pr.sc.05, pr.obs.05, operator="-")
pr.sc.95.sesgo <- gridArithmetics(pr.sc.95, pr.obs.95, operator="-")
pr.eqm.05.sesgo <- gridArithmetics(pr.eqm.05, pr.obs.05, operator="-")
pr.eqm.95.sesgo <- gridArithmetics(pr.eqm.95, pr.obs.95, operator="-")

tmax.rcm.05.sesgo <- gridArithmetics(tmax.rcm.05, tmax.obs.05, operator="-")
tmax.rcm.95.sesgo <- gridArithmetics(tmax.rcm.95, tmax.obs.95, operator="-")
tmax.sc.05.sesgo <- gridArithmetics(tmax.sc.05, tmax.obs.05, operator="-")
tmax.sc.95.sesgo <- gridArithmetics(tmax.sc.95, tmax.obs.95, operator="-")
tmax.eqm.05.sesgo <- gridArithmetics(tmax.eqm.05, tmax.obs.05, operator="-")
tmax.eqm.95.sesgo <- gridArithmetics(tmax.eqm.95, tmax.obs.95, operator="-")

tmin.rcm.05.sesgo <- gridArithmetics(tmin.rcm.05, tmin.obs.05, operator="-")
tmin.rcm.95.sesgo <- gridArithmetics(tmin.rcm.95, tmin.obs.95, operator="-")
tmin.sc.05.sesgo <- gridArithmetics(tmin.sc.05, tmin.obs.05, operator="-")
tmin.sc.95.sesgo <- gridArithmetics(tmin.sc.95, tmin.obs.95, operator="-")
tmin.eqm.05.sesgo <- gridArithmetics(tmin.eqm.05, tmin.obs.05, operator="-")
tmin.eqm.95.sesgo <- gridArithmetics(tmin.eqm.95, tmin.obs.95, operator="-")


library(gridExtra)
library(grid)


# Crear el gráfico con observaciones y RCM para percentiles 5 y 95
pr.qq1 <- spatialPlot(makeMultiGrid(pr.obs.05,
                                    pr.obs.95, skip.temporal.check=TRUE),
                      names.attr=c("OBS percentil 5",
                                   "OBS percentil 95"),
                      main="Precipitación - Observaciones", layout=c(1,2),
                      color.theme="Blues", backdrop.theme="coastline", as.table=TRUE, 
                      colorkey = list(space = "right", title = list("mm/día", cex = 1)))

                      # Crear el gráfico con los sesgos para percentiles 5 y 95
pr.qq2 <- spatialPlot(makeMultiGrid(pr.rcm.05.sesgo,pr.sc.05.sesgo, pr.eqm.05.sesgo,
                                    pr.rcm.95.sesgo, pr.sc.95.sesgo, pr.eqm.95.sesgo, skip.temporal.check=TRUE),
                      names.attr=c("Histórico RAW percentil 5", "Scaling percentil 5", "EQM percentil 5",
                                   "Histórico RAW percentil 95", "Scaling percentil 95", "EQM percentil 95"),
                      main="Precipitación - Sesgo histórico sin corregir y corregido", layout=c(3,2),
                      color.theme="RdBu", backdrop.theme="coastline", as.table=TRUE, 
                      set.max=3, set.min=-3, at=seq(-3,3,0.5), rev.colors=FALSE,
                      colorkey = list(space = "right", title = list("mm/día", cex = 1)))

# Crear el gráfico con observaciones y RCM para percentiles 5 y 95 de tmax
tmax.qq1 <- spatialPlot(makeMultiGrid(tmax.obs.05,
                                      tmax.obs.95, skip.temporal.check=TRUE),
                        names.attr=c("OBS percentil 5",
                                     "OBS percentil 95"),
                        main="Temperatura máxima - Observaciones", layout=c(1,2),
                        color.theme="Reds", backdrop.theme="coastline", as.table=TRUE, 
                        colorkey = list(space = "right", title = list("ºC", cex = 1)))

# Crear el gráfico con los sesgos para percentiles 5 y 95 de tmax
tmax.qq2 <- spatialPlot(makeMultiGrid(tmax.rcm.05.sesgo,tmax.sc.05.sesgo, tmax.eqm.05.sesgo,
                                      tmax.rcm.95.sesgo,tmax.sc.95.sesgo, tmax.eqm.95.sesgo, skip.temporal.check=TRUE),
                        names.attr=c("Histórico RAW percentil 5", "Scaling percentil 5", "EQM percentil 5",
                                     "Histórico RAW percentil 95", "Scaling percentil 95", "EQM percentil 95"),
                        main="Temperatura máxima - Sesgo histórico sin corregir y corregido", layout=c(3,2),
                        color.theme="RdBu", backdrop.theme="coastline", as.table=TRUE, 
                        set.max=5, set.min=-5, at=seq(-5,5,0.5), rev.colors=TRUE,
                        colorkey = list(space = "right", title = list("ºC", cex = 1)))

# Crear el gráfico con observaciones y RCM para percentiles 5 y 95 de temperatura mínima
tmin.qq1 <- spatialPlot(makeMultiGrid(tmin.obs.05,
                                      tmin.obs.95, skip.temporal.check=TRUE),
                        names.attr=c("OBS percentil 5",
                                     "OBS percentil 95"),
                        main="Temperatura mínima - Observaciones", layout=c(1,2),
                        color.theme="Reds", backdrop.theme="coastline", as.table=TRUE, 
                        colorkey = list(space = "right", title = list("ºC", cex = 1)))

# Crear el gráfico con los sesgos para percentiles 5 y 95 de temperatura mínima
tmin.qq2 <- spatialPlot(makeMultiGrid(tmin.rcm.05.sesgo,tmin.sc.05.sesgo, tmin.eqm.05.sesgo,
                                      tmin.rcm.95.sesgo,tmin.sc.95.sesgo, tmin.eqm.95.sesgo, skip.temporal.check=TRUE),
                        names.attr=c("Histórico RAW percentil 5","Scaling percentil 5", "EQM percentil 5",
                                     "Histórico RAW percentil 95","Scaling percentil 95", "EQM percentil 95"),
                        main="Temperatura mínima - Sesgo histórico sin corregir y corregido", layout=c(3,2),
                        color.theme="RdBu", backdrop.theme="coastline", as.table=TRUE, 
                        set.max=4, set.min=-4, at=seq(-4,4,0.5), rev.colors=TRUE,
                        colorkey = list(space = "right", title = list("ºC", cex = 1)))

library(gridExtra)
library(grid)

pdf("qq.plots.var.esen.pdf", width=12, height=13)
grid.arrange(
  arrangeGrob(pr.qq1, pr.qq2, ncol=2, widths=c(0.85, 2)),  # Ajustar el ancho relativo
  arrangeGrob(tmax.qq1, tmax.qq2, ncol=2, widths=c(0.85, 2)),
  arrangeGrob(tmin.qq1, tmin.qq2, ncol=2, widths=c(0.85, 2)),
  ncol=1
)
dev.off()

