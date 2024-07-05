options(java.parameters = "-Xmx32g")
library(SPEI)
library(zoo)
library(loadeR)  
library(transformeR)
library(drought4R)

path <- "/lustre/gmeteo/WORK/fuentesm/TFM"

# # Load the data
# pr <- readRDS("/oceano/gmeteo/users/fuentesm/AEMET/TFM/section4.2/pr.rcm.eqm.DD.1976-2005.rds")
# tmax <- readRDS("/oceano/gmeteo/users/fuentesm/AEMET/TFM/section4.2/tasmax.rcm.eqm.DD.1976-2005.rds")
# tmin <- readRDS("/oceano/gmeteo/users/fuentesm/AEMET/TFM/section4.2/tasmin.rcm.eqm.DD.1976-2005.rds")

# # Redim
# pr <- redim(pr, drop = TRUE)
# tmax <- redim(tmax, drop = TRUE)
# tmin <- redim(tmin, drop = TRUE)

# # Calculate the PET  (daily)
# pet <-  petGrid(tasmin = tmin, tasmax = tmax, pr = pr, method = "hargreaves-samani")
# pet <- redim(pet, drop = TRUE)

# # Monthly agregation
# pr.MM <- aggregateGrid(grid = pr, aggr.m = list(FUN = "sum", na.rm = TRUE))
# pet.MM <- aggregateGrid(grid = pet, aggr.m = list(FUN = "mean", na.rm = TRUE))

# # Calculate Hidric Balance
# wbalance <- gridArithmetics(pr.MM, pet.MM, operator = "-")
# # Choose the gridbox
# wbalance.gb <- subsetGrid(wbalance, lonLim = c(-4.1), latLim = c(40.4))

# # Calcular el SPEI con un período de escala de 6 meses
# spei.gb <- spei(wbalance.gb$Data, scale = 6, na.rm = TRUE)

# spei.obs <- speiGrid(et0.grid = pet.MM, pr.grid = pr.MM, scale = 6, ref.start = c(1976,1), ref.end=c(2005,12), na.rm = TRUE)
# spei.obs.gb <- subsetGrid(spei.obs, lonLim = c(-4.1), latLim = c(40.4))


# png("/oceano/gmeteo/users/fuentesm/AEMET/spei.gridbox/spei.eqm.png", width = 1200, height = 400)
# plot(ts(spei.gb$fitted, start =c(1976,1), frequency=12), main="SPEI-6 EQM", ylab="SPEI-6")
# grid()
# abline(h=0)
# lines(ts(spei.obs.gb$Data, start =c(1976,1), frequency=12), col="red")
# legend("topright", legend=c("SPEI-6 C4R", "SPEI-6 Beguería"), col=c("red", "black"), lty=1:1, cex=0.8)
# dev.off()


# ----------------------------------------------------------------------------------------------------
latLim <- c(40.4)
lonLim <- c(-4.1)

# Observations
pr.obs <- readRDS(paste0(path, "/section4.2/pr.obs.DD.1976-2005.rds"))
tmax.obs <- readRDS(paste0(path, "/section4.2/tasmax.obs.DD.1976-2005.rds"))
tmin.obs <- readRDS(paste0(path, "/section4.2/tasmin.obs.DD.1976-2005.rds"))

# Redim
pr.obs <- redim(pr.obs, drop = TRUE)
tmax.obs <- redim(tmax.obs, drop = TRUE)
tmin.obs <- redim(tmin.obs, drop = TRUE)

# Calculate the PET  (daily)
pet.obs <-  petGrid(tasmin = tmin.obs, tasmax = tmax.obs, pr = pr.obs, method = "hargreaves-samani")
pet.obs <- redim(pet.obs, drop = TRUE)

# Monthly agregation
pr.obs.MM <- aggregateGrid(grid = pr.obs, aggr.m = list(FUN = "sum", na.rm = TRUE))
pet.obs.MM <- aggregateGrid(grid = pet.obs, aggr.m = list(FUN = "mean", na.rm = TRUE))
tmax.obs.MM <- aggregateGrid(grid = tmax.obs, aggr.m = list(FUN = "mean", na.rm = TRUE))
tmin.obs.MM <- aggregateGrid(grid = tmin.obs, aggr.m = list(FUN = "mean", na.rm = TRUE))    

# # Calculate Hidric Balance
# wbalance.obs <- gridArithmetics(pr.obs.MM, pet.obs.MM, operator = "-")

# Calculate SPEI C4R
spei.obs.c4r <- speiGrid(et0.grid = pet.obs.MM, pr.grid = pr.obs.MM, scale = 6, ref.start = c(1976,1), ref.end=c(2005,12), na.rm = TRUE)

# Choose the gridbox
pr.obs.MM.gb <- subsetGrid(pr.obs.MM, lonLim = lonLim, latLim = latLim)
tmax.obs.MM.gb <- subsetGrid(tmax.obs.MM, lonLim = lonLim, latLim = latLim)
tmin.obs.MM.gb <- subsetGrid(tmin.obs.MM, lonLim = lonLim, latLim = latLim)
pet.obs.MM.gb <- subsetGrid(pet.obs.MM, lonLim = lonLim, latLim = latLim)
# wbalance.obs.gb <- subsetGrid(wbalance.obs, lonLim = lonLim, latLim = latLim)
spei.obs.gb <- subsetGrid(spei.obs.c4r, lonLim = lonLim, latLim = latLim)

# # Calcular el SPEI con un período de escala de 6 meses
# spei.obs.gb <- spei(wbalance.obs.gb$Data, scale = 6, na.rm = TRUE)


# MODEL RCM RAW
pr.rcm <- readRDS(paste0(path, "/section4.2/pr.rcm.raw.DD.1976-2005.rds"))
tmax.rcm <- readRDS(paste0(path, "/section4.2/tasmax.rcm.raw.DD.1976-2005.rds"))
tmin.rcm <- readRDS(paste0(path, "/section4.2/tasmin.rcm.raw.DD.1976-2005.rds"))

# Redim
pr.rcm <- redim(pr.rcm, drop = TRUE)
tmax.rcm <- redim(tmax.rcm, drop = TRUE)
tmin.rcm <- redim(tmin.rcm, drop = TRUE)

# Calculate the PET  (daily)
pet.rcm <-  petGrid(tasmin = tmin.rcm, tasmax = tmax.rcm, pr = pr.rcm, method = "hargreaves-samani")
pet.rcm <- redim(pet.rcm, drop = TRUE)

# Monthly agregation
pr.rcm.MM <- aggregateGrid(grid = pr.rcm, aggr.m = list(FUN = "sum", na.rm = TRUE))
pet.rcm.MM <- aggregateGrid(grid = pet.rcm, aggr.m = list(FUN = "mean", na.rm = TRUE))
tmax.rcm.MM <- aggregateGrid(grid = tmax.rcm, aggr.m = list(FUN = "mean", na.rm = TRUE))
tmin.rcm.MM <- aggregateGrid(grid = tmin.rcm, aggr.m = list(FUN = "mean", na.rm = TRUE))

# Calculate Hidric Balance
# wbalance.rcm <- gridArithmetics(pr.rcm.MM, pet.rcm.MM, operator = "-")

# Calculate SPEI C4R
spei.rcm.c4r <- speiGrid(et0.grid = pet.rcm.MM, pr.grid = pr.rcm.MM, scale = 6, ref.start = c(1976,1), ref.end=c(2005,12), na.rm = TRUE)

# Choose the gridbox
pr.rcm.gb <- subsetGrid(pr.rcm.MM, lonLim = lonLim, latLim = latLim)
tmax.rcm.gb <- subsetGrid(tmax.rcm.MM, lonLim = lonLim, latLim = latLim)
tmin.rcm.gb <- subsetGrid(tmin.rcm.MM, lonLim = lonLim, latLim = latLim)
pet.rcm.MM.gb <- subsetGrid(pet.rcm.MM, lonLim = lonLim, latLim = latLim)
# wbalance.rcm.gb <- subsetGrid(wbalance.rcm, lonLim = lonLim, latLim = latLim)
spei.rcm.gb <- subsetGrid(spei.rcm.c4r, lonLim = lonLim, latLim = latLim)

# Calcular el SPEI con un período de escala de 6 meses
# spei.rcm.gb <- spei(wbalance.rcm.gb$Data, scale = 6, na.rm = TRUE)

# Model Correction EQM
pr.eqm <- readRDS(paste0(path, "/section4.2/pr.rcm.eqm.DD.1976-2005.rds"))
tmax.eqm <- readRDS(paste0(path, "/section4.2/tasmax.rcm.eqm.DD.1976-2005.rds"))
tmin.eqm <- readRDS(paste0(path, "/section4.2/tasmin.rcm.eqm.DD.1976-2005.rds"))

# Redim
pr.eqm <- redim(pr.eqm, drop = TRUE)
tmax.eqm <- redim(tmax.eqm, drop = TRUE)
tmin.eqm <- redim(tmin.eqm, drop = TRUE)

# Calculate the PET  (daily)
pet.eqm <-  petGrid(tasmin = tmin.eqm, tasmax = tmax.eqm, pr = pr.eqm, method = "hargreaves-samani")
pet.eqm <- redim(pet.eqm, drop = TRUE)

# Monthly agregation
pr.eqm.MM <- aggregateGrid(grid = pr.eqm, aggr.m = list(FUN = "sum", na.rm = TRUE))
pet.eqm.MM <- aggregateGrid(grid = pet.eqm, aggr.m = list(FUN = "mean", na.rm = TRUE))
tmax.eqm.MM <- aggregateGrid(grid = tmax.eqm, aggr.m = list(FUN = "mean", na.rm = TRUE))
tmin.eqm.MM <- aggregateGrid(grid = tmin.eqm, aggr.m = list(FUN = "mean", na.rm = TRUE))

# Calculate Hidric Balance
# wbalance.eqm <- gridArithmetics(pr.eqm.MM, pet.eqm.MM, operator = "-")

# Calculate SPEI C4R
spei.eqm.c4r <- speiGrid(et0.grid = pet.eqm.MM, pr.grid = pr.eqm.MM, scale = 6, ref.start = c(1976,1), ref.end=c(2005,12), na.rm = TRUE)

# Choose the gridbox
pr.eqm.gb <- subsetGrid(pr.eqm.MM, lonLim = lonLim, latLim = latLim)
tmax.eqm.gb <- subsetGrid(tmax.eqm.MM, lonLim = lonLim, latLim = latLim)
tmin.eqm.gb <- subsetGrid(tmin.eqm.MM, lonLim = lonLim, latLim = latLim)
pet.eqm.MM.gb <- subsetGrid(pet.eqm.MM, lonLim = lonLim, latLim = latLim)
# wbalance.eqm.gb <- subsetGrid(wbalance.eqm, lonLim = lonLim, latLim = latLim)
spei.eqm.gb <- subsetGrid(spei.eqm.c4r, lonLim = lonLim, latLim = latLim)

# Calcular el SPEI con un período de escala de 6 meses
# spei.eqm.gb <- spei(wbalance.eqm.gb$Data, scale = 6, na.rm = TRUE)

#----------------------------------------------------------------------
png(paste0(path, "/pr.png"), width = 1200, height = 400)
plot(ts(pr.obs.MM.gb$Data, start =c(1976,1), frequency=12), main="Precipitación // Latitud = 40.4 Longitud = -4.1 // 1976-2005", ylab="mm/mes", xlab="Tiempo / mes")
grid()
lines(ts(pr.rcm.gb$Data, start =c(1976,1), frequency=12), col="red")
lines(ts(pr.eqm.gb$Data, start =c(1976,1), frequency=12), col="blue")
legend("topright", legend=c("Pr OBS", "Pr RCM RAW", "Pr RCM EQM"), col=c("black", "red", "blue"), lty=1:1, cex=0.8)
dev.off()

png(paste0(path, "/tmax.png"), width = 1200, height = 400)
plot(ts(tmax.obs.MM.gb$Data, start =c(1976,1), frequency=12), main="Tmax // Latitud = 40.4 Longitud = -4.1 // 1976-2005", ylab="ºC", xlab="Tiempo / mes")
grid()
lines(ts(tmax.rcm.gb$Data, start =c(1976,1), frequency=12), col="red")
lines(ts(tmax.eqm.gb$Data, start =c(1976,1), frequency=12), col="blue")
legend("topright", legend=c("Tmax OBS", "Tmax RCM RAW", "Tmax RCM EQM"), col=c("black", "red", "blue"), lty=1:1, cex=0.8)
dev.off()

png(paste0(path, "/tmin.png"), width = 1200, height = 400)
plot(ts(tmin.obs.MM.gb$Data, start =c(1976,1), frequency=12), main="Tmin // Latitud = 40.4 Longitud = -4.1 // 1976-2005", ylab="ºC", xlab="Tiempo / mes")
grid()
lines(ts(tmin.rcm.gb$Data, start =c(1976,1), frequency=12), col="red")
lines(ts(tmin.eqm.gb$Data, start =c(1976,1), frequency=12), col="blue")
legend("topright", legend=c("Tmin OBS", "Tmin RCM RAW", "Tmin RCM EQM"), col=c("black", "red", "blue"), lty=1:1, cex=0.8)
dev.off()

png(paste0(path, "/spei.png"), width = 1200, height = 400)
plot(ts(spei.obs.gb$Data, start =c(1976,1), frequency=12), main="SPEI // Latitud = 40.4 Longitud = -4.1 // 1976-2005", ylab="SPEI 6m", xlab="Tiempo / mes")
grid()
lines(ts(spei.rcm.gb$Data, start =c(1976,1), frequency=12), col="red")
lines(ts(spei.eqm.gb$Data, start =c(1976,1), frequency=12), col="blue")
legend("topright", legend=c("SPEI OBS", "SPEI RCM RAW", "SPEI RCM EQM"), col=c("black", "red", "blue"), lty=1:1, cex=0.8)
dev.off()

png(paste0(path, "/pet.png"), width = 1200, height = 400)
plot(ts(pet.obs.MM.gb$Data, start =c(1976,1), frequency=12), main="PET // Latitud = 40.4 Longitud = -4.1 // 1976-2005", ylab="mm/mes", xlab="Tiempo / mes")
grid()
lines(ts(pet.rcm.MM.gb$Data, start =c(1976,1), frequency=12), col="red")
lines(ts(pet.eqm.MM.gb$Data, start =c(1976,1), frequency=12), col="blue")
legend("topright", legend=c("PET OBS", "PET RCM RAW", "PET RCM EQM"), col=c("black", "red", "blue"), lty=1:1, cex=0.8)
dev.off()

pr <- readPNG(paste0(path, "/pr.png"))
tmax <- readPNG(paste0(path, "/tmax.png"))
tmin <- readPNG(paste0(path, "/tmin.png"))
spei <- readPNG(paste0(path, "/spei.png"))
pet <- readPNG(paste0(path, "/pet.png"))


pdf(file = paste0(path, "/timeseries_combined.pdf"), width = 12, height = 12)  # Ajusta el tamaño del PDF según sea necesario
grid.arrange(
  rasterGrob(pr, interpolate=TRUE),
  rasterGrob(tmax, interpolate=TRUE),
  rasterGrob(tmin, interpolate=TRUE),
  rasterGrob(pet, interpolate=TRUE),
  rasterGrob(spei, interpolate=TRUE),
  nrow = 5  # Número de filas en la disposición de los gráficos
)
dev.off()


# ----------------------------------------------------------------------------------------------------
# Hago un qqplot
png(paste0(path, "/qqplot_pr.png"), width = 800, height = 800)
qqplot(pr.obs.MM.gb$Data, pr.eqm.gb$Data, main="QQplot Pr OBS vs Pr RCM EQM", xlab="Pr OBS", ylab="Pr RCM RAW")
grid()
abline(0,1)
dev.off()

png(paste0(path, "/qqplot_tmax.png"), width = 800, height = 800)
qqplot(tmax.obs.MM.gb$Data, tmax.eqm.gb$Data, main="QQplot Tmax OBS vs Tmax RCM EQM", xlab="Tmax OBS", ylab="Tmax RCM RAW")
grid()
abline(0,1)
dev.off()

png(paste0(path, "/qqplot_tmin.png"), width = 800, height = 800)
qqplot(tmin.obs.MM.gb$Data, tmin.eqm.gb$Data, main="QQplot Tmin OBS vs Tmin RCM EQM", xlab="Tmin OBS", ylab="Tmin RCM RAW")
grid()
abline(0,1)
dev.off()

png(paste0(path, "/qqplot_pet.png"), width = 800, height = 800)
qqplot(pet.obs.MM.gb$Data, pet.eqm.MM.gb$Data, main="QQplot PET OBS vs PET RCM EQM", xlab="PET OBS", ylab="PET RCM RAW")
grid()
abline(0,1)
dev.off()

png(paste0(path, "/qqplot_spei.png"), width = 800, height = 800)
qqplot(spei.obs.gb$Data, spei.eqm.gb$Data, main="QQplot SPEI OBS vs SPEI RCM EQM", xlab="SPEI OBS", ylab="SPEI RCM RAW")
grid()
abline(0,1)
dev.off()

library(gridExtra)

# Cargar las imágenes generadas
qqplot_pr <- readPNG(paste0(path, "/qqplot_pr.png"))
qqplot_tmax <- readPNG(paste0(path, "/qqplot_tmax.png"))
qqplot_tmin <- readPNG(paste0(path, "/qqplot_tmin.png"))
qqplot_pet <- readPNG(paste0(path, "/qqplot_pet.png"))
qqplot_spei <- readPNG(paste0(path, "/qqplot_spei.png"))

# Crear la disposición de los gráficos usando grid.arrange

grid.arrange(
  rasterGrob(qqplot_pr, interpolate=TRUE),
  rasterGrob(qqplot_tmax, interpolate=TRUE),
  rasterGrob(qqplot_tmin, interpolate=TRUE),
  rasterGrob(qqplot_pet, interpolate=TRUE),
  rasterGrob(qqplot_spei, interpolate=TRUE),
  nrow = 2  # Número de filas en la disposición de los gráficos
)

pdf(file = paste0(path, "/qqplots_combined.pdf"), width = 12, height = 12)  # Ajusta el tamaño del PDF según sea necesario
grid.arrange(
  rasterGrob(qqplot_pr, interpolate=TRUE),
  rasterGrob(qqplot_tmax, interpolate=TRUE),
  rasterGrob(qqplot_tmin, interpolate=TRUE),
  rasterGrob(qqplot_pet, interpolate=TRUE),
  rasterGrob(qqplot_spei, interpolate=TRUE),
  nrow = 2  # Número de filas en la disposición de los gráficos
)
dev.off()