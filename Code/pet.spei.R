options(java.parameters = "-Xmx32g")
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

# Read the data
path <- "/lustre/gmeteo/WORK/fuentesm/TFM"

pr <- readRDS(paste0(path, "/section4.1/pr.obs.DD.1971-2020.rds"))
tmax <- readRDS(paste0(path, "/section4.1/tasmax.obs.DD.1971-2020.rds"))
tmin <- readRDS(paste0(path, "/section4.1/tasmin.obs.DD.1971-2020.rds"))

# Redim
pr <- redim(pr, drop = TRUE)
tmax <- redim(tmax, drop = TRUE)
tmin <- redim(tmin, drop = TRUE)

# Calculate the PET  (daily)
pet <-  petGrid(tasmin = tmin, tasmax = tmax, pr = pr, method = "hargreaves-samani")
pet <- redim(pet, drop = TRUE)

# Monthly agregation
pr.MM <- aggregateGrid(grid = pr, aggr.m = list(FUN = "sum", na.rm = TRUE))
pet.MM <- aggregateGrid(grid = pet, aggr.m = list(FUN = "mean", na.rm = TRUE))

# Calculate the SPEI
spei <- speiGrid(et0.grid = pet.MM, pr.grid = pr.MM, scale = 6, ref.start = c(1976,1), ref.end=c(2005,12), na.rm = TRUE)

# Save the SPEI
saveRDS(spei, paste0(path, "/section4.1/spei.obs.MM.1971-2020.rds"))

# SPEI for months
spei.month <- list()
for (i in 1:12){
    spei.month[[i]] <- subsetGrid(spei, season = c(i))
    names(spei.month)[i] <- as.character(i)
}


# Parametros Mapas
MIN <- -0.06
MAX <- 0.06
STEP <- 0.01
UNITS <- "SPEI"

# Month to study
month <- 8

# Vector with the names of the months
months <- c("ENERO", "FEBRERO", "MARZO", "ABRIL", "MAYO", "JUNIO", "JULIO", "AGOSTO", "SEPTIEMBRE", "OCTUBRE", "NOVIEMBRE", "DICIEMBRE")
name.month <- months[month]

MAIN.TITLE <- paste("TENDENCIAS SPEI 6M OBSERVACIONES", name.month, "1971-2020")
path1 <- paste0(path, "/section4.1/spei.obs.", tolower(name.month), ".1971-2020.png")
sub.spei <- spei.month[[as.character(month)]]

# Calculate the trend
trend <- valueIndex(sub.spei, index.code = "Trend", return.NApercentage = FALSE)
sig <- valueIndex(sub.spei, index.code = "TrendSig", return.NApercentage = FALSE)
clim <- climatology(sig)
points <- visualizeR::map.stippling(clim, thereshold = 0.05, condition = "LT", pch = 19, cex = 0.25, col = "black")

# Plot the trend map for the month 
png(path1, width= 1000, height=700)
spatialPlot(trend, 
            backdrop.theme = "coastline",
            color.theme = "RdBu", 
            at = seq(MIN, MAX, STEP), set.max = MAX, set.min = MIN,
            main = list(MAIN.TITLE, cex = 1.5),
            colorkey = list(space = "bottom", height = 0.8, width = 0.75,
                            labels = list(cex = 0.6),
                            title = list(UNITS, cex = 1.5)),
            sp.layout=list(points)
            )
dev.off()


# Plot the trend maps for all the months
plot.list <- list()
months <- c("ENERO", "FEBRERO", "MARZO", "ABRIL", "MAYO", "JUNIO", "JULIO", "AGOSTO", "SEPTIEMBRE", "OCTUBRE", "NOVIEMBRE", "DICIEMBRE")

for (i in 1:12){
    name.month <- months[i]
    MAIN.TITLE <- name.month
    sub.spei <- spei.month[[as.character(i)]]
    
    trend <- valueIndex(sub.spei, index.code = "Trend", return.NApercentage = FALSE)
    sig <- valueIndex(sub.spei, index.code = "TrendSig", return.NApercentage = FALSE)
    clim <- climatology(sig)
    points <- visualizeR::map.stippling(clim, thereshold = 0.05, condition = "LT", pch = 19, cex = 0.25, col = "black")

    plot <- spatialPlot(trend, 
            backdrop.theme = "coastline",
            color.theme = "RdBu", 
            at = seq(-0.06, 0.06, 0.01), set.max = 0.06, set.min = -0.06,
            main = list(MAIN.TITLE, cex = 0.75),
            colorkey = list(space = "bottom", height = 0.8, width = 0.75,
                            labels = list(cex = 0.6),
                            title = list("SPEI", cex = 0.75)),
            sp.layout=list(points)
            )

    plot.list[[name.month]] <- plot
}

library(gridExtra)
library(grid)

# Crear el archivo PDF
library(gridExtra)

pdf(paste0(path, "/section4.1/spei.obs.1971-2020.pdf"), width = 9, height = 12)

# Create a grob (graphical object) for the main title
main_title <- textGrob("TENDENCIAS SPEI 6M OBSERVACIONES 1971-2020", gp=gpar(cex=1.5))

# Arrange the plots in a grid and add the main title
grid.arrange(
    plot.list$ENERO, plot.list$FEBRERO, plot.list$MARZO, plot.list$ABRIL, plot.list$MAYO, plot.list$JUNIO, 
    plot.list$JULIO, plot.list$AGOSTO, plot.list$SEPTIEMBRE, plot.list$OCTUBRE, plot.list$NOVIEMBRE, plot.list$DICIEMBRE,
    ncol = 3,
    top = main_title
)

dev.off()


