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
library(drought4R)

tmax <- readRDS("/oceano/gmeteo/users/fuentesm/AEMET/TFM/section4.1/tasmax.obs.DD.1971-2020.rds")
tmin <- readRDS("/oceano/gmeteo/users/fuentesm/AEMET/TFM/section4.1/tasmin.obs.DD.1971-2020.rds")
pr <- readRDS("/oceano/gmeteo/users/fuentesm/AEMET/TFM/section4.1/pr.obs.DD.1971-2020.rds")

years = 1996:2020

tmax <- subsetGrid(tmax, years = years)
tmin <- subsetGrid(tmin, years = years)
pr <- subsetGrid(pr, years = years)

pet <- petGrid(tasmin = tmin, tasmax = tmax, pr = pr, method = "hargreaves-samani")
pet <- redim(pet, drop = TRUE)

pr.MM <- aggregateGrid(pr, aggr.m = list(FUN = "sum", na.rm = TRUE))
pet.MM <- aggregateGrid(pet, aggr.m = list(FUN = "mean", na.rm = TRUE))

spei <- speiGrid(et0.grid = pet.MM, pr.grid = pr.MM, scale = 6, ref.start = c(min(years),1), ref.end=c(max(years),12), na.rm = TRUE)

spei.gb <- subsetGrid(spei, lonLim = c(-4.1), latLim = c(40.4))

tmax.MM <- aggregateGrid(tmax, aggr.m = list(FUN = "mean", na.rm = TRUE))
tmin.MM <- aggregateGrid(tmin, aggr.m = list(FUN = "mean", na.rm = TRUE))
pr.MM <- aggregateGrid(pr, aggr.m = list(FUN = "sum", na.rm = TRUE))

# SPEI for months
spei.month <- list()
for (i in 1:12){
    spei.month[[i]] <- subsetGrid(spei, season = c(i))
    names(spei.month)[i] <- as.character(i)
}

# Plot the trend maps for all the months
plot.list <- list()
months <- c("ENERO", "FEBRERO", "MARZO", "ABRIL", "MAYO", "JUNIO", "JULIO", "AGOSTO", "SEPTIEMBRE", "OCTUBRE", "NOVIEMBRE", "DICIEMBRE")
UNITS <- "ºC / año"

for (i in 1:12){
    name.month <- months[i]
    MAIN.TITLE <- name.month
    sub.spei <- spei.month[[as.character(i)]]
    
    trend <- valueIndex(sub.spei, index.code = "Trend", return.NApercentage = FALSE)
    sig <- valueIndex(sub.spei, index.code = "TrendSig", return.NApercentage = FALSE)
    clim <- climatology(sig)
    points <- visualizeR::map.stippling(clim, thereshold = 0.05, condition = "LT", pch = 19, cex = 0.01, col = "black")

    plot <- spatialPlot(trend, 
            backdrop.theme = "coastline",
            color.theme = "RdBu",
            rev.colors=TRUE, 
            at = seq(-0.1, 0.1, 0.01), set.max = 0.06, set.min = -0.06,
            main = list(MAIN.TITLE, cex = 0.75),
            colorkey = list(space = "bottom", height = 0.8, width = 0.75,
                            labels = list(cex = 0.6),
                            title = list(UNITS, cex = 0.75)),
            sp.layout=list(points)
            )

    plot.list[[name.month]] <- plot
}

library(gridExtra)
library(grid)


pdf(paste0("section4.1/trends.tmax.obs.", min(years), "-", max(years), ".pdf"), width = 9, height = 12)

# Create a grob (graphical object) for the main title
main_title <- textGrob(paste0("TENDENCIAS TMAX OBSERVACIONES", min(years), "-", max(years)), gp=gpar(cex=1.5))

# Arrange the plots in a grid and add the main title
grid.arrange(
    plot.list$ENERO, plot.list$FEBRERO, plot.list$MARZO, plot.list$ABRIL, plot.list$MAYO, plot.list$JUNIO, 
    plot.list$JULIO, plot.list$AGOSTO, plot.list$SEPTIEMBRE, plot.list$OCTUBRE, plot.list$NOVIEMBRE, plot.list$DICIEMBRE,
    ncol = 3,
    top = main_title
)
dev.off()
