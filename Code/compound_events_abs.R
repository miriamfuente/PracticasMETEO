############################################################################
#### CHARACTERIZATION OF COMPOUND EXTREME EVENTS FOR ABSOLUTE THERESOLDS ####
#############################################################################

# Libraries necesaries for the script
options(java.parameters = "-Xmx32g")
library(loadeR)
library(transformeR)
library(visualizeR)
library(climate4R.UDG)
library(dplyr)
library(climate4R.value)
library(VALUE)
library(gridExtra)
library(grid)

# Define the variables to process the data
pr <- readRDS("/lustre/gmeteo/WORK/fuentesm/TFM/section4.1/pr.obs.DD.1971-2020.rds")
tmax <- readRDS("/lustre/gmeteo/WORK/fuentesm/TFM/section4.1/tasmax.obs.DD.1971-2020.rds")
years <- 1971:1996

pr <- subsetGrid(pr, years =years)
tmax <- subsetGrid(tmax, years = years)
pr_abs <- 0.1 # mm/dia
tmax_abs <- 30 # ºC
seasons <- list(Winter = c(12, 1, 2), Spring = c(3, 4, 5), Summer = c(6, 7, 8), Autumn = c(9, 10, 11))
earth.layer <- climatology(tmax)
earth.layer$Data[!is.na(earth.layer$Data)] <- 1

processed_data <- function(data){
    # Function to obtain the data by season and year
    # data: a grid with the data
    new_data <- lapply(as.character(years), function(y) {
        new_data_year <- list()
        for (season_name in names(seasons)) {
            season_months <- seasons[[season_name]]
            
            if (season_name == "Winter") {
                # Winter, include December of the previous year
                if (as.numeric(y) != min(years)) {
                    previous_dec <- subsetGrid(data, year = as.numeric(y) - 1, season = c(12))
                    current_jan_feb <- subsetGrid(data, year = as.numeric(y), season = c(1, 2))
                    winter_data <- bindGrid(previous_dec, current_jan_feb, dimension = "time")
                    new_data_year[[season_name]] <- winter_data
                } else {
                    current_jan_feb <- subsetGrid(data, year = as.numeric(y), season = c(1, 2))
                    new_data_year[[season_name]] <- current_jan_feb
                }
            } else {
                # Rest of the seasons
                data_season_year <- subsetGrid(data, year = as.numeric(y), season = season_months)
                new_data_year[[season_name]] <- data_season_year
            }
        }
        return(new_data_year)
    })
    names(new_data) <- as.character(years)
    return(new_data)
}

pr <- processed_data(pr)
tmax <- processed_data(tmax)

binarization <- function(grid_data, pr, tmax, variable) {
    # Iterate over the years in the data grid
    for (year in names(grid_data)) {
        # Iterate over the seasons of each year
        for (season in names(grid_data[[year]])) {
        # Get current station data
        season_data <- grid_data[[year]][[season]]
        # Dimension of the data
        dims <- dim(season_data$Data)
        # Iterate over dimensions and convert to binary
            for (a in 1:dims[1]) {
                for (i in 1:dims[2]) {
                for (j in 1:dims[3]) {
                    if (!is.na(season_data$Data[a, i, j])) {
                    # If it is precipitation
                    if(variable == "pr"){
                        if (season_data$Data[a, i, j] <= pr) {
                        season_data$Data[a, i, j] <- 1
                        } else {
                        season_data$Data[a, i, j] <- 0
                        }
                    # If it is tmax
                    } else if (variable == "tmax"){
                        if (season_data$Data[a, i, j] >= tmax) {
                        season_data$Data[a, i, j] <- 1
                        } else {
                        season_data$Data[a, i, j] <- 0
                        }
                    }
                    }
                }
                }
            }
        # Update the data grid with the binarized station
        grid_data[[year]][[season]] <- season_data
        }
    }
    # Return the binarized data grid
    return(grid_data)
}

pr_bin <- binarization(pr, pr_abs, tmax_abs, "pr")
tmax_bin <- binarization(tmax, pr_abs, tmax_abs ,"tmax")

# Create a function that iter over the years and seasons of two datagrids and execute the function gridArithmetics
compound_event <- function(grid1, grid2) {
    grid3 <- grid1
    # Iterate over the years in the grid
    for (year in names(grid1)) {
        # Iterate over the seasons in each year
        for (season in names(grid1[[year]])) {
        # Get the data of the current season
        season_data1 <- grid1[[year]][[season]]
        season_data2 <- grid2[[year]][[season]]
        # Apply the function to the data
        event_compount <- gridArithmetics(season_data1, season_data2, operator = "*")
        # Update the data grid with the result
        grid3[[year]][[season]] <- event_compount
        }
    }
    # Return the updated grid
    return(grid3)
}

CE <- compound_event(pr_bin, tmax_bin)  


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

# Name of the seasons
season_names <- c("Winter", "Spring", "Summer", "Autumn")
# List to store the results
num.CE.period <- list()
# Iterate over the seasons
for (season in season_names) {
    # Obtain the data for the current season
    data_season <- bindGrid_stations(CE, season = season)
    data_season <- climatology(data_season, clim.fun = list(FUN = "sum", na.rm = TRUE))
    print("Done")
    data_season <- gridArithmetics(data_season, earth.layer, operator = "*")
    # Almacenar los resultados en la lista
    num.CE.period[[season]] <- data_season
}

pdf(paste0("num.CE.season", years, ".pdf"), width=9, height=12)
spatialPlot(makeMultiGrid(num.CE.period$Winter, num.CE.period$Spring, num.CE.period$Summer, num.CE.period$Autumn, skip.temporal.check=TRUE), 
            backdrop.theme = "coastline", as.table=TRUE, names.attr=c("Invierno", "Primavera", "Verano", "Otoño"),
            color.theme="RdBu", rev.colors=TRUE, main="Eventos Extremos Compuestos por estación (1971-1996)",
            layout=c(2,2),
            colorkey = list(space = "bottom",
                title = list("Nº de días", cex = 1)))
dev.off()

##########
# TRENDS #
##########
winter <- bindGrid_stations(CE, season = "Winter")
spring <- bindGrid_stations(CE, season = "Spring")
summer <- bindGrid_stations(CE, season = "Summer")
autumn <- bindGrid_stations(CE, season = "Autumn")

trend.winter <- valueIndex(winter, index.code = "Trend", return.NApercentage = FALSE)# La pendiente
sig.winter <- valueIndex(winter, index.code = "TrendSig", return.NApercentage = FALSE) # La significancia (1 o 0)
clim.w = climatology(sig.winter)
points.winter <- visualizeR::map.stippling(clim.w, thereshold = 0.05, condition = "LT", pch = 19, cex = 0.05, col = "black")
plot.winter <- spatialPlot(trend.winter, backdrop.theme = "coastline", color.theme = "RdBu", at = seq(-0.1, 0.1, by = 0.01), 
                set.max = 0.1, set.min = -0.1, rev.colors = TRUE, main = "Invierno", sp.layout = list(points.winter),
                colorkey=list(space = "bottom", title = list("Nº de días/año", cex = 1)))

trend.spring <- valueIndex(spring, index.code = "Trend", return.NApercentage = FALSE)# La pendiente
sig.spring <- valueIndex(spring, index.code = "TrendSig", return.NApercentage = FALSE) # La significancia (1 o 0)
clim.spring = climatology(sig.spring)
points.spring <- visualizeR::map.stippling(clim.spring, thereshold = 0.05, condition = "LT", pch = 19, cex = 0.05, col = "black")
plot.spring <- spatialPlot(trend.spring, backdrop.theme = "coastline", color.theme = "RdBu", at = seq(-0.1, 0.1, by = 0.01),
                 set.max = 0.1, set.min = -0.1, rev.colors = TRUE, main = "Primavera", sp.layout = list(points.spring),
                 colorkey=list(space = "bottom", title = list("Nº de días/año", cex = 1)))

trend.summer <- valueIndex(summer, index.code = "Trend", return.NApercentage = FALSE)# La pendiente
sig.summer <- valueIndex(summer, index.code = "TrendSig", return.NApercentage = FALSE) # La significancia (1 o 0)
clim.summer = climatology(sig.summer)
points.summer <- visualizeR::map.stippling(clim.summer, thereshold = 0.05, condition = "LT", pch = 19, cex = 0.05, col = "black")
plot.summer <- spatialPlot(trend.summer, backdrop.theme = "coastline", color.theme = "RdBu", at = seq(-0.1, 0.1, by = 0.01),
                 set.max = 0.1, set.min = -0.1, rev.colors = TRUE, main = "Verano", sp.layout = list(points.summer),
                 colorkey=list(space = "bottom", title = list("Nº de días/año", cex = 1)))

trend.autumn <- valueIndex(autumn, index.code = "Trend", return.NApercentage = FALSE)# La pendiente
sig.autumn <- valueIndex(autumn, index.code = "TrendSig", return.NApercentage = FALSE) # La significancia (1 o 0)
clim.autumn = climatology(sig.autumn)
points.autumn <- visualizeR::map.stippling(clim.autumn, thereshold = 0.05, condition = "LT", pch = 19, cex = 0.05, col = "black")
plot.autumn <- spatialPlot(trend.autumn, backdrop.theme = "coastline", color.theme = "RdBu", at = seq(-0.1, 0.1, by = 0.01),
                 set.max = 0.1, set.min = -0.1, rev.colors = TRUE, main = "Otoño", sp.layout = list(points.autumn),
                 colorkey=list(space = "bottom", title = list("Nº de días/año", cex = 1)))

# Create the PDF file
pdf(paste0("trend.CE.season",years,".pdf"), width=9, height=8)
main.title <- textGrob("Tendencias de los eventos extremos compuestos por estación (1971-1996)",
                        gp=gpar(cex=1.5, fontface="bold"))
grid.arrange(plot.winter, plot.spring, plot.summer, plot.autumn,
            ncol = 2, top = main.title)
dev.off()
