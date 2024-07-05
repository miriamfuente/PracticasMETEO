############################################################################
#### CHARACTERIZATION OF COMPOUND EXTREME EVENTS FOR ABSOLUTE THERESOLDS ####
#############################################################################

# Libraries necesaries for the script
options(java.parameters = "-Xmx32g")
library(loadeR)
library(transformeR)

# Define the variables to process the data
pr <- readRDS("/oceano/gmeteo/users/fuentesm/AEMET/TFM/section4.1/pr.obs.DD.1982-2021.rds")
tmax <- readRDS("/oceano/gmeteo/users/fuentesm/AEMET/TFM/section4.1/tasmax.obs.DD.1982-2021.rds")
quantile_pr <- 0.90
quantile_tmax <- 0.10
years <- 1982:2021
seasons <- list(Winter = c(12, 1, 2), Spring = c(3, 4, 5), Summer = c(6, 7, 8), Autumn = c(9, 10, 11))

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

pr_bin <- binarization(pr, quantile_pr, quantile_tmax, "pr")
tmax_bin <- binarization(tmax, quantile_pr, quantile_tmax ,"tmax")

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

earth.layer <- climatology(tmax)
earth.layer$Data[!is.na(earth.layer$Data)] <- 1

# Name of the seasons
season_names <- c("Winter", "Spring", "Summer", "Autumn")
# List to store the results
dataPeriod <- list()
# Iterate over the seasons
for (season in season_names) {
    # Obtain the data for the current season
    data_season <- bindGrid_stations(CE, season = season)
    data_season <- climatology(data_season, clim.fun = list(FUN = "sum", na.rm = TRUE))
    print("Done")
    data_season <- gridArithmetics(data_season, earth.layer, operator = "*")
    # Almacenar los resultados en la lista
    dataPeriod[[estacion]] <- data_season
}

