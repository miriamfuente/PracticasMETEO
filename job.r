options(java.parameters = "-Xmx20g") 
library(loadeR)
library(transformeR)
library(visualizeR)
library(climate4R.UDG)
library(dplyr)
library(climate4R.value)
library(VALUE)

# Leer el archivo CSV
df <- read.csv("https://data.meteo.unican.es/inventory.csv") # inventario con muchos datasets
# Filtrar los enlaces que necesitas
sub <- subset(df, product == 'AEMET-5KM-regular')
obs.url <- as.character(sub$location)
# Ejecutar dataInventory con los enlaces filtrados
di <- dataInventory(obs.url)


pr <- loadGridData(obs.url, var = "pr")
tmax <- loadGridData(obs.url, var = "tasmax")

pr <- upscaleGrid(pr, times = 2, aggr.fun = list(FUN = mean, na.rm = TRUE))
tmax <- upscaleGrid(tmax, times = 2, aggr.fun = list(FUN = mean, na.rm = TRUE))

pr_h <- pr

# Parte Humeda Precipitacion
dimension <- dim(pr_h$Data)
for (a in 1:dimension[3]) {
  for (i in 1:dimension[4]) {
    for (j in 1:dimension[5]) {
      if (!is.na(pr_h$Data[1,1,a,i,j])) { # Check for missing values
          if (pr_h$Data[1,1,a,i,j] <= 0.1) {
              pr_h$Data[1,1,a,i,j] <- NaN
            }
          }
        }
      }
  }

seasons <- list(Winter = c(12, 1, 2), Spring = c(3, 4, 5), Summer = c(6, 7, 8), Autumn = c(9, 10, 11))
# Function to apply climatology to a dataset for a specific season and quantile level
ref_quantile <- function(data, quantile) {
    # data: a grid with the data
    # quantile: number of the quantile to calculate
  results <- list()
  for (season_name in names(seasons)) {
    season_month <- seasons[[season_name]]
    season_data <- subsetGrid(data, season = season_month)
    quantile_result <- climatology(season_data, clim.fun = list(FUN = "quantile", probs = quantile, na.rm = TRUE))
    results[[season_name]] <- quantile_result
  }
  return(results)
}

pr_h <- ref_quantile(pr_h, 0.10)
tmax_h <- ref_quantile(tmax, 0.90)

# Define the seasons and years to process
seasons <- list(Winter = c(12, 1, 2), Spring = c(3, 4, 5), Summer = c(6, 7, 8), Autumn = c(9, 10, 11))
years <- seq(1951, 2022, by = 1)

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
                    current_en_feb <- subsetGrid(data, year = as.numeric(y), season = c(1, 2))
                    winter_data <- bindGrid(previous_dec, current_en_feb, dimension = "time")
                    new_data_year[[season_name]] <- winter_data
                } else {
                    current_en_feb <- subsetGrid(data, year = as.numeric(y), season = c(1, 2))
                    new_data_year[[season_name]] <- current_en_feb
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

binarization <- function(grid_data, quantile, variable) {
# Iterate over the years in the data grid
  for (year in names(grid_data)) {
    # Iterate over the seasons of each year
    for (season in names(grid_data[[year]])) {
      # Get current station data
      season_data <- grid_data[[year]][[season]]
      season_quantile <- quantile[[season]]
      
      # Dimension of the data
      dims <- dim(season_data$Data)
      # Iterate over dimensions and convert to binary
      for (a in 1:dims[1]) {
        for (i in 1:dims[2]) {
          for (j in 1:dims[3]) {
            if (!is.na(season_data$Data[a, i, j]) && !is.na(season_quantile$Data[1, i, j])) {
              # If it is precipitation
              if(variable == "pr"){
                if (season_data$Data[a, i, j] < season_quantile$Data[1, i, j]) {
                  season_data$Data[a, i, j] <- 1
                } else {
                  season_data$Data[a, i, j] <- 0
                }
              # If it is tmax
              } else if (variable == "tmax"){
                if (season_data$Data[a, i, j] > season_quantile$Data[1, i, j]) {
                  season_data$Data[a, i, j] <- 1
                } else {
                  season_data$Data[a, i, j] <- 0
                }
              }
            }
          }
        }
      }
      
      # Actualizar el grid de datos con la estación binarizada
      grid_data[[year]][[season]] <- season_data
    }
  }
  
  # Devolver el grid de datos binarizado
  return(grid_data)
}

pr_bin <- binarization(pr, pr_h, "pr")
tmax_bin <- binarization(tmax, tmax_h, "tmax")

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

capa_tierra <- tmax_h$Spring
capa_tierra$Data[!is.na(capa_tierra$Data)] <- 1

# Create a function that iter over the years and seasons of one data grid and execute the function climatology to summarize the data
sum_events <- function(grid) {
  grid1 <- grid
  # Iterate over the years in the grid
  for (year in names(grid)) {
    # Iterate over the seasons in each year
    for (season in names(grid[[year]])) {
      # Get the data of the current season
      season_data1 <- grid[[year]][[season]]
      # Apply the function to the data
      sum_events <- climatology(season_data1, clim.fun = list(FUN = "sum", na.rm = TRUE))
      sum_events <- gridArithmetics(sum_events, capa_tierra, operator = "*")
      # Update the data grid with the result
      grid1[[year]][[season]] <- sum_events
    }
  }
  # Return the updated grid
  return(grid1)
}

sumCE <- sum_events(CE)

# Función para realizar bindGrid de una estación específica para varios años
bindGridEstaciones <- function(datos, estacion) {
  # Extraemos los años disponibles en los datos
  years <- names(datos)
  
  # Creamos una lista para almacenar los grids de cada año
  lista_grids <- list()
  
  # Iteramos sobre los años
  for (year in years) {
    # Obtenemos el grid para la estación especificada
    grid <- datos[[year]][[estacion]]
    # Lo añadimos a la lista
    lista_grids[[year]] <- grid
  }
  
  # Realizamos el bindGrid con todos los grids de la lista
  resultado <- bindGrid(lista_grids, dimension = "time", skip.temporal.check = TRUE)
  return(resultado)
}

# Crear un vector con los nombres de las estaciones
estaciones <- c("Winter", "Spring", "Summer", "Autumn")

# Crear una lista para almacenar los resultados
dataPeriod <- list()

# Iterar sobre las estaciones
for (estacion in estaciones) {
  # Obtener los datos para la estación actual
  datos_estacion <- bindGridEstaciones(sumCE, estacion = estacion)
  datos_estacion <- climatology(datos_estacion, clim.fun = list(FUN = "sum", na.rm = TRUE))
  datos_estacion <- gridArithmetics(datos_estacion, capa_tierra, operator = "*")
  
  # Almacenar los resultados en la lista
  dataPeriod[[estacion]] <- datos_estacion
}

fullPeriod <- bindGrid(dataPeriod$Winter , dataPeriod$Spring, dataPeriod$Summer, dataPeriod$Autumn, dimension = "member", skip.temporal.check = TRUE)
png("CE_AEMET.png", width = 800, height = 600)  # Guarda el gráfico como un archivo PNG
spatialPlot(fullPeriod, color.theme = "RdBu", rev.colors = TRUE, as.table = TRUE, names.attr = c("Winter", "Spring", "Summer", "Autumn"), backdrop.theme = "countries", main = "Sum Compound Event 1951-2022")
dev.off()  # Finaliza la creación del archivo PNG

winter <- bindGridEstaciones(sumCE, "Winter")
spring <- bindGridEstaciones(sumCE, "Spring")
summer <- bindGridEstaciones(sumCE, "Summer")
autumn <- bindGridEstaciones(sumCE, "Autumn")

trend.winter <- valueIndex(winter, index.code = "Trend", return.NApercentage = FALSE)# La pendiente
sig.winter <- valueIndex(winter, index.code = "TrendSig", return.NApercentage = FALSE) # La significancia (1 o 0)
# Si significancia es 0 entonces la pendiente no es significativa = NA
clim.w = climatology(sig.winter)
points.winter <- visualizeR::map.stippling(clim.w, thereshold = 0.05, condition = "LT", pch = 19, cex = 0.25, col = "black")
# Representación
png("trend_winter.png", width = 800, height = 600)  # Guarda el gráfico como un archivo PNG
spatialPlot(trend.winter, backdrop.theme = "coastline", color.theme = "RdBu", at = seq(-0.7, 0.7, by = 0.1), set.max = 0.7, set.min = -0.7, rev.colors = TRUE, main = "Winter AEMET 1951-2022 / p 10_h tmax 90", sp.layout = list(points.winter))
dev.off()  # Finaliza la creación del archivo PNG

trend.spring <- valueIndex(spring, index.code = "Trend", return.NApercentage = FALSE)# La pendiente
sig.spring <- valueIndex(spring, index.code = "TrendSig", return.NApercentage = FALSE) # La significancia (1 o 0)
# Si significancia es 0 entonces la pendiente no es significativa = NA
clim.spring = climatology(sig.spring)
points.spring <- visualizeR::map.stippling(clim.spring, thereshold = 0.05, condition = "LT", pch = 19, cex = 0.25, col = "black")
# Representación
png("trend_spring.png", width = 800, height = 600)  # Guarda el gráfico como un archivo PNG
spatialPlot(trend.spring, backdrop.theme = "coastline", color.theme = "RdBu", at = seq(-0.7, 0.7, by = 0.1), set.max = 0.7, set.min = -0.7, rev.colors = TRUE, main = "Spring AEMET 1951-2022 / p 10_h tmax 90", sp.layout = list(points.spring))
dev.off()  # Finaliza la creación del archivo PNG

trend.summer <- valueIndex(summer, index.code = "Trend", return.NApercentage = FALSE)# La pendiente
sig.summer <- valueIndex(summer, index.code = "TrendSig", return.NApercentage = FALSE) # La significancia (1 o 0)
# Si significancia es 0 entonces la pendiente no es significativa = NA
clim.summer = climatology(sig.summer)
points.summer <- visualizeR::map.stippling(clim.summer, thereshold = 0.05, condition = "LT", pch = 19, cex = 0.25, col = "black")
# Representación
png("trend_summer.png", width = 800, height = 600)  # Guarda el gráfico como un archivo PNG
spatialPlot(trend.summer, backdrop.theme = "coastline", color.theme = "RdBu", at = seq(-0.7, 0.7, by = 0.1), set.max = 0.7, set.min = -0.7, rev.colors = TRUE, main = "Summer AEMET 1951-2022 / p 10_h tmax 90", sp.layout = list(points.summer))
dev.off()  # Finaliza la creación del archivo PNG

trend.autumn <- valueIndex(autumn, index.code = "Trend", return.NApercentage = FALSE)# La pendiente
sig.autumn <- valueIndex(autumn, index.code = "TrendSig", return.NApercentage = FALSE) # La significancia (1 o 0)
# Si significancia es 0 entonces la pendiente no es significativa = NA
clim.autumn = climatology(sig.autumn)
points.autumn <- visualizeR::map.stippling(clim.autumn, thereshold = 0.05, condition = "LT", pch = 19, cex = 0.25, col = "black")
# Representación
png("trend_autumn.png", width = 800, height = 600)  # Guarda el gráfico como un archivo PNG
spatialPlot(trend.autumn, backdrop.theme = "coastline", color.theme = "RdBu", at = seq(-0.7, 0.7, by = 0.1), set.max = 0.7, set.min = -0.7, rev.colors = TRUE, main = "Autumn AEMET 1951-2022 / p 10_h tmax 90", sp.layout = list(points.autumn))
dev.off()  # Finaliza la creación del archivo PNG

trends <- bindGrid(trend.winter, trend.spring, trend.summer, trend.autumn, dimension = "member", skip.temporal.check = TRUE)
# points <- bindGrid(points.winter, points.spring, points.summer, points.autumn, dimension = "member", skip.temporal.check = TRUE)
png("trends.png", width = 800, height = 600)  # Guarda el gráfico como un archivo PNG
spatialPlot(trends, backdrop.theme = "coastline", names.attr = c("Winter", "Spring", "Summer", "Autumn"), main = "Trend 1984 - 2005", as.table = TRUE, color.theme = "RdBu", at = seq(-0.7, 0.7, by = 0.1), set.max = 0.7, set.min = -0.7, rev.colors = TRUE)
dev.off()  # Finaliza la creación del archivo PNG