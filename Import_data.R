source("Tools.R")
required_packages <- c("tsibble", "magrittr", "dplyr", "jsonlite", "lubridate")
Require.packages(required_packages)

if (!file.exists("./Data/climate_data.Rda")) {
  climate.files <- list.files(path = "./Data", full.names = TRUE, pattern = ".csv")
  climate.list <- lapply(climate.files, function(i) read.csv(i))
  climate.colnames <- unique(unlist(lapply(climate.list, names)))

  climate.data <- do.call(rbind,
            lapply(climate.list,
               function(x) data.frame(c(x, sapply(setdiff(climate.colnames, names(x)),
  function(y) NA)))))

  climate.data %<>% filter(., station == 28079035)
  climate.data %<>% transform(date = as.character(date))
  climate.data %<>% transform(date = parse_date_time(date, "ymd HMS"))
  climate.data %<>% as_tsibble(key = station, index = date)

  saveRDS(climate.data, file = "./Data/climate_data.Rda")
}

climate.data.hourly <- readRDS(file = "./Data/climate_data.Rda")
climate.data <- climate.data.hourly %>%
    index_by(ymd = ymd(date)) %>%
    summarise(
#TODO: do this
    )


if (!file.exists("./Data/weather_data.Rda")) {
  if (!file.exists("./Data/weather_data.csv")) {
    weather.files <- list.files(path = "./Data", full.names = TRUE, pattern = ".json")
    weather.list <- lapply(weather.files, function(i) as_tibble(fromJSON(i, flatten = TRUE)))
    weather.colnames <- unique(unlist(lapply(weather.list, names)))

    weather.data <- do.call(rbind,
                lapply(weather.list,
                           function(x) data.frame(c(x, sapply(setdiff(weather.colnames, names(x)),
                                                  function(y) NA)))))

    weather.data %<>% subset(select = -c(nombre, provincia, altitud))
    names(weather.data) <- c("date", "station", "medTemp", "prec", "minTemp", "minTempTime", "maxTemp", "maxTempTime", "windDir", "avgWindSpeed", "maxGustSpeed", "maxGustTime", "sun", "maxPres", "maxPresTime", "minPres", "minPresTime")

    #Write this to CSV for easier access later
    write.csv(weather.data, file = "./Data/weather_data.csv")
  }

  weather.data <- read.csv(file = "./Data/weather_data.csv", dec = ",", stringsAsFactors = FALSE)
  weather.data[weather.data == "Varias"] <- NA
  weather.data[weather.data == "Ip"] <- 0
  weather.data %<>%
  subset(select = -c(X)) %>%
    transform(date = as.character(date)) %>%
    transform(date = ymd(date)) %>%
    as_tsibble(key = station, index = date)

  saveRDS(weather.data, file = "./Data/weather_data.Rda")
}

weather.data <- readRDS(file = "./Data/weather_data.Rda")
