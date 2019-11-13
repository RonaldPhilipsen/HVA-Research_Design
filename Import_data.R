source("Tools.R")
required_packages <- c("tsibble", "magrittr", "dplyr", "jsonlite", "lubridate", "rmarkdown")
Require.packages(required_packages)

if (!file.exists("./Data/climate_data.Rda")) {
  climate.files <- list.files(path = "./Data", full.names = TRUE, pattern = ".csv")
  climate.list <- lapply(climate.files, function(i) read.csv(i))
  climate.colnames <- unique(unlist(lapply(climate.list, names)))

  climate.data.hourly <- do.call(rbind,
            lapply(climate.list,
               function(x) data.frame(c(x, sapply(setdiff(climate.colnames, names(x)),
  function(y) NA)))))

  climate.data.hourly %<>% filter(., station == 28079024)
  climate.data.hourly %<>% subset(select = -c(PM25, NO, CH4))

  saveRDS(climate.data.hourly, file = "./Data/climate_data.Rda")
}

climate.data.hourly <- readRDS(file = "./Data/climate_data.Rda")
names(climate.data.hourly) <- c("Hour", "BEN", "CO", "EBE", "MXY", "NMHC", "NO_2", "NOx",
                                "OXY", "O_3", "PM10", "PXY", "SO_2", "TCH", "TOL", "station"
                                )
climate.data.hourly %<>% transform(Hour = as.character(Hour))
climate.data.hourly %<>% transform(Hour = parse_date_time(Hour, "ymd HMS"))
climate.data.hourly %<>% as_tsibble(key = station, index = Hour)
climate.data.daily <- index_by(climate.data.hourly, date = as.Date(Hour)) %>%
summarise(
      BEN = mean(BEN),
      CO = mean(CO),
      EBE = mean(EBE),
      MXY = mean(MXY),
      NMHC = mean(NMHC),
      NO_2 = mean(NO_2),
      NOx = mean(NOx),
      OXY = mean(OXY),
      O_3 = mean(O_3),
      PM10 = mean(PM10),
      PXY = mean(PXY),
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
    weather.data$prec <- as.character(weather.data$prec)
    weather.data[weather.data == "Ip"] <- "0,0"
    #Write this to CSV for easier access later
    write.csv(weather.data, file = "./Data/weather_data.cvv", row.names = FALSE)
  }

  weather.data <- read.csv(file = "./Data/weather_data.cvv", dec = ",", stringsAsFactors = FALSE)
  weather.data[weather.data == "Varias"] <- NA

  weather.data %<>%
  subset(select = -c(minTempTime, maxTempTime, maxPresTime, maxGustTime, minPresTime)) %>%
    transform(date = as.character(date)) %>%
    transform(date = ymd(date)) %>%
    as_tsibble(key = station, index = date)

  saveRDS(weather.data, file = "./Data/weather_data.Rda")
}

weather.data <- readRDS(file = "./Data/weather_data.Rda")
merged <- left_join(climate.data.daily, weather.data, by = c("date"))
merged %<>% subset(select = -c(station))
merged$date = format(merged$date, "%Y/%m/%d")

saveRDS(merged, file = "./Data/merged.Rda")
write.csv(merged, file = "./merged.csv", row.names = FALSE)