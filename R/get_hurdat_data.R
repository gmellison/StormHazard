# Prepare Storm Hazard Dataset  ###

require(rvest)
require(dplyr)
require(stringr)
require(lubridate)

# This script downloads and preps the Hurdat2 data file
# For details of these datasets, see the data_readme.

data_dir <- "data/"
hurdat_file <- paste0(data_dir,"landfalls.csv")

recook <- TRUE 

#### Get the Hurdat2 data:
### data schema

if (file.exists(hurdat_file) & !recook) {
        hurdat_data <- read.csv(hurdat_file)
} else {
  schema <- c(
              "date",
              "hours",
              "id",        ## id indicates intensity peak, landfall, etc
              "status",    ## status indicates hurricane, depression, cyclone
              "lat",
              "lon",
              "windspeed_max",
              "pressure_min",
              "wind_34_ne",
              "wind_34_se",
              "wind_34_sw",
              "wind_34_nw",
              "wind_50_ne",
              "wind_50_se",
              "wind_50_sw",
              "wind_50_nw",
              "wind_64_ne",
              "wind_64_se",
              "wind_64_sw",
              "wind_64_nw")
  # read hurdat data from the noaa website
  data <- read.table("https://www.nhc.noaa.gov/data/hurdat/hurdat2-1851-2020-020922.txt",
                     col.names=schema,
                     fill=TRUE)
  # get rid of commas
  data <- data.frame(apply(data,2,
                           function(x) stringr::str_remove(x, ",")), stringsAsFactors = FALSE)
  
  ### pull out all the rows that contain "id", "name", "nrow" 
  #       they start the blocks of data for each storm, 
  #       so use them to prep the data.
  
  info_rows <- data[stringr::str_detect(data$date, "^A"),1:3]
  names(info_rows) <- c("h_id", "h_name", "n_rows")
  info_rows$n_rows <- as.numeric(info_rows$n_rows)
  info_rows$h_name <- as.character(info_rows$h_name)
  info_rows$h_id <- as.character(info_rows$h_id)
  
  # init a list to store all the hurricane data in
  h_list <- vector(nrow(info_rows), mode="list")
  names(h_list) <- info_rows$h_id 
  
  # loop through all the info rows
  for (i in 1:nrow(info_rows)) {
    # get the indices for each storm:
    #   from start = info row + 1 to end = start + (nrow-1)
    i_start <- which(data$date == info_rows$h_id[i]) + 1
    i_end <- i_start + info_rows$n_rows[i]-1
    
    # grab the rows, and add cols for name and id
    h_df <- data[i_start:i_end, ]
    h_df$h_name <- info_rows$h_name[i]
    h_df$h_id <- info_rows$h_id[i] 
   
    # store it in the proper place in the list (indexed on hurricane id -- name not unique) 
    h_list[[info_rows$h_id[i]]] <- h_df
  }
  
  # squish all the list entries into one dataframe and save
  hurdat_data <- bind_rows(h_list)
  hurdat_data$date <- ymd(hurdat_data$date)
  hurdat_data <- filter(hurdat_data,year(date) >= 1980)

  # make lat/lon numeric
  lat_ns <- stringr::str_extract(hurdat_data$lat, "[NSEW]")
  lon_ew <- stringr::str_extract(hurdat_data$lon, "[NSEW]")
 
  hurdat_data$lat <- as.numeric(stringr::str_extract(hurdat_data$lat, "[0-9.]*"))
  hurdat_data$lon <- as.numeric(stringr::str_extract(hurdat_data$lon, "[0-9.]*"))
  
  hurdat_data$lat <- ifelse(lat_ns == "S", -1, 1) * hurdat_data$lat
  hurdat_data$lon <- ifelse(lon_ew == "W", -1, 1) * hurdat_data$lon

  # landfalls only
  hurdat_data <- hurdat_data[hurdat_data$id=="L",]
  write.csv(hurdat_data,  file=hurdat_file)

  # just write a csv of datetime/lat/lon for landfalls:
  landfall <- select(hurdat_data, date, hours, lat, lon)
  landfall_time <- parse_date_time(landfall$hours, "HM")
  landfall$datetime <- parse_date_time(paste0(landfall$date, landfall$hours, sep=" "),"Y-m-d HM")
  write.csv(select(landfall, datetime, lat, lon), "landfalls.csv", row.names=FALSE)
}

## Clean Hurdat
# group to one line per storm
# compute radius 
# keep only name, year, pressure, windspeed, radius columns
wind_50_cols = which(str_starts(names(hurdat_data), "wind_50_"))
hurdat_data$radius_50kt = apply(hurdat_data[,wind_50_cols],1,max)

h_data <- hurdat_data %>%
  mutate(name = str_to_upper(h_name)) %>%
  mutate(year = year(date)) %>%
  group_by(name, year) %>%
  summarise(pressure = min(pressure_min, na.rm=TRUE),
            windspeed = max(windspeed_max, na.rm = TRUE),
            radius = max(radius_50kt, na.rm=TRUE),
            lat_min=min(lat,na.rm=TRUE),lat_max=max(lat,na.rm=TRUE),
            lon_min=min(lon,na.rm=TRUE),lon_max=max(lon,na.rm=TRUE),
            date_min=min(date,na.rm=TRUE),date_max=max(date,na.rm=TRUE))

write.csv(file=hurdat_file, h_data)

# now write a temp file to be used by the get_nldas.py script
h_data$precip <- NA
write.csv(file="data/hurdat_temp.csv", h_data)

