### Prepare Storm Hazard Dataset  ###
# Author: Gregory Ellison, University of Georgia Department of Statistics
# Contact: gme88782@uga.edu
# Date: 03/11/2022

require(rvest)
require(dplyr)
require(stringr)
require(lubridate)

# This script cleans and combines the Hurdat2, Tropical Storm Rainfall Point Maxima, and Surgedat datasets.
# For details of these datasets, see the data_readme.

data_dir <- "data/"
hurdat_file <- paste0(data_dir,"data_hurdat.csv")
rainfall_file <- paste0(data_dir,"data_rainfall.csv")
tornado_file <- paste0(data_dir,"data_tornado.csv")
surge_file <- paste0(data_dir,"data_surge.csv")
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
}


## Clean Hurdat
# group to one line per storm
# compute radius 
# keep only name, year, pressure, windspeed, radius columns
wind_50_cols = which(str_starts(names(hurdat_data), "wind_50_"))
hurdat_data$radius_50kt = apply(hurdat_data[,wind_50_cols],1,max)

lat_ns <- stringr::str_extract(hurdat_data$lat, "[NSEW]")
lon_ew <- stringr::str_extract(hurdat_data$lon, "[NSEW]")

hurdat_data$lat <- as.numeric(stringr::str_extract(hurdat_data$lat, "[0-9.]*"))
hurdat_data$lon <- as.numeric(stringr::str_extract(hurdat_data$lon, "[0-9.]*"))

hurdat_data$lat <- ifelse(lat_ns == "S", -1, 1) * hurdat_data$lat
hurdat_data$lon <- ifelse(lon_ew == "W", -1, 1) * hurdat_data$lon

h_data <- hurdat_data %>%
  mutate(name = str_to_upper(h_name)) %>%
  mutate(year = year(date)) %>%
  mutate() %>% 
  group_by(name, year) %>%
  summarise(pressure = min(pressure_min, na.rm=TRUE),
            windspeed = max(windspeed_max, na.rm = TRUE),
            radius = max(radius_50kt, na.rm=TRUE),
            lat_min=min(lat,na.rm=TRUE),lat_max=max(lat,na.rm=TRUE),
            lon_min=min(lon,na.rm=TRUE),lon_max=max(lon,na.rm=TRUE),
            date_min=min(date,na.rm=TRUE),date_max=max(date,na.rm=TRUE))


write.csv(file=hurdat_file, h_data)


##############################################
#                                            #
#           Rainfall Point Maxima:           #
#                                            #
##############################################

# first check if the file already exists:
if (file.exists(rainfall_file) & !recook) {
        rainfall_data <- read.csv(rainfall_file)
} else {

  # read the html direct from the noaa website
  rainfall_max <- xml2::read_html("https://www.wpc.ncep.noaa.gov/tropical/rain/tcmaxima.html")
  rainfall_table <- rvest::html_table(rainfall_max)
  
  # get the table, store the names and delete the names row
  df <- rainfall_table[[1]]
  names(df) <- df[1, ]
  df <- df[-1, ]
  
  # loop through the table and clean the contents
  point_maxima <- lapply(1:nrow(df), function(i) {
  
    row <- df[i, ]
    name <- row$Name
    # Name contains both storm name and dates, extract them both:
    h_name <- str_to_lower(str_split_fixed(name, " ", 3)[1])
    h_name <- str_replace_all(h_name, "\n", "")
    
    h_date <- str_split_fixed(name, "[()]", 4)[2]
    h_month <- str_split_fixed(h_date, " ", 2)[1]
    h_year <- as.numeric(str_split_fixed(h_date, " ", 2)[2])
    
    # multiple ovservations are split by '\n'
    amounts <- unlist(stringr::str_split(row$Amount, "[\n]"))
    n_amounts <- length(amounts)
    locations <- stringr::str_split_fixed(row$Location, "[\n]", n_amounts)[1,]
    
    row_df <- data.frame(name = str_to_upper(h_name),
                         month = h_month,
                         year = h_year,
                         amount = as.numeric(amounts),
                         location = locations)
    row_df
  }) %>% bind_rows # take all the cleaned rows and squish them into a dataframe
  
  
  # get rid of unnamed storms and nas
  rainfall_data <- filter(point_maxima, name != "UNNAMED")
  rainfall_data <- na.omit(rainfall_data)
}


## Clean Rainfall
# group to one line per storm (by name/year)
# convert rainfall from in to mm
r_data <- rainfall_data %>% 
  group_by(name, year) %>% 
  summarise(amount = max(amount,na.rm=TRUE)) %>%
  mutate(rainfall_mm = amount * 25.4) %>%
  select(name,year,rainfall_mm)


write.csv(rainfall_data, file = r_file)

################################
#                              #
#           Surgedat           #
#                              #
################################

if (file.exists(surge_file) & !recook) {
        surge_data <- read.csv(surge_file)
} else {
        # online surge database doesn't contain full dataset. 
        # try to get it from paper pdf instead
        # surge_data <- read.csv("http://surge.climate.lsu.edu/files/gompeaksurgedb.csv")
        # read the tables from the 
        surge_tables <- tabulizer::extract_tables("data/Needham_Keim_IJOC_2012.pdf")
        surge_df_list <- lapply(surge_tables[3:7], function(tab) {
                       df <- data.frame(tab[-1,])
                       names(df) <- c("rank","surge_m","name","year","location","state","lat","lon","conf")
                       return(df)
        })
        surge_data <- bind_rows(surge_df_list)
        write.csv(surge_data, file=surge_file)
}

## Clean Surge
# group to max surge per storm
# keep only name, year, surge_m
s_data <- surge_data %>% 
        na.omit %>% 
        select(year,name,surge_m)%>% 
        group_by(name,year)%>% 
        summarise(surge=max(surge_m, na.rm=TRUE))%>% 
        mutate(year=as.numeric(year),
               name=str_to_upper(name))


# get surge reconstruction data from GSSR

# pull metadata:
surge_meta <- read.csv(
  "https://raw.githubusercontent.com/moinabyssinia/gssr/gh-pages/metadata/allMetaDataRMSEReanalysis.csv"
)

# filter out everything outside range of hurdat data
surge_meta <- surge_meta %>% 
  filter(lat_x > min(h_data$lat_min) & lat_x < max(h_data$lat_max) & 
         lon_x > min(h_data$lon_min) & lon_x < max(h_data$lon_max))

s_join <- h_data %>% 
        select(name,year,date_min,date_max,lat_min,lat_max,lon_min,lon_max)

storm_surge_stations <- lapply(1:nrow(surge_meta), function(i) {

  cat(sprintf("%s / %s \n", i, nrow(surge_meta)))

  lat <- surge_meta$lat[i]
  lon <- surge_meta$lon[i]

  storms_covered <- lapply(1:nrow(s_join), function(j) {
     if ( s_join$lat_min[j] < lat & s_join$lat_max[j] > lat & 
          s_join$lon_min[j] < lon & s_join$lon_max[j] > lon ) {

       data.frame(storm=s_join$name[j],
                  year=s_join$year[j],
                  start_time = s_join$date_min[j],
                  end_time =  s_join$date_max[j],
                  storm_lat_min = s_join$lat_min[j],
                  storm_lat_max = s_join$lat_max[j],
                  storm_lon_min = s_join$lon_min[j],
                  storm_lon_max = s_join$lon_max[j]
       )
     }
  }) %>% bind_rows

  storms_covered$station <- surge_meta$tg[i]
  storms_covered$station_lat <- surge_meta$lat_x[i]
  storms_covered$station_lon <- surge_meta$lon_x[i]
  storms_covered$best_reconstruction <- surge_meta$lowestRMSE[i]
  storms_covered

}) %>% bind_rows

# now download and extract data from each station, based on storm starts and ends
storms_with_sccr_surge <- lapply(unique(storm_surge_stations$station), function(surge_station) {
  station_df <- filter(storm_surge_stations, station == surge_station)
  station_name <- stringr::str_replace(surge_station,".csv","")
  best_recon <- unique(station_df$best_reconstruction)
  best_recon_path <- ifelse(best_recon == "ERA-FIVE", "erafive", 
                       ifelse(best_recon == "ERA-Interim", "eraint", 
                          ifelse(best_recon == "MERRA", "merra",
                            ifelse(best_recon == "20CR", "20cr", 
                              ifelse(best_recon == "ERA-20c", "era20c", NA)))))
  if (is.na(best_recon_path)) return(NULL)  
  best_recon_7z_path <- sprintf("%s/%s.7z", best_recon_path, station_name)
  recon_dl_path <- sprintf("https://github.com/moinabyssinia/gssr/raw/gh-pages/%s",best_recon_7z_path)
  local_dl_path <- sprintf("./data/%s/%s_%s.7z",station_name,station_name,best_recon_path)
  if (!dir.exists(sprintf("./data/%s/",station_name))) dir.create(sprintf("./data/%s/",station_name))
  
  # download the station best recon 7z file:
  if (!file.exists(local_dl_path)) {
    download.file(stringr::str_replace(recon_dl_path,",","%2C"),destfile=local_dl_path,method="wget")
  }

  # unzip, read, and keep only the max surge per storm
  station_reconstruction <- read.csv(archive::archive_read(local_dl_path))

  storm_rows <- lapply(unique(station_df$storm), function(storm_name) {
                 storm_meta <- filter(station_df,storm==storm_name)
                 station_recon_filtered <- filter(station_reconstruction, 
                                           date > storm_meta$start_time[1] 
                                           & date < storm_meta$end_time[1])
                 max_surge_recon <- max(station_recon_filtered$surge_reconsturcted) 
                 storm_meta$max_surge_reconstructed <- max_surge_recon
                 storm_meta
  }) %>% bind_rows
  storm_rows

}) %>% bind_rows

storms_with_sccr_surge %>% filter(storm=="HARVEY"&year==2017) %>% '$'('station') 

storm_max_surge <- storms_with_sccr_surge %>% 
        group_by(storm, year) %>% 
        summarize(max_surge = max(max_surge_reconstructed))


####################################
#                                  #
#           Tornado Data           #
#                                  #
####################################
if (file.exists(tornado_file) & !recook) {
        tornado_data <- read.csv(tornado_file)
} else {
  # get the tornado data from sheet 3 of the online .xls file
  # there are some superfluous rows to get rid of also
  tornado_data <- rio::import("https://www.spc.noaa.gov/misc/edwards/TCTOR/tctor.xls",which=2)
  tornado_data <- na.omit(tornado_data) # get rid of empty rows
  tornado_data <- unique(tornado_data)  # get rid of dupes
  tornado_data <- tornado_data[ str_detect(str_to_lower(tornado_data[,1]), "total", negate=TRUE), ] # get rid of 'total' rows
  # clean up the name and year columns
  tornado_data$name <- str_to_upper(str_split_fixed(tornado_data[,1],"-",2)[,1])
  tornado_data$yy <- str_split_fixed(tornado_data[,1],"-",2)[,2]
  tornado_data$year <- ifelse(as.numeric(tornado_data$yy) <= 22, paste0("20", tornado_data$yy), paste0("19", tornado_data$yy))
  tornado_data <- select(tornado_data, year, name, Tors)

}


## Clean Tornado
# only need to make sure year is numeric 
t_data <- tornado_data %>% 
        mutate(year=as.numeric(year))

joined <- h_data %>% 
        left_join(r_data, by=c("name","year")) %>% 
        left_join(s_data, by=c("name","year")) %>%
        left_join(t_data, by=c("name","year"))



# write the final dataset
write.csv(joined,file="data/stormhazards.csv")
data_old <- read.csv("data/stormhazards.csv")

data_old %>% filter(name=="DANNY")
storm_max_surge %>% filter(storm=="DANNY")

