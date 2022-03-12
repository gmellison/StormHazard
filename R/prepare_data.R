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
surge_file <- paste0(data_dir,"data_surge.csv")
update <- FALSE

#### Get the Hurdat2 data:
### data schema

if (file.exists(hurdat_file) & !update) {
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
  write.csv(file=hurdat_file, hurdat_data)
}

##############################################
#                                            #
#           Rainfall Point Maxima:           #
#                                            #
##############################################

# first check if the file already exists:
if (file.exists(rainfall_file) & !update) {
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
  write.csv(rainfall_data, file = rainfall_file)
}



################################
#                              #
#           Surgedat           #
#                              #
################################

if (file.exists(surge_file) & !update) {
        surge_data <- surge_file
} else {
  surge_data <- read.csv("http://surge.climate.lsu.edu/files/gompeaksurgedb.csv")
}


################################
#                              #
#      Clean and Join Data     #
#                              #
################################

# group to one line per storm (by name/year)
# convert rainfall from in to mm
rainfall_data %>% 
  group_by(name, year) %>% 
  summarise(amount = max(amount,na.rm=TRUE)) %>%
  mutate(rainfall_mm = amount * 25.4) %>%
  select(name,year,rainfall_mm)

# group to one line per storm
# compute radius 
# keep only name, year, pressure, windspeed, radius columns
h_data <- hurdat_data %>% head
  mutate(name = str_to_upper(h_name)) %>% 
  mutate(year = year(date))

joined <- left_join(h_data, select(point_maxima, name, year, lf_idx, amount, location),
                    by = c("name_lower" = "name", "year" = "year", "lf_idx" = "lf_idx")) %>% 
  arrange(datetime, lf_idx)

joined <- select(joined, cols_out)

