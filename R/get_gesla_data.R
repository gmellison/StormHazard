library(dplyr)
library(arrow) 
library(geslaR)
library(cli)

# this script downloads and joins the gesla data to the hurdat data.
# hurdat data should already be stored in the data subdir

hurdat <- read.csv("data/data_hurdat.csv")

if (!file.exists("data/GESLA3_All.csv")) {
    download_gesla()
}
gesla_meta <- read.csv("data/GESLA3_All.csv")


i <- 1
ges_sites <- lapply(1:nrow(hurdat), function(i) {
    d <- 1.5
    lat <- hurdat$lat[i]
    lon <- hurdat$lon[i]
    year <- lubridate::year(hurdat$date[i])
    month <- lubridate::month(hurdat$date[i])
    day <- lubridate::day(hurdat$date[i])
    dists <- sqrt((gesla_meta$LATITUDE - lat)^2 + (gesla_meta$LONGITUDE - lon)^2)
    nsites <- sum(dists < d)
    if(nsites == 0) return(data.frame())
    site_name <- gesla_meta$SITE.NAME[dists < d]
    country <- gesla_meta$COUNTRY[dists < d]
    start_date <- gesla_meta$START.DATE.TIME[dists < d]
    end_date <- gesla_meta$END.DATE.TIME[dists < d]
    site_code <- gesla_meta$SITE.CODE[dists < d]
    file_name <- gesla_meta$FILE.NAME[dists < d]

    return(data.frame(hurdat_row = i, site_name, country, start_date, end_date, dist_from_l=dists[dists < d], file_name, site_code, year, month, day))
}) %>% bind_rows()

all_gesla_landfalls <- geslaR::query_gesla(country=unique(ges_sites$country), site_name=unique(ges_sites$site_name), year=1980:2020)

avg_sl <- all_gesla_landfalls |> 
        group_by(country, file_name, site_name, year, month) |> 
        summarise(monthly_avg_sl = median(sea_level, na.rm=TRUE)) |>
        arrange(country, file_name, site_name, year, month) |>
        mutate(sl_l1 = lag(monthly_avg_sl, 1), sl_l2 = lag(monthly_avg_sl,2), sl_l3 = lag(monthly_avg_sl,3), 
               sl_l4 = lag(monthly_avg_sl, 4), sl_l5 = lag(monthly_avg_sl,5), sl_l6 = lag(monthly_avg_sl,6),
               sl_l7 = lag(monthly_avg_sl, 7), sl_l8 = lag(monthly_avg_sl,8), sl_l9 = lag(monthly_avg_sl,9), 
               sl_l10 = lag(monthly_avg_sl, 10)) |>
        mutate(last10 = (ifelse(!is.na(sl_l1), sl_l1, 0) + ifelse(!is.na(sl_l2), sl_l2, 0) + 
                        ifelse(!is.na(sl_l3), sl_l3, 0) + ifelse(!is.na(sl_l4), sl_l4, 0) + 
                        ifelse(!is.na(sl_l5), sl_l5, 0) + ifelse(!is.na(sl_l6), sl_l6, 0) + 
                        ifelse(!is.na(sl_l7), sl_l7, 0) + ifelse(!is.na(sl_l8), sl_l8, 0) + 
                        ifelse(!is.na(sl_l9), sl_l9, 0) + ifelse(!is.na(sl_l10), sl_l10, 0))/10)

max_surge_inR <- all_gesla_landfalls |> 
        group_by(country, file_name, site_name, year, month, day) |> 
        summarise(max_sl = max(sea_level, na.rm=TRUE)) |> 
        collect()

max_surge <-  max_surge_inR %>%
        left_join(avg_sl, by=c("file_name", "country", "site_name", "year", "month")) %>% 
        mutate(daily_max_surge = max_sl - last10) %>%
        select(-sl_l1, -sl_l2, -sl_l3, -sl_l4, -sl_l5, -sl_l6, -sl_l7, -sl_l8, -sl_l9, -sl_l10)

landfall_max_surge <- ges_sites %>%
        left_join(max_surge, by=c("file_name", "year", "month", "day")) %>%
        filter(!is.na(daily_max_surge))

hurdat_surges <- landfall_max_surge %>%
        group_by(hurdat_row) %>%
        summarise(max_surge = max(daily_max_surge, na.rm=TRUE))

hurdat$surge_gesla <- NA
hurdat$surge_gesla[hurdat_surges$hurdat_row] <- hurdat_surges$max_surge

write.csv("data/landfalls_surge.csv")
