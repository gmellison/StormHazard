library(dplyr)
library(tidyr)
library(ggplot2)

data <- read.csv("data/landfalls_surge.csv")
rf <- read.csv("data/landfalls_rain.csv")
head(data)

# join rainfalls 
rf <- select(rf, c("h_name","date","precip","hours"))
data <- left_join(data, rf, by = c("h_name", "date", "hours"))
data$year <- lubridate::year(data$date)

# join tornados
tor <- read.csv("data/tctor.csv")
data <- left_join(data, tor, by=c("h_name","year"))

df <- data %>% select(h_name, surge_gesla, precip, date, hours, id, windspeed_max, tors) 
head(df)

# convert precip to rf/km^2
latlon_to_km2 <- function(lat, lon, side=1) {
    lat_rad <- lat * pi/180
    lon_rad <- lon * pi/180
    
    lat_cf <- 110.574
    lon_cf <- 111.32 * cos(lat_rad)
    area <- side * lat_cf * side * lon_cf
    return(area)
}

data$precip_hr <- data$precip / 48 #data$precip / latlon_to_km2(data$lat, data$lon)

# data <- add_categories(data, category_cutoffs = "saffir-simpson") 
data$wind_z <- (data$windspeed_max - mean(data$windspeed_max, na.rm=TRUE))/sd(data$windspeed_max, na.rm=TRUE)
data$rain_z <- (data$precip_hr - mean(data$precip_hr, na.rm=TRUE))/sd(data$precip_hr, na.rm=TRUE)
data$surge_z <- (data$surge_gesla - mean(data$surge_gesla, na.rm=TRUE))/sd(data$surge_gesla, na.rm=TRUE)
data$tors_z <-  (data$tors - mean(data$tors, na.rm=TRUE))/sd(data$tors, na.rm=TRUE)

df <- data %>% select(h_name, h_id, date, 
                        "Wind"=windspeed_max, "Surge"=surge_gesla, "Rain"=precip_hr, "Tornado"=tors,
                        "WindZ"=wind_z, "SurgeZ"=surge_z, "RainZ"=rain_z, "TornadoZ"=tors_z)
histran <- c(-2.2,6.2)
png("plots/wind.png")
hist(df$WindZ, xlab="Z-score", main="Windspeed", xlim=histran)
dev.off()

png("plots/surge.png")
hist(df$SurgeZ, xlab="Z-score", main="Storm Surge", xlim=histran)
dev.off()

png("plots/rain.png")
hist(df$RainZ, xlab="Z-score", main="Rainfall", xlim=histran)
dev.off()

png("plots/tors.png")
hist(df$TornadoZ, xlab="Z-score", main="Tornado", xlim=histran)
dev.off()

png("plots/z_hists.png",w=960,h=720)
par(mfrow=c(2,2))
par(oma=c(2,2,1,1))
par(mar=c(4,2.5,3,3), cex.main=1.5, cex.lab=1.5, cex.axis=1.5)
hist(df$WindZ, xlab="Z-score", main="Windspeed", xlim=histran)
hist(df$SurgeZ, xlab="Z-score", main="Storm Surge", xlim=histran)
hist(df$RainZ, xlab="Z-score", main="Rainfall", xlim=histran)
hist(df$TornadoZ, xlab="Z-score", main="Tornado", xlim=histran)
dev.off()


kat_row <- df[df$h_name == "KATRINA", ][4,]
kat <- kat_row %>% pivot_longer(cols=c(WindZ, SurgeZ, RainZ, TornadoZ), names_to="Hazard")
kat$Hazard <- stringr::str_replace_all(kat$Hazard, "Z", "")
ggplot(kat) + 
        geom_bar(aes(x=Hazard, 
                     y=value),
                 col="black",
                 fill="grey65",
                 stat="identity") + 
        labs(x="Hazard", y="Z-Score", title=sprintf("Katrina (%s)", kat$date)) + 
        coord_cartesian(ylim=c(-1,4.5)) +
        theme_minimal()
ggsave("plots/katrina.png")

charl_row <- df[df$h_name == "CHARLEY", ][4,]
charl <- charl_row %>% pivot_longer(cols=c(WindZ, SurgeZ, RainZ, TornadoZ), names_to="Hazard")
charl$Hazard <- stringr::str_replace_all(charl$Hazard, "Z", "")
ggplot(charl) + 
        geom_bar(aes(x=Hazard, 
                     y=value),
                 col="black",
                 fill="grey65",
                 stat="identity") + 
        labs(x="Hazard", y="Z-Score", title=sprintf("Charley (%s)", charl$date)) +
        coord_cartesian(ylim=c(-1,4.5)) +
        theme_minimal()
ggsave("plots/charley.png")

matt_row <- df[df$h_name == "MATTHEW",][7,]
matt <- matt_row %>% pivot_longer(cols=c(WindZ, SurgeZ, RainZ, TornadoZ), names_to="Hazard")
matt$Hazard <- stringr::str_replace_all(matt$Hazard, "Z", "")
ggplot(matt) + 
        geom_bar(aes(x=Hazard, 
                     y=value),
                 fill="grey65",
                 col="black",
                 stat="identity") + 
        labs(x="Hazard", y="Z-Score", title=sprintf("Matthew (%s)", matt$date)) +
        coord_cartesian(ylim=c(-1,4.5)) +
        theme_minimal()
ggsave("plots/matthew.png")

ike_row <- df[df$h_name == "IKE",][4,]
ike <- ike_row %>% pivot_longer(cols=c(WindZ, SurgeZ, RainZ, TornadoZ), names_to="Hazard")
ike$Hazard <- stringr::str_replace_all(ike$Hazard, "Z", "")
ggplot(ike) + 
        geom_bar(aes(x=Hazard, 
                     y=value),
                 col="black",
                 fill="grey65",
                 stat="identity") + 
        labs(x="Hazard", y="Z-Score", title=sprintf("Ike (%s)", ike$date)) +
        coord_cartesian(ylim=c(-1,4.5)) +
        theme_minimal()
ggsave("plots/ike.png")

tab <- rbind(kat_row, ike_row, matt_row, charl_row) %>%
        select(h_name, date, Rain, RainZ, Surge, SurgeZ, Tornado, TornadoZ, Wind, WindZ) %>%
        write.csv("data/manuscript_tab.csv")



# not used
#jean_row <- df[df$h_name == "JEANNE", ][5,]
#jean <- jean_row %>% pivot_longer(cols=c(WindZ, SurgeZ, RainZ, TornadoZ), names_to="Hazard")
#jean$Hazard <- stringr::str_replace_all(jean$Hazard, "Z", "")
#ggplot(jean) + 
#        geom_bar(aes(x=Hazard, 
#                     y=value),
#                 col="black",
#                 fill="grey65",
#                 stat="identity") + 
#        labs(x="Hazard", y="Z-Score", title=sprintf("Jeanne (%s)", jean$date))
#        coord_cartesian(ylim=c(-1,4.5)) +
#        theme_minimal()
#ggsave("plots/jeanne.png")
#
#harv_row <- df[df$h_name == "HARVEY", ][7,]
#harv <- harv_row %>% pivot_longer(cols=c(WindZ, SurgeZ, RainZ, TornadoZ), names_to="Hazard")
#harv$Hazard <- stringr::str_replace_all(harv$Hazard, "Z", "")
#ggplot(harv) + 
#        geom_bar(aes(x=Hazard, 
#                     y=value),
#                 col="black",
#                 fill="grey65",
#                 stat="identity") + 
#        labs(x="Hazard", y="Z-Score", title=sprintf("Harvey (%s)", harv$date)) +
#        coord_cartesian(ylim=c(-1,4.5)) +
#        theme_minimal()
#ggsave("plots/harvey.png")
#
#
