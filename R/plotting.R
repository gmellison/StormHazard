library(ggplot2)
library(dplyr)
library(tidyr)
library(knitr)

data <- read.csv("data/StormDataWithZScores.csv")

add_categories <- function(data, category_cutoffs="saffir-simpson") {

 
        if (! (category_cutoffs %in% c("averaged", "saffir-simpson")) ) {
                stop("category_cutoffs argument must be one of \"averaged\" or  \"saffir-simpson\"")
        }
   
        sscore_cuts <- c(65, 82, 95, 112, 136)
        
        s <- sd(data$MaxWindSpeed,na.rm=TRUE)
        m <- mean(data$MaxWindSpeed,na.rm=TRUE)
            
            # z-score categories base on saff-simp (windspeeds)
        zscore_cuts <- (sscore_cuts - m) / s
  
        if (category_cutoffs=="averaged") {      
            # Surge: z-score category cuts based on surge "saff-simp" analogue
            surge_cuts <- c(4,5,8,12,18)
            s <- sd(data$Surge,na.rm=TRUE)
            m <- mean(data$Surge,na.rm = TRUE)
            s_zscore_cuts <- (surge_cuts - m) / s
            
            # Pressure: z-score category cuts based on pressure "saff-simp" analogue
            pressure_cuts <- -1 * c(990,979,965,945,920)
            s <- sd(-1 * data$Pressure,na.rm=TRUE)
            m <- mean(-1 * data$Pressure,na.rm = TRUE)
            p_zscore_cuts <- (pressure_cuts - m) / s
            
            # averaged cutoffs by averaging wind,pressure,surge cutoffs:
            zscore_cuts <- (1/3) * (p_zscore_cuts + s_zscore_cuts + zscore_cuts)
        }

        data$WindCat     <- .bincode(data$MaxWindSpeed_Z, c(-5,zscore_cuts,10))-1
        data$RainCat     <- .bincode(data$RainfallPointMax_Z, c(-5,zscore_cuts,10))-1
        data$SurgeCat    <- .bincode(data$Surge_Z, c(-5,zscore_cuts,10))-1
        data$PressureCat <- .bincode(-1 * data$Pressure_Z, c(-5,zscore_cuts,10))-1
        data$TornadoCat  <- .bincode(data$Tornado_Z, c(-5,zscore_cuts,10))-1
        data$RadiusCat   <- .bincode(data$Radius_Z, c(-5,zscore_cuts,10))-1

}
