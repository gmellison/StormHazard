library(ggplot2)
library(dplyr)
library(tidyr)
library(knitr)

add_categories <- function(data, category_cutoffs="saffir-simpson") {

 
        if (! (category_cutoffs %in% c("averaged", "saffir-simpson")) ) {
                stop("category_cutoffs argument must be one of \"averaged\" or  \"saffir-simpson\"")
        }
   
        sscore_cuts <- c(65, 82, 95, 112, 136)
        
        s <- sd(data$windwpeed,na.rm=TRUE)
        m <- mean(data$windspeed,na.rm=TRUE)
            
            # z-score categories base on saff-simp (windspeeds)
        zscore_cuts <- (sscore_cuts - m) / s
 
       # TODO: center/scale raw data columns

        if (category_cutoffs=="averaged") {      
            # Surge: z-score category cuts based on surge "saff-simp" analogue
            surge_cuts <- c(4,5,8,12,18)
            s <- sd(data$surge,na.rm=TRUE)
            m <- mean(data$surge,na.rm = TRUE)
            s_zscore_cuts <- (surge_cuts - m) / s
            
            # Pressure: z-score category cuts based on pressure "saff-simp" analogue
            pressure_cuts <- -1 * c(990,979,965,945,920)
            s <- sd(-1 * data$Pressure,na.rm=TRUE)
            m <- mean(-1 * data$Pressure,na.rm = TRUE)
            p_zscore_cuts <- (pressure_cuts - m) / s
            
            # averaged cutoffs by averaging wind,pressure,surge cutoffs:
            zscore_cuts <- (1/3) * (p_zscore_cuts + s_zscore_cuts + zscore_cuts)
        }

        data$WindCat     <- .bincode(MaxWindSpeed_Z, c(-5,zscore_cuts,10))-1
        data$RainCat     <- .bincode(RainfallPointMax_Z, c(-5,zscore_cuts,10))-1
        data$SurgeCat    <- .bincode(Surge_Z, c(-5,zscore_cuts,10))-1
        data$PressureCat <- .bincode(-1 * Pressure_Z, c(-5,zscore_cuts,10))-1
        data$TornadoCat  <- .bincode(Tornado_Z, c(-5,zscore_cuts,10))-1
        data$RadiusCat   <- .bincode(Radius_Z, c(-5,zscore_cuts,10))-1

        return(data)

}

# function to generate the bar plot, based on a single row of the h dataframe.
make_bar_plot <- function(data, name, year) {
  
  # rename things nicely
  h <- data %>% 
          filter(name ==str_to_upper()) %>%
    select(c("name", "year", "RainfallCat", "WindCat", "SurgeCat", "PressureCat", "TornadoCat", "RadiusCat"))
  
  # format data as key/value for easy bar plotting   
  plt_h <- pivot_longer(h, names_to = "Hazard", values_to = "Category",
                        cols = c("Surge", "Windspeed", "Rainfall", "Tornado", "Pressure", "Size")
  )
 
  plt_h$Category[is.na(plt_h$Category)] <- 0
  
  ggplot(plt_h) + 
    geom_bar(aes(y = 2^(Category+1), 
                 fill = factor(Category),
                 x = factor(Hazard),
                 col = factor(Hazard)),
             stat= "identity") +
    scale_fill_manual(values=c("0"="yellow", "1"="orange", 
                               "2"="tomato1", "3" = "tomato3", "4"="red2", "5"="black"),
                      guide="none") +
    scale_color_manual(values=c(rep("black",6)), guide="none") + 
    coord_cartesian(ylim = c(0,2^6)) +
    labs(y = "Hazard Level", x = "") + #, title = sprintf("%s - %s", plt_h$Name[1], plt_h$year[1])) +
    ggthemes::theme_clean() + 
    theme(axis.text.x = element_text(angle = 45, vjust = 0.65, size = 10.5),
          panel.background = element_blank(),
          legend.background = element_blank(),
          plot.background = element_blank()) +
    labs(title= sprintf("%s - %s", h$Name, h$year)) + 
    scale_y_continuous(breaks = 2^c(1,2,3,4,5,6), labels = c("TS", "1", "2", "3","4", "5"))
}
