source("R/plotting.R")

data <- read.csv("data/StormHazards.csv")
data <- add_categories(data, category_cutoffs = "saffir-simpson") 

plt <- make_bar_plot(data, "KATRINA", 2005)
plot(plt)
