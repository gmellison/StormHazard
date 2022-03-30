# Data Sources and Processing

# HURDAT2

HURDAT2 is a tropical storm data analysis product, and is publicly available here: https://www.aoml.noaa.gov/hrd/hurdat/Data_Storm.html.

From HURDAT, we are mainly concerned with storm name, year, central pressure, windspeed, and extants of windspeed.
Note that hurdat contains best track estimates of tropical storms in 3 hour increments; we summarize the 
data to include only the most extreme hazard events per storm. 

The data processing of the HURDAT2 data consists of 
1. Estimating storm radius by calculating the MAX(50 knot wind extant) across the 4 quadrants, NW, NE, SE, SW. 
This give estimated maximum distance from the center of the storm in which winds achieve a velocity of 50 knots/hr.
2. Grouping by storm name and year, and computing MAX(windspeed), MIN(central pressure), and MAX(radius)

# Storm Surge Data
Storm surge data from SURGEDAT - formerly maintained by SRCC at LSU:
Link: SURGEDAT

The SURGEDAT database contains storm name, year, and storm surge height (in both meters and feet). Most storms 
contain only a single entry, but some contain multiple observations (e.g. Katrina, 2005). We keep 
only the maximum observed surge per storm. 

The SURGEDAT database online seems to be incomplete, compared with the original SURGEDAT paper. We extract the 
data directly from the SURGEDAT paper (Needham & Kein, 2012) using the `tabulizer` R package.

Another, possible more complete, source of storm surge data is found in the following paper: https://www.nature.com/articles/s41597-021-00906-x#Tab2. future versions of the dataset will likely be updated to use the 
surge estimates described therein. 

# Precipitation Data

 NOAA weather station rainfall point estimates, located here: https://www.wpc.ncep.noaa.gov/tropical/rain/tcmaxima.html. 

 The data are extracted directly from the HTML webpage, and cleaned in `prepare_data.R`. The maximum observed
 rainfall event is kept per each storm. 
