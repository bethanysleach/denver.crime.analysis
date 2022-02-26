library(readr)
library(rgdal)
library(broom)
library(ggplot2)
library(sf)
library(ggmap)
library(rstudioapi)
library(RColorBrewer)
library(ggthemes)
library(lubridate)
library(dplyr)
library(tidyr)
library(DT)
library(scales)
library(calendR)
library(tmap)
library(tmaptools)
library(OpenStreetMap)
library(ggsn)
library(ggrepel)
library(grid)
library(forcats)
library(magrittr)
library(plyr)
library(data.table)

denver_crime_dataset <- read_csv("/Users/bethanyleach/Downloads/crime.csv")
denver_offense_codes <- read_csv("/Users/bethanyleach/Downloads/offense_codes.csv")

denver_crime_revised <- as_tibble(denver_crime_dataset)
str(denver_crime_revised)

denver_offense_codes_revised <- as_tibble(denver_offense_codes)
str(denver_offense_codes)

jointdataset <- merge(denver_crime_revised, denver_offense_codes_revised, by = 'OFFENSE_CATEGORY_ID')

#denver_crime_revised <- denver_crime_revised[complete.cases(denver_crime_revised), ]
#View(denver_crime_revised)

register_google("AIzaSyBZx2Za3bJfe2OY0QxgPgef-4GX9jd61Pg")

denver_area_map <- get_map("Denver",zoom=11)
