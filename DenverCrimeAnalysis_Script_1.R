library(readr)
library(rgdal)
library(rgeos)
library(sp)
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
library(viridis)
library(maps)
library(cowplot)
library(leaflet)
library(ggrepel)
library(chron)
library(tidyverse)
library(hms)
library(stringr)

denver_crime_dataset <- read_csv("/Users/bethanyleach/Downloads/crime.csv")
denver_offense_codes <- read_csv("/Users/bethanyleach/Downloads/offense_codes.csv")

denver_crime_revised <- as_tibble(denver_crime_dataset)
str(denver_crime_revised)
View(denver_crime_revised)

denver_offense_codes_revised <- as_tibble(denver_offense_codes)
str(denver_offense_codes_revised)
View(denver_offense_codes_revised)

denver_crimes_codes_joined <- inner_join(denver_crime_revised, denver_offense_codes_revised, 
                                         by = c("OFFENSE_CODE", "OFFENSE_CODE_EXTENSION", "OFFENSE_TYPE_ID", 
                                                "OFFENSE_CATEGORY_ID", "IS_CRIME", "IS_TRAFFIC"))
#denver_crime_revised <- denver_crime_revised[complete.cases(denver_crime_revised), ]
#View(denver_crime_revised)

register_google("AIzaSyBZx2Za3bJfe2OY0QxgPgef-4GX9jd61Pg")

denver_area_map <- get_map("Denver",zoom=11)
google_denver <- ggmap(denver_area_map)

denver_bb <- c(
        left=-105.3218,
        bottom = 39.6144,
        right = -104.6097,
        top = 39.9043
)
denver_stamen <- get_stamenmap(
        bbox = denver_bb,
        zoom = 10
)
ggmap(denver_stamen)

#Crimes vs Traffic Percentage Graph
unique(denver_crimes_codes_joined$OFFENSE_CATEGORY_NAME)

denver_crimes_codes_joined_is_isnt_crime <- denver_crimes_codes_joined 

is_crime_traffic <- denver_crimes_codes_joined_is_isnt_crime %>%
        group_by(IS_CRIME, IS_TRAFFIC) %>%
        tally() %>%
        complete(IS_TRAFFIC, fill = list(n=0)) %>%
        mutate(percentage = n / sum(n) * 100)

ggplot(is_crime_traffic, aes(IS_TRAFFIC, percentage, fill = IS_CRIME)) + 
        geom_bar(stat = 'identity', position = 'dodge') + 
        xlab("Crime                             Traffic") + 
        theme(legend.position="none", axis.ticks.x = element_blank(), 
              axis.text.x = element_blank(), 
              axis.title.x = element_text(angle = 0)) + 
        ylab("Percentage") + ggtitle("Percentage of Crimes vs Traffic Incidents")

#Traffic Incident Investigation
denver_crimes_codes_joined_is_isnt_crime <- denver_crimes_codes_joined %>%
        filter(IS_TRAFFIC ==1)
unique(denver_crimes_codes_joined_is_isnt_crime$OFFENSE_CATEGORY_NAME)

#Crime Mapped
unique(denver_crimes_codes_joined$NEIGHBORHOOD_ID)

denver_crimes_mapped <- denver_crimes_codes_joined %>%
        filter(IS_TRAFFIC == 0)

denver_crimes_mapped_layered <- ggmap(denver_area_map) + geom_point(data=denver_crimes_mapped, 
                                                                    aes(x=GEO_LON, y=GEO_LAT, 
                                                                        color =OFFENSE_CATEGORY_NAME))

denver_crimes_mapped_layered <- ggmap(denver_area_map) + 
        stat_density2d(data = denver_crimes_mapped, aes(x = GEO_LON, y = GEO_LAT, fill = ..density..), geom = 'tile', contour = F, alpha = .5) +  
        scale_fill_viridis()

______________________________________________________________
us_map %>% 
        ggplot(aes(x = long, y = lat, group = group)) +
        geom_polygon(fill = "lightblue", color = "black") + 
        theme_void()


us_map <- map_data("state")
us_map %>% 
        filter(region %in% c("colorado")) %>%
        ggplot(aes(x = long, y = lat, group = group)) +
        geom_path()


us_map %>% 
        filter(region %in% c("colorado")) %>%
        ggplot(aes(x = long, y = lat, group = group)) +
        geom_polygon(fill = "lightgreen", color = "black") +
        theme_void()

colorado <- map_data('county', region = 'colorado')

denver <- colorado %>%
        filter(subregion %in% c("denver"))

ggplot(colorado, aes(x = long, y = lat, group = group)) + 
        geom_polygon(fill = "lightgreen", color = "black") + 
        theme_void()


maryland <- map_data('county', region = 'maryland')
baltimore <- maryland %>%
        filter(subregion %in% c("baltimore city", "baltimore"))



usa <- map_data("world", region = "USA")

usa <- map_data("USA")
ggplot(data = USA, aes(x = long, y = lat, group = group)) + 
        geom_polygon(fill = "white", color = "black") + theme_void()

ggplot(data=usa, aes(x = long, y = lat, group = group)) +
        geom_polygon(fill = "white", color = "black")+
        coord_fixed(1.8, xlim = c(-180,0)) + 
        theme_void()
mainland <- ggplot(data=usa, aes(x = long, y = lat, group = group)) +
        geom_polygon(fill = "white", color = "black")+
        coord_fixed(1.5, xlim = c(-125,-68), ylim = c(50,22)) + 
        theme_void()

usa_mainland <- subset(usa, subregion != "Alaska")
usa_mainland <- subset(usa_mainland, subregion != "Hawaii")
usa_states <- map_data("state")


ggplot(data = usa_mainland, aes(x = long, y = lat, group = group)) + 
        geom_polygon(data = usa_states, fill = "white", color = "black") +
        coord_fixed(1.4) + 
        theme_void() 

colorado_shade <- subset(usa_states, region == "colorado")
__________________________________________________________________
ggplot(data = usa_states, aes(x = long, y = lat, group = group)) + 
        geom_polygon(data = usa_states, fill = "white", color = "black") +
        geom_polygon(data = colorado_shade, fill = "lightgreen", color = "black") +
        coord_fixed(1.4) + 
        theme_void() 

colorado <- subset(usa_states, region == "colorado")

ggplot(data = colorado, aes(x = long, y = lat, group = group)) + 
        geom_polygon(fill = "white", color = "black") +
        coord_fixed(1.4) + 
        theme_void() 
_____________________________________________________
usa_states <- map_data("state")
colorado <- subset(usa_states, region %in% c("colorado"))

usa_counties <- map_data("county")
colorado_counties <- subset(usa_counties, region == "colorado")

denver <- subset(colorado_counties, subregion == "denver")

colorado_counties_highlight <- ggplot(data = colorado, aes(x = long, y = lat, group = group)) + 
        geom_polygon(data = colorado_counties, fill = "white", color = "black") +
        geom_polygon(data = denver, fill = "lightgreen", color = "black") +
        coord_fixed(1.4) + 
        theme_void() 

#ggplot(data = denver, aes(x = long, y = lat)) + 
#        geom_polygon(fill = "white", color = "black") +
#        coord_fixed(1.4) + 
#        theme_void() + geom_point(data=denver_crimes_mapped, 
#                                  aes(x=GEO_LON, y=GEO_LAT))

crimeColoradoMerge <- merge(colorado, denver_crimes_codes_joined, by = "region")
str(murderMap)



*denver_crimes_mapped

merged_co_map <- denver_crimes_mapped %>%
        left_join(colorado, by = c("GEO_LON" = "long", "GEO_LAT" = "lat"))

merged_co_plot <- ggplot(merged_co_map, aes(x = GEO_LON, y = GEO_LAT, color=OFFENSE_CATEGORY_ID)) +
        geom_polygon(color = "black") + 
        scale_fill_gradient(low = "black", high = "red", guide = "legend")

View(merged_co_map)

_______________________________________________
usa_states <- map_data("state")
colorado <- subset(usa_states, region %in% c("colorado"))

usa_counties <- map_data("county")
colorado_counties <- subset(usa_counties, region == "colorado")

denver <- subset(colorado_counties, subregion == "denver")



merged_co_plot_denver <- ggplot(data = denver, aes(x = long, y = lat, group = group)) + 
        coord_fixed(1.4) + 
        theme_void() 

DenverMap <- qmap("Denver", zoom = 12, color = "bw", legend = "topleft")
crime_denver_plot <- DenverMap + geom_point(aes(x = GEO_LON, y = GEO_LAT, 
                                                color = OFFENSE_CATEGORY_ID), 
                                            data = denver_crimes_mapped)


denver_crimes_mapped_layered <- ggmap(denver_area_map) + geom_point(data=denver_crimes_mapped, 
                                                                    aes(x=GEO_LON, y=GEO_LAT, 
                                                                        color = OFFENSE_CATEGORY_ID))

ggplot(murderMap, aes(x = long, y = lat, group = group, fill = Murders)) + 
        geom_polygon(color = "black") + scale_fill_gradient(low = "black", high = "red", guide = "legend")


##Exploratory Analysis in diving deeper into crimes##

denver_crime_datetime_separate <- denver_crimes_codes_joined %>%
        mutate(FIRST_OCCURRENCE_DATE= mdy_hms(FIRST_OCCURRENCE_DATE),
               day = day(FIRST_OCCURRENCE_DATE),
               month = month(FIRST_OCCURRENCE_DATE),
               year = year(FIRST_OCCURRENCE_DATE),
               dayofweek = wday(FIRST_OCCURRENCE_DATE),
               minute = wday(FIRST_OCCURRENCE_DATE),
               second = second(FIRST_OCCURRENCE_DATE))

View(denver_crime_datetime_separate)

denver_crime_datetime_separate$date <- as.Date(denver_crime_datetime_separate$FIRST_OCCURRENCE_DATE) 

denver_crime_datetime_separate$time <- format(as.POSIXct(denver_crime_datetime_separate$FIRST_OCCURRENCE_DATE),    
                    format = "%H:%M:%S")


first_500_denver_crime_datetime_separate <- denver_crime_datetime_separate[1:500,]
first_500_denver_crime_datetime_separate$time <- as.character(first_500_denver_crime_datetime_separate$time) 
first_500_denver_crime_datetime_separate

datatable(first_500_denver_crime_datetime_separate, options = list(pageLength = 25,scrollX='400px'))


#Traffic Accident Information - Types
unique(denver_cols_map_traffic$OFFENSE_TYPE_ID)

denver_cols_map_traffic_2 <- denver_cols_map_traffic %>%
        group_by(OFFENSE_TYPE_ID) %>%
        dplyr::summarize(total = n())

denver_cols_map_traffic_2

denver_cols_map_traffic_2_plot <-ggplot(denver_cols_map_traffic_2, aes(x=OFFENSE_TYPE_ID, y=total, fill=OFFENSE_TYPE_ID)) + 
        geom_bar(stat="identity") + theme(legend.position = "none", axis.text.x = element_text(angle=45, vjust=1, hjust=0.5)) +
        ggtitle("Distribution of Different Categories of Crimes") + ylab("Total Number of Crimes") + xlab("Category of Crime")







#sprintf("Number of Rows in Dataframe: %s", format(nrow(denver_crime_datetime_separate),big.mark = ","))









#denver_crime_datetime_separate$time <- format(as.POSIXct(denver_crime_datetime_separate$FIRST_OCCURRENCE_DATE), format = "%H:%M:%S")
#View(denver_crime_datetime_separate)

#size = OFFENSE_CATEGORY_ID
#denver_crimes_codes_joined_is_isnt_crime <- denver_crimes_codes_joined %>%
#        group_by(IS_CRIME, IS_TRAFFIC) %>%
#        dplyr::summarize(total_count = n())

#ggplot(denver_crimes_codes_joined_is_isnt_crime, aes(x = IS_CRIME, y = total_count, fill=IS_TRAFFIC)) +
#        geom_col(position = "dodge")


#ggplot(denver_crimes_codes_joined_is_isnt_crime, aes(x = IS_CRIME, y = total_count, fill=IS_TRAFFIC)) +
#        geom_bar(stat='identity', position='dodge')

