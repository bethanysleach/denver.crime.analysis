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
library(scales)
library(ggrepel)
library(plotly)

denver_crime_dataset_2 <- read_csv("/Users/bethanyleach/Downloads/crime-2.csv")
denver_offense_codes <- read_csv("/Users/bethanyleach/Downloads/offense_codes.csv")

denver_crime_revised_2 <- as_tibble(denver_crime_dataset_2)
str(denver_crime_revised_2)
View(denver_crime_revised_2)

denver_offense_codes_revised <- as_tibble(denver_offense_codes)
str(denver_offense_codes_revised)
View(denver_offense_codes_revised)

denver_crimes_codes_joined_2 <- inner_join(denver_crime_revised_2, denver_offense_codes_revised, 
                                         by = c("OFFENSE_CODE", "OFFENSE_CODE_EXTENSION", "OFFENSE_TYPE_ID", 
                                                "OFFENSE_CATEGORY_ID", "IS_CRIME", "IS_TRAFFIC"))

#Maps of Denver
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

#Crime vs Traffic Percentage Graph
unique(denver_crimes_codes_joined_2$OFFENSE_CATEGORY_NAME)

denver_crimes_codes_joined_is_isnt_crime_2 <- denver_crimes_codes_joined_2 

is_crime_traffic_2 <- denver_crimes_codes_joined_is_isnt_crime_2 %>%
        group_by(IS_CRIME, IS_TRAFFIC) %>%
        tally() %>%
        complete(IS_TRAFFIC, fill = list(n=0)) %>%
        mutate(percentage = n / sum(n) * 100)

ggplot(is_crime_traffic_2, aes(IS_TRAFFIC, percentage, fill = IS_CRIME)) + 
        geom_bar(stat = 'identity', position = 'dodge') + 
        xlab("Crime                             Traffic") + 
        theme(legend.position="none", axis.ticks.x = element_blank(), 
              axis.text.x = element_blank(), 
              axis.title.x = element_text(angle = 0)) + 
        ylab("Percentage") + ggtitle("Percentage of Crimes vs Traffic Incidents")

#TRAFFIC FROM 1/17-3/22
df_traffic_over_time <- denver_cols_map_traffic %>%
        group_by(date) %>%
        dplyr::summarize(total = n()) %>%
        arrange(date)

df_traffic_over_time_plot <- ggplot(df_traffic_over_time, aes(x = date, y = total)) +
        geom_line(color = "purple", size = 0.05) +
        geom_smooth(color = "navy") +
        scale_x_date(breaks = date_breaks("1 year"), labels = date_format("%Y")) +
        xlab("Date of Crime (Year)") + ylab("Number of Crimes Committed") + 
        ggtitle("Denver: Daily Number of Traffic Incidents Committed from 1/2017 - 3/2022") +
        theme(axis.text.x = element_text(angle=30, vjust=.5, hjust=1))

df_traffic_over_time_plot


#Heat map of Time - Traffic Accidents
#The grepl in R is a built-in function that 
#searches for matches of a string or string vector. 
denver_cols_map_traffic_info <- denver_cols_map_traffic %>%
        filter(grepl("traffic-accident", OFFENSE_TYPE_ID))

denver_cols_map_traffic_info_daily <- denver_cols_map_traffic_info %>%
        group_by(date) %>%
        dplyr::summarize(total = n()) %>%
        arrange(date)

traffic_time_of_day <- ggplot(denver_cols_map_traffic_info_daily, aes(x = date, y = total)) +
        geom_line(color = "orange", size = 0.1) +
        geom_smooth(color = "lightblue") + scale_x_date(breaks = date_breaks("1 year"), labels = date_format("%Y")) +
        labs(x = "Year of Traffic Accident", y = "Number of Traffic Accidents", title = "Daily Traffic Accidents in Denver from 1/2017 - 3/2022")



get_hour_of_day <- function(x) {
        return (as.numeric(strsplit(x,":")[[1]][1]))
} 

time_day_traffic_accident <- denver_cols_map_traffic_info %>%
        mutate(Hour = sapply(time, get_hour_of_day)) %>%
        group_by(dayofweek, Hour) %>%
        dplyr::summarize(total = n())



day_of_week_format <- c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")
hour_order_format <- c(paste(c(12,1:11),"AM"), paste(c(12,1:11),"PM"))


time_day_traffic_accident$dayofweek <- factor(time_day_traffic_accident$dayofweek, label = revalue(day_of_week_format))
time_day_traffic_accident$Hour <- factor(time_day_traffic_accident$Hour, level = 0:23, label = hour_order_format)


##HEATMAP PLOT TRAFFIC ACCIDENTS__________________________
mine.heatmap <- ggplot(data = time_day_traffic_accident, mapping = aes(x = Hour,
                                                                       y = dayofweek,
                                                                       fill = total)) + geom_tile() +xlab(label = "Hour") + 
        ggtitle("Denver: Number of Traffic Accidents from 1/2017 - 3/2022") +
        xlab("Hour of the Day") + ylab("Day of the Week")

mine.heatmap
View(time_day_traffic_accident)

#TRAFFIC INCIDENT Investigation other than 'Traffic Accident'
#filter out traffic-accident

denver_crimes_codes_joined_is_isnt_crime_2 <- denver_crimes_codes_joined_2 %>%
        filter(IS_TRAFFIC ==1, OFFENSE_TYPE_ID != "traffic-accident") %>%
        group_by(OFFENSE_TYPE_ID) %>%
        dplyr::summarize(total = n())

unique(denver_crimes_codes_joined_is_isnt_crime_2$OFFENSE_CATEGORY_ID)
unique(denver_crimes_codes_joined_is_isnt_crime_2$OFFENSE_TYPE_ID)

denver_crimes_codes_joined_is_isnt_crime_2_wo_hom <- denver_crimes_codes_joined_is_isnt_crime_2 %>%
        filter(OFFENSE_TYPE_ID != "traf-vehicular-homicide", OFFENSE_TYPE_ID != "traf-vehicular-assault")

denver_crimes_codes_joined_is_isnt_crime_2_wo_hom$percentage <- 
        denver_crimes_codes_joined_is_isnt_crime_2_wo_hom$total / 
        sum(denver_crimes_codes_joined_is_isnt_crime_2_wo_hom$total)

pie_traffic_misc <- ggplot(data = denver_crimes_codes_joined_is_isnt_crime_2_wo_hom, aes(x="", y=percentage, fill=OFFENSE_TYPE_ID)) +
        geom_col(color = "black") + 
        coord_polar("y", start=0) +
        geom_text(aes(x=1.6, label=paste0(round(percentage*100), "%")),
                  position = position_stack(vjust=0.5)) +
        theme(panel.background = element_blank(),
              axis.line = element_blank(),
              axis.text = element_blank(),
              axis.ticks = element_blank(),
              axis.title = element_blank(),
              plot.title = element_text(hjust = 0.5, size = 18)) +
        ggtitle("Subtypes of Traffic Accidents - Roughly One Third \n of the 100,000 Accidents in the Dataset") +
        scale_fill_discrete(name = "Miscellaneous Traffic Type")
        
pie_traffic_misc

denver_cols_map_traffic_2_plot <-ggplot(denver_crimes_codes_joined_is_isnt_crime_2, aes(x=OFFENSE_TYPE_ID, y=total, fill=OFFENSE_TYPE_ID)) + 
        geom_bar(stat="identity") + theme(legend.position = "none", axis.text.x = element_text(angle=45, vjust=1, hjust=0.5)) +
        ggtitle("Distribution of Different Categories of Miscellanous Traffic Related Incidents") + ylab("Total Number of Crimes") + xlab("Category of Crime")


denver_crimes_codes_joined_is_isnt_crime_2_map <- denver_crimes_codes_joined_2 %>%
        filter(IS_TRAFFIC ==1, OFFENSE_TYPE_ID != "traffic-accident", GEO_LON >= -105.3218 & GEO_LON <= -104.6096839) 


denver_misc_traffic_mapped_layered_2 <- ggmap(denver_area_map) + geom_point(data=denver_crimes_codes_joined_is_isnt_crime_2_map, 
                                                                      aes(x=GEO_LON, y=GEO_LAT, 
                                                                          color = OFFENSE_TYPE_ID))

##Exploratory Analysis in diving deeper - TRAFFIC mapping
denver_cols_map_traffic<- denver_crime_datetime_separate_2 %>%
        filter(IS_CRIME == 0)

denver_cols_map_traffic$summary_box <- paste("<b>Incident #: </b>", denver_cols_map_traffic$incident_id,
                                             "<br>", "<b>Incident Address: </b>", denver_cols_map_traffic$INCIDENT_ADDRESS,
                                             "<br>", "<b>Category: </b>", denver_cols_map_traffic$OFFENSE_CATEGORY_ID,
                                             "<br>", "<b>Day of the week: </b>", denver_cols_map_traffic$dayofweek,
                                             "<br>", "<b>Date: </b>", denver_cols_map_traffic$date,
                                             "<br>", "<b>Time: </b>", denver_cols_map_traffic$time,
                                             "<br>", "<b>Denver Neighborhood: </b>", denver_cols_map_traffic$NEIGHBORHOOD_ID,
                                             "<br>", "<b>Denver Police district ID #: </b>", denver_cols_map_traffic$DISTRICT_ID,
                                             "<br>", "<b>Longitude: </b>", denver_cols_map_traffic$GEO_LON,
                                             "<br>", "<b>Latitude: </b>", denver_cols_map_traffic$GEO_LAT)

leaflet(denver_cols_map_traffic, width = "100%") %>% 
        addTiles() %>%
        addTiles(group = "OSM (default)") %>%
        addProviderTiles(provider = "Esri.WorldStreetMap",group = "World StreetMap") %>%
        addProviderTiles(provider = "Esri.WorldImagery",group = "World Imagery") %>%
        addMarkers(lng = ~GEO_LON, lat = ~GEO_LAT, popup = denver_cols_map_traffic$summary_box, clusterOptions = markerClusterOptions()) %>%
        addLayersControl(
                baseGroups = c("OSM (default)","World StreetMap", "World Imagery"),
                options = layersControlOptions(collapsed = FALSE)) 

#CRIME FROM 1/17-3/22
df_crime_over_time <- denver_cols_map_crime %>%
        group_by(date) %>%
        dplyr::summarize(total = n()) %>%
        arrange(date)

df_crime_over_time_plot <- ggplot(df_crime_over_time, aes(x = date, y = total)) +
        geom_line(color = "purple", size = 0.05) +
        geom_smooth(color = "navy") +
        scale_x_date(breaks = date_breaks("1 year"), labels = date_format("%Y")) +
        xlab("Date of Crime (Year)") + ylab("Number of Crimes Committed") + 
        ggtitle("Denver: Daily Number of Crimes Committed from 1/2017 - 3/2022") +
        theme(axis.text.x = element_text(angle=30, vjust=.5, hjust=1))

df_crime_over_time_plot

#Crime Mapped
unique(denver_crimes_codes_joined_2$NEIGHBORHOOD_ID)

denver_crimes_mapped_2 <- denver_crimes_codes_joined_2 %>%
        filter(IS_TRAFFIC == 0)

denver_crimes_mapped_layered_2 <- ggmap(denver_area_map) + geom_point(data=denver_crimes_mapped_2, 
                                                                    aes(x=GEO_LON, y=GEO_LAT, 
                                                                        color =OFFENSE_CATEGORY_NAME))

denver_crimes_mapped_layered_2 <- ggmap(denver_area_map) + 
        stat_density2d(data = denver_crimes_mapped_2, aes(x = GEO_LON, y = GEO_LAT, fill = ..density..), geom = 'tile', contour = F, alpha = .5) +  
        scale_fill_viridis()

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
                                            data = denver_crimes_mapped_2)


denver_crimes_mapped_layered_2 <- ggmap(denver_area_map) + geom_point(data=denver_crimes_mapped_2, 
                                                                    aes(x=GEO_LON, y=GEO_LAT, 
                                                                        color = OFFENSE_CATEGORY_ID))

##Exploratory Analysis in diving deeper - CRIMES mapping
denver_crime_datetime_separate_2 <- denver_crimes_codes_joined_2 %>%
        mutate(FIRST_OCCURRENCE_DATE= mdy_hms(FIRST_OCCURRENCE_DATE),
               day = day(FIRST_OCCURRENCE_DATE),
               month = month(FIRST_OCCURRENCE_DATE),
               year = year(FIRST_OCCURRENCE_DATE),
               dayofweek = wday(FIRST_OCCURRENCE_DATE),
               minute = wday(FIRST_OCCURRENCE_DATE),
               second = second(FIRST_OCCURRENCE_DATE))

denver_crime_datetime_separate_2$date <- as.Date(denver_crime_datetime_separate_2$FIRST_OCCURRENCE_DATE) 

denver_crime_datetime_separate_2$time <- format(as.POSIXct(denver_crime_datetime_separate_2$FIRST_OCCURRENCE_DATE),    
                                                format = "%H:%M:%S")

View(denver_crime_datetime_separate_2)

first_500_denver_crime_datetime_separate_2 <- denver_crime_datetime_separate_2[1:500,]
first_500_denver_crime_datetime_separate_2$time <- as.character(first_500_denver_crime_datetime_separate_2$time) 
first_500_denver_crime_datetime_separate_2

datatable(first_500_denver_crime_datetime_separate_2, options = list(pageLength = 25,scrollX='400px'))


denver_crime_datetime_separate_2 <- denver_crime_datetime_separate_2 %>%
        filter(GEO_LON >= -105.3218 & GEO_LON <= -104.6096839)

denver_cols_map_crime<- denver_crime_datetime_separate_2 %>%
        filter(IS_TRAFFIC == 0)

denver_cols_map_crime$summary_box <- paste("<b>Incident #: </b>", denver_cols_map_crime$incident_id,
                                     "<br>", "<b>Incident Address: </b>", denver_cols_map_crime$INCIDENT_ADDRESS,
                                     "<br>", "<b>Category: </b>", denver_cols_map_crime$OFFENSE_CATEGORY_ID,
                                     "<br>", "<b>Day of the week: </b>", denver_cols_map_crime$dayofweek,
                                     "<br>", "<b>Date: </b>", denver_cols_map_crime$date,
                                     "<br>", "<b>Time: </b>", denver_cols_map_crime$time,
                                     "<br>", "<b>Denver Neighborhood: </b>", denver_cols_map_crime$NEIGHBORHOOD_ID,
                                     "<br>", "<b>Denver Police district ID #: </b>", denver_cols_map_crime$DISTRICT_ID,
                                     "<br>", "<b>Longitude: </b>", denver_cols_map_crime$GEO_LON,
                                     "<br>", "<b>Latitude: </b>", denver_cols_map_crime$GEO_LAT)

        
leaflet(denver_cols_map_crime, width = "100%") %>% 
        addTiles() %>%
        addTiles(group = "OSM (default)") %>%
        addProviderTiles(provider = "Esri.WorldStreetMap",group = "World StreetMap") %>%
        addProviderTiles(provider = "Esri.WorldImagery",group = "World Imagery") %>%
        addMarkers(lng = ~GEO_LON, lat = ~GEO_LAT, popup = denver_cols_map_crime$summary_box, clusterOptions = markerClusterOptions()) %>%
        addLayersControl(
                baseGroups = c("OSM (default)","World StreetMap", "World Imagery"),
                options = layersControlOptions(collapsed = FALSE))       
                # addProviderTiles(provider = "NASAGIBS.ViirsEarthAtNight2012",group = "Nighttime Imagery") %>%


#Type of CategoryID
denver_cols_map_crime_category_id <- sort(table(denver_cols_map_crime$OFFENSE_CATEGORY_ID),decreasing = TRUE)
denver_cols_map_crime_category_id <- data.frame(denver_cols_map_crime_category_id[denver_cols_map_crime_category_id > 100])
colnames(denver_cols_map_crime_category_id) <- c("Category_of_Crime", "Number_of_Crimes")
denver_cols_map_crime_category_id$percentage <- denver_cols_map_crime_category_id$Number_of_Crimes / sum(denver_cols_map_crime_category_id$Number_of_Crimes)


barchart_different_crime_types <-ggplot(denver_cols_map_crime_category_id, aes(x=Category_of_Crime, y=Number_of_Crimes, fill=Category_of_Crime)) + 
        geom_bar(stat="identity") + theme(legend.position = "none", axis.text.x = element_text(angle=45, vjust=1, hjust=0.5)) +
        ggtitle("Distribution of Different Categories of Crimes") + ylab("Total Number of Crimes") + xlab("Category of Crime")

barchart_different_crime_types

#Piechart of Category_ID of Crime
revised_category_pie_position <- denver_cols_map_crime_category_id %>% 
        mutate(cum_sum = rev(cumsum(rev(percentage))), 
               position = percentage/2 + lead(cum_sum, 1),
               position = if_else(is.na(position), percentage/2, position))

crime_category_info<-ggplot(denver_cols_map_crime_category_id, aes(x="", y=percentage, fill=Category_of_Crime)) + 
        geom_bar(stat="identity", color = "white", width=1) + 
        geom_text(label = "", position = position_stack(vjust=0.5)) + 
        geom_label_repel(data = revised_category_pie_position, aes(y=position, label = paste0((round(percentage*100, 1)), "%")),
                         size = 4, nudge_x = 1, show.legend = FALSE) +
        ggtitle("Distribution of the Different Categories of Crime") +
        theme(panel.background = element_blank(),
              axis.line = element_blank(),
              axis.text = element_blank(),
              axis.ticks = element_blank(),
              axis.title = element_blank(),
              plot.title = element_text(hjust = 0.5, size = 18)) 


crime_category_pie <- crime_category_info + coord_polar("y", start=pi/3) 
crime_category_pie

##Examining 'All other Crimes' CATEGORY_ID
denver_cols_map_crime5 <- denver_cols_map_crime %>%
        filter(OFFENSE_CATEGORY_ID == "all-other-crimes")

unique(denver_cols_map_crime5$OFFENSE_TYPE_ID)

denver_cols_map_crime6 <- denver_cols_map_crime5 %>%
        group_by(OFFENSE_TYPE_ID) %>%
        dplyr::summarize(total = n()) %>%
        slice_max(order_by = total, n = 12)

denver_cols_map_crime6_all_other_crimes <- sort(table(denver_cols_map_crime5$OFFENSE_TYPE_ID),decreasing = TRUE)
denver_cols_map_crime6_all_other_crimes <- data.frame(denver_cols_map_crime6_all_other_crimes[denver_cols_map_crime6_all_other_crimes > 100])
colnames(denver_cols_map_crime6_all_other_crimes) <- c("Type_of_Offense", "Number_of_Crimes")
denver_cols_map_crime6_all_other_crimes$percentage <- denver_cols_map_crime6_all_other_crimes$Number_of_Crimes / sum(denver_cols_map_crime6_all_other_crimes$Number_of_Crimes)


top_12_offense_type_all_other_crimes <- ggplot(denver_cols_map_crime6, aes(x = reorder(OFFENSE_TYPE_ID, -total), y=total, fill=OFFENSE_TYPE_ID)) +
        geom_bar(stat = "identity") + ggtitle("Top 12 Descriptions of 'all other crimes'") + 
        theme(legend.position = "none", axis.text.x = element_text(angle=45, vjust=1, hjust=0.5)) +
        xlab("Type of Crime") + ylab("Total Number of Crimes")

#Piechart of 'All other Crimes'
revised_all_other_pie_position <- denver_cols_map_crime6_all_other_crimes_12 %>% 
        mutate(cum_sum = rev(cumsum(rev(percentage))), 
               position = percentage/2 + lead(cum_sum, 1),
               position = if_else(is.na(position), percentage/2, position))

denver_cols_map_crime6_all_other_crimes_12 <- denver_cols_map_crime6_all_other_crimes %>%
        slice_max(order_by = Number_of_Crimes, n = 12)

pie_all_other_crimes <- ggplot(denver_cols_map_crime6_all_other_crimes_12, aes(x="", y=percentage, fill=Type_of_Offense)) + 
        geom_bar(stat="identity", color = "white", width=1) + 
        geom_text(label = "", position = position_stack(vjust=0.5)) + 
        geom_label_repel(data = revised_all_other_pie_position, aes(y=position, label = paste0((round(percentage*100, 1)), "%")),
                         size = 4, nudge_x = 1, show.legend = FALSE) +
        ggtitle("Distribution of Offenses under 'All Other Crimes'") +
        theme(panel.background = element_blank(),
              axis.line = element_blank(),
              axis.text = element_blank(),
              axis.ticks = element_blank(),
              axis.title = element_blank(),
              plot.title = element_text(hjust = 0.5, size = 18)) 

pie_all_other_crimes_fin <- pie_all_other_crimes + coord_polar("y", start=pi/3) 
pie_all_other_crimes_fin

##Examining 'Other Crimes Against Persons' CATEGORY_ID
denver_cols_map_crime7 <- denver_cols_map_crime %>%
        filter(OFFENSE_CATEGORY_ID == "other-crimes-against-persons")

unique(denver_cols_map_crime7$OFFENSE_TYPE_ID)

denver_cols_map_crime8 <- denver_cols_map_crime7 %>%
        group_by(OFFENSE_TYPE_ID) %>%
        dplyr::summarize(total = n()) %>%
        slice_max(order_by = total, n = 4)

denver_cols_map_crime7_other_crimes_persons <- sort(table(denver_cols_map_crime7$OFFENSE_TYPE_ID),decreasing = TRUE)
denver_cols_map_crime7_other_crimes_persons <- data.frame(denver_cols_map_crime7_other_crimes_persons[denver_cols_map_crime7_other_crimes_persons > 100])
colnames(denver_cols_map_crime7_other_crimes_persons) <- c("Type_of_Offense", "Number_of_Crimes")
denver_cols_map_crime7_other_crimes_persons$percentage <- denver_cols_map_crime7_other_crimes_persons$Number_of_Crimes / sum(denver_cols_map_crime7_other_crimes_persons$Number_of_Crimes)


top_other_crimes_against_person <- ggplot(denver_cols_map_crime8, aes(x = reorder(OFFENSE_TYPE_ID, -total), y=total, fill=OFFENSE_TYPE_ID)) +
        geom_bar(stat = "identity") + ggtitle("Top Descriptions of 'other crimes against persons'") + 
        theme(legend.position = "none", axis.text.x = element_text(angle=45, vjust=0.8, hjust=0.8)) +
        xlab("Type of Crime") + ylab("Total Number of Crimes")

#Piechart Other Crimes Against Persons
revised_other_crimes_persons_pie_position <- denver_cols_map_crime7_other_crimes_persons %>% 
        mutate(cum_sum = rev(cumsum(rev(percentage))), 
               position = percentage/2 + lead(cum_sum, 1),
               position = if_else(is.na(position), percentage/2, position))

pie_other_crimes_persons <- ggplot(denver_cols_map_crime7_other_crimes_persons, aes(x="", y=percentage, fill=Type_of_Offense)) + 
        geom_bar(stat="identity", color = "white", width=1) + 
        geom_text(label = "", position = position_stack(vjust=0.5)) + 
        geom_label_repel(data = revised_other_crimes_persons_pie_position, aes(y=position, label = paste0((round(percentage*100, 1)), "%")),
                         size = 4, nudge_x = 1, show.legend = FALSE) +
        ggtitle("Distribution of Offenses under 'Other Crimes Against Persons'") +
        theme(panel.background = element_blank(),
              axis.line = element_blank(),
              axis.text = element_blank(),
              axis.ticks = element_blank(),
              axis.title = element_blank(),
              plot.title = element_text(hjust = 0.5, size = 18)) 

pie_other_crimes_persons_fin <- pie_other_crimes_persons + coord_polar("y", start=pi/3) 
pie_other_crimes_persons_fin

#PUBLIC DISORDER EXAMINATION
denver_cols_map_crime9 <- denver_cols_map_crime %>%
        filter(OFFENSE_CATEGORY_ID == "public-disorder")

unique(denver_cols_map_crime9$OFFENSE_TYPE_ID)

denver_cols_map_crime10 <- denver_cols_map_crime9 %>%
        group_by(OFFENSE_TYPE_ID) %>%
        dplyr::summarize(total = n()) %>%
        slice_max(order_by = total, n = 12)

denver_cols_map_crime9_public_disorder<- sort(table(denver_cols_map_crime9$OFFENSE_TYPE_ID),decreasing = TRUE)
denver_cols_map_crime9_public_disorder <- data.frame(denver_cols_map_crime9_public_disorder[denver_cols_map_crime9_public_disorder > 100])
colnames(denver_cols_map_crime9_public_disorder) <- c("Type_of_Offense", "Number_of_Crimes")
denver_cols_map_crime9_public_disorder$percentage <- denver_cols_map_crime9_public_disorder$Number_of_Crimes / sum(denver_cols_map_crime9_public_disorder$Number_of_Crimes)


public_disorder_crimes <- ggplot(denver_cols_map_crime10, aes(x = reorder(OFFENSE_TYPE_ID, -total), y=total, fill=OFFENSE_TYPE_ID)) +
        geom_bar(stat = "identity") + ggtitle("Top Descriptions of Types of Public Disorder") + 
        theme(legend.position = "none", axis.text.x = element_text(angle=45, vjust=0.8, hjust=0.8)) +
        xlab("Type of Crime") + ylab("Total Number of Crimes")

#Piechart Public Disorder
revised_public_disorder_pie_position <- denver_cols_map_crime9_public_disorder %>% 
        mutate(cum_sum = rev(cumsum(rev(percentage))), 
               position = percentage/2 + lead(cum_sum, 1),
               position = if_else(is.na(position), percentage/2, position))

pie_public_disorder<- ggplot(denver_cols_map_crime9_public_disorder, aes(x="", y=percentage, fill=Type_of_Offense)) + 
        geom_bar(stat="identity", color = "white", width=1) + 
        geom_text(label = "", position = position_stack(vjust=0.5)) + 
        geom_label_repel(data = revised_public_disorder_pie_position, aes(y=position, label = paste0((round(percentage*100, 1)), "%")),
                         size = 4, nudge_x = 1, show.legend = FALSE) +
        ggtitle("Distribution of Offenses under 'Public Disorder'") +
        theme(panel.background = element_blank(),
              axis.line = element_blank(),
              axis.text = element_blank(),
              axis.ticks = element_blank(),
              axis.title = element_blank(),
              plot.title = element_text(hjust = 0.5, size = 18)) 

pie_public_disorder_fin <- pie_public_disorder + coord_polar("y", start=pi/3) 
pie_public_disorder_fin

#LARCENY Examination
denver_cols_map_crime_larceny <- denver_cols_map_crime %>%
        filter(OFFENSE_CATEGORY_ID == "larceny")

unique(denver_cols_map_crime_larceny$OFFENSE_TYPE_ID)

denver_cols_map_crime_larceny_1 <- denver_cols_map_crime_larceny %>%
        group_by(OFFENSE_TYPE_ID) %>%
        dplyr::summarize(total = n())

denver_cols_map_crime_larceny<- sort(table(denver_cols_map_crime_larceny$OFFENSE_TYPE_ID),decreasing = TRUE)
denver_cols_map_crime_larceny <- data.frame(denver_cols_map_crime_larceny[denver_cols_map_crime_larceny > 100])
colnames(denver_cols_map_crime_larceny) <- c("Type_of_Offense", "Number_of_Crimes")
denver_cols_map_crime_larceny$percentage <- denver_cols_map_crime_larceny$Number_of_Crimes / sum(denver_cols_map_crime_larceny$Number_of_Crimes)


top_larceny_crimes <- ggplot(denver_cols_map_crime_larceny_1, aes(x = reorder(OFFENSE_TYPE_ID, -total), y=total, fill=OFFENSE_TYPE_ID)) +
        geom_bar(stat = "identity") + ggtitle("Top Descriptions of Types of Larceny") + 
        theme(legend.position = "none", axis.text.x = element_text(angle=45, vjust=0.8, hjust=0.8)) +
        xlab("Type of Crime") + ylab("Total Number of Crimes")

#Piechart Larceny
revised_larceny_pie_position <- denver_cols_map_crime_larceny %>% 
        mutate(cum_sum = rev(cumsum(rev(percentage))), 
               position = percentage/2 + lead(cum_sum, 1),
               position = if_else(is.na(position), percentage/2, position))

pie_larceny <- ggplot(denver_cols_map_crime_larceny, aes(x="", y=percentage, fill=Type_of_Offense)) + 
        geom_bar(stat="identity", color = "white", width=1) + 
        geom_text(label = "", position = position_stack(vjust=0.5)) + 
        geom_label_repel(data = revised_larceny_pie_position, aes(y=position, label = paste0((round(percentage*100, 1)), "%")),
                         size = 4, nudge_x = 1, show.legend = FALSE) +
        ggtitle("Distribution of Offenses under 'Larceny'") +
        theme(panel.background = element_blank(),
              axis.line = element_blank(),
              axis.text = element_blank(),
              axis.ticks = element_blank(),
              axis.title = element_blank(),
              plot.title = element_text(hjust = 0.5, size = 18)) 

pie_larceny_fin <- pie_larceny + coord_polar("y", start=pi/3) 
pie_larceny_fin

#Burglary Exploration
denver_cols_map_crime_burglary <- denver_cols_map_crime %>%
        filter(OFFENSE_CATEGORY_ID == "burglary")

unique(denver_cols_map_crime_burglary$OFFENSE_TYPE_ID)

denver_cols_map_crime_burglary_1 <- denver_cols_map_crime_burglary %>%
        group_by(OFFENSE_TYPE_ID) %>%
        dplyr::summarize(total = n())

denver_cols_map_crime_burglary<- sort(table(denver_cols_map_crime_burglary$OFFENSE_TYPE_ID),decreasing = TRUE)
denver_cols_map_crime_burglary <- data.frame(denver_cols_map_crime_burglary[denver_cols_map_crime_burglary > 80])
colnames(denver_cols_map_crime_burglary) <- c("Type_of_Offense", "Number_of_Crimes")
denver_cols_map_crime_burglary$percentage <- denver_cols_map_crime_burglary$Number_of_Crimes / sum(denver_cols_map_crime_burglary$Number_of_Crimes)

burglary_crimes_plot <- ggplot(denver_cols_map_crime_burglary_1, aes(x = reorder(OFFENSE_TYPE_ID, -total), y=total, fill=OFFENSE_TYPE_ID)) +
        geom_bar(stat = "identity") + ggtitle("Top Descriptions of Types of Burglaries") + 
        theme(legend.position = "none", axis.text.x = element_text(angle=45, vjust=0.8, hjust=0.8)) +
        xlab("Type of Burglary") + ylab("Total Number of Crimes")


#Pie Chart Burglary
revised_burglary_pie_position <- denver_cols_map_crime_burglary %>% 
        mutate(cum_sum = rev(cumsum(rev(percentage))), 
               position = percentage/2 + lead(cum_sum, 1),
               position = if_else(is.na(position), percentage/2, position))

pie_burglary <- ggplot(denver_cols_map_crime_burglary, aes(x="", y=percentage, fill=Type_of_Offense)) + 
        geom_bar(stat="identity", color = "white", width=1) + 
        geom_text(label = "", position = position_stack(vjust=0.5)) + 
        geom_label_repel(data = revised_burglary_pie_position, aes(y=position, label = paste0((round(percentage*100, 1)), "%")),
                         size = 4, nudge_x = 1, show.legend = FALSE) +
        ggtitle("Distribution of Types of Burglary'") +
        theme(panel.background = element_blank(),
              axis.line = element_blank(),
              axis.text = element_blank(),
              axis.ticks = element_blank(),
              axis.title = element_blank(),
              plot.title = element_text(hjust = 0.5, size = 18)) 

pie_burglary_fin <- pie_burglary + coord_polar("y", start=pi/3) 
pie_burglary_fin

#Drug-Alcohol Exploration
denver_cols_map_crime_drug_alcohol <- denver_cols_map_crime %>%
        filter(OFFENSE_CATEGORY_ID == "drug-alcohol")

unique(denver_cols_map_crime_drug_alcohol$OFFENSE_TYPE_ID)

denver_cols_map_crime_drug_alcohol_1 <- denver_cols_map_crime_drug_alcohol %>%
        group_by(OFFENSE_TYPE_ID) %>%
        dplyr::summarize(total = n()) 

denver_cols_map_crime_drug_alcohol_2 <- denver_cols_map_crime_drug_alcohol_1 %>%
        slice_max(order_by = total, n=19)

denver_cols_map_crime_drug_alcohol<- sort(table(denver_cols_map_crime_drug_alcohol$OFFENSE_TYPE_ID),decreasing = TRUE)
denver_cols_map_crime_drug_alcohol <- data.frame(denver_cols_map_crime_drug_alcohol[denver_cols_map_crime_drug_alcohol > 100])
colnames(denver_cols_map_crime_drug_alcohol) <- c("Type_of_Offense", "Number_of_Crimes")
denver_cols_map_crime_drug_alcohol$percentage <- denver_cols_map_crime_drug_alcohol$Number_of_Crimes / sum(denver_cols_map_crime_drug_alcohol$Number_of_Crimes)


drugs_alcohol_crimes_plot <- ggplot(denver_cols_map_crime_drug_alcohol_2, aes(x = reorder(OFFENSE_TYPE_ID, -total), y=total, fill=OFFENSE_TYPE_ID)) +
        geom_bar(stat = "identity") + ggtitle("Top 19 Descriptions of Types of Drug-Alcohol Crimes") + 
        theme(legend.position = "none", axis.text.x = element_text(angle=90, vjust=0.8, hjust=0.8)) +
        xlab("Type of Crime") + ylab("Total Number of Crimes")

#Piechart Alcohol-Drugs
revised_drug_alcohol_pie_position <- denver_cols_map_crime_drug_alcohol %>% 
        mutate(cum_sum = rev(cumsum(rev(percentage))), 
               position = percentage/2 + lead(cum_sum, 1),
               position = if_else(is.na(position), percentage/2, position))

pie_drugs_alcohol <- ggplot(denver_cols_map_crime_drug_alcohol, aes(x="", y=percentage, fill=Type_of_Offense)) + 
        geom_bar(stat="identity", color = "white", width=1) + 
        geom_text(label = "", position = position_stack(vjust=0.5)) + 
        geom_label_repel(data = revised_drug_alcohol_pie_position, aes(y=position, label = paste0((round(percentage*100, 1)), "%")),
                         size = 4, nudge_x = 1, show.legend = FALSE) +
        ggtitle("Distribution of Types of Drug/Alcohol Crimes'") +
        theme(panel.background = element_blank(),
              axis.line = element_blank(),
              axis.text = element_blank(),
              axis.ticks = element_blank(),
              axis.title = element_blank(),
              plot.title = element_text(hjust = 0.5, size = 18)) 

pie_drugs_alcohol_fin <- pie_drugs_alcohol + coord_polar("y", start=pi/3) 
pie_drugs_alcohol_fin

#Aggravated Assault Exploration 
denver_cols_map_crime_agr_assault <- denver_cols_map_crime %>%
        filter(OFFENSE_CATEGORY_ID == "aggravated-assault")

unique(denver_cols_map_crime_agr_assault$OFFENSE_TYPE_ID)

denver_cols_map_crime_agr_assault_1 <- denver_cols_map_crime_agr_assault %>%
        group_by(OFFENSE_TYPE_ID) %>%
        dplyr::summarize(total = n())

denver_cols_map_crime_agr_assault<- sort(table(denver_cols_map_crime_agr_assault$OFFENSE_TYPE_ID),decreasing = TRUE)
denver_cols_map_crime_agr_assault <- data.frame(denver_cols_map_crime_agr_assault[denver_cols_map_crime_agr_assault > 100])
colnames(denver_cols_map_crime_agr_assault) <- c("Type_of_Offense", "Number_of_Crimes")
denver_cols_map_crime_agr_assault$percentage <- denver_cols_map_crime_agr_assault$Number_of_Crimes / sum(denver_cols_map_crime_agr_assault$Number_of_Crimes)


aggr_assault_crimes_plot <- ggplot(denver_cols_map_crime_agr_assault_1, aes(x = reorder(OFFENSE_TYPE_ID, -total), y=total, fill=OFFENSE_TYPE_ID)) +
        geom_bar(stat = "identity") + ggtitle("Top Descriptions of Types of Aggravated Assault") + 
        theme(legend.position = "none", axis.text.x = element_text(angle=45, vjust=0.8, hjust=0.8)) +
        xlab("Type of Crime") + ylab("Total Number of Crimes")

#Pie Chart Aggravated Assault
revised_agg_assault_pie_position <- denver_cols_map_crime_agr_assault %>% 
        mutate(cum_sum = rev(cumsum(rev(percentage))), 
               position = percentage/2 + lead(cum_sum, 1),
               position = if_else(is.na(position), percentage/2, position))

pie_agg_assault <- ggplot(denver_cols_map_crime_agr_assault, aes(x="", y=percentage, fill=Type_of_Offense)) + 
        geom_bar(stat="identity", color = "white", width=1) + 
        geom_text(label = "", position = position_stack(vjust=0.5)) + 
        geom_label_repel(data = revised_agg_assault_pie_position, aes(y=position, label = paste0((round(percentage*100, 1)), "%")),
                         size = 4, nudge_x = 1, show.legend = FALSE) +
        ggtitle("Distribution of Types of Aggravated Assault") +
        theme(panel.background = element_blank(),
              axis.line = element_blank(),
              axis.text = element_blank(),
              axis.ticks = element_blank(),
              axis.title = element_blank(),
              plot.title = element_text(hjust = 0.5, size = 18)) 

pie_agg_assault_fin <- pie_agg_assault + coord_polar("y", start=pi/3) 
pie_agg_assault_fin

