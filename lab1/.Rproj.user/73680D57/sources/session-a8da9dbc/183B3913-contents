#step 1
library(tidyverse)
library(ggplot2)
library(sf)
p.counties <- "./County_Boundaries.shp"
p.stations <- "./Non-Tidal_Water_Quality_Monitoring_Stations_in_the_Chesapeake_Bay.shp"
d.counties <- sf::read_sf(p.counties) %>% st_make_valid()
d.stations <- sf::read_sf(p.stations)
glimpse(d.counties)
glimpse(d.stations)
d.stations %>% sf::st_is_valid()
d.counties %>% sf::st_is_valid()
d.counties %>% dplyr::select(GEOID10, ALAND10) %>% head()
d.counties %>% dplyr::select(-NAME10) %>% head()
d.counties %>% dplyr::select(GEOID10:CLASSFP10) %>% head()
d.counties %>% dplyr::select(-(GEOID10:CLASSFP10)) %>% head()
d.counties %>% dplyr::select(starts_with("C"))     

#grouping data
d.counties <- d.counties %>% group_by(STATEFP10) %>% mutate(statelandarea = sum(ALAND10))
d.counties %>% as_tibble() %>% dplyr::select(-geometry) %>% group_by(STATEFP10) %>% summarise(statelandarea=sum(ALAND10))

#plots
d.counties %>% ggplot(.,aes(x=as.factor(STATEFP10), y=ALAND10))+
  geom_boxplot(aes(fill = STATEFP10))
d.counties %>% ggplot(.,aes(x=ALAND10))+
  geom_histogram(aes(fill = STATEFP10))+
  labs(title = "a silly little plot")

#spatial operations
d.counties %>% sf::st_crs()
d.stations %>% sf::st_crs()
d.counties %>% sf::st_crs() == d.stations %>% sf::st_crs()
del.counties <- d.counties %>% dplyr::filter(STATEFP10 == 10)
del.stations <- sf::st_intersection(d.stations, del.counties)
glimpse(del.stations)
plot(del.stations)
del.counties %>% st_area()


#TASKS

#Task 1: basic data manipulation
##calculating the percent of land area for each county out of the total area of the state
d.counties <- d.counties %>% group_by(STATEFP10) %>% mutate(StateArea = sum(ALAND10+AWATER10))
d.counties <- d.counties %>% mutate(percentLandA = ((ALAND10/StateArea)*100))
glimpse(d.counties$percentLandA)
##which county has the most water per land in each state
d.counties %>% dplyr::slice_max(AWATER10/statelandarea)
##how many counties in each state (how many rows/obs does each state have)
d.counties %>% count(.,STATEFP10)
##which station has the shortest name
d.stations %>% dplyr::slice_min(STATION_NA)

#Task 2: plotting attribute data
##scatterplot of land area and water area
ggplot(d.counties, aes(x=ALAND10, y=AWATER10))+
  geom_point(color=d.counties$STATEFP10)+
  xlab("Area of Land")+
  ylab("Area of Water")+
  ggtitle("Proportion of land to water area")
##histogram of stations' drainage area
ggplot(d.stations, aes(x=Drainage_A))+
  geom_histogram()+
  xlab("number of stations")+
  ylab("area of drainage")+
  ggtitle("Station Drainage Area")
##again but color by state
library(tidyr)
d.stations <- d.stations %>% separate(col = STATION_NA, into = c("STATION_na", "STATE"), sep = -2)
ggplot(d.stations, aes(x=Drainage_A,fill=STATE))+
  geom_histogram()+
  xlab("number of stations")+
  ylab("area of drainage")+
  ggtitle("Station Drainage Area")

#Task 3: write a function
taskfunct <- function(c) {
  mean<-mean(c)
  med<-median(c)
  max<-max(c)
  min<-min(c)
  sort<-sort(c)
  return(list(order=sort,statsumm=c(mean,med,max,min)))
         }
taskfunct(c(1,0,-1))
taskfunct(c(10,1000,100))
taskfunct(c(.1,.001,1e8))
taskfunct(c("a","b","c"))

#Task4: a more complex spatial analysis
##number of stations per state
d.stations %>% count(STATE)
## calc avg county size in NY
###finding which stateID is NY
which(d.counties$NAME10 == "Broome")
which(d.counties$NAME10 == "Madison")
which(d.counties$NAME10 == "Livingston")
d.counties$STATEFP10[63]
d.counties$STATEFP10[81]
d.counties$STATEFP10[18]
###subsetting NY and finding county area average
nycounties <- d.counties %>% dplyr::filter(STATEFP10 == 36)
nycounties %>% mean(Shape_Area)
mean(nycounties$Shape_Area)
##which state has monitoring stations with the greatest avg drainage area
which(d.stations$Drainage_A == max(d.stations$Drainage_A))                                
d.stations$STATE[28]

#Questions
del.stations <- sf::st_intersection(d.stations, del.counties)
glimpse(del.stations)
maybe.del <- sf::st_intersection(del.counties, d.stations)
glimpse(maybe.del)
