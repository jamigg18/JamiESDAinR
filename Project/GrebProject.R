#loading packages
library(sf)
library(tidyverse)
library(tmap)

# my data
stream.health.1217 <- sf::read_sf("./Stream_Health_Ratings_in_the_Chesapeake_Bay_Watershed_First_Interval_(2012-2017).shp") %>% sf::st_make_valid()
stream.health.0005 <- sf::read_sf("./Stream_Health_Pre_Baseline.shp") %>% sf::st_make_valid()
dam.removal.8911 <- sf::read_sf("./Streams_Opened_by_Dam_Removals_2011_Baseline.shp") %>% sf::st_make_valid()
glimpse(stream.health.0005)
glimpse(stream.health.1217)
glimpse(dam.removal.8911)

#checking crs
st_crs(dam.removal.8911) == st_crs(stream.health.0005)


#removing NAs from stream health dfs
health.pre06 <- stream.health.0005 %>% na.omit(Rating)
glimpse(health.pre06)
health.post11 <- stream.health.1217 %>% na.omit(Rating)
glimpse(health.post11)

#filtering to 2006-2011
dam.removals <- dam.removal.8911 %>% dplyr::filter(., DamRemoval > 2005)


#exploratory analysis
pre_hist <- health.pre06 %>% ggplot(.,aes(x = Rating)) +
  geom_bar(fill = "purple4", na.rm = TRUE) +
  theme_classic() +
  labs(title = "Stream Health Ratings 2000-2005", 
       subtitle = "in Chesapeake Bay Watershed",
       x = "Quality Rating",
       y = "Count")
pre_hist
post_hist <- health.post11 %>% ggplot(.,aes(x = Rating)) +
  geom_bar(fill = "purple4", na.rm = TRUE) +
  theme_classic() +
  labs(title = "Stream Health Ratings 2012-2017", 
       subtitle = "in Chesapeake Bay Watershed",
       x = "Quality Rating",
       y = "Count")
post_hist
library(gridExtra)
grid.arrange(pre_hist, post_hist, ncol=1)

hist(dam.removals$DamRemoval)




#mapping together
health.pre06 %>% tm_shape(.) + tm_polygons(fill = "Rating") +
  tm_shape(dam.removals) + tm_lines()

#map overlay post dam removal
health.post11 %>% tm_shape(.) + tm_polygons(fill = "Rating") +
  tm_shape(dam.removals) + tm_lines()


#intersections
ratings.with.dams <- st_intersection(dam.removals, health.pre06)
glimpse(ratings.with.dams)
ratings.after.removal <- st_intersection(dam.removals, health.post11)

#visualizing
map_predam <- tm_shape(ratings.with.dams)+ tm_lines(col = "Rating")
map_postdam <- tm_shape(ratings.after.removal)+ tm_lines(col = "Rating")
map_predam
map_postdam
tmap_arrange(map_predam, map_postdam)


# removing duplicates
pre.dam <- ratings.with.dams %>% na.omit(GNIS_NAME) %>% distinct(GNIS_NAME, .keep_all = TRUE)
post.dam <- ratings.after.removal %>% na.omit(GNIS_NAME) %>% distinct(GNIS_NAME, .keep_all = TRUE)
glimpse(pre.dam)
glimpse(post.dam)

#lets make simpler tables please
pre.dam.small <- pre.dam %>% mutate(rating05 = Rating) %>% select(GNIS_NAME, rating05)
glimpse(pre.dam.small)
post.dam.small <- post.dam %>% mutate(rating12 = Rating) %>% select(GNIS_NAME, rating12)
glimpse(post.dam.small)

#join them and remove whats not in both datasets
health.data <- st_join(pre.dam.small, post.dam.small, by = "GIS_NAME") %>%
  na.omit(GNIS_NAME.y)
health.data
glimpse(health.data)


##analyze based on rating
#turn the ratings into numbers
numeric.rating <- c("excellent" = 5, "good" = 4, "fair" = 3, "poor" = 2, "very_poor" = 1)
health.data$numrating05 <- numeric.rating[health.data$rating05]
health.data$numrating12 <- numeric.rating[health.data$rating12]
glimpse(health.data)


#make new column of the rating difference
health.data$rating.change <- health.data$numrating12 - health.data$numrating05
glimpse(health.data)
#histogram
health.data %>% ggplot(.,aes(x = rating.change)) +
  geom_bar(fill = "purple4") +
  theme_classic() +
  labs(title = "Change in Stream Health Ratings Before and After Dam Removal", 
       subtitle = "in Chesapeake Bay Watershed",
       x = "Difference in Rating",
       y = "Count")


#map the change
tmap::ttm()
health.data %>% tm_shape(.) + tm_lines(lwd = 3, col = "rating.change",
        palette=c("red","black","gold","green","green4"))


#table of how many streams' ratings changed
health.data$rating.change.chr <- as.character(health.data$rating.change)
chr.rating.change <- c("-1" = "worsened by one rating", "0" = "rating did not change",
                       "1" = "improved by one rating", "2" = "improved by two ratings",
                       "3" = "improved by three ratings", "4" = "improved by four ratings")
health.data$rate.chng <- chr.rating.change[health.data$rating.change.chr]
glimpse(health.data)
health.data %>% count(rate.chng)
1+5+51+1
changes <- c("improved", "did not change", "worsened")
counts <- c("58", "37", "1")
change <- data.frame("RatingChange" = changes , "StreamCount" = counts)
change
tableGrob(change) %>% grid.arrange()
