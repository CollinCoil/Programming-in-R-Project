library(tidyverse)
BnB_data <- read_csv("Project_Data\\BnB_data.csv")



# counts of each city

BnB_data %>%
  count(City) %>%
  arrange(desc(n))
BnB_data %>%
  count(room_type) 

#count of weekday vs weekend
BnB_data %>%
  count(Time) %>%
  arrange(desc(n))

#both

BnB_data %>%
  mutate(combine = paste(City, Time, sep = "_")) %>%
  count(combine) %>%
  arrange(desc(n))

#price

BnB_data %>%
  group_by(City, Time) %>%
  summarize(price = mean(realSum))


#plot 1: price bar plot 

x <- BnB_data %>%
  group_by(City, Time) %>%
  summarize(price = mean(realSum)) %>%
  arrange(desc(price))

ggplot(data = x) +
  geom_bar(mapping = aes(x = reorder(City, price), y = price, fill = Time), stat = "identity", position=position_dodge()) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  geom_text(aes(x = City, y = price, label = round(price), group = Time), 
            position = position_dodge(width = .9), vjust = -.5, size = 3, color = "black") + 
  labs(title = "AirBnB Prices in European Cities", y = "Price", x = "City")


#plot 2



#maps stuff
library(tidyverse)
library(ggmap)
library(maps)
library(mapdata)


#Please see https://bar.rady.ucsd.edu/maps.html for more information about
#mapping in R. 

##AMSTERDAM

#find good max for color gradient

amsterdam <- BnB_data %>%
  filter(City == "Amsterdam")
amsterdam_max <- mean(amsterdam$realSum) + 2 * sd(amsterdam$realSum)
amsterdam_max

load("amsterdamMap.RData")


#plots

ggmap(amsterdamMap) + 
  geom_point(data = filter(BnB_data, City == "Amsterdam"),
             mapping = aes(x=lng,y=lat, color = realSum), size = .15) +
  scale_colour_gradient(
    colours=c('red','yellow','green'), 
    limits=c(0,amsterdam_max), 
    oob = scales::squish) +
  labs(title = "Mapping of AirBnB Listings in Amsterdam", y = "Latitude", 
       x = "Longitude", color = "Price") 

#lets zoom this plot

max_lng <- max(amsterdam$lng)
min_lng <- min(amsterdam$lng)
max_lat <- max(amsterdam$lat)
min_lat <- min(amsterdam$lat)

amsterdam_map<- ggmap(amsterdamMap) + 
  geom_point(data = filter(BnB_data, City == "Amsterdam"),
             mapping = aes(x=lng,y=lat, color = realSum), size = .9) +
  scale_colour_gradientn(
    colours=c('red','yellow','green'), 
    limits=c(0,amsterdam_max), 
    oob = scales::squish) +
  scale_x_continuous(limits = c(min_lng, max_lng), expand = c(0, 0)) +
  scale_y_continuous(limits = c(min_lat, max_lat), expand = c(0, 0)) +
  labs(title = "Mapping of AirBnB Listings in Amsterdam", y = "Latitude", 
       x = "Longitude", color = "Price") 
amsterdam_map

#note geom_rect error

##lets make a loop to make the rest of our graphs

##lets load in all of our maps
city_names <- c("Amsterdam", "Athens", "Barcelona", "Berlin", "Budapest", "Lisbon", "London", "Paris", "Rome", "Vienna")
for (name in city_names) {
  load_name <- paste("Maps\\", tolower(name),"Map.RData", sep = "")
  load(load_name)
}

myKey <- "KEY"
register_google(key = myKey, account_type = "standard", day_limit = 100)

for (name in city_names) {
  filtered <- BnB_data %>%
    filter(City == name)
  gradient_max <- mean(filtered$realSum) + 2 * sd(filtered$realSum)
  max_lng <- max(filtered$lng)
  min_lng <- min(filtered$lng)
  max_lat <- max(filtered$lat)
  min_lat <- min(filtered$lat)
  map_lng <- (max_lng + min_lng)/2
  map_lat <- (max_lat + min_lat)/2
  ggmap_name <- get_map(lon = map_lng, lat = map_lat)
  map<- ggmap(ggmap_name) + 
    geom_point(data = filter(BnB_data, City == name),
               mapping = aes(x=lng,y=lat, color = realSum), size = .9) +
    scale_colour_gradientn(
      colours=c('red','yellow','green'), 
      limits=c(0,gradient_max), 
      oob = scales::squish) +
    scale_x_continuous(limits = c(min_lng, max_lng), expand = c(0, 0)) +
    scale_y_continuous(limits = c(min_lat, max_lat), expand = c(0, 0)) +
    labs(title = paste("Mapping of AirBnB Listings in", name), y = "Latitude", 
         x = "Longitude", color = "Price") 
  map_name <- paste(name, "MapPrint", sep = "_")
  assign(map_name, map)
}


              

