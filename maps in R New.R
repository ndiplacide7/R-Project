library(tidyverse)
library(sf)
library(raster)

sf_kenya <- raster::getData("GADM", country ="KE", level =1) %>% sf::st_as_sf()
plot(sf_kenya)

View(sf_kenya)

confirmed.cases <- read.csv("C:\\Users\\Administrator\\Desktop\\ADVANCED DATA MINING\\Lecture Nine\\COUNTY DATA.csv", header=TRUE)

count_df<-data.frame(confirmed.cases[1:47,])


sf_kenya_joined <-  sf_kenya  %>% left_join(count_df, by = "NAME_1")

View(sf_kenya_joined )

ggplot() + 
  geom_sf(data = sf_kenya_joined,aes(fill = Count))+
scale_fill_gradient(low = "blue", high = "red")

 
 
