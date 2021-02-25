# script to determine occupancy

#go the directory that has all of the outputs you want
#note the script can be altered to pull in multiple runs
setwd("outputs/output_test_fullconnect_10patch_50_500/batchrun0mcrun0")

library(tidyverse)
library(cowplot)
library(ggmap)
library(rgdal)
library(rgeos)
library(maptools)

#pick years to import
data_counts = list.files(pattern = "ind9|ind49", 
                         full.names = TRUE, 
                         recursive = TRUE, 
                         include.dirs = TRUE) %>%
  map_df(function(x) read_csv(x, col_types = cols(.default = "c")) %>% mutate(filename=gsub(".csv","",basename(x))))

#navigate back to main project directory
setwd("../../../")

#create year column from file name
data_df <- separate(data = data_counts, col = filename, into = c('Rest', 'Year'), sep = "ind")
data_df$Rest <- NULL


#remove rows from the initialization, maybe year 1
data_df$Year <- as.numeric(data_df$Year)
data_df$sex <- as.factor(data_df$sex)
data_df$size <- as.numeric(data_df$size)
data_df$age <- as.numeric(data_df$age)
data_df$mature <- as.factor(data_df$mature)
data_df$XCOORD <- as.numeric(data_df$XCOORD)
data_df$YCOORD <- as.numeric(data_df$YCOORD)

#quick plot to look at distribution of any of the above variables
test <- ggplot(data = data_df, aes(x = age)) +
  geom_histogram() + #use stat = "count" if looking at factors
  ylab("Count") +
  xlab("Variable") + #change as needed
  theme_bw(base_size = 14)

ggsave(plot = test, path = "C:/Users/burnmore/Documents/GitHub/YCT_ABM/outputs/plots", filename="test.png",
       width = 10, height = 10, units = "in", dpi=400)


#then summarize. if you wanted to color code or do something else on the map you should add the variable to the group_by argument and add back in after reprojecting at line 70

for_viz <- data_df %>%
group_by(PatchID, XCOORD, YCOORD, Year, .drop = FALSE) %>% 
  summarise(n = n())


#copied coordinates from ArcGIS Pro
#bottom left: 111.3488931°W 43.6781923°N 
#top right: 111.0184507°W 43.9036550°N 

map <- get_map(c(left = -111.3488931, bottom = 43.6781923, top = 43.9036550, right = -111.0184507))

#create object out of the Google map
p <- ggmap(map)

#for points, need to change UTM to LONG LAT and add back in n
coordinates(for_viz) <- ~XCOORD+YCOORD #establish coordinates
<<<<<<< HEAD
data_viz_UTM <- SpatialPoints(for_viz, proj4string=CRS("+proj=utm +zone=11 +datum=WGS84"))
#set CRS info
=======
data_viz_UTM <- sp::SpatialPoints(for_viz, proj4string=CRS("+proj=utm +zone=11 +datum=WGS84")) #set CRS info
>>>>>>> a28f1d300694be315072c544dbcad8c98b1d19b1
data_viz_LONGLAT <- spTransform(data_viz_UTM, CRS("+proj=longlat +datum=WGS84")) #transform to match Google map
data_viz_ll_df <- as.data.frame(data_viz_LONGLAT) #convert to data frame for ggplot
data_viz_ll_df$n <- for_viz$n #add back in count data
data_viz_ll_df$Year <- for_viz$Year #add back in year data



#add in stream layer data for map. we will use it for the lines, but could plot based on temp if desired
#this is a big shape file, so these steps will take a while

stream <- readOGR("data_gis/NorWeST_PredictedStreamTempLines_SnakeBear_Aug.shp")
stream_repro <- spTransform(stream, CRS("+proj=longlat +datum=WGS84"))
stream_fort <- fortify(stream_repro)


#this loop will create and export a map for each year. you can also pull the ggplot bits out if not wanted
for (i in unique(data_viz_ll_df$Year)) {

  Year = i 
  
  data_plot <- data_viz_ll_df %>%
    filter(Year == i)
  
  myplot <- p + 
    geom_path(data = stream_fort, aes(long, lat, group = group), color = "blue") +
  geom_point(data = data_plot, aes(x = XCOORD, y = YCOORD, size = n),
             color = "black", fill = "orange", shape = 21) + 
    #add alpha = n to aes if lots of overlapping points to the above line
  scale_size_continuous(range = c(1, 10)) +
  #scale_alpha_continuous(range = c(0.25, .75)) + use if points overlapping
  theme(legend.position = "none") +
  ylab("Latitude") +
  xlab("Longitude") +
  ggtitle(paste("Teton Year",i,sep = " ")) +
  theme_bw(base_size = 16)+
  theme(legend.position = "none")
  
  ggsave(myplot, filename=paste("Teton_Year",i,".png",sep=""), path = "outputs/plots/", dpi = 400, width = 10, height = 10, units = "in")
  
}
