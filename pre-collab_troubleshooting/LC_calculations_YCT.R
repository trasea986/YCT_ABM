#Script for Least Cost calculations

library(raster)
library(gdistance)
library(geosphere)
library(rgdal)



#bring in raster reclassified file
stream <- raster("raster_lcd_5.asc")
plot(stream)

#bring in points
Patches <- read.csv("PatchVars_yct_LCD.csv")


#create transition object

tr1 <- transition(1/stream, transitionFunction=mean, directions=16)
tr1 <- geoCorrection(tr1, type="c")


#need to now create point file, which is same as above but just the x and y

Points <- Patches[-c(1,1)]


#now to calculate distance matrix. first part is based on just euclidean distance, which I did before but am not including here

#CSF_sp_latlong2 <- CSF_sp_latlong[,c(2,1)]
#eucdist <- distm(SpatialPoints(as.matrix(CSF_sp_latlong2)), fun=distGeo)

#costDist seemed to be weird, next up option is shortest path like the image...
distance2 <- costDistance(tr1, as.matrix(Points))


write.csv(as.matrix(distance2), "yct_lc_dist2_inv.csv")





#display results?
plot(stream)
#plot(SpatialPoints(CSF_sp), add=TRUE, pch=20, col="red")
#text(CSF_sp[,1]+2, CSF_sp[,2]+2, 1:nrow(CSF_sp))
#too many points, subsetting CSF to only first 50 sites
plot(SpatialPoints(Points), add=TRUE, pch=20, col="blue")



for (i in 1:50) {
  plot(shortestPath(tr1, as.matrix(sp_sub[1,]), as.matrix(sp_sub[i,]), output="SpatialLines"), add=TRUE)
}

#make a movie
#to install ImageMagick


library(animation)

saveGIF(
  {for (i in 1:50) {
    p <- plot(SA_2016)
    p <- plot(SpatialPoints(CSF_sp), add=TRUE, pch=20, col="red")
    p <- plot(shortestPath(tr1, as.matrix(CSF_sp[1,]), as.matrix(CSF_sp[i,]), output="SpatialLines"), add=TRUE)}
    print(p)
  },
  movie.name = "CSF_2016.gif", interval = .2, nmax =674, ani.width = 600, ani.height = 600,
  outdir = getwd()
)
