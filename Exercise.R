####################  Exercise for Exergetic ################################
# Tyryn Carnegie

rm(list = ls())
graphics.off()
setwd("C:/Users/DELL/Documents/Job applications/Exegetic")
# install.packages("osrm")
library(osrm)
# install.packages("sp")
library(sp)
#install.packages("cartography")
## With this, I got straight from Andrew. I wanted to use the ggmap package
#  but it was being problematic with the query limits.

nominatim_osm <- function(address = NULL)
{
  if(suppressWarnings(is.null(address)))
    return(data.frame())
  tryCatch(
    d <- jsonlite::fromJSON( 
      gsub('\\@addr\\@', gsub('\\s+', '\\%20', address), 
           'http://nominatim.openstreetmap.org/search/@addr@?format=json&addressdetails=0&limit=1')
    ), error = function(c) return(data.frame())
  )
  if(length(d) == 0) return(data.frame())
  return(data.frame(lon = as.numeric(d$lon), lat = as.numeric(d$lat)))
}

Durban <- nominatim_osm("115 St Andrew's Drive, Durban North, KwaZulu-Natal")
PMB <- nominatim_osm("67 Boshoff Street, Pietermaritzburg, KwaZulu-Natal, South Africa")
Fairview <- nominatim_osm("4 Paul Avenue, Fairview, Empangeni, KwaZulu-Natal, South Africa")
Vryheid <- nominatim_osm("166 Kerk Street, Vryheid, KwaZulu-Natal, South Africa")
Ixopo <- nominatim_osm("9 Margaret Street, Ixopo, KwaZulu-Natal, South Africa")
Ladysmith <-nominatim_osm("16 Poort Road, Ladysmith, KwaZulu-Natal, South Africa")

## Used the osrm help doc as a template. It basically did everything needed.

# Creating csv table that gives the optimal path with times and distances
combined <- rbind(Durban, PMB, Fairview, Vryheid, Ixopo, Ladysmith)
combined$points <- c("Durban", "PMB", "Fairview", "Vryheid", "Ixopo",
                     "Ladysmith")
combined <- combined[, c(3, 1, 2)]
trips <- osrmTrip(combined, overview = "simplified")
path <- trips[[1]]$trip
write.table(path, file = "pathway.txt", sep = ",", quote = FALSE, row.names = F)

# Map of the path taken. 
if(require("cartography")){
  osm <- getTiles(x = trips[[1]]$trip, crop = TRUE,
                  type = "osm", zoom = 11)
  tilesLayer(x = osm)
  plot(trips[[1]]$trip, col = "black", lwd = 4, add=T)
  plot(trips[[1]]$trip, col = c("red", "white"), lwd = 1, add=T)
  points(combined[, 2:3], pch = 22, bg = "red", cex = 1)
}

# I really wanted to label the points. I couldn't figure out how to though I
# suspect it would involve text(....) with a couple of arguments I couldn't
# get right.

# # Answers to the questions:
# 1. The most challenging part of the exercise was figuring out which packages
# to use. I first used ggmaps, spent a while trying to get it to work, failed, 
# then used Andrew's suggestion of osrm.

# 2. Finding the correct package to use. I also spent an annoying amount of time
# trying to get label the map with the place names.

# 3. I really enjoyed the opportunity to work on a bit of code that produced 
# something I haven't done before. Up until now, all my 'coding' experience
# has been with econometrics. 

# 4. I did not enjoy the amount of time spent trying to get the google API 
# to work. That was frustrating. Also, I wish I could have labelled the map.
# However once Andrew had given me his code for the nominatim_osm function
# and suggested osrm, it was fairly straightforward. 

