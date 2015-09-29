#created by Nick Sard
#created on 4/17/2015

#This script was created to make a study site map of the west coast 

#loading libraries
library(maps)
library(maptools)
library(ggplot2)
library(grid)
library(ggplot2)
library(dplyr)

#defining some necessary functions to make a north arrow and scale bar that I found on the web
#read more about the functions here: http://editerna.free.fr/wp/?p=76

# Result #
#--------#
# Return a list whose elements are :
#   - rectangle : a data.frame containing the coordinates to draw the first rectangle ;
# 	- rectangle2 : a data.frame containing the coordinates to draw the second rectangle ;
# 	- legend : a data.frame containing the coordinates of the legend texts, and the texts as well.
#
# Arguments : #
#-------------#
# lon, lat : longitude and latitude of the bottom left point of the first rectangle to draw ;
# distanceLon : length of each rectangle ;
# distanceLat : width of each rectangle ;
# distanceLegend : distance between rectangles and legend texts ;
# dist.units : units of distance "km" (kilometers) (default), "nm" (nautical miles), "mi" (statute miles).
createScaleBar <- function(lon,lat,distanceLon,distanceLat,distanceLegend, dist.units = "km"){
  # First rectangle
  bottomRight <- gcDestination(lon = lon, lat = lat, bearing = 90, dist = distanceLon, dist.units = dist.units, model = "WGS84")
  
  topLeft <- gcDestination(lon = lon, lat = lat, bearing = 0, dist = distanceLat, dist.units = dist.units, model = "WGS84")
  rectangle <- cbind(lon=c(lon, lon, bottomRight[1,"long"], bottomRight[1,"long"], lon),
                     lat = c(lat, topLeft[1,"lat"], topLeft[1,"lat"],lat, lat))
  rectangle <- data.frame(rectangle, stringsAsFactors = FALSE)
  
  # Second rectangle t right of the first rectangle
  bottomRight2 <- gcDestination(lon = lon, lat = lat, bearing = 90, dist = distanceLon*2, dist.units = dist.units, model = "WGS84")
  rectangle2 <- cbind(lon = c(bottomRight[1,"long"], bottomRight[1,"long"], bottomRight2[1,"long"], bottomRight2[1,"long"], bottomRight[1,"long"]),
                      lat=c(lat, topLeft[1,"lat"], topLeft[1,"lat"], lat, lat))
  rectangle2 <- data.frame(rectangle2, stringsAsFactors = FALSE)
  
  # Now let's deal with the text
  onTop <- gcDestination(lon = lon, lat = lat, bearing = 0, dist = distanceLegend, dist.units = dist.units, model = "WGS84")
  onTop2 <- onTop3 <- onTop
  onTop2[1,"long"] <- bottomRight[1,"long"]
  onTop3[1,"long"] <- bottomRight2[1,"long"]
  
  legend <- rbind(onTop, onTop2, onTop3)
  legend <- data.frame(cbind(legend, text = c(0, distanceLon, distanceLon*2)), stringsAsFactors = FALSE, row.names = NULL)
  return(list(rectangle = rectangle, rectangle2 = rectangle2, legend = legend))
}

# Result #
#--------#
# Returns a list containing :
#  - res : coordinates to draw an arrow ;
#	- coordinates of the middle of the arrow (where the "N" will be plotted).
#
# Arguments : #
#-------------#
# scaleBar : result of createScaleBar() ;
# length : desired length of the arrow ;
# distance : distance between legend rectangles and the bottom of the arrow ;
# dist.units : units of distance "km" (kilometers) (default), "nm" (nautical miles), "mi" (statute miles).
createOrientationArrow <- function(scaleBar, length, distance = 1, dist.units = "km"){
  lon <- scaleBar$rectangle2[1,1]
  lat <- scaleBar$rectangle2[1,2]
  
  # Bottom point of the arrow
  begPoint <- gcDestination(lon = lon, lat = lat, bearing = 0, dist = distance, dist.units = dist.units, model = "WGS84")
  lon <- begPoint[1,"long"]
  lat <- begPoint[1,"lat"]
  
  # Let us create the endpoint
  onTop <- gcDestination(lon = lon, lat = lat, bearing = 0, dist = length, dist.units = dist.units, model = "WGS84")
  
  leftArrow <- gcDestination(lon = onTop[1,"long"], lat = onTop[1,"lat"], bearing = 225, dist = length/5, dist.units = dist.units, model = "WGS84")
  
  rightArrow <- gcDestination(lon = onTop[1,"long"], lat = onTop[1,"lat"], bearing = 135, dist = length/5, dist.units = dist.units, model = "WGS84")
  
  res <- rbind(
    cbind(x = lon, y = lat, xend = onTop[1,"long"], yend = onTop[1,"lat"]),
    cbind(x = leftArrow[1,"long"], y = leftArrow[1,"lat"], xend = onTop[1,"long"], yend = onTop[1,"lat"]),
    cbind(x = rightArrow[1,"long"], y = rightArrow[1,"lat"], xend = onTop[1,"long"], yend = onTop[1,"lat"]))
  
  res <- as.data.frame(res, stringsAsFactors = FALSE)
  
  # Coordinates from which "N" will be plotted
  coordsN <- cbind(x = lon, y = (lat + onTop[1,"lat"])/2)
  
  return(list(res = res, coordsN = coordsN))
}

# Result #
#--------#
# This function enables to draw a scale bar on a ggplot object, and optionally an orientation arrow #
# Arguments : #
#-------------#
# lon, lat : longitude and latitude of the bottom left point of the first rectangle to draw ;
# distanceLon : length of each rectangle ;
# distanceLat : width of each rectangle ;
# distanceLegend : distance between rectangles and legend texts ;
# dist.units : units of distance "km" (kilometers) (by default), "nm" (nautical miles), "mi" (statute miles) ;
# rec.fill, rec2.fill : filling colour of the rectangles (default to white, and black, resp.);
# rec.colour, rec2.colour : colour of the rectangles (default to black for both);
# legend.colour : legend colour (default to black);
# legend.size : legend size (default to 3);
# orientation : (boolean) if TRUE (default), adds an orientation arrow to the plot ;
# arrow.length : length of the arrow (default to 500 km) ;
# arrow.distance : distance between the scale bar and the bottom of the arrow (default to 300 km) ;
# arrow.North.size : size of the "N" letter (default to 6).
scaleBar <- function(lon, lat, distanceLon, distanceLat, distanceLegend, dist.unit = "km", rec.fill = "white", rec.colour = "black", rec2.fill = "black", rec2.colour = "black", legend.colour = "black", legend.size = 3, orientation = TRUE, arrow.length = 500, arrow.distance = 300, arrow.North.size = 6){
  laScaleBar <- createScaleBar(lon = lon, lat = lat, distanceLon = distanceLon, distanceLat = distanceLat, distanceLegend = distanceLegend, dist.unit = dist.unit)
  # First rectangle
  rectangle1 <- geom_polygon(data = laScaleBar$rectangle, aes(x = lon, y = lat), fill = rec.fill, colour = rec.colour)
  
  # Second rectangle
  rectangle2 <- geom_polygon(data = laScaleBar$rectangle2, aes(x = lon, y = lat), fill = rec2.fill, colour = rec2.colour)
  
  # Legend
  scaleBarLegend <- annotate("text", label = paste(laScaleBar$legend[,"text"], dist.unit, sep=""), x = laScaleBar$legend[,"long"], y = laScaleBar$legend[,"lat"], size = legend.size, colour = legend.colour)
  
  res <- list(rectangle1, rectangle2, scaleBarLegend)
  
  if(orientation){# Add an arrow pointing North
    coordsArrow <- createOrientationArrow(scaleBar = laScaleBar, length = arrow.length, distance = arrow.distance, dist.unit = dist.unit)
    arrow <- list(geom_segment(data = coordsArrow$res, aes(x = x, y = y, xend = xend, yend = yend)), annotate("text", label = "N", x = coordsArrow$coordsN[1,"x"], y = coordsArrow$coordsN[1,"y"], size = arrow.North.size, colour = "black"))
    res <- c(res, arrow)
  }
  return(res)
}


########################################################################
## Description: Function to insert a Compasse Rose (North Arrow) in Maps
## developed with ggplot2 R package
##
## Maintainer: Rodrigo Sant'Ana
## Author: Rodrigo Sant'Ana
## Created: Ter Dez 30 21:27:56 2014 (-0200)
## Version: 0.0.1
## Last-Updated: Ter Dez 30 21:29:51 2014 (-0200)
##           By: Rodrigo Sant'Ana
##
## Database info: No data info...
##
### Commentary:
##
### Code:
########################################################################

compass.rose <- function(loc, size, bearing = 0, cex = 1, w.subplot,
                         h.subplot, ...) {
  ### Loading required packages...
  library(ggplot2)
  library(gridExtra)
  ### Empty theme for ggplot2...
  theme_nothing <- function(base_size = 12, base_family = "Helvetica")
  {
    theme_bw(base_size = base_size, base_family = base_family) %+replace%
      theme(
        rect = element_blank(),
        line = element_blank(),
        text = element_blank(),
        legend.position = "none",
        axis.ticks.margin = unit(0, "lines")
      )
  }
  ### Checking arguments...
  if(missing(loc)) stop("Argument loc is missing")
  if(missing(size)) stop("Argument size is missing")
  ### Setting color scheme...
  cols <- rep(c("white", "black"), 8)
  ### Calculating polygons coordinates...
  rad <- rep(size/c(1, 4, 2, 4), 4)
  x <- rad[(0:15)+1]*cos((0:15)*pi/8+bearing)+loc[1]
  y <- rad[(0:15)+1]*sin((0:15)*pi/8+bearing)+loc[2]
  out <- list(NULL)
  for(i in 1:15) {
    x1 <- c(x[i], x[i+1], loc[1])
    y1 <- c(y[i], y[i+1], loc[2])
    out[i] <- list(data.frame(x = x1, y = y1, col = cols[i]))
  }
  out[16] <- list(data.frame(x = c(x[16], x[1], loc[1]),
                             y = c(y[16], y[1], loc[2]),
                             col = rep(cols[16], 3)))
  df <- as.data.frame(do.call(rbind, out))
  ### Preparing subwindom for Compass Rose drawing...
  vp <- viewport(width = w.subplot, height = h.subplot,
                 x = loc[1],
                 y = loc[2])
  wr <- ggplot(data = df, aes(x = x, y = y, fill = col, colour = col)) +
    geom_polygon() + coord_equal() +
    annotate(geom = "text", x = mean(df$x), y = max(df$y)+.1,
             label = "N", size = 4) +
    scale_fill_manual(values = c("white", "black")) +
    scale_colour_manual(values = c("black", "black")) +
    theme_nothing()
  print(wr, vp = vp)
}

########################################################################
##
## This program is free software; you can redistribute it and/or
## modify it under the terms of the GNU General Public License as
## published by the Free Software Foundation; either version 3, or
## (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program; see the file COPYING.  If not, write to
## the Free Software Foundation, Inc., 51 Franklin Street, Fifth
## Floor, Boston, MA 02110-1301, USA.
##
########################################################################


#getting the data to make the plot
#getting just the USA state polygons
states <- map_data("state")

#getting all the polygons for the world and
#then filtering out just the ones specific to
#Canada
canada <- map_data("world")
canada <- filter(canada, region=="Canada")

#Generate full map by layering - note I use the functions defined above to add a scale bar
p1 <- ggplot() +
  geom_polygon(data=states, aes(x=long, y=lat, group=group),fill="white", colour="black")+
  geom_polygon(data=canada, aes(x=long, y=lat, group=group),fill="grey", colour="black")+
  coord_map("gilbert",xlim = c(-127,-118),ylim = c(35,50))+
  annotate("text", x = -121, y = 44, label = "Oregon",size=7)+
  annotate("text", x = -121, y = 46.7, label = "Washington",size=7)+
  annotate("text", x = -122, y = 41, label = "California",size=7)+
  annotate("text", x = -126.5, y = 43, label = "Pacific Ocean",size=6,angle=90)+
  labs(x="Longitude", y="Latitude")+
  theme_bw()+
  theme(axis.title = element_text(size=22),
        axis.text = element_text(size=18),
        panel.background = element_rect(fill = "light blue"))+
  scaleBar(lon = -126.5, lat = 35.5, 
             distanceLon = 125, distanceLat = 30, 
             distanceLegend = 100, dist.unit = "km",
             orientation = FALSE, legend.size = 4)
p1

#now adding the compass rose
p1
compass.rose(loc = c(.65,.85),w.subplot = 0.15,h.subplot = .15, size=1)

#no lat and long, with the addtion of the compass rose
ggplot() +
  geom_polygon(data=states, aes(x=long, y=lat, group=group),fill="white", colour="black")+
  geom_polygon(data=canada, aes(x=long, y=lat, group=group),fill="grey", colour="black")+
  annotate("text", x = -121, y = 44, label = "Oregon",size=7)+
  annotate("text", x = -121, y = 46.7, label = "Washington",size=7)+
  annotate("text", x = -122, y = 41, label = "California",size=7)+
  annotate("text", x = -126.5, y = 43, label = "Pacific Ocean",size=6,angle=90)+
  coord_map("gilbert",xlim = c(-127,-118),ylim = c(35,50))+
  labs(x="", y="")+
  theme_bw()+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks=element_blank(),
        panel.background = element_rect(fill = "light blue"))+
  scaleBar(lon = -126.5, lat = 35.5, 
           distanceLon = 125, distanceLat = 30, 
           distanceLegend = 100, dist.unit = "km",
           orientation = FALSE, legend.size = 4)
compass.rose(loc = c(.65,.85),w.subplot = 0.15,h.subplot = .15, size=1)