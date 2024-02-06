### Lab 1: shape file and maps  
# read a shape file and display same data  ------------
### Bivand RS, Pebesma EJ, G?mez-Rubio V, 2008 Applied Spatial Data Analysis with R. libro on line, Springer
#
library(sf); require(ggplot2)
# 
# lattice data
  aus.poly<-st_read("austrianuts3.shp",quiet=TRUE)
  class(aus.poly)
  aus.poly= st_set_crs(aus.poly,4326 )  # CRS=WGS84 
                                        # EPSG =European Petroleum Survey Group. It is a four-five digit number which represents a particular CRS
   ggplot(data = st_boundary(aus.poly)) + 
      geom_sf()
  aus.poly  # dbf table containing alpha numeric information associated with Shpe file
 centr=st_centroid(aus.poly)  # calculate centroid of each polygon 
 ggplot(data = st_boundary(aus.poly)) + 
   geom_sf(data = centr,  size = 2.5)+
   ggspatial::annotation_north_arrow(which_north = "true") +
   ggspatial::annotation_scale(location="br") +   
   geom_sf_text(data = centr,     
                aes(label = NAME),     
                size = 4,     
                color = "blue" ,     
                nudge_x = -0.1,     
                nudge_y = 0.05) +  
   geom_sf()
# note: centroids inherit the CRS of polygons 
 class(centr); st_crs(centr)
# extract part of thee shap file 
 wien.poly<-aus.poly
 library(dplyr)
 wien.poly= wien.poly %>%
     filter(ID == "AT13")
 ggplot(data = wien.poly) + 
   ggspatial::annotation_north_arrow(which_north = "true") +
   ggspatial::annotation_scale(location="br") +
   geom_sf()
# add same data
 distkm <- read.table("dist.txt", header=T); 	
  str(distkm);  summary(distkm) 
# join data to the shape file 
 dati.sp=merge(aus.poly, distkm,by.y = "id", by.x ="ID",all.x=T)
 class(dati.sp)
 head(dati.sp)
# draw a map
 ggplot(data=dati.sp)+
   geom_sf(aes(fill=dist, geometry=geometry))+
   scale_fill_continuous(low="yellow", high="red", na.value="gray")+
   labs(title = "Car use - average distances driven by car")+ 
   ggspatial::annotation_north_arrow(which_north = "true") +
   ggspatial::annotation_scale(location="br") 
# read a raster  ------------
library(stars)
tif=read_stars("L7_ETMs.tif")
tif
plot(tif)
