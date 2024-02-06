### Lab 2: variogram estimation and spatial prediction  

require(gstat)
require(sf)
require(ggplot2)

#### load and filter data ####
load("RG_ST_pointdataset.RData")   # set the path first
obj1=Agrimonia_month_full
obj=obj1[obj1$Time=="2018-07-01",c("Latitude","Longitude","AQ_PM10" )] # select one month
  head(obj)
  class(obj)  
  st_crs(obj)
names(obj)[c(1,2)]=c("y","x")  # simpler names

obj = st_as_sf(obj, coords = c("x", "y"),crs=4326)  # EPSG=WGS84
  head(obj)  # describe the data now
  plot(obj)
  summary(obj)

obj.gb <- obj %>%
    st_transform(crs = 3003)  # change system to GB
  obj.gb    
  
#### load and adjust the shape file of the Lombardy Region ####
lomb.poly<-st_read("Lombardia.shp",quiet=TRUE)  # set the path first
lomb.poly= st_set_crs(lomb.poly, 3003)

ggplot(data = st_boundary(lomb.poly)) + 
  geom_sf()

#### plot the PM10 sensors on the map ####
ggplot(data = st_boundary(lomb.poly)) + 
  geom_sf() + 
  geom_sf(data = obj.gb,  size = 2.5) + 
  theme_bw()  +
  ggspatial::annotation_north_arrow(which_north = "true") +
  ggspatial::annotation_scale(location="br")

#### variagram estimation ####
# empirical semivariogram
vgm = variogram(AQ_PM10~1, data=obj.gb)#, cutoff=60)
  plot(vgm,  ylim=c(0,22))
vgm

vgm.ex.fit = fit.variogram(vgm, model = vgm(psill=25, "Exp", range=80000, nugget=5), 
                       fit.method = 6)  #OLS
# explore this function for other types of estimation methods
  vgm.ex.fit
  plot(vgm, vgm.ex.fit,ylim=c(0,22),lwd=4)
#### prediction. Make a grid prediction first####
st_bbox(lomb.poly) %>%
  stars::st_as_stars(dx = 5500) %>%  #dx sets he grid size 
  st_crop(lomb.poly) -> gr     # crop the grid inside the border of Lombardy

st_crs(gr)
st_crs(obj)
gr=st_transform(gr,crs = 3003)  # change system to GB
  gr    

gr=st_as_sf(gr) # change to a vector format
  plot(gr, main="")
  
#### prediction. kriging and mapping ####
krg.or <- krige(AQ_PM10~1, locations=obj.gb,  newdata=gr, model = vgm.ex.fit)
  # explore this function for other types of kriging predictions
  plot(krg.or["var1.pred"], main="PM10 concentration")
  plot(krg.or["var1.var"], main="MSE of PM10 prediction")	
# Better maps can be obtained using the ggplot package