################################################################################
################################################################################
## SCRIPT : ANNUAL EGG PRODUCTION FOR NEA MACKEREL. GRIDDING WESTERN AREA 
## version SEPTEMBER 2017
##
## Gersom Costas (Instituto Espa?ol de Oceanografia)
## gersom.costas@ieo.es
################################################################################
################################################################################
##DESCRIPTION 
# The  mackerel and western horse mackerel egg surveys (WGMEGG) cover spawning
# areas for NEA mackerel and Western horse mackerel stocks. 
#The north-east Atlantic shelf area is sub-divided  into two spawning areas: 
#western and southern areas.  The southern area for NEA mackerel is regarded as 
#being from 36? N to 44? N in the east and 45? N in the west. 
#The western area for NEA mackerel is from 44? N (45? N in the west) to 63? N  
#The western area for Western horse mackerel includes the Cantabrian Sea and is 
#from 43? N to 63? N with same western boundary as for mackerel
#The plankton survey grid was designed according to the procedure described in 
#AEPM manual (ICES, 2016). The basic sampling unit is 0.5 degree longitude * 0.5 degree latitude,
# half of an ICES rectangle. 
#In the Cantabrian coast and in Gulf of Cadiz, the standard half ICES rectangle was
#changed to a quarter degree latitude by one degree longitude because transects 
#in those regions were done perpendicular to the 200 m depth contour line.

################################################################################

##  SPATIAL GRID  OF MACKEREL SOUTHERN AREA.  CREATING GRID

#################################################################################
##  Southern area:

#Latitude: lower or equal 45 degree N   y bigger  or equal  36 degree N
#Longitude: bigger or equal 1 degree W   y lower or equal  10.5 degree W
#################################################################################
# clear workspace
rm(list = ls())


##################
# load libraries
##################
if(!require(plyr)) install.packages("plyr") ; require(plyr)
if(!require(sp)) install.packages("sp") ; require(sp)
if(!require(maptools)) install.packages("maptools") ; require(maptools)
if(!require(dplyr)) install.packages("dplyr") ; require(dplyr)
if(!require(maps)) install.packages("maps") ; require(maps)
if(!require(mapdata)) install.packages("mapdata") ; require(mapdata)


#Importing Western Component MAC rectangles have been sampled along survey temporal series.  
#importing grid data by region. rectangle dimensions in southern area is a combination of differnt size rectangles.
#importing grid data by region. rectangle dimensions depende on  region


  RECT2 <- read.csv("data/RECT2.csv")#Cantabrian Sea, rectangles 0.25x1
  RECT3 <- read.csv("data/RECT3.csv")#Iberian Atlatic coast rectangles 0.5x0.5
  RECT5 <- read.csv("data/RECT5.csv")#Gulf of Cadiz, rectangles 0.25x1

#  Mackerel southern area
  
  RECT_south<-rbind(RECT2,RECT3,RECT5)#
  summary(RECT_south) 

##Importing  file of proportion in rectangles area that correspond sea(removing covered area by land from rectangle area)
## Rectangle area   (cos(RECT$lat*pi/180)*30*1853.2)*30*1853.2 
#all rectangles, rectangles 0.5? x 0.5? and 0.25? x 1? (Cantrabian Sea)  
  
  
  RECTall <- read.csv("data/rect_searatio_all.csv")



##Matching covered area by land to western area rectangles( step by step) 


  RECT_south<-join(RECT_south[ ,c("lat","lon","cat")],RECTall[ ,c("lat","lon","r1","R2" ,"RECT","Area","sea_ratio","Area_minus_land")],by=c("lat","lon")) 
 summary(RECT_south)
 RECT_south<-RECT_south[complete.cases(RECT_south),]#1 rectagle in Gibraltar street with NA->delete
 RECT_south<-droplevels(RECT_south)

 RECT2<-join(RECT2[ ,c("lat","lon","cat")],RECTall[ ,c("lat","lon","r1","R2" ,"RECT","Area","sea_ratio","Area_minus_land")],by=c("lat","lon"))  
 summary(RECT2)
 RECT2<-droplevels(RECT2) 
 
 RECT3<-join(RECT3[ ,c("lat","lon","cat")],RECTall[ ,c("lat","lon","r1","R2" ,"RECT","Area","sea_ratio","Area_minus_land")],by=c("lat","lon")) 
 summary(RECT3)
 RECT3<-droplevels(RECT3)
 
 RECT5<-join(RECT5[ ,c("lat","lon","cat")],RECTall[ ,c("lat","lon","r1","R2" ,"RECT","Area","sea_ratio","Area_minus_land")],by=c("lat","lon")) 
 summary(RECT5)
 #Problem with NA to be gridded
 RECT5<-RECT5[complete.cases(RECT5),]#1 rectagle in Gibraltar street with NA->delete
 RECT5<-droplevels(RECT5)
 ##################
 
 
 
##To Spatial pixel
 
 gridded(RECT2) = ~lon+lat
 gridded(RECT5) = ~lon+lat
 gridded(RECT3) = ~lon+lat
 
##transform to Spatial Polygon
 
 RECT2_p <- as(RECT2, "SpatialPolygons")
 RECT3_p <- as(RECT3, "SpatialPolygons") 
 RECT5_p <- as(RECT5, "SpatialPolygons") 
 
#PLOTING 
 
 plot(RECT2_p)
 plot(RECT3_p)
 plot(RECT5_p)
 
 slotNames(RECT2_p)# # slot names
  
# Original Rectangle names. 
 row.names(RECT2_p) 
 row.names(RECT3_p) 
 row.names(RECT5_p) 
 
## Use the spChFIDs() method in maptools to change the  IDs of at least one of your objects to disambiguate them 
 
 RECT2_p <- spChFIDs(RECT2_p, as.character(RECT2@data[, 4]))
 RECT3_p <- spChFIDs(RECT3_p, as.character(RECT3@data[, 4]))
 RECT5_p <- spChFIDs(RECT5_p, as.character(RECT5@data[, 4]))
 
 
# join spatialpoligonos ( step by step).
 
 rownames(RECT_south)<-RECT_south$RECT
 
 RECT_p<-spRbind(RECT2_p,RECT3_p)
 RECT_p<-spRbind(RECT_p,RECT5_p)
 
 ## Projection 
 proj4string(RECT_p) <- CRS("+proj=longlat   +ellps=WGS84 +datum=WGS84")
 RECT_p<-SpatialPolygonsDataFrame(RECT_p, data=RECT_south)
 
 plot(RECT_p)
 class(RECT_p)
 summary(RECT_p)
 
 save.image("AEPM_grid_mack_southern.RData")  

 
  
### import land and ploting

par(mfrow=c(1,1))
europe<-map(database = "worldHires",  xlim = c(-11,0), ylim = c(36,46),fill=T,plot=F)
europe$names
IDs <- sapply(strsplit(europe$names, ":"), function(x) x[1])
europemap <- map2SpatialPolygons(europe,IDs=IDs, proj4string=CRS("+proj=longlat   +ellps=WGS84 +datum=WGS84"))
proj4string(RECT_p) <- CRS("+proj=longlat   +ellps=WGS84 +datum=WGS84")
plot(RECT_p, border="grey")
plot(europemap,add=T, fill=T,col="yellow", border="grey")



