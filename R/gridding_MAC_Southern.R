###################################
## SCRIPT : ANNUAL EGG PRODUCTION FOR NEA MACKEREL. GRIDDING WESTERN AREA 
## version October 2017
##
## Gersom Costas 
## gersom.costas@ieo.es
#
###################################
#
##DESCRIPTION

# The international mackerel and horse mackerel egg surveys (WGMEGS) cover spawning areas for NEA mackerel and horse mackerel stocks. The spatial and temporal distribution of sampling is designed to ensure an adequate coverage of both mackerel  and horse mackerel spawning areas. 
#The Northeast Atlantic shelf area is subdivided into 'western' and 'southern' areas for the purposes of estimating egg production and SSB. 
#The western area for NEA mackerel is from 44 degrees N (45 degrees N in the west) to 63 degrees N. It includes Biscay, the Celtic Sea and the shelf edge to the northwest of Scotland. 
#The 'southern' area for mackerel is regarded as being from 36 degrees N to 44 degrees N in the east and 45 degrees N in the west . It extends from Cape Trafalgar in the Gulf of Cadiz, around the coast of Portugal to 11 degrees W, the Cantabrian Sea and southern Biscay.
#The plankton survey grid was designed according to the procedure described in AEPM manual (ICES, 2016). The basic sampling unit is 0.5 degree longitude * 0.5 degree latitude,  half of an ICES rectangle. 
#But in the Cantabrian coast and in Gulf of Cadiz, the standard half ICES rectangle was changed to a quarter degree latitude by one degree longitude because transects in those regions were done perpendicular to the 200 m depth contour line.

####################################################

##  SPATIAL GRID  SOUTHERN AREA.

###  Southern area:

# Latitude: lower or equal 45 degree N   y bigger  or equal  36 degree N
# Longitude: bigger or equal 1 degree W   y lower or equal  10.5 degree W

####################################################


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


# Importing mackerel Southern Component  rectangles have been sampled along MEGS survey time series.  
# importing grid data by region. southern area contains different size rectangles.
# Rectangle dimensions depende on  region: Cantabrian Sea, Iberian Atlatic coast and Gulf of Cadiz


  RECT2 <- read.csv("data/RECT2.csv")#Cantabrian Sea, rectangles 0.25x1 degrees
  
  RECT3 <- read.csv("data/RECT3.csv")#Iberian Atlatic coast rectangles 0.5x0.5 degrees
  
  RECT5 <- read.csv("data/RECT5.csv")#Gulf of Cadiz, rectangles 0.25x1 degrees

  
# Mackerel southern area. Combining different grids in an object (data.frame)
  
  RECT_south<-rbind(RECT2,RECT3,RECT5)
  

# Importing  file of proportion in rectangles area that correspond sea(removing covered area by land from rectangle area)
# Rectangle area   (cos(RECT$lat*pi/180)*30*1853.2)*30*1853.2 

  RECTall <- read.csv("data/rect_searatio_all.csv")


# Matching covered area by land to southern area rectangles 

##general (RECT_south)
  
  RECT_south<-join(RECT_south[ ,c("lat","lon","cat")],RECTall[ ,c("lat","lon","r1","R2" ,"RECT","Area","sea_ratio","Area_minus_land")],by=c("lat","lon"))
  summary(RECT_south)

  #Problem with NA to be gridded. rectagle in Gibraltar street with NA->delete
  RECT_south<-RECT_south[complete.cases(RECT_south),]
  RECT_south<-droplevels(RECT_south)
 
 
##region grid (RECT2, RECT3, RECT5)
  
 RECT2<-join(RECT2[ ,c("lat","lon","cat")],RECTall[ ,c("lat","lon","r1","R2" ,"RECT","Area","sea_ratio","Area_minus_land")],by=c("lat","lon"))
 
 RECT2<-droplevels(RECT2) 
 
 RECT3<-join(RECT3[ ,c("lat","lon","cat")],RECTall[ ,c("lat","lon","r1","R2" ,"RECT","Area","sea_ratio","Area_minus_land")],by=c("lat","lon"))
 
 RECT3<-droplevels(RECT3)
 
 RECT5<-join(RECT5[ ,c("lat","lon","cat")],RECTall[ ,c("lat","lon","r1","R2" ,"RECT","Area","sea_ratio","Area_minus_land")],by=c("lat","lon"))
 
 #Problem with NA to be gridded.  rectagle in Gibraltar street with NA->delete
 RECT5<-RECT5[complete.cases(RECT5),]
 RECT5<-droplevels(RECT5)

 
# Convert data frame to Spatial pixel
 
 gridded(RECT2) = ~lon+lat
 gridded(RECT5) = ~lon+lat
 gridded(RECT3) = ~lon+lat
 
 
# Convert spatial pixel to Spatial Polygon
 
 RECT2_p <- as(RECT2, "SpatialPolygons")
 RECT3_p <- as(RECT3, "SpatialPolygons") 
 RECT5_p <- as(RECT5, "SpatialPolygons") 
 
 
#Ploting grid 
 
 plot(RECT2_p)
 plot(RECT3_p)
 plot(RECT5_p)
 

# Automatic nectangle names (ID) 
 
 row.names(RECT2_p) 
 row.names(RECT3_p) 
 row.names(RECT5_p) 
 
 
# Use the spChFIDs() method in maptools to change the  IDs of at least one of your objects to disambiguate them 
 
 RECT2_p <- spChFIDs(RECT2_p, as.character(RECT2@data[, 4]))
 
 RECT3_p <- spChFIDs(RECT3_p, as.character(RECT3@data[, 4]))
 
 RECT5_p <- spChFIDs(RECT5_p, as.character(RECT5@data[, 4]))
 
 
# joining region spatialpolygonos ( step by step).
 
 rownames(RECT_south)<-RECT_south$RECT
 
 RECTsouth_p<-spRbind(RECT2_p,RECT3_p)
 
 RECTsouth_p<-spRbind(RECTsouth_p,RECT5_p)
 
 
# Projection grid
 
 proj4string(RECTsouth_p) <- CRS("+proj=longlat   +ellps=WGS84 +datum=WGS84")

 
# Convert to SpatialPolygonsDataFrame. Data frame represents rectangle characteristics (polygons)
 
 RECTsouth_p<-SpatialPolygonsDataFrame(RECTsouth_p, data=RECT_south)
 

# ploting

 png("images/southern_survey_grid.png",
     width = 7, height = 7, units = "in", pointsize = 10,
     bg = "white", res = 800,
     type = "cairo-png")
 
 par(mar=c(2,2,2,2) + 0.1)
 
 map(database = "worldHires",  xlim = c(-11,1), ylim = c(36,46),fill=T, type="n")
 
 plot(RECTsouth_p, border="grey",  xlim = c(-11,1), ylim = c(36,46))
 
 degAxis(2, at = c(seq(36,46, by=3)),cex.axis = 0.5,las=2)
 
 degAxis(1, at = c(seq(-11,1, by=3)), cex.axis = 0.5, las=2)
 
 map(database = "worldHires", xlim = c(-11,1), ylim = c(36,46),fill=T, col="darkgreen",add=T)
 
 title("Mackerel southern area grid")
 
 box()
 
 dev.off()

 
 
  
 rm(RECT2, RECT2_p,    RECT3,      RECT3_p,    RECT5,      RECT5_p,    RECTall)
 
 
 save.image("AEPM_grid_mack_southern.RData")  
 



