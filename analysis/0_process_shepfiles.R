
library(rgdal)
library(rgeos)


# define parameters -----------------------------------------------------------


in_pth <- file.path("data-raw", "shapefiles")

# define geographic coordinate system
# long,lat geographical, datum WGS84
geo_CRS <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")

# define projected coordinate system
# eastings and northings (GDA94/Australian Albers, EPSG:3577)
# http://www.spatialreference.org/
prj_CRS <- CRS('+proj=aea +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=132 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs')


# load data -------------------------------------------------------------------


daly <- readOGR(dsn = in_pth, layer = "Catch_500")

rivers <- readOGR(dsn = in_pth, layer = "River_500")


# create region outline -------------------------------------------------------


outline <- gUnaryUnion(daly, id = daly@data$OID_)

outline <- as(outline, "SpatialPolygonsDataFrame")


# project shp files -----------------------------------------------------------


proj4string(daly) <- geo_CRS

proj4string(rivers) <- geo_CRS

proj4string(outline) <- geo_CRS

daly_prj <- spTransform(daly, prj_CRS)

rivers_prj <- spTransform(rivers, prj_CRS)

outline_prj <- spTransform(outline, prj_CRS)


# save shapefiles -------------------------------------------------------------


writeOGR(daly_prj, "data", "daly_prj", driver = "ESRI Shapefile")

writeOGR(rivers_prj, "data", "rivers_prj", driver = "ESRI Shapefile")

writeOGR(outline, "data", "outline", driver = "ESRI Shapefile")

writeOGR(outline_prj, "data", "outline_prj", driver = "ESRI Shapefile")
