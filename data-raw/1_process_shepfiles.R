
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

aus_outline <- readOGR(dsn = in_pth, layer = "australia_outline")


# create region outline -------------------------------------------------------


outline <- gUnaryUnion(daly, id = daly@data$OID_)

daly_outline <- as(outline, "SpatialPolygonsDataFrame")


# project shp files -----------------------------------------------------------


proj4string(daly) <- geo_CRS

proj4string(rivers) <- geo_CRS

proj4string(daly_outline) <- geo_CRS

proj4string(aus_outline) <- geo_CRS

daly_prj <- spTransform(daly, prj_CRS)

rivers_prj <- spTransform(rivers, prj_CRS)

daly_outline_prj <- spTransform(daly_outline, prj_CRS)

aus_outline_prj <- spTransform(aus_outline, prj_CRS)


# save shapefiles -------------------------------------------------------------


devtools::use_data(daly, daly, overwrite = TRUE)

devtools::use_data(daly_prj, daly_prj, overwrite = TRUE)

devtools::use_data(rivers, rivers, overwrite = TRUE)

devtools::use_data(rivers_prj, rivers_prj, overwrite = TRUE)

devtools::use_data(daly_outline, daly_outline, overwrite = TRUE)

devtools::use_data(daly_outline_prj, daly_outline_prj, overwrite = TRUE)

devtools::use_data(aus_outline, aus_outline, overwrite = TRUE)

devtools::use_data(aus_outline_prj, aus_outline_prj, overwrite = TRUE)
