
library(rgdal)


# define parameters -----------------------------------------------------------


in_pth <- file.path("data_raw", "shapefiles")


# load data -------------------------------------------------------------------


daly <- readOGR(dsn = in_pth, layer = "Catch_500")


