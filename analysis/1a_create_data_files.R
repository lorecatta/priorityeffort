# load the package
devtools::load_all()

library(rgdal)


# load data -------------------------------------------------------------------


fishes <- readOGR(dsn = file.path("data", "shapefiles"),
                         layer = "Catch_500")

daly_rivers <- readOGR(dsn = file.path("data", "shapefiles"),
                       layer = "River_500")

