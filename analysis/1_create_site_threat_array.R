
library(rgdal)


# define parameters -----------------------------------------------------------


thr_in_pth <- file.path("data_raw", "shapefiles", "threats")

threat_names <- c("Buffalo", "Pig", "Grazing", "Weed")


# load data -------------------------------------------------------------------


buffalos <- readOGR(dsn = thr_in_pth, layer = "buffalos")

pigs <- readOGR(dsn = thr_in_pth, layer = "pigs")

para_grass <- readOGR(dsn = thr_in_pth, layer = "para_grass")

grazing <- readOGR(dsn = thr_in_pth, layer = "grazing")


# create site-threat array ----------------------------------------------------


invasives <- cbind(buffalos@data[, "buff_inc"],
                   pigs@data[, "pig_inc"],
                   para_grass@data[, "MAX_paragr"])

invasives_scal <- (invasives - min(invasives)) * (1 / (3 - min(invasives)))

site_threat_array <- round(cbind(invasives_scal, grazing@data[, "MEAN"]), 3)

colnames(site_threat_array) <- 1:dim(site_threat_array)[2]
colnames(site_threat_array) <- threat_names

devtools::use_data(site_threat_array, site_threat_array, overwrite = TRUE)
