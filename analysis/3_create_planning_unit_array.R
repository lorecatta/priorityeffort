
devtools::load_all()

library(rgdal)


# define parameters -----------------------------------------------------------


in_pth <- file.path("data_raw", "shapefiles")

threat_names <- c("buffalo", "pig", "weed", "grazing")

# define geographic coordinate system
# long,lat geographical, datum WGS84
geo_CRS <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")

# define projected coordinate system
# eastings and northings (GDA94/Australian Albers, EPSG:3577)
# http://www.spatialreference.org/
prj_CRS <- CRS('+proj=aea +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=132 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs')

# AUS$ per ha
cost_per_ha <- c(buffalo_ha = 0.62,
                 pig_ha = 1.63,
                 weed_ha = 47.42,
                 grazing_ha = 35.28)


# load data -------------------------------------------------------------------


daly <- readOGR(dsn = in_pth, layer = "Catch_500")

rivers <- readOGR(dsn = in_pth, layer = "River_500")

para_grass <- readOGR(dsn = file.path("data_raw", "shapefiles", "threats"),
                      layer = "para_grass")


# create planning unit array --------------------------------------------------


proj4string(daly) <- geo_CRS
proj4string(rivers) <- geo_CRS

daly_prj <- spTransform(daly, prj_CRS)
rivers_prj <- spTransform(rivers, prj_CRS)

all_areas <- sapply(daly_prj@polygons, function(x) x@area) # sq meters

all_lenghts <- sp::SpatialLinesLengths(rivers_prj, longlat = FALSE) # meters

pu_table <- data.frame(pu_id = seq_len(nrow(daly_prj@data)),
                       pu_area = all_areas / 10000, # from sq m to hectars
                       river_l = all_lenghts,
                       river_a = (all_lenghts * 100) / 10000)


###

# Calculate the treated area of each planning unit

# Here I assume that different incidence clasess correspond to
# different proportions of a planning unit covered with para grass

# Incidence classes: 0, 1, 2, 4
# Corresponding proportions of para grass cover: 0, <10%, 10-50%, >50%
# For each proportion, I consider the mean of the range

pu_table$prop_buffalo <- 1
pu_table$prop_pig <- 1
pu_table$prop_grazing <- 1
pu_table$prop_weed <- 0

weed_incidence <- para_grass@data

for (i in 1:nrow(pu_table)) {

  incidence_class <- weed_incidence[i, "MAX_paragr"]

  if(incidence_class == 1) {

    pu_table[i, "prop_weed"] <- 0.05

  }

  if(incidence_class == 2) {

    pu_table[i, "prop_weed"] <- 0.3

  }

}

###


n <- nrow(pu_table)

pu_table <- cbind(pu_table, vapply(cost_per_ha, rep, numeric(n), n))

cost_table <- setNames(as.data.frame(matrix(0,
                                            nrow = nrow(pu_table),
                                            ncol = length(threat_names))),
                       nm = threat_names)

pu_table <- cbind(pu_table, cost_table)

pu_table$buffalo <- pu_table$pu_area * pu_table$prop_buffalo * pu_table$buffalo_ha

pu_table$pig <- pu_table$pu_area * pu_table$prop_pig *pu_table$pig_ha

pu_table$grazing <- pu_table$river_a * pu_table$prop_grazing * pu_table$grazing_ha

pu_table$weed <- pu_table$pu_area * pu_table$prop_weed * pu_table$weed_ha


###

# scale relative to species responses

planning_unit <- pu_table[, threat_names] / 10000

mask <- ifelse(site_threat_array > 0, 1, site_threat_array)

planning_unit <- planning_unit * mask

planning_unit <- as.matrix(planning_unit)


# save internal data file -----------------------------------------------------


devtools::use_data(planning_unit, planning_unit, overwrite = TRUE)
