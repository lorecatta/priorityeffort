
data(site_threat_array)

library(rgdal)


# define parameters -----------------------------------------------------------


thr_in_pth <- file.path("data-raw", "shapefiles", "threats")

threat_names <- c("buffalo", "pig", "weed", "grazing")

# AUS$ per ha
# order is important
cost_per_ha <- c(buffalo_ha = 0.62,
                 pig_ha = 1.63,
                 weed_ha = 47.42,
                 grazing_ha = 35.28)

cost_scale_factor <- 10000


# load data -------------------------------------------------------------------


para_grass <- readOGR(dsn = thr_in_pth, layer = "para_grass")


# create planning unit array --------------------------------------------------


all_areas <- sapply(daly_prj@polygons, function(x) x@area) # sq meters

all_areas_ha <- all_areas / 10000 # from sq m to hectars

all_lenghts <- sp::SpatialLinesLengths(rivers_prj, longlat = FALSE) # meters

# order is important
pu_table <- cbind(all_areas_ha,
                  all_areas_ha,
                  all_areas_ha,
                  (all_lenghts * 100) / 10000)

colnames(pu_table) <- threat_names


###

# Calculate the treated area of each planning unit

# Here I assume that different incidence clasess correspond to
# different proportions of a planning unit covered with para grass

# Incidence classes: 0, 1, 2, 4
# Corresponding proportions of para grass cover: 0, <10%, 10-50%, >50%
# For each proportion, I consider the mean of the range

n <- nrow(pu_table)

# order is important
threat_prop <- cbind(rep(1, n), rep(1, n), rep(0, n), rep(1, n))

colnames(threat_prop) <- threat_names

weed_incidence <- para_grass@data

for (i in 1:nrow(pu_table)) {

  incidence_class <- weed_incidence[i, "MAX_paragr"]

  if(incidence_class == 1) {

    threat_prop[i, "weed"] <- 0.05

  }

  if(incidence_class == 2) {

    threat_prop[i, "weed"] <- 0.3

  }

}

###


treated_area <- pu_table * threat_prop

cost_ha <- vapply(cost_per_ha, rep, numeric(n), n)

costs <- treated_area * cost_ha

###

# scale relative to species responses
planning_unit <- costs[, threat_names] / cost_scale_factor

# keep only PUs where the threat occurs
mask <- ifelse(site_threat_array > 0, 1, site_threat_array)
planning_unit <- planning_unit * mask


# save internal data files ----------------------------------------------------


devtools::use_data(planning_unit, planning_unit, overwrite = TRUE)

devtools::use_data(treated_area, treated_area, overwrite = TRUE)
