# load the package
devtools::load_all()


library(rgdal)
library(dplyr)


# define parameter ------------------------------------------------------------


spp_in_pth <- file.path("data_raw", "shapefiles", "species")

to_remove <- c("OID_",
               "Shape_Leng",
               "Shape_Area",
               "HydroID",
               "GridID",
               "NextDownID",
               "PU_ID")


# load data -------------------------------------------------------------------


fishes <- readOGR(dsn = spp_in_pth, layer = "Fish_Daly_final_agg")

birds <- readOGR(dsn = spp_in_pth, layer = "Birds_Daly_final_agg")

turtles <- readOGR(dsn = spp_in_pth, layer = "Turtles_Daly_final_agg")

eco_g_lookup <- read.table(file.path("data_raw", "Eco_group_lookup.txt"),
                           sep = ",",
                           header = TRUE,
                           stringsAsFactors = FALSE)


# create site-species array ---------------------------------------------------


site_species_array <- cbind(fishes@data[, setdiff(names(fishes@data), to_remove)],
                            turtles@data[, setdiff(names(turtles@data), to_remove)],
                            birds@data[, setdiff(names(birds@data), to_remove)])

sp_names_df <- data.frame(sp_names = colnames(site_species_array),
                          area_of_occ = colSums(site_species_array),
                          target = 0,
                          spf = 0,
                          stringsAsFactors = FALSE)


# create conservation feature array -------------------------------------------


# remove `mel_sino` and `ARDEO_AU`
cons_feat_array <- inner_join(sp_names_df, eco_g_lookup)

cons_feat_array <- cons_feat_array[, c("ID",
                                       "sp_names",
                                       "area_of_occ",
                                       "target",
                                       "spf",
                                       "FaunalGroup",
                                       "EcologicalGroup")] # ID first

devtools::use_data(site_species_array, site_species_array, overwrite = TRUE)

devtools::use_data(cons_feat_array, cons_feat_array, overwrite = TRUE)
