

input_pt <- "data_raw"


# load data -------------------------------------------------------------------


cons_feat_array_df <- read.csv(file.path(input_pt,"Conservation Feature.csv"),
                               header = TRUE)

site_species_array_df <- read.csv(file.path(input_pt, "Planning Unit vs. Conservation Feature.csv"),
                                  header = TRUE)

site_threat_array_df <- read.csv(file.path(input_pt, "Planning Unit vs. Threat.csv"),
                                 header = TRUE)

planning_unit_df <- read.csv(file.path(input_pt, "Planning Unit.csv"),
                             header = TRUE)

species_responses_df <- read.csv(file.path(input_pt, "Species Responses.csv"),
                                 header = TRUE)


# set area-of-occ scaled targets ----------------------------------------------


#cons_feat_array_df <- set_scaled_targets(cons_feat_array_df, occurrence_limits, target_limits)


# ---------------------------------------- pre processing

cons_feat_array <- setNames(as.matrix(cons_feat_array_df[, base_info]),
                            colnames(cons_feat_array_df[, base_info]))

site_species_array <- setNames(as.matrix(site_species_array_df),
                               colnames(site_species_array_df))

site_threat_array <- setNames(as.matrix(site_threat_array_df),
                              colnames(site_threat_array_df))

planning_unit <- setNames(as.matrix(planning_unit_df),
                          colnames(planning_unit_df))

species_responses <- setNames(as.matrix(species_responses_df),
                              colnames(species_responses_df))


# save internal package data files

devtools::use_data(cons_feat_array, cons_feat_array, overwrite = TRUE)
devtools::use_data(site_species_array, site_species_array, overwrite = TRUE)
devtools::use_data(site_threat_array, site_threat_array, overwrite = TRUE)
devtools::use_data(planning_unit, planning_unit, overwrite = TRUE)
devtools::use_data(species_responses, species_responses, overwrite = TRUE)
