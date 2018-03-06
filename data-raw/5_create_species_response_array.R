
library(priorityeffort)
library(XLConnect)


# define parameters -----------------------------------------------------------


in_pth <- "data-raw"

variables <- c("PP_BestGuess", "norm_lower", "norm_upper")

base_information <- c("FaunalGroup", "Threat", "Intensity", "EcologicalGroup")

fauna_expert_indices <- list(f = 1:5, t = 6:9, b = 10:13)


# load data -------------------------------------------------------------------


responses_wb <- loadWorkbook(file.path(in_pth, "individual_expert_responses.xlsx"))

responses_lst <- readWorksheet(responses_wb, sheet = getSheets(responses_wb))

# calculating normalized lower bounds
responses_lst_1 <- lapply(responses_lst, cap_norm_lower_bound)

# calculating normalized upper bounds
responses_lst_2 <- lapply(responses_lst_1, cap_norm_upper_bound)

# keep only response values for buffalo, pig, weed and grazing
responses_lst_3 <- lapply(responses_lst_2,
                          subset_responses,
                          c(1, 2, 4, 7), # indices in the original response file
                          c(1, 2, 4, 3)) # indices in the site-threat array

# averaging species responses across experts
species_responses <- average_responses(responses_ind_experts = responses_lst_3,
                                       vars = variables,
                                       base_info = base_information,
                                       fauna_ex_ind = fauna_expert_indices)

species_responses[, variables] <- round(species_responses[, variables], 3)

species_responses <- as.matrix(species_responses)


# save internal data file -----------------------------------------------------


devtools::use_data(species_responses, species_responses, overwrite = TRUE)
