
library(ggplot2)
library(reshape2)
library(XLConnect)
 
# ---------------------------------------- load parameters


exp_ID <- 18

source(file.path("R", "pre_processing", paste0("parameters_exp_", exp_ID, ".R")))

base_info <- c("area_of_occ", "target", "spf", "FaunalGroup", "EcologicalGroup")


# --------------------------------------- load other function


source(file.path("R", "pre_processing", "pre_processing_functions.R"))
source(file.path("R", "processing", "count_persistence.R"))
source(file.path("R", "post_processing", "calculate_species_representation_error.R")) 
source(file.path("R", "post_processing", "calculate_normalized_bounds.R"))
source(file.path("R", "post_processing", "subset_responses.R"))
source(file.path("R", "post_processing", "average_responses.R"))
source(file.path("R", "post_processing", "bar_plot_representation_error.R"))
source(file.path("R", "post_processing", "calculate_uncertainty_diagnostics.R"))
source(file.path("R", "post_processing", "bar_plot_species_proportions.R"))
source(file.path("R", "utility_functions.r"))


# ---------------------------------------- load data


cons_feat_array_df <- read.csv(file.path("data", "input_files", "Conservation Feature.csv"), header = TRUE)
site_species_array_df <- read.csv(file.path("data", "input_files","Planning Unit vs. Conservation Feature.csv"), header=TRUE)
site_threat_array_df <- read.csv(file.path("data", "input_files", "Planning Unit vs. Threat.csv"), header = TRUE)
planning_unit_df <- read.csv(file.path("data", "input_files", "Planning Unit.csv"), header = TRUE)
responses_wb <- loadWorkbook(file.path("data", "individual_expert_responses.xlsx"))

results_exp <- readRDS(file.path("output", 
                                 paste0("output_exp_", parameters$Exp), 
                                 paste0("queue_obj_exp_", parameters$Exp, ".rds")))

output_summary_best <- read.csv(file.path("output", 
                                          paste("output_exp", parameters$Exp, sep = "_"), 
                                          paste0("run_summary_exp_", parameters$Exp, ".csv")))


# ---------------------------------------- start 


# convert df to matrices 
cons_feat_array <- as.matrix(cons_feat_array_df[, base_info])
site_species_array <- as.matrix(site_species_array_df)
site_threat_array <- as.matrix(site_threat_array_df)
planning_unit <- as.matrix(planning_unit_df)


# ---------------------------------------- pre-process species responses of individual experts 


variables <- c("PP_BestGuess", "norm_lower", "norm_upper") 

base_information <- c("FaunalGroup", "Threat", "Intensity", "EcologicalGroup")

fauna_expert_indices <- list(f = 1:5, t = 6:9, b = 10:13)

# convert excel workbook of species responses to list 
responses_lst <- readWorksheet(responses_wb, sheet = getSheets(responses_wb))

# calculating normalized lower bounds
responses_lst_1 <- lapply(responses_lst, function(x) {
  x$norm_lower <- pmax(cal_norm_lower_bound(x$PP_BestGuess, x$PP_Lower, x$Confidence, 100), 0)
  x})             

# calculating normalized upper bounds
responses_lst_2 <- lapply(responses_lst_1, function(x) {
  x$norm_upper <- pmin(cal_norm_upper_bound(x$PP_BestGuess, x$PP_Upper, x$Confidence, 100), 1)
  x})

# keep only response values for buffalos, pigs, grazing and weeds
responses_lst_3 <- lapply(responses_lst_2, subset_responses, c(1, 2, 4, 7)) 

# averaging species responses across experts
species_responses <- average_responses(
  responses_ind_experts = responses_lst_3,
  vars = variables, 
  base_info = base_information,
  fauna_ex_ind = fauna_expert_indices)

# create a list of list of response values, calculated using best guesses, lower bounds and upper bounds
response_list <- lapply(
  seq_len(3), function(i) {
    get.responses.to.actions (species_responses, cons_feat_array, estimate = i)
  }
) 
  
# get responses as best guesses
responses_to_actions_BG <- response_list[[1]]
  
site_threat_array_cat <- get.threat.intensity.category (parameters, site_threat_array)
required.actions <- get.required_actions (site_threat_array_cat, responses_to_actions_BG, cons_feat_array)
action_costs <- get.action.costs (site_threat_array_cat, planning_unit)


# ---------------------------------------- analysis of uncertainty 1 


####
## uncertainty = variation in the responses (averaged across experts) of each functional group to the threats  
####

# calculate errors of species representation (commission and omission errors) 
# when using best guess, lower bound and upper bound,
# averaged across experts, as true species responses 

analysis_sub_directory <- "analysis_1"

response_types <- c("best_guess", "lower_bound", "upper_bound")

exp_des <- data.frame(response_type = seq_along(response_types))

ID_exp = rep(output_summary_best$ID_exp, length(response_types))
ID_run = rep(output_summary_best$ID_run, length(response_types))

exp_des <- cbind(ID_exp, ID_run, exp_des)

exp_des_lst <- df_to_list(exp_des, use_names = TRUE)
  
all_exp_des_runs <- lapply(
  exp_des_lst, 
  calculate_representation_error,
  b = response_list,
  c = response_types,
  parms = parameters, 
  cons_features = cons_feat_array, 
  site_threat_array_cat.mat = site_threat_array_cat, 
  all_run_results = results_exp, 
  action_cost_list = action_costs, 
  responses_to_actions_EXP = responses_to_actions_BG,
  required_actions = required.actions,
  site_species_array.mat = site_species_array,
  output_folder = analysis_sub_directory,
  output_by_species = TRUE)

all_errors <- lapply(all_exp_des_runs, calc_uncertainty_diagnostics)

all_errors <- do.call("rbind", all_errors)

data_to_plot <- cbind(exp_des, all_errors)

data_to_plot$response_type <- factor(data_to_plot$response_type, labels = response_types)  

# plot percentage change
bar_plot_representation_error(aa = data_to_plot, 
                              parms = parameters, 
                              analysis = analysis_sub_directory,
                              by_var = "response_type")


# ---------------------------------------- plot number of species above/below/at target


data_to_plot$response_type <- factor(data_to_plot$response_type, labels = c("Best guess", "Lower bound", "Upper bound"))

data_to_plot_long <- melt(data_to_plot, 
                          id.vars = "response_type",
                          measure.vars = c("prop_below", "prop_above", "prop_at"),
                          variable.name = "species_prop")

levels(data_to_plot_long$species_prop) <- c("Species below target", "Species above target", "Species at target")


data_to_plot_long <- data_to_plot_long[!data_to_plot_long$species_prop == "Species at target", ]

bar_plot_species_proportions(aa = data_to_plot_long, 
                             parms = parameters, 
                             analysis = analysis_sub_directory, 
                             by_var = "response_type")


# ---------------------------------------- plot mean (negative and positive) percentage change from target (+_SE)


data_to_plot_long_2 <- reshape(data_to_plot, 
                               varying = 13:16,
                               sep = "_", 
                               direction = "long")

data_to_plot_long_2$time <- factor(data_to_plot_long_2$time, levels = c("neg", "pos"), labels = c("Below target", "Above target"))

plot.file.name <- paste0("perc_change_from_target_exp_", 
                         parameters$Exp, 
                         "_",
                         analysis_sub_directory, 
                         ".tiff")

tiff(file.path("figures", 
               paste("figures_exp", parameters$Exp, sep = "_"), 
               "uncertainty_analysis",
               plot.file.name), 
     width = 7, 
     height = 3, 
     units = "in", 
     compression = "lzw", 
     res = 300)

neg <- ggplot(data_to_plot_long_2[data_to_plot_long_2$time=="Below target",]) +
  aes(x = response_type, y = Msf) + 
  geom_bar(stat = "identity", width = 0.5) +
  geom_errorbar(aes(ymin = Msf - SEsf, ymax = Msf + SEsf), 
                width = .15) + 
  facet_wrap(~ time) +
  scale_x_discrete("", labels = c("Best guess", "Lower bound", "Upper bound")) +
  scale_y_continuous("% change from target",
                     limits = c(-20, 0),
                     breaks = seq(-20,0,5)) +
  theme(axis.title.x = element_text(vjust = 0.5),
        axis.title.y = element_text(vjust = 0.5)) 

pos <- ggplot(data_to_plot_long_2[data_to_plot_long_2$time=="Above target",]) +
  aes(x = response_type, y = Msf) + 
  geom_bar(stat = "identity", width = 0.5) +
  geom_errorbar(aes(ymin = Msf - SEsf, ymax = Msf + SEsf), 
                width = .15) + 
  facet_wrap(~ time) +
  scale_x_discrete("", labels = c("Best guess", "Lower bound", "Upper bound")) +
  scale_y_continuous("% change from target",
                     limits = c(0, 280),
                     breaks = c(0, 50, 100, 150, 200, 250)) +
  theme(axis.title.x = element_text(vjust = 0.5),
        axis.title.y = element_text(vjust = 0.5)) 

p <- gridExtra::grid.arrange(neg, pos, ncol=2)

print(p)

dev.off()


# ---------------------------------------- write out outputs


dir.create(file.path("output", paste("output_exp", parameters$Exp, sep = "_"), 
                     "uncertainty_analysis",
                     analysis_sub_directory), 
           FALSE, TRUE)

file_name <- paste0("total_errors_exp_", parameters$Exp, ".csv")

write.table(data_to_plot, 
            file.path("output", paste("output_exp", parameters$Exp, sep = "_"), 
                      "uncertainty_analysis",
                      analysis_sub_directory, 
                      file_name), 
            sep = ",", row.names = FALSE)
