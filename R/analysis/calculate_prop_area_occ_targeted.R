library(ggplot2)

input_pt <- file.path("data", "input_files")

target_levels <- c(seq(50, 1000, 50), seq(1250, 10000, 250))

cons_feat_array_df <- read.csv(
  file.path(
    input_pt,"Conservation Feature.csv"), 
  header = TRUE)

out_df <- setNames(as.data.frame(matrix(0, nrow = length(target_levels), ncol = 2)),
                   nm = c("target", "proportion"))
  
out_df[,1] <- target_levels 

for (i in seq_along(target_levels)){
  
  tar <- target_levels[i]  

  cons_feat_array_df$target <- tar

  cons_feat_array_df$prop <- (cons_feat_array_df$target / cons_feat_array_df$area_of_occ) * 100

  cons_feat_array_df$prop[cons_feat_array_df$prop > 100] <- 100
    
  out_df[i,2] <- mean(cons_feat_array_df$prop)
}

ggplot(out_df, aes(target, proportion)) + 
  geom_point() +
  scale_x_continuous(breaks = seq(0, 10000, 500),
                     labels = seq(0, 10000, 500)) +
  scale_y_continuous("proportion of species area of occupancy targeted",
                     breaks = seq(0, 100, 10),
                     labels = seq(0, 100, 10),
                     limits = c(0, 100))

