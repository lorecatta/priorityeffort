
library(ggplot2)
library(rgdal)
library(rgeos)
library(dplyr)
library(RColorBrewer)



# ---------------------------------------- load data


threat_incidence_df <- read.table(
  file.path("data", 
            "threats",
            "buffalo_incidence.txt"), 
  header = TRUE,
  sep = ",")

cons_feat_array_df <- read.csv(
  file.path("data",
            "input_files",
            "Conservation Feature.csv"), 
  header = TRUE,
  stringsAsFactors = FALSE)

daly_subcatch <- readOGR(dsn = file.path("data", "shapefiles"), layer = "Catch_500")


# ---------------------------------------- make incidence data more interesting


threat_incidence_df[threat_incidence_df$buff_inc == 2, "buff_inc"] <- 3 
threat_incidence_df[threat_incidence_df$buff_inc == 1, "buff_inc"] <- 2 
threat_incidence_df[threat_incidence_df$buff_inc == 0, "buff_inc"] <- 1 


# ----------------------------------------


# dissolve subcatchment layer to get region outline
daly_subcatchment_layer_outline <- gUnaryUnion(daly_subcatch, id = daly_subcatch@data$OID_)

daly_subcatch_fort <- fortify(daly_subcatch, region = "GridID") %>% 
  mutate(id = as.numeric(id))

daly_outline_fort <- fortify(daly_subcatchment_layer_outline)

threat_incidence_df$GridID <- seq_len(nrow(threat_incidence_df))

dat_df_xy <- left_join(daly_subcatch_fort, threat_incidence_df, by = c("id" = "GridID"))

# YlOrRd <- brewer.pal(9, "YlOrRd")[c(2,5,7,9)]
# color.palette <- colorRampPalette(YlOrRd) # this is a function!

out_nm <- "one_threat_distribution.png"

cols <- c("1" = "grey80", "2" = "grey50", "3" = "grey20")

png(file.path("figures", out_nm), 
    width = 4, 
    height = 3, 
    units = "in", 
    pointsize = 12,
    res = 300)

p <- ggplot() +
  geom_polygon(data = dat_df_xy,
               aes(x = long, 
                   y = lat,
                   fill = as.factor(buff_inc), 
                   group = group)) +
  geom_path(data = daly_outline_fort,
            aes(x = long,
                y = lat,
                group = group),
            color = "black",
            size = 0.5) +
  scale_fill_manual(values = cols,
                    breaks = c(1, 2, 3),
                    labels = c("Low", "Medium", "High"),
                    guide = guide_legend(title = "",
                                         keyheight = 1.5, 
                                         keywidth = 1.5)) +
  scale_x_continuous(limits = c(130,134)) +
  theme_void() +
  theme(legend.text = element_text(size = 15),
        legend.title = element_text(size = 10),
        legend.position = c(0.15, 0.17),
        legend.key.size = unit(0.5, "in"),
        plot.margin = unit(c(0, 0, 0, 0), "cm"))

print(p)

dev.off()
