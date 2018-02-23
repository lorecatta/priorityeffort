
devtools::load_all()

library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(grid)
library(ggsn)


# define parameters -----------------------------------------------------------


parameters <- list(
  Exp = 1,
  Replicates = 10,
  no_ITER = 1000000,
  Temp_zero = 1,
  cooling_factor = 0.99999,
  fixed_targets = FALSE,
  occurrence_limits = c(500, 10000),
  target_limits = c(1, 0.1),
  TargetLevel = c(seq(50, 1000, 50), seq(1250, 10000, 250)),
  spf = 10,
  print_every_iter = 5)

threat_names <- c("buffalo", "pig", "weed", "grazing")

action_names <- c("Shooting of water buffalos",
                  "Shooting of feral pigs",
                  "Chemical spraying of Paragrass",
                  "Riparian fencing")

out_pt <- file.path("figures", paste0("exp_", parameters$Exp))

plot_file_name <- "nice_effort_map.png"


# load data -------------------------------------------------------------------


results_exp <- readRDS(
  file.path("output", paste0("exp_", parameters$Exp), "solution.rds"))

output_summary_best <- read.csv(
  file.path("output", paste0("exp_", parameters$Exp), "summary_table.csv"),
  header = TRUE)


# make ggplot friendly  -------------------------------------------------------


daly_fort <- fortify(daly, region = "GridID") %>%
  mutate(id = as.numeric(id))

outline_fort <- fortify(daly_outline)

rivers_fort <- fortify(rivers)

aus_outline_fort <- fortify(aus_outline)


# join solution array to shapefile --------------------------------------------


best_run <- output_summary_best$ID_run

# solution array
sol_array <- as.data.frame(results_exp[[3]]$site_action_array)

sol_array$Grid_ID <- seq_len(nrow(sol_array))

daly_fort <- left_join(daly_fort, sol_array, by = c("id" = "Grid_ID"))

daly_fort_long <- reshape2::melt(daly_fort,
                                 id.vars = c("long", "lat", "id", "group"),
                                 measure.vars = threat_names,
                                 variable.name = "threat")

daly_fort_long <- filter(daly_fort_long, value != 0)

daly_fort_long$value <- factor(daly_fort_long$value,
                               levels = c(1, 2, 3),
                               labels = c("Low", "Medium", "High"))


# plot ------------------------------------------------------------------------


my_labs <- as_labeller(setNames(action_names, threat_names))

my_col <- brewer.pal(5, "YlOrBr")[c(2, 3, 5)]

offset <- 0.5

xlim <- range(daly_fort_long$long) + c(-offset, offset)
ylim <- range(daly_fort_long$lat) + c(-offset, offset)

insetrec <- data.frame(xmin = min(xlim),
                       xmax = max(xlim),
                       ymin = min(ylim),
                       ymax = max(ylim))

png(file.path(out_pt, plot_file_name),
    width = 17,
    height = 12,
    units = "cm",
    res = 600)

b <- ggplot() +
  geom_polygon(data = daly_fort_long,
               aes(x = long,
                   y = lat,
                   group = group,
                   fill = value),
               color = NA) +
  geom_path(data = outline_fort, aes(long, lat), size = 0.2) +
  geom_path(data = rivers_fort,
            aes(long, lat, group = group),
            colour = "lightblue",
            size = 0.2) +
  scale_x_continuous("Longitude") +
  scale_y_continuous("Latitude") +
  facet_wrap(~ threat, ncol = 2, labeller = labeller(threat = my_labs)) +
  coord_equal() +
  scale_fill_manual(values = my_col,
                    guide = guide_legend(title = "Management effort",
                                         keywidth = 1.5,
                                         keyheight = 1.5)) +
  theme_bw() +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"),
        axis.title.x = element_text(margin = margin(t = 10)),
        legend.position = "right",
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 11),
        strip.text = element_text(size = 9)) +
  scalebar(data = daly_fort_long,
           location = "bottomleft",
           dist = 50,
           st.dist = 0.05,
           st.size = 2,
           height = 0.02,
           dd2km = TRUE,
           model = "WGS84",
           anchor = c(x = 130.2, y = -16.2),
           facet.var = "threat",
           facet.lev = "buffalo")

a <- ggplot() +
  geom_path(data = aus_outline_fort,
            aes(x = long, y = lat, group = group),
            colour = "black",
            size = 0.2) +
  geom_rect(data = insetrec,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            alpha = 0,
            colour = "red",
            size = 0.8,
            linetype = 1) +
  coord_equal()+
  theme_void() +
  theme(plot.background = element_rect(fill = "white",
                                       linetype = 1,
                                       size = 0.3,
                                       colour = "black"))


grid.newpage()

vpb_ <- viewport(width = 1, height = 1, x = 0.5, y = 0.5)  # the larger map

vpa_ <- viewport(width = 0.25, height = 0.25, x = 0.84, y = 0.8)  # the inset in upper right

print(b, vp = vpb_)
print(a, vp = vpa_)
north2(ggp = b, x = 0.175, y = 0.64, scale = 0.07, symbol = 12)

dev.off()
