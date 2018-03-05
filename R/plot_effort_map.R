#' Create and save a map of selected effort for each action.
#'
#' @param solution a solution object. A list.
#' @param catch_shp the shape file of the subcatchments.
#' @param river_shp the shape file of the rivers.
#' @param catch_shp_outline the shape file of the study area border.
#'
#' @export
plot_effort_map <- function(solution, catch_shp, river_shp, catch_shp_outline) {

  #browser()

  exp_id <- solution$exp
  run_id <- solution$run

  site_action_array <- as.data.frame(solution$site_action_array)

  # create GridID column
  site_action_array <- cbind(site_action_array, GridID = seq(1, nrow(site_action_array), 1))

  # join site action array to the shapefile
  catch_shp@data <- left_join(catch_shp@data, site_action_array, by = 'GridID')

  # convert levels of effort to factors
  catch_shp@data$buffalo <- factor(catch_shp@data$buffalo,
                                   levels = c(1, 2, 3),
                                   labels = c("Low", "Medium", "High"))

  catch_shp@data$pig <- factor(catch_shp@data$pig,
                               levels = c(1, 2, 3),
                               labels = c("Low", "Medium", "High"))

  catch_shp@data$weed <- factor(catch_shp@data$weed,
                                levels = c(1, 2, 3),
                                labels = c("Low", "Medium", "High"))

  catch_shp@data$grazing <- factor(catch_shp@data$grazing,
                                   levels = c(1, 2, 3),
                                   labels = c("Low", "Medium", "High"))

  # get coordinates box for helping placing arrow and scale bar later
  bb <- bbox(catch_shp)

  # create list objects for shapefiles to overaly
  river_shp_list <- list("sp.lines",
                         river_shp,
                         col = "grey")

  catch_shp_outline_list <- list("sp.polygons",
                                 catch_shp_outline,
                                 col = "black",
                                 first = FALSE)

  north_arrow <- list("SpatialPolygonsRescale",
                      layout.north.arrow(),
                      offset = c(bb[1,1] + 60000, bb[2,1] + 42000),
                      scale = 60000,
                      which = 4)

  scale_bar <- list("SpatialPolygonsRescale",
                    layout.scale.bar(),
                    offset = c(bb[1,1] + 20000, bb[2,1] + 27000),
                    scale = 100000,
                    fill = c("transparent", "black"),
                    which = 4)

  # labels for scale bar
  text1 <- list("sp.text",
                c(bb[1,1] + 23000, bb[2,1] + 12000),
                "0",
                cex = 1.5,
                which = 4)

  text2 <- list("sp.text",
                c(bb[1,1] + 155000, bb[2,1] + 12000),
                "100 km",
                cex = 1.5,
                which = 4)

  # Useful for checking parameters of trellis graphics
  #names(trellis.par.get())
  #trellis.par.get("strip.border")

  ###
  # NOTE: How to costumize the key (legend) in a spplot with polygons
  ###
  # Disable the default colorkey (colorkey=FALSE) and
  # define a custom key using the "key" argument. It expects
  # a list whose components define the elements of the
  # legend. Read the help page of lattice::xyplot for details about "key".

  # avoid using the ligthest colour
  myColors <- brewer.pal(5, "YlOrBr")[c(2, 3, 5)]

  myKey <- list(text = list(lab = levels(catch_shp@data$buffalo)),
                rectangles = list(col = myColors, border = FALSE),
                space = "right",
                columns = 1,
                title = "Management effort",
                cex.title = 1.2,
                padding.text = 3)

  plot_file_name <- sprintf("Effort_map_run_%s%s", run_id, ".png")

  out_pt <- file.path("figures", paste("exp", exp_id, sep="_"))

  dir.create(out_pt, FALSE, TRUE)

  png(file.path(out_pt, plot_file_name),
      width = 11,
      height = 9,
      units = "in",
      res = 200)

  # Colour plot. May take a while.
  p <- spplot(catch_shp,
              c("buffalo", "pig", "grazing", "weed"),
              col = NA,
              names.attr = c("Shooting of water buffalos",
                             "Shooting of feral pigs",
                             "Riparian fencing",
                             "Chemical spraying of Paragrass"),
              layout = c(2, 2),
              as.table = TRUE,
              main = list(label = "Map of management effort", cex = 1.3),
              colorkey = FALSE,
              key = myKey,
              col.regions = myColors,
              par.settings = list(axis.line = list(col = "transparent"),
                                  strip.background = list(col = "transparent"),
                                  strip.border = list(lty = 0)),
              par.strip.text = list(col = "black", font = 1.8),
              sp.layout = list(river_shp_list,
                               catch_shp_outline_list,
                               scale_bar,
                               text1,
                               text2,
                               north_arrow))

  print(p)

  dev.off()

}
