plot.effort.maps <- function(run_id, allresults, summary_results, daly_subcatchment_layer, daly_river_layer)
{
  exp_id <- unique(summary_results[,"ID_exp"])
  
  # get site action array from specific run 
  site_action_array <- data.frame(allresults[[run_id]][[7]])
  
  # get parameters of the run 
  estimate <- summary_results[summary_results[,"ID_run"]==run_id, "estimate"]
  target_level <- summary_results[summary_results[,"ID_run"]==run_id, "target_level"]
  
  # create GridID column
  site_action_array <- cbind(site_action_array, GridID=seq(1,nrow(site_action_array),1))
  
  # join site action array to the shapefile 
  daly_subcatchment_layer@data <- left_join(daly_subcatchment_layer@data, site_action_array, by = 'GridID')
  
  # convert levels of effort to factors 
  daly_subcatchment_layer@data$Buffalo <- factor(daly_subcatchment_layer@data$Buffalo, levels=c(1,2,3), labels=c("Low","Medium","High"))
  daly_subcatchment_layer@data$Pig <- factor(daly_subcatchment_layer@data$Pig, levels=c(1,2,3), labels=c("Low","Medium","High"))
  daly_subcatchment_layer@data$Grazing <- factor(daly_subcatchment_layer@data$Grazing, levels=c(1,2,3), labels=c("Low","Medium","High"))
  daly_subcatchment_layer@data$Weed <- factor(daly_subcatchment_layer@data$Weed, levels=c(1,2,3), labels=c("Low","Medium","High"))
  
  # dissolve subcatchment layer to get region outline
  daly_subcatchment_layer_outline <- gUnaryUnion(daly_subcatchment_layer, id = daly_subcatchment_layer@data$OID_)
  
  # define geographic coordinate system / long,lat 
  geograhic_CRS <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")  # geographical, datum WGS84
  
  proj4string(daly_subcatchment_layer) <- geograhic_CRS
  proj4string(daly_river_layer) <- geograhic_CRS
  proj4string(daly_subcatchment_layer_outline) <- geograhic_CRS
  
  # define projected coordinate system / eastings and northings (GDA94/Australian Albers, EPSG:3577) 
  # look up http://www.spatialreference.org/
  projected_CRS <- CRS('+proj=aea +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=132 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs')
  
  daly_subcatchment_layer_proj <- spTransform(daly_subcatchment_layer, projected_CRS)
  daly_river_layer_proj <- spTransform(daly_river_layer, projected_CRS)
  daly_subcatchment_layer_outline_proj <- spTransform(daly_subcatchment_layer_outline, projected_CRS) 
  
  # get coordinates box for helping placing arrow and scale bar later  
  bb <- bbox(daly_subcatchment_layer_proj) 
  
  # create list objects for shapefiles to overaly 
  daly_river_layer_list <- list("sp.lines", 
                                daly_river_layer_proj, 
                                col = "grey")
  
  daly_subcatchment_layer_outline_list <- list("sp.polygons", 
                                               daly_subcatchment_layer_outline_proj, 
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
  text1 <- list("sp.text", c(bb[1,1] + 23000, bb[2,1] + 12000), "0", cex=1.5, which = 4) 
  text2 <- list("sp.text", c(bb[1,1] + 155000, bb[2,1] + 12000), "100 km", cex=1.5, which = 4) 
  
  # Useful for checking parameters of trellis graphics
  #names(trellis.par.get())
  #trellis.par.get("strip.border")
  
  ###
  # NOTE: How to costumize the key (legend) in a spplot with polygons
  ###
  # Disable the default colorkey (colorkey=FALSE) and define a custom key using the "key"
  # argument. It expects a list whose components define the elements of the
  # legend. You should read the help page of lattice::xyplot for details about "key".
  
  myColors <- brewer.pal(5, "YlOrBr")[c(2,3,5)] #avoid using the ligthest colour 
  
  myKey <- list(text=list(lab=levels(daly_subcatchment_layer_proj@data$Buffalo)),
                rectangles=list(col = myColors, border=FALSE),
                space='right',
                columns=1,
                title='Management effort',
                cex.title=1.2,
                padding.text=3)
  
  plot_file_name <- paste("Effort_map_run", run_id, sep="_")
  
  out_pt <- file.path("figures", paste("figures_exp", exp_id, sep="_"))
            
  dir.create(out_pt, FALSE, TRUE)          
  
  tiff(file.path(out_pt, paste0(plot_file_name, ".tiff")), 
       width = 10, 
       height = 7, 
       units = "in", 
       compression = "lzw", 
       res = 300)
  
  # Colour plot. May take a while.    
  print(spplot(daly_subcatchment_layer_proj, c("Buffalo", "Pig", "Grazing", "Weed"), col=NA,
               names.attr = c("Shooting of water buffalos", "Shooting of feral pigs", "Riparian fencing", "Chemical spraying of Paragrass"),
               layout = c(2,2),
               as.table = TRUE,
               main = list(label = "Map of management effort", cex = 1.3),
               colorkey = FALSE,
               key = myKey,
               col.regions = myColors,
               par.settings = list(axis.line=list(col = "transparent"),
                                   strip.background=list(col = "transparent"),
                                   strip.border=list(lty=0)),
               par.strip.text = list(col="black", font=1.8),
               sp.layout = list(daly_river_layer_list,
                                daly_subcatchment_layer_outline_list,
                                scale_bar,
                                text1,
                                text2,
                                north_arrow)))
  
  dev.off()
}
