library(agrometAPI) # github pokyah
library(geoTools) # github pokyah
library(dplyr)
library(chron)
library(gstat)
library(tidyr)
library(sp)
library(raster)
library(sf)
library(ggsn) #must be from github
library(sf)
library(leaflet)
library(dplyr)

dfrom = "2018-07-01"
dto = "2018-07-03"

plu.obs <- get_from_agromet_API(dfrom = dfrom, dto = dto, sensors = "plu")
plu.obs <- prepare_agromet_API_data.fun(plu.obs)

st_info <- plu.obs %>%
  filter(mtime == unique(plu.obs$mtime)[1]) %>%
  dplyr::select(one_of(c(
    "sid", "poste",
    "longitude", "latitude",
    "altitude"))
  )

# Filtering observations to keep only the useful ones and adding cumuls
plu.obs <- plu.obs  %>%
  filter(network_name == "pameseb") %>%
  filter(type_name != "Sencrop") %>%
  filter(!is.na(to)) %>%
  filter(state == "Ok") %>%
  filter(!is.na(plu)) %>%
  mutate(date = as.Date.POSIXct(mtime)) %>%
  group_by(date) %>%
  mutate(cum.obs = sum(plu))

plu.norm <- get_from_agromet_API(table_name = "get_tmy", sensors = "plu_sum", month_day = "all")
plu.norm <- prepare_agromet_API_data.fun(plu.norm, table_name = "get_tmy")
plu.norm <- plu.norm %>%
  mutate(julian = julian(month, day, 1970) + 1)

plu.obs$julian <- julian(plu.obs$date, format = "%Y/%m/%d", origin = as.Date("2018-01-01"))[1]

plu <- plu.obs %>% dplyr::left_join(
  dplyr::select(plu.norm, one_of(c("plu_sum", "julian", "sid"))), by = c("julian","sid"))

summary <- plu %>%
  group_by(sid) %>%
  summarise_at(vars(plu_sum, cum.obs), sum)

summary <- summary %>%
  mutate(defExHyd = cum.obs - plu_sum) %>%
  mutate(ind_plu = cum.obs / plu_sum)

summary <- summary %>%
  left_join(st_info, by = "sid")

summary.sp <- data.frame(summary)
coordinates(summary.sp) <- ~longitude+latitude
crs(summary.sp) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") #we know it from API meta
summary.sp <- spTransform(summary.sp, CRS("+init=epsg:3812"))

wallonia.sp <- raster::getData('GADM', country = "BE ", level = 1)
wallonia.sp <- subset(wallonia.sp, NAME_1 == "Wallonie")
wallonia.sp <- spTransform(summary.sp, CRS("+init=epsg:3812"))

grid.sf <- build.vs.grid.fun("BE", "Wallonie", 1000, "centers", sf.bool = TRUE, EPSG.chr = "3812")
grid.sp <- as(grid.sf, "Spatial")
grid.sp <- spTransform(grid.sp, CRS("+init=epsg:3812"))
#grid.sp <- spTransform(grid.sp, CRS("+init=epsg:3812"))
grid.grid <- grid.sp
gridded(grid.grid) = TRUE

# grid.sp <- spTransform(grid.sp, CRS("+init=epsg:4326"))
# summary.sp <- spTransform(summary.sp, CRS("+init=epsg:4326"))
# wallonia.sp <- spTransform(summary.sp, CRS("+init=epsg:4326"))

# interpolating
defExHyd.idw = idw(defExHyd~1, summary.sp, grid.grid)
ind_plu.idw = idw(ind_plu~1, summary.sp, grid.grid)

defExHyd.idw.grid <- as(defExHyd.idw, "SpatialGridDataFrame") # ==> NA's
ind_plu.idw.grid <- as(ind_plu.idw, "SpatialGridDataFrame") # ==> NA's

# mapping
defExHyd.plot.map <- static.ggmap(gridded.data.df = as.data.frame(defExHyd.idw),
  boundaries.sf = st_as_sf(wallonia.sp),
  layer.error.bool = FALSE,
  legend.error.bool = FALSE,
  pretty_breaks.bool = TRUE,
  title.chr = "déficit hydrique",
  target.chr = "var1.pred",
  legend.chr = "ahah"
)
# defExHyd.plot <- spplot(defExHyd.idw["var1.pred"], do.log = F, colorkey = TRUE,  main = "Def Hyd")
# ind_plu.plot <- spplot(ind_plu.idw["var1.pred"],  main = "Ind plu")

# making it spatial object class sf
defExHyd.idw.sf <- st_as_sf(defExHyd.idw)
# Now we need to inject this point info into polygon centered arounds the points to fake a raster layer but which is interactive
# making the gridded mlr.krg a raster
grid.r <- raster::raster(grid.sp)
# convert raster to polygons
grid.sp = raster::rasterToPolygons(grid.r, dissolve = F)
class(grid.sp) # SpatialPolygonsDataFrame
# converting to sf for later joining
grid.sf <- st_as_sf(grid.sp)
st_crs(grid.sf)
# injecting the prediction and se data into the polygon grid doing a spatial join
# interpolated.sf <- st_join(grid.sf, interpolated.sf) %>% select(one_of(c("response", "se")))
defExHyd.idw.sf.pg.sf <- dplyr::bind_cols(grid.sf, defExHyd.idw.sf)
defExHyd.idw.sf.pg.sf <- defExHyd.idw.sf.pg.sf %>% select(one_of(c("var1.pred", "var1.var")))
defExHyd.idw.sf.pg.sf <- st_transform(defExHyd.idw.sf.pg.sf, 4326)
# Do we have polygons ?
head(defExHyd.idw.sf.pg.sf)

leafletize(defExHyd.idw.sf)

# defining the function to create a palette of different levels of alpha for the choosen color
alphaPal <- function(color) {
  alpha <- seq(0,1,0.1)
  r <- col2rgb(color, alpha=T)
  r <- t(apply(r, 1, rep, length(alpha)))
  # Apply alpha
  r[4,] <- alpha*255
  r <- r/255.0
  codes <- (rgb(r[1,], r[2,], r[3,], r[4,]))
  return(codes)
}

# defining the function to map using leaflet
leafletize <- function(data.sf){
  # to make the map responsive
  responsiveness.chr = "\'<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\'"

  # defining the color palette for the response
  varPal <- colorNumeric(
    palette = "Spectral",
    domain = data.sf$response
  )

  # defining the transparent colorpal for the se
  uncPal <- colorNumeric(
    palette = alphaPal("#e6e6e6"),
    domain = data.sf$se,
    alpha = TRUE
  )

  #
  prediction.map <- leaflet::leaflet(data.sf) %>%
    addProviderTiles(group = "Stamen",
      providers$Stamen.Toner,
      options = providerTileOptions(opacity = 0.25)
    ) %>%
    addProviderTiles(group = "Satellite",
      providers$Esri.WorldImagery,
      options = providerTileOptions(opacity = 1)
    ) %>%
    fitBounds(sf::st_bbox(data.sf)[[1]],
      sf::st_bbox(data.sf)[[2]],
      sf::st_bbox(data.sf)[[3]],
      sf::st_bbox(data.sf)[[4]]
    ) %>%
    addLayersControl(baseGroups = c("Stamen", "Satellite"),
      overlayGroups = c("prediction", "se"),
      options = layersControlOptions(collapsed = TRUE)
    ) %>%
    addEasyButton(easyButton(
      icon="fa-crosshairs", title="Locate Me",
      onClick=JS("function(btn, map){ map.locate({setView: true}); }"))) %>%
    htmlwidgets::onRender(paste0("
      function(el, x) {
      $('head').append(",responsiveness.chr,");
      }")
    ) %>%
    addPolygons(
      group = "prediction",
      color = "#444444", stroke = FALSE, weight = 1, smoothFactor = 0.5,
      opacity = 1.0, fillOpacity = 0.9,
      fillColor = ~varPal(response),
      highlightOptions = highlightOptions(color = "white", weight = 2,
        bringToFront = TRUE)
    )%>%
    addLegend(
      position = "bottomright", pal = varPal, values = ~response,
      title = "prediction",
      group = "prediction",
      opacity = 1
    )%>%
    addPolygons(
      group = "se",
      color = "#444444", stroke = FALSE, weight = 1, smoothFactor = 0.5,
      opacity = 1.0, fillOpacity = 1,
      fillColor = ~uncPal(se),
      highlightOptions = highlightOptions(color = "white", weight = 2,
        bringToFront = TRUE),
      label = ~ paste("prediction:", signif(data.sf$response, 2), "\n","se: ", signif(data.sf$se, 2))
    ) %>%
    addLegend(
      group = "se",
      position = "bottomleft", pal = uncPal, values = ~se,
      title = "se",
      opacity = 1
    )
  return(prediction.map)
}
# create the map object
prediction.map <- leafletize(st_as_sf(defExHyd.idw))

# render it
html <- list(h3(paste0("interactive prediction map")),
  prediction.map
)
tagList(html)
```


