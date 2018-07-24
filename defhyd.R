library(agrometAPI) # github pokyah
library(geoTools) # github pokyah
library(dplyr)
library(chron)
library(gstat)
library(tidyr)
library(sp)
library(raster)
library(sf)
library(leaflet)
library(dplyr)
source("gstat.R")

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

# SPATIAL

wallonia.sp <- raster::getData('GADM', country = "BE ", level = 1)
wallonia.sp <- subset(wallonia.sp, NAME_1 == "Wallonie")
wallonia.sp <- spTransform(wallonia.sp, CRS("+init=epsg:3812"))

grid.sf <- build.vs.grid.fun("BE", "Wallonie", 1000, "centers", sf.bool = TRUE, EPSG.chr = "3812")
grid.sp <- as(grid.sf, "Spatial")
# grid.sp <- spTransform(grid.sp, CRS("+init=epsg:3812"))
grid.df = as.data.frame(grid.sp)
#grid.sp <- spTransform(grid.sp, CRS("+init=epsg:3812"))
grid.grid <- grid.sp
gridded(grid.grid) = TRUE

# END SPATIAL

# interpolating
#defExHyd.idw = idw(defExHyd~1, summary.sp, grid.grid)
#ind_plu.idw = idw(ind_plu~1, summary.sp, grid.grid)


# INTERPOLATION MLR

# interpolating with gstat mlr
summary.df = as.data.frame(summary.sp)
# renaming cols
summary.df = dplyr::rename(summary.df, x = longitude, y = latitude)
grid.df = dplyr::rename(grid.df, x = coords.x1, y = coords.x2)

# defining the regression task
task.defExHyd = makeRegrTask(id = "defExHyd",  data = summary.df[c(4,8,9)], target = "defExHyd")
# defining the learner
lrn.idw = makeLearner(cl = "regr.gstat", id = "idw", predict.type = "response", locations = ~x+y)
# training the model
mod.idw = train(lrn.idw, task.defExHyd)
# predicting
newdata.pred.idw = predict(object = mod.idw, newdata = grid.df)
mlr.idw <- dplyr::bind_cols(grid.df, newdata.pred.idw$data)


# MAPPING

# DYNAMIC
# keeping what we need
interpolated.df <- mlr.idw[c(4,5,6,7)]
# making it spatial object class sf
interpolated.sf <- st_as_sf(interpolated.df,coords = c("x","y"))
# defining the crs
st_crs(interpolated.sf) <- 3812
# transforming to geographic CRS (EPSG = 4326)
#interpolated.sf <- st_transform(interpolated.sf, crs = 4326)
# Now we need to inject this point info into polygon centered arounds the points to fake a raster layer but which is interactive
class(mlr.idw.sp) # SpatialPixelsDataFrame
# making the gridded mlr.krg a raster
grid.r <- raster::raster(mlr.idw.sp, values = TRUE)
# convert raster to polygons
grid.pg.sp = raster::rasterToPolygons(grid.r, dissolve = F)
class(grid.pg.sp) # SpatialPolygonsDataFrame
# converting to sf for later joining
grid.pg.sf <- st_as_sf(grid.pg.sp)
st_crs(grid.pg.sf) <- 3812
# transforming to geographic CRS (EPSG = 4326)
#grid.sf <- st_transform(grid.pg.sf, crs = 4326)
# injecting the prediction and se data into the polygon grid doing a spatial join
# interpolated.sf <- st_join(grid.sf, interpolated.sf) %>% select(one_of(c("response", "se")))
interpolated.pg.sf = st_join(grid.pg.sf, interpolated.sf) #FIXME
interpolated.pg.sf = interpolated.pg.sf[c(2,3,4)]
# Do we have polygons ?
head(interpolated.pg.sf)
#project to geographic CRS
interpolated.pg.sf <- st_transform(interpolated.pg.sf, crs = 4326)
#interacrtive mapping
interactive.map = leafletize(interpolated.pg.sf, se.bool = FALSE)

#STATIC
interpolated.sp = mlr.idw
coordinates(interpolated.sp) <- ~x+y
gridded(interpolated.sp) = TRUE
pred.awful.map <- spplot(mlr.idw.sp["response"], do.log = T, colorkey = TRUE, main = mod.krg$learner$id)
interpolated.sp.grid = as(interpolated.sp, "SpatialGridDataFrame")
boundaries.sf <- st_as_sf(wallonia.sp, crs =  "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
boundaries.sf <- st_transform(boundaries.sf, crs = "+proj=lcc +lat_1=49.83333333333334 +lat_2=51.16666666666666 +lat_0=50.797815 +lon_0=4.359215833333333 +x_0=649328 +y_0=665262 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
interpolated.df.grid = as.data.frame(interpolated.sp.grid)
interpolated.df.grid  = dplyr::rename(interpolated.df.grid, coords.x1 = x, coords.x2 = y)

# mapping static
defExHyd.plot.map = build.static.ggmap(gridded.data.df = interpolated.df.grid,
  boundaries.sf = boundaries.sf,
  layer.error.bool = FALSE,
  legend.error.bool = FALSE,
  pretty_breaks.bool = TRUE,
  title.chr = "déficit hydrique",
  target.chr = "response",
  legend.chr = "mm"
)



defExHyd.idw.grid = as(defExHyd.idw, "SpatialGridDataFrame") # ==> NA's
ind_plu.idw.grid = as(ind_plu.idw, "SpatialGridDataFrame") # ==> NA's

defExHyd.idw.df = as.data.frame(defExHyd.idw.grid)
ind_plu.idw.df = as.data.frame(ind_plu.idw.grid)

boundaries.sf <- st_as_sf(wallonia.sp, crs =  "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
boundaries.sf <- st_transform(boundaries.sf, crs = "+proj=lcc +lat_1=49.83333333333334 +lat_2=51.16666666666666 +lat_0=50.797815 +lon_0=4.359215833333333 +x_0=649328 +y_0=665262 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")

# defining the function to create a palette of different levels of alpha for the choosen color
alphaPal <- function(color) {
  alpha <- seq(0,1,0.1)
  r <- col2rgb(color, alpha = T)
  r <- t(apply(r, 1, rep, length(alpha)))
  # Apply alpha
  r[4,] <- alpha*255
  r <- r/255.0
  codes <- (rgb(r[1,], r[2,], r[3,], r[4,]))
  return(codes)
}


# https://stackoverflow.com/questions/37830819/developing-shiny-app-as-a-package-and-deploying-it-to-shiny-server





