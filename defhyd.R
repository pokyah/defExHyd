library(agrometAPI) # github pokyah
library(geoTools) # github pokyah
library(chron)
library(gstat)
library(tidyr)
library(sp)
library(raster)
library(sf)
library(leaflet)
library(dplyr)
library(mlr)
source("gstat.R")

# wallonia borders (for EPSG CRS and proj4 see epsg.io)
wallonia.sp <- raster::getData('GADM', country = "BE ", level = 1)
wallonia.sp <- subset(wallonia.sp, NAME_1 == "Wallonie")
wallonia.sp <- spTransform(wallonia.sp, CRS("+proj=lcc +lat_1=49.83333333333334 +lat_2=51.16666666666666 +lat_0=50.797815 +lon_0=4.359215833333333 +x_0=649328 +y_0=665262 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs "))
wallonia.sf = st_as_sf(wallonia.sp)

# interpolation grid - https://stackoverflow.com/questions/43436466/create-grid-in-r-for-kriging-in-gstat/43444232
grd = sp::makegrid(x = wallonia.sp, cellsize = 250,
  pretty = TRUE)
colnames(grd) <- c('x','y')
grd_pts <- SpatialPoints(coords = grd,
  proj4string = CRS(proj4string(wallonia.sp)))
grid.sp <- grd_pts[wallonia.sp, ]
grid.df = as.data.frame(grid.sp)
grid.grid <- grid.sp
gridded(grid.grid) = TRUE
grid.sf = st_as_sf(grid.sp)

# extracting normal data from API
plu.norm <- get_from_agromet_API(table_name = "get_tmy", sensors = "plu_sum", month_day = "all")
plu.norm <- prepare_agromet_API_data.fun(plu.norm, table_name = "get_tmy")
plu.norm <- plu.norm %>%
  mutate(julian = julian(month, day, 1970) + 1)

######



dfrom = "2018-02-01"
dto = "2018-03-04"

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
  filter(from <= min(plu.obs$from, na.rm = TRUE)) %>%
  group_by(date) %>%
  mutate(cum.obs = sum(plu))

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

# SPATIAL
# make station data spatial
summary.sp <- data.frame(summary)
coordinates(summary.sp) <- ~longitude+latitude
crs(summary.sp) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") #we know it from API meta
summary.sp <- spTransform(summary.sp, CRS("+proj=lcc +lat_1=49.83333333333334 +lat_2=51.16666666666666 +lat_0=50.797815 +lon_0=4.359215833333333 +x_0=649328 +y_0=665262 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs "))



# INTERPOLATION
defExHyd.idw.sp = idw(defExHyd~1, summary.sp, grid.grid)
defExHyd.idw.df = data.frame(defExHyd.idw.sp)
defExHyd.idw.sf <- st_as_sf(defExHyd.idw.sp)

indPlu.idw.sp = idw(ind_plu~1, summary.sp, grid.grid)
indPlu.idw.df = data.frame(defExHyd.idw.sp)
indPlu.idw.sf <- st_as_sf(defExHyd.idw.sp)

# MAPPING - Static
defExHyd.idw.grid = defExHyd.idw.sp
gridded(defExHyd.idw.grid) = TRUE
defExHyd.idw.grid = as(defExHyd.idw.grid, "SpatialGridDataFrame")
defExHyd.idw.grid.df = as.data.frame(defExHyd.idw.grid)
defExHyd.idw.grid.df  = dplyr::rename(defExHyd.idw.grid.df, coords.x1 = x, coords.x2 = y)
indPlu.idw.grid = indPlu.idw.sp
gridded(indPlu.idw.grid) = TRUE
indPlu.idw.grid = as(indPlu.idw.grid, "SpatialGridDataFrame")
indPlu.idw.grid.df = as.data.frame(indPlu.idw.grid)
indPlu.idw.grid.df  = dplyr::rename(indPlu.idw.grid.df, coords.x1 = x, coords.x2 = y)

defExHyd.plot.map = build.static.ggmap(gridded.data.df = defExHyd.idw.grid.df,
  boundaries.sf = wallonia.sf,
  layer.error.bool = FALSE,
  legend.error.bool = FALSE,
  pretty_breaks.bool = TRUE,
  title.chr = "déficit hydrique (mm)",
  target.chr = "var1.pred",
  legend.chr = ""
)
indPl.plot.map = build.static.ggmap(gridded.data.df = indPlu.idw.grid.df,
  boundaries.sf = wallonia.sf,
  layer.error.bool = FALSE,
  legend.error.bool = FALSE,
  pretty_breaks.bool = TRUE,
  title.chr = "Indice pluviométrique (mm)",
  target.chr = "var1.pred",
  legend.chr = ""
)

# MAPPING - Static
defExHyd.idw.sf = st_as_sf(defExHyd.idw.sp)
# Now we need to inject this point info into polygon centered arounds the points to fake a raster layer but which is interactive
grid.r <- raster::raster(defExHyd.idw.sp, values = TRUE)
# convert raster to polygons
grid.pg.sp = raster::rasterToPolygons(grid.r, dissolve = F)
class(grid.pg.sp) # SpatialPolygonsDataFrame
# converting to sf for later joining
grid.pg.sf = st_as_sf(grid.pg.sp)
# st_crs(grid.pg.sf) <- 3812
# injecting the prediction and se data into the polygon grid doing a spatial join
defExHyd.idw.sf.pg.sf = st_join(grid.pg.sf, defExHyd.idw.sf) #FIXME
# Do we have polygons ?
head(defExHyd.idw.sf.pg.sf)
#project to geographic CRS
defExHyd.idw.sf.pg.sf <- st_transform(defExHyd.idw.sf.pg.sf, crs = 4326)
#interacrtive mapping
interactive.map = leafletize(defExHyd.idw.sf.pg.sf, se.bool = FALSE)


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





