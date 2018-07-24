# For the example, using the output of mlr gstat KED from previous section
# loading the data
data(meuse)
data(meuse.grid)
# imputing values to missing data
meuse = impute(meuse, classes = list(numeric = imputeMean(), factor = imputeMode()),
dummy.classes = "integer")$data
meuse.grid = impute(meuse.grid, classes = list(numeric = imputeMean(), factor = imputeMode()),
dummy.classes = "integer")$data
# adding a column with log zinc
meuse <- meuse %>% dplyr::mutate(log_zinc = log(zinc))
# adding a column with sqrt dist
meuse <- meuse %>% dplyr::mutate(sqrt_dist = sqrt(dist))
meuse.grid <- meuse.grid %>% dplyr::mutate(sqrt_dist = sqrt(dist))
# defining the regression task
task = makeRegrTask(id = "meuse",  data = meuse, target = "log_zinc")
task.krg <- dropFeatures(task = task, features = getTaskFeatureNames(task)[-c(1,2,15)])
# defining the learner
lrn.krg = makeLearner(cl = "regr.gstat", id= "ln(zn) mlr kriging with external drift", predict.type = "response", model = list(psill=1, model="Exp", range=300, nugget=1), locations = ~x+y)
# training the model
mod.krg = train(lrn.krg, task.krg)
# kriging
newdata.pred.krg = predict(object = mod.krg, newdata = meuse.grid)
mlr.krg <- bind_cols(data.frame(meuse.grid), newdata.pred.krg$data)
# mapping
coordinates(mlr.krg) <- ~x+y
gridded(mlr.krg) = TRUE
pred.plot <- spplot(mlr.krg["response"], do.log = T, colorkey = TRUE, main = mod.krg$learner$id)
pred.plot
# SE - defining the standard error learner by altering the previous one.
se.lrn.krg = setPredictType(lrn.krg, predict.type = "se")
# training the SE model
se.mod.krg = train(se.lrn.krg, task.krg)
# SE kriging
se.newdata.pred.krg = predict(object = se.mod.krg, newdata = meuse.grid)
se.mlr.krg <- bind_cols(data.frame(meuse.grid), se.newdata.pred.krg$data)

# mapping with leaflet

# loading the required libraries
library(leaflet)
library(dplyr)
library(htmltools)
library(sf)
