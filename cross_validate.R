library(foreach)
library(hypervolume)

# K-fold cross validation to estimate test error of model
cross_validate <- function(path, loss = function(x, y) {mean((x - y)^2)}, hv_param = get_volume) {
  paths = list.files(path = path)
  losses <- foreach(i = paths, .combine = c) %do% {
    hvs = list.files(file.path(path, i))
    train_model = readRDS(file.path(path, i, hvs[2]))
    test_model = readRDS(file.path(path, i, hvs[1]))
    loss(hv_param(train_model), hv_param(test_model))
  }
  return(losses)
}
