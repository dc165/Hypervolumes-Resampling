library(hypervolume)
library(dplyr)
library(doParallel)
library(foreach)


# K-fold cross validation to estimate test error of model
cross_validate <- function(hv, k = 5, loss = function(x, y) {mean((x - y)^2)}) {
  hvs = c()
  losses = c()
  npoints = nrow(hv@Data)
  shuffled = hv@Data[sample(1:npoints, npoints, replace = FALSE),]
  for(i in 1:k) {
    range = (floor((i-1)*npoints/k) + 1):floor(i*npoints/k)
    train_dat = shuffled[-1 * range,]
    test_dat = shuffled[range,]
    hv_model <- hypervolume(data = train_dat,
                              method = ifelse(hv@Method == 'One-class support vector machine', "svm", tolower(strsplit(hv@Method, " ")[[1]])[1]),
                              verbose = FALSE
    )
    # losses <- c(losses, loss(hypervolume_estimate_probability(hv_model, test_dat), hypervolume_estimate_probability(hv, test_dat)))
    losses <- c(losses, loss(get_volume(hv), get_volume(hv_model)))
  }
  return(mean(losses))
}

# take a sample of points_per_sample points each iteration to get n resampled hypervolumes
bootstrap <- function(hv, n = 10, points_per_resample = nrow(hv@Data)) {
  hvs <- new("HypervolumeList")
  hvs@HVList <- foreach(i = 1:n, .combine = c) %do% {
    sample_dat = hv@Data[sample(1:nrow(hv@Data), points_per_resample, replace = TRUE),]
    hypervolume(sample_dat,
                method = ifelse(hv@Method == 'One-class support vector machine', "svm", tolower(strsplit(hv@Method, " ")[[1]])[1]),
                verbose = FALSE,
                name = paste("resample", as.character(i))
                )
  }
  return(hvs)
}

# minimal performance difference compared to bootstrap
bootstrap2 <- function(hv, n = 10, points_per_resample = -1) {
  hvs <- new("HypervolumeList")
  hvs@HVList <- lapply(1:n, function(x) {hypervolume(hv@Data[sample(1:nrow(hv@Data), ifelse(points_per_resample == -1, nrow(hv@Data), points_per_resample), replace = TRUE),],
                              method = ifelse(hv@Method == 'One-class support vector machine', "svm", tolower(strsplit(hv@Method, " ")[[1]])[1]),
                              verbose = FALSE,
                              name = paste("resample")
  )})
  return(hvs)
}

# returns a function that returns hypervolume object from file
# potenetial memory leak, mitigated with rm? mem_change(hvs <- bootstrap3(hv, n = 3)) when from 2MB to 22KB
# return some kind of promise?
bootstrap3 <- function(hv, n = 10, points_per_resample = -1) {
  paths <- foreach(i = 1:n, .combine = c) %do% {
    sample_dat = hv@Data[sample(1:nrow(hv@Data), ifelse(points_per_resample == -1, nrow(hv@Data), points_per_resample), replace = TRUE),]
    hsample <- hypervolume(sample_dat,
                method = ifelse(hv@Method == 'One-class support vector machine', "svm", tolower(strsplit(hv@Method, " ")[[1]])[1]),
                verbose = FALSE,
                name = paste("resample", as.character(i))
    )
    path = paste0(hsample@Name, '.rds')
    saveRDS(hsample, path)
    rm(sample_dat, hsample)
    list(path)
  }
  return(function() {hvs = new("HypervolumeList"); hvs@HVList <- lapply(paths, readRDS); return(hvs)})
}

# Outputs k hypervolumes each with (k-1)/k of the total points used to create input hypervolume
k_split <- function(hv, k = 5) {
  hvs <- new("HypervolumeList")
  npoints = nrow(hv@Data)
  shuffled = hv@Data[sample(1:npoints, npoints, replace = FALSE),]
  hvs@HVList <- foreach(i = 1:k, .combine = c) %do% {
    range = (floor((i-1)*npoints/k) + 1):floor(i*npoints/k)
    train_dat = shuffled[-1 * range,]
    hypervolume(data = train_dat,
                method = ifelse(hv@Method == 'One-class support vector machine', "svm", tolower(strsplit(hv@Method, " ")[[1]])[1]),
                verbose = FALSE
    )
  }
  return(hvs)
}

# Single interface for resampling
resample <- function(hv, method = 'bootstrap', n = 3, points_per_sample = -1, k = 5) {
  if (method == 'bootstrap') {
    return(bootstrap(hv, n, points_per_resample))
  } else if (method == 'k-fold') {
    return(k_split(hv, k))
  } else if (method == 'LOO') {
    return(k_split(hv, nrow(hv@Data)))
  } else if (method == 'bootstrap3') {
    return(bootstrap3(hv, n, points_per_resample))
  }
}