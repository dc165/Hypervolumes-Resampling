library(hypervolume)
library(foreach)

# take a sample of points_per_sample points each iteration to get n resampled hypervolumes
bootstrap <- function(name, hv, n = 10, points_per_resample = nrow(hv@Data)) {
  dir.create(file.path('./Objects', name))
  foreach(i = 1:n, .combine = c) %do% {
    sample_dat = hv@Data[sample(1:nrow(hv@Data), points_per_resample, replace = TRUE),]
    h = hypervolume(sample_dat,
                    method = ifelse(hv@Method == 'One-class support vector machine', "svm", tolower(strsplit(hv@Method, " ")[[1]])[1]),
                    verbose = FALSE,
                    name = paste("resample", as.character(i))
    )
    path = paste0(h@Name, '.rds')
    saveRDS(h, file.path('./Objects', name, path))
  }
  return(file.path(getwd(), 'Objects', name))
}

# n bootstrap hypervolumes for each size in points per resample
bootstrap_seq <- function(name, hv, n = 10, points_per_resample = ncol(hv@Data), seq = 3:nrow(hv@Data)) {
  dir.create(file.path('./Objects', name))
  foreach(i = seq) %do% {
    subdir = paste('sample size', as.character(i))
    dir.create(file.path('./Objects', name, subdir))
    h = hypervolume(hv@Data[sample(1:nrow(hv@Data), i, replace = TRUE),],
                    method = ifelse(hv@Method == 'One-class support vector machine', "svm", tolower(strsplit(hv@Method, " ")[[1]])[1]),
                    verbose = FALSE,
                    name = paste("resample", as.character(i))
    )
    bootstrap(file.path(name, subdir), h, n, points_per_resample)
  }
  return(file.path(getwd(), 'Objects', name))
}


# Outputs k hypervolumes each with (k-1)/k of the total points used to create input hypervolume
k_split <- function(name, hv, k = 5) {
  dir.create(file.path('./Objects', name))
  npoints = nrow(hv@Data)
  shuffled = hv@Data[sample(1:npoints, npoints, replace = FALSE),]
  foreach(i = 1:k, .combine = c) %do% {
    range = (floor((i-1)*npoints/k) + 1):floor(i*npoints/k)
    train_dat = shuffled[-1 * range,]
    test_dat = shuffled[range,]
    dir_name = paste('split', as.character(i))
    dir.create(file.path('./Objects', name, dir_name))
    h = hypervolume(data = train_dat,
                method = ifelse(hv@Method == 'One-class support vector machine', "svm", tolower(strsplit(hv@Method, " ")[[1]])[1]),
                verbose = FALSE,
                name = paste("train", as.character(i))
    )
    path = paste0(h@Name, '.rds')
    saveRDS(h, file.path('./Objects', name, dir_name, path))
    t = hypervolume(data = test_dat,
                    method = ifelse(hv@Method == 'One-class support vector machine', "svm", tolower(strsplit(hv@Method, " ")[[1]])[1]),
                    verbose = FALSE,
                    name = paste("test", as.character(i))
    )
    path = paste0(t@Name, '.rds')
    saveRDS(t, file.path('./Objects', name, dir_name, path))
  }
  return(file.path(getwd(), 'Objects', name))
}

# Single interface for resampling
resample <- function(name, hv, method, n = 10, points_per_resample = nrow(hv@Data), k = 5, seq = 3:nrow(hv@Data)) {
  dir.create('./Objects', showWarnings = FALSE)
  if (method == 'bootstrap') {
    return(bootstrap(name, hv, n, points_per_resample))
  } else if (method == 'bootstrap_seq') {
    return(bootstrap_seq(name, hv, n, points_per_resample, seq))
  } else if (method == 'k-fold') {
    return(k_split(name, hv, k))
  } else if (method == 'LOO') {
    return(k_split(name, hv, nrow(hv@Data)))
  }
}
