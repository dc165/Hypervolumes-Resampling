library(hypervolume)
library(foreach)
library(progress)
library(mvtnorm)
source('Utils.R')

# take a sample of points_per_sample points each iteration to get n resampled hypervolumes
bootstrap <- function(name, hv, n = 10, points_per_resample = 'sample_size', verbose = TRUE) {
  dir.create(file.path('./Objects', name))
  if(verbose) {
    pb = progress_bar$new(total = n)
  }
  foreach(i = 1:n, .combine = c) %do% {
    if(points_per_resample == 'sample_size') {
      sample_dat = hv@Data[sample(1:nrow(hv@Data), nrow(hv@Data), replace = TRUE),]
    } else {
      sample_dat = hv@Data[sample(1:nrow(hv@Data), points_per_resample, replace = TRUE),]
    }
    h = copy_param_hypervolume(hv, sample_dat, name = paste("resample", as.character(i)))
    path = paste0(h@Name, '.rds')
    saveRDS(h, file.path('./Objects', name, path))
    if(verbose) {
      pb$tick()
    }
  }
  return(file.path(getwd(), 'Objects', name))
}

# n bootstrap hypervolumes for each size in seq.
bootstrap_seq <- function(name, hv, n = 10, points_per_resample = 'sample_size', seq = 3:nrow(hv@Data), verbose = TRUE) {
  dir.create(file.path('./Objects', name))
  foreach(i = seq) %do% {
    subdir = paste('sample size', as.character(i))
    dir.create(file.path('./Objects', name, subdir))
    h = copy_param_hypervolume(hv, hv@Data[sample(1:nrow(hv@Data), i, replace = TRUE),], name = paste("resample", as.character(i)))
    bootstrap(file.path(name, subdir), h, n, points_per_resample, verbose)
    saveRDS(h, file.path('./Objects', name, subdir, 'original.rds'))
  }
  return(file.path(getwd(), 'Objects', name))
}


# Outputs k hypervolumes each with (k-1)/k of the total points used to create input hypervolume
k_split <- function(name, hv, k = 5, verbose = TRUE) {
  dir.create(file.path('./Objects', name))
  npoints = nrow(hv@Data)
  shuffled = hv@Data[sample(1:npoints, npoints, replace = FALSE),]
  if(verbose) {
    pb = progress_bar$new(total = k*3)
  }
  foreach(i = 1:k, .combine = c) %do% {
    range = (floor((i-1)*npoints/k) + 1):floor(i*npoints/k)
    train_dat = shuffled[-1 * range,]
    test_dat = shuffled[range,]
    dir_name = paste('split', as.character(i))
    dir.create(file.path('./Objects', name, dir_name))
    h = copy_param_hypervolume(hv, data = train_dat, name = paste("train", as.character(i)))
    path = paste0(h@Name, '.rds')
    saveRDS(h, file.path('./Objects', name, dir_name, path))
    if(verbose) {
      pb$tick()
      pb$tick()
    }
    t = copy_param_hypervolume(hv, data = test_dat, name = paste("test", as.character(i)))
    path = paste0(t@Name, '.rds')
    saveRDS(t, file.path('./Objects', name, dir_name, path))
    if(verbose) {
      pb$tick()
    }
  }
  return(file.path(getwd(), 'Objects', name))
}

# Introduce bias using weights and multinomial random numbers
# Generate weights from distribution
# mu has same number of dimensions as cols_to_bias, point to bias towards
# sigma has same number of dimensions as cols_to_bias, strength of bias in each dimension
# Use mu and sigma as parameters of multivariate normal distribution or input own weight function that takes in a matrix argument
# cols_to_bias are indices of hv@Data
sampling_bias_bootstrap <- function(name, hv, n = 10, points_per_resample = 'sample_size', verbose = TRUE, mu = NULL, sigma = NULL, cols_to_bias = 1:ncol(hv@Data), weight_func = NULL) {
  dir.create(file.path('./Objects', name))
  if(verbose) {
    pb = progress_bar$new(total = n)
  }
  foreach(i = 1:n, .combine = c) %do% {
    if(is.null(weight_func)) {
      if(length(mu) == 1) {
        weights = dnorm(hv@Data[,cols_to_bias], mean = mu, sd = sqrt(sigma))
      } else {
        weights = dmvnorm(hv@Data[,cols_to_bias], mean = mu, sigma = diag(sigma))
      }
    } else {
      weights = weight_func(hv@Data[,cols_to_bias])
    }
    if(points_per_resample == 'sample_size') {
      points = apply(rmultinom(nrow(hv@Data), 1, weights) == 1, 2, which)
    } else {
      points = apply(rmultinom(points_per_resample, 1, weights) == 1, 2, which)
    }
    sample_dat = hv@Data[points,]
    h = copy_param_hypervolume(hv, sample_dat, name = paste("resample", as.character(i)))
    path = paste0(h@Name, '.rds')
    saveRDS(h, file.path('./Objects', name, path))
    if(verbose) {
      pb$tick()
    }
  }
  return(file.path(getwd(), 'Objects', name))
}

# Single interface for resampling
# Creates Objects directory in current working directory
# Returns absolute path to file with hypervolume files
resample <- function(name, hv, method, n = 10, points_per_resample = 'sample_size', seq = 3:nrow(hv@Data), verbose = TRUE, mu = NULL, sigma = NULL, cols_to_bias = 1:ncol(hv@Data)) {
  if (n <= 0) {
    stop("Invalid value for n")
  } else if (points_per_resample != "sample size" & points_per_resample <= 0) {
    stop("Invalid value for points_per_resample")
  } else if (seq[1] <= 0) {
    stop("Invalid input for seq")
  } else if (method == 'biased bootstrap' & (length(mu) != length(sigma) | length(mu) != length(cols_to_bias))) {
    stop("mu, sigma, and cols_to_bias must have same length")
  }
  dir.create('./Objects', showWarnings = FALSE)
  if (method == 'bootstrap') {
    return(bootstrap(name, hv, n, points_per_resample, verbose))
  } else if (method == 'bootstrap seq') {
    return(bootstrap_seq(name, hv, n, points_per_resample, seq, verbose))
  } else if (method == 'biased bootstrap') {
    return(sampling_bias_bootstrap(name, hv, n, points_per_resample, verbose, mu, sigma, cols_to_bias))
  }
}
