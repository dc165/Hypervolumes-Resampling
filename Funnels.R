library(hypervolume)
library(dplyr)
library(ggplot2)

funnel <- function(output_path, hv, func = mean, CI = .95, dat = 'TTFF', length.out = 50, from_model = FALSE, from_data = TRUE) {
  
  # Take confidence interval at 50 different sample sizes starting at 1 sample
  # Reconsider length.out
  sample_size = seq(1, length(hv@Data[,dat]), length.out = length.out)
  
  # Non-parametric bootstrap
  # Make size of resample and number of resamples changable?
  upper_quantile = c()
  lower_quantile = c()
  sample_mean = c()
  model_upper_q = c()
  model_lower_q = c()
  model_sample_mean = c()
  
  for(i in sample_size) {
    x = sample(hv@Data[,dat], i, replace = FALSE)
    y = sample(hv@RandomPoints[,dat], i, replace = FALSE)
    
    if(from_data) { 
      # Confidence interval and expected values generated from original data
      sample_mean = c(sample_mean, func(x))
      resample = replicate(1000, func(sample(x, replace = TRUE)))
      lower_quantile = c(lower_quantile, quantile(resample, (1-CI)/2)[[1]])
      upper_quantile = c(upper_quantile, quantile(resample, 1 - (1-CI)/2)[[1]])
    }
    
    if(from_model) {
      # Confidence interval and expected values generated from model
      model_sample_mean = c(model_sample_mean, func(y))
      resample = replicate(1000, func(sample(y, replace = TRUE)))
      model_lower_q = c(model_lower_q, quantile(resample, (1-CI)/2)[[1]])
      model_upper_q = c(model_upper_q, quantile(resample, 1 - (1-CI)/2)[[1]])
    }
  }
  
  # Add true values & create dataframe
  plotting_dat <- data.frame(sample_size)
  if(from_data) {
    sample_mean[length.out] = func(hv@Data[,dat])
    plotting_dat <- plotting_dat %>% mutate(sample_mean = sample_mean, lower_quantile = lower_quantile, upper_quantile = upper_quantile)
  }
  if(from_model) {
    model_sample_mean[length.out] = func(hv@RandomPoints[,dat])
    plotting_dat <- plotting_dat %>% mutate(model_sample_mean = model_sample_mean, model_lower_q = model_lower_q, model_upper_q = model_upper_q)
  }
  
  # Plot sampled points
  gg = ggplot(plotting_dat, aes(x = sample_size))
  if(from_data) {
    gg = gg + geom_line(aes(y = sample_mean), col = 'darkred') + 
      geom_line(aes(y = lower_quantile), col = 'red') + 
      geom_line(aes(y = upper_quantile), col = 'red') +
      geom_point(aes(y = lower_quantile), col = 'darkred') + 
      geom_point(aes(y = upper_quantile), col = 'darkred')
  }
  if(from_model) {
    gg = gg + geom_line(aes(y = model_sample_mean), col = 'darkblue') + 
      geom_line(aes(y = model_lower_q), col = 'blue') + 
      geom_line(aes(y = model_upper_q), col = 'blue') +
      geom_point(aes(y = model_lower_q), col = 'darkblue') + 
      geom_point(aes(y = model_upper_q), col = 'darkblue')
  }

  ggsave('test.png')
}

funnel <- function(output_path, hv, parameter = 'Volume', CI = .95, dat = 'TTFF', length.out = 5, from_model = FALSE, from_data = TRUE) {
  
  # Take confidence interval at 50 different sample sizes starting at 1 sample
  # Reconsider length.out
  sample_size = seq(10, length(hv@Data[,dat]), length.out = length.out)
  
  # Non-parametric bootstrap
  # Make size of resample and number of resamples changable?
  upper_quantile = c()
  lower_quantile = c()
  sample_mean = c()
  model_upper_q = c()
  model_lower_q = c()
  model_sample_mean = c()
  
  for(i in sample_size) {
    x = sample(hv@Data[,dat], i, replace = FALSE)
    y = sample(hv@RandomPoints[,dat], i, replace = FALSE)
    
    if(from_data) { 
      # Confidence interval and expected values generated from original data
      sample_mean = c(sample_mean, hypervolume(data.frame(x))@Volume)
      resample = replicate(5, hypervolume(data.frame(sample(x, replace = TRUE)))@Volume)
      lower_quantile = c(lower_quantile, quantile(resample, (1-CI)/2)[[1]])
      upper_quantile = c(upper_quantile, quantile(resample, 1 - (1-CI)/2)[[1]])
    }
    
    if(from_model) {
      # Confidence interval and expected values generated from model
      model_sample_mean = c(model_sample_mean, func(y))
      resample = replicate(1000, func(sample(y, replace = TRUE)))
      model_lower_q = c(model_lower_q, quantile(resample, (1-CI)/2)[[1]])
      model_upper_q = c(model_upper_q, quantile(resample, 1 - (1-CI)/2)[[1]])
    }
  }
  
  # Add true values & create dataframe
  plotting_dat <- data.frame(sample_size)
  if(from_data) {
    sample_mean[length.out] = hv@Volume
    plotting_dat <- plotting_dat %>% mutate(sample_mean = sample_mean, lower_quantile = lower_quantile, upper_quantile = upper_quantile)
  }
  if(from_model) {
    model_sample_mean[length.out] = func(hv@RandomPoints[,dat])
    plotting_dat <- plotting_dat %>% mutate(model_sample_mean = model_sample_mean, model_lower_q = model_lower_q, model_upper_q = model_upper_q)
  }
  
  # Plot sampled points
  gg = ggplot(plotting_dat, aes(x = sample_size))
  if(from_data) {
    gg = gg + geom_line(aes(y = sample_mean), col = 'darkred') + 
      geom_line(aes(y = lower_quantile), col = 'red') + 
      geom_line(aes(y = upper_quantile), col = 'red') +
      geom_point(aes(y = lower_quantile), col = 'darkred') + 
      geom_point(aes(y = upper_quantile), col = 'darkred')
  }
  if(from_model) {
    gg = gg + geom_line(aes(y = model_sample_mean), col = 'darkblue') + 
      geom_line(aes(y = model_lower_q), col = 'blue') + 
      geom_line(aes(y = model_upper_q), col = 'blue') +
      geom_point(aes(y = model_lower_q), col = 'darkblue') + 
      geom_point(aes(y = model_upper_q), col = 'darkblue')
  }
  
  ggsave('test.png')
}
