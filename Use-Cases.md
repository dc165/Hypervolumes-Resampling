Use Cases
================
Daniel Chen
4/15/2020

``` r
data(iris)
source('resample.R')
```

    ## Loading required package: Rcpp

    ## Loading required package: rgl

``` r
source('cross_validate.R')
source('Funnels.R')
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
hv = hypervolume(iris[,c(1, 2)])
```

    ## 
    ## Building tree... 
    ## done.
    ## Ball query... 
    ## 
    ## done.
    ## 
    ## Building tree... 
    ## done.
    ## Ball query... 
    ## 
    ## done.
    ## 
    ## Building tree... 
    ## done.
    ## Ball query... 
    ## 
    ## done.
    ## 
    ## Building tree... 
    ## done.
    ## Ball query... 
    ## 
    ## done.
    ## Requested probability quantile 0.950000, obtained 0.949020 - setting threshold value 0.000180.
    ##  For a closer match, you can increase num.thresholds in hypervolume_threshold.

## Generate Hypervolume and Resample

Using the hypervolume that models the first two columns of the iris data
set, we generate a sequence of
bootstraps.

``` r
# Samples i rows from the data that generated hv for each i in seq, then generates a bootstrap on each of these i resamples and saves to file.
# Runtime ~2hrs
# hv_bootstrap_seq <- resample('hv_bootstrap_seq', hv, method = 'bootstrap_seq', points_per_resample = NULL, seq = seq(10, 150, length.out = 29))
# Here is the result of running the above code
hv_bootstrap_seq <- "./Objects/hv_bootstrap_seq"
```

By creating funnel plots using the bootstrap sequence, we can see that
our mean resampled data and mean random points both converge to the mean
of our original sample.

``` r
par(mfrow=c(1,2))
funnel(hv_bootstrap_seq, title = 'From resampled data', func = function(x) {mean(x@Data[,1])}) + 
  geom_line(aes(y = mean(hv@Data[,1]))) + 
  ylim(5.25, 6.5) +
  ylab('Mean Sepal Length')
funnel(hv_bootstrap_seq, title = 'From random points', func = function(x) {mean(x@RandomPoints[,1])}) + 
  geom_line(aes(y = mean(hv@Data[,1]))) + 
  ylim(5.25, 6.5) +
  ylab(NULL)
```

<img src="Use-Cases_files/figure-gfm/unnamed-chunk-3-1.png" width="50%" /><img src="Use-Cases_files/figure-gfm/unnamed-chunk-3-2.png" width="50%" />
