Combined Volume Approach
================

``` r
library(ggplot2)
library(hypervolume)
```

    ## Loading required package: Rcpp

    ## Loading required package: rgl

``` r
library(doParallel)
```

    ## Loading required package: foreach

    ## Loading required package: iterators

    ## Loading required package: parallel

## Combined data from hypervolumes drawn from same distribution approximate distribution of original population

Drawn from normal distribution

``` r
sample1 = rnorm(150)
sample2 = rnorm(150)
func = function(x) {dnorm(x)}
ggplot(data.frame(sample1, sample2, x = seq(-3, 3, length.out = 150))) + 
  geom_histogram(aes(x = sample1, y = ..density.., fill = "red", alpha = 0.5), bins = 20) + 
  geom_histogram(aes(x = sample2, y = ..density.., fill = "blue", alpha = 0.5), bins = 20) +
  stat_function(aes(x = x), fun = func)
```

    ## Warning: `mapping` is not used by stat_function()

![](Overlap-statistics_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
ggplot(data.frame(samples = c(sample1, sample2), x = seq(-3, 3, length.out = 300))) +
  geom_histogram(aes(x = samples, y = ..density.., alpha = 0.5), bins = 30) +
  stat_function(fun = func)
```

![](Overlap-statistics_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->
Drawn from sample data

``` r
data("quercus")
qsample1 = quercus[sample(1:nrow(quercus), 150),]
qsample2 = quercus[sample(1:nrow(quercus), 150),]
combined_sample = rbind(qsample1, qsample2)
ggplot(data = combined_sample) + geom_histogram(aes(x = Latitude, y = ..density..), fill = "red", alpha = 0.5) + geom_histogram(data = quercus, aes(x = Latitude, y = ..density..), fill = "blue", alpha = 0.5) + ggtitle("Red: combined samples \nBlue: original data")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](Overlap-statistics_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
hv1 = hypervolume(qsample1[,2:3])
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
    ## Requested probability quantile 0.950000, obtained 0.949079 - setting threshold value 0.000004.
    ##  For a closer match, you can increase num.thresholds in hypervolume_threshold.

``` r
hv2 = hypervolume(qsample2[,2:3])
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
    ## Requested probability quantile 0.950000, obtained 0.944834 - setting threshold value 0.000004.
    ##  For a closer match, you can increase num.thresholds in hypervolume_threshold.

``` r
population_hat = hypervolume(combined_sample[,2:3])
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
    ## Requested probability quantile 0.950000, obtained 0.948349 - setting threshold value 0.000002.
    ##  For a closer match, you can increase num.thresholds in hypervolume_threshold.

``` r
#null_path = resample("quercus_150_null", population_hat, "bootstrap", n = 500, points_per_resample = 150, cores = 20)
null_path = "C:/Users/14842/Desktop/Hypervolumes-Resampling/Objects/quercus_150_null"

#Observed statistics
observed = hypervolume_overlap_statistics(hypervolume_set(hv1, hv2, check.memory = FALSE))
```

    ## Choosing num.points.max=25955 (use a larger value for more accuracy.)
    ## Using minimum density of 57.608363
    ## Retaining 8642 points in hv1 and 8195 points in hv2.
    ## Beginning ball queries... 
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
    ## Finished ball queries.

``` r
# Generate null distribution of statitics
null = foreach(i = 1:250, .combine = rbind) %:%
  foreach(j = 251:500, .combine = rbind) %dopar% {
    h1 = readRDS(file.path(null_path, list.files(null_path)[i]))
    h2 = readRDS(file.path(null_path, list.files(null_path)[j]))
    hypervolume_overlap_statistics(hypervolume_set(h1, h2, check.memory = FALSE))
  }
```

``` r
ggplot(data.frame(null), aes(x = jaccard, y = ..density..)) + 
  geom_histogram(bins = 100) + 
  geom_point(aes(x = observed["jaccard"], y = 0), color = "red") + 
  ggtitle("Distribution of jaccard index")
```

![](Overlap-statistics_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
ggplot(data.frame(null), aes(x = sorensen, y = ..density..)) + 
  geom_histogram(bins = 100) + 
  geom_point(aes(x = observed["sorensen"], y = 0), color = "red") + 
  ggtitle("Distribution of sorensen index")
```

![](Overlap-statistics_files/figure-gfm/unnamed-chunk-9-2.png)<!-- -->

``` r
ggplot(data.frame(null), aes(x = frac_unique_1, y = ..density..)) + 
  geom_histogram(bins = 100) + 
  geom_point(aes(x = observed["frac_unique_1"], y = 0), color = "red") + 
  ggtitle("Distribution of fraction of Hypervolume 1 that is unique")
```

![](Overlap-statistics_files/figure-gfm/unnamed-chunk-9-3.png)<!-- -->

``` r
ggplot(data.frame(null), aes(x = frac_unique_2, y = ..density..)) + 
  geom_histogram(bins = 100) + 
  geom_point(aes(x = observed["frac_unique_2"], y = 0), color = "red") + 
  ggtitle("Distribution of fraction of Hypervolume 2 that is unique")
```

![](Overlap-statistics_files/figure-gfm/unnamed-chunk-9-4.png)<!-- -->
\#\# Get power of each statistic from example case  
setosa and versicolor clearly do not share the same distribution

``` r
data(iris)
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) + geom_point()
```

![](Overlap-statistics_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
hv_setosa = hypervolume(iris[iris$Species == "setosa", 1:2])
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
    ## 
    ## Building tree... 
    ## done.
    ## Ball query... 
    ## 
    ## done.
    ## Requested probability quantile 0.950000, obtained 0.948773 - setting threshold value 0.001392.
    ##  For a closer match, you can increase num.thresholds in hypervolume_threshold.

``` r
hv_versicolor = hypervolume(iris[iris$Species == "versicolor", 1:2])
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
    ## 
    ## Building tree... 
    ## done.
    ## Ball query... 
    ## 
    ## done.
    ## Requested probability quantile 0.950000, obtained 0.949398 - setting threshold value 0.001319.
    ##  For a closer match, you can increase num.thresholds in hypervolume_threshold.

``` r
hv_combine = hypervolume(iris[iris$Species == "versicolor" | iris$Species == "setosa", 1:2])
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
    ## Requested probability quantile 0.950000, obtained 0.949934 - setting threshold value 0.000360.
    ##  For a closer match, you can increase num.thresholds in hypervolume_threshold.

``` r
#power_test_path = resample("power test bootstrap", hv_combine, "bootstrap", n = 200, points_per_resample = 50)
power_test_path = "C:/Users/14842/Desktop/Hypervolumes-Resampling/Objects/power test bootstrap"

# Observed statistics
observed_iris_stats = hypervolume_overlap_statistics(hypervolume_set(hv_setosa, hv_versicolor, check.memory = FALSE))
```

    ## Choosing num.points.max=25955 (use a larger value for more accuracy.)
    ## Using minimum density of 4534.710243
    ## Retaining 10729 points in hv1 and 13603 points in hv2.
    ## Beginning ball queries... 
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
    ## Finished ball queries.

``` r
# Generate null distribution of statitics
null_iris = foreach(i = 1:100, .combine = rbind) %:%
  foreach(j = 101:200, .combine = rbind) %dopar% {
    h1 = readRDS(file.path(power_test_path, list.files(power_test_path)[i]))
    h2 = readRDS(file.path(power_test_path, list.files(power_test_path)[j]))
    hypervolume_overlap_statistics(hypervolume_set(h1, h2, check.memory = FALSE))
  }
```

``` r
ggplot(data.frame(null_iris), aes(x = jaccard, y = ..density..)) + 
  geom_histogram(bins = 100) + 
  geom_point(aes(x = observed_iris_stats["jaccard"], y = 0), color = "red") + 
  ggtitle("Distribution of jaccard index")
```

![](Overlap-statistics_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

``` r
ggplot(data.frame(null_iris), aes(x = sorensen, y = ..density..)) + 
  geom_histogram(bins = 100) + 
  geom_point(aes(x = observed_iris_stats["sorensen"], y = 0), color = "red") + 
  ggtitle("Distribution of sorensen index")
```

![](Overlap-statistics_files/figure-gfm/unnamed-chunk-14-2.png)<!-- -->

``` r
ggplot(data.frame(null_iris), aes(x = frac_unique_1, y = ..density..)) + 
  geom_histogram(bins = 100) + 
  geom_point(aes(x = observed_iris_stats["frac_unique_1"], y = 0), color = "red") + 
  ggtitle("Distribution of fraction of Hypervolume 1 that is unique")
```

![](Overlap-statistics_files/figure-gfm/unnamed-chunk-14-3.png)<!-- -->

``` r
ggplot(data.frame(null_iris), aes(x = frac_unique_2, y = ..density..)) + 
  geom_histogram(bins = 100) + 
  geom_point(aes(x = observed_iris_stats["frac_unique_2"], y = 0), color = "red") + 
  ggtitle("Distribution of fraction of Hypervolume 2 that is unique")
```

![](Overlap-statistics_files/figure-gfm/unnamed-chunk-14-4.png)<!-- -->

``` r
stopCluster(cl)
stopImplicitCluster()
registerDoSEQ()
```
