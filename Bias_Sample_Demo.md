Empirical CDF
================
Daniel Chen
4/22/2020

``` r
library(hypervolume)
```

    ## Loading required package: Rcpp

    ## Loading required package: rgl

``` r
library(plot3D)
library(foreach)
library(ggplot2)
```

``` r
data(iris)
hv = hypervolume(iris[,c(1, 2)])
```

## Empirical Cumulative Distribution Function of Iris Data

``` r
ecdfs = foreach(i = 1:nrow(iris), .combine = c) %do% {
  joint_ecdf(iris[i,c(1, 2)], hv)
}
iris['ecdf'] = ecdfs
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, col = ecdf)) + geom_point()
```

<img src="IrisECDF.png" width="2187" />

## Empirical CDF of Bootstrapped Resample

``` r
iris2 = iris[sample(1:nrow(iris), nrow(iris), replace = TRUE),]
hv2 = hypervolume(iris2[,c(1,2)])
ecdfs2 = foreach(i = 1:nrow(iris2), .combine = c) %do% {
  joint_ecdf(iris2[i,c(1, 2)], hv2)
}
iris2['ecdf'] = ecdfs2
ggplot(iris2, aes(x = Sepal.Length, y = Sepal.Width, col = ecdf)) + geom_point()
```

<img src="BootstrapECDF.png" width="2187" />

## Resample with bias towards the median

``` r
iris4 = sampling_bias_hv(hv, 150)
hv4 = hypervolume(iris4)
ecdfs4 = foreach(i = 1:nrow(iris4), .combine = c) %do% {
  joint_ecdf(iris4[i,c(1, 2)], hv4)
}
iris4 = data.frame(iris4)
colnames(iris4) = c('Sepal.Length', 'Sepal.Width')
iris4['ecdf'] = ecdfs4
ggplot(data.frame(iris4), aes(x = Sepal.Length, y = Sepal.Width, col = ecdf)) + geom_point()
```

<img src="Median_biasECDF.png" width="2187" />
