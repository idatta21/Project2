Project 2
================
Ipsita Datta & Victoria Seng
10/15/2021

## Introduction

VICTORIA TODO: describe data set and vars we are working with (target =
shares) and mention purpose of analysis methods

I’ll add more here once we get a feel for which vars we want to work
with and for our models

## Data

Importing the data and grabbing a vector of the data channels for later
use.

``` r
newsPop <- read_csv("./Data/OnlineNewsPopularity.csv")

newsChannels <- newsPop %>%
  select(starts_with("data_channel_is_")) %>%
    names

names
```

    ## function (x)  .Primitive("names")

Subsetting the data by the type of data channels :

``` r
bus <- newsPop %>%
  filter(data_channel_is_bus == TRUE) %>%
    select(-starts_with("data_channel_is_"))
```

``` r
lifestyle<-newsPop %>%
  filter(data_channel_is_lifestyle == TRUE) %>%
    select(-starts_with("data_channel_is_"))
```

``` r
 entertainment<-newsPop %>%
  filter(data_channel_is_lifestyle == TRUE) %>%
    select(-starts_with("data_channel_is_"))
```

``` r
 socmed<-newsPop %>%
  filter(data_channel_is_lifestyle == TRUE) %>%
    select(-starts_with("data_channel_is_"))
```

``` r
 tech<-newsPop %>%
  filter(data_channel_is_lifestyle == TRUE) %>%
    select(-starts_with("data_channel_is_"))
```

``` r
 world<-newsPop %>%
  filter(data_channel_is_lifestyle == TRUE) %>%
    select(-starts_with("data_channel_is_"))
```

## Summarizations

TODO: three graphs apiece and “some” summary statistics each. Let’s try
both doing a contingency table and a couple of summary stats.

I’m going to work with weekday a bit so need to transpose to make it
easier to work with.

``` r
busTP <- bus %>%
  pivot_longer(starts_with("weekday_is"), names_to = "wd", values_to = "wdVal") %>%
    filter(wdVal == TRUE) %>%
      mutate(weekday = substr(wd, 12, nchar(wd))) %>%
        select(-wd, -wdVal) 
```

This contingency table shows the overall number of shares by weekday. A
higher number means more shares have happened on that day.

``` r
GDAtools::wtable(busTP$weekday, w = busTP$shares)
```

    ##    friday    monday  saturday    sunday  thursday   tuesday wednesday       Sum 
    ##   1966657   4482214   1075736   1215518   3560327   3466021   3401897  19168370

Here are some summary stats by weekday. Again, bigger means more shares.

``` r
busTP %>%
  group_by(weekday) %>%
    summarize(min = min(shares), mean = mean(shares), median = median(shares), iqr = IQR(shares), max = max(shares))
```

    ## # A tibble: 7 x 6
    ##   weekday     min  mean median   iqr    max
    ##   <chr>     <dbl> <dbl>  <dbl> <dbl>  <dbl>
    ## 1 friday       22 2364.   1400 1420. 102200
    ## 2 monday        1 3887.   1400 1651  690400
    ## 3 saturday    150 4427.   2600 2150  144400
    ## 4 sunday      692 3544.   2200 2400   56900
    ## 5 thursday     81 2885.   1300 1297. 306100
    ## 6 tuesday      44 2932.   1300 1387  310800
    ## 7 wednesday    63 2677.   1300 1312  158900

## Modeling

IPSITA TODO: Brief explanation of the idea of a linera regression model

IPSITA TODO: Fit linear regression and random forest model

VICTORIA TODO: Fit linear regression and boosted tree model

## Comparison

## Automation

Vic note: I’m ok with doing either automation or the comparison, so let
me know which one you’d be more comfortable tackling.
