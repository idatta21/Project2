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
newsChannels
```

    ## [1] "data_channel_is_lifestyle"     "data_channel_is_entertainment"
    ## [3] "data_channel_is_bus"           "data_channel_is_socmed"       
    ## [5] "data_channel_is_tech"          "data_channel_is_world"

Subsetting the data by the type of data channels :

``` r
bus <- newsPop %>%
  filter(data_channel_is_bus == TRUE) %>%
    select(-starts_with("data_channel_is_"))
```

## Summarizations

TODO: three graphs apiece and “some” summary statistics each. Let’s try
both doing a contingency table and a couple of summary stats.

I’m going to work with weekday a bit so need to transpose to make it
easier to work with.

``` r
TP <- bus %>%
  pivot_longer(starts_with("weekday_is"), names_to = "wd", values_to = "wdVal") %>%
    filter(wdVal == TRUE) %>%
      mutate(weekday = substr(wd, 12, nchar(wd))) %>%
        select(-wd, -wdVal) 
```

This contingency table shows the overall number of shares by weekday. A
higher number means more shares have happened on that day.

``` r
GDAtools::wtable(TP$weekday, w = TP$shares)
```

    ##    friday    monday  saturday    sunday  thursday   tuesday wednesday       Sum 
    ##   1966657   4482214   1075736   1215518   3560327   3466021   3401897  19168370

``` r
GDAtools::wtable(TP$weekday, w = TP$kw_max_max)
```

    ##     friday     monday   saturday     sunday   thursday    tuesday  wednesday 
    ##  621229600  860841600  181677500  258820700  907211600  879178400  939551600 
    ##        Sum 
    ## 4648511000

Here are some summary stats by weekday. Again, bigger means more shares.

``` r
TP %>%
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

A higher number or bar height means more shares have happened on that
day.

``` r
TP %>%
  group_by(weekday) %>%
    summarize(min = min(kw_max_max), mean = mean(kw_max_max), median = median(kw_max_max), stddev=sd(kw_max_max),iqr = IQR(kw_max_max), max = max(kw_max_max))
```

    ## # A tibble: 7 x 7
    ##   weekday     min    mean median  stddev    iqr    max
    ##   <chr>     <dbl>   <dbl>  <dbl>   <dbl>  <dbl>  <dbl>
    ## 1 friday    28000 746670. 843300 220859.      0 843300
    ## 2 monday        0 746610. 843300 218158. 152900 843300
    ## 3 saturday  37400 747644. 843300 204422. 152900 843300
    ## 4 sunday    37400 754579. 843300 206016.      0 843300
    ## 5 thursday  11100 735180. 843300 231586. 152900 843300
    ## 6 tuesday   17100 743806. 843300 220836. 152900 843300
    ## 7 wednesday 17100 739222. 843300 223310. 152900 843300

``` r
plot1<-ggplot(bus,aes(x=TP$weekday,y=kw_max_max,fill=kw_max_max))
plot1+ geom_col() + 
  scale_x_discrete("weekday") + 
  scale_y_continuous(" Best keyword (max. shares) ") +
  ggtitle("bestkeyword shares across weekdays") 
```

![](./images/bestkeyword_bar-1.png)<!-- -->

Here is a boxplot of shares by weekend status. I hate how squashed it is
and need to find a way to programatically deal with outliers.

``` r
g <- ggplot(data = bus, aes(x = as_factor(is_weekend), y = shares, group = as_factor(is_weekend)))
g + geom_jitter(aes(color = as_factor(is_weekend))) +
  geom_boxplot() +
      labs(x = "Weekend Status (1 = Weekend, 0 = Weekday)", 
           y = "Number of Shares", 
           title = "Box Plot with Jitter of Shares by Weekend Status") +
        scale_y_continuous(labels = scales::comma, expand = expansion(mult = c(0,0)))
```

![](./images/weekend_box-1.png)<!-- -->

Here is a scatter plot of shares vs positive word rate with a grouping
on weekday/weekend. The Y axis represents shares. More points higher on
the Y axis on the lefthand side of the graph indicate less positive
articles getting more shares. More points higher on the Y axis on the
righthand side of the graph indicate more positive articles getting more
shares. The prevelence of one color over another in any part of the
graph is indicative of whether the article was published on a weekday or
a weekend.

``` r
g <- ggplot(data = bus, aes(rate_positive_words, shares))
g + geom_point(aes(color = as_factor(is_weekend)), position = "jitter") + 
  labs(x = "Rate Positive Words", 
       y = "Number of Shares", 
       title = "Positive Word Rate vs Number of Shares by Weekday Status",
       color = "Weekend? (1 = Yes, 0 = No)") + 
    scale_y_continuous(labels = scales::comma)
```

![](./images/pos_v_share-1.png)<!-- -->

``` r
g1 <- ggplot(data = bus, aes(rate_negative_words, shares))
g1 + geom_point(aes(color = as_factor(is_weekend)), position = "jitter") + 
  labs(x = "negative words rate", 
       y = "Number of Shares", 
       title = "negative words rate vs Number of Shares by Weekend Status",
       color = "Weekend? (1 = Yes, 0 = No)") + 
    scale_y_continuous(labels = scales::comma) 
```

![](./images/negative_words_V_shares_scatterplot-1.png)<!-- --> ##
spliting data in train(70%) and test (30%)

``` r
TPIndex <- createDataPartition(y =TP$shares , p= 0.7, list = FALSE)
TP_Train <- TP[TPIndex,]
TP_Test <- TP[-TPIndex,]
```

## Modeling

IPSITA TODO: Brief explanation of the idea of a linear regression model
#######I will describe this later A linear regression model describes
the relationship between a dependent variable, y, and one or more
independent variables, X. Linear models are a way of describing a
response variable in terms of a linear combination of predictor
variables. The response should be a continuous variable and be at least
approximately normally distributed. Such models find wide application,
but cannot handle clearly discrete or skewed continuous responses.
IPSITA TODO: Fit linear regression and random forest model

VICTORIA TODO: Fit linear regression and boosted tree model

## Comparison

## Automation

Vic note: I’m ok with doing either automation or the comparison, so let
me know which one you’d be more comfortable tackling.
