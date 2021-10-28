Project 2
================
Ipsita Datta & Victoria Seng
10/15/2021

# Predicting News Article Popularity

## Introduction

This data set contains a selection of news articles in six different
channels, along with attributes about those articles. Our goal is to fit
and test the best model using the variables provided to predict the
response, number of shares, for future articles.

Predictors include day of the week on which the article was published,
keywords in the title and body, videos and images in the article,
polarity (positive and negative) of the title and content, and more.

We will do some exploratory analysis of the variables and fit four
models: two linear regression models, and two ensemble tree-based
models, a random forest and a boosted tree.

### Links to Channel Reports

-   [Business](data_channel_is_bus.md)
-   [Entertainment](data_channel_is_entertainment.md)
-   [Lifestyle](data_channel_is_lifestyle.md)
-   [Social Media](data_channel_is_socmed.md)
-   [Technology](data_channel_is_tech.md)
-   [World News](data_channel_is_world.md)

### Libraries

## Data

First, we import the news popularity data csv. We’ll also pull the
channel names for later use.

``` r
newsPop <- read_csv("./Data/OnlineNewsPopularity.csv")

channel <- newsPop %>%
  select(starts_with("data_channel_is_")) %>%
    names
```

Next, we subset the channel for use in the initial EDA and model
fitting.

``` r
newsTP <- newsPop %>%
  pivot_longer(starts_with("data_channel_is_"), names_to = "chname", values_to = "v") %>%
    filter(v == TRUE) %>%
      mutate(channel = chname) %>%
        select(-chname, -v) 
```

``` r
subNews <- newsTP %>%
  filter(channel == params$channel) %>%
    select(-starts_with("data_channel_is_"), -url, -timedelta, -channel)
```

## Summaries

For the first few summaries, we will use this transposed data that turns
the binary weekday variables into one categorical variable.

``` r
TP <- subNews %>%
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
    ##   2916920   3980347   1298232   2042670   3548004   3479822   3696732  20962727

Here is the contingency table for best keywords shares by weekdays

``` r
GDAtools::wtable(TP$weekday, w = TP$kw_max_max)
```

    ##     friday     monday   saturday     sunday   thursday    tuesday  wednesday        Sum 
    ##  753761100 1039300900  299076600  407939900  938550800  990129000  983827300 5412585600

Here is contingency table for the number of maximum shares of an article
that was linked in the article by weekday

``` r
GDAtools::wtable(TP$weekday, w = TP$self_reference_max_shares)
```

    ##    friday    monday  saturday    sunday  thursday   tuesday wednesday       Sum 
    ##   7910767  11480790   4002819   6813138  10409826  10822353  11574462  63014155

Here are some summary stats by weekday. Again, bigger means more shares.

``` r
TP %>%
  group_by(weekday) %>%
    summarize(min = min(shares), mean = mean(shares), median = median(shares), iqr = IQR(shares), max = max(shares))
```

    ## # A tibble: 7 x 6
    ##   weekday     min  mean median   iqr    max
    ##   <chr>     <dbl> <dbl>  <dbl> <dbl>  <dbl>
    ## 1 friday       58 3001.   1200 1150  210300
    ## 2 monday       59 2931.   1100 1102  112600
    ## 3 saturday     65 3416.   1600 1400   68300
    ## 4 sunday      171 3811.   1700 2600   69500
    ## 5 thursday     57 2882.   1100 1196  197600
    ## 6 tuesday      47 2708.   1100 1115   98000
    ## 7 wednesday    49 2855.   1100 1132. 138700

A higher number means more best keywords tend to be shared more often by
weekday

``` r
TP %>%
  group_by(weekday) %>%
    summarize(min = min(kw_max_max), mean = mean(kw_max_max), median = median(kw_max_max), stddev=sd(kw_max_max),iqr = IQR(kw_max_max), max = max(kw_max_max))
```

    ## # A tibble: 7 x 7
    ##   weekday     min    mean median  stddev   iqr    max
    ##   <chr>     <dbl>   <dbl>  <dbl>   <dbl> <dbl>  <dbl>
    ## 1 friday    28000 775474. 843300 182406.     0 843300
    ## 2 monday        0 765317. 843300 201172.     0 843300
    ## 3 saturday  51900 787044. 843300 172876.     0 843300
    ## 4 sunday    37400 761082. 843300 205392.     0 843300
    ## 5 thursday  28000 762430. 843300 207386.     0 843300
    ## 6 tuesday   17100 770528. 843300 187469.     0 843300
    ## 7 wednesday 17100 759712. 843300 205788.     0 843300

Transpose newsPop data to create pie chart across all the data channels

``` r
newsPop_TP <- newsPop %>%
  pivot_longer(starts_with("data_channel_is_"), names_to = "dc", values_to = "dcVal") %>%
    filter(dcVal == TRUE) %>%
      mutate(datachannel = substr(dc, 17, nchar(dc))) %>%
        select(datachannel,shares,-dc, -dcVal) 
```

Here is the pie chart showing overall which datachannel is doing best

``` r
df<-newsPop_TP %>% group_by(datachannel) %>% summarise_each(funs(sum))
pct <- round(df$shares/sum(df$shares)*100)
lbls <- paste(df$datachannel, pct) # add percents to labels
lbs<-paste(lbls,"%",sep="")
pie3D(df$shares,labels=lbs,explode=0.1,
   main="Pie Chart of Shares across datachannels ")
```

![](./images/piechart-1.png)<!-- -->

Here is the bar plot showing best keyword shares by weekdays. Higher
means for best keyword shares on that day

``` r
plot1<-ggplot(TP,aes(x=weekday,y=kw_max_max,fill=weekday))
plot1+ geom_col() + 
  scale_x_discrete("weekday") + 
  ggtitle("bestkeyword shares across weekdays")+
  scale_y_continuous(labels = scales::comma)
```

![](./images/bestkeyword_bar-1.png)<!-- -->

``` r
plot2<-ggplot(TP,aes(x=weekday,y=shares,fill=weekday))
plot2+ geom_col() + 
  scale_x_discrete("weekday") + 
  ggtitle(" shares across weekdays")+
  scale_y_continuous(labels = scales::comma)
```

![](./images/share_weekdays_bar-1.png)<!-- -->

Here is a boxplot with jitter of shares by weekend status. The lines on
the box plot show quartile values by weekend status.

``` r
g <- ggplot(data = subNews, aes(x = as_factor(is_weekend), y = shares, group = as_factor(is_weekend)))
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
g <- ggplot(data = subNews, aes(rate_positive_words, shares))
g + geom_point(aes(color = as_factor(is_weekend)), position = "jitter") + 
  labs(x = "Rate Positive Words", 
       y = "Number of Shares", 
       title = "Positive Word Rate vs Number of Shares by Weekday Status",
       color = "Weekend? (1 = Yes, 0 = No)") + 
    scale_y_continuous(labels = scales::comma)
```

![](./images/pos_v_share-1.png)<!-- -->

Here is the scatterplot showing number of images vs shares. From the
graph , we can say less number of images have more shares. Specifically
those articles has zero or one image.

``` r
g <- ggplot(data = subNews, aes(num_imgs, shares))
g + geom_point( position = "jitter") + 
  labs(x = "num_imgs", 
       y = "Number of Shares", 
       title = "num_imgs vs Number of Shares "
       ) + 
    scale_y_continuous(labels = scales::comma)
```

![](./images/images_v_shares-1.png)<!-- -->

Here is a scatter plot of shares vs negative word rate with a grouping
on weekday/weekend. The Y axis represents shares. More points higher on
the Y axis on the lefthand side of the graph indicate less negative
articles getting more shares. Less points higher on the Y axis on the
righthand side of the graph indicate more negative articles getting less
shares. The prevelence of one color over another in any part of the
graph is indicative of whether the article was published on a weekday or
a weekend.

``` r
g1 <- ggplot(data = subNews, aes(rate_negative_words, shares))
g1 + geom_point(aes(color = as_factor(is_weekend)), position = "jitter") + 
  labs(x = "negative words rate", 
       y = "Number of Shares", 
       title = "negative words rate vs Number of Shares by Weekend Status",
       color = "Weekend? (1 = Yes, 0 = No)") + 
    scale_y_continuous(labels = scales::comma) 
```

![](./images/negative_words_V_shares_scatterplot-1.png)<!-- -->

Here is the scatter plot showing points in the middle has more share as
a function of number of words in the title. On Left of the graph with
less word in the title has less share by weekdays

``` r
g2 <- ggplot(data = subNews, aes(n_tokens_title, shares))
g2 + geom_point(aes(color = as_factor(is_weekend)), position = "jitter") + 
  labs(x = "Number of words in the title", 
       y = "Number of Shares", 
       title = "Number of words in the title vs Number of Shares by Weekend Status",
       color = "Weekend? (1 = Yes, 0 = No)") + 
    scale_y_continuous(labels = scales::comma) 
```

![](./images/wordsIntitle-1.png)<!-- -->

This correlation plot visually shows the positive or negative
correlation coefficient between the various predictor varibles. The
larger the circle and more saturated the color, the stronger the
correlation. Blue is negative, and red is positive.

``` r
c <- cor(subNews %>% select(-shares))
cp <- corrplot(c, order = 'AOE', cl.pos = 'n', tl.cex = 0.5)
```

![](./images/corrplot-1.png)<!-- -->

## Model Fitting

A linear regression model describes the relationship between a dependent
variable and one or more independent variables, i.e y=beta0+beta1 \*
x1+beta2 \* x2 +….+betan \* xn where y is dependent response variable
and x1 to xn are independent variable (also called predictors)

Linear models are a way of describing a response variable in terms of a
linear combination of predictor variables.

### First Linear Model

splitting data in train(70%) and test (30%)

``` r
Index <- createDataPartition(y = subNews$shares , p= 0.7, list = FALSE)
Train <- subNews[Index,]
Test <- subNews[-Index,]
```

Based on the p-values and correlation coefficients, the predictors are
added to the model to predict shares.

Performance on Test data set for Linear Regression model

``` r
pred <- predict(lmfit, newdata =Test)
lm_Test<-round(postResample(pred, obs = Test$shares),4)
```

### Second Linear Model

This linear regression model chooses its predictors based on excluding
any variable that has a correlation coefficient of above 0.3 or below
-0.3 with any other variable. This should choose variables for each
channel.

``` r
cTib <- as_tibble(c) %>%
  mutate(v1 = names(as_tibble(c))) %>%
    pivot_longer(cols = !v1, names_to = "v2", values_to = "corr") %>%
      filter(abs(corr) > 0.3 & v1 != v2)

exclude <- distinct(cTib, v1) %>% 
  unlist() %>% 
    unname()

vChan <- subNews %>%
  select(-all_of(exclude))

vIndex <- createDataPartition(y =vChan$shares , p= 0.7, list = FALSE)
vTrain <- vChan[vIndex,]
vTest <- vChan[-vIndex,]
```

This is the model. It is pre-processed with centering and scaling and it
uses 10-fold CV. The variables involved may vary from channel to channel
as described above.

``` r
vMod <- train(
  shares ~ .,
  data = vTrain,
  method = "lm",
  preProcess = c("center","scale"),
  trControl = trainControl(method = "cv", number = 10)
)

vPred <- predict(vMod, newdata = vTest)
v_linreg<-round(postResample(vPred, obs = vTest$shares), 4)
round(postResample(vPred, obs = vTest$shares), 4)
```

    ##      RMSE  Rsquared       MAE 
    ## 6927.5297    0.0049 2910.4953

### Random Forest model

A random forest model is a supervised learning algorithm which extend
the idea of bagging method to solve regression or classification
problems. It creates multiple trees from bootstrap samples like as
bagging method and then use a random subset of predictors for each
bootstrap sample/tree fit. It doesn’t use all the predictor.

``` r
randomforestFit <- train(
               shares ~ .,
               data = Train,
               method = "rf",
               preProcess = c("center", "scale"),
               trControl = trainControl(method = "cv",number = 5),
               tuneGrid = data.frame(mtry = seq(1,10,1)))
```

Performance on Test data set for Randomforest model

``` r
pred <- predict(randomforestFit, newdata =Test)
rf_test<-round(postResample(pred, obs = Test$shares),4)
```

### Boosted tree model

The boosted tree model slowly builds

``` r
vBoost <- train(
  shares ~ ., 
  data = Train,
  method = "gbm",
  preProcess = c("center", "scale"),
  trControl = trainControl(method = "repeatedcv", number = 5, repeats = 3)
)
```

Trying out the boosted tree model on the test data.

``` r
pr <- predict(vBoost, newdata = Test)
boostTest <-round(postResample(pr, obs = Test$shares), 4)
```

## Comparison

We have created 4 models - 2 linear regression model,1 Random Forest
Model and 1 Boosted tree model. Here in this given data set, we are
trying to predict the number of shares of a news article . We are
comparing four model with respect to their RMSE. Model with lowest RMSE
works best.

``` r
tab<-matrix(round(c(lm_Test[1],v_linreg[1],rf_test[1],boostTest[1]),1))
 
colnames(tab) <- c('RMSE')
rownames(tab) <- c('LM1','LM2','RandomForest','Boosted')
print(tab)
```

    ##                RMSE
    ## LM1          8808.4
    ## LM2          6927.5
    ## RandomForest 8879.9
    ## Boosted      8813.8

## Automation

The render code is run from readme\_render\_p2.R

Here is the un-evaluated render code:

``` r
## The render code to output the .md files

library(tidyverse)

newsPop <- read_csv("./Data/OnlineNewsPopularity.csv")

channel <- newsPop %>%
  select(starts_with("data_channel_is_")) %>%
  names

out <- paste0(channel, ".md")
params <- lapply(channel, FUN = function(x){list(channel = x)})

docs <- tibble(out, params)

apply(
  docs, 
  MARGIN = 1, 
  FUN = function(x){
    rmarkdown::render(input = "./README.Rmd", 
                      output_file = x[[1]], 
                      output_format = "github_document",
                      output_dir = "./", 
                      output_options = list(
                        toc = FALSE, 
                        html_preview = FALSE, 
                        keep_html = FALSE
                      ),
                      params = x[[2]]
    )
  }
)
```
