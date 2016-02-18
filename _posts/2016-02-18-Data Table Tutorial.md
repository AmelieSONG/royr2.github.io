---
title: "A data.table Tutorial"
layout: post
---

This post is more for my learning but hopefully helps someone get acquainted with `data.table()` as well.  

I'll use the `PimaIndiansDiabetes` dataset from the `mlbench` package. By the way I really like this package. It has a lot of databases from the [UC Irvine Machine Learning Database](http://archive.ics.uci.edu/ml/) and is an excellent source of data for doing some data analysis and implementing some machine learning algorithms. 

I'll structure this tutorial using assignment style Questions and Answers.




{% highlight r %}
library(data.table)
library(mlbench)
library(ggplot2)
library(magrittr)
{% endhighlight %}

### Basic structure
Here's how I understand the basic setup for using an object of type `data.table`.


{% highlight r %}
# mat is of type data.table
mat[Filter, Select, Group By]
{% endhighlight %}

- **Filter**: Select specific Rows
- **Select**: Select specific columns
- **Group By**: Return the result of filtering and selecting grouped by some categorical variable

Let's see some examples

### Creating a data table

``` r
data("PimaIndiansDiabetes")
ds <- data.table(PimaIndiansDiabetes)

# Since a data table supports all functions that be 
# used on a dataframe we can do this
str(ds)
```



{% highlight text %}
## Classes 'data.table' and 'data.frame':	768 obs. of  9 variables:
##  $ pregnant: num  6 1 8 1 0 5 3 10 2 8 ...
##  $ glucose : num  148 85 183 89 137 116 78 115 197 125 ...
##  $ pressure: num  72 66 64 66 40 74 50 0 70 96 ...
##  $ triceps : num  35 29 0 23 35 0 32 0 45 0 ...
##  $ insulin : num  0 0 0 94 168 0 88 0 543 0 ...
##  $ mass    : num  33.6 26.6 23.3 28.1 43.1 25.6 31 35.3 30.5 0 ...
##  $ pedigree: num  0.627 0.351 0.672 0.167 2.288 ...
##  $ age     : num  50 31 32 21 33 30 26 29 53 54 ...
##  $ diabetes: Factor w/ 2 levels "neg","pos": 2 1 2 1 2 1 2 1 2 2 ...
##  - attr(*, ".internal.selfref")=<externalptr>
{% endhighlight %}

### Filtering or Subsetting
One thing I always struggle with is subsetting data quickly and efficiently. `data.table` is awesome at that.

> Q: How many people above the age of 50 had diabetes?


{% highlight r %}
subset <- ds[age > 50 & diabetes == "pos", ]
nrow(subset)
{% endhighlight %}



{% highlight text %}
## [1] 38
{% endhighlight %}

> Q: How many people with a plasma glucose level range of [120 - 150] had diabetes?


{% highlight r %}
subset <- ds[glucose %in% 120:150 & diabetes == "pos"]
nrow(subset)
{% endhighlight %}



{% highlight text %}
## [1] 93
{% endhighlight %}

### Setting a Key
`data.table` can utilize **binary search** to filter rows if the `data.table` object is sorted. This is done by setting a **key**.


{% highlight r %}
# Check for a key
haskey(ds)
{% endhighlight %}



{% highlight text %}
## [1] FALSE
{% endhighlight %}



{% highlight r %}
# Set a key
setkey(ds, diabetes)

# Note that after setting a key, the entire dataset is 
# sorted using that key (in this case glucose)

head(ds)
{% endhighlight %}



{% highlight text %}
##    pregnant glucose pressure triceps insulin mass pedigree age diabetes
## 1:        1      85       66      29       0 26.6    0.351  31      neg
## 2:        1      89       66      23      94 28.1    0.167  21      neg
## 3:        5     116       74       0       0 25.6    0.201  30      neg
## 4:       10     115        0       0       0 35.3    0.134  29      neg
## 5:        4     110       92       0       0 37.6    0.191  30      neg
## 6:       10     139       80       0       0 27.1    1.441  57      neg
{% endhighlight %}

Setting a **key** will also allow for the `.()` operator to be used. 

### Using the dot operator to subset

{% highlight r %}
setkey(ds, age, diabetes)
subset <- ds[.(50, "pos"),]
head(subset)
{% endhighlight %}



{% highlight text %}
##    pregnant glucose pressure triceps insulin mass pedigree age diabetes
## 1:        6     148       72      35       0 33.6    0.627  50      pos
## 2:       11     138       74      26     144 36.1    0.557  50      pos
## 3:        9     112       82      24       0 28.2    1.282  50      pos
## 4:        6     147       80       0       0 29.5    0.178  50      pos
## 5:        6     162       62       0       0 24.3    0.178  50      pos
{% endhighlight %}



{% highlight r %}
nrow(subset)
{% endhighlight %}



{% highlight text %}
## [1] 5
{% endhighlight %}

### Selecting columns
We'll now move on to the second argument in the `data.table()` framework.

> Q: Whats the range of Plasma Glucose Concentration of people who had diabetees and were 21 years old?


{% highlight r %}
# Note that we have set a key
key(ds)
{% endhighlight %}



{% highlight text %}
## [1] "age"      "diabetes"
{% endhighlight %}



{% highlight r %}
# We can use the key to answer this question
# Note the use of the pipe operator
ds[.(21, "pos"), glucose] %>% range  # super clean and easy 
{% endhighlight %}



{% highlight text %}
## [1] 113 177
{% endhighlight %}

This kind of subsetting and column selection can come in handy when trying to make some exploratory charts. Say we want to make a simple scatterplot to compare `glucose` vs `pressure` for people in the age bracket of 25 to 30.


{% highlight r %}
ds[.(25:30), .(glucose, pressure, diabetes)] %>% 
  reshape2::melt() %>% 
  ggplot(aes(variable, value, fill = variable)) + 
  geom_boxplot() + 
  facet_grid(~ diabetes) + 
  ggtitle("Glucose vs Pressure")
{% endhighlight %}

![center](/assets/figures/Data Table Tutorial/unnamed-chunk-6-1.png)

Easy isn't it?

> Q: What's the average value oftriceps skin fold thickness for people in the age bracket of [20, 30] who have diabetes?


{% highlight r %}
# Note that this returns a locical vector of TRUE / FALSE values
# since we can only subset based on rows and not columns
ds[.(20:30, "pos"), triceps] %>% mean(na.rm = T)
{% endhighlight %}



{% highlight text %}
## [1] 25.22222
{% endhighlight %}

### Grouping
Grouping the result of filtering rows and selecting columns by specific variables and being able to do computations on is very useful.

> Q: What was the average body mass index of people in the age group of [21, 30], had diabetes grouped by the variable pregnant?


{% highlight r %}
# Note the use of the by argument
ds[.(21:30, "pos"), mean(mass), by = pregnant]
{% endhighlight %}



{% highlight text %}
##     pregnant       V1
##  1:        0 40.60000
##  2:        1 41.29091
##  3:        2 34.99286
##  4:        4 34.92500
##  5:        3 32.10000
##  6:        8 28.40000
##  7:        5 39.43333
##  8:        9 29.00000
##  9:        6 31.50000
## 10:        7 26.50000
## 11:       10  0.00000
{% endhighlight %}

We can use this to create some charts as well.


{% highlight r %}
ds$AgeInterval <- cut(ds$age, breaks = seq(21, 81, by = 10), include.lowest = T)

cols <- c("#00cc66", "#ff884d")

ds[,.(mean(glucose), mean(pressure)), by = .(AgeInterval, diabetes), nomatch = 0L] %>% 
  reshape2::melt() %>% 
  ggplot(aes(AgeInterval, value, fill = variable)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  facet_wrap(~ diabetes) + 
  scale_fill_manual(values = cols, labels = c("Glucose", "Pressure")) + 
  ggtitle("Avg. Glucose and Pressure grouped by age group")
{% endhighlight %}

![center](/assets/figures/Data Table Tutorial/unnamed-chunk-9-1.png)

For more details visit the [data.table vignette](https://rawgit.com/wiki/Rdatatable/data.table/vignettes/datatable-keys-fast-subset.html). 





