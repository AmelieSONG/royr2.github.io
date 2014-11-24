---
title: "lm() vs lmRob()"
author: "Riddhiman Roy"
date: "September 15th 2014"
output:
  html_document:
    keep_md: yes
    self_contained: yes
layout: post
---

Let's explore some robust regression. After having identified good quality predictors for your our purposes, it makes more sense for us to use an estimation technique that is not easily affected by discrepancies in the data and is reliable even if the data consists of outliers. 

### Data and Libraries  

Let's get some data to work with. I am going to use the [quantmod](http://cran.r-project.org/web/packages/quantmod/index.html) package . Let's just use a simple example.

{% highlight r %}
#install.packages('quantmod')
#install.packages('dynlm')
#install.packages('zoo')
options(warn = -1)
library(quantmod)
library(zoo)

gdp = getSymbols('GDP', src = 'FRED', auto.assign = FALSE)
ind = getSymbols('INDPRO', src = 'FRED', auto.assign = FALSE)

# Convert to timeseries data
gdp = zoo(x = as.numeric(gdp), order.by = as.yearmon(index(gdp)))
ind = zoo(x = as.numeric(ind), order.by = as.yearmon(index(ind)))

# Convert to growth percentages
gdp = log(gdp/lag(gdp,-1))
ind = log(ind/lag(ind,-1))
{% endhighlight %}

I'll be using the `dynlm()` function instead of `lm()` since it can handle timeseries data directly.


{% highlight r %}
#install.packages('dynlm')
library(dynlm)

fit = dynlm(gdp ~ ind)
{% endhighlight %}

Where there is `lm()` there is `summary()`


{% highlight r %}
summary(fit)
{% endhighlight %}



{% highlight text %}
## 
## Time series regression with "zoo" data:
## Start = Apr 1947, End = Jul 2014
## 
## Call:
## dynlm(formula = gdp ~ ind)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.039960 -0.005261 -0.000773  0.004702  0.034021 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 0.0146347  0.0005864   24.96   <2e-16 ***
## ind         0.6362679  0.0622009   10.23   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.009436 on 268 degrees of freedom
## Multiple R-squared:  0.2808,	Adjusted R-squared:  0.2781 
## F-statistic: 104.6 on 1 and 268 DF,  p-value: < 2.2e-16
{% endhighlight %}

### Bootstrapping - OLS
Let's bootstrap the OLS estimate and see what the bootstrapped standard error is. 


{% highlight r %}
#Note how dynlm() handles data internally using data frames
mat = data.frame(gdp = as.numeric(fit$model[,1]),
                 ind = as.numeric(fit$model[,2]))
class(mat)
{% endhighlight %}



{% highlight text %}
## [1] "data.frame"
{% endhighlight %}

I'll use the `boot()` function to do this. Let's also create a function which can fit a lm model and return the coefficient on the **Industrial Production Index**. 


{% highlight r %}
#install.packeges('boot')
library(boot)

lmFUNC = function(x,indices){
  fit = lm(gdp~ind, data = x[indices,])
  return(coef(fit)[2])
}

lm.boot = boot(data = mat, R = 1000, statistic = lmFUNC)
summary(lm.boot)
{% endhighlight %}



{% highlight text %}
##      R original  bootBias   bootSE bootMed
## 1 1000  0.63627 0.0029981 0.074579  0.6366
{% endhighlight %}

The coefficient on the industrial production index has a rather high standard error but a much smaller bias value. The bias and standard error can be very easily calculated as shown below. Also, we can plot a histogram and a q-q plot of the bootstrapped estimates using the `plot()` function. Note that **t** here refers to the value of the coefficient on **Industrial Production Index**.


{% highlight r %}
# Bias
(bias = mean(lm.boot$t) - lm.boot$t0)
{% endhighlight %}



{% highlight text %}
##         ind 
## 0.002998114
{% endhighlight %}



{% highlight r %}
# Standard Error
(se = sd(lm.boot$t))
{% endhighlight %}



{% highlight text %}
## [1] 0.0745794
{% endhighlight %}



{% highlight r %}
# Plot
plot(lm.boot)
{% endhighlight %}

<img src="/assets/figures/lm vs lmRob/unnamed-chunk-6-1.png" title="center" alt="center" style="display: block; margin: auto;" />

### Outlier

It is well known that OLS estimates are sensitive to small changes in data and or outliers. We can show this emperically. Let's artificially introduce some outliers into our data. Let's first identify the data point with the highest cook's distance. Looks like it's the **14<sup>th</sup>** datapoint in our GDP dataset. Let's try to distort this specific datapoint. 


{% highlight r %}
par(mfrow = c(1,2))
plot(fit, which = 5)
plot(fit, which = 4)
{% endhighlight %}

<img src="/assets/figures/lm vs lmRob/unnamed-chunk-7-1.png" title="center" alt="center" style="display: block; margin: auto;" />

{% highlight r %}
par(mfrow = c(1,1))

gdp.mod = gdp

# Change the 14th datapoint
gdp.mod[14] = gdp[14] + 1.1*gdp[14]
matplot(cbind(gdp,gdp.mod),type = 'l', xlab ="", main = "Original GDP vs Modified GDP")
{% endhighlight %}

<img src="/assets/figures/lm vs lmRob/unnamed-chunk-7-2.png" title="center" alt="center" style="display: block; margin: auto;" />

{% highlight r %}
 # Refit
fit.mod = dynlm(gdp.mod ~ ind)
coef(fit.mod)
{% endhighlight %}



{% highlight text %}
## (Intercept)         ind 
##  0.01471512  0.72117785
{% endhighlight %}



{% highlight r %}
coef(fit)
{% endhighlight %}



{% highlight text %}
## (Intercept)         ind 
##  0.01463469  0.63626785
{% endhighlight %}

The coefficient on the **Industrial Production Index** has changed by **13.3450079** %. We can bootstrap the estimate again to check how the standard error of the bootstrapped estimate changes. 


{% highlight r %}
mat = data.frame(gdp = as.numeric(fit.mod$model[,1]),
                 ind = as.numeric(fit.mod$model[,2]))
lm.boot.mod = boot(data = mat, R = 1000, statistic = lmFUNC)
rbind(summary(lm.boot),
      summary(lm.boot.mod))
{% endhighlight %}



{% highlight text %}
##      R original   bootBias   bootSE bootMed
## 1 1000  0.63627  0.0029981 0.074579 0.63660
## 2 1000  0.72118 -0.0024191 0.129563 0.70099
{% endhighlight %}



{% highlight r %}
# Plot
plot(lm.boot.mod)
{% endhighlight %}

![center](/assets/figures/lm vs lmRob/unnamed-chunk-8-1.png) 

Notice that the plots seem to suggest that the bootstrapped estimates are skewed (unlike in the first case). Let's go trough the same exercise using `lmRob()`

### lmRob()
Let's first fit the data and see what the coefficients look like and then we can bootstrap the estimates.


{% highlight r %}
#install.packages('robust')
#install.packages('moments')
library(robust)
library(moments)
mat = na.omit(merge(gdp, ind))

#Convert to data frame
mat = data.frame(gdp = as.numeric(mat[,1]),
                 ind = as.numeric(mat[,2]))

fit = lmRob(formula = gdp ~ ind, data = mat)

summary(fit)
{% endhighlight %}



{% highlight text %}
## 
## Call:
## lmRob(formula = gdp ~ ind, data = mat)
## 
## Residuals:
##        Min         1Q     Median         3Q        Max 
## -0.0391192 -0.0049595 -0.0007241  0.0049683  0.0333129 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 0.0147333  0.0005438  27.094   <2e-16 ***
## ind         0.5251668  0.0591619   8.877   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.007539 on 268 degrees of freedom
## Multiple R-Squared: 0.1866 
## 
## Test for Bias:
##             statistic   p-value
## M-estimate      10.10 6.404e-03
## LS-estimate     33.96 4.233e-08
{% endhighlight %}

Note the lower R-Squared value.


{% highlight r %}
lmRobFUNC = function(x,indices){
  fit = lmRob(gdp~ind, data = x[indices,])
  return(coef(fit)[2])
} 

lm.boot.rob = boot(data = mat, R = 1000, statistic = lmRobFUNC)
summary(lm.boot.rob)
{% endhighlight %}



{% highlight text %}
##      R original  bootBias   bootSE bootMed
## 1 1000  0.52517 0.0012635 0.069818 0.52466
{% endhighlight %}



{% highlight r %}
plot(lm.boot.rob)
{% endhighlight %}

![center](/assets/figures/lm vs lmRob/unnamed-chunk-10-1.png) 

Now let's introduce the outlier as we did earlier and see what happens. 


{% highlight r %}
mat = na.omit(merge(gdp.mod, ind))

#Convert to data frame
mat = data.frame(gdp.mod = as.numeric(mat[,1]),
                 ind = as.numeric(mat[,2]))

fit = lmRob(formula = gdp ~ ind, data = mat)
summary(fit)
{% endhighlight %}



{% highlight text %}
## 
## Call:
## lmRob(formula = gdp ~ ind, data = mat)
## 
## Residuals:
##        Min         1Q     Median         3Q        Max 
## -0.0391192 -0.0049595 -0.0007241  0.0049683  0.0333129 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 0.0147333  0.0005438  27.094   <2e-16 ***
## ind         0.5251668  0.0591619   8.877   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.007539 on 268 degrees of freedom
## Multiple R-Squared: 0.1866 
## 
## Test for Bias:
##             statistic   p-value
## M-estimate      10.10 6.404e-03
## LS-estimate     33.96 4.233e-08
{% endhighlight %}



{% highlight r %}
lm.boot.rob.mod = boot(data = mat, R = 1000, statistic = lmRobFUNC)
summary(lm.boot.rob.mod)
{% endhighlight %}



{% highlight text %}
##      R original bootBias   bootSE     bootMed
## 1 1000  0.52517 -0.52466 0.063767 -8.6064e-05
{% endhighlight %}



{% highlight r %}
plot(lm.boot.rob.mod)
{% endhighlight %}

<img src="/assets/figures/lm vs lmRob/unnamed-chunk-11-1.png" title="center" alt="center" style="display: block; margin: auto;" />

Note how the coefficient on Industrial Production Index is the same when compared to the case when there was no outlier. The bootstrapped estimates are fairly normally distributed. But note that the boot strapped estimates have a **higher bias** but a **lower standard error**. In the wake of outliers robust estimation might make more sense. This effect is much better seen in the following plot of the residuals from the two (OLS vs Robust) fits. 


{% highlight r %}
fit.compare = fit.models(list(Robust = "lmRob", 
                "LS" = "lm"), formula = gdp ~ ind, data = mat)
par(mfrow = c(2,2))
plot(fit.compare)
{% endhighlight %}

<img src="/assets/figures/lm vs lmRob/unnamed-chunk-12-1.png" title="center" alt="center" style="display: block; margin: auto;" /><img src="/assets/figures/lm vs lmRob/unnamed-chunk-12-2.png" title="center" alt="center" style="display: block; margin: auto;" /><img src="/assets/figures/lm vs lmRob/unnamed-chunk-12-3.png" title="center" alt="center" style="display: block; margin: auto;" /><img src="/assets/figures/lm vs lmRob/unnamed-chunk-12-4.png" title="center" alt="center" style="display: block; margin: auto;" />

{% highlight r %}
par(mfrow = c(1,1))
{% endhighlight %}

Thoughts? Feel free to comment below !  Thanks !





