---
title: "Bootstrapping StepAIC()"
author: "Riddhiman Roy"
date: "March 15th 2014"
output:
  html_document:
    keep_md: yes
    self_contained: yes
layout: post
---

I work in the field of finance and find that people often rely on OLS regressions for doing predictive analysis. It's easy to implement and everyone knows about it. OLS regression gives us a very well developed mathematical framework which can be used to develop linear relationships. These relationships can then be used to create forward looking projections. But what about the features? How does one go about selecting the right feature set which can be used to reliably predict the variable under consideration?

Stepwise variable selection is one of the most commonly used variable selection algorithms. The reliability of stepwise and or subset selection techniques is debatable. If you are a statistician chances are you are not a big fan of such algorithms. None the less, such techniques are popular and I have come across a lot of people using them.

One of the ways one could make the stepwise selection process more reliable is by bootstrapping it.  The process can be broken down into the following steps:

1. Select a random subset from your data set
2. Run stepwise selection on this subset
3. Record the features that were selected
4. Repeat steps 1 to 3 many times

The end result is that now we can see how many times a specific feature (predictor variable) gets selected. Now, this bootstrapping technique just creates a random data set from the given data set (drawing with replacement). One could try to run a bootstrap procedure by creating a random starting point for the stepwise algorithm to work on.

Hopefully this little extra step would give you some insight as to which predictor variables could be most useful.

The bootStepAIC package is written by 'Dimitris Rizopoulos'.

## R Code

### Data loading and exploratory analysis

The following code snippets follow the post above. For this simple example I'll use the **Managers** data set in the **Performance Analytics** package. 

Lets first install the required package.

{% highlight r %}
#Install the package called "Performance Analytics"
install.packages("PerformanceAnalytics",repos="http://cran.us.r-project.org")

#Loads package into memory
library(PerformanceAnalytics)

#Load 'Managers' dataset
data(managers)
{% endhighlight %}

{% highlight r %}
#The data sets looks like this
head(managers)
{% endhighlight %}



{% highlight text %}
##               HAM1 HAM2    HAM3    HAM4 HAM5 HAM6 EDHEC LS EQ SP500 TR
## 1996-01-31  0.0074   NA  0.0349  0.0222   NA   NA          NA   0.0340
## 1996-02-29  0.0193   NA  0.0351  0.0195   NA   NA          NA   0.0093
## 1996-03-31  0.0155   NA  0.0258 -0.0098   NA   NA          NA   0.0096
## 1996-04-30 -0.0091   NA  0.0449  0.0236   NA   NA          NA   0.0147
## 1996-05-31  0.0076   NA  0.0353  0.0028   NA   NA          NA   0.0258
## 1996-06-30 -0.0039   NA -0.0303 -0.0019   NA   NA          NA   0.0038
##            US 10Y TR US 3m TR
## 1996-01-31   0.00380  0.00456
## 1996-02-29  -0.03532  0.00398
## 1996-03-31  -0.01057  0.00371
## 1996-04-30  -0.01739  0.00428
## 1996-05-31  -0.00543  0.00443
## 1996-06-30   0.01507  0.00412
{% endhighlight %}

Now that we have the data set loaded up, we can do some basic exploratory analysis. The **Managers** data set is made of the following variables:  

1. **Hedge Fund Returns**
 * HAM1 to HAM 6
2. **Risk Factors**
 * EDHEC Long Short Equity
 * SP500
 * US 10 Year Treasury yield
 * US 3month Treasuty yield

Lets try to plot the kind of relationship **HAM2** has with each of the **risk factors**. We'll use the **pairs** function.


{% highlight r %}
#Create a dataframe using HAM2 and the risk factors
df=data.frame(managers[,c(2,7:10)])

#The data frame should look like this
head(df)
{% endhighlight %}



{% highlight text %}
##            HAM2 EDHEC.LS.EQ SP500.TR US.10Y.TR US.3m.TR
## 1996-01-31   NA          NA   0.0340   0.00380  0.00456
## 1996-02-29   NA          NA   0.0093  -0.03532  0.00398
## 1996-03-31   NA          NA   0.0096  -0.01057  0.00371
## 1996-04-30   NA          NA   0.0147  -0.01739  0.00428
## 1996-05-31   NA          NA   0.0258  -0.00543  0.00443
## 1996-06-30   NA          NA   0.0038   0.01507  0.00412
{% endhighlight %}

{% highlight r %}
#Pairs plot
pairs(df,main="Pairs Plot for HAM2",pch=16,col="slateblue")
{% endhighlight %}

<img src="/assets/figures/BootStepAIC/managers-1.png" title="center" alt="center" style="display: block; margin: auto;" />

### Variable selection

Now for the fun part. As a simple example, lets try to use the avaliable risk factors to predict the returns for **manager 2 (HAM2)**

### LM Fit

As a first case, lets use all the available risk factors to predict future returns for **HAM2**. We'll be using the **lm** function.


{% highlight r %}
#Formula for lm function
form=formula(HAM2~EDHEC.LS.EQ + SP500.TR + US.10Y.TR + US.3m.TR)
fit=lm(form,data=df)

#The output from the summary function on a lm object should look like this
summary(fit)
{% endhighlight %}



{% highlight text %}
## 
## Call:
## lm(formula = form, data = df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.055374 -0.018682 -0.003635  0.013378  0.079518 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -0.005115   0.005328  -0.960   0.3390    
## EDHEC.LS.EQ  1.539069   0.166290   9.255 1.41e-15 ***
## SP500.TR    -0.193175   0.076937  -2.511   0.0134 *  
## US.10Y.TR    0.045813   0.116777   0.392   0.6956    
## US.3m.TR     1.429513   1.546454   0.924   0.3572    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.0253 on 115 degrees of freedom
##   (12 observations deleted due to missingness)
## Multiple R-squared:  0.5254,	Adjusted R-squared:  0.5089 
## F-statistic: 31.83 on 4 and 115 DF,  p-value: < 2.2e-16
{% endhighlight %}

We didn't do so bad. We got an R-Square of around *50%*. Our adjusted and multiple R-Square values are close to each other, we had enough degrees of freedom and most of our predictor variables are statistically significant. 


{% highlight r %}
#Lets check fitted vs actual values
plot(fit$model[,1],fit$fitted.values,  
     main="Actual vs Fitted values",  
     pch=16,col="indianred",cex=1.5,  
     xlab="Actual Values",ylab="Fitted Values")  
grid()
rstd=round(sd(fit$residuals),4)
legend("bottomleft",  
       legend=paste("Residual std err = ",rstd),bty='n')
{% endhighlight %}

<img src="/assets/figures/BootStepAIC/plot lm-1.png" title="center" alt="center" style="display: block; margin: auto;" />

### Stepwise Selection

Now lets see if we can improve on the model by removing any of the predictor variables. We'll use the **stepAIC** function in the **MASS** package.

{% highlight r %}
install.packages("MASS",repos="http://cran.us.r-project.org")
{% endhighlight %}

{% highlight r %}
#"::" can be used to make a reference to a specific library
#I am using trace = F to suppress the output from the stepAIC function
stepfit=MASS::stepAIC(object=fit,scope=list(upper=~.,lower=~1),direction="backward",trace=F)
summary(stepfit)
{% endhighlight %}



{% highlight text %}
## 
## Call:
## lm(formula = HAM2 ~ EDHEC.LS.EQ + SP500.TR, data = df)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.050498 -0.017211 -0.005262  0.012211  0.081154 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -0.0005315  0.0026008  -0.204  0.83843    
## EDHEC.LS.EQ  1.5548911  0.1645687   9.448 4.35e-16 ***
## SP500.TR    -0.2007421  0.0759434  -2.643  0.00933 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.02521 on 117 degrees of freedom
##   (12 observations deleted due to missingness)
## Multiple R-squared:  0.5208,	Adjusted R-squared:  0.5126 
## F-statistic: 63.59 on 2 and 117 DF,  p-value: < 2.2e-16
{% endhighlight %}

In this case the stepwise procedure removes both the treasury variables to arrive at a slightly more parsimonious model containing only 2 variables.  

### Comparison Table  

Parameter     |Full Model|Trimmed Model
--------------| ---------|-------------
 R-Squared    |   52.54% | 52.08%
 Adj R-Squared|   50.89% | 51.26

Notice that even though we have only two predictor variables, the model's predictive power hasn't changed a lot. 

What if we wanted to add a treasury variable to the mix? Which one would you pick? If we had some prior knowledge regarding HAM2 portfolio holdings, we might be able to make a qualitative judgement regarding whether to use the 10 year or the 3 month treasury yield. If not, we could use the bootstrap procedure to derive some useful information.


{% highlight r %}
#Install the package
install.packages("bootStepAIC",repos="http://cran.us.r-project.org")
{% endhighlight %}

{% highlight r %}
#Run bootstrap procedure
bootStepAIC::boot.stepAIC(object=fit,data=df)
{% endhighlight %}

{% highlight text %}
## 
## Summary of Bootstrapping the 'stepAIC()' procedure for
## 
## Call:
## lm(formula = form, data = df)
## 
## Bootstrap samples: 100 
## Direction: backward 
## Penalty: 2 * df
## 
## Covariates selected
##             (%)
## EDHEC.LS.EQ 100
## SP500.TR     80
## US.3m.TR     34
## US.10Y.TR    20
## 
## Coefficients Sign
##             + (%) - (%)
## EDHEC.LS.EQ   100     0
## US.3m.TR      100     0
## US.10Y.TR      90    10
## SP500.TR        0   100
## 
## Stat Significance
##                (%)
## EDHEC.LS.EQ 100.00
## SP500.TR     81.25
## US.10Y.TR    50.00
## US.3m.TR     44.12
## 
## 
## The stepAIC() for the original data-set gave
## 
## Call:
## lm(formula = HAM2 ~ EDHEC.LS.EQ + SP500.TR, data = df)
## 
## Coefficients:
## (Intercept)  EDHEC.LS.EQ     SP500.TR  
##  -0.0005315    1.5548911   -0.2007421  
## 
## 
## Stepwise Model Path 
## Analysis of Deviance Table
## 
## Initial Model:
## HAM2 ~ EDHEC.LS.EQ + SP500.TR + US.10Y.TR + US.3m.TR
## 
## Final Model:
## HAM2 ~ EDHEC.LS.EQ + SP500.TR
## 
## 
##          Step Df     Deviance Resid. Df Resid. Dev       AIC
## 1                                   115 0.07362914 -877.5447
## 2 - US.10Y.TR  1 9.853917e-05       116 0.07372768 -879.3843
## 3  - US.3m.TR  1 6.114188e-04       117 0.07433909 -880.3932
{% endhighlight %}

Let's draw some observations from the above output:

* EDHEC LS EQ is selected as a predictor everytime
* US 3M TR is selected as a predictor 38% of the time (US 10Y TR is selected 21% of the time)
* Coefficient sign on the treasury variables suggest a positive relationship (since 90-95% of the time the coefficients were positive in nature)
* US 3m TR had a statistically significant coefficient 53.33% of the time

The above observations can make it easier for us to decide which of the two treasury variables we could use as a possible predictor. We could also use this information to align our view regarding the HAM2 portfolio. If the portfolio is supposed to be more sensitive towards short term interest rates then this quantitative analysis supports that view. 

This really was a simple example but hopefully I was able to get my point across.   
Feel free to leave comments below. Thanks !

Credits:

* [Knitr](http://yihui.name/knitr/): *Yihui Xie*
* [Performance Analytics](http://cran.r-project.org/web/packages/PerformanceAnalytics/index.html): *Peter Carl and Brian Peterson*
* [MASS](http://cran.r-project.org/web/packages/MASS/index.html)  : *Brian Ripley*
