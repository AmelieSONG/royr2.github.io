---
title: "Sparse Group Lasso"
layout: post
---

As a follow up to my last post, here's a post on **Sparse Group Lasso**.

It's very similar to the  **Group Lasso** criterion in that it provides for sparsity at the group level but different to the extent that it also leads to within group sparsity. 

Here's the [paper](http://www.stanford.edu/~hastie/Papers/SGLpaper.pdf) by Simon, Friedman, Hastie and Tibshirani.

The objective function looks like this (I'm going to borrow notations from the above paper)

$$ \frac{1}{2n}||(y-\sum_{l=1}^{m}(X^l\beta^l))||_{2}^{2}+(1-\alpha)\lambda\sum_{l=1}^{m}\sqrt{p_l}||\beta^l||_2+ \alpha\lambda||\beta||_1 $$

Note that if $$\alpha = 1 $$ the solution would simply be the **Lasso** and if $$\alpha = 0$$ the solution would be the **Group Lasso**.

I'll be using the same input files as in my last post. You can download the input files I am using [here](https://github.com/royr2/royr2.github.io/tree/master/assets/downloads)   

I'll be using the **[SGL](http://cran.us.r-project.org/web/packages/SGL/SGL.pdf)** package for this exercise. 

### R-Code

{% highlight r %}
# install.packages("SGL")
# install.packages("zoo")  
# install.packages("RColorBrewer")  

library(zoo)
library(SGL)
library(RColorBrewer)

hist=read.csv("historical data.csv")
proj=read.csv("projections.csv")

hist=data.frame(Date=as.Date(as.yearqtr(hist[,1])),hist[,-1])
proj=data.frame(Date=proj[,1],proj[,-1])

X=hist[,c(-1,-4)]
X=as.matrix(X)
Y=hist[,4]
grp=c(1,1,1,2,2,2,2,3,3,3,3,4,4)

data=list(x=X, y=Y)

cvFit=cvSGL(data=data,index=grp,type="linear")
plot(cvFit)
{% endhighlight %}

<img src="/assets/figures/SparseGroupLasso/unnamed-chunk-1-1.png" title="center" alt="center" style="display: block; margin: auto;" />

Based on this plot I'll pick a log(lambda) value of -3.4 (ish).


{% highlight r %}
beta=cvFit$fit$beta

#Print the lambda values to see which lambda is close to log(lambda)~ -3.4
cvFit$lambdas
{% endhighlight %}



{% highlight text %}
##  [1] 0.20666290 0.17651729 0.15076898 0.12877653 0.10999209 0.09394771
##  [7] 0.08024370 0.06853867 0.05854104 0.05000174 0.04270806 0.03647830
## [13] 0.03115727 0.02661240 0.02273049 0.01941483 0.01658282 0.01416391
## [19] 0.01209784 0.01033314
{% endhighlight %}



{% highlight r %}
idx=13

#The model corrosponding to this lambda value is:
data.frame(Variables=colnames(X),Coefficients=beta[,idx])
{% endhighlight %}



{% highlight text %}
##                                     Variables Coefficients
## 1                             Real.GDP.Growth     0.000000
## 2               Real.Disposable.Income.Growth     0.000000
## 3                          CPI.Inflation.Rate     0.000000
## 4                           X3.Month.Treasury    -2.755984
## 5                            X5.Year.Treasury     0.000000
## 6                           X10.Year.Treausry     0.000000
## 7                         BBB.Corporate.Yield     0.000000
## 8                               Mortgage.Rate    -3.795816
## 9                                  Prime.Rate    -3.622225
## 10                  House.Price.Index.pct.chg    -1.243893
## 11 Commercial.Real.Estate.Price.Index.pct.chg     0.000000
## 12      Market.Volatility.Index..VIX..pct.chg     0.000000
## 13 Dow.Jones.Total.Stock.Market.Index.pct.chg     0.000000
{% endhighlight %}
Note how the algorithm chose only a few variables from groups 2 and 3. I have used the same groupings as in my last post:

* GDP, income and inflation rate as *Group1*  
* All treasury yields and BBB Corporate yield as *Group2*  
* Mortage rate, prime rate ,HPI % change and commercial RE % change as *Group3*  
* Dow Jones and VIX as *Group4*

Now to extract the model and do projections at that value of lambda. 

{% highlight r %}
#Fit SGL model first
mdl=SGL(data=data,index=grp,type="linear")

#Matrix of future covariate values
X.new=as.matrix(proj[,-c(1,4)])

#Get projections
projections=predictSGL(x=mdl,newX=X.new,lam=idx)
plt=cbind(proj$Unemployment.Rate,projections)

#Plot
cols = brewer.pal(5, name = "Set1")
matplot(plt, main = "Predicted vs Fed Projections", type = "l", lwd = 2, 
        col = cols[c(1,2)], ylab = "Unemplyoment %", xlab = "Time");grid()
legend(x = 10, y = 8, legend = c("Fed Projection", " Our Prediction"), 
       fill = c(cols[c(1,2)]), bty = "n", cex = 0.7)
{% endhighlight %}

<img src="/assets/figures/SparseGroupLasso/unnamed-chunk-3-1.png" title="center" alt="center" style="display: block; margin: auto;" />

Note that we ran all of the above computations are run at an $$ \alpha $$ value of 0.95 (default). 
This can be easily changed as below:


{% highlight r %}
Fit=SGL(data=data,index=grp,type="linear",alpha=0.25)
{% endhighlight %}

