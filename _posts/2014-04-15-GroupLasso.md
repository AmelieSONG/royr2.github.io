---
title: "Group Lasso"
layout: post
---

There is a nice extention to the **Lasso** which lets variable selection work on a group of variables. Hence, instead of a single variable entering the mix, an entire group of variables enter the regression equation together (see [*Yuan and Lin*](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.79.2062&rep=rep1&type=pdf)). Also see [link](http://blog.cos.name/bigknife/files/2012/07/group-lars.pdf) 

The **Lasso** estimate is the solution to:

$$   
||y-X\beta||^2 \mbox{subject to} \sum_{i=1}^{P}(|\beta_{i}|) \le t
$$   

The **Group Lasso** modifies this objective function by taking into account the coefficients from each group:

$$
\frac{1}{2}||y-\sum_{j=1}^{J}x_j\beta_j||^2 + \lambda\sum_{j=1}^{J}||\beta_j||_{K_j}
$$

where $$\vert \vert \cdot \vert \vert$$ is the euclidean norm.

There are a lot of different packages in **R** which implement *group Lasso*. I'll use a package called **gglasso** written by [Yi Yang and Hui Zou](http://cran.r-project.org/web/packages/gglasso/gglasso.pdf)

For this little exercise let's try to predict the **US Unemployment Rate** using some information that's provided by the **Fed** ([link](http://www.federalreserve.gov/bankinforeg/stress-tests/supervisory-baseline-adverse-and-severely-adverse-scenarios.htm)). These are the **CCAR**/**DFAST** regulatory stress testing scenarios that banks need to use to project their financial statement line items. The Fed provides forward looking projections for a variety of different macro-economic indicators. Let's use them to create forward looking unemployment projections. 

You can download the input files I am using [here](https://github.com/royr2/royr2.github.io/tree/master/assets/downloads).   

R-Code
===

Data and Libraries  
---

{% highlight r %}
#install.packages("gglasso")
#install.packages("zoo")
#install.packages("RColorBrewer")
library(gglasso)
library(RColorBrewer)
library(zoo)

hist=read.csv("historical data.csv")
proj=read.csv("projections.csv")

hist=data.frame(Date=as.Date(as.yearqtr(hist[,1])),hist[,-1])
proj=data.frame(Date=proj[,1],proj[,-1])
{% endhighlight %}

Now let's plot and have a look at the data. 

{% highlight r %}
#Note that columns in a data frame can be referenced using the '$' operator
plot(y=hist$Unemployment.Rate,x=hist$Date,main="Unemployment",lwd=2,col="slateblue",type="l",
     xlab="Time",ylab="Unemployment %")
grid()
{% endhighlight %}

<img src="/assets/figures/GroupLasso/unnamed-chunk-2-1.png" title="center" alt="center" style="display: block; margin: auto;" />

>**Note:**
The time series is not stationary and ideally we shouldn't be using such a time series for any kind of regression analysis but for now let's keep things simple. We wont be using any kind of transformations on our data. 

## Variable Selection
Now let's use group lasso to see which variable groups enter the regression equation. First, let's assign group numbers to our variables. I am going to use the following groupings:  

* GDP, income and inflation rate as *Group1*
* All treasury yields and BBB Corporate yield as *Group2*
* Mortage rate, prime rate ,HPI % change and commercial RE % change as *Group3*
* Dow Jones and VIX as *Group4*

{% highlight r %}
names(hist)
{% endhighlight %}



{% highlight text %}
##  [1] "Date"                                      
##  [2] "Real.GDP.Growth"                           
##  [3] "Real.Disposable.Income.Growth"             
##  [4] "Unemployment.Rate"                         
##  [5] "CPI.Inflation.Rate"                        
##  [6] "X3.Month.Treasury"                         
##  [7] "X5.Year.Treasury"                          
##  [8] "X10.Year.Treausry"                         
##  [9] "BBB.Corporate.Yield"                       
## [10] "Mortgage.Rate"                             
## [11] "Prime.Rate"                                
## [12] "House.Price.Index.pct.chg"                 
## [13] "Commercial.Real.Estate.Price.Index.pct.chg"
## [14] "Market.Volatility.Index..VIX..pct.chg"     
## [15] "Dow.Jones.Total.Stock.Market.Index.pct.chg"
{% endhighlight %}



{% highlight r %}
#Remove Dates and Unemployment from the model matrix 
X=hist[,c(-1,-4)]
X=as.matrix(X)
Y=hist[,4]
grp=c(1,1,1,2,2,2,2,3,3,3,3,4,4)
fit=gglasso(x=X,y=Y,group=grp,loss='ls')
coef.mat=fit$beta

#Group1 enters the equation
g1=max(which(coef.mat[1,]==0))

#Group2 enters the equation
g2=max(which(coef.mat[4,]==0))

#Group3 enters the equation
g3=max(which(coef.mat[8,]==0))

#Group4 enters the equation
g4=max(which(coef.mat[12,]==0))

#Coefficient Plot. Let's also use some nice colors

cols=brewer.pal(5,name="Set1")

plot(fit$b0,main="Coefficient vs Step",
     ylab="Intercept",xlab="Step (decreasing Lambda =>)",
     col=cols[1],
     xlim=c(-1,100),
     ylim=c(5,max(fit$b0)),
     type="l",lwd=4)
grid()
par(new=T)

x=c(g1,g2,g3,g4)
y=c(fit$b0[g1],fit$b0[g2],fit$b0[g3],fit$b0[g4])

plot(x=x,y=y,pch=13,lwd=2,cex=2,col=cols[-1],
     xlim=c(-1,100),ylim=c(5,max(fit$b0)),
     xaxt='n',yaxt='n',xlab="",ylab="")

lmda=round(fit$lambda[c(g1,g2,g3,g4)],2)
text(x=x-0.5,y=y+0.1,labels=c("Group1","Group2","Group3","Group4"),pos=3,cex=0.9)
text(x=x-0.5,y=y-0.1,labels=paste("Lambda\n=",lmda),pos=1,cex=0.8)
{% endhighlight %}

<img src="/assets/figures/GroupLasso/unnamed-chunk-3-1.png" title="center" alt="center" style="display: block; margin: auto;" />

The intercept is not penalized and hence is always present in the regression equation. But as the plot above shows, each group enters the regression equation at a particular value of lambda. For example:


{% highlight r %}
coef.mat[,c(12,13,14)]
{% endhighlight %}



{% highlight text %}
##                                                     s11          s12
## Real.GDP.Growth                             0.000000000  0.000000000
## Real.Disposable.Income.Growth               0.000000000  0.000000000
## CPI.Inflation.Rate                          0.000000000  0.000000000
## X3.Month.Treasury                           0.000000000  0.000000000
## X5.Year.Treasury                            0.000000000  0.000000000
## X10.Year.Treausry                           0.000000000  0.000000000
## BBB.Corporate.Yield                         0.000000000  0.000000000
## Mortgage.Rate                               0.000000000  0.000000000
## Prime.Rate                                  0.000000000  0.000000000
## House.Price.Index.pct.chg                   0.000000000  0.000000000
## Commercial.Real.Estate.Price.Index.pct.chg  0.000000000  0.000000000
## Market.Volatility.Index..VIX..pct.chg      -0.002530964 -0.002655155
## Dow.Jones.Total.Stock.Market.Index.pct.chg  0.002571568  0.002952220
##                                                     s13
## Real.GDP.Growth                             0.000000000
## Real.Disposable.Income.Growth               0.000000000
## CPI.Inflation.Rate                          0.000000000
## X3.Month.Treasury                           0.000000000
## X5.Year.Treasury                            0.000000000
## X10.Year.Treausry                           0.000000000
## BBB.Corporate.Yield                         0.000000000
## Mortgage.Rate                              -0.003270736
## Prime.Rate                                 -0.005926023
## House.Price.Index.pct.chg                  -0.001951581
## Commercial.Real.Estate.Price.Index.pct.chg -0.004634945
## Market.Volatility.Index..VIX..pct.chg      -0.002592666
## Dow.Jones.Total.Stock.Market.Index.pct.chg  0.003267311
{% endhighlight %}
Note how at step = 13 group 3 enters the regression equation and all the variables in group3 have coefficients > 0. Group4 was already in the regression equation starting at step = 1 (see plot)

Hence, group4 enters the regression first, then group3, then group1 and group2 enters last.

Let's get to cross validation and some forward looking projections.

## Cross Validation and Inferences
Thankfully the **gglasso** package has a built in function that provides cross validation capabilities.


{% highlight r %}
#Cross Validation
fit.cv=cv.gglasso(x=X,y=Y,group=grp,nfolds=10)
plot(fit.cv)
{% endhighlight %}

<img src="/assets/figures/GroupLasso/unnamed-chunk-5-1.png" title="center" alt="center" style="display: block; margin: auto;" />

{% highlight r %}
#Pick the best Lambda
lmbda=fit.cv$lambda.1se
(coefs=coef.gglasso(object=fit,s=lmbda))
{% endhighlight %}



{% highlight text %}
##                                                       1
## (Intercept)                                15.603969368
## Real.GDP.Growth                             0.093737728
## Real.Disposable.Income.Growth              -0.016536569
## CPI.Inflation.Rate                         -0.016866825
## X3.Month.Treasury                          -0.098563832
## X5.Year.Treasury                            0.419097056
## X10.Year.Treausry                           1.125019983
## BBB.Corporate.Yield                         0.222636779
## Mortgage.Rate                              -2.325980613
## Prime.Rate                                 -0.608963096
## House.Price.Index.pct.chg                  -0.231779631
## Commercial.Real.Estate.Price.Index.pct.chg -0.012288433
## Market.Volatility.Index..VIX..pct.chg       0.007157314
## Dow.Jones.Total.Stock.Market.Index.pct.chg  0.016550570
{% endhighlight %}



{% highlight r %}
#At best lambda get coefficients and fitted values
plt=cbind(Y,predict.gglasso(object=fit,newx=X,s=lmbda,type='link'))
matplot(plt,main="Predicted vs Actual",type='l',lwd=2,col=cols[c(1,2)],
        ylab="Unemplyoment %",
        xlab="Time")
grid()
legend(x=40,y=5,legend=c("Actual","Predicted"),fill=c(cols[c(1,2)]),bty="n",cex=0.7)
{% endhighlight %}

<img src="/assets/figures/GroupLasso/unnamed-chunk-5-2.png" title="center" alt="center" style="display: block; margin: auto;" />

Let's now try to generate some forward looking projections and compare them against what the Fed has provided us.

{% highlight r %}
#Get forward looking projections
X=as.matrix(proj[,-c(1,4)])
plt=cbind(proj$Unemployment.Rate,predict.gglasso(object=fit,newx=X,s=lmbda,type='link'))
matplot(plt,main="Predicted vs Fed Projections",type='l',lwd=2,col=cols[c(1,2)],
        ylab="Unemplyoment %",
        xlab="Time")
grid()
legend(x=10,y=8,legend=c("Fed Projection"," Our Prediction"),
       fill=c(cols[c(1,2)]),bty="n",cex=0.7)
{% endhighlight %}

<img src="/assets/figures/GroupLasso/unnamed-chunk-6-1.png" title="center" alt="center" style="display: block; margin: auto;" />

Note that since we used a group penalty, the model has all the groups and hence all the variables are in the regression equation. The jaggedness of the predicted line does point towards some overfitting. But we can draw some interesting conclusions from this exercise.

Remember our groupings:

* GDP, income and inflation rate as *Group1*
* All treasury yields and BBB Corporate yield as *Group2*
* Mortage rate, prime rate ,HPI % change and commercial RE % change as *Group3*
* Dow Jones and VIX as *Group4*

Group4 is made of the two variables which are direct indicators of market conditions. Group3 variables reflect housing market conditions. National income and personal income make up group1 and interest rates make up group2. It does make sense that market conditions would have a dominant effect on employment conditions. The housing market is also dependent on employment conditions and vice versa. 

I am sure this analysis could have been done better but I hope this made sense and was helpful. I'll follow this up with a post on sparse group lasso. Let me know what you think in the comments below ! 

**Credits:**

* [Knitr](http://yihui.name/knitr/): *Yihui Xie*
* [Gglasso](http://cran.r-project.org/web/packages/gglasso/gglasso.pdf): *Yi Yang and Hui Zou*
* [RColorBrewer](http://cran.r-project.org/web/packages/RColorBrewer/RColorBrewer.pdf): *Erich Neuwirth*
* [Zoo](http://cran.r-project.org/web/packages/zoo/zoo.pdf): *Achim Zeileis Gabor Grothendieck, Jeffrey A. Ryan, Felix Andrews*
