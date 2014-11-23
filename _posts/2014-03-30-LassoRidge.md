---
title: "Lasso and Ridge Regression"
author: "Riddhiman Roy"
date: "March 30th 2014"
output:
  html_document:
    keep_md: yes
    self_contained: yes
layout: post
---

I wanted to follow up on my last post with a post on using **Ridge and Lasso regression** . **Lasso** can also be used for **variable selection**.

### Ridge Regression
[*Ridge regression*](http://www.stat.cmu.edu/~ryantibs/datamining/lectures/16-modr1.pdf) modifies the *least squares* objective function by adding to it a penalty term (*L2 Norm*). Hence, the objective function that needs to be minimized can be given as: 

$$ \sum_{i}(y_i-x^T\beta)^2+\lambda\sum_{j=1}^{p}\beta_j^2 $$

The penalty term shrinks the coefficients in the final regression equation. \(\lambda\) decides how small the coefficients will be. Larger the value of $$ \lambda $$ smaller the coefficients. 

*Ridge regression* cannot be used for variable selection in its truest sense but can be used to derive some inferences about the predictor variables. We can fine tune the $$ \lambda $$ parameter to drive the coefficients of a few variables to be very small, thereby having some control on the number of variables having significantly large coefficients. 

Note that since *ridge regression* uses the *L2* norm, the coefficients will *not exactly equal zero*. So, as a final step we could either choose to ignore those variables that have very small coefficients and fit our model just using the remaining variables and get a clean model or, keep the output from ridge regression as is. 

### LASSO
[*Lasso*](http://statweb.stanford.edu/~tibs/lasso/lasso.pdf) is very similar to *ridge regression*. The only differ*ence being the penalty that is added to the *least squares* objective function. *Lasso* uses the *L1* norm instead. The objective function looks like this:

$$ sum_{i}(y_i-x^T\beta)^2+\lambda\sum_{j=1}^{p}\ |beta_j|$$

The one advantage *lasso* has over *ridge regression* is that coefficients can *exactly equal  zero*. This property of the *L1 norm* is why *lasso* can be used for *continuous variable selection*. 

Now let's look at some R code to help implement these two bad boys. 

We'll be using the following packages:

* [Performance Analytics](http://cran.r-project.org/web/packages/PerformanceAnalytics/index.html)
* [MASS](http://cran.r-project.org/web/packages/MASS/index.html)
* [LARS](http://cran.r-project.org/web/packages/lars/lars.pdf)


## R-Code

### Data and Libraries
Let's install and load up the needed libraies and data sets first.

{% highlight r %}
#Load library
#install.packages(c("PerformanceAnalytics","MASS","lars"))
library(PerformanceAnalytics)
library(MASS)
library(lars)
data(managers)
{% endhighlight %}

Ridge is up first. The *MASS* package has an implementation called *lm.ridge*. Let's use that here. 

{% highlight r %}
#Fit the model first without specifying the lambda parameter
fit.ridge <- lm.ridge(formula=HAM2~.,data=managers[,c(2,7:10)])

#OLS regression
fit.ols <- lm(formula=HAM2~.,data=managers[,c(2,7:10)])

#Let's compare the Coefficients
table=cbind(coef(fit.ridge),coef(fit.ols))
colnames(table)=c("Ridge","OLS")
print(table)
{% endhighlight %}

{% highlight text %}
##                     Ridge         OLS
##               -0.00511546 -0.00511546
## `EDHEC LS EQ`  1.53906910  1.53906910
## `SP500 TR`    -0.19317529 -0.19317529
## `US 10Y TR`    0.04581282  0.04581282
## `US 3m TR`     1.42951282  1.42951282
{% endhighlight %}

This clearly shows that at the heart of ridge regression sits good old *OLS* :-)  
Now let's see how the $$ \lambda $$ parameter affects the coefficients.

{% highlight r %}
#Let's start with a random value of Lambda
fit.ridge <- lm.ridge(formula=HAM2~.,data=managers[,c(2,7:10)],lambda=100)

table=cbind(coef(fit.ridge),coef(fit.ols))
colnames(table)=c("Lambda=100","OLS")
print(table)
{% endhighlight %}

{% highlight text %}
##                Lambda=100         OLS
##               0.002125474 -0.00511546
## `EDHEC LS EQ` 0.617579990  1.53906910
## `SP500 TR`    0.061262635 -0.19317529
## `US 10Y TR`   0.005726839  0.04581282
## `US 3m TR`    1.357404708  1.42951282
{% endhighlight %}
Note the coefficients have shrunk in value. Let's try to do the same for multiple values of $$\lambda$$

{% highlight r %}
#Let's start with a random value of Lambda
fit.ridge <- lm.ridge(formula=HAM2~.,data=managers[,c(2,7:10)],lambda=seq(from=0,to=100,by=1))

#Let's plot this information
matplot(x=t(fit.ridge$coef),type='l',
        main="Ridge Regression Lambda vs Coefficient Plot",
        xlab="Lambda Values",
        ylab="Coefficients",
        col=c("black","red","blue","green"))
grid()
legend("topright",legend=rownames(fit.ridge$coef),  
       fill=c("black","red","blue","green"),
       bty='n',
       cex=1)
{% endhighlight %}

<img src="/assets/figures/LassoRidge/unnamed-chunk-4-1.png" title="center" alt="center" style="display: block; margin: auto;" />
The plot is self explanatory and shows how coefficients are affected as $$\lambda$$ increases. This clearly shows that *EDHEC LS EQ* and *SP500* are the two most important predictor variables as far as predicting *HAM 2* returns are concerned. This is also supported by the stepwise variable selection process (see below).

{% highlight r %}
bootStepAIC::boot.stepAIC(object=fit.ols,data=data.frame(managers[,c(2,7:10)]))
{% endhighlight %}

{% highlight text %}
## 
## Summary of Bootstrapping the 'stepAIC()' procedure for
## 
## Call:
## lm(formula = HAM2 ~ ., data = managers[, c(2, 7:10)])
## 
## Bootstrap samples: 100 
## Direction: backward 
## Penalty: 2 * df
## 
## Covariates selected
##             (%)
## EDHEC.LS.EQ 100
## SP500.TR     79
## US.3m.TR     31
## US.10Y.TR    24
## 
## Coefficients Sign
##              + (%)  - (%)
## EDHEC.LS.EQ 100.00   0.00
## US.3m.TR    100.00   0.00
## US.10Y.TR    83.33  16.67
## SP500.TR      0.00 100.00
## 
## Stat Significance
##                (%)
## EDHEC.LS.EQ 100.00
## SP500.TR     82.28
## US.3m.TR     41.94
## US.10Y.TR    20.83
## 
## 
## The stepAIC() for the original data-set gave
## 
## Call:
## lm(formula = HAM2 ~ `EDHEC LS EQ` + `SP500 TR`, data = managers[, 
##     c(2, 7:10)])
## 
## Coefficients:
##   (Intercept)  `EDHEC LS EQ`     `SP500 TR`  
##    -0.0005315      1.5548911     -0.2007421  
## 
## 
## Stepwise Model Path 
## Analysis of Deviance Table
## 
## Initial Model:
## HAM2 ~ `EDHEC LS EQ` + `SP500 TR` + `US 10Y TR` + `US 3m TR`
## 
## Final Model:
## HAM2 ~ `EDHEC LS EQ` + `SP500 TR`
## 
## 
##            Step Df     Deviance Resid. Df Resid. Dev       AIC
## 1                                     115 0.07362914 -877.5447
## 2 - `US 10Y TR`  1 9.853917e-05       116 0.07372768 -879.3843
## 3  - `US 3m TR`  1 6.114188e-04       117 0.07433909 -880.3932
{% endhighlight %}

### LASSO
Now let's quickly review the LASSO implementation. This is provided by the **LARS** package. Again let's first see if we can get back the OLS estimates from *Lasso*.

{% highlight r %}
library(lars)
#Matrix of predictors and center
df=data.frame(na.omit(managers[,c(2,7:10)]))
x=as.matrix(cbind(1,df[,-1]))

#Response variable
y=as.numeric(df[,1])

#Lasso fit
fit.lasso=lars(x=x,y=y,type="lasso",normalize=F,intercept=F)

table=cbind(coef.lars(fit.lasso,s=0,mode="lambda"),
            coef(fit.ols))
colnames(table)=c("LASSO","OLS")

#And if there is any justice in this world these should be equal
table
{% endhighlight %}



{% highlight text %}
##                   LASSO         OLS
## 1           -0.00511546 -0.00511546
## EDHEC.LS.EQ  1.53906910  1.53906910
## SP500.TR    -0.19317529 -0.19317529
## US.10Y.TR    0.04581282  0.04581282
## US.3m.TR     1.42951282  1.42951282
{% endhighlight %}
As expected, we'll get back the OLS estimates if the penalty parameter $$\lambda$$ = 0 yay !

**Caveat**  
Please note that we chose **not to normalize** the predictor variables as shown in the code snippet(s) above. This was just to show that the *lasso* will return the standard *ols* estimates if $$\lambda$$=0. This is **not ideal**. Both *L1 and L2* norms will not penalize the predictor variables equally if they are not on the same scale. Ideally, the predictors should be normalized and centered before running lasso/ridge.

Now let's see what happens when we increase the value of lambda. The lars function generates estimates for all possible values of \(\lambda\). Hence, once the fitting is done its just a matter of extracting the required coefficients at a particular \(\lambda\). This can be done using the **coef.lars()** function. 

Let's visualize the *lasso path* first. This time we'll be normalizing the predictors and centering the design matrix to conform to generally accepted practices (see [link](http://www.stat.cmu.edu/~ryantibs/datamining/lectures/17-modr2-marked.pdf)).

{% highlight r %}
#Center
df=df-apply(df,2,mean)
#Fit Lasso
fit.lasso=lars(x=df[,-1],y=df[,1],type="lasso",trace=F,normalize=T,intercept=F)
#Plot Lasso Path
plot(fit.lasso)
{% endhighlight %}

<img src="/assets/figures/LassoRidge/unnamed-chunk-7-1.png" title="center" alt="center" style="display: block; margin: auto;" />

The plot is easy enough to interpret. Note that the x-axis is the *fraction of L1 norm* and not $$\lambda$$. Hence, when $$\lambda=0$$ $$\frac{\beta}{max(\beta)}$$=1. So, as $$\lambda$$ goes to zero, more and more variables enter the regression equation. 

To see the relationship between $$\lambda$$ and $$\frac{\beta}{max(\beta)}$$ see the following code snippet. By trial and error I was able to find out that variable 3 (*US 10Y TR*) enters the equation at $$\lambda$$=0.0096. From there I can back into the *L1 fraction*.


{% highlight r %}
#Get coefficients at lambda=0 (Max coefficient values)
coef.max=coef.lars(fit.lasso,s=0,mode="lambda")
maxL1norm=sum(abs(coef.max))

#Get coefficients at lambda = 0.0096
(coef.lambda=coef.lars(fit.lasso,s=0.0096,mode="lambda"))
{% endhighlight %}



{% highlight text %}
##  EDHEC.LS.EQ     SP500.TR    US.10Y.TR     US.3m.TR 
##  1.408227814 -0.131437233  0.007295828 -0.158764030
{% endhighlight %}



{% highlight r %}
L1norm=sum(abs(coef.lambda))
L1norm/maxL1norm
{% endhighlight %}


{% highlight text %}
## [1] 0.7874097
{% endhighlight %}
This is close to the $$\frac{\beta}{max(\beta)}$$ value suggested by the plot (see the vertical line labeled "3")  

### Cross Validation
Now that we know how to go about fitting the lasso, let's use  *cross validation* to select the best value for $$\lambda$$. We'll use the **cv.lars()** function.


{% highlight r %}
cv.lars(x=df[,-1],y=df[,1],intercept=F,type="lasso")
title("Lars CV")
grid()
{% endhighlight %}

<img src="/assets/figures/LassoRidge/unnamed-chunk-9-1.png" title="center" alt="center" style="display: block; margin: auto;" />

{% highlight r %}
#The plot suggests that a L1 fraction = 0.5 should be good enough since the drop in CV MSE after that is not significant. To retrieve the coefficients at this L1 Fraction - 
coef.lars(fit.lasso,s=0.5,mode="fraction")
{% endhighlight %}



{% highlight text %}
## EDHEC.LS.EQ    SP500.TR   US.10Y.TR    US.3m.TR 
##    1.069299    0.000000    0.000000    0.000000
{% endhighlight %}

Lets look at how well the model fits the data at this $$ \lambda $$ value.

{% highlight r %}
y_hat=predict.lars(object=fit.lasso,newx=df[,-1],s=0.5,mode="fraction")
mat=cbind(df[,1],y_hat$fit)

matplot(mat,type='l',col=c("black","red"),main="In Sample Plot",ylab="HAM2")
legend("topright",legend=c("Actual","Predicted"),fill=c("black","red"),bty='n')
grid()
{% endhighlight %}

<img src="/assets/figures/LassoRidge/unnamed-chunk-10-1.png" title="center" alt="center" style="display: block; margin: auto;" />

Not too shabby :P

Hopefully this was helpful in some way. Let me know what you think in the comments below !

Thanks !

Credits:

* [Knitr](http://yihui.name/knitr/): *Yihui Xie*
* [Performance Analytics](http://cran.r-project.org/web/packages/PerformanceAnalytics/index.html): *Peter Carl and Brian Peterson*
* [lars](http://cran.r-project.org/web/packages/lars/index.html)  : *Brad Efron and Trevor Hastie*
* [MASS](http://cran.r-project.org/web/packages/MASS/index.html)  : *Brian Ripley*
* [bootStepAIC](http://cran.r-project.org/web/packages/bootStepAIC/bootStepAIC.pdf)  : *Dimitris Rizopoulos*


Some interesting links related to this post:

* http://cbio.ensmp.fr/~jvert/svn/tutorials/practical/linearregression/linearregression.R  
* http://statweb.stanford.edu/~tibs/lasso/simple.html  
* http://www.stat.cmu.edu/~ryantibs/datamining/lectures/17-modr2.pdf
