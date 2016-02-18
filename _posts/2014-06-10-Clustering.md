---
title: "Clustering"
layout: post
---

Here's an example of how one could use 3d plots and some clustering to make some inferences from available data. I am using a set of financial indicators available on [**FRED**](https://research.stlouisfed.org/fred2/]). You can download it [**here**](https://github.com/royr2/royr2.github.io/tree/master/assets/downloads). I wanted to compare two different time periods using this data: 

* The dot com bubble
* The housing bubble

Let's first install and load all the packages we need.


{% highlight r %}
#Install Packages 
#install.packages(c("zoo", "RColorBrewer", "rgl","quantmod","ggplot2"))
options(warn=-1)
#Load Packages 
library(class)
library(zoo)
library(RColorBrewer)
library(rgl)
library(quantmod)
library(ggplot2)
{% endhighlight %}

Next let's load in the dataset we want to work with.


{% highlight r %}
#Clean the workspace
rm(list=ls())

#Load Data
dat = read.table(file="financial_indicators.txt",header=T,sep="\t")
{% endhighlight %}

Let's look at the data first.


{% highlight r %}
#Set some colors
cols=brewer.pal(n=8,name="Set1")

#Convert to zoo object
dat = zoo(x=dat[,-1],order.by=as.Date(dat[,1],format="%Y-%m-%d"))

#Print the first few rows 
head(dat)
{% endhighlight %}



{% highlight text %}
##            CANDH CFNAI CFNAIDIFF CFNAIMA3 EUANDH KCFSI PANDI SOANDI
## 1967-03-01 -0.04 -0.49        NA       NA  -0.06    NA -0.37  -0.02
## 1967-04-01 -0.01 -0.15        NA       NA  -0.23    NA  0.21  -0.11
## 1967-05-01 -0.06 -0.65     -0.22    -0.43   0.11    NA -0.60  -0.10
## 1967-06-01  0.07 -0.02     -0.20    -0.27   0.10    NA -0.15  -0.03
## 1967-07-01 -0.07 -0.34     -0.25    -0.33   0.19    NA -0.28  -0.18
## 1967-08-01 -0.01  1.49      0.13     0.38   0.41    NA  0.81   0.28
##            UMCSENT USSLIND
## 1967-03-01      NA      NA
## 1967-04-01      NA      NA
## 1967-05-01      NA      NA
## 1967-06-01      NA      NA
## 1967-07-01      NA      NA
## 1967-08-01      NA      NA
{% endhighlight %}



{% highlight r %}
#Plot the data set
plot(dat,main="Financial Indicators",xlab="Time",col=cols,lwd=1)
{% endhighlight %}

<img src="/assets/figures/Clustering/unnamed-chunk-3-1.png" title="center" alt="center" style="display: block; margin: auto;" />

I have got 10 different financial indicators. Let's reduce the number of dimensions using some principal component analysis.


{% highlight r %}
#PCA Decomposition
pca = princomp(x=na.omit(dat),cor=T,scores=T)
plot(pca,type="lines")
{% endhighlight %}

<img src="/assets/figures/Clustering/unnamed-chunk-4-1.png" title="center" alt="center" style="display: block; margin: auto;" />

{% highlight r %}
#Take the first three components
mat = pca$scores[,1:3]
{% endhighlight %}

I took the first three principal components since it'll be easier to make plots in 3 dimensions.


{% highlight r %}
plot3d(mat,col=cols[1],size=10,type='p')

#Note that the command will create a separate plot window you can interact with.
{% endhighlight %}
<img src="/assets/figures/Clustering/Base.png" title="center" alt="center" style="display: block; margin: auto;" />

Let's add some time overlays to this plot. Let's look at recession dates +- 1 year.

{% highlight r %}
#2007 / 2008 Recession
#Recession Start Date
t1 = "2006-12-01"

#Recession End Date
t2 = "2010-06-01"
  
idx = rownames(mat)>=t1 & rownames(mat)<=t2
mat.rec = mat[idx,]

#Base Plot
plot3d(mat[,1:3],col=cols[2],size=10,type='p',main="2007/08 Recession (in Black)")

#Time Overlay
plot3d(mat.rec[,1:3],col="black",size=2,type='s',add=T)
{% endhighlight %}

The plot looks something like this (recession in black)

<img src="/assets/figures/Clustering/Rec 1.png" title="center" alt="center" style="display: block; margin: auto;" />

In comparision, the dot com bubble looks like this:

{% highlight r %}
#Dot Com Bubble
#Start Date
t1 = "2000-03-01"

#End Date
t2 = "2002-11-01"

idx = rownames(mat)>=t1 & rownames(mat)<=t2
mat.rec = mat[idx,]

#Base Plot
plot3d(mat[,1:3],col=cols[3],size=10,type='p')

#Time Overlay
plot3d(mat.rec[,1:3],col="black",size=2,type='s',add=T)
{% endhighlight %}

<img src="/assets/figures/Clustering/Rec 2.png" title="center" alt="center" style="display: block; margin: auto;" />

The same financial indicators seem to be behaving very differently during the two economic recessions. This seems to support the fact the the dot com bubble and the housing bubble were very different in nature. [See here](http://conversableeconomist.blogspot.com/2011/10/why-didnt-dot-com-crash-hurt-like.html)

Lets use some k-means clustering to visualize this effect


{% highlight r %}
#K-Means clustering using 2 centers
clust = kmeans(x=mat,centers=2)

#Combine data with cluster and color information
mat = data.frame(mat,cluster = as.factor(clust$cluster))
mat = data.frame(mat,cols = cols[as.factor(clust$cluster)])

#Plot
plot3d(mat,col=mat[,"cols"],size=10,type='p')
text3d(x=clust$centers[,1],y=clust$centers[,2],z=clust$centers[,3],texts=c("c1","c2","c3"),col="red",cex=2)
{% endhighlight %}

<img src="/assets/figures/Clustering/Cluster.png" title="center" alt="center" style="display: block; margin: auto;" />

Most of the points in the 07/08 recession seem to fall in cluster 1. Let's compare the two in one single plot

{% highlight r %}
#Base Plot
plot3d(mat,col="gray",size=10,type='p')
text3d(x=clust$centers[,1],y=clust$centers[,2],z=clust$centers[,3],texts=c("c1","c2"),col="red",cex=2)

#Compare the two recessions
#Housing Bubble in RED
t1 = "2007-12-01"
t2 = "2009-06-01"
  
idx = rownames(mat)>=t1 & rownames(mat)<=t2
mat.rec = mat[idx,]
plot3d(mat.rec[,1:3],col=cols[1],size=2,type='s',add=T)

#Dot com bubble in BLUE
t1 = "1997-04-01"
t2 = "2003-06-01"
  
idx = rownames(mat)>=t1 & rownames(mat)<=t2
mat.rec = mat[idx,]
plot3d(mat.rec[,1:3],col=cols[2],size=2,type='s',add=T)
{% endhighlight %}

Note that the housing bubble data is in red and the dotcom bubble data in blue

Let's also compare some common economic indicators. Let's download some data from FRED.


{% highlight r %}
#Get Data
getSymbols(Symbols=c("SPCS10RSA","UNRATE","GDPC1","BAA10Y"),src="FRED")
{% endhighlight %}



{% highlight text %}
## [1] "SPCS10RSA" "UNRATE"    "GDPC1"     "BAA10Y"
{% endhighlight %}



{% highlight r %}
#Merge
mat = merge(SPCS10RSA,UNRATE,GDPC1,BAA10Y)
mat = mat[which(index(mat)=="1990-01-01"):nrow(mat),]

#Convert to Quarterly
mat = aggregate(x=mat,by=as.yearqtr,FUN=function(x){
  round(mean(x,na.rm=T),2)
})

#Get limits
xmin1 = which(index(mat)=="2007 Q4")
xmax1 = which(index(mat)=="2009 Q2")

xmin2 = which(index(mat)=="2001 Q1")
xmax2 = which(index(mat)=="2002 Q1")

ymin = -Inf
ymax = Inf

limits = data.frame(xmin1,xmax1,xmin2,xmax2,ymin,ymax)

#Convert to data.frame
mat = data.frame(mat,dates=index(mat),idx=1:nrow(mat))

p1 = ggplot(mat,aes(x=idx))+
  geom_line(aes(y=SPCS10RSA),size=1,col=cols[2])+
  xlab("Time")+ggtitle("House Price Index")+
  geom_rect(data=limits,
            aes(xmin=xmin1,xmax=xmax1,ymin=ymin,ymax=ymax),
            fill="black",alpha=0.2,inherit.aes=F)+
  geom_rect(data=limits,
            aes(xmin=xmin2,xmax=xmax2,ymin=ymin,ymax=ymax),
            fill="black",alpha=0.2,inherit.aes=F)
  
p2 = ggplot(mat,aes(x=idx))+
  geom_line(aes(y=UNRATE),size=1,col=cols[1])+
  xlab("Time")+ggtitle("Unemployment")+
  geom_rect(data=limits,
            aes(xmin=xmin1,xmax=xmax1,ymin=ymin,ymax=ymax),
            fill="black",alpha=0.2,inherit.aes=F)+
  geom_rect(data=limits,
            aes(xmin=xmin2,xmax=xmax2,ymin=ymin,ymax=ymax),
            fill="black",alpha=0.2,inherit.aes=F)

p3 = ggplot(mat,aes(x=idx))+
  geom_line(aes(y=GDPC1),size=1,col=cols[3])+
  xlab("Time")+ggtitle("GDP")+
  geom_rect(data=limits,
            aes(xmin=xmin1,xmax=xmax1,ymin=ymin,ymax=ymax),
            fill="black",alpha=0.2,inherit.aes=F)+
  geom_rect(data=limits,
            aes(xmin=xmin2,xmax=xmax2,ymin=ymin,ymax=ymax),
            fill="black",alpha=0.2,inherit.aes=F)

p4 = ggplot(mat,aes(x=idx))+
  geom_line(aes(y=BAA10Y),size=1,col=cols[4])+
  xlab("Time")+ggtitle("Interest Rate Spread")+
  geom_rect(data=limits,
            aes(xmin=xmin1,xmax=xmax1,ymin=ymin,ymax=ymax),
            fill="black",alpha=0.2,inherit.aes=F)+
  geom_rect(data=limits,
            aes(xmin=xmin2,xmax=xmax2,ymin=ymin,ymax=ymax),
            fill="black",alpha=0.2,inherit.aes=F)

source("Multiplot.R")
multiplot(p1,p2,p3,p4,cols=2)
{% endhighlight %}

<img src="/assets/figures/Clustering/unnamed-chunk-10-1.png" title="center" alt="center" style="display: block; margin: auto;" />

Note that code for the function 'multiplot' can be found [here][link]

[link]:http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/

I think the plots above speak to how bad the recent recession was. The fact that the 07/08 recession had debt as one of it's primary drivers is evident in how wide the interest rate spreads were at the time. The extensive use of credit derivatives and securitization helped pushed homeowner-level defaults and losses to big investment banks and other institutional investors only magnifying the effect. 

Here are some posts I came across which provide good insight.  
http://conversableeconomist.blogspot.com/2011/10/why-didnt-dot-com-crash-hurt-like.html  
http://jaredbernsteinblog.com/why-was-the-housing-bubble-so-much-more-damaging-than-the-dot-com-bubble/

Let me know what you think in the comments below...
















