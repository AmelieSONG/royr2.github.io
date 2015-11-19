# Factor Models

This R-Markdown document replicates this presentation by Eric Zivot [Link](http://faculty.washington.edu/ezivot/research/factorModelTutorial_handout.pdf).


```r
# install.packages(c('ellipse',
#                    'PerformanceAnalytics',
#                    'zoo',
#                    'gridExtra'))
# install.packages("fEcofin", repos="http://R-Forge.R-project.org")
```

## Single Factor Model
This following Section will do the following ssing the Berndt dataset:
- Fit a single factor model where the factor is the **market**
- This is done for each return series in the Berndt Dataset
- Estimation essentially consists of cmomputing intercept and beta using **OLS**


```r
library(ellipse)
library(fEcofin)
library(PerformanceAnalytics)
library(zoo)
library(matrixcalc)

# Set Options ####
rm(list = ls())
options(digit = 4)

data(berndtInvest)
head(berndtInvest)
```

```
##    X.Y..m..d CITCRP  CONED CONTIL DATGEN    DEC  DELTA GENMIL GERBER
## 1 1978-01-01 -0.115 -0.079 -0.129 -0.084 -0.100 -0.028 -0.099 -0.048
## 2 1978-02-01 -0.019 -0.003  0.037 -0.097 -0.063 -0.033  0.018  0.160
## 3 1978-03-01  0.059  0.022  0.003  0.063  0.010  0.070 -0.023 -0.036
## 4 1978-04-01  0.127 -0.005  0.180  0.179  0.165  0.150  0.046  0.004
## 5 1978-05-01  0.005 -0.014  0.061  0.052  0.038 -0.031  0.063  0.046
## 6 1978-06-01  0.007  0.034 -0.059 -0.023 -0.021  0.023  0.008  0.028
##      IBM MARKET  MOBIL  PANAM   PSNH  TANDY TEXACO  WEYER  RKFREE
## 1 -0.029 -0.045 -0.046  0.025 -0.008 -0.075 -0.054 -0.116 0.00487
## 2 -0.043  0.010 -0.017 -0.073 -0.025 -0.004 -0.010 -0.135 0.00494
## 3 -0.063  0.050  0.049  0.184  0.026  0.124  0.015  0.084 0.00526
## 4  0.130  0.063  0.077  0.089 -0.008  0.055  0.000  0.144 0.00491
## 5 -0.018  0.067 -0.011  0.082  0.019  0.176 -0.029 -0.031 0.00513
## 6 -0.004  0.007 -0.043  0.019  0.032 -0.014 -0.025  0.005 0.00527
```

```r
# Remove dates and add Rownames ####
bern.df = berndtInvest[,-1]
rownames(bern.df) = as.character(berndtInvest[,1])

# Sharpe's Single Index Model ####
returns.mat = as.matrix(bern.df[,c(-10, -17)]) #Removes the risk free rate and market columns
market.mat = as.matrix(bern.df[, 10, drop = F]) #Creates the market vector DROP = F

N = nrow(bern.df)

# Create the Model Matrix ####
X.mat = cbind(rep(1, N), market.mat)
colnames(X.mat)[1] = "Intercept"

# Multivariate Least Squares ####
# beta = inv(X'X) * X'y
G.hat = solve(crossprod(X.mat)) %*% crossprod(X.mat, returns.mat)
head(G.hat)
```

```
##                CITCRP      CONED      CONTIL       DATGEN         DEC
## Intercept 0.002515032 0.01723480 -0.01143085 -0.006902336 0.007954277
## MARKET    0.667776160 0.09102103  0.73835702  1.028159817 0.843053447
##                 DELTA     GENMIL      GERBER         IBM       MOBIL
## Intercept 0.004843298 0.01283686 0.007657913 0.003278102 0.006208399
## MARKET    0.489460537 0.26776451 0.624806704 0.453024298 0.713515240
##                  PANAM         PSNH      TANDY      TEXACO        WEYER
## Intercept -0.006699213 -0.007191747 0.01024021 0.003360902 -0.001796003
## MARKET     0.730140314  0.212632283 1.05549413 0.613276833  0.816867373
```

```r
# Check
G.hat.check = matrix.inverse(t(X.mat) %*% X.mat) %*% t(X.mat) %*% returns.mat
all.equal(G.hat, G.hat.check)
```

```
## [1] TRUE
```

```r
# Using lm
fit = lm(returns.mat ~ market.mat)
#summary(fit)

# Check
G.hat.lm = as.matrix(coef(fit))
all.equal(G.hat.lm, G.hat)
```

```
## [1] "Attributes: < Component \"dimnames\": Component 1: 2 string mismatches >"
```

```r
# Betas
beta.hat = G.hat[2,]

# Errors
e.hat = returns.mat - X.mat %*% G.hat

# Diagonal Elements (DOF = N-2 since we estimated 2 parameters: intercept and market)
# Essentially calculated the residual error squares
diag.hat = diag(crossprod(e.hat)/(N - 2))

# Same as
ESS = apply(e.hat, 2, function(x){
  sum(x ^ 2)})/(N - 2)
# 
# Total Sum of Squares
TSS = apply(returns.mat, 2, function(x){
  sum((x - mean(x)) ^ 2)
})
# 
#R Squared = 1-DOF * ESS/TSS
R2 = 1 - (N - 2) * ESS / TSS

# So far we calculated:
# 1. A single factor model using OLS where the factor is the market
# 2. Estimation consisteded of intercept and betas
# 3. We also calculated Residual Sum of Squares (ESS)
# 4. And we calcuated the R-Squared values (1 - ESS/TSS)

# Estimation Results ####
print(cbind(beta.hat, ESS, R2))
```

```
##          beta.hat         ESS         R2
## CITCRP 0.66777616 0.004510916 0.31776881
## CONED  0.09102103 0.002509642 0.01531620
## CONTIL 0.73835702 0.020334006 0.11215752
## DATGEN 1.02815982 0.011423433 0.30363127
## DEC    0.84305345 0.006563956 0.33782921
## DELTA  0.48946054 0.008152075 0.12162670
## GENMIL 0.26776451 0.003928272 0.07918778
## GERBER 0.62480670 0.005923786 0.23693790
## IBM    0.45302430 0.002546361 0.27523463
## MOBIL  0.71351524 0.004105181 0.36881762
## PANAM  0.73014031 0.015008039 0.14337179
## PSNH   0.21263228 0.011872481 0.01762690
## TANDY  1.05549413 0.011161778 0.31985965
## TEXACO 0.61327683 0.004634366 0.27661463
## WEYER  0.81686737 0.004153586 0.43082920
```

Next, we will do the following:
- Graphically represent the computations
- Compute correlations and plot them


```r
library(ggplot2)
library(gridExtra)
library(corrplot)

df = data.frame(Beta = beta.hat,
                R.Squared = R2)

# Plot the Betas and R-Squared values
plt1 = ggplot(df, aes(x = rownames(df), y = Beta)) + 
  geom_bar(stat = 'identity') + 
  ggtitle("Estimated Betas for Market Factor") +
  coord_flip()
  
plt2 = ggplot(df, aes(x = rownames(df), y = R2)) +
  geom_bar(stat = 'identity') + 
  ggtitle("Estimated Betas for Market Factor") +
  coord_flip()

grid.arrange(plt1, plt2, ncol = 2)
```

![](Factor_Models_Eric_files/figure-html/unnamed-chunk-3-1.png) 

For variances we have:

$$ r_i = intercept + \beta_i r_{market} + \epsilon_i  $$
$$ \sigma^2 _{r_i} = \beta^2 \sigma^2_{r_{market}} + \sigma^2_\epsilon $$

For covariances we have
$$ \sigma^2_{r_i,r_j} = \Sigma_{t = 1}^{t = n} (r_i - \bar{r_i})(r_j - \bar{r_j})$$
$$$$


```r
# Compute Single Factor Correlations ####
# Eric
cov.si = as.numeric(var(market.mat)) * beta.hat %*% t(beta.hat) + diag(diag.hat)
cor.si = cov2cor(cov.si)
rownames(cor.si) = colnames(cor.si)

# Check
# Check Variances
beta2 = beta.hat ^ 2
sigma.market = var(market.mat)

VAR = beta2 * sigma.market + (ESS)
all.equal(as.numeric(VAR), diag(cov.si))
```

```
## [1] TRUE
```

```r
# Check Covariances
#DO THIS !!

# Plot Correlations ####
corrplot(cor.si, order = "AOE", main = "Correlation Matrix for Factor Model", mar = c(2,2,2,2))
```

![](Factor_Models_Eric_files/figure-html/unnamed-chunk-4-1.png) 

```r
corrplot(cor(returns.mat), order = "AOE", main = "Correlation Matrix for Factor Model", mar = c(2,2,2,2))
```

![](Factor_Models_Eric_files/figure-html/unnamed-chunk-4-2.png) 

The idea is to use these single factor covariances in estimating portfolio weights. The following snippet shows that the portfolio weights do not differ much when using either sample covariances or single factor covariances.

Global minimum variance portfolio weights are given by 
$$ m = {\Sigma^-1 1}/{1'\Sigma^-1 1}


```r
#Using factor covariances
w.gmin.si = solve(cov.si) %*% rep(1,nrow(cov.si))
w.gmin.si = as.data.frame(w.gmin.si/sum(w.gmin.si))
colnames(w.gmin.si) = "Single.Index"

#Using sample covariances
# use sample covariance
w.gmin.sample = solve(var(returns.mat))%*%rep(1,nrow(cov.si))
w.gmin.sample = as.data.frame(w.gmin.sample/sum(w.gmin.sample))
colnames(w.gmin.sample) = "Sample"

plt1 = ggplot(w.gmin.si, aes(rownames(w.gmin.si), Single.Index)) + 
  geom_bar(stat = "identity") + 
  ggtitle("Global Min Variance Portfolio (Using Factor Model)")

plt2 = ggplot(w.gmin.sample, aes(rownames(w.gmin.sample), Sample)) + 
  geom_bar(stat = "identity") + 
  ggtitle("Global Min Variance Portfolio (Using Sample Covariances)")

grid.arrange(plt1, plt2)
```

![](Factor_Models_Eric_files/figure-html/unnamed-chunk-5-1.png) 

## Industry Factor Model
First create a matrix representing which stock belongs to which industry sector. In this we use the following industry classification:
- Oil
- Tech
- Other
This matrix then becomes the **factor sensitivity matrix**, essentially, the factors. Note that in the previous we used a single factor - the **market**.


```r
n.stocks = ncol(returns.mat)
tech.dum = oil.dum = other.dum = matrix(0,n.stocks,1)
rownames(tech.dum) = rownames(oil.dum) = rownames(other.dum) = colnames(returns.mat)
tech.dum[c(4,5,9,13),] = 1
oil.dum[c(3,6,10,11,14),] = 1
other.dum = 1 - tech.dum - oil.dum
B.mat = cbind(tech.dum, oil.dum, other.dum)
colnames(B.mat) = c("TECH", "OIL", "OTHER") 
B.mat
```

```
##        TECH OIL OTHER
## CITCRP    0   0     1
## CONED     0   0     1
## CONTIL    0   1     0
## DATGEN    1   0     0
## DEC       1   0     0
## DELTA     0   1     0
## GENMIL    0   0     1
## GERBER    0   0     1
## IBM       1   0     0
## MOBIL     0   1     0
## PANAM     0   1     0
## PSNH      0   0     1
## TANDY     1   0     0
## TEXACO    0   1     0
## WEYER     0   0     1
```
Now that the factor sensitivity matrix has been created, we need to estimate the **Factor Returns**. We will again OLS to do so.

Note:
- We have **3** factors - Oil, Tech and Other
- We have N = `ncol(returns.mat)` assets.
- We have T = `nrow(returns.mat)` time periods.
- We need to estimate factor returns across T time periods for K = 3 factors.
- Essentially, the betas are factor realizations at every point in time. 
- Hence for each time period we will have 3 betas
- For this computation, the returns matrix needs to be transposed for conformity


```r
# The model matrix 
# Eric Method
X.mat = B.mat #No intercept
F.hat = solve(crossprod(X.mat)) %*% t(X.mat) %*% t(returns.mat)

#Check
fit = lm(t(returns.mat) ~ X.mat - 1)
F.hat.check = coef(fit)

all.equal(as.matrix((F.hat)), 
          as.matrix(F.hat.check))
```

```
## [1] "Attributes: < Component \"dimnames\": Component 1: 3 string mismatches >"
```
Next, we need to plot these estimated industry factors. 


```r
df = as.data.frame(t(F.hat))
plt1 = ggplot(df, aes(x = as.Date(rownames(df)), y = TECH)) + 
  geom_line() + 
  ggtitle("The Tech Factor")

plt2 = ggplot(df, aes(x = as.Date(rownames(df)), y = OIL)) + 
  geom_line() + 
  ggtitle("The Tech Factor")

plt3 = ggplot(df, aes(x = as.Date(rownames(df)), y = OTHER)) + 
  geom_line() + 
  ggtitle("The Tech Factor")

grid.arrange(plt1, plt2, plt3)
```

![](Factor_Models_Eric_files/figure-html/unnamed-chunk-8-1.png) 

```r
# Industry Factor Correlations ####
# cov.ind = 
```








