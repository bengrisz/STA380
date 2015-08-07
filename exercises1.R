# homework 1

# PART 1: Exploratory Analysis

Votes = read.csv('georgia2000.csv', header=TRUE)

summary(Votes)

# county - self-explanatory
# ballots - no. ballots cast
# votes - no. votes recorded
# equip - voting equipment (LEVER, OPTICAL, PAPER, PUNCH)
# poor - 1 if >25% population below 1.5x federal poverty line, 0 otherwise
# perAA - percentage of residents African American
# urban - 1 if predominantly urban, 0 otherwise
# atlanta - 1 if in Atlanta, 0 otherwise
# gore - no. votes for Gore
# bush - no. votes for Bush

### Focus: vote undercount, difference between ballots and legal votes recorded
###     -> use plots, tables, and numerical summaries
### 2 things:
### 1) whether voting with certain kinds of equip leads to higher undercount
### 2) whether these effects have disparate impact on poor/minority communities

Votes$undercount = Votes$ballots - Votes$votes
summary(Votes)

boxplot(undercount~equip, data=Votes)

lm.fit = glm(undercount~equip, data=Votes)
summary(lm.fit)

#######################
# PART 2: Bootstrapping

library(fImport)

# download 5 years of daily data on the following ETFs:
# SPY, TLT, LQD, EEM, VNQ
# - explore data and understand risk/return properties of these assets

# Consider 3 portfolios:
#   - Even split: 20% assets in each
#   - Safer than even split (min. 3 ETFs)
#   - More aggressive (higher returns, more risky)

# Consider potential investment of $100,000 in one of the above portfolios
# Write a brief report:
# - give evidence to support risk/return properties the 5 ETFs
# - outlines choice of 'safe' and 'aggressive' portfolios
# - bootstrap method to estimate 5%-value-at-risk over 4 weeks for each porfolio
# - compare all your results to allow intelligent decision for client

# ----- Assume that portfolio rebalances every day

library(mosaic)
library(fImport)
library(foreach)

mystocks = c("SPY", "TLT", "LQD", "EEM", "VNQ")
myprices = yahooSeries(mystocks, from='2010-08-05', to='2015-08-05')

YahooPricesToReturns = function(series) {
  mycols = grep('Adj.Close', colnames(series))
  closingprice = series[,mycols]
  N = nrow(closingprice)
  percentreturn = as.data.frame(closingprice[2:N,]) / as.data.frame(closingprice[1:(N-1),]) - 1
  mynames = strsplit(colnames(percentreturn), '.', fixed=TRUE)
  mynames = lapply(mynames, function(x) return(paste0(x[1], ".PctReturn")))
  colnames(percentreturn) = mynames
  as.matrix(na.omit(percentreturn))
}

myreturns = YahooPricesToReturns(myprices)
pairs(myreturns)
cor(myreturns)

#--------------
myexpected = c(mean(myreturns[,1]),
               mean(myreturns[,2]),
               mean(myreturns[,3]),
               mean(myreturns[,4]),
               mean(myreturns[,5]))

riskfree = myexpected[2]

myexcess = matrix(0, nrow(myreturns), ncol(myreturns))

for(i in 1:nrow(myreturns)) {
  for (j in 1:ncol(myreturns)) {
    myexcess[i,j] = myreturns[i,j] - riskfree
  }
}

mybeta2 = cov(myexcess[,2], myexcess[,1])/var(myexcess[,1])
mybeta2
mybeta3 = cov(myexcess[,3], myexcess[,1])/var(myexcess[,1])
mybeta3
mybeta4 = cov(myexcess[,4], myexcess[,1])/var(myexcess[,1])
mybeta4
mybeta5 = cov(myexcess[,5], myexcess[,1])/var(myexcess[,1])
mybeta5

#--------------
totalwealth = 100000

weights1 = c(0.2, 0.2, 0.2, 0.2, 0.2)

holdings = weights1 * totalwealth
n_days = 20

sim1 = foreach(i=1:5000, .combine='rbind') %do% {
  totalwealth = 100000
  weights = c(0.2, 0.2, 0.2, 0.2, 0.2)
  holdings = weights * totalwealth
  wealthtracker = rep(0, n_days)
  for(today in 1:n_days) {
    return.today = resample(myreturns, 1, orig.ids=FALSE)
    holdings = holdings + holdings*return.today
    totalwealth = sum(holdings)
    wealthtracker[today] = totalwealth
    holdings = weights * totalwealth
  }
  wealthtracker
}

head(sim1)
hist(sim1[,n_days], 25)

# Profit/loss
hist(sim1[,n_days]- 100000)

# Calculate 5% value at risk
quantile(sim1[,n_days], 0.05) - 100000

## uncorrelated
## LQD & EEM
## LQD & VNQ
## LQD & SPY
## TLT & EEM

## positive
## SPY & EEM
## SPY & VNQ
## TLT & LQD
## EEM & VNQ

## negative
## SPY & TLT
## TLT & EEM (weak)



############################
# PART 3: Clustering and PCA

## data in wine.csv
# 6500 bottles of wine
# 11 chemical properties
# whether wine is red or white
# quality of wine on scale of 1-10

# Run PCA
# Run clustering
# Summarize results

# - Which dimensionality reduction technique makes sense?
# - Convince that model predicts red/white based on "unsupervised" chem. properties
# - Does this technique also predict the quality of the wine?
library(cluster)
library(fpc)

Wine = read.csv('wine.csv', header=TRUE)

wine_chem = Wine[,1:11]
wine_scaled = scale(wine_chem, center=TRUE, scale=TRUE)
mu = attr(wine_scaled,"scaled:center")
sigma = attr(wine_scaled,"scaled:scale")

summary(wine_scaled)


pc.wine = prcomp(wine_scaled, scale=TRUE)
summary(pc.wine)
plot(pc.wine)


loadings = pc.wine$rotation
scores = pc.wine$x
qplot(scores[,1], scores[,2], color=Wine$color, xlab='Component 1', ylab='Component 2')
qplot(scores[,1], scores[,2], color=Wine$quality, xlab='Component 1', ylab='Component 2')

o1 = order(loadings[,1])
colnames(wine_scaled)[head(o1,4)]
colnames(wine_scaled)[tail(o1,4)]

o2 = order(loadings[,2])
colnames(wine_scaled)[head(o2,4)]
colnames(wine_scaled)[tail(o2,4)]

# k-means
cluster_chem = kmeans(wine_scaled, centers = 2, n=50)
plotcluster(wine_scaled, cluster_chem$cluster)
qplot(color, density, data=Wine, color=factor(cluster_chem$cluster))
table(Wine$color,cluster_chem$cluster)

# Here we can see that we can pretty accurately predict whether the wine
# is red or white based on just this k-means clustering.

cluster_chem = kmeans(wine_scaled, centers = 10, n=50)
qplot(color, quality, data=Wine, color=factor(cluster_chem$cluster))
plotcluster(wine_scaled, cluster_chem$cluster)
table(Wine$quality,cluster_chem$cluster)

#############################
# PART 4: Market Segmentation

## data in social_marketing.csv

# data from Twitter followers of NutrientH20
# they want to understand social-media audience better -> hone messaging

# Data Collection:
# - take random sample of brand's followers
# - take every follower's tweets over 7-day period
# - every tweet categorized using scheme of 36 categories
# - categories are broad areas of interest (sports, politics, etc.)
# - done by hired annotators (unreliable)

# Dataset:
# - each row is a user, # posts falling into each category
# - includes 'spam' and 'adult'
# - includes 'uncategorized' and 'chatter'

# Objective:
# - analyze as you see fit
# - create report identifying any interesting market segments that stand out
# - How to define market segments?
#     - correlated interests
#     - clusters
#     - latent factors
# - come up with general, interesting insights about the followers

twitter.data = read.csv('social_marketing.csv', header=TRUE)
twitter.data = twitter.data[,2:37]
twitter.scaled = scale(twitter.data, center=TRUE, scale=TRUE)




# k-means
cluster_cat = kmeans(twitter.scaled, centers = 2, n=50)
#plotcluster(twitter.scaled, cluster_cat$cluster)
qplot(religion, sports_fandom, data=twitter.data, color=factor(cluster_cat$cluster))
qplot(parenting, family, data=twitter.data, color=factor(cluster_cat$cluster))

#table(Wine$color,cluster_chat$cluster)


cluster_cat = kmeans(twitter.scaled, centers = 10, n=50)
qplot(religion, sports_fandom, data=twitter.scaled, color=factor(cluster_cat$cluster))
qplot(parenting, family, data=twitter.data, color=factor(cluster_cat$cluster))
plotcluster(twitter.scaled, cluster_cat$cluster)

pc.twitter = prcomp(twitter.scaled, scale=TRUE)

loadings = pc.twitter$rotation
scores = pc.twitter$x
# COLOR BY K-MEANS CLUSTER
cluster_cat = kmeans(twitter.scaled, centers = 2, n=50)
qplot(scores[,1], scores[,2], color=cluster_cat$cluster, xlab='Component 1', ylab='Component 2')
cluster_cat = kmeans(twitter.scaled, centers = 3, n=50)
qplot(scores[,1], scores[,2], color=cluster_cat$cluster, xlab='Component 1', ylab='Component 2')
cluster_cat = kmeans(twitter.scaled, centers = 4, n=50)
qplot(scores[,1], scores[,2], color=cluster_cat$cluster, xlab='Component 1', ylab='Component 2')
cluster_cat = kmeans(twitter.scaled, centers = 5, n=50)
qplot(scores[,1], scores[,2], color=cluster_cat$cluster, xlab='Component 1', ylab='Component 2')


o1 = order(loadings[,1])
colnames(twitter.scaled)[head(o1,15)]
colnames(twitter.scaled)[tail(o1,15)]

o2 = order(loadings[,2])
colnames(twitter.scaled)[head(o2,15)]
colnames(twitter.scaled)[tail(o2,15)]
