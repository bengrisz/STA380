# homework 1

# PART 1: Exploratory Analysis

Votes = read.csv('../data/georgia2000.csv', header=TRUE)

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

Votes$difference = Votes$ballots - Votes$votes

summary(Votes)




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



