library(ade4)
#now load a table that contains only the variables of interest (no other info). If there are missing values, you can estimate the median for that behavior, and insert it for the missing value (but be sure to make to note this)
#below, the data file is named “data”
dudi.pca(data,scale=TRUE,)->pca.oft
#in the above, you scale=TRUE because variables are measured on different scales. This means you are using a correlation rather than a covariance matrix
summary(pca.oft$c1)
#this gives you the loadings
summary(pca.oft$eig) #gives you eigenvalues, which you can use to estimate prop variance explained by each PC
pc<-pca.oft$l1
#this gives you the composite variables, you can then append/paste them to the data file with the other data (but make sure they are all in the same order or just cbind to your dataframe