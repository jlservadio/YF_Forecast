

rm(list = ls())

setwd('YF/Forecast')

source('Code/MakeLag.R')
source('Code/makeROC.R')

dat = read.csv('YFForec_Data.csv')

library(parallel)
library(tictoc)

dat$WWF_MHTNUM[which(!(dat$WWF_MHTNUM %in% c(1, 7)))] = 10
dat$WWF_MHTNUM = as.factor(dat$WWF_MHTNUM)


dat$Cases.bin = 1 * (dat$Cases > 0)

num.cores = 20


#
# Splitting training and testing data sets
#

train = dat[which(dat$Year <= 2013), ]
test = dat[which(dat$Year > 2013), ]
test = test[test$Year < 2017, ]
test = test[-which(test$Year == 2016 & test$Month == 12), ]


table(train$Month[train$Inc > 0])
table(test$Month[test$Inc > 0])

train$Month[which(train$Month %in% c(6:11))] = 6; train$Month = as.factor(train$Month)
test$Month[which(test$Month %in% c(6:11))] = 6; test$Month = as.factor(test$Month)


#
# Maximum: Lag 2
# 

lag.per = 2


train$Tmp.L = make.lag(train, 'max_Tmp', 'GID_2', 'Week', lag.per)
train$Tmp.L2 = make.lag(train, 'max_Tmp2', 'GID_2', 'Week', lag.per)
train$Rain.L = make.lag(train, 'max_Rain', 'GID_2', 'Week', lag.per)
train$Rain.L2 = make.lag(train, 'max_Rain2', 'GID_2', 'Week', lag.per)
train$Hum.L = make.lag(train, 'max_Hum', 'GID_2', 'Week', lag.per)
train$Hum.L2 = make.lag(train, 'max_Hum2', 'GID_2', 'Week', lag.per)
train$Cases.bin.L = make.lag(train, 'Cases.bin', 'GID_2', 'Week', lag.per)
train$Incidence.L = make.lag(train, 'Inc', 'GID_2', 'Week', lag.per)


test$Tmp.L = make.lag(test, 'max_Tmp', 'GID_2', 'Week', lag.per)
test$Tmp.L2 = make.lag(test, 'max_Tmp2', 'GID_2', 'Week', lag.per)
test$Rain.L = make.lag(test, 'max_Rain', 'GID_2', 'Week', lag.per)
test$Rain.L2 = make.lag(test, 'max_Rain2', 'GID_2', 'Week', lag.per)
test$Hum.L = make.lag(test, 'max_Hum', 'GID_2', 'Week', lag.per)
test$Hum.L2 = make.lag(test, 'max_Hum2', 'GID_2', 'Week', lag.per)
test$Cases.bin.L = make.lag(test, 'Cases.bin', 'GID_2', 'Week', lag.per)
test$Incidence.L = make.lag(test, 'Inc', 'GID_2', 'Week', lag.per)

train2 = train
test2 = test

train2$Keep = make.lag(train2, 'Cases.bin', 'GID_2', 'Week', 8)
test2$Keep = make.lag(test2, 'Cases.bin', 'GID_2', 'Week', 8)

train2 = train2[!is.na(train2$Keep), ]
test2 = test2[!is.na(test2$Keep), ]


train2 = train2[ , c('Cases.bin', 'Inc', 'Tmp.L', 'Tmp.L2', 'Rain.L', 'Rain.L2', 'Hum.L', 'Hum.L2', 
	'Month', 'Cases.bin.L', 'Incidence.L', 'MEAN.DrDens', 'WWF_MHTNUM', 'NDVI', 'Elevation_m'), ]
test2 = test2[ , c('Cases.bin', 'Inc', 'Tmp.L', 'Tmp.L2', 'Rain.L', 'Rain.L2', 'Hum.L', 'Hum.L2', 
	'Month', 'Cases.bin.L', 'Incidence.L', 'MEAN.DrDens', 'WWF_MHTNUM', 'NDVI', 'Elevation_m'), ]

vars.to.include = c(3:ncol(train2))
covars = matrix(NA, nrow = (2^length(vars.to.include))-1, ncol = length(vars.to.include))
for (i in 1:nrow(covars)) {
	covars[i, ] = as.numeric(intToBits(i))[1:ncol(covars)]
}
for (i in 1:ncol(covars)) {
	covars[ , i] = covars[ , i] * vars.to.include[i]
}


covars = covars[-which(covars[ , 2] > 0 & covars[ , 1] == 0), ]
covars = covars[-which(covars[ , 4] > 0 & covars[ , 3] == 0), ]
covars = covars[-which(covars[ , 6] > 0 & covars[ , 5] == 0), ]
covars = covars[-which(covars[ , 8] > 0 & covars[ , 9] > 0), ]


keep.examp = c(1:2) 
while(prod(apply(covars[keep.examp, ], 2, sum)) == 0) {
	keep.examp = c(keep.examp, sample(c(1:nrow(covars)), 1))
	keep.examp = keep.examp[order(keep.examp)]
	keep.examp = keep.examp[!duplicated(keep.examp)]
}
while(length(keep.examp) < 160) {
	keep.examp = c(keep.examp, sample(c(1:nrow(covars)), 1), nrow(covars))
	keep.examp = keep.examp[order(keep.examp)]
	keep.examp = keep.examp[!duplicated(keep.examp)]	
}

length(keep.examp)


tic()
all.res = list()

all.res = mclapply(c(1:nrow(covars)), function(x) {
	
	cur.res = list()
	cur.res[[1]] = covars[x, ]
	
	m1 = tryCatch( { glm(Cases.bin ~ ., data = train2[ , c(1, covars[x, ])], 
		family = binomial(link = logit)) }, error = function(e) { 
		glm(Cases.bin ~ 1, data = train2, family = binomial(link = logit)) } )

	m2 = tryCatch( { glm(Inc ~ ., data = train2[train2$Inc > 0, c(2, covars[x, ])], 
		family = Gamma(link = log)) }, error = function(e) { 
		glm(Inc ~ 1, data = train2[which(train2$Inc > 0), ], family = Gamma(link = log)) } )
		
	pred = plogis(predict(m1, test2))
	roc = make.roc(pred[!is.na(pred)], test2$Cases.bin[!is.na(pred)])
	gv = test.g(m2, test2)
	
	cur.res[[2]] = roc$auc
	cur.res[[3]] = gv['SAE']
	
	names(cur.res) = c('covs', 'AUC', 'SAE')
	all.res[[x]] = cur.res
	
	}, mc.cores = num.cores)
toc()

all.res.lag2 = all.res







#
# Maximum: Lag 3
# 

lag.per = 3


train$Tmp.L = make.lag(train, 'max_Tmp', 'GID_2', 'Week', lag.per)
train$Tmp.L2 = make.lag(train, 'max_Tmp2', 'GID_2', 'Week', lag.per)
train$Rain.L = make.lag(train, 'max_Rain', 'GID_2', 'Week', lag.per)
train$Rain.L2 = make.lag(train, 'max_Rain2', 'GID_2', 'Week', lag.per)
train$Hum.L = make.lag(train, 'max_Hum', 'GID_2', 'Week', lag.per)
train$Hum.L2 = make.lag(train, 'max_Hum2', 'GID_2', 'Week', lag.per)
train$Cases.bin.L = make.lag(train, 'Cases.bin', 'GID_2', 'Week', lag.per)
train$Incidence.L = make.lag(train, 'Inc', 'GID_2', 'Week', lag.per)


test$Tmp.L = make.lag(test, 'max_Tmp', 'GID_2', 'Week', lag.per)
test$Tmp.L2 = make.lag(test, 'max_Tmp2', 'GID_2', 'Week', lag.per)
test$Rain.L = make.lag(test, 'max_Rain', 'GID_2', 'Week', lag.per)
test$Rain.L2 = make.lag(test, 'max_Rain2', 'GID_2', 'Week', lag.per)
test$Hum.L = make.lag(test, 'max_Hum', 'GID_2', 'Week', lag.per)
test$Hum.L2 = make.lag(test, 'max_Hum2', 'GID_2', 'Week', lag.per)
test$Cases.bin.L = make.lag(test, 'Cases.bin', 'GID_2', 'Week', lag.per)
test$Incidence.L = make.lag(test, 'Inc', 'GID_2', 'Week', lag.per)


train2 = train
test2 = test

train2$Keep = make.lag(train2, 'Cases.bin', 'GID_2', 'Week', 8)
test2$Keep = make.lag(test2, 'Cases.bin', 'GID_2', 'Week', 8)

train2 = train2[!is.na(train2$Keep), ]
test2 = test2[!is.na(test2$Keep), ]


train2 = train2[ , c('Cases.bin', 'Inc', 'Tmp.L', 'Tmp.L2', 'Rain.L', 'Rain.L2', 'Hum.L', 'Hum.L2', 
	'Month', 'Cases.bin.L', 'Incidence.L', 'MEAN.DrDens', 'WWF_MHTNUM', 'NDVI', 'Elevation_m'), ]
test2 = test2[ , c('Cases.bin', 'Inc', 'Tmp.L', 'Tmp.L2', 'Rain.L', 'Rain.L2', 'Hum.L', 'Hum.L2', 
	'Month', 'Cases.bin.L', 'Incidence.L', 'MEAN.DrDens', 'WWF_MHTNUM', 'NDVI', 'Elevation_m'), ]

vars.to.include = c(3:ncol(train2))
covars = matrix(NA, nrow = (2^length(vars.to.include))-1, ncol = length(vars.to.include))
for (i in 1:nrow(covars)) {
	covars[i, ] = as.numeric(intToBits(i))[1:ncol(covars)]
}
for (i in 1:ncol(covars)) {
	covars[ , i] = covars[ , i] * vars.to.include[i]
}


covars = covars[-which(covars[ , 2] > 0 & covars[ , 1] == 0), ]
covars = covars[-which(covars[ , 4] > 0 & covars[ , 3] == 0), ]
covars = covars[-which(covars[ , 6] > 0 & covars[ , 5] == 0), ]
covars = covars[-which(covars[ , 8] > 0 & covars[ , 9] > 0), ]


tic()
all.res = list()

all.res = mclapply(c(1:nrow(covars)), function(x) {
		
	cur.res = list()
	cur.res[[1]] = covars[x, ]
	
		
	m1 = tryCatch( { glm(Cases.bin ~ ., data = train2[ , c(1, covars[x, ])], 
		family = binomial(link = logit)) }, error = function(e) { 
		glm(Cases.bin ~ 1, data = train2, family = binomial(link = logit)) } )

	m2 = tryCatch( { glm(Inc ~ ., data = train2[train2$Inc > 0, c(2, covars[x, ])], 
		family = Gamma(link = log)) }, error = function(e) { 
		glm(Inc ~ 1, data = train2[which(train2$Inc > 0), ], family = Gamma(link = log)) } )

	pred = plogis(predict(m1, test2))
	roc = make.roc(pred[!is.na(pred)], test2$Cases.bin[!is.na(pred)])
	gv = test.g(m2, test2)
	
	cur.res[[2]] = roc$auc
	cur.res[[3]] = gv['SAE']
	
	names(cur.res) = c('covs', 'AUC', 'SAE')
	all.res[[x]] = cur.res
	
	}, mc.cores = num.cores)
toc()

all.res.lag3 = all.res





#
# Maximum: Lag 4
# 

lag.per = 4


train$Tmp.L = make.lag(train, 'max_Tmp', 'GID_2', 'Week', lag.per)
train$Tmp.L2 = make.lag(train, 'max_Tmp2', 'GID_2', 'Week', lag.per)
train$Rain.L = make.lag(train, 'max_Rain', 'GID_2', 'Week', lag.per)
train$Rain.L2 = make.lag(train, 'max_Rain2', 'GID_2', 'Week', lag.per)
train$Hum.L = make.lag(train, 'max_Hum', 'GID_2', 'Week', lag.per)
train$Hum.L2 = make.lag(train, 'max_Hum2', 'GID_2', 'Week', lag.per)
train$Cases.bin.L = make.lag(train, 'Cases.bin', 'GID_2', 'Week', lag.per)
train$Incidence.L = make.lag(train, 'Inc', 'GID_2', 'Week', lag.per)


test$Tmp.L = make.lag(test, 'max_Tmp', 'GID_2', 'Week', lag.per)
test$Tmp.L2 = make.lag(test, 'max_Tmp2', 'GID_2', 'Week', lag.per)
test$Rain.L = make.lag(test, 'max_Rain', 'GID_2', 'Week', lag.per)
test$Rain.L2 = make.lag(test, 'max_Rain2', 'GID_2', 'Week', lag.per)
test$Hum.L = make.lag(test, 'max_Hum', 'GID_2', 'Week', lag.per)
test$Hum.L2 = make.lag(test, 'max_Hum2', 'GID_2', 'Week', lag.per)
test$Cases.bin.L = make.lag(test, 'Cases.bin', 'GID_2', 'Week', lag.per)
test$Incidence.L = make.lag(test, 'Inc', 'GID_2', 'Week', lag.per)


train2 = train
test2 = test

train2$Keep = make.lag(train2, 'Cases.bin', 'GID_2', 'Week', 8)
test2$Keep = make.lag(test2, 'Cases.bin', 'GID_2', 'Week', 8)

train2 = train2[!is.na(train2$Keep), ]
test2 = test2[!is.na(test2$Keep), ]


train2 = train2[ , c('Cases.bin', 'Inc', 'Tmp.L', 'Tmp.L2', 'Rain.L', 'Rain.L2', 'Hum.L', 'Hum.L2', 
	'Month', 'Cases.bin.L', 'Incidence.L', 'MEAN.DrDens', 'WWF_MHTNUM', 'NDVI', 'Elevation_m'), ]
test2 = test2[ , c('Cases.bin', 'Inc', 'Tmp.L', 'Tmp.L2', 'Rain.L', 'Rain.L2', 'Hum.L', 'Hum.L2', 
	'Month', 'Cases.bin.L', 'Incidence.L', 'MEAN.DrDens', 'WWF_MHTNUM', 'NDVI', 'Elevation_m'), ]

vars.to.include = c(3:ncol(train2))
covars = matrix(NA, nrow = (2^length(vars.to.include))-1, ncol = length(vars.to.include))
for (i in 1:nrow(covars)) {
	covars[i, ] = as.numeric(intToBits(i))[1:ncol(covars)]
}
for (i in 1:ncol(covars)) {
	covars[ , i] = covars[ , i] * vars.to.include[i]
}


covars = covars[-which(covars[ , 2] > 0 & covars[ , 1] == 0), ]
covars = covars[-which(covars[ , 4] > 0 & covars[ , 3] == 0), ]
covars = covars[-which(covars[ , 6] > 0 & covars[ , 5] == 0), ]
covars = covars[-which(covars[ , 8] > 0 & covars[ , 9] > 0), ]


tic()
all.res = list()

all.res = mclapply(c(1:nrow(covars)), function(x) {		
	
	cur.res = list()
	cur.res[[1]] = covars[x, ]
	
	m1 = tryCatch( { glm(Cases.bin ~ ., data = train2[ , c(1, covars[x, ])], 
		family = binomial(link = logit)) }, error = function(e) { 
		glm(Cases.bin ~ 1, data = train2, family = binomial(link = logit)) } )

	m2 = tryCatch( { glm(Inc ~ ., data = train2[train2$Inc > 0, c(2, covars[x, ])], 
		family = Gamma(link = log)) }, error = function(e) { 
		glm(Inc ~ 1, data = train2[which(train2$Inc > 0), ], family = Gamma(link = log)) } )
		
	pred = plogis(predict(m1, test2))
	roc = make.roc(pred[!is.na(pred)], test2$Cases.bin[!is.na(pred)])
	gv = test.g(m2, test2)
	
	cur.res[[2]] = roc$auc
	cur.res[[3]] = gv['SAE']
	
	names(cur.res) = c('covs', 'AUC', 'SAE')
	all.res[[x]] = cur.res
	
	}, mc.cores = num.cores)
toc()

all.res.lag4 = all.res






#
# Maximum: Lag 5
# 

lag.per = 5


train$Tmp.L = make.lag(train, 'max_Tmp', 'GID_2', 'Week', lag.per)
train$Tmp.L2 = make.lag(train, 'max_Tmp2', 'GID_2', 'Week', lag.per)
train$Rain.L = make.lag(train, 'max_Rain', 'GID_2', 'Week', lag.per)
train$Rain.L2 = make.lag(train, 'max_Rain2', 'GID_2', 'Week', lag.per)
train$Hum.L = make.lag(train, 'max_Hum', 'GID_2', 'Week', lag.per)
train$Hum.L2 = make.lag(train, 'max_Hum2', 'GID_2', 'Week', lag.per)
train$Cases.bin.L = make.lag(train, 'Cases.bin', 'GID_2', 'Week', lag.per)
train$Incidence.L = make.lag(train, 'Inc', 'GID_2', 'Week', lag.per)


test$Tmp.L = make.lag(test, 'max_Tmp', 'GID_2', 'Week', lag.per)
test$Tmp.L2 = make.lag(test, 'max_Tmp2', 'GID_2', 'Week', lag.per)
test$Rain.L = make.lag(test, 'max_Rain', 'GID_2', 'Week', lag.per)
test$Rain.L2 = make.lag(test, 'max_Rain2', 'GID_2', 'Week', lag.per)
test$Hum.L = make.lag(test, 'max_Hum', 'GID_2', 'Week', lag.per)
test$Hum.L2 = make.lag(test, 'max_Hum2', 'GID_2', 'Week', lag.per)
test$Cases.bin.L = make.lag(test, 'Cases.bin', 'GID_2', 'Week', lag.per)
test$Incidence.L = make.lag(test, 'Inc', 'GID_2', 'Week', lag.per)


train2 = train
test2 = test

train2$Keep = make.lag(train2, 'Cases.bin', 'GID_2', 'Week', 8)
test2$Keep = make.lag(test2, 'Cases.bin', 'GID_2', 'Week', 8)

train2 = train2[!is.na(train2$Keep), ]
test2 = test2[!is.na(test2$Keep), ]


train2 = train2[ , c('Cases.bin', 'Inc', 'Tmp.L', 'Tmp.L2', 'Rain.L', 'Rain.L2', 'Hum.L', 'Hum.L2', 
	'Month', 'Cases.bin.L', 'Incidence.L', 'MEAN.DrDens', 'WWF_MHTNUM', 'NDVI', 'Elevation_m'), ]
test2 = test2[ , c('Cases.bin', 'Inc', 'Tmp.L', 'Tmp.L2', 'Rain.L', 'Rain.L2', 'Hum.L', 'Hum.L2', 
	'Month', 'Cases.bin.L', 'Incidence.L', 'MEAN.DrDens', 'WWF_MHTNUM', 'NDVI', 'Elevation_m'), ]

vars.to.include = c(3:ncol(train2))
covars = matrix(NA, nrow = (2^length(vars.to.include))-1, ncol = length(vars.to.include))
for (i in 1:nrow(covars)) {
	covars[i, ] = as.numeric(intToBits(i))[1:ncol(covars)]
}
for (i in 1:ncol(covars)) {
	covars[ , i] = covars[ , i] * vars.to.include[i]
}


covars = covars[-which(covars[ , 2] > 0 & covars[ , 1] == 0), ]
covars = covars[-which(covars[ , 4] > 0 & covars[ , 3] == 0), ]
covars = covars[-which(covars[ , 6] > 0 & covars[ , 5] == 0), ]
covars = covars[-which(covars[ , 8] > 0 & covars[ , 9] > 0), ]


tic()
all.res = list()

all.res = mclapply(c(1:nrow(covars)), function(x) {
		
	cur.res = list()
	cur.res[[1]] = covars[x, ]
	
	m1 = tryCatch( { glm(Cases.bin ~ ., data = train2[ , c(1, covars[x, ])], 
		family = binomial(link = logit)) }, error = function(e) { 
		glm(Cases.bin ~ 1, data = train2, family = binomial(link = logit)) } )

	m2 = tryCatch( { glm(Inc ~ ., data = train2[train2$Inc > 0, c(2, covars[x, ])], 
		family = Gamma(link = log)) }, error = function(e) { 
		glm(Inc ~ 1, data = train2[which(train2$Inc > 0), ], family = Gamma(link = log)) } )
		
	pred = plogis(predict(m1, test2))
	roc = make.roc(pred[!is.na(pred)], test2$Cases.bin[!is.na(pred)])
	gv = test.g(m2, test2)
	
	cur.res[[2]] = roc$auc
	cur.res[[3]] = gv['SAE']
	
	names(cur.res) = c('covs', 'AUC', 'SAE')
	all.res[[x]] = cur.res
	
	}, mc.cores = num.cores)
toc()

all.res.lag5 = all.res






#
# Maximum: Lag 6
# 

lag.per = 6


train$Tmp.L = make.lag(train, 'max_Tmp', 'GID_2', 'Week', lag.per)
train$Tmp.L2 = make.lag(train, 'max_Tmp2', 'GID_2', 'Week', lag.per)
train$Rain.L = make.lag(train, 'max_Rain', 'GID_2', 'Week', lag.per)
train$Rain.L2 = make.lag(train, 'max_Rain2', 'GID_2', 'Week', lag.per)
train$Hum.L = make.lag(train, 'max_Hum', 'GID_2', 'Week', lag.per)
train$Hum.L2 = make.lag(train, 'max_Hum2', 'GID_2', 'Week', lag.per)
train$Cases.bin.L = make.lag(train, 'Cases.bin', 'GID_2', 'Week', lag.per)
train$Incidence.L = make.lag(train, 'Inc', 'GID_2', 'Week', lag.per)


test$Tmp.L = make.lag(test, 'max_Tmp', 'GID_2', 'Week', lag.per)
test$Tmp.L2 = make.lag(test, 'max_Tmp2', 'GID_2', 'Week', lag.per)
test$Rain.L = make.lag(test, 'max_Rain', 'GID_2', 'Week', lag.per)
test$Rain.L2 = make.lag(test, 'max_Rain2', 'GID_2', 'Week', lag.per)
test$Hum.L = make.lag(test, 'max_Hum', 'GID_2', 'Week', lag.per)
test$Hum.L2 = make.lag(test, 'max_Hum2', 'GID_2', 'Week', lag.per)
test$Cases.bin.L = make.lag(test, 'Cases.bin', 'GID_2', 'Week', lag.per)
test$Incidence.L = make.lag(test, 'Inc', 'GID_2', 'Week', lag.per)


train2 = train
test2 = test

train2$Keep = make.lag(train2, 'Cases.bin', 'GID_2', 'Week', 8)
test2$Keep = make.lag(test2, 'Cases.bin', 'GID_2', 'Week', 8)

train2 = train2[!is.na(train2$Keep), ]
test2 = test2[!is.na(test2$Keep), ]


train2 = train2[ , c('Cases.bin', 'Inc', 'Tmp.L', 'Tmp.L2', 'Rain.L', 'Rain.L2', 'Hum.L', 'Hum.L2', 
	'Month', 'Cases.bin.L', 'Incidence.L', 'MEAN.DrDens', 'WWF_MHTNUM', 'NDVI', 'Elevation_m'), ]
test2 = test2[ , c('Cases.bin', 'Inc', 'Tmp.L', 'Tmp.L2', 'Rain.L', 'Rain.L2', 'Hum.L', 'Hum.L2', 
	'Month', 'Cases.bin.L', 'Incidence.L', 'MEAN.DrDens', 'WWF_MHTNUM', 'NDVI', 'Elevation_m'), ]

vars.to.include = c(3:ncol(train2))
covars = matrix(NA, nrow = (2^length(vars.to.include))-1, ncol = length(vars.to.include))
for (i in 1:nrow(covars)) {
	covars[i, ] = as.numeric(intToBits(i))[1:ncol(covars)]
}
for (i in 1:ncol(covars)) {
	covars[ , i] = covars[ , i] * vars.to.include[i]
}


covars = covars[-which(covars[ , 2] > 0 & covars[ , 1] == 0), ]
covars = covars[-which(covars[ , 4] > 0 & covars[ , 3] == 0), ]
covars = covars[-which(covars[ , 6] > 0 & covars[ , 5] == 0), ]
covars = covars[-which(covars[ , 8] > 0 & covars[ , 9] > 0), ]


tic()
all.res = list()

all.res = mclapply(c(1:nrow(covars)), function(x) {
		
	cur.res = list()
	cur.res[[1]] = covars[x, ]
	
	m1 = tryCatch( { glm(Cases.bin ~ ., data = train2[ , c(1, covars[x, ])], 
		family = binomial(link = logit)) }, error = function(e) { 
		glm(Cases.bin ~ 1, data = train2, family = binomial(link = logit)) } )

	m2 = tryCatch( { glm(Inc ~ ., data = train2[train2$Inc > 0, c(2, covars[x, ])], 
		family = Gamma(link = log)) }, error = function(e) { 
		glm(Inc ~ 1, data = train2[which(train2$Inc > 0), ], family = Gamma(link = log)) } )
		
	pred = plogis(predict(m1, test2))
	roc = make.roc(pred[!is.na(pred)], test2$Cases.bin[!is.na(pred)])
	gv = test.g(m2, test2)
	
	cur.res[[2]] = roc$auc
	cur.res[[3]] = gv['SAE']
	
	names(cur.res) = c('covs', 'AUC', 'SAE')
	all.res[[x]] = cur.res
	
	}, mc.cores = num.cores)
toc()

all.res.lag6 = all.res







#
# Maximum: Lag 7
# 

lag.per = 7


train$Tmp.L = make.lag(train, 'max_Tmp', 'GID_2', 'Week', lag.per)
train$Tmp.L2 = make.lag(train, 'max_Tmp2', 'GID_2', 'Week', lag.per)
train$Rain.L = make.lag(train, 'max_Rain', 'GID_2', 'Week', lag.per)
train$Rain.L2 = make.lag(train, 'max_Rain2', 'GID_2', 'Week', lag.per)
train$Hum.L = make.lag(train, 'max_Hum', 'GID_2', 'Week', lag.per)
train$Hum.L2 = make.lag(train, 'max_Hum2', 'GID_2', 'Week', lag.per)
train$Cases.bin.L = make.lag(train, 'Cases.bin', 'GID_2', 'Week', lag.per)
train$Incidence.L = make.lag(train, 'Inc', 'GID_2', 'Week', lag.per)


test$Tmp.L = make.lag(test, 'max_Tmp', 'GID_2', 'Week', lag.per)
test$Tmp.L2 = make.lag(test, 'max_Tmp2', 'GID_2', 'Week', lag.per)
test$Rain.L = make.lag(test, 'max_Rain', 'GID_2', 'Week', lag.per)
test$Rain.L2 = make.lag(test, 'max_Rain2', 'GID_2', 'Week', lag.per)
test$Hum.L = make.lag(test, 'max_Hum', 'GID_2', 'Week', lag.per)
test$Hum.L2 = make.lag(test, 'max_Hum2', 'GID_2', 'Week', lag.per)
test$Cases.bin.L = make.lag(test, 'Cases.bin', 'GID_2', 'Week', lag.per)
test$Incidence.L = make.lag(test, 'Inc', 'GID_2', 'Week', lag.per)


train2 = train
test2 = test

train2$Keep = make.lag(train2, 'Cases.bin', 'GID_2', 'Week', 8)
test2$Keep = make.lag(test2, 'Cases.bin', 'GID_2', 'Week', 8)

train2 = train2[!is.na(train2$Keep), ]
test2 = test2[!is.na(test2$Keep), ]


train2 = train2[ , c('Cases.bin', 'Inc', 'Tmp.L', 'Tmp.L2', 'Rain.L', 'Rain.L2', 'Hum.L', 'Hum.L2', 
	'Month', 'Cases.bin.L', 'Incidence.L', 'MEAN.DrDens', 'WWF_MHTNUM', 'NDVI', 'Elevation_m'), ]
test2 = test2[ , c('Cases.bin', 'Inc', 'Tmp.L', 'Tmp.L2', 'Rain.L', 'Rain.L2', 'Hum.L', 'Hum.L2', 
	'Month', 'Cases.bin.L', 'Incidence.L', 'MEAN.DrDens', 'WWF_MHTNUM', 'NDVI', 'Elevation_m'), ]

vars.to.include = c(3:ncol(train2))
covars = matrix(NA, nrow = (2^length(vars.to.include))-1, ncol = length(vars.to.include))
for (i in 1:nrow(covars)) {
	covars[i, ] = as.numeric(intToBits(i))[1:ncol(covars)]
}
for (i in 1:ncol(covars)) {
	covars[ , i] = covars[ , i] * vars.to.include[i]
}


covars = covars[-which(covars[ , 2] > 0 & covars[ , 1] == 0), ]
covars = covars[-which(covars[ , 4] > 0 & covars[ , 3] == 0), ]
covars = covars[-which(covars[ , 6] > 0 & covars[ , 5] == 0), ]
covars = covars[-which(covars[ , 8] > 0 & covars[ , 9] > 0), ]


tic()
all.res = list()

all.res = mclapply(c(1:nrow(covars)), function(x) {
	
	cur.res = list()
	cur.res[[1]] = covars[x, ]
	
	m1 = tryCatch( { glm(Cases.bin ~ ., data = train2[ , c(1, covars[x, ])], 
		family = binomial(link = logit)) }, error = function(e) { 
		glm(Cases.bin ~ 1, data = train2, family = binomial(link = logit)) } )

	m2 = tryCatch( { glm(Inc ~ ., data = train2[train2$Inc > 0, c(2, covars[x, ])], 
		family = Gamma(link = log)) }, error = function(e) { 
		glm(Inc ~ 1, data = train2[which(train2$Inc > 0), ], family = Gamma(link = log)) } )
		
	pred = plogis(predict(m1, test2))
	roc = make.roc(pred[!is.na(pred)], test2$Cases.bin[!is.na(pred)])
	gv = test.g(m2, test2)
	
	cur.res[[2]] = roc$auc
	cur.res[[3]] = gv['SAE']
	
	names(cur.res) = c('covs', 'AUC', 'SAE')
	all.res[[x]] = cur.res
	
	}, mc.cores = num.cores)
toc()

all.res.lag7 = all.res






#
# Maximum: Lag 8
# 

lag.per = 8


train$Tmp.L = make.lag(train, 'max_Tmp', 'GID_2', 'Week', lag.per)
train$Tmp.L2 = make.lag(train, 'max_Tmp2', 'GID_2', 'Week', lag.per)
train$Rain.L = make.lag(train, 'max_Rain', 'GID_2', 'Week', lag.per)
train$Rain.L2 = make.lag(train, 'max_Rain2', 'GID_2', 'Week', lag.per)
train$Hum.L = make.lag(train, 'max_Hum', 'GID_2', 'Week', lag.per)
train$Hum.L2 = make.lag(train, 'max_Hum2', 'GID_2', 'Week', lag.per)
train$Cases.bin.L = make.lag(train, 'Cases.bin', 'GID_2', 'Week', lag.per)
train$Incidence.L = make.lag(train, 'Inc', 'GID_2', 'Week', lag.per)


test$Tmp.L = make.lag(test, 'max_Tmp', 'GID_2', 'Week', lag.per)
test$Tmp.L2 = make.lag(test, 'max_Tmp2', 'GID_2', 'Week', lag.per)
test$Rain.L = make.lag(test, 'max_Rain', 'GID_2', 'Week', lag.per)
test$Rain.L2 = make.lag(test, 'max_Rain2', 'GID_2', 'Week', lag.per)
test$Hum.L = make.lag(test, 'max_Hum', 'GID_2', 'Week', lag.per)
test$Hum.L2 = make.lag(test, 'max_Hum2', 'GID_2', 'Week', lag.per)
test$Cases.bin.L = make.lag(test, 'Cases.bin', 'GID_2', 'Week', lag.per)
test$Incidence.L = make.lag(test, 'Inc', 'GID_2', 'Week', lag.per)


train2 = train
test2 = test

train2$Keep = make.lag(train2, 'Cases.bin', 'GID_2', 'Week', 8)
test2$Keep = make.lag(test2, 'Cases.bin', 'GID_2', 'Week', 8)

train2 = train2[!is.na(train2$Keep), ]
test2 = test2[!is.na(test2$Keep), ]


train2 = train2[ , c('Cases.bin', 'Inc', 'Tmp.L', 'Tmp.L2', 'Rain.L', 'Rain.L2', 'Hum.L', 'Hum.L2', 
	'Month', 'Cases.bin.L', 'Incidence.L', 'MEAN.DrDens', 'WWF_MHTNUM', 'NDVI', 'Elevation_m'), ]
test2 = test2[ , c('Cases.bin', 'Inc', 'Tmp.L', 'Tmp.L2', 'Rain.L', 'Rain.L2', 'Hum.L', 'Hum.L2', 
	'Month', 'Cases.bin.L', 'Incidence.L', 'MEAN.DrDens', 'WWF_MHTNUM', 'NDVI', 'Elevation_m'), ]

vars.to.include = c(3:ncol(train2))
covars = matrix(NA, nrow = (2^length(vars.to.include))-1, ncol = length(vars.to.include))
for (i in 1:nrow(covars)) {
	covars[i, ] = as.numeric(intToBits(i))[1:ncol(covars)]
}
for (i in 1:ncol(covars)) {
	covars[ , i] = covars[ , i] * vars.to.include[i]
}


covars = covars[-which(covars[ , 2] > 0 & covars[ , 1] == 0), ]
covars = covars[-which(covars[ , 4] > 0 & covars[ , 3] == 0), ]
covars = covars[-which(covars[ , 6] > 0 & covars[ , 5] == 0), ]
covars = covars[-which(covars[ , 8] > 0 & covars[ , 9] > 0), ]


tic()
all.res = list()

all.res = mclapply(c(1:nrow(covars)), function(x) {
	
	cur.res = list()
	cur.res[[1]] = covars[x, ]
	
	m1 = tryCatch( { glm(Cases.bin ~ ., data = train2[ , c(1, covars[x, ])], 
		family = binomial(link = logit)) }, error = function(e) { 
		glm(Cases.bin ~ 1, data = train2, family = binomial(link = logit)) } )

	m2 = tryCatch( { glm(Inc ~ ., data = train2[train2$Inc > 0, c(2, covars[x, ])], 
		family = Gamma(link = log)) }, error = function(e) { 
		glm(Inc ~ 1, data = train2[which(train2$Inc > 0), ], family = Gamma(link = log)) } )
		
	pred = plogis(predict(m1, test2))
	roc = make.roc(pred[!is.na(pred)], test2$Cases.bin[!is.na(pred)])
	gv = test.g(m2, test2)
	
	cur.res[[2]] = roc$auc
	cur.res[[3]] = gv['SAE']
	
	names(cur.res) = c('covs', 'AUC', 'SAE')
	all.res[[x]] = cur.res
	
	}, mc.cores = num.cores)
toc()

all.res.lag8 = all.res


for (i in length(all.res.lag2):1) {
	if (is.null(all.res.lag2[[i]])) { all.res.lag2 = all.res.lag2[-i] } 
	if (is.null(all.res.lag3[[i]])) { all.res.lag3 = all.res.lag3[-i] } 
	if (is.null(all.res.lag4[[i]])) { all.res.lag4 = all.res.lag4[-i] } 
	if (is.null(all.res.lag5[[i]])) { all.res.lag5 = all.res.lag5[-i] } 
	if (is.null(all.res.lag6[[i]])) { all.res.lag6 = all.res.lag6[-i] } 
	if (is.null(all.res.lag7[[i]])) { all.res.lag7 = all.res.lag7[-i] } 
	if (is.null(all.res.lag8[[i]])) { all.res.lag8 = all.res.lag8[-i] } 
}


max.pre.res = list(all.res.lag2, all.res.lag3, all.res.lag4, all.res.lag5, 
	all.res.lag6, all.res.lag7, all.res.lag8)
save(max.pre.res, file = 'max_pre_res.Rdata')

aucs = saes = matrix(NA, nrow = nrow(covars), ncol = 7)
for (i in 1:nrow(aucs)) {
	
	aucs[i, 1] = all.res.lag2[[i]][['AUC']]; saes[i, 1] = all.res.lag2[[i]][['SAE']]
	aucs[i, 2] = all.res.lag3[[i]][['AUC']]; saes[i, 2] = all.res.lag3[[i]][['SAE']]
	aucs[i, 3] = all.res.lag4[[i]][['AUC']]; saes[i, 3] = all.res.lag4[[i]][['SAE']]
	aucs[i, 4] = all.res.lag5[[i]][['AUC']]; saes[i, 4] = all.res.lag5[[i]][['SAE']]
	aucs[i, 5] = all.res.lag6[[i]][['AUC']]; saes[i, 5] = all.res.lag6[[i]][['SAE']]
	aucs[i, 6] = all.res.lag7[[i]][['AUC']]; saes[i, 6] = all.res.lag7[[i]][['SAE']]
	aucs[i, 7] = all.res.lag8[[i]][['AUC']]; saes[i, 7] = all.res.lag8[[i]][['SAE']]
	
}


rbind(apply(aucs, 2, max), apply(saes, 2, min))


