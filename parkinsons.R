
## Inits
libs <- c("RCurl","chemometrics","caret","Rlof")
lapply(libs, require, character.only=TRUE)

## Read data
url <- getURL("https://archive.ics.uci.edu/ml/machine-learning-databases/parkinsons/parkinsons.data")
data <- read.csv(text=url)


## Univariate plots
features <- setdiff(names(data), c("name","status"))
for (feature in features) {
	hist(data[,feature],main=feature)
}

## Mahalanbois
for (dimensions in 1:length(features)) {
	outliers <- Moutlier(data[features[c(1:dimensions)]],quantile=0.975)
	dataOut <- data
	dataOut$md <- outliers$md
	dataOut$inlier <- ifelse(dataOut$md >= outliers$cutoff, 0, 1)
	cm <- confusionMatrix(as.factor(dataOut$inlier),as.factor(dataOut$status))
	print(features[c(1:dimensions)])
	print(cm)
	print("-------------------------")
}


## Robust Mahalanobis
for (dimensions in 1:length(features)) {
	outliers <- Moutlier(data[features[c(1:dimensions)]],quantile=0.975)
	dataOut <- data
	dataOut$rd <- outliers$rd
	dataOut$inlier <- ifelse(dataOut$rd >= outliers$cutoff, 0, 1)
	cm <- confusionMatrix(as.factor(dataOut$inlier),as.factor(dataOut$status))
	print(features[c(1:dimensions)])
	print(cm)
	print("-------------------------")
}

## LoF
for (dimensions in 1:length(features)) {
	outliers <- lof(data[features[c(1:dimensions)]], k=20)
	dataLoF<- data
	dataLoF$lof <- outliers
	dataLoF$inlier <- ifelse(dataLoF$lof > 1, 0, 1)
	cm <- confusionMatrix(as.factor(dataLoF$inlier),as.factor(dataLoF$status))
	print(features[c(1:dimensions)])
	print(cm)
	print("-------------------------")
}
