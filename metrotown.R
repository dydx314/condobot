load("clean_data.RData")

full_data$Sold.Date <- full_data$List.Date + full_data$DOM
full_data$SoldMonth <- as.numeric(format(full_data$Sold.Date,"%m"))

date <- as.Date(full_data$List.Date, "%m/%d/%Y") 
sold_date2 <- format(date + full_data$DOM,format = "%Y/%m")
full_data <- cbind(full_data, sold_date2)

outlier <- which(full_data$Age >= 999 | is.na(full_data$Price) | is.na(full_data$Sold.Price.per.SqFt) | is.na(full_data$DOM))
full_data <- full_data[-outlier,]

subset <-subset(full_data, Neighborhood == "Metrotown")

trend_price <- aggregate(subset$Sold.Price.per.SqFt, list(subset$sold_date), mean)

names(trend_price) <- c("date","trend_price")

trend_price$trend_price[142:144]=c(757.27,785.35,803.66)

s <- merge(subset,trend_price,by.x = "sold_date2", by.y = "date")

metrotown<- subset(s, select=-c(S.A, PicCount, Pics, ML.., Status, Address, List.Date, 
																Sold.Date, TypeDwel, List.Sales.Rep.1...Agent.Full.Name, Age.Type))


metrotown$TotalPrkng <- replace(metrotown$TotalPrkng, is.na(metrotown$TotalPrkng), -1)

metrotown$Prev.Price <- replace(metrotown$Prev.Price, is.na(metrotown$Prev.Price), 0)

metrotown$Prev.Sold <- metrotown$Prev.Price != 0

metrotown$StratMtFee <- replace(metrotown$StratMtFee, is.na(metrotown$StratMtFee), 0)

metrotown$Tot.BR <- as.factor(metrotown$Tot.BR)
metrotown$Tot.Baths <- as.factor(metrotown$Tot.Baths)
metrotown$TotalPrkng <- as.factor(metrotown$TotalPrkng)
metrotown$Neighborhood <- as.factor(metrotown$Neighborhood)
metrotown$Age <- as.numeric(metrotown$Age)
metrotown$Prev.Sold <- as.factor(metrotown$Prev.Sold)

metrotown$Bylaw.Restrictions <- as.character(metrotown$Bylaw.Restrictions)
metrotown$Bylaw.Restrictions = paste0(metrotown$Bylaw.Restrictions, ',')



metrotown$Pets.Allowed = grepl("Pets Allowed,", metrotown$Bylaw.Restrictions)
metrotown$Rentals.Allowed = grepl("Rentals Allowed,", metrotown$Bylaw.Restrictions)
metrotown$Age.Restrictions = grepl("Age Restrictions,", metrotown$Bylaw.Restrictions)
metrotown$Smoking.Restrictions = grepl("Smoking Restrictions,", metrotown$Bylaw.Restrictions)
metrotown$Pets.Allowed.Rst = grepl("Pets Allowed w/Rest.,", metrotown$Bylaw.Restrictions)
metrotown$Rentals.Allowed.Rst = grepl("Rentals Allwd w/Restrctns,", metrotown$Bylaw.Restrictions)



metrotown$Pets.Allowed <- as.factor(metrotown$Pets.Allowed)
metrotown$Rentals.Allowed <- as.factor(metrotown$Rentals.Allowed)
metrotown$Age.Restrictions <- as.factor(metrotown$Age.Restrictions)
metrotown$Smoking.Restrictions <- as.factor(metrotown$Smoking.Restrictions)
metrotown$Pets.Allowed.Rst <- as.factor(metrotown$Pets.Allowed.Rst)
metrotown$Rentals.Allowed.Rst <- as.factor(metrotown$Rentals.Allowed.Rst)


metrotown <- subset(metrotown, select=-c(Bylaw.Restrictions, Yr.Blt, Prev.Status))

district <- metrotown
performCV2 <- function(district, ntree) {
	y <- as.vector(district$Sold.Price.per.SqFt)
	n <- length(y)  
	# exclude confounding variables (directly derived from price)
	"
	district_ <- subset(district, select = -c(Sold.Price.per.SqFt, Neighborhood, Price, sold_date2))
	training <- district_[1:3446,]
	ytraining <- y[1:3446]
	test <- district_[3447:n,]
	ytest <- y[3447:n]
  "
	index <- which(district$sold_date2 %in% c("2017/10", "2017/11", "2017/12"))
	training <- district[-c(index),]
	test <- district[index,]
	ytraining <- as.vector(training$Sold.Price.per.SqFt)
	ytest <- as.vector(test$Sold.Price.per.SqFt)
	training <- subset(training, select = -c(Sold.Price.per.SqFt, Neighborhood, Price, sold_date2))
	test <- subset(test, select = -c(Sold.Price.per.SqFt, Neighborhood, Price, sold_date2))
	
	
	rf=randomForest(x = training, y=ytraining, xtest=test,ytest=ytest, ntree=ntree)
	pr <- rf$test$predicted
	mspe<- mean((ytest - pr)^2)
	prmspe <- mean(abs(ytest - pr)/pr)

	
	# returns mean-squared prediction error and percentage root-mean squared prediction error
	return(rbind(mspe,prmspe))
}

performCV2(metrotown, 500)

#### Random Forest Proposed 2
library(dplyr)
load("clean_data.RData")

full_data$Sold.Date <- full_data$List.Date + full_data$DOM
full_data$SoldMonth <- as.numeric(format(full_data$Sold.Date,"%m"))

date <- as.Date(full_data$List.Date, "%m/%d/%Y") 
sold_date2 <- format(date + full_data$DOM,format = "%Y/%m")
full_data <- cbind(full_data, sold_date2)

outlier <- which(full_data$Age >= 999 | is.na(full_data$Price) | is.na(full_data$Sold.Price.per.SqFt) | is.na(full_data$DOM))
full_data <- full_data[-outlier,]

subset <-subset(full_data, Neighborhood == "Metrotown")
base_price <- aggregate(subset$Sold.Price.per.SqFt, list(subset$sold_date), mean)
names(base_price) <- c("sold_date2","base_price")
trend_price$base_price[142:144] <- c(757.27,785.35,803.66)

my_dat <- left_join(base_price, subset, by = "sold_date2") 
metrotown <- subset(my_dat, select=-c(S.A, PicCount, Pics, ML.., Status, Address, List.Date, 
																Sold.Date, TypeDwel, List.Sales.Rep.1...Agent.Full.Name, Age.Type)) %>%
	mutate(adjusted_price_per_sf = Sold.Price.per.SqFt - base_price)
metrotown$TotalPrkng <- replace(metrotown$TotalPrkng, is.na(metrotown$TotalPrkng), -1)

metrotown$Prev.Price <- replace(metrotown$Prev.Price, is.na(metrotown$Prev.Price), 0)

metrotown$Prev.Sold <- metrotown$Prev.Price != 0

metrotown$StratMtFee <- replace(metrotown$StratMtFee, is.na(metrotown$StratMtFee), 0)

metrotown$Tot.BR <- as.factor(metrotown$Tot.BR)
metrotown$Tot.Baths <- as.factor(metrotown$Tot.Baths)
metrotown$TotalPrkng <- as.factor(metrotown$TotalPrkng)
metrotown$Neighborhood <- as.factor(metrotown$Neighborhood)
metrotown$Age <- as.numeric(metrotown$Age)
metrotown$Prev.Sold <- as.factor(metrotown$Prev.Sold)

metrotown$Bylaw.Restrictions <- as.character(metrotown$Bylaw.Restrictions)
metrotown$Bylaw.Restrictions = paste0(metrotown$Bylaw.Restrictions, ',')



metrotown$Pets.Allowed = grepl("Pets Allowed,", metrotown$Bylaw.Restrictions)
metrotown$Rentals.Allowed = grepl("Rentals Allowed,", metrotown$Bylaw.Restrictions)
metrotown$Age.Restrictions = grepl("Age Restrictions,", metrotown$Bylaw.Restrictions)
metrotown$Smoking.Restrictions = grepl("Smoking Restrictions,", metrotown$Bylaw.Restrictions)
metrotown$Pets.Allowed.Rst = grepl("Pets Allowed w/Rest.,", metrotown$Bylaw.Restrictions)
metrotown$Rentals.Allowed.Rst = grepl("Rentals Allwd w/Restrctns,", metrotown$Bylaw.Restrictions)



metrotown$Pets.Allowed <- as.factor(metrotown$Pets.Allowed)
metrotown$Rentals.Allowed <- as.factor(metrotown$Rentals.Allowed)
metrotown$Age.Restrictions <- as.factor(metrotown$Age.Restrictions)
metrotown$Smoking.Restrictions <- as.factor(metrotown$Smoking.Restrictions)
metrotown$Pets.Allowed.Rst <- as.factor(metrotown$Pets.Allowed.Rst)
metrotown$Rentals.Allowed.Rst <- as.factor(metrotown$Rentals.Allowed.Rst)

metrotown <- subset(metrotown, select=-c(Bylaw.Restrictions, Yr.Blt, Prev.Status))

performCV3 <- function(district, ntree) {
# 	y <- as.vector(district$Sold.Price.per.SqFt)
# 	n <- length(y)  
# 	# exclude confounding variables (directly derived from price)
# 	"
# 	district_ <- subset(district, select = -c(Sold.Price.per.SqFt, Neighborhood, Price, sold_date2))
# 	training <- district_[1:3446,]
# 	ytraining <- y[1:3446]
# 	test <- district_[3447:n,]
# 	ytest <- y[3447:n]
#   "
	index <- which(district$sold_date2 %in% c("2017/10", "2017/11", "2017/12"))
	training <- district[-c(index),]
	test <- district[index,]
	ytraining <- as.vector(training$adjusted_price_per_sf)
	ytest <- as.vector(test$adjusted_price_per_sf)
	training_rf <- subset(training, select = -c(adjusted_price_per_sf, base_price, Sold.Price.per.SqFt, Neighborhood, Price, sold_date2))
	test_rf <- subset(test, select = -c(adjusted_price_per_sf, base_price, Sold.Price.per.SqFt, Neighborhood, Price, sold_date2))
	
	rf=randomForest(x = training_rf, y=ytraining, xtest=test_rf, ytest=ytest, ntree=ntree)
	pr <- rf$test$predicted + test$base_price
	mspe<- mean((test$Sold.Price.per.SqFt - pr)^2)
	prmspe <- mean(abs(test$Sold.Price.per.SqFt - pr)/pr)
	
	# returns mean-squared prediction error and percentage root-mean squared prediction error
	return(rbind(mspe,prmspe))
}

performCV3(metrotown, 500)