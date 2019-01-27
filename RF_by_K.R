library(dplyr)
library(tidyr)
library(stringr)
library(randomForest)
load("clean_data.RData")


#Read in and clean the data
dist <- read.csv("distance_skytrain.csv", header = T) %>%
	mutate(ID = as.character(ML..)) %>%
	select(-ML..)

trend_base_2018 <- read.csv("Data_for_RF/VAR.csv", header = T) %>%
	gather(Neighborhood, monthBase, -Date) %>%
	rename(soldYearMonth = Date)
write.csv(trend_base_2018, "RealEstateShinyApp/Dataset/trend_base_long.csv")

BOC_interest <- read.csv("Data_for_RF/boc_interest_rate.csv", header = T) %>%
	mutate(YearMonth = str_sub(Date, 1, 7)) %>%
	select(-Date) %>%
	rename(Interest = V122530)
policy <- read.csv("Data_for_RF/policy.csv", header = T) %>%
	mutate(YearMonth = str_sub(Date, 1, 7)) %>%
	select(-Date)

curcy1 <- read.csv("Data_for_RF/cad_usd.csv", header = T) %>%
	mutate(YearMonth = str_sub(Date, 1, 7)) %>%
	select(-Date)
curcy2 <- read.csv("Data_for_RF/cad_cny.csv", header = T) %>%
	mutate(YearMonth = str_sub(Date, 1, 7)) %>%
	select(-Date)
otherFacotrs <- full_join(curcy1, curcy2, by = "YearMonth") %>%
	left_join(policy,., by = "YearMonth") %>%
	left_join(., BOC_interest, by = "YearMonth")

cleaned_data_full_rf <-
	full_data %>% 
	mutate(soldDate = List.Date + DOM, 
				 soldDay = as.numeric(str_sub(soldDate, -2)),
				 soldMonth = as.numeric(str_sub(soldDate,6, 7)), 
				 soldYear = as.numeric(str_sub(soldDate, 1, 4)), 
				 soldYearMonth = as.factor(str_sub(soldDate, 1, 7)),
				 nthFloor = as.numeric(str_sub(sub(" .*", "", Address), -2))) %>%
	filter(Age < 200, 
				 !is.na(Sold.Price.per.SqFt), 
				 !is.na(Neighborhood)) %>%
	mutate(parking = as.factor(ifelse(is.na(TotalPrkng), "Not_Available", 
												 ifelse(TotalPrkng == 0, "Zero", 
												 			 ifelse(TotalPrkng == 1, "One",
												 			 			 ifelse(TotalPrkng == 2, "Two",
												 			 			 			 ifelse(TotalPrkng == 3, "Three",
												 			 			 			 			 ifelse(TotalPrkng == 4, "Four", "Five_or_More"))))))),
				 prevPrice = replace(Prev.Price, is.na(Prev.Price), 0), 
				 preSold = (prevPrice != 0),
				 stratMtFee = replace(StratMtFee, is.na(StratMtFee), 0), #Do not make sense
				 nthFloor = replace(nthFloor, is.na(nthFloor), 0), #Do not make sense
				 locker = as.factor(as.numeric(Locker)),  #Y - 3, N - 2, "" - 1
				 region = as.factor(Neighborhood),
				 ID = as.character(ML..),
				 Bylaw.Restrictions = paste0(as.character(Bylaw.Restrictions),','),
				 petsAllowed = grepl("Pets Allowed,", Bylaw.Restrictions),
				 rentalsAllowed = grepl("Rentals Allowed,", Bylaw.Restrictions),
				 ageRstr = grepl("Age Restrictions,", Bylaw.Restrictions),
				 smokingRstr = grepl("Smoking Restrictions,", Bylaw.Restrictions),
				 petsWithRstr = grepl("Pets Allowed w/Rest.,", Bylaw.Restrictions),
				 rentalsWithRstr = grepl("Rentals Allwd w/Restrctns,", Bylaw.Restrictions)) %>%
	dplyr::select(DOM, Tot.BR:Age, Sold.Price.per.SqFt, soldDay:rentalsWithRstr)

#combine with trend and dist to skytrain
trend_base <- 
	cleaned_data_full_rf %>%
	group_by(soldYearMonth, region) %>%
	mutate(monthBase = mean(Sold.Price.per.SqFt)) %>%
	select(soldYearMonth, region, monthBase) %>%
	as.data.frame() 

data_rf_full <- left_join(cleaned_data_full_rf, dist, by = "ID") %>%
	left_join(., trend_base, by = c("soldYearMonth", "region")) %>% 
	left_join(., otherFacotrs, by = c("soldYearMonth"= "YearMonth")) %>%
	unique() %>%
	select(-c(soldYearMonth, ID))

data_rf_metro <- data_rf_full %>%
	filter(region == "Metrotown") %>%
	select(-region)

data_rf_colligwod <- data_rf_full %>%
	filter(region == "Collingwood") %>%
	select(-region)

data_rf_whalley <- data_rf_full %>%
	filter(region == "Whalley") %>%
	select(-region)


#Fit random forest
rf_fit_metro <- randomForest(Sold.Price.per.SqFt~., data = data_rf_metro, keep.inbag=T)
rf_fit_colligwod <- randomForest(Sold.Price.per.SqFt~., data = data_rf_colligwod, keep.inbag=T)
rf_fit_whalley <- randomForest(Sold.Price.per.SqFt~., data = data_rf_whalley, keep.inbag=T)

save(rf_fit_metro, file = "RealEstateShinyApp/Dataset/rf_fit_metro.RData")
save(rf_fit_colligwod, file = "RealEstateShinyApp/Dataset/rf_fit_colligwod.RData")
save(rf_fit_whalley, file = "RealEstateShinyApp/Dataset/rf_fit_whalley.RData")


