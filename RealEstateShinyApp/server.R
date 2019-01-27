library(leaflet)
library(rstudioapi)
library(randomForest)
library(dplyr)
library(stringr)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
partialData = read.csv(file = "./Dataset/fullOriginal8000.csv", header = TRUE)
partialData2000 = partialData[1:2000,]


#Load RF models
load("Dataset/rf_fit_metro.RData")
load("Dataset/rf_fit_colligwod.RData")
load("Dataset/rf_fit_whalley.RData")
trend_2018 <- read.csv(file = "Dataset/trend_base_long.csv", header = TRUE)


getColor <- function(realEstate) {
	sapply(realEstate$Sold.Price.per.SqFt, function(price) {
		if(price <= 300) {
			"#FF0000"
		} else if(price <= 400) {
			"#52E74B"
			print("Come here")
		} else {
			"#6754D8"
		} })
}

pal <- colorNumeric(c("blue", "green", "red"), 200:700, alpha = FALSE)

icons <- awesomeIcons(
	icon = 'home',
	iconColor = 'black',
	library = 'ion',
	markerColor = 'red'
)

server <- function(input, output) {
	output$markerMap = renderLeaflet({
		leaflet(partialData2000) %>% addTiles() %>%
	addCircleMarkers(~longitude, ~latitude, 
		popup = paste("Status:", partialData2000$Status, "<br>",
                      "Price:", partialData2000$Price, "<br>",
                      "TotalFloorArea:", partialData2000$TotFlArea, "<br>",
											"Price Per SQ", partialData2000$Sold.Price.per.SqFt, "<br>"),
		color = ~pal(partialData2000$Sold.Price.per.SqFt)
		)

		})
	
		monthBase_df <- reactive({
			trend_2018 %>%
				filter(soldYearMonth == format(input$sellDate, "%Y-%m"),
							 Neighborhood == input$regionInput)
		})


	output$RF_estimate <- renderPrint({

		newobs <-
			data_frame(DOM = as.numeric(difftime(input$sellDate, input$listDate, units = c("days"))),
			Tot.BR = input$totBed,
			TotFlArea = input$totArea,
			Tot.Baths = input$totBath,
			Age = 2018 - as.integer(input$builtyr),
			soldDay = as.numeric(str_sub(input$sellDate, -2)),
			soldMonth = as.numeric(str_sub(input$sellDate, 6, 7)),
			soldYear = as.numeric(str_sub(input$sellDate, 1, 4)),
			nthFloor = input$floor,
		  parking = factor(ifelse(input$parkingInput == "0", "Zero",
		  												ifelse(input$parkingInput == "1", "One",
		  															 ifelse(input$parkingInput == "2", "Two",
		  															 			 ifelse(input$parkingInput == "3", "Three",
		  															 			 			 ifelse(input$parkingInput == "4", "Four",
		  															 			 			 			 ifelse(input$parkingInput == "NA", "Not_Available", "Five_or_More")))))),
		  								 levels=c("Five_or_More", "Four", "Not_Available", "One", "Three", "Two", "Zero")),
			prevPrice = as.numeric(input$previousSoldPr),
			preSold = ifelse(input$preSold == "Yes", TRUE, FALSE),
			stratMtFee = input$stratFee,
			locker = factor(ifelse(input$lockerInput == "Yes", "3",
												ifelse(input$lockerInput == "No", "2", "1")), levels = c("1", "2", "3")),
			petsAllowed = ifelse(input$petInput == "Pets Allowed", TRUE, FALSE),
			rentalsAllowed = ifelse(input$rentalInput == "Rental Allowed", TRUE, FALSE),
			ageRstr = ifelse(input$ageRstr == "Yes", TRUE, FALSE),
			smokingRstr = ifelse(input$smokgRstr == "Yes", TRUE, FALSE),
			petsWithRstr = ifelse(input$petInput == "Allowed w/Rest", TRUE, FALSE),
			rentalsWithRstr = ifelse(input$rentalInput == "Allowed w/Rest", TRUE, FALSE),
			Distance.Skytrain = input$dist,
			monthBase = monthBase_df()[1, 4],
			FB_Tax = ifelse(input$FB_Tax == "Yes", 1, 0),
			FTHBP = ifelse(input$FTHBP == "Yes", 1, 0),
			BCIIP = ifelse(input$BCIIP == "Yes", 1, 0),
			CAD_USD = input$cadToUSD,
			CAD_CNY = input$cadToCNY,
			Interest = input$interest
		)

		if(input$regionInput == "Collingwood"){
			colligwod_predict <- 
				c(predict(rf_fit_colligwod, newobs, type="response")[1], 
					predict(rf_fit_colligwod, newobs, type="response")[1] * as.numeric(input$totArea))
			names(colligwod_predict) <- c("Predicted $/sf", "Predicted Price")
			colligwod_predict
			
		}else if(input$regionInput == "Metrotown"){
			metro_predict <- 
				c(predict(rf_fit_metro, newobs, type="response")[1], 
				predict(rf_fit_metro, newobs, type="response")[1] * as.numeric(input$totArea))
			names(metro_predict) <- c("Predicted $/sf", "Predicted Price")
			metro_predict
		}else{
			whalley_predict <- 
				c(predict(rf_fit_whalley, newobs, type="response")[1], 
					predict(rf_fit_whalley, newobs, type="response")[1] * as.numeric(input$totArea))
			names(whalley_predict) <- c("Predicted $/sf", "Predicted Price")
			whalley_predict
		}

	})
	
	# output$RF_Pr_estimate <- renderPrint({
	# 	
	# 	# if(input$regionInput == "Collingwood"){
	# 	# 	rf.preds <- rfPredVar(rf_fit_colligwod, rf.data = data_rf_colligwod, CI = TRUE)
	# 	# }else if(input$regionInput == "Metrotown"){
	# 	# 	rf.preds <- rfPredVar(rf_fit_metro, rf.data = data_rf_metro, CI = TRUE)
	# 	# }else{
	# 	# 	rf.preds <- rfPredVar(rf_fit_whalley, rf.data = data_rf_whalley, CI = TRUE)
	# 	# }
	# 	input$totArea *RF_estimate
	# })
	
	output$regionImpVar <- renderPlot({
		if(input$regionInput == "Collingwood"){
			varImpPlot(rf_fit_colligwod,type=2)
		}else if(input$regionInput == "Metrotown"){
			varImpPlot(rf_fit_metro,type=2)
		}else{
			varImpPlot(rf_fit_whalley,type=2)
		}
	})
  
}