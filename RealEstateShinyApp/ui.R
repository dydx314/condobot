
library(leaflet)

# partialData = read.csv(file = "/Users/weininghu/Desktop/STAT450_STAT550_RealEstate/STAT550_analysis/RealEstateShinyApp/Dataset/partialData.csv", header = TRUE)
ui <- fluidPage(
	titlePanel("BC Condo ShinyApp"),
	sidebarLayout(
		sidebarPanel(
			radioButtons("regionInput", "Select a subregion",
									 choices = c("Metrotown", "Collingwood", "Whalley"),
									 selected = "Metrotown"),
			dateInput("listDate", "List Date/Estimated List Date"),
			dateInput("sellDate", "Estimated Sell Date"),
			textInput("dist", "Distance to Skytrain", 1000), ##need to change later
			textInput("floor", "Which floor", 10),
			textInput("totArea", "Total Floor Area", 1000),
			textInput("stratFee", "Strata Maint Fee", 300), 
			textInput("builtyr", "Built Year", 2010), #need to change later
			selectInput("totBed", "# of Bedrooms",
									choices = c("0", "1", "2", "3", "4"),
									selected = "2"),
			selectInput("totBath", "# of Bathrooms",
									choices = c("0", "1", "2", "3", "4"),
									selected = "1"),
			selectInput("parkingInput", "# of parking",
									choices = c("0", "1", "2", "3", "4", "5 or more", "NA"),
									selected = "1"),
			selectInput("lockerInput", "Locker",
									choices = c("Yes", "No", "NA"),
									selected = "Yes"),
			selectInput("rentalInput", "Bylaw Rental Restr",
									choices = c("Rental Allowed", "Allowed w/Rest", "Not Allowed"),
									selected = "Rental Allowed"),
			selectInput("petInput", "Bylaw Pet Restr",
									choices = c("Pets Allowed", "Allowed w/Rest", "Not Allowed"),
									selected = "Pets Allowed"),
			selectInput("ageRstr", "Age Restr",
									choices = c("Yes", "No"),
									selected = "No"),
			selectInput("smokgRstr", "Smoking Restr",
									choices = c("Yes", "No"),
									selected = "No"),
			selectInput("preSold", "Previously Sold?",
									choices = c("Yes", "No"),
									selected = "No"),
			textInput("previousSoldPr", "Previously Sold Price", 0),
			selectInput("FB_Tax", "FB Tax in Effect?",
									choices = c("Yes", "No"),
									selected = "Yes"),
			selectInput("FTHBP", "FTHBP in Effect?",
									choices = c("Yes", "No"),
									selected = "Yes"),
			selectInput("BCIIP", "BCIIP in Effect?",
									choices = c("Yes", "No"),
									selected = "Yes"),
			textInput("interest", "BOC Interest Rate %", 1.5),
			textInput("cadToCNY", "CAD to RMB", 4.8),
			textInput("cadToUSD", "CAD to USD", 0.75)
			),
		
		mainPanel(
			navbarPage("Navigation",
								 tabPanel("Map Visualization", 
								 				 leafletOutput("markerMap", width = 800, height = 500)),
								 tabPanel(style = "position:fixed; width:inherit;",
								 				 "Random Forest Prediction",
								 				 verbatimTextOutput("RF_estimate"),
								 				 #verbatimTextOutput("RF_Pr_estimate"),
								 				 plotOutput("regionImpVar", width = 800, height = 500)
								 				 ),
								 tabPanel("Recurrent Neural Network Prediction")
			)
		)
	)
)




