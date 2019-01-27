library(rstudioapi)
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

originalData = read.csv(file = "Dataset/fullData.csv")
originalData["longitude"] = NA
originalData["latitude"] = NA

getlonglatFromAdddress = function(address) {
	require(RJSONIO)
	url = "http://maps.google.com/maps/api/geocode/json?address="
	url = URLencode(paste(url, address, "&sensor=false", sep = ""))
	x = fromJSON(url, simplify = FALSE)
	if (x$status == "OK") {
		out = c(x$results[[1]]$geometry$location$lng,
						 x$results[[1]]$geometry$location$lat)
	} else {
		out = NA
	}
	Sys.sleep(0.2)  # API only allows 5 requests per second
	out
}

for (i in 1:nrow(originalData)) {
	homeAddress = as.character(originalData[i,]$Address)
	neighborhood = as.character(originalData[i,]$Neighborhood)
	fullAdress = paste(homeAddress, neighborhood, 'Vancouver', sep = ',')
	longlat = getlonglatFromAdddress(fullAdress)
	originalData[i,]$longitude = longlat[1]
	originalData[i,]$latitude = longlat[2]
}

