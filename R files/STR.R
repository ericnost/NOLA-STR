library(tidyr)
library(readr)
library(rgdal)
library(plyr)
library(rgeos)
library(tidycensus)
#geojson, tibble, sp
str <- read_csv("Airdna data/louisiana-new-orleans_Property_2017-11-07.csv")

#filter to hotel definition
hotels <- str[ which (str$`Property Type` == "Condominium" | str$`Property Type` == "House" | str$`Property Type` == "Apartment"),]
hotels <- hotels[ which (hotels$`Listing Type` == "Entire home/apt" & hotels$Bedrooms <= 3),]
hotels <- hotels[ which (hotels $`Annual Revenue LTM` > 0),]
hotels <- hotels[ which (hotels$`Count Reservation Days LTM` + hotels$`Count Available Days LTM` >=210 & hotels$`Count Reservation Days LTM` >= 91),]
hotels[length(hotels)+1]<-1
#colnames(hotels[length(hotels)])<-c("count") #broken

#census
#median_contract_rent <- read_csv("Census data/2016 ACS tract/ACS_16_5YR_B25058_with_ann.csv")
#median_gross_rent <- read_csv("Census data/2016 ACS tract/ACS_16_5YR_B25064_with_ann.csv")
#aggregate_gross_rent <- read_csv("Census data/2016 ACS tract/ACS_16_5YR_B25065_with_ann.csv")
#home_costs <- read_csv("Census data/2016 ACS tract/ACS_16_5YR_B25089_with_ann.csv")
#tenure <- read_csv("Census data/2016 ACS tract/ACS_16_5YR_B25003_with_ann.csv")

key<-"cc96dba63907bd5fce666ebe3e17ae1e9f58e0b8"
census_api_key(key)
median_contract_rent <- get_acs(geography = "tract", 
              variables = c(medconrent = "B25058_001"), 
              state = "LA",
              county = "Orleans")
median_gross_rent <- get_acs(geography = "tract", 
                            variables = c(medgrossrent = "B25058_001"), 
                            state = "LA",
                            county = "Orleans")
aggregate_gross_rent <- get_acs(geography = "tract", 
                               variables = c(agggrossrent = "B25065_001"), 
                               state = "LA",
                               county = "Orleans")
home_costs <- get_acs(geography = "tract", 
                      variables = c(homecosts = "B25089_001"), 
                      state = "LA",
                      county = "Orleans")
tenure <- get_acs(geography = "tract", 
                  variables = c(tenure = "B25003_001"), 
                  state = "LA",
                  county = "Orleans")
  
tracts <- readOGR(dsn = "shapefiles/tracts", layer = "census tracts") 
plot(tracts,border="wheat3", col="wheat1")

dfs<-list(median_contract_rent, median_gross_rent, aggregate_gross_rent, home_costs, tenure)
data<-join_all(dfs, by="GEOID")
data<-data[,-c(6,10,14,18)]
#change colnames

#spatial join
#csv to shp
hotels_spatial<-hotels
coordinates(hotels_spatial)<-c("Longitude", "Latitude")
#writeOGR(hotels_spatial, "output", "hotels", driver="ESRI Shapefile") #check geometry
#by.hotel<-overGeomGeomDF(hotels_spatial, tracts) #gives tract data for each point #fn = mean
#by.tract<-overGeomGeomDF(tracts, hotels_spatial) #gives last point for each tract
hotels_spatial_numbers<-hotels_spatial[c(15, 37, 38, 45)] #replace with colnames #other column numbers here as necessary

by.tract.mean<-overGeomGeomDF(tracts, hotels_spatial_numbers, fn=mean)
by.tract.sum<-overGeomGeomDF(tracts, hotels_spatial_numbers, fn=sum)
by.tract<-cbind(by.tract.mean, by.tract.sum)
colnames(by.tract)<-c("Annual Revenue Mean", "Reservation Days Mean", "Available Days Mean", "Count mean", "Revenue Sum", "Reservation Days Sum", "Available Days Sum", "Count")

tracts@data<-cbind(tracts@data, by.tract)
tracts@data<-merge(tracts@data, data, by="GEOID") #move up? #merge census data with spatial join
#nas to zero??!?!?!!?!

#calculations
#closed rent gap
tracts@data[length(tracts@data)+1]<-(tracts$Count / tracts$`Estimate; Total: - Renter occupied` )*100
tracts@data[length(tracts@data)+1]<-((tracts$`Revenue Sum`/12) / ((tracts$`Revenue Sum`/12) + tracts$`Estimate; Aggregate gross rent`))*100
tracts@data[length(tracts@data)+1]<-((tracts$`Annual Revenue Mean` /12) / tracts$`Estimate; Median contract rent` )*100
#change colnames

writeOGR(tracts, "shapefiles/output", "tracts", driver="ESRI Shapefile") #output!