# Exploratory Data Analysis -- Project Assignment 2

# 6. Compare emissions from motor vehicle sources in Baltimore City with
# emissions from motor vehicle sources in Los Angeles County, California (fips
# == "06037"). Which city has seen greater changes over time in motor vehicle
# emissions?

## Load all the data
NEI <- readRDS("./summarySCC_PM25.rds")
SCC <- readRDS("./Source_Classification_Code.rds")

## Extract data for Baltimore and Los Angeles
NEIBalLa <- NEI[NEI$fips %in% c("24510", "06037"), ]
## Add a column for the city names
## (Because I have troubles labelling the facets...)
NEIBalLa$CityName <- factor(NEIBalLa$fips,
                            levels = c("06037", "24510"),
                            labels = c("Baltimore City", "Los Angeles County"))


## Extract motor vehicle related data
## Get the indices of rows where
## - EI.Sector contains "Mobile"
## - EI.Sector contains "Non-Road"
## And get the difference of the sets
idxMobile <- grep("Mobile", SCC[, "EI.Sector"], ignore.case = TRUE)
idxNonRoad <- grep("Non-Road", SCC[, "EI.Sector"], ignore.case = TRUE)
## setdiff: we want what is in idxMobile AND is not in idxNonRoad
indices <- setdiff(idxMobile, idxNonRoad)
SCCMotor <- SCC[indices, ]

## Extract corresponding NEI data, based on SCC
NEIMotor <- NEIBalLa[NEIBalLa$SCC %in% SCCMotor$SCC, ]

library(ggplot2)
## create a table to sum the Emissions by year and by CityName (or fips)
## and convert it to a data.frame
df <- as.data.frame(xtabs(Emissions ~ year + CityName, data = NEIMotor))

g <- ggplot(df, aes(x = year, y = Freq))
g <- g + xlab("Year") +
    ylab("Total Pollutant Emissions (tons)") +
    ggtitle("Total Pollutant Emissions") +
    geom_point() +
    facet_grid(. ~ CityName) +
    facet_wrap(~ CityName) +
    geom_smooth(aes(group = 1), method = "lm") 

png("plot6.png")
print(g)
dev.off()
## Conclusion: seems to show increase in Baltimore City, and a very slight
## decrease in Los Angeles county.
## Overall pollution is higher in Baltimore City.


