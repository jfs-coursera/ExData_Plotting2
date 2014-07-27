# Exploratory Data Analysis -- Project Assignment 2

# 5. How have emissions from motor vehicle sources changed from 1999–2008 in
# Baltimore City?

## Load all the data
NEI <- readRDS("./summarySCC_PM25.rds")
SCC <- readRDS("./Source_Classification_Code.rds")

## Extract data for Baltimore only
NEIBaltimore <- NEI[NEI$fips == "24510", ]

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
NEIMotor <- NEIBaltimore[NEIBaltimore$SCC %in% SCCMotor$SCC, ]

## Compute sums grouping by year
sumEmissions <- tapply(NEIMotor$Emissions, NEIMotor$year, sum)
## Create a matrix for easier plotting
mat <- cbind(as.numeric(names(sumEmissions)),
             as.vector(sumEmissions))

library(ggplot2)
## Convert matrix to data frame for ggplot
df <- as.data.frame(mat)

g <- ggplot(df, aes(x = V1, y = V2))
g <- g + xlab("Year") +
    ylab("Total Pollutant Emissions (tons)") +
    ggtitle("Total Pollutant Emissions in Baltimore City\nMotor Vehicle Sources") +
    geom_point() +
    geom_smooth(aes(group = 1), method = "lm") 

png("plot5.png")
print(g)
dev.off()
## Conclusion: seems to show a decrease

