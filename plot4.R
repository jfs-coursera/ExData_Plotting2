# Exploratory Data Analysis -- Project Assignment 2

# 4. Across the United States, how have emissions from coal combustion-related
# sources changed from 1999–2008?

## Load all the data
NEI <- readRDS("./summarySCC_PM25.rds")
SCC <- readRDS("./Source_Classification_Code.rds")

## Extract Coal related data
## Get the indices of rows where EI.Sector contains "Coal"
indices <- grep("Coal", SCC[, "EI.Sector"], ignore.case = TRUE)
SCCCoal <- SCC[indices, ]

## Extract corresponding NEI data, based on SCC
NEICoal <- NEI[NEI$SCC %in% SCCCoal$SCC, ]

## Compute sums grouping by year, divide by 1K to display small numbers
sumEmissions <- tapply(NEICoal$Emissions / 1e+3, NEICoal$year, sum)
## Create a matrix for easier plotting
mat <- cbind(as.numeric(names(sumEmissions)),
            as.vector(sumEmissions))

library(ggplot2)
## Convert matrix to data frame for ggplot
df <- as.data.frame(mat)

g <- ggplot(df, aes(x = V1, y = V2))
g <- g + xlab("Year") +
    ylab("Total Pollutant Emissions (Thousands of tons)") +
    ggtitle("Total Pollutant Emissions in the U.S.A.\nCoal Combustion-Related Sources") +
    geom_point() +
    geom_smooth(aes(group = 1), method = "lm") 

png("plot4.png")
print(g)
dev.off()
## Conclusion: seems to show a decrease

