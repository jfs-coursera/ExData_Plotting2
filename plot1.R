# Exploratory Data Analysis -- Project Assignment 2

# 1. Have total emissions from PM2.5 decreased in the United States from 1999 to
# 2008? Using the base plotting system, make a plot showing the total PM2.5
# emission from all sources for each of the years 1999, 2002, 2005, and 2008.

## Load all the data
NEI <- readRDS("./summarySCC_PM25.rds")
SCC <- readRDS("./Source_Classification_Code.rds")


## Compute sums grouping by year, divide by 1M to display small numbers later
## in the y-axis
sumEmissions <- tapply(NEI$Emissions / 1e+6, NEI$year, sum)
## Create a two-columns matrix for easier plotting
mat <- cbind(as.numeric(names(sumEmissions)),
             as.vector(sumEmissions))

png("plot1.png")
par(lend = "square")
plot(mat[,2] ~ mat[,1],
     type = "h", lwd = 5,
     xlab = "Year", ylab = "Total Pollutant Emissions (Millions of tons)",
     main = "Pollutant Emissions per Year in the U.S.A.",
     ylim = c(0, ceiling(max(mat[,2]))),
     col = "blue", xaxt = "n"
    )
axis(1, at = mat[,1])  ## display only the years indicated in the matrix
abline(lm(mat[,2] ~ mat[,1]), lwd = 2, col = "grey")
dev.off()
## Conclusion: abline() seems to show a decreasing trend.

