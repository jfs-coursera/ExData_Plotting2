# Exploratory Data Analysis -- Project Assignment 2

# 2. Have total emissions from PM2.5 decreased in the Baltimore City, Maryland
# (fips == "24510") from 1999 to 2008? Use the base plotting system to make a
# plot answering this question.

## Load all the data
NEI <- readRDS("./summarySCC_PM25.rds")
SCC <- readRDS("./Source_Classification_Code.rds")

## Extract data for Baltimore only
## Extract the columns we need so we can work with a smaller data frame
NEIBaltimore <- NEI[NEI$fips == "24510", c("Emissions", "year")]

## Compute sums grouping by year
sumEmissions <- tapply(NEIBaltimore$Emissions, NEIBaltimore$year, sum)
## Create a two-columns matrix for easier plotting
mat <- cbind(as.numeric(names(sumEmissions)),
             as.vector(sumEmissions))

png("plot2.png")
par(lend = "square")
plot(mat[,2] ~ mat[,1],
     type = "h", lwd = 5,
     xlab = "Year", ylab = "Total Pollutant Emissions (tons)",
     main = "Pollutant Emissions per Year in Baltimore City, Maryland",
     ylim = c(0, ceiling(max(mat[,2]))),
     col = "blue", xaxt = "n"
    )
axis(1, at = mat[,1])  ## display only the years indicated in the matrix
abline(lm(mat[,2] ~ mat[,1]), lwd = 2, col = "grey")
dev.off()
## Conclusion: abline() seems to show a decreasing trend.

