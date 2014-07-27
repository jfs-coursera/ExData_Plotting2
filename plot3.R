# Exploratory Data Analysis -- Project Assignment 2

# 3. Of the four types of sources indicated by the type (point, nonpoint,
# onroad, nonroad) variable, which of these four sources have seen decreases in
# emissions from 1999–2008 for Baltimore City? Which have seen increases in
# emissions from 1999–2008? Use the ggplot2 plotting system to make a plot
# answer this question.

## Load all the data
NEI <- readRDS("./summarySCC_PM25.rds")
SCC <- readRDS("./Source_Classification_Code.rds")

## Extract data for Baltimore only
## Extract the data we need so we can work with a smaller data frame
NEIBaltimore <- NEI[NEI$fips == "24510", c("Emissions", "type", "year")]

library(ggplot2)
## create a table to sum the Emissions by year and by type
## and convert it to a data.frame
df <- as.data.frame(xtabs(Emissions ~ year + type, data = NEIBaltimore))

g <- ggplot(df, aes(x = year, y = Freq))
g <- g + xlab("Year") +
     ylab("Total Pollutant Emissions (tons)") +
     ggtitle("Total Pollutant Emissions in Baltimore City, Maryland") +
     geom_point() +
     facet_grid(. ~ type) +
     facet_wrap(~ type, ncol = 2) +  
     geom_smooth(aes(group = 1), method = "lm") 

png("plot3.png")
print(g)
dev.off()
## Conclusion: only POINT seems to show kind of an increasing trend.
