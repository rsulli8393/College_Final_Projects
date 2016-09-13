library("rworldmap")
library("ggplot2")
library("reshape2")
#load data
HDI <- read.csv("~/Documents/Senior Year/Spring Semester/Data Science/Final Project/HumanDevelopment.csv")
View(HDI)
#create map information
spdf <- joinCountryData2Map(HDI, joinCode="NAME", nameJoinColumn="Country")
#HDI index for women
mapCountryData(spdf, nameColumnToPlot="HDIFemale",colourPalette = c("#eff3ff", "#9ecae1", "#6baed6", "#4292c6", "#2171b5","#084594"), catMethod="fixedWidth",mapTitle = "Women's Human Development Index")
#female in school
mapCountryData(spdf, nameColumnToPlot="FemaleYearSchool", catMethod="fixedWidth", mapTitle = "Average Amount Years Females Spend In School")
#Female income in Africa
mapCountryData(spdf, nameColumnToPlot="FemaleIncome", mapRegion="Africa", numCats=5, catMethod="fixedWidth", addLegend=FALSE, mapTitle = "Average Female Income in Africa")
#South America
mapCountryData(spdf, nameColumnToPlot="FemaleIncome", mapRegion="latin america", numCats=5, catMethod="fixedWidth", addLegend=FALSE, mapTitle = "Average Female Income in South America")
#Asia
mapCountryData(spdf, nameColumnToPlot="FemaleIncome", mapRegion="asia", numCats=5, catMethod="fixedWidth", addLegend=FALSE, mapTitle = "Average Female Income in Asia")
#Comparisons
hist(HDI$HDIMale, col=rgb(1,0,0,0.5), ylim=c(0,50), main="Human Development Index", xlab="HDI")
hist(HDI$HDIFemale, col=rgb(0,0,1,0.5), add=T)
legend("topright", c("Male", "Female"), fill=c("pink", rgb(0,0,1,0.5)))
#School Comparisons
hist(HDI$MaleSchooling, col="light blue", ylim=c(0,35), main="Schooling", xlab="Years In School")
hist(HDI$FemaleSchooling, col=rgb(1,0,0,0.5), add=T)
legend("topright", c("Male", "Female"), fill=c("light blue", rgb(1,0,0,0.5)))
#income comparisons
hist(HDI$MaleIncome, col="light blue", ylim=c(0,100), main="Income", xlab="Income in Dollars")
hist(HDI$FemaleIncome, col=rgb(1,0,0,0.5), add=T)
legend("topright", c("Male", "Female"), fill=c("light blue", rgb(1,0,0,0.5)))
#Create datasets
dfm <- melt(HDI[,c("Region","FemaleIncome","MaleIncome")])
dfn <- melt(HDI[,c("Region","FemaleSchooling","MaleSchooling")])
dfo <- melt(HDI[,c("Region","HDIFemale","HDIMale")])
dfp <- data.frame(dfm,dfn,dfo)
#HDI by Income
ggplot(dfp) + aes(x= value.2,y = value, color = Region) + geom_line() + xlab('Human Development Index') + ylab('Income')
#Education by Income Female
ggplot(HDI) + aes(x= FemaleSchooling,y = FemaleIncome, color = Region) + geom_line() + xlab('Years In School') + ylab('Income') + ggtitle("Female Schooling to Income")
#Education by Income Male
ggplot(HDI) + aes(x= MaleSchooling,y = MaleIncome, color = Region) + geom_line() + xlab('Years In School') + ylab('Income') + ggtitle("Male Schooling to Income")
#Lifetime
Life <- ggplot(HDI,aes(FemaleLifeExpectancy)) + geom_freqpoly(color='purple',bins=20, group = 1)
Life + geom_freqpoly(data=HDI, aes(x= MaleLifeExpectancy, group=2,bins=20, show.legend=TRUE), color="red") + xlab('Life Expectancy In Years') + ggtitle('Life Expectancy')
#HDI by Region
ggplot(HDI,aes(factor(Region),HDIFemale),fill=factor(Region)) + geom_boxplot(fill=c("light blue", "light green","pink","purple","light yellow","orange","grey"))+xlab('Regions') + ggtitle('Female Human Development Index') + ylab('HDI')
#gender development
ggplot(HDI,aes(factor(Region),GenderDevelopmet),fill=factor(Region)) + geom_boxplot(fill=c("light blue", "light green","pink","purple","light yellow","orange","grey"))+xlab('Regions') + ggtitle('Gender Development By Region') + ylab('Gender Development')
#internet access
InternetRegion <- read.csv("~/Documents/Senior Year/Spring Semester/Data Science/Final Project/InternetRegion.csv")
ggplot(InternetRegion,aes(Value)) + geom_dotplot(dotsize=.80) + ggtitle("Global Internet Access Per 100 People") + ylab("Percent of Countries") + xlab("Amount Per 100")
ggplot(InternetRegion,aes(factor(Region),Value),fill=factor(Region)) + geom_jitter(aes(color=Region)) + xlab("") + ylab("Internet Percent Per 100") + ggtitle('Internet Per 100 People')
#dotplot
ggplot(InternetRegion,aes(Value)) + geom_dotplot(aes(fill=Region), dotsize=.80) + ggtitle("Global Internet Access Per 100 People") + ylab("Percent of Countries") + xlab("Amount Per 100")
#literacy
YouthLiteracy1524 <- read.csv("~/Documents/Senior Year/Spring Semester/Data Science/Final Project/YouthLiteracy1524.csv")
ggplot(YouthLiteracy1524,aes(Value)) + geom_histogram(aes(fill=Subgroup)) + ggtitle("Youth Literacy") + ylab("Percent of Countries") + xlab("Percent Able to Read")
#beat wife
ggplot(BeatWife,aes(Value)) + geom_dotplot(aes(fill=Subgroup),dotsize=1.5) + ggtitle("People Who Believe It Is Ok To Beat Wife") + ylab("Percent of Countries") + xlab("Percent That Think It is OK")
#map graph
mapCountryData(bw, nameColumnToPlot="Value",colourPalette = c("#eff3ff","#c6dbef", "#9ecae1", "#6baed6", "#4292c6", "#2171b5","#084594"), catMethod="fixedWidth",mapTitle = "Believe Husband Has Right To Beat Wife")


