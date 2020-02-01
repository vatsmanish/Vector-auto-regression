#setworking direcotry
setwd("C:\\Users\\MANDY\\Desktop\\dataset")
#load the data set
gdp = read.csv("gdp.csv")
#View dataset
View(df)

li
install.packages("WDI")
install.packages("reshape2")
install.packages("wbstats")
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("scales")
install.packages("useful")
install.packages("numDiffs")
install.packages("forecast")
install.packages("vars")
install.packages("VAR")

gdp <- wb(country = c("US","CA","AUS","IND","PAK","SAU"),
                       indicator = c("NY.GDP.PCAP.CD","NY.GDP.MKTP.CD"),
startdate = 1960, enddate = 2011)
# give it good names
names(gdp) <- c("iso2c","Country","Year","PerCapGDP","GDP")
ggplot(gdp,aes(Year,PerCapGDP,color=Country,linetype=Country))+
   geom_line()+scale_y_discrete(label='dollar')
# no. of rows in the dataset
nrow(gdp)
#head of the dataset
head(gdp)
gdpcast <- dcast(Year ~ Country,
                 data = gdp[, c("Country","Year","PerCapGDP")],
                 value.var = "PerCapGDP")
head(gdpcast)

#convert first 10 rows
gdpTs <- ts(data = gdpcast[, -1], start = min(gdpcast$Year),
              end = max(gdpcast$Year))
plot(gdpTs, plot.type="single",col=1:8)
