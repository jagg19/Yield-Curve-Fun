#Load libraries
if("ustyc" %in% rownames(installed.packages()) == FALSE){
  install.packages("ustyc")
}  else{ require(ustyc)}


if("tidyr" %in% rownames(installed.packages()) == FALSE){
  install.packages("tidyr")
}  else{ require(tidyr)}


if("stringr" %in% rownames(installed.packages()) == FALSE){
  install.packages("stringr")
}  else{ require(stringr)}



if("ggplot2" %in% rownames(installed.packages()) == FALSE){
  install.packages("ggplot2")
}  else{ require(ggplot2)}



#Get yield curve data, first compare recent yields to the yields post financial crisis

#Here I choose the last 6 years
my.df.yc_18 <- getYieldCurve(year = 2018)$df
my.df.yc_17 <- getYieldCurve(year = 2017)$df
my.df.yc_16 <- getYieldCurve(year = 2016)$df
my.df.yc_15 <- getYieldCurve(year = 2015)$df
my.df.yc_14 <- getYieldCurve(year = 2014)$df
my.df.yc_13 <- getYieldCurve(year = 2013)$df

#Combine it to one table
yc_13_18 <- rbind(my.df.yc_13,my.df.yc_14,my.df.yc_15, my.df.yc_16, my.df.yc_17, my.df.yc_18)
yc_13_18$ref_date <- as.Date(rownames(yc_13_18))

#Fill in zeros - I noticed a 0 when I plotted this so I fixed it up here before calling plot
which(yc_13_18$BC_10YEAR == 0)
yc_13_18[1073,] <- yc_13_18[1072,]


#I choose to plot yields during the most recent rate raising enviroment, against prior 6 year historic yields.
my.df.yc_07 <- getYieldCurve(year = 2007)$df
my.df.yc_06 <- getYieldCurve(year = 2006)$df
my.df.yc_05 <- getYieldCurve(year = 2005)$df
my.df.yc_04 <- getYieldCurve(year = 2004)$df
my.df.yc_03 <- getYieldCurve(year = 2003)$df
my.df.yc_02 <- getYieldCurve(year = 2002)$df
my.df.yc_01 <- getYieldCurve(year = 2001)$df
my.df.yc_00 <- getYieldCurve(year = 2000)$df


#Combine it to one table
yc_00_07 <- rbind(my.df.yc_00,my.df.yc_01,my.df.yc_02,my.df.yc_03,my.df.yc_04, my.df.yc_05, my.df.yc_06, my.df.yc_07)
yc_00_07$ref_date <- as.Date(rownames(yc_00_07))


#10/2 year spread for both time periods
ten_two_spread.chart_13_18 <-yc_13_18[,9]-yc_13_18[,5]
ten_two_spread.chart_00_07 <-yc_00_07[,9]-yc_00_07[,5]


#Plot all 4 charts in one window
par(mfrow=c(2,2))
plot(yc_00_07$ref_date, yc_00_07[,9], type = "l", main ="10 yr 2000/07", xlab = "Dates", ylab = "Yield%")
plot(yc_13_18$ref_date, yc_13_18[,9], type = "l", main ="10 yr 2013/today", xlab = "Dates", ylab = "Yield%")
plot(yc_00_07$ref_date,ten_two_spread.chart_00_07, type = "l", main ="10-2 spread 2000/07", xlab = "Dates", ylab = "Yield%")
plot(yc_13_18$ref_date,ten_two_spread.chart_13_18, type = "l", main ="10-2 spread 2013/today", xlab = "Dates", ylab = "Yield%")



#Now focus on YTD changes in yields + analyze the yield curve
my.df.yc <- my.df.yc_18
my.df.yc$ref.date <- as.Date(rownames(my.df.yc))


#Change to long format and convert to factor
my.df.yc_gather <-  gather(data = my.df.yc,
                  key = "maturity",
                  value = "rate",
                  -ref.date)


my.df.yc_gather$maturity <- as.factor(my.df.yc_gather$maturity)


#Keep only longer term yields (names with YEAR)
idx <- str_detect(my.df.yc_gather$maturity, "YEAR")
my.df.yc_gather_yr_only <- my.df.yc_gather[idx,]



#Change name to year number with regex
# obs: regex ([0-9]+) extracts all numbers within a string - This takes the 1 year, 2 year, 3 year, 5 year, 10 year, etc and puts it in order with year number
out <- str_extract_all(string = my.df.yc_gather_yr_only$maturity,
                       pattern = "([0-9]+)")

my.df.yc_gather_yr_only$maturity <- as.numeric(out)


#Keep only last date of each, which is most recent update of data (todays yield curve), and the previous days to calculate the change
last.date <- max(my.df.yc_gather_yr_only$ref.date)
previous.date <- last.date - 1

my.df.yc_gather_yr_only_last_date <- my.df.yc_gather_yr_only[my.df.yc_gather_yr_only$ref.date == last.date,]
my.df.yc_gather_yr_only_previous_date <- my.df.yc_gather_yr_only[my.df.yc_gather_yr_only$ref.date == previous.date,]

#Calculate the spread between 10 year yield and two year yield
tod <- my.df.yc_gather_yr_only_last_date$rate[6] - my.df.yc_gather_yr_only_last_date$rate[2]
yest <- my.df.yc_gather_yr_only_previous_date$rate[6] - my.df.yc_gather_yr_only_previous_date$rate[2]
ten_two_spread <- paste(tod*100, "Basis Points,","a", round((tod-yest)/yest*100,2),"% change from yesterday")


#Plot it!
p <- ggplot(my.df.yc_gather_yr_only_last_date, aes(x=maturity, y=rate))
p <- p + geom_point(size=2)
p <- p + geom_line(size=1)
p <- p + labs(x = "Maturity (years)", 
              y="Yield Rate",
              title = paste0("US Yield Curve (",last.date,")"))


#Plot 10-2 spread over time
par(mfcol=c(1,1))
ten_two_spread.chart <- my.df.yc[,9]-my.df.yc[,5]
plot(my.df.yc$ref.date,ten_two_spread.chart, type = "l")


#Set number of periods 
n.periods <- 5


#Set sequence of observations
my.seq <- floor(seq(1,nrow(my.df.yc_gather_yr_only), length.out = n.periods))


#Get actual dates from sequence
my.dates <- my.df.yc_gather_yr_only$ref.date[my.seq]


#Find rows for dates in df
idx <- my.df.yc_gather_yr_only$ref.date %in% my.dates
my.df.yc.periods <- my.df.yc_gather_yr_only[idx, ]


#Plot it!
p1 <- ggplot(my.df.yc.periods, aes(x=maturity, 
                                  y=rate, 
                                  color= factor(ref.date)))
p1 <- p1 + geom_point(size=2)
p1 <- p1 + geom_line(size=1)
p1 <- p1 + labs(x = 'Maturity (years)', 
              y='Yield Rate',
              title = 'US Yield Curve')

#========================================================================================================================================
#Plot the yield curve over 5 periods YTD to visualize the change over this period.
print(p1)

#Plot yield curve
print(p)

#Dataframe of today's yields in the yield curve (today's yields are updated after market close)
my.df.yc_gather_yr_only_last_date

#Lets find out the spread between 10 & 2 year yield, and show the %change from yesterday
ten_two_spread

