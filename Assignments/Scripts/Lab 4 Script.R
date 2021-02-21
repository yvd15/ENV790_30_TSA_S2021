getwd()
setwd("/Users/yashdoshi/Desktop/Duke/Courses/Spring 2021/Time Series Analysis/Labs/Lab Work/ENV790_30_TSA_S2021/Data")

library(stats)
library(forecast)
library(readxl)
library(Kendall)
eia = read_excel("Table_10.1_Renewable_Energy_Production_and_Consumption_by_Source.xlsx")
eia$Month = as.Date(eia$Month, format = "%m/%y")
eia[2:3] = NULL
eia[5:12] = NULL

#-------------------------------------------

#QUESTION 1

eia_ts = ts(eia[,2:4], start = 1973, frequency = 12)

library(ggplot2)

#Initial Plot
#Total Biomass Energy Production
p = ggplot(data = eia, aes(x = Month, 
                           y = `Total Biomass Energy Production (Trillion Btu)`)) + 
  geom_line(color = "#69b3a2") + 
  xlab("Time") + 
  ylab("Total Biomass Energy Production") +
  theme_bw() +
  ggtitle("Plot of Total Biomass Energy Production (Trillion Btu)") +
  scale_x_date(date_minor_breaks = "2 years", date_labels = "%Y %b") +
  geom_smooth(method = "lm", color = 2)
p

acf1 = Acf(eia$`Total Biomass Energy Production (Trillion Btu)`, lag = 40,
           plot = TRUE, main = "ACF of Total Biomass Energy Production")

pacf1 = Pacf(eia$`Total Biomass Energy Production (Trillion Btu)`, lag = 40,
             plot = TRUE, main = "PACF for Total Biomass Energy Production")


#Total Renewable Energy Production

q = ggplot(data = eia, aes(x = Month, 
                           y = `Total Renewable Energy Production (Trillion Btu)`)) +
  geom_line(color = "#69b3a2") +
  xlab("Time") +
  ylab("Total Renewable Energy Production (Trillion Btu)") +
  theme_bw() +
  ggtitle("Plor of Total Renewable Energy Production (Trillion Btu)") +
  geom_smooth(method = "lm", col = 2) +
  scale_x_date(date_minor_breaks = "2 years", date_labels = "%Y %b")
q

acf2 = Acf(eia$`Total Renewable Energy Production (Trillion Btu)`, lag = 40,
           plot = TRUE, main = "ACF for Total Renewable Energy Production")

pacf2 = Pacf(eia$`Total Renewable Energy Production (Trillion Btu)`, lag = 40,
             plot = TRUE, main = "PACF for Total Renewable Energy Production")


#Hydroelectric Power Consumption

he = ggplot(data = eia, aes(x = Month, y = `Hydroelectric Power Consumption (Trillion Btu)`)) +
  geom_line(color = "#69b3a2") +
  xlab("Time") +
  ylab("Hydroelectric Power Consumption (Trillion Btu)") +
  theme_bw() +
  geom_smooth(method = "lm", col = "2") +
  ggtitle("Plot for Hydroelectric Power Consumption (Trillion Btu)") +
  scale_x_date(date_minor_breaks = "2 years", date_labels = "%Y %b")
he

acf3 = Acf(eia$`Hydroelectric Power Consumption (Trillion Btu)`, lag = 40,
           plot = TRUE, main = "ACF for Hydroelectric Power Consumption")
pacf3 = Pacf(eia$`Hydroelectric Power Consumption (Trillion Btu)`, lag = 40,
             plot = TRUE, main = "PACF for Hydroelectric Power Consumption")  


#-------------------------------------------------------------

#Differencing the time series

diff1 = diff(eia_ts[,1], lag = 1)
plot(diff1, col = "2", ylab = "Difference of lag 1",
     main = "Differencing Total Biomass Energy Production at Lag 1")
abline(h = mean(diff1), col = "darkorchid4", lwd = 3)


diff2 = diff(eia_ts[,2], lag = 1)
plot(diff2, col = "2", ylab = "Difference of lag 1", 
     main = "Differencing otal Renewable Energy Production at Lag 1")
abline(h = mean(diff2), col = "darkorchid4", lwd = 3)


diff3 = diff(eia_ts[,3], lag = 1)
plot(diff3, col = "2", ylab = "Difference of lag 1", 
     main = "Differencing Hydroelectric Power Consumption at Lag 1")
abline(h = mean(diff3), col = "darkorchid4", lwd = 3)

#--------------------------------------------------------------

#QUESTION 2

#Mann-Kendall Test

mk1 = MannKendall(eia_ts[,1])
summary(mk1)

mk2 = MannKendall(eia_ts[,2])
summary(mk2)

mk3 = SeasonalMannKendall(eia_ts[,3])
summary(mk3)


#Spearman’s Rank Correlation Coefficient 

my_date = as.numeric(eia$Month)

sp1 = cor.test(eia$`Total Biomass Energy Production (Trillion Btu)`, my_date, 
               method = "spearman", exact = FALSE)
sp1

sp2 = cor.test(eia$`Total Renewable Energy Production (Trillion Btu)`, my_date,
               method = "spearman", exact = FALSE)
sp2

sp3 = cor.test(eia$`Hydroelectric Power Consumption (Trillion Btu)`, my_date,
               method = "spearman", exact = FALSE)
sp3


#--------------------------------------------------------------------
#--------------------------------------------------------------------

#QUESTION 3

eia2 = read_excel("Table_10.1_Renewable_Energy_Production_and_Consumption_by_Source.xlsx")
eia2$Month = as.Date(eia2$Month, format = "%m/%y")
eia2[2:7] = NULL
eia2[4:8] = NULL
str(eia2)
eia2[,2] = as.numeric(eia2$`Solar Energy Consumption (Trillion Btu)`)
eia2[,3] = as.numeric(eia2$`Wind Energy Consumption (Trillion Btu)`)

eia2 = na.omit(eia2)

head(eia2)

#-------------------------------------------------------------------

#QUESTION 4

#Solar Energy Consumption
a = ggplot(data = eia2, aes(x = Month, 
                           y = `Solar Energy Consumption (Trillion Btu)`)) + 
  geom_line(color = "orange1") + 
  xlab("Time") + 
  ylab("Solar Energy Consumption (Trillion Btu)") +
  theme_bw() +
  ggtitle("Plot of Solar Energy Consumption (Trillion Btu)") +
  scale_x_date(date_minor_breaks = "2 years", date_labels = "%Y %b")
a

#Wind Energy Consumption
b = ggplot(data = eia2, aes(x = Month, 
                            y = `Wind Energy Consumption (Trillion Btu)`)) +
  geom_line(color = "turquoise3") +
  xlab("Time") + ylab("Wind Energy Consumption (Trillion Btu)") +
  theme_bw() +
  ggtitle("Plot of Wind Enegry Consumption (Trillion Btu)") +
  scale_x_date(date_minor_breaks = "2 years", date_labels = "%Y %b")
b

#--------------------------------------------------------------------------

#QUESTION 5

eia2_ts = ts(eia2[,2:3], start = 1984, frequency = 12)
head(eia2_ts)


decompose_solar = decompose(eia2_ts[,1], type = "additive")
plot(decompose_solar, col = "orange1")

decompose_wind = decompose(eia2_ts[,2], type = "additive")
plot(decompose_wind, col = "turquoise3")

#-------------------------------------------------------------------------

#QUESTION 6

decomp_solar = decompose(eia2_ts[,1], type = "multiplicative")
plot(decomp_solar, col = "orange1")

decomp_wind = decompose(eia2_ts[,2], type = "multiplicative")
plot(decomp_wind, col = "turquoise3")


acf30 = Acf(eia2$`Solar Energy Consumption (Trillion Btu)`, lag = 40)
