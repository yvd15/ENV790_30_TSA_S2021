library(ggplot2)
today = Sys.Date()
library(lubridate)
format(today, format = "%B")
format(today, format = "%y")
format(today, format = "%M")
str_today = "2021-February-03"
class(str_today)
date = ymd(str_today)
date
lubridate::origin


#echo = TRUE,tidy.opts=list(width.cutoff=80), tidy=FALSE) 


#-----------------------------------------

#QUESTION 1

setwd("/Users/yashdoshi/Desktop/Duke/Courses/Spring 2021/Time Series Analysis/Labs/Lab Work/ENV790_30_TSA_S2021/Data")
library(readxl)
eia = read_excel("Table_10.1_Renewable_Energy_Production_and_Consumption_by_Source.xlsx")
eia$Month = as.Date(eia$Month, format = "%m/%y")
eia[2:3] = NULL
eia[5:12] = NULL

eia = as.data.frame(eia)


eiats = ts(data = eia[,2:4], start = 1973, frequency = 12)

btu = ncol(eia)

for(i in 1:btu){
  print(ggplot(eia, aes(x = eia$Month, y = eia[,i])))
}

for(i in 2:btu){
  par(mfrow = c(1,3))
  plot(eia[,i], col = "#69b3a2", ylab = "Trillion Btu", type = "l")
  acf(eia[,i], lag.max = 40)
  pacf(eia[,i])
}

tsdisplay(eiats)

#--------------------------

#QUESTION 3


#TOTAL BIOMASS ENERGY PRODUCTION

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

t = c(1:nrow(eia))

lm1 = lm(eia$`Total Biomass Energy Production (Trillion Btu)` ~ t)
summary(lm1)

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

lm2 = lm(eia$`Total Renewable Energy Production (Trillion Btu)` ~ t)
summary(lm2)

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

lm3 = lm(eia$`Hydroelectric Power Consumption (Trillion Btu)` ~ t)
summary(lm3)

#------------------------------

#QUESTION 4

#Biomass
b0 = as.numeric(lm1$coefficients[1])
b1 = as.numeric(lm1$coefficients[2])


detrend_data = eia$`Total Biomass Energy Production (Trillion Btu)` - (b0 + b1*t)

pe = ggplot(data = eia, aes(x = Month, 
                            y = `Total Biomass Energy Production (Trillion Btu)`)) + 
  geom_line(color = "#69b3a2") + 
  xlab("Time") + 
  ylab("Total Biomass Energy Production") +
  theme_bw() +
  ggtitle("Plot of Total Biomass Energy Production (Trillion Btu)") +
  scale_x_date(date_minor_breaks = "2 years", date_labels = "%Y %b") +
  geom_smooth(method = "lm", color = 2) +
  geom_line(mapping = aes(y = detrend_data), col = "darkorchid4") +
  geom_smooth(aes(y = detrend_data), col = "4", method = "lm") 
pe
  
  
#Renewable Energy
b00 = as.numeric(lm2$coefficients[1])
b11 = as.numeric(lm2$coefficients[2])

detrend_data2 = eia$`Total Renewable Energy Production (Trillion Btu)` - (b00 + b11*t)

qe = ggplot(data = eia, aes(x = Month, 
                            y = `Total Renewable Energy Production (Trillion Btu)`)) +
  geom_line(color = "#69b3a2") +
  xlab("Time") +
  ylab("Total Renewable Energy Production (Trillion Btu)") +
  theme_bw() +
  ggtitle("Plor of Total Renewable Energy Production (Trillion Btu)") +
  geom_smooth(method = "lm", col = 2) +
  scale_x_date(date_minor_breaks = "2 years", date_labels = "%Y %b") +
  geom_line(mapping = aes(y = detrend_data2), col = "darkorchid4") +
  geom_smooth(aes(y = detrend_data2), col = "4", method = "lm")
qe

#Hydroelectric Consumption
b000 = as.numeric(lm3$coefficients[1])
b111 = as.numeric(lm3$coefficients[2])

detrend_data3 = eia$`Hydroelectric Power Consumption (Trillion Btu)` - (b000 + b111*t)

hee = ggplot(data = eia, aes(x = Month, y = `Hydroelectric Power Consumption (Trillion Btu)`)) +
  geom_line(color = "#69b3a2") +
  xlab("Time") +
  ylab("Hydroelectric Power Consumption (Trillion Btu)") +
  theme_bw() +
  geom_smooth(method = "lm", col = "2") +
  ggtitle("Plot for Hydroelectric Power Consumption (Trillion Btu)") +
  scale_x_date(date_minor_breaks = "2 years", date_labels = "%Y %b") +
  geom_line(mapping = aes(y = detrend_data3), col = "darkorchid4") +
  geom_smooth(aes(y = detrend_data3), col = "4", method = "lm")
hee

#-----------------------

#QUESTION 5

#ACF 

acf1 = acf(detrend_data, lag.max = 40, plot = FALSE)
plot(acf1, main = "Detrended ACF of Total Biomass Energy Production", col = "2")

acf2= acf(detrend_data2, lag.max = 40, plot = FALSE)
plot(acf2, main = "Detrended ACF of Total Renewable Energy Production", col = "2")

acf3 = acf(detrend_data3, lag.max = 40, plot = FALSE)
plot(acf3, main = "Detrended ACF of Hydroelectric Power Consumption", col = "2")

#PACF

pacf1 = pacf(detrend_data, lag.max = 40, plot = FALSE)
plot(pacf1, main = "Detrended PACF of Total Biomass Energy Production", 
     col = "darkorchid4")

pacf2 = pacf(detrend_data2, lag.max = 40, plot = FALSE)
plot(pacf2, main = "Detrended PACF of Total Renewable Energy Production", 
     col = "darkorchid4")

pacf3 = pacf(detrend_data3, lag.max = 40, plot = FALSE)
plot(pacf3, main = "Detrended PACF of Hydroelectric Power Consumption", 
     col = "darkorchid4")

#--------------------------------------------

#QUESTION 6

dummies = seasonaldummy(eiats[,3])
seaslm = lm(eia$`Hydroelectric Power Consumption (Trillion Btu)` ~ dummies)
summary(seaslm)

beta0 = seaslm$coefficients[1]
beta1 = seaslm$coefficients[2:12]

nobs = nrow(eia)

seas_comp = array(0,nobs)

for(i in 1:nobs){
  seas_comp[i] = (beta0 + beta1 %*% dummies[i,])
}

ggplot(eia, aes(x = Month, y = `Hydroelectric Power Consumption (Trillion Btu)`)) +
  geom_line(color = "#69b3a2") +
  theme_bw() +
  ggtitle("Plot for Original Hydroelectric Power Consumption  \n and its Seasonal component") +
  scale_x_date(date_minor_breaks = "2 years", date_labels = "%Y %b") +
  geom_line(aes(y = seas_comp), col = "2")

#-------------------------------------------

#QUESTION 7

deseason = eia[,4] - seas_comp

ggplot(eia, aes(x = Month, y = `Hydroelectric Power Consumption (Trillion Btu)`)) +
  geom_line(color = "#69b3a2") +
  theme_bw() +
  ggtitle("Plot for Seasoned and Deseasoned Hydroelectric Power Consumption") +
  scale_x_date(date_minor_breaks = "2 years", date_labels = "%Y %b") +
  geom_line(aes(y = deseason), col = "2")

#-------------------------------------------

#QUESTION 8

acf4 = acf(deseason,lag.max = 40, plot = FALSE)
plot(acf4, main = "Deseasoned ACF of Hydroelectric Power Consumption")

pacf4 = pacf(deseason, lag.max = 40, plot = FALSE)
plot(pacf4, main = "Deseasoned PACF of Hydroelectric Power Consumption")
