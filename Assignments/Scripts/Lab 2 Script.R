library(forecast)
library(tseries)
library(dplyr)
getwd()

setwd("/Users/yashdoshi/Desktop/Duke/Courses/Spring 2021/Time Series Analysis/Labs/Lab Work/ENV790_30_TSA_S2021/Data")
library(readxl)
eia = read_excel("Table_10.1_Renewable_Energy_Production_and_Consumption_by_Source.xlsx")

str(eia)
eia$Month = as.Date(eia$Month, format = "%m/%y")
eia[2:3] = NULL
eia[5:12] = NULL
head(eia)

eiats = ts(data = eia, start = 1, end = 574, frequency = 1)
eiats
is.ts(eiats)
head(eiats)

meantbep = mean(eia$`Total Biomass Energy Production (Trillion Btu)`)
meantbep
meantrep = mean(eia$`Total Renewable Energy Production (Trillion Btu)`)
meantrep
meanhepc = mean(eia$`Hydroelectric Power Consumption (Trillion Btu)`)
meanhepc

summary(eiats)
sd1 = sd(eia$`Total Biomass Energy Production (Trillion Btu)`)
sd1
sd2 = sd(eia$`Total Renewable Energy Production (Trillion Btu)`)
sd2
sd3 = sd(eia$`Hydroelectric Power Consumption (Trillion Btu)`)
sd3

library(ggplot2)
library(hrbrthemes)
#-------------------------------------------------------------------------------

#CORRELATION

cor1 = cor(eia$`Total Biomass Energy Production (Trillion Btu)`, 
    eia$`Total Renewable Energy Production (Trillion Btu)`)
cor1

cor2 = cor(eia$`Total Biomass Energy Production (Trillion Btu)`,
           eia$`Hydroelectric Power Consumption (Trillion Btu)`)
cor2

cor3 = cor(eia$`Total Renewable Energy Production (Trillion Btu)`,
           eia$`Hydroelectric Power Consumption (Trillion Btu)`)
cor3
#-------------------------------------------------------------------------------
#Plot for Total Biomass Energy Production
p = ggplot(data = eiats, aes(x = Month, 
               y = `Total Biomass Energy Production (Trillion Btu)`)) + 
  geom_line(color = "#69b3a2") + 
  xlab("Time") + 
  ylab("Total Biomass Energy Production") +
  theme_bw() +
  ggtitle("Plot of Total Biomass Energy Production \n (Trillion Btu)")
p + scale_x_date(date_minor_breaks = "2 years", date_labels = "%Y %b") +
  geom_hline(yintercept = mean(eiats$`Total Biomass Energy Production (Trillion Btu)`), 
               color = 2)

#ACF and PACF
acf1 = acf(eia$`Total Biomass Energy Production (Trillion Btu)`, lag.max = 40, 
    plot = FALSE)
plot(acf1, main = "Total Biomass Energy Production", col = 2)
pacf1 = pacf(eia$`Total Biomass Energy Production (Trillion Btu)`, lag.max = 40,
     plot = FALSE)
plot(pacf1, main = "Total Biomass Energy Production", col = "darkorchid4")

#--------------------------------------------------------------------------------

#Plot for Renewable Energy Production
q = ggplot(eia, aes(x = Month, 
                    y = `Total Renewable Energy Production (Trillion Btu)`)) +
  geom_line(color = "#69b3a2") + 
  xlab("Time") + 
  ylab("Total Renewable Energy Production") +
  theme_bw() +
  ggtitle("Plot of Total Renewable Energy Production (Trillion Btu)")
q + geom_hline(yintercept = mean(eia$`Total Renewable Energy Production (Trillion Btu)`),
                color = 2) + scale_x_date(date_minor_breaks = "2 years",
                                          date_labels = "%Y %b")


#ACF and PACF
acf2 = acf(eia$`Total Renewable Energy Production (Trillion Btu)`, lag.max = 40, 
    plot = FALSE)
plot(acf2, main = "Total Renewable Energy Production", col = 2)
pacf2 = pacf(eia$`Total Renewable Energy Production (Trillion Btu)`, lag.max = 40,
     plot = FALSE)
plot(pacf2, main = "Total Renewable Energy Production", col = "darkorchid4")

#-------------------------------------------------------------------------------

#Plot for Hydroelectric Power Consumption
he = ggplot(eia, aes(x = Month, 
                     y = `Hydroelectric Power Consumption (Trillion Btu)`)) +
  geom_line(color = "#69b3a2") + 
  xlab("Time") + 
  ylab("Hydroelectric Power Consumption") +
  theme_bw() +
  ggtitle("Plot of Hydroelectric Power Consumption (Trillion Btu)")
he + geom_hline(yintercept = mean(eia$`Hydroelectric Power Consumption (Trillion Btu)`),
                color = 2) + scale_x_date(date_minor_breaks = "2 years",
                                          date_labels = "%Y %b")


#ACF and PACF
acf3 = acf(eia$`Hydroelectric Power Consumption (Trillion Btu)`, lag.max = 40, 
            plot = FALSE)
plot(acf3, main = "ACF of Hydroelectric Power Consumption", col = 2)
pacf3 = pacf(eia$`Hydroelectric Power Consumption (Trillion Btu)`,lag.max = 40,
             plot = FALSE)
plot(pacf3, main = "PACF of Hydroelectric Power Consumption", col = "darkorchid4")

