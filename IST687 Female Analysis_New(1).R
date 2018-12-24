library(data.table)
library(plotly)
library(ggplot2)
library(sqldf)
install.packages("lubridate")
library(lubridate)
install.packages("arules")
library(arules)
install.packages("kernlab") 
library(kernlab)
install.packages("arulesViz")
library(arulesViz)

setwd("~/Downloads/iSchool/2018 Fall/IST687 Data Science/IST687Project")
csvFile <- "~/Downloads/iSchool/2018 Fall/IST687 Data Science/IST687Project/Satisfaction Survey.csv"
#Importing the dataset
Satisfaction<-read.csv(file = "Satisfaction Survey.csv")

#Checking structure and summary of the dataset
str(Satisfaction)
summary(Satisfaction)
View(Satisfaction)

#Converting satisfaction column to character and then to numeric
Satisfaction$Satisfaction<-as.character(Satisfaction$Satisfaction)
Satisfaction$Satisfaction<-as.numeric(Satisfaction$Satisfaction)

##Data Cleaning

#Removing NA's
Satisfaction<-Satisfaction[complete.cases(Satisfaction[ , 1:28]),]
#removing redundant values from individual columns
Satisfaction$Orgin.City<-gsub(",.*","",Satisfaction$Orgin.City)
Satisfaction$Destination.City<-gsub(",.*","",Satisfaction$Destination.City)

#change column name
colnames(Satisfaction) <- gsub("\\.","_",colnames(Satisfaction))

##Data analysis based on Female Customers

#create a dataframe only contains female data
femaledf<-sqldf("select * from Satisfaction where Gender ='Female'")
View(femaledf)

# Type of reason for Travel #
travel_reason = data.frame(table(as.factor(femaledf$Type_of_Travel)))
names(travel_reason) <- c("Travel Type","Count")
travel_reason_plot <- plot_ly(travel_reason, x = travel_reason$`Travel Type`, y =travel_reason$Count, type = 'bar', name = 'Travel Reasons') 
travel_reason_plot
###most females travel for business(focus on business travel)

# Type of Class #
class = data.frame(table(as.factor(femaledf$Class)))
names(class) <- c("Class","Count")
class_plot <- plot_ly(class, x =class$Class,
                                y =class$Count, type = 'bar', name = 'Class')
class_plot
###most females fly with ECO class(focus on economy class)

### Since There are lot more people travelling in ECO class,
### we are going to focus our analysis in this business segment
ECO_fdf<-sqldf("select * from femaledf where Class='Eco'")
View(ECO_fdf)

# Type of reason for Travel for ECO FEMALE #
travel_ECO_F_reason = data.frame(table(as.factor(ECO_fdf$Type_of_Travel)))
names(travel_ECO_F_reason) <- c("Travel Type","Count")
travel_ECO_F_reason_plot <- plot_ly(travel_ECO_F_reason, x = travel_ECO_F_reason$`Travel Type`, y =travel_reason$Count, type = 'bar', name = 'Travel Reasons') 
travel_ECO_F_reason_plot
###most females in ECO class travel for business

#Frequency of travel month#
ECO_fdf$Flight_date <- mdy(ECO_fdf$Flight_date)
month_travel=month(ECO_fdf$Flight_date)
month_travel_freq= data.frame(table(month_travel))
month_travel_freq$month_travel <- c("Jan","Feb","March")
month_travel_plot = plot_ly(month_travel_freq, x = month_travel_freq$month_travel,
                            y =month_travel_freq$Freq, type = 'bar', name = 'Month Travel')
month_travel_plot
###In these 3 months, they all travel over 15k and travel most in March (no focus)

#focus on business travel
BEFdf<-sqldf("select * from ECO_fdf where Type_of_Travel='Business travel'")
View(BEFdf)
str(BEFdf)
summary(BEFdf)

#convert Airline_status into numbers
BEFdf$Airline_Status_as_Number<-NA
BEFdf$Airline_Status_as_Number[which(BEFdf$Airline_Status=="Blue")]<-1
BEFdf$Airline_Status_as_Number[which(BEFdf$Airline_Status=="Silver")]<-2
BEFdf$Airline_Status_as_Number[which(BEFdf$Airline_Status=="Gold")]<-3
BEFdf$Airline_Status_as_Number[which(BEFdf$Airline_Status=="Platinum")]<-4
View(BEFdf$Airline_Status_as_Number)
View(BEFdf)

summary(BEFdf$Flight_cancelled)
# No   Yes 
# 31378     0 
#Therefore, we do not consider the variable Flight_cancelled since we have no cancel data in this subdataset

#linear model
#age
m1<-lm(formula=Satisfaction~Age,data=BEFdf)
summary.lm(m1)
#Call:
# lm(formula = Satisfaction ~ Age, data = BEFdf)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2.7218 -0.6881  0.2926  0.3119  1.3480 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  3.7482982  0.0140963 265.906  < 2e-16 ***
#   Age         -0.0012038  0.0003166  -3.802 0.000144 ***
#   ---
#   Signif. codes:  
#   0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.7504 on 31376 degrees of freedom
# Multiple R-squared:  0.0004606,	Adjusted R-squared:  0.0004287 
# F-statistic: 14.46 on 1 and 31376 DF,  p-value: 0.0001436

#price
m2<-lm(formula=Satisfaction~Price_Sensitivity,data=BEFdf)
summary.lm(m2)
# Call:
#   lm(formula = Satisfaction ~ Price_Sensitivity, data = BEFdf)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2.7531 -0.6641  0.2914  0.3359  1.4250 
# 
# Coefficients:
#   Estimate Std. Error t value
# (Intercept)        3.753129   0.010946 342.865
# Price_Sensitivity -0.044525   0.008032  -5.543
# Pr(>|t|)    
# (Intercept)        < 2e-16 ***
#   Price_Sensitivity 2.99e-08 ***
#   ---
#   Signif. codes:  
#   0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.7502 on 31376 degrees of freedom
# Multiple R-squared:  0.0009784,	Adjusted R-squared:  0.0009465 
# F-statistic: 30.73 on 1 and 31376 DF,  p-value: 2.994e-08

#X__of_Flight_with_other_Airlines
m3<-lm(formula=Satisfaction~X__of_Flight_with_other_Airlines,data=BEFdf) 
summary.lm(m3)
# Call:
#   lm(formula = Satisfaction ~ X__of_Flight_with_other_Airlines, 
#      data = BEFdf)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2.7136 -0.6946  0.3030  0.3054  1.3072 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                      3.6923225  0.0063704  579.60   <2e-16 ***
#   X__of_Flight_with_other_Airlines 0.0004623  0.0004530    1.02    0.308    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.7505 on 31376 degrees of freedom
# Multiple R-squared:  3.318e-05, Adjusted R-squared:  1.309e-06 
# F-statistic: 1.041 on 1 and 31376 DF,  p-value: 0.3076

#No__of_other_Loyalty_Cards
m4<-lm(formula=Satisfaction~No__of_other_Loyalty_Cards,data=BEFdf) 
summary.lm(m4)
# Call:
#   lm(formula = Satisfaction ~ No__of_other_Loyalty_Cards, data = BEFdf)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2.7337 -0.6606  0.2663  0.3394  1.4857 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                 3.733704   0.005658 659.848   <2e-16 ***
#   No__of_other_Loyalty_Cards -0.036574   0.003762  -9.721   <2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.7494 on 31376 degrees of freedom
# Multiple R-squared:  0.003003, Adjusted R-squared:  0.002971 
# F-statistic:  94.5 on 1 and 31376 DF,  p-value: < 2.2e-16

#Shopping_Amount_at_Airport
m5<-lm(formula=Satisfaction~Shopping_Amount_at_Airport,data=BEFdf) #
summary.lm(m5)
# Call:
#   lm(formula = Satisfaction ~ Shopping_Amount_at_Airport, data = BEFdf)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2.8001 -0.6864  0.3036  0.3136  1.3136 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                3.686e+00  4.845e-03 760.894  < 2e-16 ***
#   Shopping_Amount_at_Airport 3.346e-04  7.268e-05   4.604 4.16e-06 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.7503 on 31376 degrees of freedom
# Multiple R-squared:  0.0006752, Adjusted R-squared:  0.0006433 
# F-statistic:  21.2 on 1 and 31376 DF,  p-value: 4.156e-06

#Eating_and_Drinking_at_Airport
m6<-lm(formula=Satisfaction~Eating_and_Drinking_at_Airport,data=BEFdf) 
summary.lm(m6)
# Call:
#   lm(formula = Satisfaction ~ Eating_and_Drinking_at_Airport, data = BEFdf)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2.8888 -0.6793  0.2965  0.3207  1.3368 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                    3.663e+00  6.927e-03 528.842  < 2e-16 ***
#   Eating_and_Drinking_at_Airport 5.372e-04  8.669e-05   6.197 5.83e-10 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.7501 on 31376 degrees of freedom
# Multiple R-squared:  0.001222, Adjusted R-squared:  0.001191 
# F-statistic:  38.4 on 1 and 31376 DF,  p-value: 5.826e-10

#Scheduled_Departure_Hour
m7<-lm(formula=Satisfaction~Scheduled_Departure_Hour,data=BEFdf) 
summary.lm(m7)
# Call:
#   lm(formula = Satisfaction ~ Scheduled_Departure_Hour, data = BEFdf)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2.7179 -0.6889  0.2966  0.3111  1.3277 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)              3.670199   0.012617  290.90   <2e-16 ***
#   Scheduled_Departure_Hour 0.002075   0.000914    2.27   0.0232 *  
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.7505 on 31376 degrees of freedom
# Multiple R-squared:  0.0001642, Adjusted R-squared:  0.0001324 
# F-statistic: 5.153 on 1 and 31376 DF,  p-value: 0.02321

#Departure_Delay_in_Minutes
m8<-lm(formula=Satisfaction~Departure_Delay_in_Minutes,data=BEFdf) 
summary.lm(m8)
# Call:
#   lm(formula = Satisfaction ~ Departure_Delay_in_Minutes, data = BEFdf)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2.7205 -0.6951  0.2795  0.2906  2.2489 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                 3.7205059  0.0045440  818.77   <2e-16 ***
#   Departure_Delay_in_Minutes -0.0015892  0.0001141  -13.93   <2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.7482 on 31376 degrees of freedom
# Multiple R-squared:  0.006145, Adjusted R-squared:  0.006113 
# F-statistic:   194 on 1 and 31376 DF,  p-value: < 2.2e-16

#Arrival_Delay_in_Minutes
m9<-lm(formula=Satisfaction~Arrival_Delay_in_Minutes,data=BEFdf) 
summary.lm(m9)
# Call:
#   lm(formula = Satisfaction ~ Arrival_Delay_in_Minutes, data = BEFdf)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2.7240 -0.6921  0.2760  0.2866  2.3284 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)               3.7240313  0.0045499  818.49   <2e-16 ***
#   Arrival_Delay_in_Minutes -0.0017748  0.0001124  -15.79   <2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.7476 on 31376 degrees of freedom
# Multiple R-squared:  0.007889, Adjusted R-squared:  0.007857 
# F-statistic: 249.5 on 1 and 31376 DF,  p-value: < 2.2e-16

#Flight_Distance
m10<-lm(formula=Satisfaction~Flight_Distance,data=BEFdf) 
summary.lm(m10)
# Call:
#   lm(formula = Satisfaction ~ Flight_Distance, data = BEFdf)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2.7082 -0.6948  0.3019  0.3053  1.3074 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)     3.692e+00  7.097e-03 520.305   <2e-16 ***
#   Flight_Distance 5.953e-06  7.191e-06   0.828    0.408    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.7505 on 31376 degrees of freedom
# Multiple R-squared:  2.184e-05, Adjusted R-squared:  -1.003e-05 
# F-statistic: 0.6853 on 1 and 31376 DF,  p-value: 0.4078

#Price_Sensitivity
m11<-lm(formula=Satisfaction~ Price_Sensitivity,data=BEFdf)
summary.lm(m11)
# Call:
#   lm(formula = Satisfaction ~ Price_Sensitivity, data = BEFdf)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2.7531 -0.6641  0.2914  0.3359  1.4250 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)        3.753129   0.010946 342.865  < 2e-16 ***
#   Price_Sensitivity -0.044525   0.008032  -5.543 2.99e-08 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.7502 on 31376 degrees of freedom
# Multiple R-squared:  0.0009784, Adjusted R-squared:  0.0009465 
# F-statistic: 30.73 on 1 and 31376 DF,  p-value: 2.994e-08

#Flight_time_in_minutes
m12<-lm(formula=Satisfaction~Flight_time_in_minutes,data=BEFdf)
summary.lm(m12)
# Call:
#   lm(formula = Satisfaction ~ Flight_time_in_minutes, data = BEFdf)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2.7001 -0.6965  0.3014  0.3036  1.3185 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)             3.701e+00  7.835e-03  472.30   <2e-16 ***
#   Flight_time_in_minutes -3.089e-05  5.935e-05   -0.52    0.603    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.7505 on 31376 degrees of freedom
# Multiple R-squared:  8.631e-06, Adjusted R-squared:  -2.324e-05 
# F-statistic: 0.2708 on 1 and 31376 DF,  p-value: 0.6028

#status
m13<-lm(formula=Satisfaction~Airline_Status_as_Number,data=BEFdf)
summary.lm(m13)
# Call:
#   lm(formula = Satisfaction ~ Airline_Status_as_Number, data = BEFdf)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -3.2965 -0.5632  0.1924  0.4368  1.4368 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)              3.318756   0.008855  374.79   <2e-16 ***
#   Airline_Status_as_Number 0.244428   0.005073   48.18   <2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.7242 on 31376 degrees of freedom
# Multiple R-squared:  0.06888,	Adjusted R-squared:  0.06885 
# F-statistic:  2321 on 1 and 31376 DF,  p-value: < 2.2e-16

#Year_of_First_Flight
m14<-lm(formula=Satisfaction~Year_of_First_Flight,data=BEFdf)
summary.lm(m14)
#Call:
# lm(formula = Satisfaction ~ Year_of_First_Flight, data = BEFdf)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2.7659 -0.6676  0.2762  0.3324  1.3605 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)          -24.477131   2.883295  -8.489   <2e-16 ***
#   Year_of_First_Flight   0.014037   0.001437   9.772   <2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.7494 on 31376 degrees of freedom
# Multiple R-squared:  0.003034,	Adjusted R-squared:  0.003002 
# F-statistic: 95.48 on 1 and 31376 DF,  p-value: < 2.2e-16

#No_of_Flights_p_a_
m15<-lm(formula=Satisfaction~No_of_Flights_p_a_,data=BEFdf)
summary.lm(m15)
# Call:
#   lm(formula = Satisfaction ~ No_of_Flights_p_a_, data = BEFdf)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2.7752 -0.6686  0.2636  0.3266  1.6900 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)         3.7751809  0.0070216  537.65   <2e-16 ***
#   No_of_Flights_p_a_ -0.0048455  0.0003484  -13.91   <2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.7482 on 31376 degrees of freedom
# Multiple R-squared:  0.006127,	Adjusted R-squared:  0.006095 
# F-statistic: 193.4 on 1 and 31376 DF,  p-value: < 2.2e-16

#important variables:
# No_of_Flights_p_a_
# Year_of_First_Flight
# Airline_Status_as_Number
# Arrival_Delay_in_Minutes
# Departure_Delay_in_Minutes
# Eating_and_Drinking_at_Airport
# No__of_other_Loyalty_Cards

mm<-lm(formula=Satisfaction~No_of_Flights_p_a_+Airline_Status_as_Number+Year_of_First_Flight+Eating_and_Drinking_at_Airport+No__of_other_Loyalty_Cards
       +Departure_Delay_in_Minutes+Arrival_Delay_in_Minutes,data=BEFdf)
summary.lm(mm)
# Call:
#   lm(formula = Satisfaction ~ No_of_Flights_p_a_ + Airline_Status_as_Number + 
#        Year_of_First_Flight + Eating_and_Drinking_at_Airport + No__of_other_Loyalty_Cards + 
#        Departure_Delay_in_Minutes + Arrival_Delay_in_Minutes, data = BEFdf)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -3.3996 -0.5296  0.2046  0.4274  2.0541 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                    -1.800e+01  2.768e+00  -6.504 7.95e-11 ***
#   No_of_Flights_p_a_             -3.755e-03  3.413e-04 -11.002  < 2e-16 ***
#   Airline_Status_as_Number        2.331e-01  5.094e-03  45.763  < 2e-16 ***
#   Year_of_First_Flight            1.069e-02  1.379e-03   7.751 9.39e-15 ***
#   Eating_and_Drinking_at_Airport  6.093e-05  8.361e-05   0.729    0.466    
# No__of_other_Loyalty_Cards     -3.727e-02  3.656e-03 -10.192  < 2e-16 ***
#   Departure_Delay_in_Minutes      1.895e-03  4.076e-04   4.649 3.36e-06 ***
#   Arrival_Delay_in_Minutes       -3.526e-03  4.018e-04  -8.776  < 2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.7183 on 31370 degrees of freedom
# Multiple R-squared:  0.0843,	Adjusted R-squared:  0.08409 
# F-statistic: 412.6 on 7 and 31370 DF,  p-value: < 2.2e-16

#ignore Eating_and_Drinking_at_Airport,Year_of_First_Flight,Departure_Delay_in_Minutes
mm1<-lm(formula=Satisfaction~No_of_Flights_p_a_+Airline_Status_as_Number+No__of_other_Loyalty_Cards
       +Arrival_Delay_in_Minutes,data=BEFdf)
summary.lm(mm1)
# Call:
#   lm(formula = Satisfaction ~ No_of_Flights_p_a_ + Airline_Status_as_Number + 
#        No__of_other_Loyalty_Cards + Arrival_Delay_in_Minutes, data = BEFdf)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -3.3988 -0.5380  0.1998  0.4244  2.1460 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                 3.4555029  0.0119843  288.34   <2e-16 ***
#   No_of_Flights_p_a_         -0.0037303  0.0003410  -10.94   <2e-16 ***
#   Airline_Status_as_Number    0.2358195  0.0050733   46.48   <2e-16 ***
#   No__of_other_Loyalty_Cards -0.0373687  0.0036577  -10.22   <2e-16 ***
#   Arrival_Delay_in_Minutes   -0.0017214  0.0001081  -15.92   <2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.7192 on 31373 degrees of freedom
# Multiple R-squared:  0.08189,	Adjusted R-squared:  0.08178 
# F-statistic: 699.6 on 4 and 31373 DF,  p-value: < 2.2e-16

#SELECT  linear model mm since higher R-squared

#svm model

#Map each numeric attribute to a category
#Since we want to create rules, we should convert 
#the attributes that have a numeric range into buckets (ex. low or high)
createBucketSurvey1<-function(vec){
  vBuckets<-replicate(length(vec),"Medium")
  vBuckets[vec>4]<-"High"
  vBuckets[vec<2]<-"Low"
  return(vBuckets)
}

createBucketSurvey2<-function(vec){
  q<-quantile(vec,c(0.4,0.6))
  vBuckets<-replicate(length(vec),"Medium")
  vBuckets[vec<=q[1]]<-"Low"
  vBuckets[vec>q[2]]<-"High"
  return(vBuckets)
}

createBucketSurvey3<-function(vec){
  vBuckets<-replicate(length(vec),"Medium")
  vBuckets[vec>3]<-"High"
  vBuckets[vec<2]<-"Low"
  return(vBuckets)
}#for airline status

createBucketSurvey4<-function(vec){
  vBuckets<-replicate(length(vec),"Medium")
  vBuckets[vec>2]<-"High"
  vBuckets[vec<2]<-"Low"
  return(vBuckets)
}#PRICE SENSITIVITY

BEFdf$happyCust<-createBucketSurvey1(BEFdf$Satisfaction)
View(BEFdf$happyCust)

dim(BEFdf)
#[1] 31378    30
randIndex<-sample(1:dim(BEFdf)[1])
#set up the cut point that would divide the hotelSurvey dataset into a two
#thirds training set and a one third test set
cutP2_3<-floor(2*dim(BEFdf)[1]/3)
cutP2_3
#[1] 20918
#train data
trainData<-BEFdf[randIndex[1:cutP2_3],]
#test data
testData<-BEFdf[randIndex[(cutP2_3+1):dim(BEFdf)[1]],]
dim(testData)
#[1] 10460    30
dim(trainData)
#[1] 20918    30
svmOutput<-ksvm(happyCust~Age+Price_Sensitivity+Year_of_First_Flight+No_of_Flights_p_a_+
                X__of_Flight_with_other_Airlines+No__of_other_Loyalty_Cards+
                Shopping_Amount_at_Airport+Eating_and_Drinking_at_Airport+
                Scheduled_Departure_Hour+Departure_Delay_in_Minutes+
                Arrival_Delay_in_Minutes+Flight_time_in_minutes+Flight_Distance+
                Airline_Status_as_Number,
                data=trainData,kernel="rbfdot",
                kpar="automatic",C=5,cross=3,prob.model=TRUE)
svmOutput
# Support Vector Machine object of class "ksvm" 
# 
# SV type: C-svc  (classification) 
# parameter : cost C = 5 
# 
# Gaussian Radial Basis kernel function. 
# Hyperparameter : sigma =  0.0554849383573678 
# 
# Number of Support Vectors : 6324 
# 
# Objective Function Value : -1369.427 -19468.89 -1728.201 
# Training error : 0.10001 
# Cross validation error : 0.10565 
# Probability model included.
str(BEFdf)
svmPred <- predict(svmOutput, testData)
str(svmPred)
svmErr<-sum(testData[,30]!=svmPred)*100/length(svmPred)
svmErr

svmOutput2<-ksvm(happyCust~Age+Price_Sensitivity+Year_of_First_Flight+No_of_Flights_p_a_+
                  X__of_Flight_with_other_Airlines+No__of_other_Loyalty_Cards+
                  Shopping_Amount_at_Airport+Eating_and_Drinking_at_Airport+
                  Scheduled_Departure_Hour+Departure_Delay_in_Minutes+
                  Arrival_Delay_in_Minutes+Flight_time_in_minutes+Flight_Distance+
                  Airline_Status_as_Number,
                data=trainData,kernel="rbfdot",
                kpar="automatic",C=5,cross=3,prob.model=TRUE)











#arule for validation
#attributes range from 0 to 5 use function createBucketSurvey1
happyCust<-createBucketSurvey1(BEFdf$Satisfaction)
View(happyCust)
#other attributes use function createBucketSurvey2
depart_delay<-createBucketSurvey2(BEFdf$Departure_Delay_in_Minutes)
View(depart_delay)
arrival_delay<-createBucketSurvey2(BEFdf$Arrival_Delay_in_Minutes)
View(arrival_delay)
no_flights_pa<-createBucketSurvey2(BEFdf$No_of_Flights_p_a_)
View(no_flights_pa)
other_cards<-createBucketSurvey2(BEFdf$No__of_other_Loyalty_Cards)
View(other_cards)
age_BEF<-createBucketSurvey2(BEFdf$Age)
View(age_BEF)
Year1Flight<-createBucketSurvey2(BEFdf$Year_of_First_Flight)
View(Year1Flight)
no_flights_w_other<-createBucketSurvey2(BEFdf$X__of_Flight_with_other_Airlines)
View(no_flights_w_other)
shop_amount<-createBucketSurvey2(BEFdf$Shopping_Amount_at_Airport)
View(shop_amount)
eat_drink<-createBucketSurvey2(BEFdf$Eating_and_Drinking_at_Airport)
View(eat_drink)
hour<-createBucketSurvey2(BEFdf$Scheduled_Departure_Hour)
View(hour)
minutes<-createBucketSurvey2(BEFdf$Flight_time_in_minutes)
View(minutes)
distance<-createBucketSurvey2(BEFdf$Flight_Distance)
View(distance)
#3
status<-createBucketSurvey3(BEFdf$Airline_Status_as_Number)
View(status)
#4
price<-createBucketSurvey4(BEFdf$Price_Sensitivity)
View(price)

#Coerce the hotelSurvey data frame into a sparse transactions matrix
ruleDF<-data.frame(happyCust,depart_delay,arrival_delay,no_flights_pa,other_cards,age_BEF,Year1Flight,
                   no_flights_w_other,shop_amount,eat_drink,hour,minutes,distance,status,price)
View(ruleDF)
str(ruleDF)
SurveyX<-as(ruleDF,"transactions")#coerce
View(SurveyX)
#contents of SurveyX
inspect(SurveyX)#inspect
itemFrequency(SurveyX)#itemfrequency
#itemFrequencyPlot(SurveyX)#itemfrequency plot
#apriori
ruleset<-apriori(SurveyX,
                 parameter=list(support=0.03,confidence=0.06),
                 appearance=list(default="lhs",rhs=("happyCust=High")))
summary(ruleset)
# > summary(ruleset)
# set of 48 rules
# 
# rule length distribution (lhs + rhs):sizes
# 1  2  3  4 
# 1 23 23  1 
# 
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.0     2.0     2.5     2.5     3.0     4.0 
# 
# summary of quality measures:
#   support          confidence           lift            count     
# Min.   :0.03012   Min.   :0.07336   Min.   :0.7691   Min.   : 945  
# 1st Qu.:0.03420   1st Qu.:0.09591   1st Qu.:1.0055   1st Qu.:1073  
# Median :0.03767   Median :0.10239   Median :1.0734   Median :1182  
# Mean   :0.04061   Mean   :0.11613   Mean   :1.2175   Mean   :1274  
# 3rd Qu.:0.04268   3rd Qu.:0.11476   3rd Qu.:1.2031   3rd Qu.:1339  
# Max.   :0.09539   Max.   :0.20190   Max.   :2.1167   Max.   :2993  
# 
# mining info:
#   data ntransactions support confidence
# SurveyX         31378    0.03       0.06
goodrules <- ruleset[quality(ruleset)$lift > 1.2031]
inspect(goodrules)
# > inspect(goodrules)
# lhs                                       rhs              support    confidence lift     count
# [1]  {status=Medium}                        => {happyCust=High} 0.06485436 0.1867315  1.957655 2035 
# [2]  {Year1Flight=High}                     => {happyCust=High} 0.04327873 0.1199859  1.257907 1358 
# [3]  {no_flights_pa=Low}                    => {happyCust=High} 0.04812289 0.1163597  1.219891 1510 
# [4]  {no_flights_pa=Low,status=Medium}      => {happyCust=High} 0.03244311 0.2019040  2.116720 1018 
# [5]  {other_cards=Low,status=Medium}        => {happyCust=High} 0.03027599 0.1892807  1.984381  950 
# [6]  {no_flights_w_other=Low,status=Medium} => {happyCust=High} 0.03014851 0.2001693  2.098534  946 
# [7]  {shop_amount=Low,status=Medium}        => {happyCust=High} 0.03091338 0.1875483  1.966218  970 
# [8]  {arrival_delay=Low,status=Medium}      => {happyCust=High} 0.03623558 0.1849683  1.939170 1137 
# [9]  {depart_delay=Low,status=Medium}       => {happyCust=High} 0.03547071 0.1810934  1.898546 1113 
# [10] {status=Medium,price=Low}              => {happyCust=High} 0.05025814 0.1885687  1.976916 1577 
# [11] {Year1Flight=High,price=Low}           => {happyCust=High} 0.03432341 0.1313575  1.377125 1077 
# [12] {no_flights_pa=Low,price=Low}          => {happyCust=High} 0.03986870 0.1249875  1.310344 1251 

#####We find the best rules for analysis, which are based on the value of Lift(>1.2031). 12 rules are approporate for getting insights.


#airlines
#satisfaction/arilines
#ggplot(BEFdf,aes(y=Age,x=Airline_Name))+geom_point()
