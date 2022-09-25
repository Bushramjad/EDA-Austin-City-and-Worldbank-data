#Bushra Amjad
#i21-2083

#importing dataset
WorldBankData <- read.csv("~/Downloads/WorldBankData.csv")

#Data preprocessing - removed null values using is.na w.r.t internet.usage column for better results
WorldBankData_u <- subset(WorldBankData, !is.na(WorldBankData$internet.users))

#checking total income groups in dataset
table(WorldBankData$IncomeGroup)

#Best fit Model for denmark 

#creating a subset of Denmark
Denmark = WorldBankData[WorldBankData$Country =='Denmark'& WorldBankData$year>1990,]

#A plot that will show the relationship between the year and internet usage in Denmark
plot(Denmark$year, Denmark$internet.users, xlab="Year", ylab = "Internet users", main = "Denmark",col="red")

#1. Linear Model
#A linear regression is a statistical model that analyzes the relationship between a response variable 
#(often called y) and one or more variables and their interactions (often called x or explanatory variables).
lmLinear <- lm(Denmark$internet.users ~ Denmark$year, data = d)

#ploting fit line on graph
abline(lmLinear)

#Displaying Model Summary
summary(lmLinear)

#2. Logistic Model
#Logistic model measures the relationship between the categorical dependent variable and one or more 
#independent variables by estimating probabilities using a logistic function
Logistic = nls(Denmark$internet.users ~ SSlogis(d$year, a, b, c), data = Denmark)

#ploting fit line on graph
lines(Denmark$year, predict(Logistic), col="steelblue")

#Displaying Model Summary
summary(Logistic)

#3. Exponential Model
#Exponential regression is a type of regression that can be used to model the Exponential growth 
#and Exponential decay
lmExp <- lm(log(Denmark$internet.users) ~ Denmark$year, data = Denmark)

#Displaying Model Summary
summary(lmExp)

#ploting fit line on graph
p <- exp(predict (lmExp,list(time=Denmark$internet.users)))
lines (Denmark$year,p, col="green")

#After careful analysis of the model summaries - R2, error values and by visualizing the data i am 
#concluding that Logistic model is the best-fitting model for Denmark.

#Best fit Model for Belarus

Belarus_u = WorldBankData_u[WorldBankData_u$Country =='Belarus' & WorldBankData_u$year>1990,]
Belarus = WorldBankData[WorldBankData$Country =='Belarus' & WorldBankData$year>1990,]

#A plot that will show the relationship between the year and internet usage in Belarus
plot(Belarus_u$year, Belarus_u$internet.users, xlab="Year", ylab = "Internet users", main = "Belarus",col="red")

#1. Linear Model
#A linear regression is a statistical model that analyzes the relationship between a response variable 
#(often called y) and one or more variables and their interactions (often called x or explanatory variables).
lmLinear <- lm(Belarus_u$internet.users ~ Belarus_u$year, data = Belarus_u)

#ploting fit line on graph
abline(lmLinear)

#Displaying Model Summary
summary(lmLinear)

#2. Exponential Model
#Exponential regression is a type of regression that can be used to model the Exponential growth 
#and Exponential decay
lmExp <- lm(log(Belarus_u$internet.users) ~ Belarus_u$year, data = Belarus_u)

#Displaying Model Summary
summary(lmExp)

#ploting fit line on graph
p <- exp(predict (lmExp,list(time=Belarus_u$internet.users)))
lines (Belarus_u$year,p, col="green")

#3. Logistic Model
#Logistic model measures the relationship between the categorical dependent variable and one or more 
#independent variables by estimating probabilities using a logistic function
Logistic = nls(Belarus_u$internet.users ~ SSlogis(Belarus_u$year, a, b, c), data = Belarus_u)

#plot(d$year, d$internet.users)
lines(Belarus_u$year, predict(Logistic), col="steelblue")

#After carefull analysis of the model summaries - R2, error values and by visualising the data i am 
#concluding that Exponential model is the best-fitting model for Belarus.

#Q2. Does income level have an impact on the speed with which a country adopts use of the internet?
#Yes, according to the the above analysis, it is seen that income level have an impact on the speed with 
#which a country adopts use of the internet

high = WorldBankData[WorldBankData$IncomeGroup =='High income',]
low = WorldBankData[WorldBankData$IncomeGroup =='Low income',]

plot(high$mobile.users, high$internet.users, xlab="Mobile Users", ylab = "Internet users", main = "High Income",col="red")
plot(low$mobile.users, low$internet.users, xlab="Mobile Users", ylab = "Internet users", main = "Low Income",col="blue")

#As also seen from the above visualization, High income countries have more increasing internet users as compared
#to low income countries
#********************************







