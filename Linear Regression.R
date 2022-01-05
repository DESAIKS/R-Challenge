setwd("C:/Shawn Desai/TSoM - September 2021/Data Handling and Decision-Making/Part 3 - R-Course/Data-Files")

house<- read.csv("house_data.csv")

View(house)
head(house)

str(house)
house$date<- as.Date(house$date,'%Y-%m-%d %H:%M:%S')
str(house)

# Check missing values(NA)
apply(house,2, function(x) return(sum(is.na(x))))

#Replace missing values
house$bedrooms[house$bedrooms ==0]<- NA
house$sqft_basement[is.na(house$sqft_basement)] <- 0

# Fix the missing value in the bedroom.
#install.packages("VIM")
library(VIM)
house <- kNN(house,variable=c("bedrooms"), k=6)

# Check missing values(NA)
apply(house,2, function(x) return(sum(is.na(x))))

#Replace missing values
#Impute
#install.packages("mice")
#library(mice)
#impute <- mice(data, m=2,seed=500)
#print(impute)
#house<-complete(impute,2)
#View(house)

#Check for Outliers:

boxplot(house$price,main="Price",xlab = "Price", col= "orange")
summary(house)
# Top outlier
bench <- 654962 + 1.5*IQR(house$price)
house$price[house$price > bench] <- bench

#Bottom outlier
bench1 <- 654962 - 1.5*IQR(house$price)
house$price[house$price < bench1] <- bench1

boxplot(house[,3],main="Bedrooms",col = c("red"))
boxplot(house[,4],main="Bathrooms",col = c("green"))

#Bottom outlier (Bathrooms)
bench3 <- 2.500 - 1.5*IQR(house$bathrooms)
house$bathrooms[house$bathrooms < bench3] <- bench3

boxplot(house[,5],main="Sqft_living",col = c("blue"))
boxplot(house[,6],main="Sqft_lot",col = c("yellow"))

# Top outlier (Square.Ft Lot)
bench2 <- 11001 + 1.5*IQR(house$sqft_lot)
house$sqft_lot[house$sqft_lot > bench2] <- bench2

boxplot(house[,7:11],main="Floors")

#Sqft_basement
boxplot(house[,12],main="Sq.Ft Basement")
bench2 <- 610.0 + 1.5*IQR(house$sqft_basement)
house$sqft_basement[house$sqft_basement > bench2] <- bench2

boxplot(house[,13],main="Yr_built")
boxplot(house[,14],main="Yr_renovated")
# Outliers to be considered is Price, Sq.Foot Lot, Sq.Foot Above, Sq.Foot Basement

#install.packages("ggplot2")
library(ggplot2)

#Price
ggplot(house,aes(price)) + geom_histogram() + labs(title = "House Price", x = "Price")

#Sq.ft Lot
ggplot(house,aes(sqft_lot)) + geom_histogram() + labs(title = "House Square Feet Lot", x = "Sq.ft. Lot")

#house$log_sqft_lot<- log(house$sqft_lot)
#Sq.ft Above
ggplot(house,aes(sqft_above)) + geom_histogram() + labs(title = "House Square Feet Above", x = "Sq.ft. Above")


#Sq.ft Basement
ggplot(house,aes(sqft_basement)) + geom_histogram() + labs(title = "House Square Foot Basement", x = "Sq.ft. Basement")

colnames(house)

#Scatter-Plot:
ggplot(house, aes(bedrooms,price)) + geom_point() + labs(title = "House Price", x = "Bedrooms", y="Price")
ggplot(house, aes(bathrooms,price)) + geom_point() + labs(title = "House Price", x = "Bathrooms", y="Price")

ggplot(house, aes(sqft_living,price)) + geom_point() + labs(title = "House Price", x = "Sq.ft. Living", y="Price")
ggplot(house, aes(sqft_lot,price)) + geom_point() + labs(title = "House Price", x = "Sq.ft. Lot", y="Price")

ggplot(house, aes(floors,price)) + geom_point() + labs(title = "House Price", x = "Floors", y="Price")
ggplot(house, aes(waterfront,price)) + geom_point() + labs(title = "House Pricà", x = "Waterfronts", y="Price")

ggplot(house, aes(sqft_basement,price)) + geom_point() + labs(title = "House Price", x = "Sq.ft.Basement", y="Price")
ggplot(house, aes(yr_built,price)) + geom_point() + labs(title = "House Price", x = "Year Built", y="Price")
ggplot(house, aes(yr_renovated,price)) + geom_point() + labs(title = "House Price", x = "Year Renovated", y="Price")

# Remove Columns with date and character variable
library(dplyr)

hsubdata <- select(house,-c(1,15,16,17,18,19))

str(hsubdata)

#Coorelation Matrix
corM<- cor(hsubdata)
corM

plot(hsubdata)

#Linear Regression
#Create Train and Test Data
set.seed(100)
trainingRowIndex <- sample(1:nrow(hsubdata), 0.8*nrow(hsubdata))
trainingdata<- hsubdata[trainingRowIndex,]
testdata <- hsubdata[-trainingRowIndex,]

#Create Model
#First try
lmMod <- lm(price~., data = trainingdata)
summary(lmMod)
plot(lmMod, which = 1)

#Second try
lmMod <- lm(price ~ bedrooms+bathrooms+sqft_living+sqft_lot+floors+view+condition+sqft_above+sqft_basement+yr_built, data = trainingdata)
summary(lmMod)
plot(lmMod, which = 1)

pricePred <- predict(lmMod, testdata)  # predict price
pred <- as.data.frame(pricePred) # To see predicted model in dataframe
head(pred)

ModelSummary = summary(lmMod)
# Model Coefficient
modelCoeffs = ModelSummary$coefficients
Price =  -32890*bedrooms+ 46699*bathrooms - 217*sqft_living+ -0.31064*sqft_lot+ 59411*floors+ 37431* view + 19689*condition+401*sqft_above+390*sqft_basement+-2050*yr_built

#Prediction vs. Actual Price
actuals_preds <- data.frame(cbind(actuals=testdata$price, predicteds=pricePred))

correlation_accuracy <- cor(actuals_preds)
correlation_accuracy

head(actuals_preds)

   
