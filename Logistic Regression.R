library(ggplot2)
library(dplyr)
setwd("C:/Shawn Desai/TSoM - September 2021/Data Handling and Decision-Making/Part 3 - R-Course/Day 28 - Logistic Regression")

churt<- read.csv("telecom_customer_churn.csv")

View(churt)
# Data contains both continuous and categorical variables.
# tenure, monthlycharges, and totalcharges are continuous variables.
# The rest are categorical variables.
head(churt)

dim(churt)

str(churt)

#Check missing values
apply(churt,2, function(x) return(sum(is.na(x))))

#Fixed missing total charges
churt$TotalCharges <- churt$tenure * churt$MonthlyCharges

# Check for categorical variables outliers (Tables)
table(churt$gender)
table(churt$SeniorCitizen)

# Check for continuous vars outliers (Boxplots)
boxplot(churt$tenure)
boxplot(churt$MonthlyCharges)
boxplot(churt$TotalCharges)
boxplot(churt$Churn)

summary(churt)


# Univariate analysis

hist(churt$tenure)
hist(log(churt$tenure))
churt$tenurelog<- log(churt$tenure)
hist(churt$MonthlyCharges)
hist(log(churt$MonthlyCharges))

churt$monthlog<- log(churt$MonthlyCharges)

hist(churt$TotalCharges)
hist(log(churt$TotalCharges))

churt$totallog<- log(churt$TotalCharges)

colnames(churt)

# Bi variate categ
# for categorical vars we will look at the % of churners out of all customers in every value of the categorical var
prop.table(table(churt$PhoneService,churt$Churn), 1)

#6 automated univariate avalysis
#Univariate analysis - categorical variable
cont_columns <- c("tenure","MonthlyCharges","TotalCharges" )
categ_columns <- c("gender","SeniorCitizen", "Partner", "Dependents", "PhoneService" ,
                   "MultipleLines" , "InternetService" ,"OnlineSecurity","OnlineBackup",
                   "DeviceProtection", "TechSupport","StreamingTV" ,"StreamingMovies" ,
                   "Contract","PaperlessBilling","PaymentMethod","Churn" )
uniAnalysisCateg<-function(var)
{
  Freq_tbl<-data.frame(table(churt[,var]))
  Freq_tbl$var_name <- var
  colnames(Freq_tbl)<-c("values","freq","variable")
  return(Freq_tbl)
}
uniDataCateg <- data.frame(values = character(),freq = numeric(),variable = character())
for(i in 1 : length(categ_columns)){
  uniDataCateg<-rbind(uniDataCateg,uniAnalysisCateg(categ_columns[i]))
}

tot_custs<-nrow(churt)
uniDataCateg$perc<-round(100*uniDataCateg$freq/tot_custs,0)
ggplot(uniDataCateg, aes(x = values, y = perc)) + geom_bar(stat = "identity")+ facet_wrap(~ variable, scales = "free")
# write.csv(uniDataCateg, "uniDataCateg.csv", row.names=FALSE)

#continous vars
uniAnalysisCont<-function(var)
{
  Pctl_tbl<-as.vector(quantile(churt[,var], probs=c(.01, .10, .20, .50, .80, .90, .99, 1.0)))
  Pctl_tbl<-data.frame(c("P001","P010","P020","P050","P080","P090","P099","P100"),Pctl_tbl)
  Pctl_tbl<-data.frame(c(var,var,var,var),Pctl_tbl)
  colnames(Pctl_tbl)<-c("variable","quantiles","values")
  return(Pctl_tbl)
}
uniDataCont <- data.frame(variable = character(), quantiles = character(), values = numeric())
for(i in 1 : length(cont_columns)){
  uniDataCont<-rbind(uniDataCont,uniAnalysisCont(cont_columns[i]))
}
ggplot(uniDataCont, aes(x = quantiles, y = values)) + geom_bar(stat = "identity")+ facet_wrap(~ variable, scales = "free")
# write.csv(uniDataCont, "uniDataCont.csv", row.names=FALSE)


#STEP7 automated bi variate analysis
#categorical vars
#install.packages("gmodels")
library(gmodels) # to access CrossTable

bivarfn_categ<-function(var1,var2)
{
  BiVarDataCateg <- data.frame(CrossTable(var1,churt$Churn) )  # here change churn as y axis
  BiVarDataCateg <- BiVarDataCateg[BiVarDataCateg$prop.row.y==1,c("t.x","prop.row.Freq")]
  BiVarDataCateg$Variable <- var2
  colnames(BiVarDataCateg)<-c("value","response_frequency","variable")
  return(BiVarDataCateg)
}

biData <- data.frame(value = character(), response_frequency = numeric(), variable = character())
for(i in 1 : length(categ_columns)){
  biData<-rbind(biData,bivarfn_categ(churt[,categ_columns[i]],categ_columns[i]))
}

ggplot(biData, aes(x = value, y = response_frequency)) + geom_bar(stat = "identity")+ facet_wrap(~ variable, scales = "free")
write.csv(biData, "biAnalysisCateg.csv", row.names=FALSE)
head(churt)

# Variable creation, dummy and others
churt$D_Contract_monthly <- ifelse(churt$Contract=="Month-to-month" ,1,0)
churt$D_Contract_biannual <- ifelse(churt$Contract=="Two year" ,1,0)
churt$D_Dependents_No <- ifelse(churt$Dependents=="No" ,1,0)

churt$D_DeviceProtection_No <- ifelse(churt$DeviceProtection=="No" ,1,0)
churt$D_InternetService_FO <- ifelse(churt$InternetService=="Fiber optic" ,1,0)

churt$D_InternetService_No <- ifelse(churt$InternetService=="No" ,1,0)
churt$D_OnlineBackup_No <- ifelse(churt$OnlineBackup=="No" ,1,0)
churt$D_OnlineSecurity <- ifelse(churt$OnlineSecurity=="No" ,1,0)
churt$D_PaperlessBilling <- ifelse(churt$PaperlessBilling=="Yes" ,1,0)
churt$D_Partner <- ifelse(churt$Partner=="No" ,1,0)
churt$D_PaymentMethod <- ifelse(churt$PaymentMethod=="Electronic check" ,1,0)
churt$D_StreamingMovies_No <- ifelse(churt$StreamingMovies=="No" ,1,0)
churt$D_StreamingTV_No <- ifelse(churt$StreamingTV=="No" ,1,0)
churt$D_TechSupport_No <- ifelse(churt$TechSupport=="No" ,1,0)
churt$D_Gender_M <- ifelse(churt$gender=="Male" ,1,0)

# Step 6 Bi variate on continous variables
# For bivariate continous IV you will check the avg of the IV
# across the values of your DV
# so for e.g. for tenure. 
# you will look at the avg of tenure across churnflag = 0 and churnflag = 1
# and if the avg is diff then it is a good variable

BiVarDataCont = group_by(churt, Churn) # change the name churn to your DV name
biDataCont <- data.frame(Churn= numeric(), avg_value = numeric(), variable = character())

# in these lines all you need to do is change the name of your IV 
biDataCont<-rbind(biDataCont,data.frame(cbind(summarise(BiVarDataCont, avg_value = mean(tenure)),variable=c("tenure","tenure"))))
biDataCont<-rbind(biDataCont,data.frame(cbind(summarise(BiVarDataCont, avg_value = mean(MonthlyCharges)),variable=c("MonthlyCharges","MonthlyCharges"))))
biDataCont<-rbind(biDataCont,data.frame(cbind(summarise(BiVarDataCont, avg_value = mean(TotalCharges)),variable=c("TotalCharges","TotalCharges"))))

ggplot(biDataCont, aes(x = Churn, y = avg_value)) + geom_bar(stat = "identity")+ facet_wrap(~ variable, scales = "free")
write.csv(biDataCont, "biAnalysisCont.csv", row.names=FALSE)

colnames(churt)
data2 <- churt[,-c(1,3,4,6:17)]
head(data2)

# STEP 10. MULTI - COLIINEARITY VIF - TEST 
#install.packages('car') # vif test
library(car)
colnames(data2)
sort(vif(lm(Churn ~ . , data = data2)))
sort(vif(lm(Churn ~ . - MonthlyCharges , data = data2)))
sort(vif(lm(Churn ~ . - MonthlyCharges - TotalCharges, data = data2)))
# now every variable has a VIF value less than 3 and so we will go ahead and use these variable
# so make a note that we will now no more use MonthlyCharges & TotalCharges in our model

# STEP 11. Training and Testing data
set.seed(113)
test <-   sample(1:nrow(data2),nrow(data2)*0.2)
train <-  -test
data.train  = data2[train,]
data.test = data2[test,]
nrow(data.train)
nrow(data.test)

# STEP 12. Running the logistic regression model on TRAINING DATA
# ITERATION 1
model <- glm(Churn ~ . -MonthlyCharges-TotalCharges, data = data.train, family = binomial()) # we are running binomial logistic regression. We use binomial when the DV has 2 levels, like in our case Churn has 2 levels 0 or 1 . Or like Default - Yes or No. Lets say you want to predict who is going to spend - High/Medium/Low. Then your DV has 3 levels and you will use MULTINOMIAL LOGISTIC REGRESSION. Which is exactly the same except the binomial parmeter will change to multinomial
summary(model)

# INTERPRETATION 1 - AIC - AKAIKE INFORMATION CRITERION
# AIC IS A NON STANDARDIZED METRIC, WHICH MEANS IT CAN TAKE ANY VALUE FOR ANY MODEL.
# WITH EVERY MODEL ITERATION YO UNEED TO REDUCE THE AIC
# LOWER AIC THAN YOU HAVE A BETTER MODEL

# INTERPRETATION 2 - WE NEED TO REMOVE ALL THE INSIGNIFICANT VARS. 
# REMOVE THE ONE WITH HIGHEST P VALUE.
# IN OUR CASE WE CAN SEE THAT D_PARTNER HAS P VALUE = 0.781518 
# MEANING 8 OUT OF 10 TIMES THIS VARIABLE D_PARTNER WILL NOT EXPLAIN THE CHURN 
# SO WE WILL REMOVE THIS AND RE-RUN THE MODEL
model <- glm(Churn ~ . -MonthlyCharges-TotalCharges-D_DeviceProtection_No, data = data.train, family = binomial()) # we are running binomial logistic regression. We use binomial when the DV has 2 levels, like in our case Churn has 2 levels 0 or 1 . Or like Default - Yes or No. Lets say you want to predict who is going to spend - High/Medium/Low. Then your DV has 3 levels and you will use MULTINOMIAL LOGISTIC REGRESSION. Which is exactly the same except the binomial parmeter will change to multinomial
summary(model)

model <- glm(Churn ~ . -MonthlyCharges-TotalCharges-D_DeviceProtection_No-D_Partner-D_Gender_M, data = data.train, family = binomial()) # we are running binomial logistic regression. We use binomial when the DV has 2 levels, like in our case Churn has 2 levels 0 or 1 . Or like Default - Yes or No. Lets say you want to predict who is going to spend - High/Medium/Low. Then your DV has 3 levels and you will use MULTINOMIAL LOGISTIC REGRESSION. Which is exactly the same except the binomial parmeter will change to multinomial
summary(model)

model <- glm(Churn ~ . -MonthlyCharges-TotalCharges-D_DeviceProtection_No-D_Partner-D_Gender_M-D_OnlineBackup_No-SeniorCitizen, data = data.train, family = binomial()) # we are running binomial logistic regression. We use binomial when the DV has 2 levels, like in our case Churn has 2 levels 0 or 1 . Or like Default - Yes or No. Lets say you want to predict who is going to spend - High/Medium/Low. Then your DV has 3 levels and you will use MULTINOMIAL LOGISTIC REGRESSION. Which is exactly the same except the binomial parmeter will change to multinomial
summary(model)

# LETS USE OUR MODEL TO GET THE PREDICTED PROB TO CHURN ON THE TRAINING DATA
data.test$Score <- predict(model,data.test,type="response")
# scoring means running the logistic model to get the prob to churn
View(data.test)
