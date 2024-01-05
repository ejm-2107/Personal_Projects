#Packages to be installed
install.packages("caret")
install.packages("corrplot")
install.packages("olsrr")
install.packages("jtools")
install.packages("moments")
install.packages("lmtest")

library(tidyverse)
library(UsingR)
library(readr)
Data<- read_csv("BITS R Prep Course/Group_8_Data.csv")

#plotting single linear regression to check for linearity between all the DVs and IV and 
#also validating the Linear regression assumptions
library(jtools)
library(moments)
library(lmtest)
library(olsrr)
#1. Matches and Wickets
MatchesLM <- lm(Wickets ~ Matches, data=Data)
summary(MatchesLM)
ggplot(Data, aes(x=Matches, y=Wickets)) + geom_point() +
  geom_smooth(method="lm", se=TRUE) + labs(y="Number of Wickets taken", x="Number of Matches played")

#Checking for Homoskedasticity
ols_plot_resid_fit(MatchesLM)

#Checking for normality of resduals
# (The histogramm should show a normal distribution, especially at the tails of the distribution)
ols_plot_resid_hist(MatchesLM)

#There is a strong linear relatinship

#2 Innings and Wickets
InningsLM <- lm(Wickets ~ Innings, data=Data)
summary(InningsLM)
ggplot(Data, aes(x=Innings, y=Wickets)) + geom_point() +
  geom_smooth(method="lm", se=TRUE) + labs(y="Number of Wickets taken", x="Number of Innings bowled")


#Checking for Homoskedasticity
ols_plot_resid_fit(InningsLM)

#Checking for normality of resduals
# (The histogramm should show a normal distribution, especially at the tails of the distribution)
ols_plot_resid_hist(InningsLM)

#There is a strong linear relatinship, but normality of residuals is not that good

#3. Age and Wickets
AgesLM <- lm(Wickets ~ Age, data=Data)
summary(AgesLM)
ggplot(Data, aes(x=Age, y=Wickets)) + geom_point() +
  geom_smooth(method="lm", se=TRUE) + labs(y="Number of Wickets taken", x="Age of players")

#Checking for Homoskedasticity
ols_plot_resid_fit(AgesLM)

#Checking for normality of resduals
# (The histogramm should show a normal distribution, especially at the tails of the distribution)
ols_plot_resid_hist(AgesLM)

#The linear relationship is not good. Residual nomrality and Homoskedasticity is not very good either. 
#Might be removed when final models are created.

#4. Insta_Followers and Wickets
InstaLM <- lm(Wickets ~ Insta_Followers, data=Data)
summary(InstaLM)
ggplot(Data, aes(x=Insta_Followers, y=Wickets)) + geom_point() +
  geom_smooth(method="lm", se=TRUE) + labs(y="Number of Wickets taken", x="Number of Instagram Followers")

#Checking for Homoskedasticity
ols_plot_resid_fit(InstaLM)

#Checking for normality of resduals
# (The histogramm should show a normal distribution, especially at the tails of the distribution)
ols_plot_resid_hist(InstaLM)

#There is a weak linear relationship, but obeys Homoskedasticity and normality of residuals

#5. Twitter Followers and Wickets
TwitterLM <- lm(Wickets ~ Twitter_Followers, data=Data)
summary(TwitterLM)
ggplot(Data, aes(x=Twitter_Followers, y=Wickets)) + geom_point() +
  geom_smooth(method="lm", se=TRUE) + labs(y="Number of Wickets taken", x="Number of Twitter Followers")

#Checking for Homoskedasticity
ols_plot_resid_fit(TwitterLM)

#Checking for normality of resduals
# (The histogramm should show a normal distribution, especially at the tails of the distribution)
ols_plot_resid_hist(TwitterLM)

#The linear relationship is not good. Residual nomrality and Homoskedasticity is not very good either. 
#Might be removed when final models are created.

#6. Auction_Price and Wickets
AuctionLM <- lm(Wickets ~ Auction_Price, data=Data)
summary(AuctionLM)
ggplot(Data, aes(x=Auction_Price, y=Wickets)) + geom_point() +
  geom_smooth(method="lm", se=TRUE) + labs(y="Number of Wickets taken", x="Auction Price of each player")

#Checking for Homoskedasticity
ols_plot_resid_fit(AuctionLM)

#Checking for normality of resduals
# (The histogramm should show a normal distribution, especially at the tails of the distribution)
ols_plot_resid_hist(AuctionLM)

#There is a  linear relatinship, obeys Homoskedasticity and normality of residuals

#7 Dot_Balls and Wickets
Dot_BallsLM <- lm(Wickets ~ Dot_Balls, data=Data)
summary(Dot_BallsLM)
ggplot(Data, aes(x=Dot_Balls, y=Wickets)) + geom_point() +
  geom_smooth(method="lm", se=TRUE) + labs(y="Number of Wickets taken", x="Total number of dot balls")

#Checking for Homoskedasticity
ols_plot_resid_fit(Dot_BallsLM)

#Checking for normality of resduals
# (The histogramm should show a normal distribution, especially at the tails of the distribution)
ols_plot_resid_hist(Dot_BallsLM)

#Strong Linear Relationship, obeys Homoskedasticity and normality of residuals

#8 Economy_Rate and Wickets
Economy_RateLM <- lm(Wickets ~ Economy_Rate, data=Data)
summary(Economy_RateLM)
ggplot(Data, aes(x=Economy_Rate, y=Wickets)) + geom_point() +
  geom_smooth(method="lm", se=TRUE) + labs(y="Number of Wickets taken", x="Total number of dot balls")

#Checking for Homoskedasticity
ols_plot_resid_fit(Economy_RateLM)

#Checking for normality of resduals
# (The histogramm should show a normal distribution, especially at the tails of the distribution)
ols_plot_resid_hist(Economy_RateLM)
#The linear relationship is not good. Residual nomrality and Homoskedasticity is not very good either. 
#Might be removed when final models are created.

#Corelation Matrix
Data_Corelation <- read_csv("BITS R Prep Course/Group_8_Data.csv")
#Encoding Data
library(plyr)
library(readr)
library(dplyr)
library(caret)
glimpse(Data_Corelation)
# Male = 0 and Female = 1
Data_Corelation$Gender <- ifelse(Data_Corelation$Gender == "M",0,1)
# Right Arm = 0 and Left Arm = 1
Data_Corelation$Right_Left_Arm <- ifelse(Data_Corelation$Right_Left_Arm == "Right",0,1)
# Spin = 0 and Pace = 1
Data_Corelation$Spin_Pace <- ifelse(Data_Corelation$Spin_Pace == "Spin",0,1)
glimpse(Data_Corelation)

library(reshape2)
head(Data_Corelation)
data(Data_Corelation, package="reshape2")
library(corrplot)
M = cor(Data_Corelation)
corrplot(M, method = 'number') 


#Multiple Regression

#Finding outliers and removing them if they exsist for Categorical values

#Histograms for categorical variables
#Gender and Wickets
ggplot(Data, aes(x=Wickets, fill=Gender)) +
  geom_histogram(binwidth=10) + labs (x="Number of Wickets taken for each Gender") +
  facet_wrap(~Gender)
#Bowling Style and Wickes
ggplot(Data, aes(x=Wickets, fill=Spin_Pace)) +
  geom_histogram(binwidth=10) + labs (x="Number of Wickets taken for each bowling style") +
  facet_wrap(~Spin_Pace)
#Right/Left Hand Bowling and Wickets
ggplot(Data, aes(x=Wickets, fill=Right_Left_Arm)) +
  geom_histogram(binwidth=10) + labs (x="Number of Wickets taken for prefered bowling hand of bowlers") +
  facet_wrap(~Right_Left_Arm)

#From this, its fair to assume that we have no outliers in the categorical values we have taken

#Finding outliers and removing them if they exsist for Quantitative values

ggplot(Data, aes(x=Matches, y=Wickets)) + geom_point()
ggplot(Data, aes(x=Economy_Rate, y=Wickets)) + geom_point()# Outlier present
ggplot(Data, aes(x=Age, y=Wickets)) + geom_point() 
ggplot(Data, aes(x=Insta_Followers, y=Wickets)) + geom_point() #Hardick Pandya outlier
ggplot(Data, aes(x=Twitter_Followers, y=Wickets)) + geom_point()
ggplot(Data, aes(x=Auction_Price, y=Wickets)) + geom_point()
ggplot(Data, aes(x=Dot_Balls, y=Wickets)) + geom_point() 

# how many need to be removed? and graph after removing
ggplot(Data[Data$Economy_Rate < 18, ], aes(x=Economy_Rate, y=Wickets)) +
  geom_point()

sum(Data$Economy_Rate >= 18)

ggplot(Data[Data$Twitter_Followers < 5100000, ], aes(x=Twitter_Followers, y=Wickets)) +
  geom_point()
sum(Data$Twitter_Followers >= 5100000)

ggplot(Data[Data$Insta_Followers < 9000000, ], aes(x=Insta_Followers, y=Wickets)) +
  geom_point()
sum(Data$Insta_Followers >= 9000000)

Data <- Data[Data$Insta_Followers < 9000000, ]
Data <- Data[Data$Twitter_Followers < 5100000, ]
Data <- Data[Data$Economy_Rate < 18, ]

view(Data)
#MLR

# Fit the linear regression model
Model_1 <- lm(Wickets ~ Matches + Innings + Insta_Followers +
                Twitter_Followers + Auction_Price + Dot_Balls + Gender +  Spin_Pace, data = Data)
summary(Model_1)
coef(Model_1)
library(coefplot)
coefplot(Model_1, sort='mag')
coefplot(Model_1, sort='mag') + scale_x_continuous(limits=c(-.00000001, .000000001))

Model_2 <- lm(Wickets ~ log(Matches) + Innings + Economy_Rate + Age + Insta_Followers*Twitter_Followers 
              + Auction_Price + Dot_Balls + Gender + 
                Right_Left_Arm + Spin_Pace, data = Data)
summary(Model_2)
coef(Model_2)
library(coefplot)
coefplot(Model_2, sort='mag')
coefplot(Model_2, sort='mag') + scale_x_continuous(limits=c(-.0000000000001, .0000000000001))

Model_3 <- lm(Wickets ~ Matches + Economy_Rate + Age  + Insta_Followers*Auction_Price + 
                + Dot_Balls+Gender +  Right_Left_Arm + Spin_Pace + Twitter_Followers, data = Data)
summary(Model_3)
coef(Model_3)
library(coefplot)
coefplot(Model_3, sort='mag')
coefplot(Model_3, sort='mag') + scale_x_continuous(limits=c(-.0000000000001, .0000000000001))

Model_4 <- lm(Wickets ~  Economy_Rate + Innings  + Age + Insta_Followers+Auction_Price*Twitter_Followers + 
                + Dot_Balls +  Right_Left_Arm + Spin_Pace + Gender, data = Data)
summary(Model_4)
coef(Model_4)
library(coefplot)
coefplot(Model_4, sort='mag')
coefplot(Model_4, sort='mag') + scale_x_continuous(limits=c(-.00000000000001, .00000000000001))

Model_5 <- lm(Wickets ~ Matches + Economy_Rate + Age + Insta_Followers*Gender*Auction_Price +
                Twitter_Followers +  Dot_Balls + Right_Left_Arm + Spin_Pace, data = Data)
summary(Model_5)
coef(Model_5)
library(coefplot)
coefplot(Model_5, sort='mag')
coefplot(Model_5, sort='mag') + scale_x_continuous(limits=c(-.00000000000001, .00000000000001))

Model_6 <- lm(Wickets ~  Economy_Rate*Dot_Balls + Innings  + Age + Insta_Followers*Auction_Price +
                Twitter_Followers + Gender + 
                Right_Left_Arm + Spin_Pace, data = Data)
summary(Model_6)
coef(Model_6)
library(coefplot)
coefplot(Model_6, sort='mag')
coefplot(Model_6, sort='mag') + scale_x_continuous(limits=c(-.0000000000001, .0000000000001))

#Excluding Age, Economy_Rate and Twitter Followers
Model_7 <- lm(Wickets ~ Matches + Innings + Gender*Dot_Balls + Insta_Followers:Auction_Price + Auction_Price
               + Spin_Pace+Right_Left_Arm, data = Data)
summary(Model_7)
coef(Model_7)
library(coefplot)
coefplot(Model_7, sort='mag')
coefplot(Model_7, sort='mag') + scale_x_continuous(limits=c(-.0000000000001, .0000000000001))


# Step Function to find best model
nullModel <- lm(Wickets ~ 1, data=Data)
fullModel <- lm(Wickets ~ Spin_Pace*Twitter_Followers*Right_Left_Arm*Auction_Price*
                  Dot_Balls*Gender*Innings*Economy_Rate*Age*Insta_Followers, data = Data)

WicketsStep <- step(nullModel,scope=list(lower=nullModel, upper=fullModel), direction="both")
summary(WicketsStep)

multiplot(Model_1, Model_2, Model_3, Model_4, Model_5,Model_6,Model_7,WicketsStep,pointSize=2)
multiplot(Model_1, Model_2, Model_3, Model_4, Model_5,Model_6,Model_7,WicketsStep, pointSize=2) + scale_x_continuous(limits=c(-.00000000000001, .00000000000001))
anova(Model_1, Model_2, Model_3, Model_4, Model_5,Model_6,Model_7,WicketsStep)

#WicketsStep has one of the least residual sum of squares and all variables are significant, so we proceed with model 7
coefplot(WicketsStep)
coefplot(WicketsStep) + scale_x_continuous(limits=c(-.00000000000001, .00000000000001))
head(fortify(WicketsStep))


#fitted (y hat) values against residuals
W1 <- ggplot(aes(x=.fitted, y=.resid), data = WicketsStep) +  geom_point() +
  geom_hline(yintercept = 0) +    geom_smooth(se = FALSE) +
  labs(x="Fitted Values", y="Residuals")

W1 + geom_point(aes(color=Gender))

W2 <- ggplot(aes(x=.fitted, y=.stdresid), data = WicketsStep) +    geom_point() +
  geom_hline(yintercept = 0) +    geom_smooth(se = FALSE) +
  labs(x="Fitted Values", y="Residuals")

W2 + geom_point(aes(color=Gender))

#Q-Q plot
ggplot(WicketsStep, aes(sample=.stdresid)) + stat_qq() + geom_abline()

# a histogram of the residuals.
ggplot(WicketsStep, aes(x=.resid)) + geom_histogram()
ggplot(WicketsStep, aes(x=.stdresid)) + geom_histogram()
