if(!is.null(dev.list())) dev.off()  # clear out the past 
rm(list = ls())
cat("\014")
library(dplyr)
library(tidyr) # for spliting on the period see below
install.packages("moments")
library(moments) # for calculating moments for skewness etc.
install.packages("reshape2")
library(reshape2)
par(mfrow=c(1, 1)) 
library(ggplot2)
install.packages("tidyverse")

setwd("D:/")
data_set <-  read.csv("proportional_species_richness_V3.csv") 
data_set$period <- as.factor(data_set$period) # must set categorical vars
data_set$dominantLandClass <- as.factor(data_set$dominantLandClass)
names(data_set)

data_set_test<-data_set

# DATA EXPLORATION
# Univariate analysis


# Visualizing BEES' richness distribution and median richness value between two periods
#df_7<-data.frame(Bees=df$Bees,bryophytes=df$Bryophytes,butterflies=df$Butterflies,carabids=df$Carabids,macromoths=df$Macromoths,grasshop=df$Grasshoppers_._Crickets,Vas_Plant=df$Vascular_plants)
#boxplot(df_all,main="All taxonomic groups",ylab="Proportional species richness")

library(tidyverse)
ggplot(data_set, aes(x = Bees)) +
  stat_function(
    fun = dnorm,
    args = with(data_set, c(mean = mean(Bees), sd = sd(Bees)))
  ) +
  scale_x_continuous("Bees")

specie_count<-data_set%>%group_by(period,Bees)
specie_count_Y00 <- specie_count %>%filter(period=="Y00")
specie_count_Y00<-median(specie_count_Y00$Bees)
specie_count_Y00

specie_count_Y70 <- specie_count %>%filter(period=="Y70")
specie_count_Y70<-median(specie_count_Y70$Bees)
specie_count_Y70

# Create a grouped bar plot for BEES richness between two periods
x <- data.frame(Period = c("Y00", "Y70"),values = c(specie_count_Y00,specie_count_Y70))
ggplot(x, aes(x = Period, y = values, fill = "Values")) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Period", y = "Specie richness median Values", fill = NULL)

  
# Scatter plot to see pairwise relationship between Bees and Birds
y<-ggplot(data_set, aes(x = Bees, y = Bird)) + 
  geom_point()+
  geom_abline(intercept = 0.53, slope = 0.30, color = "red")

y
# Scatter plot to see pairwise relationship between Bird and ladybirds
z<-ggplot(data_set, aes(x = Bird, y = Ladybirds)) + 
  geom_point()+
  geom_abline(intercept = -0.20, slope = 0.95, color = "red")
z


# As we can see ladybirds vs birds line has more uniform distribution of data points as compare to birds vs bees

# Box plot
my_group<-data.frame(BEES=data_set$Bees,BIRDS=data_set$Bird,BTRFLIS=data_set$Butterflies,CARABIDS=data_set$Carabids,ISOPODS=data_set$Isopods,LDYBIRDS=data_set$Ladybirds,GRASHPRS=data_set$Grasshoppers_._Crickets)
boxplot(my_group, main = "MY SEVEN TAXONOMIC GROUP", ylab = "Proportional species richness")

# To validate above analysis I would like to perform correlation analysis among seven variables 
my_group <- c("Bees", "Bird",  "Butterflies", "Carabids", "Isopods","Ladybirds","Grasshoppers_._Crickets")
cor_data <- data_set[, my_group]
#correlation matrix
CM <- cor(cor_data)
print(CM) #show the correlation matrix


# Visualizing my correlation matrix using heatmap

library(reshape2)
# Reshaping into data frame
CM_matrix <- melt(CM)
# Create a heatmap using ggplot2
ggplot(CM_matrix, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +
  scale_fill_gradient2(low="blue", mid="white", high="purple", 
                       midpoint=0, limit=c(-1,1), space="Lab", 
                       name="Correlation Analysis") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 10, hjust = 1),
        axis.text.y = element_text(size = 10),
        axis.line = element_line(colour = "black"),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.background = element_blank(),
        legend.justification = c(1, 0.5),
        legend.position = c(0.9, 0.5),
        legend.title = element_text(size=10),
        legend.text = element_text(size=10),
        plot.title = element_text(size=14, hjust = 0.5, face = "bold"))




#  count incidents (both periods) for each land classification for later selection
df<-data_set%>%group_by(dominantLandClass)%>%count()%>%
  arrange(dominantLandClass)%>%print(n=45)


# you can select in some way, for example....
# Proj_data<- Proj_data%>%filter(grepl("w",dominantLandClass))
dq<-data_set<- data_set%>%filter(grepl("TM",Location))
#Proj_data<- Proj_data%>%filter(grepl("TM",Location)|grepl("TG",Location))
#Proj_data <- Proj_data%>%filter(dominantLandClass=="3e")
dq

all <- c(2:12)
tax_7 <- sample(all,size=7, replace = FALSE)
tax_7 <- c(2,3,5,6,8,9,11)   # a particular troublesome case
Not_tax_7 <- all[!(all%in%tax_7)]
tax_7_cols <- names(data_set[,2:12])
tax_7_cols <- names(data_set)[tax_7]
tax_7_cols



# calculate the bio div measure over 7 taxonomic groups
mean_7 <- rowMeans(data_set[,tax_7],na.rm=TRUE) # mean the 7 columns 
sum(is.na(mean_7)) # check that there are no NAs in mean_selected
# add in the biodiversity measure which is the mean over 7 taxonomic groups
data_setMA334 <- data_set%>%mutate(mean_eco_7=mean_7)
names(data_setMA334)




# you could split the data by period and compare these stats before and after 
table <- data.frame()
for(j in tax_7){
  table <- rbind(table,
                 c(tax_7_cols[j-1],
                   round(mean(data_setMA334[,j],na.rm = TRUE),digits = 2),
                   round(sd(data_setMA334[,j],na.rm = TRUE),digits = 2),
                   round(skewness(data_setMA334[,j],na.rm = TRUE),digits = 2),
                   round(var(data_setMA334[,j],na.rm = TRUE),digits=2)
                 ))}
colnames(table) <- c("taxi_group","mean","sd","skewness","variance")
table%>%arrange(sd,skewness) # something more could be done here 



# extend data exploration; with correlations between continuous variables
names(data_setMA334)
cont_vars <- data_setMA334%>%select(c(tax_7,13,14)) # includes easting and northing 
names(cont_vars)
cormat <- round(x = cor(cont_vars,use="pairwise.complete.obs"), digits = 2)
# melt the correlation matrix
melt(cormat)%>%mutate(R2 = value^2)%>%arrange(value)
melt(cormat)%>%mutate(R2 = value^2)%>%arrange(Var1,value)

plot(cont_vars$Northing~cont_vars$Easting) # a map appears !!!
# now use the eastings and northings (these may be better used as predictors )

plot(data_setMA334$mean_eco_7~data_setMA334$Easting)
cor(data_setMA334$mean_eco_7,data_setMA334$Easting)
plot(data_setMA334$mean_eco_7~data_setMA334$Northing)  # for BD7
cor(data_setMA334$mean_eco_7,data_setMA334$Northing)




# doing a linear regression with only Northing as a predictor 
lin_mod <- lm(data_setMA334$mean_eco_7~data_set$Northing)
summary(lin_mod)
abline(lin_mod,col="green")
plot(jitter(fitted(lin_mod)),residuals(lin_mod),xlab="Fitted",ylab="Residuals")
abline(h=0)
qqnorm(lin_mod$residuals)
qqline(lin_mod$residuals,col="red")


lin_mod <- lm(data_setMA334$mean_eco_7~data_set$Easting)
summary(lin_mod)
abline(lin_mod,col="green")
plot(jitter(fitted(lin_mod)),residuals(lin_mod),xlab="Fitted",ylab="Residuals")
abline(h=0)
qqnorm(lin_mod$residuals)
qqline(lin_mod$residuals,col="red")





# following code splits between the two periods to find the BD7 change
# however it may be better to use period as a predictor 


# box plot comparisons for the two periods ignoring all other varaibles 
eco_status <- data_setMA334%>%pull(mean_eco_7)
eco_period <- data_setMA334%>%pull(period)
plot(eco_status~eco_period)
names(data_setMA334)




#Hypothesis tests to test the differences in mean is significant or not

data_set_MA334_period <- data_setMA334%>%select(Location,period,mean_eco_7)
data_set_MA334_split <- data_set_MA334_period%>%pivot_wider(names_from =period,values_from=mean_eco_7)
data_set_MA334_split <- data_set_MA334_split%>%mutate(BD7_change=Y00-Y70)
head(data_set_MA334_split)
hist(data_set_MA334_split$BD7_change)  # the distribution of the BD7 change 
BD7_change <- data_set_MA334_split%>%pull(BD7_change)
# One sample t-test
t.test(BD7_change,mu=0)  # t test with H0: mu=0


#to calculate B11 mean column
mean_11 <- rowMeans(data_set[,Not_tax_7 ],na.rm=TRUE) # mean the rem 4 columns 
sum(is.na(mean_11)) # check that there are no NAs in mean_selected
# add in the biodiversity measure which is the mean over 11 taxonomic groups
data_setMA334_11 <- data_setMA334%>%mutate(mean_eco_11=mean_11)
names(data_setMA334_11)



# BD7 for Y00 and Y70
data_setMA334_period_7<-data_setMA334%>%select(Location,dominantLandClass,period,mean_eco_7)
MA_split_7<-data_setMA334_period_7%>%pivot_wider(names_from =period,values_from=mean_eco_7)
MA_split_7<-MA_split_7%>%mutate(BD7_change=Y00-Y70)
head(MA_split_7)

# BD11 for Y00 and Y70
data_setMA334_period_11<-data_setMA334_11%>%select(Location,dominantLandClass,period,mean_eco_11)
MA_split_11<-data_setMA334_period_11%>%pivot_wider(names_from =period,values_from=mean_eco_11)
MA_split_11<-MA_split_11%>%mutate(BD11_change=Y00-Y70)
head(MA_split_11)

#two sample t-test 
first_group<-c(MA_split_7$BD7_change)
second_group<-c(MA_split_11$BD11_change)
t.test(first_group,second_group)
# we can reject the null hypothesis that true mean differences between BD7 and BD11 is equal to zero, as p-value is 0.03928


# Perform chi-square test
ds<- data.frame(ecol_status = c(data_set_test$ecologicalStatus),
                periods = c(data_set_test$period))
table(ds$ecol_status,ds$periods)
# Cross-tabulating the variables
cross_tab <- table(ds$ecol_status, ds$periods)
# Performing chi-square test
chisq.test(cross_tab)

# Shows no significant effect
#we cannot reject the null hypothesis that there is no association between ecological status and periods based on this test.
#Since the p-value is 0.4935


# comparing the two distributions of bio div based on 7 and 11 taxonomic groups 
par(mfrow=c(1, 1))  # divide graph area in 1 columns
qqplot(data_setMA334$mean_eco_7,data_setMA334$ecologicalStatus)
abline(0,1,col="red")
# both cdfs together  and do a kolmogorov test H0: distributions are the same
BD7_cdf <- ecdf(data_setMA334$mean_eco_7)
BD11_cdf <- ecdf(data_setMA334$ecologicalStatus)
plot(BD11_cdf,col="red")
lines(BD7_cdf,col="green")
ks.test(data_setMA334$mean_eco_7,data_setMA334$ecologicalStatus)



#The output of the Kolmogorov-Smirnov test shows a p-value of 0.1091, 
#which is greater than the significance level of 0.05. 
#This indicates that we fail to reject the null hypothesis that the two distributions are the same. 
#Therefore, based on this test, we do not have enough evidence to say that the two variables have significantly different distributions.



# Simple linear regression part of the specified assignment
# regressions of eco_status_7 against ecologicalstatus based on all 11
plot(data_setMA334$mean_eco_7~data_setMA334$ecologicalStatus)
abline(0,1,col="red")   
lin_mod <- lm(data_setMA334$mean_eco_7~data_setMA334$ecologicalStatus)
abline(lin_mod,col="green")
plot(jitter(fitted(lin_mod)),residuals(lin_mod),xlab="Fitted",ylab="Residuals")
abline(h=0,col="blue")
qqnorm(residuals(lin_mod))
qqline(residuals(lin_mod),col="red")
# do the same for each period report and differences 
data_setMA334_Y70 <- data_setMA334%>%filter(period=="Y70")
lin_mod <- lm(data_setMA334_Y70$mean_eco_7~data_setMA334_Y70$ecologicalStatus)
lin_mod$coefficients
# for later period 
data_setMA334_Y00 <- data_setMA334%>%filter(period=="Y00")
lin_mod <- lm(data_setMA334_Y00$mean_eco_7~data_setMA334_Y00$ecologicalStatus)
lin_mod$coefficients




# linear regression of BD4 on BD7 
mean_4 <- rowMeans(data_set[,Not_tax_7 ],na.rm=TRUE) # mean the rem 4 columns 
sum(is.na(mean_4)) # check that there are no NAs in mean_selected
# add in the biodiversity measure which is the mean over 7 taxonomic groups
data_setMA334 <- data_setMA334%>%mutate(mean_eco_4=mean_4)
names(data_setMA334)




# regressions of means: eco_status_4 against others not inc eco_status_4 data
plot(data_setMA334$mean_eco_4~data_setMA334$mean_eco_7)
abline(0,1,col="red")
lin_mod <- lm(data_setMA334$mean_eco_4~df_MA334$mean_eco_7)
summary(lin_mod)
abline(lin_mod,col="green")
plot(jitter(fitted(lin_mod)),residuals(lin_mod),xlab="Fitted",ylab="Residuals")
abline(h=0,col="blue")
qqnorm(residuals(lin_mod))
qqline(residuals(lin_mod),col="red")


# t-test of BD11 on BD7
BD4_BD7<-t.test(data_setMA334_11$mean_eco_11,data_setMA334_11$mean_eco_7)
BD4_BD7


# now multiple linear regression BD4 against the selected 7 

# Create Training and Test data 
trainingRowIndex <- sample(1:nrow(data_setMA334), 0.8*nrow(data_setMA334))  # row indices for 80% training data
trainingData <-data_setMA334[trainingRowIndex, ]  # model training data
testData  <- data_setMA334[-trainingRowIndex, ]%>%na.omit # for test data remove NAs 



# Build the model on training data
lmMod_train <- lm(mean_eco_4~.,
                  data=trainingData[c(tax_7_cols,"mean_eco_4")],
                  na.action=na.omit,y=TRUE)
summary (lmMod_train)  # model summary
cor(lmMod_train$fitted.values,lmMod_train$y) # cor training data 
Eco_status_4_Pred <- predict(lmMod_train, testData) # predict to check model on test Data
cor(Eco_status_4_Pred,testData$mean_eco_4)
plot(Eco_status_4_Pred~testData$mean_eco_4)
abline(0,1,col="red")
# mis_fit_to_testData are the residuals for the train model fit to the test data 
mis_fit_to_testData <- testData$mean_eco_4-Eco_status_4_Pred
plot(mis_fit_to_testData~Eco_status_4_Pred) # look for unwanted pattern in residuals
abline(0,0,col="red")
qqnorm(mis_fit_to_testData) # check for normality of residuals in prediction
qqline(mis_fit_to_testData,col="red")



# multiple linear regression BD7 against period, easting and northing 
mult_lin_mod <- lm(mean_eco_7~.,
                   data=data_setMA334[c("mean_eco_7",
                                          "period","Easting","Northing")],
                   na.action = na.omit,y=TRUE)
summary(mult_lin_mod)
plot(mult_lin_mod$fitted.values~mult_lin_mod$y)
abline(0,1,col="red")
plot(jitter(fitted(mult_lin_mod)),residuals(mult_lin_mod),xlab="Fitted",ylab="Residuals")
abline(h=0,col="blue")
qqnorm(residuals(mult_lin_mod))
qqline(residuals(mult_lin_mod),col="red")


#multiple linear regression BD11 against dominant land class and periods
mult_lin_mod_1<-lm(mean_eco_11~.,data=data_setMA334_11[c("mean_eco_11","period","dominantLandClass")],na.action = na.omit,y=TRUE)
plot(mult_lin_mod_1$fitted.values~mult_lin_mod_1$y)
summary(mult_lin_mod_1)
abline(0,1,col="red")
plot(jitter(fitted(mult_lin_mod_1)),residuals(mult_lin_mod_1),xlab="Fitted",ylab="Residuals")
abline(h=0,col="blue")
qqnorm(residuals(mult_lin_mod_1))
qqline(residuals(mult_lin_mod_1),col="red")
# The summary shows Intercept is the value of mean_eco_11 when both the period and dominantLandClass variables are zero. The periodY70 and dominantLandClass4e coefficients indicate the expected change in mean_eco_11 when the period variable changes from Y00 to Y70 and the dominantLandClass variable changes from 4e to 4f, respectively. The F-statistic and p-value indicate the overall significance of the model. The R-squared and Adjusted R-squared values provide a measure of how well the model fits the data. The residuals section shows summary statistics of the model residuals. 
# so only period was significant therefore i m taking more columns


#multiple linear regression BD11 against dominant land class, periods, Easting and Northings
mult_lin_mod_2<-lm(mean_eco_11~.,data=data_setMA334_11[c("mean_eco_11","period","dominantLandClass","Easting","Northing")],na.action = na.omit,y=TRUE)
plot(mult_lin_mod_2$fitted.values~mult_lin_mod_2$y)
summary(mult_lin_mod_2)
abline(0,1,col="red")
plot(jitter(fitted(mult_lin_mod_2)),residuals(mult_lin_mod_2),xlab="Fitted",ylab="Residuals")
abline(h=0,col="blue")
qqnorm(residuals(mult_lin_mod_2))
qqline(residuals(mult_lin_mod_2),col="red")






# compare the effect of each significant coefficient to that of period
mult_lin_mod_2$coefficients
as.numeric(mult_lin_mod_2$coefficients[3])/as.numeric(mult_lin_mod_2$coefficients[2])
as.numeric(mult_lin_mod_2$coefficients[4])/as.numeric(mult_lin_mod_2$coefficients[2])
as.numeric(mult_lin_mod_2$coefficients[5])/as.numeric(mult_lin_mod_2$coefficients[2])

# shows the dominantland class has strongest effect on the outcome






# The following PCA method is an extension to the set book 
# PCA for visualizing the multi-dimensional spread of biodiversity values #######################

data_set$period <- as.factor(data_set$period) # must set categorical vars
data_set$dominantLandClass <- as.factor(data_set$dominantLandClass)

Proj_data_selected  <- data_set%>%filter(dominantLandClass=="4e") # Sea cliffs/hard coast, Wales


BD_measures <- na.omit(Proj_data_selected) # need to remove NAs (or replace them) in a rotation
head(BD_measures[,2:12])
nrow(BD_measures)  # BD_measures still contains the periods in a single variable

pr.out=prcomp(BD_measures[,2:12]) # Principal Components 
pr.out$center  # gives the mean corrections; the "centers"
pr.out$scale  # not scaled
pr.out$rotation[,1:2] # print out first two principal axes
screeplot(pr.out, type="lines") # plot the variances in decreasing order
plot(pr.out$x[,1],pr.out$x[,2],col=BD_measures$period, cex=0.7,pch=16) # scatter plot for first two principal components
text(pr.out$x[,1],pr.out$x[,2], BD_measures$period, cex=0.4, pos=4, col="red") # location labels

pr.out$rotation[,1:2] # the first two principal directions 

# watch out: the signs of the principal axes are not defined !
# to interpret the sign you could look at a dominating taxi group via boxplots:
plot(BD_measures$Carabids~BD_measures$period) # 4e :bad news for Carabids 
plot(BD_measures$Isopods~BD_measures$period) # 4e :bad news for Isopods 
plot(BD_measures$Bees~BD_measures$period) # good news for bees in 4e 
plot(BD_measures$Ladybirds~BD_measures$period) # bad news for ladybirds in 4e
plot(BD_measures$Bird~BD_measures$period) # good news for birds
plot(BD_measures$Grasshoppers_._Crickets~BD_measures$period) # Good news for GH 
plot(BD_measures$Butterflies~BD_measures$period)#goodnews

regression_model <- lm(Bees ~ Ladybirds, data = BD_measures)
plot(BD_measures$Bees,BD_measures$Ladybirds)
abline(regression_model)


t.test(BD_measures$Bees,BD_measures$Ladybirds)

# WE CAN SEE THAT IN 4E, BEES HAVE A DANGER AND LADYBIRDS ARE SAFE, THEREFORE NO LINEAR RELATIONSHIP CAN BE SEEN FROM SCATTERPLOT AND REGRESSION LINE
# p-value also indicates that there is significant difference between both specie's richness values















