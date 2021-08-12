
#Title: Electricity price prediction
#Author: Group 6
#Date: 07/03/2020

 ####### LOADING THE DATA AND THE PACKAGES ######
 
#loading the data 
 
df <- read.csv("/Users/antonio/Desktop/IE/TERM_2/MACHINE_LEARNING_II/GROUP ASSIGNMENT/power_market(1).csv", header = T)
deaths <- read.csv("/Users/antonio/Desktop/IE/TERM_2/MACHINE_LEARNING_II/GROUP ASSIGNMENT/weeklydeathsES.csv", header = T)
score <- read.csv("/Users/antonio/Desktop/IE/TERM_2/MACHINE_LEARNING_II/GROUP ASSIGNMENT/scoring.csv", header = T)

#load the packages
library("Hmisc")
library("dplyr")
library("tidyverse")
library("lubridate")
library("ggplot2")
library("ranger")
library("hrbrthemes")
library("gbm")
library("data.table")
library("missForest")
library("Metrics")
library("leaps")
library("xgboost")
library("tidyverse")
library("caret")
library("tseries")

##########################################
######## EXPLORATORY DATA ANALYSIS ######
##########################################

#check if data frame 
is.data.frame(df)

#calculate descriptive statistics
descriptive_statistics <- describe(df)
descriptive_statistics

#create function to find outliers
FindOutliers <- function(data) {
  lowerq = quantile(data, na.rm=TRUE)[2]
  upperq = quantile(data, na.rm = TRUE)[4]
  iqr = upperq - lowerq
  extreme.threshold.upper = (iqr * 2) + upperq
  extreme.threshold.lower = lowerq - (iqr * 2)
  result <- which(data > extreme.threshold.upper | data < extreme.threshold.lower)
  length(result)
}

#with date excluded
sapply(df[,-9], FindOutliers)

#################################################
#no big outliers (with 3*IQR no outliers detected)

#Finding missing values 
na_count <- sapply(df, function(y) sum(length(which(is.na(y)))))
na_count

#we have in import_DR and export_FR each 13 missing values 
#double check:
sum(is.na(df$import_FR))

#since only 13 we can just impute them with miss forest 

#First we try to impute them with the average 
#we create months out of the date given to account for seasonality

df_i <- df %>%
  mutate(date = as.POSIXct(date),
         month = format(date,"%B"))

na_count <- sapply(df_i, function(y) sum(length(which(is.na(y)))))
na_count

mu_imp <- mean(df_i$import_FR, na.rm = TRUE);
mu_exp <- mean(df_i$export_FR, na.rm = TRUE);

df$import_FR[is.na(df_i$import_FR)] <- mu_imp;
df$export_FR[is.na(df_i$export_FR)] <- mu_exp;

na_count <- sapply(df_i, function(y) sum(length(which(is.na(y)))))
na_count


#The second and better way is to impute missing values with miss forest

df_i <- df %>%
  mutate(date = as.POSIXct(date),
         month = format(date,"%B"))

na_count <- sapply(df_i, function(y) sum(length(which(is.na(y)))))
na_count

df_i<-  df_i %>% mutate(fc_demand = as.numeric(fc_demand),
                        fc_nuclear = as.numeric(fc_nuclear),
                        import_FR = as.numeric(import_FR),
                        export_FR = as.numeric(export_FR),
                        fc_wind = as.numeric(fc_wind),
                        fc_solar_pv = as.numeric(fc_solar_pv),
                        fc_solar_th = as.numeric(fc_solar_th),
                        price = as.numeric(price),
                        date = as.Date(date),
                        hour = as.integer(hour), 
                        month = as.factor(month))


df_i <- data.matrix(df_i[,-9])


data_i <- missForest(df_i, verbose = TRUE)
data_i <- as.data.frame(data_i$ximp)

data_i$date <- df$date

na_count <- sapply(data_i, function(y) sum(length(which(is.na(y)))))
na_count

##########################################
####### Let's visualize our data #########
##########################################


####### Demand ###############
df_i$date <- as.POSIXct(df_i$date)
ggplot(df_i, aes(x = date, y = fc_demand))  + 
  geom_line(col="darkmagenta") +
  theme_minimal() +
  labs(title = "Time series", 
       x = "Date", 
       y = "Energy demand") 

#what we can see here is that there is clear seasonality within the different seasons
#as for example we can see in winter and summer the demand is higher than in spring and autumn

####### Price ################

#development over time
ggplot(df_i, aes(x = date, y = price))  + 
  geom_line(col="darkmagenta") +
  theme_minimal() +
  labs(title = "Time series", 
       x = "Date", 
       y = "Price") 

##### Distribution ######

ggplot(df_i, aes(x=price)) +
  geom_histogram(binwidth=3, colour="darkmagenta", fill="darkmagenta") +
  geom_vline(aes(xintercept=mean(price, na.rm=T)),   # Ignore NA values for mean
             color="grey", linetype="dashed", size=1) + 
  theme_minimal()

#we can see it looks nearly normally distributed 

###### Histogram for all features:#####

length(colnames(df_i))
par(mfrow = c(3,3))

for (i in 1:ncol(df_i)){
  if (class(df_i[,i]) == "numeric"){
    a <- colnames(df_i)[i]
    hist(df_i[,i], main = a, col = "darkmagenta")
  }
}

###### Boxplots for all features:######

length(colnames(df_i))
par(mfrow = c(3,3))

for (i in 1:ncol(df_i)){
  if (class(df_i[,i]) == "numeric"){
    a <- colnames(df_i)[i]
    boxplot(df_i[,i], main = a, col = "darkmagenta")
  }
}

##### Correlation matrix ######

cormat <- round(cor(df_i[, sapply(df_i, is.numeric)],
                    use = "complete.obs", method = "pearson"),2)
head(cormat)

# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

upper_tri <- get_upper_tri(cormat)
upper_tri

# Melt the correlation matrix
library(reshape2)
melted_cormat <- melt(upper_tri, na.rm = TRUE)

# Heatmap
library(ggplot2)
ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

####################################################
############# FEATURE ENGINERRING ##################
####################################################

#convert month to factor first:
df_i$month <- as.factor(df_i$month)

###### Create the thermal gap variable #######

df_i$thermal_gap <- df_i$fc_demand - (df_i$fc_nuclear+df_i$fc_wind+df_i$fc_solar_pv+df_i$fc_solar_th)

####### Lets add the week and year to the dataframe ########

#Here is the week

strftime("2020-03-12" , format = "%V") 
class(strftime(df_i$date[180], format = "%Y") )

v <- c() 
for ( d in 1:length(df_i$date)){
  v[d]<-as.numeric(strftime(df_i$date[d], format = "%V"))
}

head(v)
df_i$week <- v

#Here is the year

r <- c() 
for ( d in 1:length(df_i$date)){
  r[d]<-as.numeric(strftime(df_i$date[d], format = "%Y"))
}
head(r)
df_i$year <- r

#Here is the day of the week (1,2,3,4,5,6,7)

d <- c() 
for ( n in 1:length(df_i$date)){
  
  d[n] <- as.numeric(strftime(df_i$date[n], format = "%u"))
}
head(d)
df_i$day_week <- d


#Here is the day of the week but factorized (1 for weekdays and 2 for weekends)

s <- c() 
for ( n in 1:length(df_i$date)){
  
  a <- as.numeric(strftime(df_i$date[n], format = "%u"))
  if (a == 6 | a == 7) {
    
    s[n] <- 2;
  } else {
    s[n] <- 1;
  }
}
head(d)
df_i$type_day_week <- s

df$type_day_week <- as.factor(df$type_day_week)
str(df_i)

###### Assign each week the value of the deaths #####

head(df_i)
head(deaths)

ts.plot(deaths)

deaths_covid <- deaths %>% dplyr::filter(Year >= "2020")
deaths_pre_covid <- deaths %>% dplyr::filter(Year < "2020")

mean_covid <- mean(deaths_covid$Total)
mean_pre_covid <- mean(deaths_pre_covid$Total)

deaths_vector<-c();

for (d in 1:length(df_i$date)){
  
  if(strftime(df_i$date[d], format = "%Y") < "2020" ){
    
    deaths_vector[d] <- mean_pre_covid-4000;
    
  } else {
    for (y in 1:length(deaths_covid$Year)) {
      
      if ( df_i$week[d] == deaths_covid$Week[y] & df_i$year[d] == deaths_covid$Year[y])
      {
        deaths_vector[d] <- deaths_covid$Total[y]
        
      }
    }
  }
}

#We have substracted 4000 to the mean so the effect of the virus is better reflected

df_i$deaths <- deaths_vector
View(df_i)

##### Lags of the variable #####

# Fist we analyze the ACF and the PACF of the price time series, to see what are 
# the most significant lags to predict the actual price. In real life, we could use those
# lags, as every hour we know the values of the hour before, but in this assignment, we 
# had to use the lags of the most correlated variables, the thermal gap.
# To sum up, we used the values of the thermal gap 1 hour before, 2 hours, 24 hours before,

#Now lets check the time series of the prices for 2019 and 2020

df_19_20 <- df_i %>% dplyr::filter(year == 2019 |year == 2020)
df_18_19_20 <- df_i %>% dplyr::filter(year == 2018 | year == 2019 |year == 2020)

#2019 and 2020

y <- df_19_20$price
nlags<- 300
ts.plot(y)
acf(y,nlags)
pacf(y,nlags, title(main = "Series"))

#We use a regular difference to make the data stationary

dif_y <- diff(y)
ts.plot(dif_y)
acf(dif_y,nlags)
a <- pacf(dif_y,nlags)
plot(a, main = "Differenced price")

#We use a seasonal difference to make the data stationary in the seasonal part

s=24
d22dP <- diff(dif_y,lag=24)
ts.plot(d22dP)
lag(acf(d22dP,nlags))
lag(pacf(d22dP,nlags))

# We can see there is a seasonality each 24 and 48 hours.
# Also, the last value has a high correlation with the actual value.
# To look for a weekly seasonality, we change the number of lags to analyze.
# Lags (pacf): 1(0.164), 2(0.152), 24(0.388), 25(0.124), 48(-0.277), 72(-0.218), 96(-0.148 ), 144(-0.216),
# Lags (acf): 24 (-0.394), 168(0.156), 336 (0.158)
# So it would be interesting for our model to include those moments.
# As we do not have the price in the validation dataset, we should use the lags of a variable 
# highly correlated with price, in this case, the thermal gap.

# Now lets use the lags of a the thermal gap
tg1 <- c()
tg2 <- c()
tg24 <- c()
tg25 <- c()
tg48 <- c()
tg72 <- c()
tg96 <- c()
tg144 <- c()
tg168 <- c()
tg336 <- c()

for (d in 1:length(df$date)) {
  if (df_i$year[d] == "2019" | df_i$year[d] == "2020" | df_i$year[d] == "2018"){
    tg1[d-8760] <- df_i$thermal_gap[d-1]
    tg2[d-8760] <- df_i$thermal_gap[d-2]
    tg24[d-8760] <- df_i$thermal_gap[d-24]
    tg25[d-8760] <- df_i$thermal_gap[d-25]
    tg48[d-8760] <- df_i$thermal_gap[d-48]
    tg72[d-8760] <- df_i$thermal_gap[d-72]
    tg96[d-8760] <- df_i$thermal_gap[d-96]
    tg144[d-8760] <- df_i$thermal_gap[d-144]
    tg168[d-8760] <- df_i$thermal_gap[d-168]
    tg336[d-8760] <- df_i$thermal_gap[d-336]
  }
}

df_18_19_20$tg1 <- tg1
df_18_19_20$tg2 <- tg2
df_18_19_20$tg24 <- tg24
df_18_19_20$tg25 <- tg25
df_18_19_20$tg48 <- tg48
df_18_19_20$tg72 <- tg72
df_18_19_20$tg96 <- tg96
df_18_19_20$tg144 <- tg144
df_18_19_20$tg168 <- tg168
df_18_19_20$tg336 <- tg336

#We had to create a new df for 2018, 2019 and 2020 to be able to substract the values

View(df_18_19_20)

#New variable trying to maximize the effect of the import and export

df_18_19_20$ratio_imp_exp <- df_18_19_20$import_FR/df_18_19_20$export_FR

#Last we are going to check for the most correlated variables

##### Correlation analysis #####

cormat <- round(cor(df_18_19_20[, sapply(df_18_19_20, is.numeric)],
                    use = "complete.obs", method = "pearson"),2)
head(cormat)

library(reshape2)
melted_cormat <- melt(cormat)
head(melted_cormat)

library(ggplot2)
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()

# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}

# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

upper_tri <- get_upper_tri(cormat)
upper_tri

# Melt the correlation matrix
library(reshape2)
melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Heatmap
library(ggplot2)
ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()


reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}


# Reorder the correlation matrix
cormat <- reorder_cormat(cormat)
upper_tri <- get_upper_tri(cormat)
# Melt the correlation matrix
melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Create a ggheatmap
ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()
# Print the heatmap
print(ggheatmap)

ggheatmap + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 3) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))

# This are the most correlated variables: tg1, tg2, tg24, tg25, tg48, tg168, thermal_gap
# year, deaths, fc_demand. All of the with a correlation over |0,5|.

##############################
########## MODELS ############
###############################

##### Train test split ######

# We created a first split although in some cases we will be using a rolling window, to avoid the 
# bias of the split.

data_i <- df_18_19_20 %>%
  mutate(date = as.POSIXct(date))

View(data_i)

train_i <- data_i %>%
  filter(date <= "2020-04-30",
         date >= "2019-01-01")

test_i <- data_i %>%
  filter(date > "2020-04-30")

######## LINEAR REGRESSION ########

########## Baseline ###############

str(data_i)

m1 <- lm(price ~. , data = train_i) 
summary(m1)

#the R^2 is only at about 50% and we have many insignificant variables

#do the prediction with the validation set
predict_m1 = predict(m1, newdata = test_i)
y_test = test_i$price

#calculate Mean Squared error 
mean((y_test-predict_m1)^2)

# Calculating RMSE        
rmse(y_test,predict_m1) 

#RMSE of 12.9 and quite low predictive power 
# This metric will be used as the baseline, so the goal of the rest of the models is to 
# out-perform it 

#Now we are going to use the variables with the highest correlation

m2 <- lm(price ~ tg1 + tg2 + tg24 +tg25 + tg48 + tg168 + deaths + 
           thermal_gap + fc_demand + year , data = train_i) 
summary(m2)

#the R^2 is about 70% 

#do the prediction with the test set
predict_m2 = predict(m2, newdata = test_i)
y_test = test_i$price

#calculate Mean Squared error 
mean((y_test-predict_m2)^2)

# Calculating RMSE        
rmse(y_test,predict_m2) 

# RMSE of 5.359375
# As this looks like a promising model, we use a rolling window for validation to obtain a more
# precise RMSE.

#ROLLING WINDOW HOME MADE

split <- c(1,6,12,18,24)
rmse_train_v <- c()
rmse_test_v <- c()
index <- 0

for (d in split) {
  index = index + 1
  
  train <- data_i%>% dplyr::filter((year == "2020" & week < d) |(year == "2019"))
  test <-  data_i %>% dplyr::filter(year == "2020" & week >= d)
  
  m2 <- lm(price ~ tg1 + tg2 + tg24 +tg25 + tg48 + tg168 + deaths + 
             thermal_gap + fc_demand + year , data = train) 
  
  predict_train <- predict(m2,train)
  predict_test <- predict(m2,test)
  y_train = train$price
  y_test = test$price
  rmse_test <- rmse(y_test, predict_test)
  rmse_train <- rmse(y_train, predict_train)
  
  rmse_train_v[index] <- rmse_train;
  rmse_test_v[index] <- rmse_test;
  
  print(sprintf("You are in the %d iteration, have some patience", index))
}

mean(rmse_train_v)
mean(rmse_test_v)

# RMSE: 9.200862
#It improved compared to the baseline, so it is a good direction.

#Although we selected the most correlated variables, we used a step-wise method to chek for 
# new important variables

library(leaps)
m3 <- regsubsets(price ~ . -date -month, data = train_i, method = "forward") 
m_best_summary <- summary(m3)

par(mfrow=c(1,1))
plot(m_best_summary$adjr2, xlab = "Number of variables", ylab = "R2 adjusted", type = "b")
plot(m_best_summary$cp, xlab = "Number of variables", ylab = "Cp", type = "b")

#We can see that with only 4 variables we are capturing most of the possible variance

coef(m3,4)

m4 <- lm(price ~ import_FR + thermal_gap + year + week, data = train_i)
summary(m4)
predict_m4 <- predict(m4, newdata = test_i)

#MSE
mean((y_validation3-predict_m4)^2)

# Calculating RMSE 
rmse(test_i$price,predict_m4) 

#the RMSE now is around 9 and the R^2 has increased to the 82%
#In this case, when introducing the two new variables (import_FR and week) into the model, we lost
# prediction power

##########################
####### BOOSTING #########
##########################


# Let's start a basic boost model 
# Let's compare which data set performs better - the one with imputed values or the 
# one with removed ones 

boost1 <- gbm(price ~ tg1 + tg2 + tg24 +tg25 + tg48 + tg168 + deaths + 
                thermal_gap + fc_demand + year, data = train_i, distribution = "gaussian", n.trees = 5000, interaction.depth = 4)
summary(boost1)

predict_m1 <-  predict(boost1, newdata = test_i)

y_test <-  test_i$price

#MSE
mean((y_test-predict_m1)^2)

#RMSE       
rmse(y_test,predict_m1) 

#RMSE: 6.912314
#MSE: 47.78008

#Lets try modifying some parameters
library(data.table)
trees <- c(1000,2000,3000,4000,5000,6000,7000,8000,9000,10000,11000)

y_validation = test_i$price
y_train = train_i$price

results <- data.table(model = c("INICIO"), 
                      mse_train = 10000, rmse_train = 1000,
                      mse_test = 100000, rmse_test = 1000);
results;

for (index in trees) {
  
  boost = gbm(price ~ tg1 + tg2 + tg24 +tg25 + tg48 + tg168 + deaths + 
                thermal_gap + fc_demand + year, data = train_i, distribution = "gaussian", n.trees = index, interaction.depth = 4)
  predict = predict(boost, newdata = train_i)
  predict_valid = predict(boost, newdata = test_i)
  
  MSE_train = mean((y_train-predict)^2)
  MSE_valid = mean((y_validation-predict_valid)^2)
  RMSE_train = rmse(y_train,predict) 
  RMSE_valid = rmse(y_validation,predict_valid) 
  results <- rbind(results,
                   data.table(model = index, 
                              mse_train = MSE_train, rmse_train = RMSE_train,
                              mse_test = MSE_valid, rmse_test = RMSE_valid));
}

results;

#It looks like the correct number of trees is 1000

boost1 <- gbm(price ~ tg1 + tg2 + tg24 +tg25 + tg48 + tg168 + deaths + 
                thermal_gap + fc_demand + year, data = train_i, distribution = "gaussian", n.trees = 1000, interaction.depth = 4)
summary(boost1)
predict_m1 <-  predict(boost1, newdata = test_i)
y_test <-  test_i$price

#RMSE       
rmse(y_test,predict_m1) 

#RMSE: 6.407 #Its better than the original one

###### XGBoost #######

#First we have to transform the data into a matrix
train_i$month <- as.integer(train_i$month)
train_mat <- train_i %>% 
  select(tg1, tg2, tg24, tg25 ,tg48 , tg168, deaths ,
           thermal_gap , fc_demand,  year) %>% 
  as.matrix() %>% 
  xgb.DMatrix(data = , label = train_i$price)
train_mat

#Same for the validation set
test_i$month <- as.integer(test_i$month)
val_mat <- test_i %>% 
  select(tg1, tg2, tg24, tg25 ,tg48 , tg168, deaths ,
         thermal_gap , fc_demand,  year) %>% 
  as.matrix() %>% 
  xgb.DMatrix(data = ., label = test_i$price)

val_mat

#Train de model

xgboost1 <- xgboost(data = train_mat, 
                    objective = "reg:squarederror",
                    nrounds = 10, max.depth = 2, eta = 0.3, nthread = 2)

xgboost1

predict_01 <- predict(xgboost1,val_mat)

head(predict_01)

y_validation = test_i$price

#MSE
mean((y_validation-predict_01)^2)

#RMSE    
rmse(y_validation,predict_01) 

# Printing the value 
print(result)    

#RMSE: 15.24715
#MSE: 232.4755
#It's not better than normal boosting, so lets try different parameter

xgboost2 <- xgboost(data = train_mat, 
                    objective = "reg:gamma",
                    nrounds = 500, max.depth = 4, eta = 0.1, nthread = 2, 
                    early_stopping_rounds = 10)

predict_02 <- predict(xgboost2,val_mat)
y_validation = test_i$price

#MSE
mean((y_validation-predict_02)^2)

#RMSE
rmse(y_validation,predict_02) 

# Not much better lets try some more avdanced methods

# Lets try a grid search to find the best hyperparameters
rounds <- c(300,400,500) 
depth <- c(2,3,4)
eta <- c(0.5,0.55,0.6,0.65,0.7)
objective <- c("reg:squarederror", "reg:squaredlogerror", "reg:gamma")

y_train = train_i$price
y_validation = test_i$price

results <- data.table(nrows = 1, max.depth = 1, eta =1,objective = 1,
                      mse_train = 1000, rmse_train = 10000,
                      mse_test = 10000, rmse_test = 10000);
results;

index = 0

for (r in rounds) {
  for (d in depth){
    for (e in eta){
      for(o in objective)
        index =+ 1
      xgboost <- xgboost(data = train_mat, 
                         objective = o,
                         nrounds = r, max.depth = d, eta = e, nthread = 2, 
                         early_stopping_rounds = 10)
      
      predict <- predict(xgboost,train_mat)
      predict_valid <- predict(xgboost,val_mat)
      
      MSE_train = mean((y_train-predict)^2)
      MSE_valid = mean((y_validation-predict_valid)^2)
      RMSE_train = rmse(y_train,predict) 
      RMSE_valid = rmse(y_validation,predict_valid) 
      results <- rbind(results,
                       data.table(nrows = r, max.depth = d, eta =e,objective = o,
                                  mse_train = MSE_train, rmse_train = RMSE_train,
                                  mse_test = MSE_valid, rmse_test = RMSE_valid));
      print(sprintf("Modelo con rounds: %f, max.depth: %f y eta: %f", r, d, e))
    }
  }
}

results1 <- results

results1 <- results1 %>%
  filter(mse_train > 0)

which.min(results1$rmse_test)
results1[which.min(results1$rmse_test)]

xgboost3 <- xgboost(data = train_mat, 
                    objective = "reg:gamma",
                    nrounds = 500, max.depth = 4, eta = 0.1, nthread = 2, 
                    early_stopping_rounds = 10)

predict_03 <- predict(xgboost3,val_mat)
y_validation = test_i$price

#MSE
mean((y_validation-predict_03)^2)

#RMSE
rmse(y_validation,predict_03) 

#Trying different parameters we saw there was no better model. As we could not improve more our model
# we proceeded to try Random Forest

###### RANDOM FOREST #####

# For random forest no need to normalize data 
# We use ranger which is a faster implementation of random forest

#DECISION TREE

# First we used a really basic decission tree

library(rpart)
library(rpart.plot)
decision_tree <- rpart(price ~ tg1 + tg2 + tg24 +tg25 + tg48 + tg168 + deaths + 
                         thermal_gap + fc_demand + year, df_18_19_20)

rpart.plot(decision_tree, type = 1, extra = 100, legend.x =NA, box.palette="GyGn", cex=0.5)


predict_train <- predict(decision_tree,train_i)
predict_test <- predict(decision_tree,test_i)
y_train = train_i$price
y_test = test_i$price
rmse_test <- rmse(y_test, predict_test)
rmse_train <- rmse(y_train, predict_train)

rmse_train_v[index] <- rmse_train;
rmse_test_v[index] <- rmse_test;

#After that, a basic model with all the variables

first_model <- ranger(price~. -date, train_i, importance = "permutation", num.trees = 10000)

y_test = test_i$price
y_train = train_i$price

predict_ranger <- predict(first_model,test_i)
predict_train<- predict(first_model,train_i)

RMSE_train = rmse(y_train,predict_train$predictions) 
RMSE_test = rmse(y_test,predict_ranger$predictions) 

RMSE_test
RMSE_train

#RMSE_test
#  6.479116
# RMSE_train
#  1.009721

# Here we performed a grid search to obtain the best hyper-parameters

group_folds <- groupKFold(data_i$price, k = 10)

group_fit_control <- trainControl(## use grouped CV folds
  index = group_folds,
  method = "cv")

rf_grid <- expand.grid(mtry = c(2, 3, 4, 5),
                       splitrule = c("variance", "extratrees"),
                       min.node.size = c(1, 3, 5,7))
rf_grid


rf_fit <- train(price~ tg1 + tg2 + tg24 +tg25 + tg48 + tg168 + deaths + 
                  thermal_gap + fc_demand + year, 
                data = data_i, 
                method = "ranger",
                trControl = group_fit_control,
                # provide a grid of parameters
                tuneGrid = rf_grid)
rf_fit

#RMSE was used to select the optimal model using the smallest value.
#The final values used for the model were mtry = 5, splitrule = variance and min.node.size = 1.

# After obtaining the parameters, we used the features selected

third_model <- ranger(price~  tg1 + tg2 + tg24 +tg25 + tg48 + tg168 + deaths + 
                        thermal_gap + fc_demand + year , train_i, mtry = 5, splitrule = "variance", min.node.size = 1, num.trees = 3000)

y_test = test_i$price
y_train = train_i$price

predict_ranger <- predict(first_model,test_i)
predict_train<- predict(first_model,train_i)

RMSE_train = rmse(y_train,predict_train$predictions) 
RMSE_test = rmse(y_test,predict_ranger$predictions) 

RMSE_test
RMSE_train

#RMSE_test
# 6.479116

# It looked really good, so we used the rolling window to chech if there was any bias in the splitting

#ROLLING WINDOW HOME MADE

split <- c(1,6,12,18,24)
rmse_train_v <- c()
rmse_test_v <- c()
index <- 0

for (d in split) {
  index = index + 1
  
  train <- data_i%>% dplyr::filter((year == "2020" & week < d) |(year == "2019"))
  test <-  data_i %>% dplyr::filter(year == "2020" & week >= d)
  
  ranger_1 <- ranger(price ~ tg1 + tg2 + tg24 +tg25 + tg48 + tg168 + deaths + 
                       thermal_gap + fc_demand + year,train_i, mtry = 5, 
                     splitrule = "variance", min.node.size = 1, num.trees = 500)
  
  predict_train <- predict(ranger_1,train)
  predict_test <- predict(ranger_1,test)
  y_train = train$price
  y_test = test$price
  rmse_test <- rmse(y_test, predict_test$predictions)
  rmse_train <- rmse(y_train, predict_train$predictions)
  
  rmse_train_v[index] <- rmse_train;
  rmse_test_v[index] <- rmse_test;
  
  print(sprintf("You are in the %d iteration, have some patience", index))
}

mean(rmse_train_v)
mean(rmse_test_v)



# RMSE: 4.943915

######## SCORE DATASET #####

score <- read.csv("/Users/antonio/Desktop/IE/TERM_2/MACHINE_LEARNING_II/GROUP ASSIGNMENT/scoring.csv", header = T)
View(score)

sapply(score[,-8], FindOutliers)

length(score$fc_demand)

#Some outliers, but as we trained our model with outlier data this is not something to worry about

score_i <- score %>%
  mutate(date = as.POSIXct(date),
         month = format(date,"%B"))

na_count <- sapply(score_i, function(y) sum(length(which(is.na(y)))))
na_count


# No missing values

##### FEATURE ENGINERRING ##########

#convert month to factor first:
score_i$month <- as.factor(score$month)


#WE KEEP ON GOING WITH THE DATA SET OF THE IMPUTET VALUES

###### Create the thermal gap variable #######

score_i$thermal_gap <- score_i$fc_demand - (score_i$fc_nuclear+score_i$fc_wind+score_i$fc_solar_pv+score_i$fc_solar_th)

####### Lets add the week and year to the df ########

#Here is the week

strftime("2020-03-12" , format = "%V") 
class(strftime(df_i$date[180], format = "%Y") )

v <- c() 
for ( d in 1:length(score_i$date)){
  v[d]<-as.numeric(strftime(score_i$date[d], format = "%V"))
}

head(v)
score_i$week <- v
View(score_i)

#Here is the year

r <- c() 
for ( d in 1:length(score_i$date)){
  r[d]<-as.numeric(strftime(score_i$date[d], format = "%Y"))
}
head(r)
score_i$year <- r

#Here is the date with the hour
r <- c() 
for ( d in 1:length(score_i$date)){
  r[d]<-as.numeric(strftime(score_i$date[d], format = "%Y"))
}
head(r)
score_i$year <- r

#Here is the day of the week (1,2,3,4,5,6,7)

d <- c() 
for ( n in 1:length(score_i$date)){
  
  d[n] <- as.numeric(strftime(score_i$date[n], format = "%u"))
}
head(d)
score_i$day_week <- d


#Here is the day of the week but factorized (1 for weekdays and 2 for weekends)

s <- c() 
for ( n in 1:length(score_i$date)){
  
  a <- as.numeric(strftime(score_i$date[n], format = "%u"))
  if (a == 6 | a == 7) {
    
    s[n] <- 2;
  } else {
    s[n] <- 1;
  }
}
head(d)
score_i$type_day_week <- s

df$type_day_week <- as.factor(df$type_day_week)
str(df_i)

###### Assign each week the value of the deaths #####

deaths_vector<-c();

View(deaths)

ts.plot(deaths)

cor(df_i$deaths,df_i$price)

deaths_covid <- deaths %>% dplyr::filter(Year >= "2020")
deaths_pre_covid <- deaths %>% dplyr::filter(Year < "2020")

for (d in 1:length(score_i$date)){
  
  if(strftime(score_i$date[d], format = "%Y") == "2020" ){
    
    for (y in 1:length(deaths_covid$Year)) {
      
      if ( score_i$week[d] == deaths_covid$Week[y] & score_i$year[d] == deaths_covid$Year[y])
      {
        deaths_vector[d] <- deaths_covid$Total[y]
      }
    }
    
  } else if(strftime(score_i$date[d], format = "%Y") == "2021") {
    for (y in 1:length(deaths_covid$Year)) {
      
      if ( score_i$week[d] == deaths_covid$Week[y] & score_i$year[d] == deaths_covid$Year[y])
      {
        deaths_vector[d] <- deaths_covid$Total[y]
        
      }
    }
  }
}


na_count <- sapply(score_i, function(y) sum(length(which(is.na(y)))))
na_count

which(is.na(score_i$deaths))

#There has been some values lost because of the terminology of the week, lets add them

score_i$deaths[is.na(score_i$deaths)] <- 10631;

#No NAs

score_i$deaths <- deaths_vector

View(df_i)
View(deaths)

#### lags of a the thermal gap ####

tg1 <- c()
tg2 <- c()
tg24 <- c()
tg25 <- c()
tg48 <- c()
tg72 <- c()
tg96 <- c()
tg144 <- c()
tg168 <- c()
tg336 <- c()

score1 <- score_i
df_19_20$price <- NULL

score2 <- rbind(df_19_20,score1)
View(score2)


for (d in 1:length(score2$date)) {
  if (score2$year[d] == "2020" | score2$year[d] == "2021"){
    tg1[d-8760] <- score2$thermal_gap[d-1]
    tg2[d-8760] <- score2$thermal_gap[d-2]
    tg24[d-8760] <- score2$thermal_gap[d-24]
    tg25[d-8760] <- score2$thermal_gap[d-25]
    tg48[d-8760] <- score2$thermal_gap[d-48]
    tg72[d-8760] <- score2$thermal_gap[d-72]
    tg96[d-8760] <- score2$thermal_gap[d-96]
    tg144[d-8760] <- score2$thermal_gap[d-144]
    tg168[d-8760] <- score2$thermal_gap[d-168]
    tg336[d-8760] <- score2$thermal_gap[d-336]
  } 
}

score3 <- score2 %>% dplyr::filter(year == "2020"| year == "2021")
View(score3)

score3$tg1 <- tg1
score3$tg2 <- tg2
score3$tg24 <- tg24
score3$tg25 <- tg25
score3$tg48 <- tg48
score3$tg72 <- tg72
score3$tg96 <- tg96
score3$tg144 <- tg144
score3$tg168 <- tg168
score3$tg336 <- tg336


#Now we have all the variables for the predictions, lets keep only the score dataset

score_final <- score3 %>% filter(date>="2020-09-01")
View(score_final)

##### PREDICTIONS #####
#As we said during the analysis, we will use RF to predict the values

score_model <- ranger(price ~ tg1 + tg2 + tg24 +tg25 + tg48 + tg168 + deaths + 
                     thermal_gap + fc_demand + year,train_i, mtry = 5, 
                   splitrule = "variance", min.node.size = 1, num.trees = 500)



score_pred <- predict(score_model,score_final)

a <- score_pred$predictions

final <- cbind(score_pred$predictions,score)

View(final)

write.table(cbind(a), file = "Predicitons_GroupF.csv", row.names =FALSE, col.names = FALSE,sep = ",")

