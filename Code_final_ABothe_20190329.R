#libraries
library(mlr) #for dummy coding 
library(dplyr) #for %>% count
library(car) #for multicollinearity
library(knitr) #latex document
library(ggplot2)
library(ggcorrplot)
library(caret) #for resamples (model comparison)

#set wd
setwd("/Users/Bisa/Documents/Studium/Masterstudium/1. Semester/Datenanalyse II/Hausarbeit")

#load data
suicide <- read.csv("master.csv", header = T, sep = ",")
happy <- read.csv ("2015.csv", header = T, sep = ",")
terrorism <- read.csv(file="GTD_data_1970-2017.csv", header = T, sep = ",")

#data preparation
'####TERRORISM####'
terrorism_sub <- terrorism[which((terrorism$crit2 == 1 & terrorism$crit1 == 1 & terrorism$crit3 == 1) & 
                                   (terrorism$doubtterr == 0 | terrorism$doubtterr == -9)), ]
terrorism_sub <- terrorism[which(terrorism$iyear == 2014), ]
ter_sub <- terrorism_sub %>% count(country_txt)
colnames(ter_sub)[1] <- "country"

ter_sub$country <- as.character(ter_sub$country)
ter_sub$country[ter_sub$country == "Bosnia-Herzegovina"] <- "Bosnia and Herzegovina"
ter_sub$country[ter_sub$country == "Russia"] <- "Russian Federation"

rm(terrorism, terrorism_sub)

'####SUICIDE####'
suicide$gdp_for_year.... <- as.numeric(as.character(gsub(",","",suicide$gdp_for_year....)))

#var to keep: country (only important for the editing of the indeces), population,
#year, sex, age, suicides.100k.pop, HDI.for.year, gdp_for_year, gdp_per_capita
sui <- suicide[, c("country", "population", "year", "suicides.100k.pop", 
                   "HDI.for.year", "gdp_for_year....", "gdp_per_capita....")] 

#dummy coding
sui_dummy <- createDummyFeatures(suicide[, c("sex", "age")], 
                                 method = "reference")
    #left out: 
    #female
    #first age group (15-24 years)

sui_sub <- cbind(sui, sui_dummy)

#select year
sui_yearsub <- sui_sub[which(sui_sub$year == 2014), ] #has many obs.

#delete country
#Puerto Rico not independent; therefore, not listed
sui_yearsub <- sui_yearsub[which(sui_yearsub$country != "Puerto Rico"), ]

#insert correct HDI 2014 value from website (http://hdr.undp.org/en/content/human-development-index-hdi)
sui_yearsub$HDI.for.year[sui_yearsub$country == "Republic of Korea"] <- 0.896
sui_yearsub$HDI.for.year[sui_yearsub$country == "Russian Federation"] <- 0.807

rm(sui, sui_dummy, sui_sub)

'####HAPPY####'
#change levels that level with most observations is taken as reference level 
#(here: Sub-Saharan Afrika)
happy$Region <- ordered(happy$Region, levels = c("Sub-Saharan Africa",
                                                 "Australia and New Zealand",
                                                 "Central and Eastern Europe", 
                                                 "Eastern Asia",
                                                 "Latin America and Caribbean", 
                                                 "Middle East and Northern Africa",
                                                 "North America", "Southeastern Asia",
                                                 "Southern Asia",
                                                 "Western Europe"))

happy_dummy <- createDummyFeatures(happy[, c("Region")], method = "reference")
happy <- cbind(happy, happy_dummy)

colnames(happy)[1] <- "country"

#adapt country name
happy$country <- as.character(happy$country)
happy$country[happy$country == "Russia"] <- "Russian Federation"

happy <- happy[, c(1,4,6:11,13:21)]

rm(happy_dummy)

'#MERGE DF'
#suicide und happy
sui_hap <- merge(x = sui_yearsub, y = happy, by = "country", all.x = TRUE)

#table(is.na(sui_hap$Happiness.Score))
for(i in 1:nrow(sui_hap)) {
  if (is.na(sui_hap$Happiness.Score[i])) {
    sui_hap$Happiness.Score[i] <- "NA"
  }
}

#missing <- sui_hap[which(sui_hap$Happiness.Score == "NA"), c(1, 5, 7)]
#missing countries: Saint Vincent and Grenadines, Saint Lucia, Cuba, Grenada, 
#Republic of Korea, Antigua and Barbuda, Belize, Seychelles
  #i <- "Seychelles"
  #sum(suicide$suicides.100k.pop[suicide$country == i & suicide$year == 2014])/12
  #region_ave <- sui_hap[which(sui_hap$`Eastern Asia` == 1),]
  #mean(region_ave$HDI.for.year)
  #mean(region_ave$gdp_per_capita....)
  #sum(region_ave$suicides.100k.pop)/12

sui_hap <- sui_hap[which(sui_hap$Happiness.Score != "NA"),]

#terrorism and sui_hap (sht)
sht <- merge(x = sui_hap, y = ter_sub, by = "country", all.x = TRUE)

#countries with no terrorist incident within 2014:
#Armenia, Austria, Belarus, Costa Rica, Croatia, Denmark, Ecuador, El Salvador,
#Estonia, Finland, Guatemala, Kazakhstan, Kuwait, Latvia, Lithuania, Luxembourg,
#Mauritius, Norway, Oman, Poland, Portugal, Qatar, Romania, Serbia, Singapore, 
#Czechoslovakia/Slovakia, Slovenia, Switzerland, Uruguay, Uzbekistan
for(i in 1:nrow(sht)) {
  if (is.na(sht$n[i])) {
    sht$n[i] <- 0
  }
}

rm(happy, sui_hap, sui_yearsub, suicide, ter_sub, i) #npop

#delete columns that are always zero and colums that are not needed for the analysis
sht <- sht[, -c(1:3,28)] 

#change score to numeric value
sht$Happiness.Score <- as.numeric(sht$Happiness.Score)

#rename colnames
colnames(sht) <- c("Suicides_100k_per_Pop", "HDI 2014", "GDP 2014", "GDP/Capita",
                   "Male", "25-34 yearolds", "35-54 yearolds", "5-14 yearolds",
                   "55-74 yearolds", ">75 yearolds", "Happiness Score", 
                   "Economy GDP/Capita", "Family", "Health-Life-Expectancy", 
                   "Freedom", "Trust-Government-Corruption", "Generosity", 
                   "Australia/New Zealand", "Central/Eastern Europe", 
                   "Eastern Asia", "Latin America/Caribbean",
                   "Middle East/Northern Africa", "North America", 
                   "Southeastern Asia", "Western Europe", "Terrorist Incidents")

save(sht,file="sht.Rda")

###############
'ANALSIS'
##############
load("sht.Rda")

#correlation plot
cor <- cor(sht_log[,-c(26)])
png("correlation.png", units="in", width=8, height=5, res=300)
ggcorrplot(cor, colors = c("#6D9EC1", "white", "#E46726"), tl.cex = 8, lab = TRUE, lab_size = 1.5)
dev.off()

#regression models

  #data partitioning
set.seed(222)
ind <- sample(2, nrow(sht), replace = T, prob = c(0.7, 0.3))
train <- sht[ind == 1,]
test <- sht[ind == 2,]

  #multiple linear regression
set.seed(1234)
lm <- lm(Suicides_100k_per_Pop~., data = train, method = "lm")
summary(lm)

library(stargazer)
stargazer(lm, title="Linear regression results", align=T, column.sep.width = "3pt", 
          font.size = "small", intercept.top = T, intercept.bottom = F, style = "all")

p1 <- predict(lm, train)
sqrt(mean((train$Suicides_100k_per_Pop-p1)^2))
p2 <- predict(lm, test)
sqrt(mean((test$Suicides_100k_per_Pop-p2)^2))

#residual diagnostics
png("residualsfit.png", units="in", width=8, height=5, res=300)
par(mfrow=c(2,2))
plot(lm, which = c(1), sub.caption = "")
dev.off()
?plot.lm

png("residuals2.png", units="in", width=8, height=5, res=300)
plot(residuals(lm))
dev.off()

#P: no linear relationship between y and x (R^2 = 0.4926; std. error = 10.02)

  #Transformation

#log (R^2 = 0.7243; std. error. 0.7435)
sht$log_suicides_100k_per_Pop <- log(sht$Suicides_100k_per_Pop)
for(i in 1:nrow(sht)) {
  if (sht$log_suicides_100k_per_Pop[i] == "-Inf") {
    sht$log_suicides_100k_per_Pop[i] <- 0
  }
}
sht_log <- sht[, -1]
sht <- sht[,-27]

#update training und testing set with new data
set.seed(222)
ind <- sample(2, nrow(sht_log), replace = T, prob = c(0.7, 0.3))
train <- sht_log[ind == 1,]
test <- sht_log[ind == 2,]

png("residualslogsum.png", units="in", width=8, height=5, res=300)
par(mfrow=c(2,2))
plot(lm_log, which = c(1,3,5,4), id.n = 9, sub.caption = "")
dev.off()

#analyzing cook's distance
#cook's dist for data: 4/(588-25-1) = 0.007117438
cook <- data.frame(cooks.distance(lm_log))
cook_test <- cook[which(cook > 0.007117438),]

train$cook <- data.frame(cooks.distance(lm_log))
train <- train[which(train$cook < 0.007117438),]
train <- train[,-27]

#leverage
2*26/588

#weiter gehts
set.seed(1234)
lm_log <- lm(log_suicides_100k_per_Pop~., data = train)
summary(lm_log)

p1 <- predict(lm_log, train)
sqrt(mean((train$log_suicides_100k_per_Pop-p1)^2))

p2 <- predict(lm_log, test)
sqrt(mean((test$log_suicides_100k_per_Pop-p2)^2))

#png("residuals2.png", units="in", width=8, height=5, res=300)
plot(residuals(lm_log))

'#transformation techniques

#sqrt
sht$sqrt_suicides_100k_per_Pop <- sqrt(sht$Suicides_100k_per_Pop)
sht_sqrt <- sht[,-1]
sht <- sht[,-27]

set.seed(222)
ind <- sample(2, nrow(sht_sqrt), replace = T, prob = c(0.7, 0.3))
train <- sht_sqrt[ind == 1,]
test <- sht_sqrt[ind == 2,]

set.seed(1234)
lm_sqrt <- lm(sqrt_suicides_100k_per_Pop~., data = train)
summary(lm_sqrt)

p1 <- predict(lm_sqrt, train)
sqrt(mean((train$sqrt_suicides_100k_per_Pop-p1)^2))

p2 <- predict(lm_sqrt, test)
sqrt(mean((test$sqrt_suicides_100k_per_Pop-p2)^2))

#1/Y
sht$y_suicides_100k_per_Pop <- 1/(sht$Suicides_100k_per_Pop)

for(i in 1:nrow(sht)) {
  if (sht$y_suicides_100k_per_Pop[i] == "Inf") {
    sht$y_suicides_100k_per_Pop[i] <- 0
  }
}

sht_y <- sht[,-1]
sht <- sht[,-27]

set.seed(222)
ind <- sample(2, nrow(sht_y), replace = T, prob = c(0.7, 0.3))
train <- sht_y[ind == 1,]
test <- sht_y[ind == 2,]

set.seed(1234)
lm_y <- lm(y_suicides_100k_per_Pop~., data = train)
summary(lm_y)

p1 <- predict(lm_y, train)
sqrt(mean((train$y_suicides_100k_per_Pop-p1)^2))

p2 <- predict(lm_y, test)
sqrt(mean((test$y_suicides_100k_per_Pop-p2)^2))
'
  #multicollinearity
vif <- vif(lm_log)

library(stargazer)
stargazer(vif, title="Variance Inflation Factor", align=F, column.sep.width = "3pt", 
          font.size = "small", style = "all")

#custom control parameters
custom <- trainControl(method = "repeatedcv",
                       number = 10,
                       repeats = 5,
                       verboseIter = T)

  #lm (adding cross validation)
set.seed(1234)
lm_log <- train(log_suicides_100k_per_Pop~., 
                data = train,
                method = "lm",
                trControl = custom)
summary(lm_log)

  #ridge
set.seed(1234)
ridge <- train(log_suicides_100k_per_Pop~., 
               data = train, 
               method = "glmnet",
               tuneGrid = expand.grid(alpha = 0, 
                                      lambda = seq(0.0001, 0.1, length = 20)),
               trControl = custom)
    #optimal lambda = 0.0842; an increasing lambda shrinks coefficients; strength of 
    #the penalty on the coefficients
plot(ridge)
    #root mean squared error with is calculated based on repeated cross validation
    #for higher lambda than 0.0.0842, the error increases

ridge$coefnames
ridge
    #gives us information about the analysis (sample size, no. of predictors, 
    #resampling method (cross validation of 10 folds with 5 times repetition)) and 
    #above the parameters alpha and lambda. Alpha is 0 because a ridge regression
    #is performed and the optimal lambda with is 0.0842

plot(ridge$finalModel, xvar = "lambda", label = T)
    #on the x-axis, log lambda is shown; y-axis presents the coefficients
    #if log lambda is about 7, all coefficient are 0; as log lambda is decreased,
    #the coefficients start to grow/increase
    #on top it shows that all independent variables are kept independently on the 
    #log lambda value
    #log lambda helps to reduce the size of the coefficient but does not select var.
plot(ridge$finalModel, xvar = "dev", label = T)
    #the fraction deviance explained explains how much variability of the model 
    #can be explained and shows the parallel growth of the coefficients
    #jump and inflation of coefficients indicates that overfitting is more likely; 
    #no inflation of variance can be observed
plot(varImp(ridge, scale = T))

  #print results
png("ridgeloglambda.png", units="in", width=8, height=5, res=300)
plot(ridge$finalModel, xvar = "lambda", label = T)
dev.off()
png("ridgedev.png", units="in", width=8, height=5, res=300)
plot(ridge$finalModel, xvar = "dev", label = T)
dev.off()
png("ridgevarimp.png", units="in", width=8, height=5, res=300)
plot(varImp(ridge, scale = T))
dev.off()
png("ridgelambda.png", units="in", width=8, height=5, res=300)
plot(ridge)
dev.off()

#lasso regression
    #advantage: does shrinkage as well as feature selection
    #if there is a group of highly correlated data which cause multicollinearity
    #lasso tends to select one feature from the group and ignore the others
set.seed(1234)
lasso <- train(log_suicides_100k_per_Pop~., 
               data = train, 
               method = "glmnet",
               tuneGrid = expand.grid(alpha = 1, 
                                      lambda = seq(0.0001, 0.01, length = 20)),
               trControl = custom)
plot(lasso)
    #graph shows the higher values of lambda increase the RMSE; the optimal is 
    #towards 0.00114; the optimal lambda is = 0.00114 due to the lasso regression analysis
    #output
lasso$coefnames 
    #shows cross validation results 
plot(lasso$finalModel, xvar = "lambda", label = T)
    #the higher the lambda, the more variables are reduced, only high performing
    #variables are kept till the end
plot(lasso$finalModel, xvar = "dev", label = T)
  #more than 40% of the variability can be explained by 6 variables, more than 50%
  #by 18
  #the var. the grow very rapidly in the end, have less importance and are more
  #likely to cause overfitting
plot(varImp(lasso, scale = T))

  #print results
png("lassologlambda.png", units="in", width=8, height=5, res=300)
plot(lasso$finalModel, xvar = "lambda", label = T)
dev.off()
png("lassodev.png", units="in", width=8, height=5, res=300)
plot(lasso$finalModel, xvar = "dev", label = T)
dev.off()
png("lassovarimp.png", units="in", width=8, height=5, res=300)
plot(varImp(lasso, scale = T))
dev.off()
png("lassolambda.png", units="in", width=8, height=5, res=300)
plot(lasso)
dev.off()

  #compare models
model_list <- list(LinearModel = lm_log, Lasso = lasso, Ridge = ridge)
res <- resamples(model_list)
summary(res)
  #by looking at the results, the lasso regression model has lowest mean of RMSE 
  #as well as the highest R^2
bwplot(res)
xyplot(res, metric = "RMSE") 
  #the models are not very different; a point above the line indicates that the
  #linear models RMSE is slight higher than for ridge; the models above the line
  #perform better with ridge regression

png("RMSElassolm.png", units="in", width=8, height=5, res=300)
xyplot(res, metric = "RMSE", label = T)
dev.off()

  #best model
lasso$bestTune
best <- lasso$finalModel
coef(best, s <- lasso$bestTune$lambda)

  #linear regression with six most important variables regarding to Lasso (best model)
set.seed(1234)
lm_log <- lm(log_suicides_100k_per_Pop~ `HDI 2014`+`5-14 yearolds`+
               `North America`+`Eastern Asia`+`Male`+`Central/Eastern Europe`, 
             data = train)
summary(lm_log)

vif(lm_log)
