#loading plm library and the dataset
library(plm)
data <- read.csv("C:/Users/gaura/Downloads/341dataset.csv")

#checking data type
str(data)

#converting the distance variable to a numeric type 
data$Distance..in.km. <- gsub("\\$|,", "", data$Distance..in.km.)
data$dist <- as.numeric(data$Distance..in.km.)

#defining the log variables 
data$T <- log(data$Total..in.mn..USD.)
data$GDPi <- log(data$GDP.India.)
data$GDPj <- log(data$GDP.country.)
data$Dij <- log(data$dist)

#creating a panel data
pdata <-pdata.frame(data, index = c("Country", "Year"))

#fitting a random effects model
re <- plm(T ~ GDPi + GDPj + Dij + Libn + Lang + Bor + MR_final, data  = pdata, model = "random")
fe <- plm(T ~ GDPi + GDPj + Dij + Libn + Lang + Bor + MR_final, data  = pdata, model = "within")

#fitting a Pooled OLS model 
pols <- plm(T ~ GDPi + GDPj + Dij + Libn + Lang + Bor + MR_final, data  = pdata, model = "pooling")

#summarizing the results of all 
summary(re)
summary(pols)
summary(fe)

#running the hausman test to check for preference (only for confirmation)
phtest(fe, re)

#results indicate that Random effects is the better fit, which is obvious as we've taken a lot of time invariant variables as our independent variables 
