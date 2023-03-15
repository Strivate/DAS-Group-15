# DAS-Group-15
library(tidyverse)

data <- read.csv("C:/Users/hello/Downloads/dataset15.csv")
coffee <- read.csv("C:/Users/hello/Documents/WeChat Files/wxid_sx7dgnj9k7g912/FileStorage/File/2023-03/coffee15.csv")

coffee <- data %>%
  drop_na() %>%
  filter(!country_of_origin=="Taiwan") %>%
  filter(!harvested %in% c(2010,2011,2012))

head(coffee)

numeric_cols <- sapply(coffee,is.numeric)
numeric_coffee <- coffee[,numeric_cols]
scaled_coffee <- as.data.frame(scale(numeric_coffee))
scaled_coffee$category_two_defects <- coffee$category_two_defects
coffee[,numeric_cols] <- scaled_coffee

n <- nrow(coffee)
for (i in 1:n){
  if (coffee[i,8] == "Good"){
    coffee[i,8] = 1
  }else{
    coffee[i,8] = 0
  }
}

library(ggplot2)
ggplot(coffee,aes(Qualityclass,aroma))+
  geom_boxplot()+
  ylim(-1,1)
ggplot(coffee,aes(Qualityclass,flavor))+
  geom_boxplot()+
  ylim(-1,1)
ggplot(coffee,aes(Qualityclass,acidity))+
  geom_boxplot()+
  ylim(-1,1)
ggplot(coffee,aes(Qualityclass,category_two_defects))+
  geom_boxplot()+
  ylim(0,10)
ggplot(coffee,aes(Qualityclass,altitude_mean_meters))+
  geom_boxplot()+
  ylim(-0.2,0.1)
ggplot(coffee,aes(Qualityclass,harvested))+
  geom_boxplot()



quantiles_aroma <- quantile(coffee$aroma,probs=c(0.25,0.75),na.rm=FALSE)
IQR_aroma <- IQR(coffee$aroma)
Lower_aroma <- quantiles_aroma[1]-1.5*IQR_aroma
Upper_aroma <- quantiles_aroma[2]+1.5*IQR_aroma
coffee<- subset(coffee,coffee$aroma>Lower_aroma & coffee$aroma< Upper_aroma)

quantiles_flavor <- quantile(coffee$flavor,probs=c(0.25,0.75),na.rm=FALSE)
IQR_flavor <- IQR(coffee$flavor)
Lower_flavor <- quantiles_flavor[1]-1.5*IQR_flavor
Upper_flavor <- quantiles_flavor[2]+1.5*IQR_flavor
coffee<- subset(coffee,coffee$flavor>Lower_flavor & coffee$flavor< Upper_flavor)

quantiles_acidity <- quantile(coffee$acidity,probs=c(0.25,0.75),na.rm=FALSE)
IQR_acidity <- IQR(coffee$acidity)
Lower_acidity <- quantiles_acidity[1]-1.5*IQR_acidity
Upper_acidity <- quantiles_acidity[2]+1.5*IQR_acidity
coffee<- subset(coffee,coffee$acidity>Lower_acidity & coffee$acidity< Upper_acidity)


# write.csv(coffee15,file="coffee15.1.csv",row.names=FALSE)


coffee$Qualityclass <- as.factor(coffee$Qualityclass)



model1 <- glm(Qualityclass~aroma+flavor+acidity,family=binomial(link="logit"),
             data=coffee)
model2 <- glm(Qualityclass~aroma*flavor+acidity,family=binomial(link="logit"),
             data=coffee)
model3 <- glm(Qualityclass~aroma+flavor*acidity,family=binomial(link="logit"),
             data=coffee)
model4 <- glm(Qualityclass~aroma*acidity+flavor,family=binomial(link="logit"),
             data=coffee)
model5 <- glm(Qualityclass~aroma*flavor+aroma*acidity,family=binomial(link="logit"),
             data=coffee)
model6 <- glm(Qualityclass~aroma*flavor+acidity*flavor,family=binomial(link="logit"),
             data=coffee)
model7 <- glm(Qualityclass~aroma*acidity+flavor*acidity,family=binomial(link="logit"),
             data=coffee)
model8 <- glm(Qualityclass~aroma*flavor+acidity*flavor+acidity*aroma,family=binomial(link="logit"),
             data=coffee)
model9 <- glm(Qualityclass~aroma*flavor*acidity,family=binomial(link="logit"),
             data=coffee)

anova(model1,model2,model3,model4,model5,model6,model7,
      model8,model9)
summary(model9)
AIC(model1)
AIC(model2)
AIC(model3)
AIC(model4)
AIC(model5)
AIC(model6)
AIC(model7)
AIC(model8)
AIC(model9)
