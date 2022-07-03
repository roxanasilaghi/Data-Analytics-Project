#Preprocesare date

library(rsample)
library(tidyverse)
library(caret)
library(corrplot)

#incarcare & vizualizare data set

weatherAUS <- read_csv("weatherAUS.csv")
View(weatherAUS)

#eliminare coloane(atribute) nefolositoare studiului nostru & vizualizare
#se poate face si cu select

weatherAUS <- weatherAUS[ -c(1:2,6:21) ]
View(weatherAUS)

#eliminare inregistrari care au cel putin un NA & vizualizare

weatherAUS <- na.omit(weatherAUS)
View(weatherAUS)

#verificare daca mai exista valori NA

any(is.na(weatherAUS))

#factorizare atribute categorice

weatherAUS$RainToday <- factor(weatherAUS$RainToday, levels = c("No", "Yes"))
weatherAUS$RainTomorrow <- factor(weatherAUS$RainTomorrow, levels = c("No", "Yes"))

#vizualizam structura

str(weatherAUS)

#creare tabel de frecventa pe var RainTomorrow

table(weatherAUS$RainTomorrow)
prop.table(table(weatherAUS$RainTomorrow))

#ploturi de densitate

weatherAUS %>% ggplot(aes(MinTemp)) + (geom_density(show.legend=TRUE))
weatherAUS %>% ggplot(aes(MaxTemp)) + (geom_density(show.legend=TRUE))
weatherAUS %>% ggplot(aes(RainFall)) + (geom_density(show.legend=TRUE))

#vizualizam datele

weatherAUS %>% ggplot(aes(MinTemp, AvgTemp)) +geom_point() + geom_smooth()
weatherAUS %>% ggplot(aes(MaxTemp, AvgTemp)) +geom_point() + geom_smooth()
ggplot(weatherAUS) +
  +     geom_boxplot(aes(x=RainTomorrow,y=MaxTemp,fill=RainTomorrow))+
  +     theme(text=element_text(size=12))

ggplot(weatherAUS) +
  +     geom_boxplot(aes(x=RainTomorrow,y=MinTemp,fill=RainTomorrow))+
  +     theme(text=element_text(size=12))

ggplot(weatherAUS) +
  +     geom_boxplot(aes(x=RainTomorrow,y=Rainfall,fill=RainTomorrow))+
  +     theme(text=element_text(size=12))

#matrice cu toate graficele (atribute numerice)

weatherAUS %>% select_if(is.numeric) %>% gather (metric, value) %>% ggplot(aes(value, fill=metric))+geom_density(show.legend=FALSE)+facet_wrap(~metric, scales="free")

#matrice de corelatie intre atribute

weatherAUS %>% filter(RainTomorrow=="Yes") %>% select_if(is.numeric) %>% cor() %>% corrplot::corrplot() 
weatherAUS %>% filter(RainTomorrow=="No") %>% select_if(is.numeric) %>% cor() %>% corrplot::corrplot() 

#Prezicerea precipitațiilor din ziua următoare celei din care provin datele


#cand se foloseste randomizare, inainte de astfel de operatii steam in seed (pentru ca in sesiuni de lucru succesive sa obtinem acelasi rezultate)
set.seed(123)

#ptr a pastra proportia dintre Yes si No a variabilei RainTomorrow (aprox 77% No) facem impartire cu stratificare (strata="RainTomorrow")
split <- initial_split(weatherAUS, prop=0.7, strata ="RainTomorrow")

#pe baza acesui split selectam setul de train si setul de test
train <-training(split)
test <-testing(split)

#verificam daca se pastreaza proportia de Yes si No in seturile de train si test
prop.table(table(train$RainTomorrow))
prop.table(table(test$RainTomorrow))

#RainTomorrow este clasa tinta(y), celelalte variabile sunt x
#features - toate atributele din setul de antrenament care nu sunt RainTomorrow
features <- setdiff(names(train), "RainTomorrow")
x <- train[,features]
y <- train$RainTomorrow

#stabilim o metoda de validare: 10-Folds Cross Validation
fitControl <- trainControl(
  method = "cv",
  number = 10 )

#invatarea modelului naive bayes
modNbSimpleCV <- train(    
  x = x,       
  y = y,       
  method = "nb",    
  trControl = fitControl )   

#afisam modelul de clasificare generat anterior
modNbSimpleCV

#cream o matrice de confuzie
confusionMatrix(modNbSimpleCV)

#facem un search grid
searchGrid <- expand.grid(
  usekernel = c(TRUE, FALSE),
  fL = 0.5,
  adjust = seq(0, 5, by = 1)) 

#cream un model de clasificare cu search grid
modNbSimpleCV <- train(    
  x = x,      
  y = y,      
  method = "nb",    
  trControl = fitControl,tuneGrid = searchGrid)

modNbSimpleCV

#realizam o predictie
pred <- predict(modNbSimpleCV, test)

#afisam predictie
pred

#adaugare probabilitate de predictie la datele prezise 
predProb <- predict(modNbSimpleCV, test, type="prob")

#afisam datele predictiei cu probabilitatile de predictie
predProb

#matricea de confuzie
confusionMatrix(pred, test$RainTomorrow)

#regresia logistica

#generare model de clasificare pentru variabila MinTeMp
mod_MT <- glm(data = weatherAUS, RainTomorrow ~ MinTemp, family = binomial)

summary(mod_MT)

#afisam graficul de predictie in functie de Mintemp
grid <- weatherAUS %>%
  data_grid(Mintemp = seq_range(MinTemp, 100)) %>%
  add_predictions(mod_MT, "prob_RT", type = "response")

#trasare grafic predictie
ggplot()+
  geom_line(data = grid, aes(MinTemp, prob_RT), color = "red", size=2)

#pregatim 2 valori pentru testarea predictiei
nd <- tribble(~MinTemp, 7, 18)

#realizam predictia
pred <- predict(mod_MT, newdata = nd, type = "response")





#generare model de clasificare pentru variabila MaxTeMp
mod_XT <- glm(data = weatherAUS, RainTomorrow ~ MaxTemp, family = binomial)

summary(mod_XT)

#afisam graficul de predictie in functie de Maxtemp
grid <- weatherAUS %>%
  data_grid(MaxTemp = seq_range(MaxTemp, 25)) %>%
  add_predictions(mod_XT, "prob_RT", type = "response")

#trasare grafic predictie
ggplot()+
  geom_line(data = grid, aes(MaxTemp, prob_RT), color = "red")




#generare model de clasificare pentru variabila Rainfall
mod_RF <- glm(data = weatherAUS, RainTomorrow ~ Rainfall, family = binomial)

summary(mod_RF)

#afisam graficul de predictie in functie de Rainfall
grid <- weatherAUS %>%
  data_grid(Rainfall = seq_range(Rainfall, 40)) %>%
  add_predictions(mod_RF, "prob_RT", type = "response")

#trasare grafic predictie
ggplot()+
  geom_line(data = grid, aes(Rainfall, prob_RT), color = "red", size=2)



#generare model de clasificare pentru variabila RainToday
mod_RTD <- glm(data = weatherAUS, RainTomorrow ~ RainToday, family = binomial)

summary(mod_RTD)

#calculam predictia pentru cele doua valori
nd <- tribble(~RainToday, "Yes", "No")

#realizam predictia
pred <- predict(mod_RTD, newdata = nd, type = "response")

pred

#regresia pentru toate variabilele
mod_all <- glm(data = weatherAUS, RainTomorrow ~ MinTemp+MaxTemp+Rainfall+RainToday, family = binomial)

summary(mod_all)


#testarea modelului de clasificare

mod_RT_train <- glm(data = train, RainTomorrow ~ MinTemp + MaxTemp + Rainfall + RainToday, family = binomial)
summary(mod_RT_train)

#predictia pe datele de testare
pred_test <- predict(mod_RT_train, newdata = test, type = "response")  #generarea predictiei
table(pred_test > 0.5, test$RainTomorrow)    #verificarea/analiza efectiva a predictiei


#caret
modGLM_all <- train(x=x, y=y, method="glm", family="binomial", trControl=fitControl)
modGLM_all

confusionMatrix(modGLM_all)

pred_all = predict(modGLM_all, newdata = test, type="raw")

#generare probabilitati predictie
pred_all_prob = predict(modGLM_all, newdata = test, type="prob")

confusionMatrix(pred_all, test$RainTomorrow)

#generarea curbei ROC

library(pROC)
dataset <- data.frame(actual.class <- test$RainTomorrow, probability <- pred_all_prob[,1])
roc.val <- roc(actual.class ~probability, test)
roc.val

adf <- data.frame(x <- roc.val$specificities, y <- roc.val$sensitivities)
ggplot(adf, aes(x, y)) + geom_line(color="blue", size=2) + scale_x_reverse() +labs(y= "sensitivity", x = "specificity") + theme(text=element_text(size=20))

weatherAUS$AvgTemp <- rowMeans(weatherAUS[ , c(1,2)], na.rm=TRUE)

