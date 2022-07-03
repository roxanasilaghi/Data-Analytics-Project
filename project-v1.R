 #incarcam setul de date pe care il folosim pe parcursul proiectului
weatherAUS <- read.csv("E:/Proiect_BD_vreme/weatherAUS.csv")

#vizualizam setul de date
View(weatherAUS)

 #eliminam coloanele care nu prezinta interes pentru intrebarile propuse
weatherAUS <- weatherAUS[ -c(3,6:21) ] 

#cautam si inlocuim inregistrarile fara valoare cu 0
weatherAUS$MaxTemp[is.na(weatherAUS$MaxTemp)] <-0
weatherAUS$Rainfall[is.na(weatherAUS$Rainfall)] <-0
weatherAUS$RainToday[is.na(weatherAUS$RainToday)] <-"No"
weatherAUS$RainTomorrow[is.na(weatherAUS$RainTomorrow)] <-"No"
#ne asiguram ca nu mai exista in setul de date
any(is.na(weatherAUS))
[1] FALSE
any(is.na(weatherAUS$RainToday))
[1] FALSE
any(is.na(weatherAUS$MaxTemp))
[1] FALSE
any(is.na(weatherAUS$Rainfall))
[1] FALSE
any(is.na(weatherAUS$RainTomorrow))
[1] FALSE

#vizualizam datele sub forma de grafice
#pentru acest lucru avem nevoie de libraria ggplot2 (install.packages("ggplot2");library(ggplot2))
ggplot(weatherAUS) +  geom_point(aes(x=Rainfall, y=MaxTemp, color = RainTomorrow, shape=RainTomorrow))
ggplot(weatherAUS) +geom_boxplot(aes(x=RainTomorrow,y=MaxTemp,fill=RainTomorrow))+  theme(text=element_text(size=12))
ggplot(weatherAUS) +geom_boxplot(aes(x=RainTomorrow,y=Rainfall,fill=RainTomorrow))+ theme(text=element_text(size=12))

#pentru a putea realiza o regresie avem nevoie de libraria caret si pachetele modelr,rsample,corrplot,ROCR;le instalam si uploadam

#cream o reprezentare categorica a datelor cu nume de variabila si frecventa sub forma unui tabel pentru variabila RainTomorrow sa vedem ponderea Yes/No
table(weatherAUS$RainTomorrow)

#afisam structura interna a setului de date pentru a putea observa ce tip de date contine fiecare variabila
str(weatherAUS)

#cream variabile de tip factor
weatherAUS[sapply(weatherAUS, is.character)] <- lapply(weatherAUS[sapply(weatherAUS, is.character)],as.factor)
#am transformat in variabila de tip factor binara variabila RainTomorrow pentru urmatoarele calcule
weatherAUS$RainTomorrow <- ifelse(weatherAUS$RainTomorrow=="Yes",1,0)
weatherAUS$RainTomorrow <- factor(weatherAUS$RainTomorrow, levels = c(0, 1))

#regresia logistica
reglog <- glm(RainTomorrow ~  MaxTemp + Rainfall,  data = weatherAUS, family=binomial)
#vizualizare rezultate obtinute
summary(reglog)

