library(openxlsx)
library(GGally)
#1. Дана выборка. 
# Рассчитать для каждого признака дисперсию, максимальный элемент, 35-й процентиль, коэффициент вариции
Data <- read.xlsx("~/r_projects/KR/t.xlsx", sheet=1)
#Дисперсия
var(Data$Показатель.экономической.эффективности.6)
#Максимальный элемент
max(Data$Показатель.экономической.эффективности.6)
#35 процентиль
quantile(Data$Показатель.экономической.эффективности.6, probs = 0.35)
#Коэффициент вариации
sd(Data$Показатель.экономической.эффективности.6)/mean(Data$Показатель.экономической.эффективности.6)
for (i in 1:ncol(Data)) {
  print(colnames(Data)[i])
  print(var(Data[,i]))
}

#2. Дана выборка. 
# Рассчитать для каждого признака стандартное отклонение, медиану, 6-й дециль, размах выборки
Data <- read.xlsx("~/r_projects/KR/t.xlsx", sheet=2)
#Стандартное отклонение
sd(Data$Важный.фактор.качества.11)
#Медиана
median(Data$Важный.фактор.качества.11)
#6-й дециль
quantile(Data$Важный.фактор.качества.11, probs=0.6)
#Размах выборки
max(Data$Важный.фактор.качества.11)-min(Data$Важный.фактор.качества.11)
library(dplyr)
#3. Дана выборка.
# Необходимо ее стандартизовать и для преобразованных значений всех признаков  рассчитать размах выборки
Data <- read.xlsx("~/r_projects/KR/t.xlsx", sheet=3)
#Стандратизация
a <- Data[,1]
###
mean_a <- mean(a)
sd_a <- sd(a)
a <- (a - mean_a) / sd_a
# OR
a <- scale(a)
###
#Размах выборки
max(a)-min(a)

#4. Дана выборка. 
# Необходимо ее стандартизовать и для преобразованных значений всех признаков  рассчитать интерквартильный размах выборки
Data <- read.xlsx("~/r_projects/KR/t.xlsx", sheet=4)
#Стандратизация
a <- (Data$Важный.фактор.качества.2 - mean(Data$Важный.фактор.качества.2))/sd(Data$Важный.фактор.качества.2)
b <- scale(Data$Важный.фактор.качества.2)
sum(a-b)
#Размах выборки
quantile(a, probs=0.75)-quantile(a, probs=0.25)

#5. Дана выборка. 
# Необходимо ее отнормировать каждый признак относительно максимального элемента и 
# для преобразованных значений  рассчитать среднее, дисперсию, интерквартильный размах
Data <- read.xlsx("~/r_projects/KR/t.xlsx", sheet=5)
#Отнормировать каждый признак относительно максимального элемента
a <- Data$Показатель.экономической.эффективности.9/max(Data$Показатель.экономической.эффективности.9)
#Среднее
mean(a)
#Дисперсия
var(a)
#Интерквантильного размаха
quantile(a, probs=0.75)-quantile(a, probs=0.25)

#6. Дана выборка, 
# отобрать наблюдения соответсвующие условию и для каждого количественного признака 
# рассчитать среднее, дисперсию, интерквартильный размах и коэффициент вариации v
# Условие: 
# Все элементы которые имеют цвет grey и значения Показатель экономической эффективности 12 Меньше чем 1.83179421965565
Data <- read.xlsx("~/r_projects/KR/t.xlsx", sheet=6)
Data1 <- filter(Data, Colors=='grey' & Показатель.экономической.эффективности.12 < 1.83179421965565)
#Среднее
mean(Data1$Показатель.экономической.эффективности.2)
#Дисперсию
var(Data1$Показатель.экономической.эффективности.2)
#Интерквантильный размах
quantile(Data1$Показатель.экономической.эффективности.2, probs=0.75)-quantile(Data1$Показатель.экономической.эффективности.2, probs=0.25)
#Коэффициент вариации
sd(Data1$Показатель.экономической.эффективности.2)/mean(Data1$Показатель.экономической.эффективности.2)

#7. Дана выборка - провести группировку по заданному правилу и построить график накопленных частностей
#Правило : Scott
Data <- read.xlsx("~/r_projects/KR/t.xlsx", sheet=7)
hist(x = Data[,1], freq = TRUE, breaks = "Scott")

#8. Провести группировку заданным методом и 
# посчитать среднее и дисперсию по всем получившимся группам
Data <- read.xlsx("~/r_projects/KR/t.xlsx", sheet=8)
H <- hist(x = Data$Выборка, freq = TRUE, breaks = "FD")
Data1 <- H$breaks
Data <- Data$Выборка
Data <- sort(Data)
l <- length(Data1)
j <- 1
for (i in 2 : length(Data1)) {
  a <- numeric(0)
  while (Data[j] <= Data1[i]) {
    a <- append(a, Data[j])
    j = j + 1
    if (j == length(Data) + 1) {
      break
    }
  }
  print(mean(a))
  print(var(a))
  print("----------")
}

mean(Data1)
var(Data1)

#9. По заданной выборке построить ковариационную матрицу и 
# найти пару признаков обладающих наибольшим ковариационным значением
Data <- read.xlsx("~/r_projects/KR/t.xlsx", sheet=9)
cov(Data)
heatmap(x = cov(Data), Rowv = NA, Colv = NA, col = heat.colors(n = 200))
Data <- cov(Data)
ggcorr(Data, label=T, label_round = 3, label_size = 2)

#10. По заданной выборке построить ковариационную матрицу и найти пару признаков обладающих наименьшим ковариационным значением
Data <- read.xlsx("~/r_projects/KR/t.xlsx", sheet=10)
cov(Data)
heatmap(x = cov(Data), Rowv = NA, Colv = NA, col = heat.colors(n = 200))
Data <- cov(Data)
ggcorr(Data, label=T, label_round = 3, label_size = 2)

#11. По заданной выборке построить корреляционную матрицу и найти все пары признаков обладающих незначимой корреляцией
Data <- read.xlsx("~/r_projects/KR/t.xlsx", sheet=11)
heatmap(x = cor(Data), Rowv = NA, Colv = NA, col = heat.colors(n = 200))
Data <- cor(Data)
ggcorr(Data, label=T, label_round = 3, label_size = 2)

#12. Дана выборка - пользуясь тестом Колмогорова-Смирнова определить принадлежит ли закону?
Data <- read.xlsx("~/r_projects/KR/t.xlsx", sheet=12)
maxD <- max(Data$X)
minD <- min(Data$X)
DataBeta <- sort((Data$X - minD)/(maxD-minD))
myBeta <- EnvStats::ebeta(DataBeta)
a <- myBeta$parameters[1]
a
b <- myBeta$parameters[2]
ks.test(x=unique(DataBeta), y=pbeta, a, b)
hist(x=DataBeta, freq=F, breaks="Scott")

#13. Дана выборка показателя для разных априорных групп - 
# доказать с помощью теста Манна-Уитни, что априорные группы различаются
Data <- read.xlsx("~/r_projects/KR/t.xlsx", sheet=13)
GroupRed <- Data$Показатель[Data$Группа=="Red"]
GroupBlue <- Data$Показатель[Data$Группа=="Blue"]
wilcox.test(GroupRed, GroupBlue)

#14. Для заданных значений показателя Y и факторов X1…Xk построить линейную регрессию и 
# построить гистограмму остатков. 
# Доказать, что они распределены по нормальному закону
Data <- read.xlsx("~/r_projects/KR/t.xlsx", sheet=14)
LM <- lm(formula=Исследуемый.показатель ~ ., data = Data)
summary(LM)
R <- unique(sort(LM$residuals))
hist(R, breaks="Scott", freq=F)
length(R)
y <- rnorm(mean = mean(R), sd=sd(R),4002)
lines(x=density(y), lwd=2)
shapiro.test(R)

R_N <- scale(R)
hist(R_N, breaks="Scott", freq=F)

MaxD <- quantile(R, probs=0.75)
MinD <- quantile(R, probs=0.25)
R_Min <- R[R >= MinD]
R_MinMax <- R_Min[R_Min <= MaxD]
hist(R_MinMax, breaks="Scott")
ks.test(x=R_MinMax, y=rnorm(mean = mean(R_MinMax), sd=sd(R_MinMax),length(R_MinMax)))

iqrR <- IQR(R)
R_Min_iqr <- R[R >= (MinD - 1.5*iqrR)]
R_MinMax_iqr <- R_Min_iqr[R_Min_iqr <= (MaxD + 1.5*iqrR)]
hist(R_MinMax_iqr, breaks="Scott", freq=F)
y <- rnorm(mean = mean(R_MinMax_iqr), sd=sd(R_MinMax_iqr),length(R_MinMax_iqr))
lines(x=density(y), lwd=2)
ks.test(x=R_MinMax_iqr, y=y)


#15. Дано - результативный показатель и несколько потенциальных факторов. 
# Построить все возможные регрессионных модели из комбинаций этих факторов, выбрать лучшую по значений коэф.детерминации

Data <- read.xlsx("~/r_projects/KR/t.xlsx", sheet=15)
LM <- lm(formula=Исследуемый.показатель ~ ., data = Data)
summary(LM)
optimalLM <- step(LM)
summary(optimalLM)
names(Data)
LM1 <- lm(formula=Исследуемый.показатель ~ Показатель.экономической.эффективности.10, data = Data)
LM2 <- lm(formula=Исследуемый.показатель ~ Показатель.экономической.эффективности.5, data = Data)
LM3 <- lm(formula=Исследуемый.показатель ~ Показатель.экономической.эффективности.2, data = Data)
LM4 <- lm(formula=Исследуемый.показатель ~ Показатель.экономической.эффективности.20, data = Data)
summary(LM1)
summary(LM2)
summary(LM3)
summary(LM4)

#...

#16. Дано - результативный показатель и множество потенциальных факторов. 
# Среди них - найти такое подмножество, 
# что позволил бы получить регрессионную модель с наибольшим значеним коэффициента детерминации. 
# Проверить не менее чем на 10 разных комбинациях 
Data <- read.xlsx("~/r_projects/KR/t.xlsx", sheet=16)
LM <- lm(formula=Исследуемый.показатель ~ ., data = Data)
summary(LM)
optimalLM <- step(LM)
summary(optimalLM)

#17. Дана выборка по среднемесячной цене финансовых инструментов. Найти наиболее волатильный (рискованный) актив
Data <- read.xlsx("~/r_projects/KR/t.xlsx", sheet=17)
length(Data)
allActiv <- data.frame("Актив"=names(Data)[2:17])
volatil <- c()
for (a in Data[2:17]){
  print(sd(a))
  volatil <- append(volatil, sd(a))
}
allActiv <- cbind(allActiv, "Волатильность"=volatil)
risk <- allActiv$Актив[allActiv$Волатильность == max(allActiv$Волатильность)]
risk

#18. Дана выборка по среднемесячной цене финансовых инструментов. 
# Доказать, что среди ценовых значений не было  выбросов
Data <- read.xlsx("~/r_projects/KR/t.xlsx", sheet=18, detectDates = T)
length(Data)
boxplot(Data[,2])

Data2 <- Data[,2]

Data_Z <- Data2[((Data2-mean(Data2))/sd(Data2))  >= -3]
Data_Z <- Data_Z[((Data_Z-mean(Data2))/sd(Data2))  <= 3]
length(Data2)
length(Data_Z)

MaxD <- quantile(Data2, probs=0.75)
MinD <- quantile(Data2, probs=0.25)
iqrData2 <- IQR(Data2)
Data2_iqr <- Data2[Data2 >= (MinD - 1.5*iqrData2)]
Data2_iqr <- Data2_iqr[Data2_iqr <= (MaxD + 1.5*iqrData2)]
length(Data2)
length(Data2_iqr)

#19. Для временного ряда провести сглаживание скользящей средней и построить линейный тренд. 
# Расчитать остаточный коэффициент вариации тренда от сглаженного ряда
library(lubridate)
library(xts)
library(forecast)
DataFile <- read.xlsx("~/r_projects/KR/t.xlsx", sheet=19, detectDates = T)
Data <- DataFile[,2]
h <- 20
MA20 <- ma(x = Data, order=41)
plot(x=Data,main="Сглаживание временного ряда")
lines(x= MA20,col='magenta', lwd=3)
d <- seq(from=1, to=length(MA20), by=1)
DataSglazhenie <- cbind("Data" = MA20,"Ind"= d)

LinTrend <- lm(data=DataSglazhenie, formula=formula(Data~Ind))
lines(LinTrend$fitted.values, col="green", lwd=3)

#20. Для временного ряда провести сглаживание моделью Брауна и построить линейный тренд. 
# Расчитать остаточный коэффициент вариации тренда от сглаженного ряда
library(tsbox)
DataFile <- read.xlsx("~/r_projects/KR/t.xlsx", sheet=20, detectDates = T)
Data <- DataFile[,2]
ETS_ann <- ets(Data, model="ANN")
plot(x=Data,main="Сглаживание временного ряда")
lines(ETS_ann$fitted, col="magenta", lwd=3)

d <- seq(from=1, to=length(ETS_ann$fitted), by=1)
DataSglazhenie <- cbind("Data" = ETS_ann$fitted,"Ind"= d)
LinTrend <- lm(data=DataSglazhenie, formula=formula(Data~Ind))
lines(LinTrend$fitted.values, col="green", lwd=3)

d <- seq(from=1, to=length(Data), by=1)
DataSglazhenie <- data.frame("Data" = Data,"Ind"= d)
LinTrend <- lm(data=DataSglazhenie, formula=formula(Data~Ind))
lines(LinTrend$fitted.values, col="orange", lwd=2)

#21. Дано несколько временных рядов, привести их к единому масштабу, использую максимум ряда. 
# В ответ представить полученный график

DataFile <- read.xlsx("~/r_projects/KR/t.xlsx", sheet=21, detectDates = T)
DataFile$Временной.ряд.6 <- DataFile$Временной.ряд.6/max(DataFile$Временной.ряд.6)
DataFile$Временной.ряд.5 <- DataFile$Временной.ряд.5/max(DataFile$Временной.ряд.5)
DataFile$Временной.ряд.8 <- DataFile$Временной.ряд.8/max(DataFile$Временной.ряд.8)
DataFile$Временной.ряд.4 <- DataFile$Временной.ряд.4/max(DataFile$Временной.ряд.4)
DataFile$Временной.ряд.11 <- DataFile$Временной.ряд.11/max(DataFile$Временной.ряд.11)
DataFile$Временной.ряд.1 <- DataFile$Временной.ряд.1/max(DataFile$Временной.ряд.1)
DataFile$Временной.ряд.10 <- DataFile$Временной.ряд.10/max(DataFile$Временной.ряд.10)
DataFile$Временной.ряд.3 <- DataFile$Временной.ряд.3/max(DataFile$Временной.ряд.3)
DataFile$Временной.ряд.12 <- DataFile$Временной.ряд.12/max(DataFile$Временной.ряд.12)
DataFile$Временной.ряд.7 <- DataFile$Временной.ряд.7/max(DataFile$Временной.ряд.7)

plot(x=DataFile$Дата, y=DataFile$Временной.ряд.6, type='l')
lines(x=DataFile$Дата, y=DataFile$Временной.ряд.5, col='red')
lines(x=DataFile$Дата, y=DataFile$Временной.ряд.8, col='blue')
lines(x=DataFile$Дата, y=DataFile$Временной.ряд.4, col='purple')
lines(x=DataFile$Дата, y=DataFile$Временной.ряд.11, col='green')
lines(x=DataFile$Дата, y=DataFile$Временной.ряд.1, col='orange')
lines(x=DataFile$Дата, y=DataFile$Временной.ряд.10, col='pink')
lines(x=DataFile$Дата, y=DataFile$Временной.ряд.3, col='yellow')
lines(x=DataFile$Дата, y=DataFile$Временной.ряд.12, col='gray')
lines(x=DataFile$Дата, y=DataFile$Временной.ряд.7, col='darkblue')


#22. Дано несколько временных рядов, привести их к единому масштабу, использую старт ряда. 
# В ответ представить график
DataFile <- read.xlsx("~/r_projects/KR/t.xlsx", sheet=22, detectDates = T)
names(DataFile)
plot(x=DataFile$Дата, y=DataFile$Временной.ряд.11, type='l')
lines(x=DataFile$Дата, y=DataFile$Временной.ряд.4, col='red')
lines(x=DataFile$Дата, y=DataFile$Временной.ряд.8, col='blue')
lines(x=DataFile$Дата, y=DataFile$Временной.ряд.1, col='purple')
lines(x=DataFile$Дата, y=DataFile$Временной.ряд.12, col='green')
lines(x=DataFile$Дата, y=DataFile$Временной.ряд.10, col='orange')
lines(x=DataFile$Дата, y=DataFile$Временной.ряд.7, col='pink')
lines(x=DataFile$Дата, y=DataFile$Временной.ряд.9, col='yellow')
lines(x=DataFile$Дата, y=DataFile$Временной.ряд.5, col='gray')
lines(x=DataFile$Дата, y=DataFile$Временной.ряд.2, col='darkblue')

DataFile$Временной.ряд.11 <- DataFile$Временной.ряд.11/DataFile$Временной.ряд.11[1]
DataFile$Временной.ряд.4 <- DataFile$Временной.ряд.4/DataFile$Временной.ряд.4[1]
DataFile$Временной.ряд.8 <- DataFile$Временной.ряд.8/DataFile$Временной.ряд.8[1]
DataFile$Временной.ряд.1 <- DataFile$Временной.ряд.1/DataFile$Временной.ряд.1[1]
DataFile$Временной.ряд.12 <- DataFile$Временной.ряд.12/DataFile$Временной.ряд.12[1]
DataFile$Временной.ряд.10 <- DataFile$Временной.ряд.10/DataFile$Временной.ряд.10[1]
DataFile$Временной.ряд.7 <- DataFile$Временной.ряд.7/DataFile$Временной.ряд.7[1]
DataFile$Временной.ряд.9 <- DataFile$Временной.ряд.9/DataFile$Временной.ряд.9[1]
DataFile$Временной.ряд.5 <- DataFile$Временной.ряд.5/DataFile$Временной.ряд.5[1]
DataFile$Временной.ряд.2 <- DataFile$Временной.ряд.2/DataFile$Временной.ряд.2[1]

plot(x=DataFile$Дата, y=DataFile$Временной.ряд.11, type='l', ylim=c(0,2))
lines(x=DataFile$Дата, y=DataFile$Временной.ряд.4, col='red')
lines(x=DataFile$Дата, y=DataFile$Временной.ряд.8, col='blue')
lines(x=DataFile$Дата, y=DataFile$Временной.ряд.1, col='purple')
lines(x=DataFile$Дата, y=DataFile$Временной.ряд.12, col='green')
lines(x=DataFile$Дата, y=DataFile$Временной.ряд.10, col='orange')
lines(x=DataFile$Дата, y=DataFile$Временной.ряд.7, col='pink')
lines(x=DataFile$Дата, y=DataFile$Временной.ряд.9, col='yellow')
lines(x=DataFile$Дата, y=DataFile$Временной.ряд.5, col='gray')
lines(x=DataFile$Дата, y=DataFile$Временной.ряд.2, col='darkblue')



DataFile <- read.xlsx("~/r_projects/KR/t.xlsx", sheet=22, detectDates = T)
names(DataFile)
plot(x=DataFile$Дата, y=DataFile$Временной.ряд.11, type='l')
lines(x=DataFile$Дата, y=DataFile$Временной.ряд.4, col='red')
lines(x=DataFile$Дата, y=DataFile$Временной.ряд.8, col='blue')
lines(x=DataFile$Дата, y=DataFile$Временной.ряд.1, col='purple')
lines(x=DataFile$Дата, y=DataFile$Временной.ряд.12, col='green')
lines(x=DataFile$Дата, y=DataFile$Временной.ряд.10, col='orange')
lines(x=DataFile$Дата, y=DataFile$Временной.ряд.7, col='pink')
lines(x=DataFile$Дата, y=DataFile$Временной.ряд.9, col='yellow')
lines(x=DataFile$Дата, y=DataFile$Временной.ряд.5, col='gray')
lines(x=DataFile$Дата, y=DataFile$Временной.ряд.2, col='darkblue')

DataFile$Временной.ряд.11 <- ((DataFile$Временной.ряд.11-DataFile$Временной.ряд.11[1])/DataFile$Временной.ряд.11[1]+1)*0.5
DataFile$Временной.ряд.4 <- ((DataFile$Временной.ряд.4-DataFile$Временной.ряд.4[1])/DataFile$Временной.ряд.4[1]+1)*0.5
DataFile$Временной.ряд.8 <- ((DataFile$Временной.ряд.8-DataFile$Временной.ряд.8[1])/DataFile$Временной.ряд.8[1]+1)*0.5
DataFile$Временной.ряд.1 <- ((DataFile$Временной.ряд.1-DataFile$Временной.ряд.1[1])/DataFile$Временной.ряд.1[1]+1)*0.5
DataFile$Временной.ряд.12 <- ((DataFile$Временной.ряд.12-DataFile$Временной.ряд.12[1])/DataFile$Временной.ряд.12[1]+1)*0.5
DataFile$Временной.ряд.10 <- ((DataFile$Временной.ряд.10-DataFile$Временной.ряд.10[1])/DataFile$Временной.ряд.10[1]+1)*0.5
DataFile$Временной.ряд.7 <- ((DataFile$Временной.ряд.7-DataFile$Временной.ряд.7[1])/DataFile$Временной.ряд.7[1]+1)*0.5
DataFile$Временной.ряд.9 <- ((DataFile$Временной.ряд.9-DataFile$Временной.ряд.9[1])/DataFile$Временной.ряд.9[1]+1)*0.5
DataFile$Временной.ряд.5 <- ((DataFile$Временной.ряд.5-DataFile$Временной.ряд.5[1])/DataFile$Временной.ряд.5[1]+1)*0.5
DataFile$Временной.ряд.2 <- ((DataFile$Временной.ряд.2-DataFile$Временной.ряд.2[1])/DataFile$Временной.ряд.2[1]+1)*0.5

plot(x=DataFile$Дата, y=DataFile$Временной.ряд.11, type='l', yli=c(0,1))
lines(x=DataFile$Дата, y=DataFile$Временной.ряд.4, col='red')
lines(x=DataFile$Дата, y=DataFile$Временной.ряд.8, col='blue')
lines(x=DataFile$Дата, y=DataFile$Временной.ряд.1, col='purple')
lines(x=DataFile$Дата, y=DataFile$Временной.ряд.12, col='green')
lines(x=DataFile$Дата, y=DataFile$Временной.ряд.10, col='orange')
lines(x=DataFile$Дата, y=DataFile$Временной.ряд.7, col='pink')
lines(x=DataFile$Дата, y=DataFile$Временной.ряд.9, col='yellow')
lines(x=DataFile$Дата, y=DataFile$Временной.ряд.5, col='gray')
lines(x=DataFile$Дата, y=DataFile$Временной.ряд.2, col='darkblue')


#### криво 1
DataFile <- read.xlsx("~/r_projects/KR/t.xlsx", sheet=22, detectDates = T)
names(DataFile)
plot(x=DataFile$Дата, y=DataFile$Временной.ряд.11, type='l')
lines(x=DataFile$Дата, y=DataFile$Временной.ряд.4, col='red')
lines(x=DataFile$Дата, y=DataFile$Временной.ряд.8, col='blue')
lines(x=DataFile$Дата, y=DataFile$Временной.ряд.1, col='purple')
lines(x=DataFile$Дата, y=DataFile$Временной.ряд.12, col='green')
lines(x=DataFile$Дата, y=DataFile$Временной.ряд.10, col='orange')
lines(x=DataFile$Дата, y=DataFile$Временной.ряд.7, col='pink')
lines(x=DataFile$Дата, y=DataFile$Временной.ряд.9, col='yellow')
lines(x=DataFile$Дата, y=DataFile$Временной.ряд.5, col='gray')
lines(x=DataFile$Дата, y=DataFile$Временной.ряд.2, col='darkblue')

DataFile$Временной.ряд.11 <- (DataFile$Временной.ряд.11-DataFile$Временной.ряд.11[1])/(max(DataFile$Временной.ряд.11)-DataFile$Временной.ряд.11[1])
DataFile$Временной.ряд.4 <- (DataFile$Временной.ряд.4-DataFile$Временной.ряд.4[1])/(max(DataFile$Временной.ряд.4)-DataFile$Временной.ряд.4[1])
DataFile$Временной.ряд.8 <- (DataFile$Временной.ряд.8-DataFile$Временной.ряд.8[1])/(max(DataFile$Временной.ряд.8)-DataFile$Временной.ряд.8[1])
DataFile$Временной.ряд.1 <- (DataFile$Временной.ряд.1-DataFile$Временной.ряд.1[1])/(max(DataFile$Временной.ряд.1)-DataFile$Временной.ряд.1[1])
DataFile$Временной.ряд.12 <- (DataFile$Временной.ряд.12-DataFile$Временной.ряд.12[1])/(max(DataFile$Временной.ряд.12)-DataFile$Временной.ряд.12[1])
DataFile$Временной.ряд.10 <- (DataFile$Временной.ряд.10-DataFile$Временной.ряд.10[1])/(max(DataFile$Временной.ряд.10)-DataFile$Временной.ряд.10[1])
DataFile$Временной.ряд.7 <- (DataFile$Временной.ряд.7-DataFile$Временной.ряд.7[1])/(max(DataFile$Временной.ряд.7)-DataFile$Временной.ряд.7[1])
DataFile$Временной.ряд.9 <- (DataFile$Временной.ряд.9-DataFile$Временной.ряд.9[1])/(max(DataFile$Временной.ряд.9)-DataFile$Временной.ряд.9[1])
DataFile$Временной.ряд.5 <- (DataFile$Временной.ряд.5-DataFile$Временной.ряд.5[1])/(max(DataFile$Временной.ряд.5)-DataFile$Временной.ряд.5[1])
DataFile$Временной.ряд.2 <- (DataFile$Временной.ряд.2-DataFile$Временной.ряд.2[1])/(max(DataFile$Временной.ряд.2)-DataFile$Временной.ряд.2[1])

plot(x=DataFile$Дата, y=DataFile$Временной.ряд.11, type='l',ylim=c(-1,1))
lines(x=DataFile$Дата, y=DataFile$Временной.ряд.4, col='red')
lines(x=DataFile$Дата, y=DataFile$Временной.ряд.8, col='blue')
lines(x=DataFile$Дата, y=DataFile$Временной.ряд.1, col='purple')
lines(x=DataFile$Дата, y=DataFile$Временной.ряд.12, col='green')
lines(x=DataFile$Дата, y=DataFile$Временной.ряд.10, col='orange')
lines(x=DataFile$Дата, y=DataFile$Временной.ряд.7, col='pink')
lines(x=DataFile$Дата, y=DataFile$Временной.ряд.9, col='yellow')
lines(x=DataFile$Дата, y=DataFile$Временной.ряд.5, col='gray')
lines(x=DataFile$Дата, y=DataFile$Временной.ряд.2, col='darkblue')


### криво 2

DataFile <- read.xlsx("~/r_projects/KR/t.xlsx", sheet=22, detectDates = T)
names(DataFile)
plot(x=DataFile$Дата, y=DataFile$Временной.ряд.11, type='l')
lines(x=DataFile$Дата, y=DataFile$Временной.ряд.4, col='red')
lines(x=DataFile$Дата, y=DataFile$Временной.ряд.8, col='blue')
lines(x=DataFile$Дата, y=DataFile$Временной.ряд.1, col='purple')
lines(x=DataFile$Дата, y=DataFile$Временной.ряд.12, col='green')
lines(x=DataFile$Дата, y=DataFile$Временной.ряд.10, col='orange')
lines(x=DataFile$Дата, y=DataFile$Временной.ряд.7, col='pink')
lines(x=DataFile$Дата, y=DataFile$Временной.ряд.9, col='yellow')
lines(x=DataFile$Дата, y=DataFile$Временной.ряд.5, col='gray')
lines(x=DataFile$Дата, y=DataFile$Временной.ряд.2, col='darkblue')

DataFile$Временной.ряд.11 <- (DataFile$Временной.ряд.11-min(DataFile$Временной.ряд.11))/(DataFile$Временной.ряд.11[1]-min(DataFile$Временной.ряд.11))
DataFile$Временной.ряд.4 <- (DataFile$Временной.ряд.4-min(DataFile$Временной.ряд.4))/(DataFile$Временной.ряд.4[1]-min(DataFile$Временной.ряд.4))
DataFile$Временной.ряд.8 <- (DataFile$Временной.ряд.8-min(DataFile$Временной.ряд.8))/(DataFile$Временной.ряд.8[1]-min(DataFile$Временной.ряд.8))
DataFile$Временной.ряд.1 <- (DataFile$Временной.ряд.1-min(DataFile$Временной.ряд.1))/(DataFile$Временной.ряд.1[1]-min(DataFile$Временной.ряд.1))
DataFile$Временной.ряд.12 <- (DataFile$Временной.ряд.12-min(DataFile$Временной.ряд.12))/(DataFile$Временной.ряд.12[1]-min(DataFile$Временной.ряд.12))
DataFile$Временной.ряд.10 <- (DataFile$Временной.ряд.10-min(DataFile$Временной.ряд.10))/(DataFile$Временной.ряд.10[1]-min(DataFile$Временной.ряд.10))
DataFile$Временной.ряд.7 <- (DataFile$Временной.ряд.7-min(DataFile$Временной.ряд.7))/(DataFile$Временной.ряд.7[1]-min(DataFile$Временной.ряд.7))
DataFile$Временной.ряд.9 <- (DataFile$Временной.ряд.9-min(DataFile$Временной.ряд.9))/(DataFile$Временной.ряд.9[1]-min(DataFile$Временной.ряд.9))
DataFile$Временной.ряд.5 <- (DataFile$Временной.ряд.5-min(DataFile$Временной.ряд.5))/(DataFile$Временной.ряд.5[1]-min(DataFile$Временной.ряд.5))
DataFile$Временной.ряд.2 <- (DataFile$Временной.ряд.2-min(DataFile$Временной.ряд.2))/(DataFile$Временной.ряд.2[1]-min(DataFile$Временной.ряд.2))

plot(x=DataFile$Дата, y=DataFile$Временной.ряд.11, type='l',ylim=c(0,1))
lines(x=DataFile$Дата, y=DataFile$Временной.ряд.4, col='red')
lines(x=DataFile$Дата, y=DataFile$Временной.ряд.8, col='blue')
lines(x=DataFile$Дата, y=DataFile$Временной.ряд.1, col='purple')
lines(x=DataFile$Дата, y=DataFile$Временной.ряд.12, col='green')
lines(x=DataFile$Дата, y=DataFile$Временной.ряд.10, col='orange')
lines(x=DataFile$Дата, y=DataFile$Временной.ряд.7, col='pink')
lines(x=DataFile$Дата, y=DataFile$Временной.ряд.9, col='yellow')
lines(x=DataFile$Дата, y=DataFile$Временной.ряд.5, col='gray')
lines(x=DataFile$Дата, y=DataFile$Временной.ряд.2, col='darkblue')

#23. Дано несколько временных рядов, привести их к единому масштабу, использую финал ряда. 
# В ответ представить график
DataFile <- read.xlsx("~/r_projects/KR/t.xlsx", sheet=23, detectDates = T)
names(DataFile)
length(DataFile$Дата)
plot(x=DataFile$Дата, y=DataFile$Временной.ряд.14, type='l')
lines(x=DataFile$Дата, y=DataFile$Временной.ряд.15, col='red')
lines(x=DataFile$Дата, y=DataFile$Временной.ряд.6, col='blue')
lines(x=DataFile$Дата, y=DataFile$Временной.ряд.2, col='purple')
lines(x=DataFile$Дата, y=DataFile$Временной.ряд.10, col='green')
lines(x=DataFile$Дата, y=DataFile$Временной.ряд.9, col='orange')
lines(x=DataFile$Дата, y=DataFile$Временной.ряд.8, col='pink')
lines(x=DataFile$Дата, y=DataFile$Временной.ряд.13, col='yellow')
lines(x=DataFile$Дата, y=DataFile$Временной.ряд.4, col='gray')
lines(x=DataFile$Дата, y=DataFile$Временной.ряд.3, col='darkblue')
lines(x=DataFile$Дата, y=DataFile$Временной.ряд.12, col='yellow')
lines(x=DataFile$Дата, y=DataFile$Временной.ряд.16, col='gray')
lines(x=DataFile$Дата, y=DataFile$Временной.ряд.7, col='darkblue')

DataFile$Временной.ряд.14 <- DataFile$Временной.ряд.14/DataFile$Временной.ряд.14[347]
DataFile$Временной.ряд.15 <- DataFile$Временной.ряд.15/DataFile$Временной.ряд.15[347]
DataFile$Временной.ряд.6 <- DataFile$Временной.ряд.6/DataFile$Временной.ряд.6[347]
DataFile$Временной.ряд.2 <- DataFile$Временной.ряд.2/DataFile$Временной.ряд.2[347]
DataFile$Временной.ряд.10 <- DataFile$Временной.ряд.10/DataFile$Временной.ряд.10[347]
DataFile$Временной.ряд.9 <- DataFile$Временной.ряд.9/DataFile$Временной.ряд.9[347]
DataFile$Временной.ряд.8 <- DataFile$Временной.ряд.8/DataFile$Временной.ряд.8[347]
DataFile$Временной.ряд.13 <- DataFile$Временной.ряд.13/DataFile$Временной.ряд.13[347]
DataFile$Временной.ряд.4 <- DataFile$Временной.ряд.4/DataFile$Временной.ряд.4[347]
DataFile$Временной.ряд.3 <- DataFile$Временной.ряд.3/DataFile$Временной.ряд.3[347]
DataFile$Временной.ряд.12 <- DataFile$Временной.ряд.12/DataFile$Временной.ряд.12[347]
DataFile$Временной.ряд.16 <- DataFile$Временной.ряд.16/DataFile$Временной.ряд.16[347]
DataFile$Временной.ряд.7 <- DataFile$Временной.ряд.7/DataFile$Временной.ряд.7[347]

plot(x=DataFile$Дата, y=DataFile$Временной.ряд.14, type='l', ylim=c(0,3))
lines(x=DataFile$Дата, y=DataFile$Временной.ряд.15, col='red')
lines(x=DataFile$Дата, y=DataFile$Временной.ряд.6, col='blue')
lines(x=DataFile$Дата, y=DataFile$Временной.ряд.2, col='purple')
lines(x=DataFile$Дата, y=DataFile$Временной.ряд.10, col='green')
lines(x=DataFile$Дата, y=DataFile$Временной.ряд.9, col='orange')
lines(x=DataFile$Дата, y=DataFile$Временной.ряд.8, col='pink')
lines(x=DataFile$Дата, y=DataFile$Временной.ряд.13, col='yellow')
lines(x=DataFile$Дата, y=DataFile$Временной.ряд.4, col='gray')
lines(x=DataFile$Дата, y=DataFile$Временной.ряд.3, col='darkblue')
lines(x=DataFile$Дата, y=DataFile$Временной.ряд.12, col='skyblue')
lines(x=DataFile$Дата, y=DataFile$Временной.ряд.16, col='brown')
lines(x=DataFile$Дата, y=DataFile$Временной.ряд.7, col='darkgreen')


#24. Рассчитать индекс производства. 
# Индекс структурных сдвигов Пааше
#x - объем, основной пок-ль, а - агрегат
#Пааше : стр.сдвигов = х1*а1:х1*а0
Data <- read.xlsx("~/r_projects/KR/t.xlsx", sheet=24)
Paashe <- Data$Объем.производства.2020*Data$Средняя.себестоимость.единицы.продукции.2020/(Data$Объем.производства.2020*Data$Средняя.себестоимость.единицы.продукции.2019)
IndPaashe <- sum(Paashe)
IndPaashe

#25. Рассчитать индекс производства. 
# Индекс постоянного состава Ласпейреса
#x - объем, основной пок-ль, а - агрегат
#Пааше : стр.сдвигов = х1*а0:х0*а0
Data <- read.xlsx("~/r_projects/KR/t.xlsx", sheet=25)
Laisperosa <- Data$Объем.производства.2020*Data$Средняя.себестоимость.единицы.продукции.2019/(Data$Объем.производства.2019*Data$Средняя.себестоимость.единицы.продукции.2019)
IndLaisperosa <- sum(Laisperosa)
IndLaisperosa
