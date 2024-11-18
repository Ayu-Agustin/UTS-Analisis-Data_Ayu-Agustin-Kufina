#Ayu Agustin Kufina_22106010039
#UTS_Analisis Data
#nomor_2

#read in your data
library(readr)
setwd("C:/Users/ASUS/Documents/Analisis Data")
data <- read_csv("depression_anxiety_data.csv",col_types = "iccc")
Data <- na.omit(data)
Data

#check the packaging
nrow(Data)
ncol(Data)
str(Data)

#look at the top and the bottom of your data
head(Data[,c(8,18)])
tail(Data[,c(8,18)])

#check your "n"s
head(table(Data$epworth_score))
library(dplyr)
filter(Data, epworth_score == "0") %>%
  select(depression_severity)

filter(Data, depression_severity == "Severe") %>%
  select(epworth_score) %>%
  as.data.frame

select(Data, depression_severity) %>% unique %>% nrow
unique(Data$depression_severity)

#validate with at least one external data source
summary(Data$epworth_score)
quantile(Data$epworth_score, seq(0,1,0.1), na.rm=TRUE)

#make a plot
library(ggplot2)
ggplot(Data, aes(x = epworth_score, y = depression_severity)) +
  geom_boxplot() +
  labs(title = "Hubungan Skor Epworth dan Tingkat Depresi",
       x = "Skor Epworth", y = "Tingkat Depresi") +
  theme_minimal()

#try the easy solution first
library(ggpubr)
ggboxplot(Data, x = "depression_severity", y = "epworth_score", add = "jitter")


#follow up
library(nnet)
model<-multinom(depression_severity~epworth_score,data=Data)
summary(model)

#nomor_3
#model as expectations
library(nnet)
model<-multinom(depression_severity~epworth_score,data=Data)
predicted_prob <- predict(model, newdata = Data, type = "probs")
head(predicted_prob)

predicted_category <- predict(model, newdata = Data)
table(predicted_category, Data$depression_severity)

#comparing model expectations to reality
#histogram variabel dependen
library(ggplot2)
ggplot(Data, aes(x = depression_severity)) +
  geom_bar(fill = "blue", alpha = 0.7) +
  labs(
    title = "Distribusi Aktual Tingkat Depresi",
    x = "Tingkat Depresi",
    y = "Frekuensi"
  ) +
  theme_minimal()
#perbandingan dengan histogram distribusi normal
p1 <- ggplot(Data, aes(x = epworth_score)) +
  geom_histogram(binwidth = 1, fill = "blue", alpha = 0.7, color = "black") +
  labs(
    title = "Distribusi Aktual Epworth Score",
    x = "Skor Epworth",
    y = "Frekuensi"
  ) +
  theme_minimal()

p2 <- ggplot(data.frame(normal_data), aes(x = normal_data)) +
  geom_histogram(binwidth = 1, fill = "orange", alpha = 0.7, color = "black") +
  labs(
    title = "Distribusi Normal Simulasi",
    x = "Skor Epworth",
    y = "Frekuensi"
  ) +
  theme_minimal()

library(ggpubr)
ggarrange(p1, p2, ncol = 2, nrow = 1)