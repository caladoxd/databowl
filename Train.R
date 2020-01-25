library(ggplot2)
library(magrittr)
library(dplyr)
library(dbplyr)
library(RSQLite)
library(tidyr)
library(purrr)
library(scales)
library(corrplot)
library(xgboost)
gc()
rm(list = ls())
#load Sqlite DB
my_db  <- src_sqlite("BIGDATA.sqlite3", create = FALSE)
table <- tbl(my_db, "trainData")
head(table) %>%
  #select(event_data,event_code) %>%
  head(10)

bigEvData <- table %>%
  head(10) %>%
  select(event_data) %>%
  tbl_df() 
valores <- strsplit(bigEvData$event_data, ",")[[1]]
bigEvData %>% tbl_df() 


x_train <- table %>% filter(installation_id=="0006a69f") %>% head(1000) %>% tbl_df()
exemplo <- x_train %>% select(event_code,event_data)

codes <- table %>% select(event_code,type) %>% distinct(event_code) %>% tbl_df()

specs <- read.csv("specs.csv") %>% tbl_df()
x_train$type <- factor(x_train$type)

table$Fare <- rescale(table$Fare)
table$Age <- rescale(table$Age)
table$SibSp <- rescale(table$SibSp)
table$Ticket <- as.integer(table$Ticket) %>% rescale()


table%>%
  filter(Fare<130, Sex == "male") %>%
  ggplot(aes(x=Age, y=log(Fare), col=Survived))+
  geom_point(position = position_jitter())

numericos <- table[c(1:3,6:8,10)]
numericos$Age <- replace_na(numericos$Age,0)
corrplot(cor(numericos),method="circle")


y_train <- table[2]
x_test <- read.csv("test.csv") %>% tbl_df() %>% select(2:6,8:10)

require(caret)

x <- cbind(x_train,y_train)
y <- y_train$Survived
# Fitting model



bst <- xgboost(data = data.matrix(x_train), label=y, max.depth = 2, eta = 1, nthread = 4, nrounds = 4, objective = "binary:logistic", verbose = 1)


predicted <- predict(bst, data.matrix(x_test)) %>% round(0)
testTable <- read.csv("test.csv") %>% rename(name=Name) %>% lapply(gsub, pattern='"', replacement='') %>% tbl_df()
gabarito <- read.csv("titanic.csv") %>% select(2:3) %>% lapply(gsub, pattern='"', replacement='') %>% tbl_df()
gabarito <- merge(testTable[c("PassengerId","name")],gabarito,by="name") %>% 
  arrange(PassengerId)# %>%
#Show score of model
gabarito$PassengerId <- as.integer(gabarito$PassengerId)
gabarito <-  arrange(gabarito,PassengerId) %>% unique()
#predicted <- read.csv("titanic_pred.csv") %>% tbl_df() %>% select(2)
score <- sum(predicted == gabarito$survived)/418
#save results on csv file
saida <- select(testTable,1)
saida$Survived <- predicted
write.csv(x = saida,file = "resultado.csv",row.names = FALSE)