library(tidyverse)
library(RPostgreSQL)
library(leaflet)


con <- dbConnect(drv ="PostgreSQL", 
                 user="expert", 
                 password="5K9hZS!LY!B6dm6",
                 host="data-hunter.cuwo7zs53t3u.us-east-1.rds.amazonaws.com", 
                 port=5432, 
                 dbname="postgres")

#list all the tables 
dbListTables(con)  

#query the database and store the data in datafame
results <- dbGetQuery(con, "SELECT 
                                id 
                               , host_id
                               , host_name
                               , street
                               , city
                               , latitude
                               , longitude
                               , room_type
                               , accommodates
                               , bathrooms
                               , bedrooms
                               , number_of_reviews
                               , review_scores_rating
                               , to_number(price, 'L9G999g999.99') as price
                            from listing
                            where review_scores_rating <> 'NULL'      
                      ;")

#disconnect from database
dbDisconnect(con)

write.csv(results, file = "listing_reviews.csv", sep = ",")


rm(results)

listing <- read.csv("listing_reviews.csv")

View(listing)

glimpse(listing)

summary(listing)


listing <- listing[!is.na(listing$host_name),]
listing <- listing[!is.na(listing$city),]
listing <- listing[!is.na(listing$bathrooms),]
listing <- listing[!is.na(listing$bedrooms),]

summary(listing)


listing %>% 
  count(cities = n_distinct(city))


prep_listing <- listing %>% 
  select(street, latitude, longitude, room_type, accommodates, bedrooms, number_of_reviews, review_scores_rating, price)

prep_listing

any(is.na(prep_listing))

summary(prep_listing)

ggplot(data = prep_listing) +
  geom_bar(mapping = aes(x = room_type))

ggplot(data = prep_listing) +
  geom_histogram(mapping = aes(x = log(accommodates)), binwidth = 0.5)

ggplot(data = prep_listing) +
  geom_histogram(mapping = aes(x = log(bedrooms)), binwidth = 0.5)

ggplot(data = prep_listing) + 
  geom_bar(mapping = aes(x = log(accommodates), fill = room_type), position = "dodge")

ggplot(data = prep_listing, mapping = aes(x = log2(price), colour = room_type)) +
  geom_freqpoly(binwidth = 0.1)


ggplot(data = prep_listing, mapping = aes(x = room_type, y = log2(price), colour = room_type)) +
  geom_boxplot()


ggplot(data = prep_listing, mapping = aes(x = log(accommodates), y = number_of_reviews )) + 
  geom_point()

ggplot(data = prep_listing, mapping = aes(x = number_of_reviews, y = log(price) )) + 
  geom_point()




prep_listing_featEng <- prep_listing %>% 
  mutate(logreviews = log(1 + number_of_reviews),
         bedrooms_per_accommodates = bedrooms / accommodates)
  
prep_listing_featEng

library(fastDummies)

resultado_dummie <- dummy_cols(prep_listing_featEng)

view(resultado_dummie)

tt <- lm(formula = price ~., data = resultado_dummie)
summary(tt)


################ DECISION TREE #######################################

library(caTools)
library(rpart)
set.seed(1)
divisao <- sample.split(prep_listing_featEng$price, SplitRatio = 0.70)
listing_treinamento <- subset(prep_listing_featEng, divisao == TRUE)
listing_teste <- subset(prep_listing_featEng, divisao == FALSE)


regressao <- rpart(formula = price ~., data = listing_treinamento)
summary(regressao)

previsoes_treinamento <- predict(regressao, newdata = listing_treinamento[-9])

library(miscTools)

cc_treinamento <- rSquared(listing_treinamento[['price']], resid = listing_treinamento[['price']] - previsoes_treinamento)


previsoes_teste <- predict(regressao, newdata = listing_teste[-9])
mae_tree <- mean(abs(listing_teste[['price']] - previsoes_teste))

mae_tree

cc_teste <- rSquared(listing_teste[['price']], resid = listing_teste[['price']] - previsoes_teste)

################ POLINOMIAL REGRESSION #######################################

prep_listing2 <- prep_listing_featEng %>% 
  select(room_type, accommodates, bedrooms, number_of_reviews, review_scores_rating, price, logreviews, bedrooms_per_accommodates)

division <- sample.split(prep_listing2$price, SplitRatio = 0.70)
listing_treinamento2 <- subset(prep_listing2, divisao == TRUE)
listing_teste2 <- subset(prep_listing2, divisao == FALSE)

divisao <- sample.split(prep_listing_featEng$price, SplitRatio = 0.70)
listing_treinamento <- subset(prep_listing_featEng, divisao == TRUE)
listing_teste <- subset(prep_listing_featEng, divisao == FALSE)



glimpse(listing_treinamento)

regressor <- lm(formula = price ~., data = listing_treinamento)
summary(regressor)

glimpse(listing_treinamento)
glimpse(listing_teste)

previsoes_training <- predict(regressor, newdata = listing_treinamento[-9])
previsoes_training

mse <- mean(abs(listing_teste[['price']] - previsoes_training))
mse


cc <- rSquared(listing_teste[['price']] , resid = listing_teste[['price']]  - previsoes_training)

################ SVM #######################################

divisao <- sample.split(prep_listing_featEng$price, SplitRatio = 0.70)
listing_treinamento <- subset(prep_listing_featEng, divisao == TRUE)
listing_teste <- subset(prep_listing_featEng, divisao == FALSE)

glimpse(listing_treinamento2)

library(e1071)
regressor_svm <- svm(formula = price ~., data = listing_treinamento, type = 'eps-regression', kernel = 'radial')
summary(regressor_svm)

previsao_treinamento <- predict(regressor_svm, newdata = listing_treinamento[-9])

cc_training <- rSquared(listing_treinamento[['price']], resid = listing_treinamento[['price']] - previsao_treinamento)
cc_training

previsao_teste <- predict(regressor_svm, newdata = listing_teste[-9])
mae_svm <- mean(abs(listing_teste[['price']] - previsao_teste))

mae_svm

cc_test <- rSquared(listing_teste[['price']], resid = listing_teste[['price']] - previsao_teste)
cc_test


########################## rede neural #######################################
library(h2o)
h2o.init(nthreads = -1)  

reg <- h2o.deeplearning(y = 'price', training_frame = as.h2o(listing_treinamento), activation = 'Rectifier',
                        hidden = c(100, 100), epochs = 100)

previsao_train_h2o <- predict(reg, newdata = as.h2o(listing_treinamento[-9]))
previsao_train_h2o <- as.vector(previsao_train_h2o)

cc_h2o <- rSquared(listing_treinamento[['price']], resid = listing_treinamento[['price']] - previsao_train_h2o)

previsoes_test_h2o <- predict(reg, newdata = as.h2o(listing_teste[-9]))
previsoes_test_h2o <- as.vector(previsoes_test_h2o)

m <- mean(abs(listing_teste2[['price']] - previsoes_test_h2o))
m



cc_h2o_test <- rSquared(listing_teste[['price']], resid = listing_teste[['price']] - previsoes_test_h2o)
cc_h2o_test

############################ randon forest #####################################
divisao <- sample.split(prep_listing_featEng$price, SplitRatio = 0.70)
listing_treinamento <- subset(prep_listing_featEng, divisao == TRUE)
listing_teste <- subset(prep_listing_featEng, divisao == FALSE)

glimpse(listing_treinamento)
glimpse(listing_teste)
summary(listing_tre)



library(randomForest)
listing_treinamento <- listing_treinamento[-1]
listing_teste <- listing_teste[-1]



regressor_rf <- randomForest(x = listing_treinamento[-8], y = listing_treinamento$price, ntree = 10)

previsao_rf <- predict(regressor_rf, newdata = listing_treinamento)
cc_rf <- rSquared(listing_treinamento[['price']], resid = listing_treinamento[['price']] - previsao_rf)
cc_rf

previsao_rf_test <- predict(regressor_rf, newdata = listing_treinamento)
cc_rf_test <- rSquared(listing_teste[['price']], resid = listing_teste[['price']] - previsao_rf_test)
cc_rf_test

RF_mae <- mean(abs(listing_treinamento[['price']] - previsao_rf_test))

RF_mae

##########################rede neural multi camada#########################
divisao <- sample.split(prep_listing_featEng$price, SplitRatio = 0.70)
listing_treinamento <- subset(prep_listing_featEng, divisao == TRUE)
listing_teste <- subset(prep_listing_featEng, divisao == FALSE)


glimpse(listing_treinamento)
summary(listing_treinamento)
glimpse(listing_teste)
summary(listing_teste)

classificador <- h2o.deeplearning(y = 'price',
                            training_frame = as.h2o(listing_treinamento),
                            activation = 'Rectifier',
                            hidden = c(100),
                            epochs = 1000)

previs <- h2o.predict(classificador, newdata = as.h2o(listing_teste[-8]))
previs <- as.vector(previs)


perf <- h2o.performance(classificador, newdata = as.h2o(listing_teste))
perf



############## leaflet ###########

library(leaflet)


leaflet(listing) %>% 
  addTiles() %>% 
  addCircleMarkers(radius = .03)



price_review_scores_rating <- lm(price ~ review_scores_rating, data = listing)
summary(price_review_scores_rating)

price_accommodates <- lm(price ~ accommodates, data = listing)
summary(price_accommodates)

price_bathrooms <- lm(price ~ bathrooms, data = listing)
summary(price_bathrooms)

price_number_of_reviews <- lm(price ~ number_of_reviews, data = listing)
summary(price_number_of_reviews)

prep_listing2





########################## kmeans ##############################


prep_listing2$room_type <- NULL

prep_listing2

df_prep_listing2 <- prep_listing2[!is.na(prep_listing2$bedrooms),]
df_prep_listing2 <- prep_listing2[!is.na(prep_listing2$bedrooms_per_accommodates),]

prep_listing_scale <- scale(df_prep_listing2[-1])



View(prep_listing_scale)

summary(prep_listing_scale)

resultado <- kmeans(prep_listing_scale, 4)

attributes(resultado)

resultado$cluster
