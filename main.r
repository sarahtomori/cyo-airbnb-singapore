#Install required packages
install.packages("readr")
install.packages("caret")
install.packages("magrittr")
install.packages("dplyr")
install.packages("wordcloud")
install.packages("SnowballC")
install.packages("corrplot")
install.packages("wesanderson")
install.packages("maps")
install.packages("mapproj")
install.packages("ggmap")
install.packages("textmining")
install.packages("tm")
install.packages("Hmisc")
install.packages("rpart")
install.packages("rsq")
install.packages("glmnet")
install.packages("neuralnet")
install.packages("stylo")

#Load the libraries

library("readr")
library("caret")
library("magrittr")
library("dplyr")
library("wordcloud")
library("SnowballC")
library("corrplot")
library("wesanderson")
library("maps")
library("mapproj")
library("ggmap")
library("textmining")
library("tm")
library("Hmisc")
library("rpart")
library("rsq")
library("glmnet")
library("neuralnet")
library("stylo")

#Read the file with the Singapore Airbnb data. 

#Singapore data set loaded from:
#http://data.insideairbnb.com/singapore/sg/singapore/2019-11-26/visualisations/listings.csv

dl <- tempfile()
download.file("http://data.insideairbnb.com/singapore/sg/singapore/2019-11-26/visualisations/listings.csv", dl)

data <- read_csv(dl)

# Create test set as 10% of the dataset
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = data$price, times = 1, p = 0.1, list = FALSE)
airbnb_singapore <- data[-test_index,]
temp <- data[test_index,]

# Make sure that host_id in the test set are present in the airbnb_singapore set
test_set <- temp %>%
  semi_join(airbnb_singapore, by = "host_id")
# Add rows removed from test set back into airbnb_singapore set
removed <- anti_join(temp, test_set)
airbnb_singapore <- rbind(airbnb_singapore, removed)
rm(temp, data, removed)

# Split airbnb_singapore data further in to validation and training sets
# Validation set will be 10% of current airbnb_singapore data
set.seed(1, sample.kind="Rounding")
# if using R 3.5 or earlier, use `set.seed(1)` instead
test_index2 <- createDataPartition(y = airbnb_singapore$price, times = 1, p = 0.1, list = FALSE)
train_set <- airbnb_singapore[-test_index2,]
temp2 <- airbnb_singapore[test_index2,]

# Make sure host_id in validation are also in training set
validation <- temp2 %>% 
  semi_join(train_set, by = "host_id")
# Add rows removed from validation back into training set
removed <- anti_join(temp2, validation)
train_set <- rbind(train_set, removed)
rm(temp2, removed)

# Exploring the data

# Check class of data
class(airbnb_singapore)

# Get brief overview of the data
glimpse(airbnb_singapore)

# See a summary of the data
summary(airbnb_singapore)

# Check whether there are any missing values
anyNA(airbnb_singapore)

# Show unique values of prices
unique(airbnb_singapore$price)

# Show number of hosts, listings, regions, neighbourhoods and room types
airbnb_singapore %>% summarise(n_hosts = n_distinct(host_id), n_listings = n_distinct(id), n_regions = n_distinct(neighbourhood_group), n_neighbourhoods = n_distinct(neighbourhood), n_types = n_distinct(
room_type))

# Show average minimum nights required
mean(airbnb_singapore$minimum_nights)

# Show average price per night
mean(airbnb_singapore$price)

# Visualisation of the data 
# Price distributions in the training set
airbnb_singapore %>% count(price) %>%
ggplot(aes(n)) + geom_histogram(fill = "#FF3366", color = "#000000", bins = 30) +
scale_x_log10() + ggtitle("Prices of vs. number of listings") + xlab("Prices") + ylab("Listings")

#Check distributions of prices in the training set
airbnb_singapore %>% ggplot(aes(price)) + geom_histogram(bins = 10, color = "#000000", fill = "#FFCCCC") +
  ggtitle("Distribution of prices across Airbnbs")

#Check the most common prices
most_common_prices <- airbnb_singapore %>% group_by(price) %>% summarise(count = n()) %>% top_n(20, count) %>% arrange(desc(count))
most_common_prices

#Check distributions of availability in the training set
airbnb_singapore %>% ggplot(aes(availability_365)) + geom_histogram(bins = 20, fill = "#009999") +
ggtitle("Availability")

#Check distributions of listings in the various regions in training set
airbnb_singapore %>% ggplot(aes(neighbourhood_group)) + geom_bar(fill = "#3399FF") +
ggtitle("Number of listings in regions")

#Check distributions of listings in the various regions in training set
avg_prices <- airbnb_singapore %>% group_by(neighbourhood_group) %>%
summarise(region_avg = mean(price)) %>%
arrange(-region_avg)
avg_prices
#Display them in a histogram
avg_prices %>% ggplot(aes(reorder(neighbourhood_group, region_avg), region_avg, fill = region_avg)) +
geom_bar(stat = "identity") + coord_flip() +
scale_fill_distiller(palette = "Greens") + labs(y = "Region price mean", x = "Region") +
ggtitle("Average Airbnb prices of regions")

#First, save the number of listings in each neighbourhood.

top_neighbourhoods <- airbnb_singapore %>% group_by(neighbourhood) %>% summarise(count = n()) %>% top_n(20, count) %>% arrange(desc(count))
top_neighbourhoods

#We can also show them in a bar chart.
top_neighbourhoods %>% ggplot(aes(reorder(neighbourhood, count), count, fill = count)) +
geom_bar(stat = "identity") + coord_flip() +
scale_fill_distiller(palette = "Reds") + labs(y = "Number", x = "Neighbourhood") +
ggtitle("Popular neighbourhoods")

#Check distributions of listings in the various room types in training set
airbnb_singapore %>% ggplot(aes(room_type)) + geom_bar(fill = "#9999FF") +
ggtitle("Number of listings in regions")

#Check distributions of review count in the training set
airbnb_singapore %>% ggplot(aes(number_of_reviews)) + geom_histogram(bins = 30, fill = "#009999") +
ggtitle("Reviews")

#Now, we import the text file with the Airbnb names. We can read the file from GitHub
filePath <- "https://github.com/sarahtomori/cyo-airbnb-singapore/blob/master/singapore-airbnb-names.txt"
text <- readLines(filePath)

#Read the data as a corpus
docs <- Corpus(VectorSource(text))

#Inspect the element of the document
inspect(docs)

#Replace special characters with text
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")
docs <- tm_map(docs, toSpace, "!")

# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("blabla1", "blabla2")) 
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
# Text stemming
# docs <- tm_map(docs, stemDocument)

#First, create a matrix that contains the frequency of the words. 
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m), decreasing=TRUE)
d <- data.frame(word = names(v), freq=v)
head(d, 10)

#Create a wordcloud that visualises the most common words used to describe the Airbnbs. 
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
max.words=200, random.order=FALSE, rot.per=0.35, 
colors=brewer.pal(8, "Dark2"))

#Show the 10 most frequently used words.
head(d, 10)

#and plot them
barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word,
col ="#FF9999", main ="Most frequent words",
ylab = "Word frequencies")

#Check whether the variables are correlated.

cordata <- airbnb_singapore[, c(1,3,7,8,10,11,12,14,15,16)]
head(cordata)

cordata.cor = cor(cordata, method = c("spearman"))
cordata.rcorr = rcorr(as.matrix(cordata))
cordata.rcorr

#Next, the following code can be used to extract the p-values from the data, using the following code:
cordata.coeff = cordata.rcorr$r
cordata.coeff

cordata.p = cordata.rcorr$P
cordata.p

#The correlation plot can be visualised using the corrplot() function
corrplot(cordata.cor)

#Data analysis
#First, we define the function that calculates RMSE
RMSE <- function(true_price, predicted_price){
sqrt(mean((true_price - predicted_price)^2, na.rm=T))
}

#Calculate average price of Airbnb prices in Singapore
avg_price <- mean(airbnb_singapore$price)
avg_price

#Check squared loss
mean((avg_price - validation$price)^2)

#Replace missing values with 0
airbnb_singapore[is.na(airbnb_singapore)] <- 0

#Replace missing values in the last_review variable with the date of the first review
airbnb_singapore$last_review <- ifelse(is.na(airbnb_singapore$last_review), as.Date(2013-10-21,  origin = "2013-10-21", tz = "GMT"), airbnb_singapore$last_review)

#Save neighbourhood, neighbourhood_group and room_type variables as factor in both training and validation sets
validation$neighbourhood <- as.factor(validation$neighbourhood)
airbnb_singapore$neighbourhood <- as.factor(airbnb_singapore$neighbourhood)
validation$room_type <- as.factor(validation$room_type)
airbnb_singapore$room_type <- as.factor(airbnb_singapore$room_type)
validation$neighbourhood_group <- as.factor(validation$neighbourhood_group)
airbnb_singapore$neighbourhood_group <- as.factor(airbnb_singapore$neighbourhood_group)

#add all levels of 'neighbourhood' in 'validation' dataset to fit$xlevels[["neighbourhood"]] in the fit object
fit$xlevels[["neighbourhood"]] <- union(fit$xlevels[["neighbourhood"]], levels(validation[["neighbourhood"]]))

#add all levels of 'room_type' in 'validation' dataset to fit$xlevels[["room_type"]] in the fit object
fit$xlevels[["room_type"]] <- union(fit$xlevels[["room_type"]], levels(validation[["room_type"]]))

#add all levels of 'neighbourhood_group' in 'validation' dataset to fit$xlevels[["neighbourhood_group"]] in the fit object
fit$xlevels[["neighbourhood_group"]] <- union(fit$xlevels[["neighbourhood_group"]], levels(validation[["neighbourhood_group"]]))

#Create lm model
fit <- lm(price ~ host_id + longitude + latitude + latitude + number_of_reviews + calculated_host_listings_count + neighbourhood_group + neighbourhood + room_type + calculated_host_listings_count + availability_365, data = airbnb_singapore)
step(fit, direction = "backward", trace = FALSE) #use backward elimination to remove redundant variables

#Let's look at the coefficients
fit$coefficients

#Use the predict function
y_hat <- predict(fit, validation)

#Check squared loss
mean((y_hat - validation$price)^2)
RMSE(validation$price, y_hat)
#and the r-squared
rss_lm <- sum((validation$price - y_hat) ^ 2)  ## residual sum of squares
tss_lm <- sum((validation$price - mean(y_hat)) ^ 2)  ## total sum of squares
rsq_lm <- 1 - rss/tss
rsq_lm

#Define the independent variables
x_var <- model.matrix(airbnb_singapore$price ~ longitude + number_of_reviews + neighbourhood_group + calculated_host_listings_count + room_type + availability_365, data = airbnb_singapore)

#Define the dependent variable
y_var <- airbnb_singapore$price

# Setting the range of lambda values
lambda_seq <- 10^seq(2, -2, by = -.1)

train_data <- data.frame(x_var, y_var)

model_glmnet <- train(y_var ~ ., data = train_data,
method = "glmnet",
metric = "RMSE",
na.action = na.replace,
lambda  = lambda_seq
)

#Check the model
summary(model_glmnet)
model_glmnet

#Choose the optimal lambda value
# Using cross validation glmnet
ridge_cv <- cv.glmnet(x_var, y_var, lambda = lambda_seq)
# Best lambda value
best_lambda <- ridge_cv$lambda.min
best_lambda

#Find the best ridge model
best_ridge <- glmnet(x_var, y_var, alpha = 0, lambda =  best_lambda)

#Get the coefficients
coef(best_ridge)

#Define y and x variables from the validation set
y_val <- validation$price
x_val <- model.matrix(validation$price ~ longitude + number_of_reviews + calculated_host_listings_count + neighbourhood_group + room_type + availability_365, data = validation)

#Save as a data frame
val_data <- data.frame(x_val, y_val)

#Use predict function and fit to validation data
glmnet_pred_val <- predict(best_ridge, s = best_lambda, newx = x_val)

#Use lm model created earlier and fit to validation data
y_test <- predict(fit, test_set)

#Calculate the MSE
mean((y_hat - test_set$price)^2)
#and the RMSE
RMSE(test_set$price, y_test)
#and the r-squared
rss <- sum((y_test - test_set$price) ^ 2)  ## residual sum of squares
tss <- sum((test_set$price - mean(y_test)) ^ 2)  ## total sum of squares
rsq <- 1 - rss/tss
rsq
