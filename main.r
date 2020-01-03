library("readr", lib.loc="/Library/Frameworks/R.framework/Versions/3.6/Resources/library")
library("caret", lib.loc="/Library/Frameworks/R.framework/Versions/3.6/Resources/library")
library("magrittr", lib.loc="/Library/Frameworks/R.framework/Versions/3.6/Resources/library")
library("dplyr", lib.loc="/Library/Frameworks/R.framework/Versions/3.6/Resources/library")
data <- read_csv("./data.csv")

# Create validation set as 10% of the dataset
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = data$price, times = 1, p = 0.1, list = FALSE)
airbnb_singapore <- data[-test_index,]
temp <- data[test_index,]

# Make sure that id and host_id in the validation set are present in the airbnb_singapore set
validation <- temp %>% 
  semi_join(airbnb_singapore, by = "id") %>%
  semi_join(airbnb_singapore, by = "host_id")
# Add rows removed from validation set back into airbnb_singapore set
removed <- anti_join(temp, validation)
airbnb_singapore <- rbind(airbnb_singapore, removed)
rm(temp, data, removed)

# Split airbnb_singapore data further in to test and training sets
# Test set will be 10% of current airbnb_singapore data
set.seed(1, sample.kind="Rounding")
# if using R 3.5 or earlier, use `set.seed(1)` instead
test_index2 <- createDataPartition(y = airbnb_singapore$price, times = 1, p = 0.1, list = FALSE)
train_set <- airbnb_singapore[-test_index2,]
temp2 <- airbnb_singapore[test_index2,]

# Make sure userId and movieId in test set are also in training set
test_set <- temp2 %>% 
  semi_join(train_set, by = "id") %>%
  semi_join(train_set, by = "host_id")
# Add rows removed from test set back into training set
removed <- anti_join(temp2, test_set)
train_set <- rbind(train_set, removed)
rm(test_index2, temp2, removed)

# Exploring the data

# Check class of data
class(airbnb_singapore)

# Get brief overview of the data
glimpse(airbnb_singapore)

# See a summary of the data
summary(airbnb_singapore)

# Check whether there are any missing values
anyNA(airbnb_singapore)

# Show unique values of movie ratings
unique(airbnb_singapore$price)

# Show number of users, movies and genres
airbnb_singapore %>% summarize(n_hosts = n_distinct(host_id), n_listings = n_distinct(id), n_regions = n_distinct(neighbourhood_group), n_neighbourhoods = n_distinct(neighbourhood), n_types = n_distinct(
room_type))

# Show average minimum nights required
mean(airbnb_singapore$minimum_nights)

# Show average price per night
mean(airbnb_singapore$price)

# Visualisation of the data 
