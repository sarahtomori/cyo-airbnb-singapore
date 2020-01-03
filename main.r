library("readr", lib.loc="/Library/Frameworks/R.framework/Versions/3.6/Resources/library")
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

