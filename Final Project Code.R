library(tidyverse)
library(lubridate)
library(caret)
library(car)
library(leaps)
library(mice)
library(randomForest)
library(ranger)
library(rpart)
library(gbm)

data = read.csv('/Users/kristinaliapchin/Downloads/analysisData.csv')

str(data)
head(data)
summary(data)

##########
## Cleaning up Analysis Data
##########
data$price = as.numeric(gsub('[$,]', '', data$price))
data$weekly_price = as.numeric(gsub('[$,]', '', data$weekly_price))
data$monthly_price = as.numeric(gsub('[$,]', '', data$monthly_price))
data$security_deposit = as.numeric(gsub('[$,]', '', data$security_deposit))
data$cleaning_fee = as.numeric(gsub('[$,]', '', data$cleaning_fee))
data$extra_people = as.numeric(gsub('[$,]', '', data$extra_people))

data$bedrooms = as.numeric(data$bedrooms)
data$bathrooms = as.numeric(data$bathrooms)

char_cols = c("name",
              "summary",
              "description",
              "amenities"
)
data[char_cols] = lapply(data[char_cols],as.character)

## Removing unnecessary cols
cols_to_delete = c("thumbnail_url",
                   "listing_url",
                   "medium_url",
                   "picture_url",
                   "xl_picture_url",
                   "square_feet",
                   "last_scraped",
                   "host_url",
                   "host_response_rate",
                   "host_thumbnail_url",
                   "host_picture_url",
                   "has_availability",
                   "street",
                   "city",
                   "country",
                   "state",
                   "smart_location",
                   "country_code",
                   "experiences_offered",
                   "calendar_last_scraped",
                   "license",
                   "jurisdiction_names",
                   "requires_license",
                   "require_guest_profile_picture",
                   "require_guest_phone_verification",
                   "is_business_travel_ready"
)

data = data %>% 
  select(-cols_to_delete)

###########
## Furhter Exploring the data
##########
## Examining correlations
num_vars = which(sapply(data, is.numeric))
data_num_vars = data[ , num_vars]

cor_data_sort = as.matrix(sort(cor(data_num_vars)[,"price"])); cor_data_sort

## Examining distribution of price
ggplot(data,aes(x=price))+
  geom_histogram(binwidth = 30)

## Looking for outliers
ggplot(data,aes(x='',y=price))+
  geom_boxplot()

## Removing outliers
sum(data$price >= 1500) / nrow(data) *100

data_clean = data %>% 
  filter(data$price <= 1500)

# Removing all rows with price == 0, as this data is flawed 
data_clean = data_clean[!data_clean$price==0,]
sum(data_clean$price == 0)

## Examining additional variables
ggplot(data_clean, aes(x=as.factor(room_type), y=price))+
  geom_boxplot()
ggplot(data_clean, aes(x=as.factor(accommodates), y=price))+
  geom_boxplot()
ggplot(data_clean, aes(x=property_type,y=price)) +
  geom_boxplot()
ggplot(data_clean, aes(x=as.factor(bedrooms),y=price)) +
  geom_boxplot()
ggplot(data_clean, aes(x=as.factor(bathrooms),y=price)) +
  geom_boxplot()
ggplot(data_clean, aes(x=weekly_price,y=price)) +
  geom_point()
ggplot(data_clean, aes(x=monthly_price,y=price)) +
  geom_point()
ggplot(data_clean, aes(x=security_deposit,y=price)) +
  geom_point()
ggplot(data_clean, aes(x=reviews_per_month,y=price)) +
  geom_point()
ggplot(data_clean, aes(x=host_total_listings_count,y=price)) +
  geom_point()


## Checking for NA's
na_variables = colSums(is.na(data_clean))
as.data.frame(na_variables)

######
## Cleaning both data sets at once
######
an_data = read.csv('/Users/kristinaliapchin/Downloads/analysisData.csv')
an_data = an_data[!an_data$price==0,]

sc_data = read.csv('/Users/kristinaliapchin/Downloads/scoringData.csv')
sc_data$price = NA

full_data = rbind(an_data, sc_data)

full_data = full_data %>% 
  select(-cols_to_delete)

full_data$price = as.numeric(gsub('[$,]', '', full_data$price))
full_data$weekly_price = as.numeric(gsub('[$,]', '', full_data$weekly_price))
full_data$monthly_price = as.numeric(gsub('[$,]', '', full_data$monthly_price))
full_data$security_deposit = as.numeric(gsub('[$,]', '', full_data$security_deposit))
full_data$cleaning_fee = as.numeric(gsub('[$,]', '', full_data$cleaning_fee))
full_data$extra_people = as.numeric(gsub('[$,]', '', full_data$extra_people))

full_data$bedrooms = as.numeric(full_data$bedrooms)
full_data$bathrooms = as.numeric(full_data$bathrooms)

char_cols = c("name",
              "summary",
              "description",
              "amenities",
              "host_about"
)
full_data[char_cols] = lapply(full_data[char_cols],as.character)

full_data = full_data %>% 
  filter(full_data$price <= 1500 | is.na(full_data$price))

######
## Feature Engineering
######
## Length of summary
full_data$noSummary = as.numeric(!full_data$summary=="")
summary(full_data$noSummary)
full_data$noSummary= factor(full_data$noSummary, labels=c('no summary','with summary'))

full_data$lengthSummary = nchar(full_data$summary)

## Length of description
full_data$noDescription = as.numeric(!full_data$description=="")
summary(full_data$noDescription)
full_data$noDescription = factor(full_data$noDescription, labels=c('no description','with description'))
full_data$lengthDescription = nchar(full_data$description)


## Lengths of host about
full_data$noHostAbout = as.numeric(!full_data$host_about=="")
summary(full_data$noHostAbout)
full_data$noHostAbout = factor(full_data$noHostAbout, labels=c('no host about','with host about'))

full_data$lengthHostAbout = nchar(full_data$host_about)

## Number of amenities
am_split = strsplit(full_data$amenities, ",")
num_am = c()
for (i in 1:length(am_split)){
  num_am[i] = length(am_split[[i]])
}
full_data$num_amenities = num_am

## Neighbourhoods
## Fixing different levels in scoring and analysis data
an_nbh_levels = levels(an_data$neighbourhood_cleansed)
sc_nbh_levels = levels(sc_data$neighbourhood_cleansed)
setdiff(sc_nbh_levels,an_nbh_levels)

## Manually mapping missing levels to closest neighbourhoods
full_data$neighbourhood_cleansed[full_data$neighbourhood_cleansed =="Chelsea, Staten Island"] = "Bull's Head"
full_data$neighbourhood_cleansed[full_data$neighbourhood_cleansed =="Huguenot"] = "Woodrow"
full_data$neighbourhood_cleansed[full_data$neighbourhood_cleansed =="Rossville"] = "Woodrow"

total_listings_per_nbh = full_data %>% 
  group_by(neighbourhood_cleansed) %>% 
  count(neighbourhood_cleansed); total_listings_per_nbh

avg_nbh_price = aggregate(price~neighbourhood_cleansed,full_data,mean); avg_nbh_price
nbh_groups = data.frame(neighbourhood_cleansed = avg_nbh_price$neighbourhood_cleansed, avg_price = avg_nbh_price$price, total_listings_per_nbh = total_listings_per_nbh$n)
summary(nbh_groups)

ggplot(nbh_groups, aes(y = avg_price)) +
  geom_boxplot()

nbh_groups_wo_outliers = nbh_groups %>% filter(avg_price < 300)

quantile(nbh_groups_wo_outliers$avg_price)

nbh_group_1 = nbh_groups %>% 
  filter(avg_price <= 78)

nbh_group_2 = nbh_groups %>% 
  filter(avg_price > 78 & avg_price <= 94)

nbh_group_3 = nbh_groups %>%
  filter(avg_price > 94 & avg_price <= 141)

nbh_group_4 = nbh_groups %>%
  filter(avg_price > 141 & avg_price <= 288)

nbh_group_5 = nbh_groups %>%
  filter(avg_price > 288)

full_data$nbh_group = ifelse(full_data$neighbourhood_cleansed %in% nbh_group_1$neighbourhood_cleansed, 1,
                             ifelse(full_data$neighbourhood_cleansed %in% nbh_group_2$neighbourhood_cleansed, 2,
                                    ifelse(full_data$neighbourhood_cleansed %in% nbh_group_3$neighbourhood_cleansed, 3,
                                           ifelse(full_data$neighbourhood_cleansed %in% nbh_group_4$neighbourhood_cleansed, 4,
                                                  ifelse(full_data$neighbourhood_cleansed %in% nbh_group_5$neighbourhood_cleansed, 5, NA)))))

## Clipping property types of less than 100 to "Other"
table(full_data$property_type)
lvl_pt = levels(full_data$property_type)
table_pt = table(full_data$property_type)
for (i in lvl_pt) {
  if (table_pt[i] < 100){
    full_data$property_type[full_data$property_type == i] = "Other"
  }
}

full_data$property_type = droplevels(full_data$property_type)


## Presence of specific amenities
full_data$TV = as.numeric(str_detect(full_data$amenities, fixed("TV", ignore_case = T)))
full_data$TV = factor(full_data$TV,labels=c("no TV","with TV"))

full_data$cableTV = as.numeric(str_detect(full_data$amenities, fixed("cable TV", ignore_case = T)))
full_data$cableTV = factor(full_data$cableTV, labels=c("no cable","with cable"))

full_data$wifi = as.numeric(str_detect(full_data$amenities, fixed("wifi", ignore_case = T)))
full_data$wifi = factor(full_data$wifi, labels=c("no wifi","with wifi"))

full_data$gym = as.numeric(str_detect(full_data$amenities, fixed("gym", ignore_case = T)))
full_data$gym = factor(full_data$gym, labels=c("no gym","with gym"))

full_data$elevator = as.numeric(str_detect(full_data$amenities, fixed("elevator", ignore_case = T)))
full_data$elevator = factor(full_data$elevator, labels=c("no elevator","with elevator"))

## How long has the host been on the platform?
full_data$host_since = as.Date(full_data$host_since)
today = as.Date("2019-04-16")
full_data$host_time = as.integer(today - full_data$host_since)

## How long has it been since the first review?
full_data$first_review = as.Date(full_data$first_review)
full_data$time_since_first_review = as.integer(today - full_data$first_review)

## How long has it been since the last review?
full_data$last_review = as.Date(full_data$last_review)
full_data$time_since_last_review = as.integer(today - full_data$last_review)

## Selecting predictors for model
mod_vars = c("id",
             "price",
             "accommodates",
             "latitude",
             "room_type",
             "bathrooms",
             "bedrooms",
             "availability_30",
             "availability_365",
             "elevator",
             "longitude",
             "time_since_last_review",
             "time_since_first_review",
             "review_scores_rating",
             "cableTV",
             "TV",
             "gym",
             "review_scores_value",
             "minimum_nights",
             "reviews_per_month",
             "calculated_host_listings_count",
             "guests_included",
             "review_scores_cleanliness",
             "review_scores_checkin",
             "property_type",
             "host_time",
             "num_amenities",
             "number_of_reviews",
             "extra_people",
             "weekly_price",
             "security_deposit",
             "cleaning_fee",
             "neighbourhood_group_cleansed",
             "nbh_group",
             "lengthSummary",
             "lengthDescription",
             "noSummary",
             "noDescription",
             "noHostAbout",
             "lengthHostAbout")


full_data_clean = full_data %>% 
  select(mod_vars)

###############
## Missing Values
###############
# I chose to leave this part out, as I did not end up using the data with imputed missing values for my final model.


##########
## Splitting data into train and test datasets
##########
## Converting to dummy variables
dmy = dummyVars(" ~ .", data = full_data_clean, drop2nd = T) 
data_dmy = data.frame(predict(dmy, newdata = full_data_clean)) 

## Splitting data back into analysis and scoring
analysis_data = data_dmy[!is.na(data_dmy$price),] # analysis data
scoringData = data_dmy[is.na(data_dmy$price),] # scoring data

analysis_data = analysis_data %>% 
  select(-"id")

scoringData = scoringData %>% 
  select(-"price")


## Splitting data into training and testing sets
set.seed(100)
split = createDataPartition(y = analysis_data$price,
                            p = 0.75,
                            list = F,
                            groups = 1000)

train = analysis_data[split, ]
test = analysis_data[-split, ]


##########
#### Feature Selection
##########
# Since this was an ongoing process, which many iterations using imputed data, I will include a separate script to illustarte this.

##########
#### Model Training
##########
## Boosting
set.seed(100)
boost= gbm(price~.,data=train,distribution="gaussian",
           n.trees = 1000,interaction.depth = 5,shrinkage = 0.1)

predBoosttrain= predict(boost,n.trees = 1000, train)
rmse_boost_train = sqrt(mean((predBoosttrain-train$price)^2)); rmse_boost_train

predBoosttest= predict(boost,n.trees = 1000, newdata = test)
rmse_boost = sqrt(mean((predBoosttest-test$price)^2)); rmse_boost

##########
## Predictions
##########
pred = predict(boost,n.trees = 1000, scoringData)

submissionFile = data.frame(id = scoringData$id, price = pred)

write.csv(submissionFile, '/Users/kristinaliapchin/Downloads/sample_submissionn_boost_3.csv',row.names = F)



