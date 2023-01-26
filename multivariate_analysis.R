## Loading the packages
library(readr)
library(dplyr)
library(partykit)
library(rpart)
library(rpart.plot)
library(caret)
library(MASS)
library(parsnip)
library(tidymodels)
library(ranger)
library(e1071) 
library(ipred)  
library(pROC)
library(verification)

## Loading the data
filtered_dataset <- read_csv("filtered_dataset.csv")

df_genre <- select(filtered_dataset, -no_of_views)
df_genre$main_tag <- as.factor(df_genre$main_tag)

### Formatting

genre <- 
  df_genre %>% 
  filter(!is.na(main_tag)) 

rnd <- rnorm(nrow(genre))
song_tree <- genre[order(rnd),] # randomize the order, delete some rap songs
song_tree <- song_tree[order(song_tree$main_tag, rnd),]
song_tree['rnd'] = rnd
song_tree2 <- subset(song_tree, (main_tag == 'Pop' & rnd > 1.05) | 
                       main_tag == 'Country' | 
                       (main_tag == 'R&B' & rnd > -0.3) | 
                       main_tag == 'Rock' | 
                       (main_tag == 'Rap' & rnd > 1.9))

song_tree <-  subset(song_tree2, select = -c(rnd, didnt, bout, got) )
train <- song_tree[1:2712, 1:(ncol(song_tree))]
test <- song_tree[2713:3390, 1:(ncol(song_tree))]


### Decision tree

tree <- rpart(main_tag~., data=train, method="class")


#### Diagram
png(file="rpart_plot.png",
    width=600, height=400)
rpart.plot(tree, box.palette = "grey")
dev.off()

summary(tree)
tree_predict <- predict(tree, df_genre, type='class')
df_genre['predicted_values'] = tree_predict
cm <- confusionMatrix(data=df_genre$predicted_values, reference = df_genre$main_tag)


### Boosting
```{r}
library(gbm)
genius_boost <- gbm(main_tag ~ ., data = train, n.trees = 1000)
summary(genius_boost)
genius_pred <- predict(genius_boost, n.trees = genius_boost$n.trees, test)


rap <- 
  filtered_dataset %>% 
  filter(!is.na(no_of_views), main_tag == 'Rap')

pop <- 
  filtered_dataset %>% 
  filter(!is.na(no_of_views), main_tag == 'Pop')

rock <- 
  filtered_dataset %>% 
  filter(!is.na(no_of_views), main_tag == 'Rock')

rnb <- 
  filtered_dataset %>% 
  filter(!is.na(no_of_views), main_tag == 'R&B')

rap <- Filter(function(x)(length(unique(x))>1), rap)
pop <- Filter(function(x)(length(unique(x))>1), pop)
rnb <- Filter(function(x)(length(unique(x))>1), rnb)
rock <- Filter(function(x)(length(unique(x))>1), rock)


### Continuous variable prediction
rap_train <- rap[1:11220, 1:ncol(rap)]
rap_test  <- rap[11221:14026, 1:ncol(rap)]
pop_train <- rap[1:1617, 1:ncol(pop)]
pop_test  <- rap[1618:2022, 1:ncol(pop)]
rnb_train <- rap[1:500, 1:ncol(rnb)]
rnb_test  <- rap[501:626, 1:ncol(rnb)]
rock_train <- rap[1:197, 1:ncol(rnb)]
rock_test  <- rap[198:247, 1:ncol(rnb)]

rf_defaults <- rand_forest(mode = "regression") # specify random forest model

# ----- Run the parsnip model -----
# 1. Rap

rand_forest(mode = "regression", mtry = 3, trees = 1000) %>%
  set_engine("ranger") %>%
  fit(
    log(no_of_views) ~ . ,
    data = rap_train
  )


# 2. Pop

rand_forest(mode = "regression", mtry = 3, trees = 1000) %>%
  set_engine("ranger") %>%
  fit(
    log(no_of_views) ~ . ,
    data = pop_train
  )


# 3. RnB

rand_forest(mode = "regression", mtry = 3, trees = 1000) %>%
  set_engine("ranger") %>%
  fit(
    log(no_of_views) ~ . ,
    data = rnb_train
  )


# 4. Rock

rand_forest(mode = "regression", mtry = 3, trees = 1000) %>%
  set_engine("ranger") %>%
  fit(
    log(no_of_views) ~ . ,
    data = rock_train
  )
