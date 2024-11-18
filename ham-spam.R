# Required libraries
# install.packages("tm")
# install.packages("wordcloud")
# install.packages("RColorBrewer")
# install.packages("e1071")
# install.packages("caret")
# install.packages("tidytext")
# install.packages("dplyr")
# install.packages("syuzhet")
# install.packages("ggplot2")
# install.packages("SnowballC")
# install.packages("ROSE")
# install.packages("pROC")
# install.packages("DMwR2")
# install.packages("themis")
library(tm)
library(wordcloud)
library(RColorBrewer)
library(e1071)
library(caret)
library(tidytext)
library(dplyr)
library(syuzhet)
library(ggplot2)
library(SnowballC)
library(ROSE)
library(pROC)
library(DMwR2)
library(themis)
library(quantmod)


# Read the data with correct column names
sms_data <- read.csv("D:\\datasets\\SMS DATA.csv", stringsAsFactors = FALSE)
names(sms_data) <- c("Type", "Text")

# Convert labels to lowercase for consistency
sms_data$Type <- tolower(sms_data$Type)

# Function to remove any non-ASCII characters from messages
clean_non_ascii <- function(text) {
  iconv(text, "UTF-8", "ASCII", sub = "")
}

# Apply character cleaning to all messages
sms_data$Text <- sapply(sms_data$Text, clean_non_ascii)

# Enhanced text preprocessing function
preprocess_text <- function(text) {
  # Convert to lowercase
  text <- tolower(text)
  # Remove punctuation
  text <- gsub("[[:punct:]]", "", text)
  # Remove numbers
  text <- gsub("\\d+", "", text)
  # Remove extra whitespace
  text <- gsub("\\s+", " ", text)
  text <- trimws(text)
  return(text)
}

# Apply preprocessing
sms_data$CleanText <- preprocess_text(sms_data$Text)

# Stemming Function
stem_text <- function(text) {
  text <- wordStem(text, language = "en")
  return(text)
}

# Apply Stemming
sms_data$CleanText <- sapply(sms_data$CleanText, stem_text)

# Remove empty documents
sms_data <- sms_data[sms_data$CleanText != "", ]

# Create separate corpora for ham and spam
create_word_freq <- function(text) {
  corpus <- Corpus(VectorSource(text))
  corpus <- tm_map(corpus, removeWords, stopwords("english"))
  dtm <- TermDocumentMatrix(corpus)
  matrix <- as.matrix(dtm)
  word_freq <- sort(rowSums(matrix), decreasing = TRUE)
  return(word_freq)
}

ham_freq <- create_word_freq(sms_data$CleanText[sms_data$Type == "ham"])
spam_freq <- create_word_freq(sms_data$CleanText[sms_data$Type == "spam"])

# Create word clouds
par(mfrow = c(1, 2), mar = c(2, 2, 2, 2))

# Ham word cloud
wordcloud(names(ham_freq), ham_freq,
          max.words = 100,
          scale = c(3, 0.5),
          colors = brewer.pal(8, "Dark2"),
          main = "Ham Messages")
title("Ham Word Cloud")

# Spam word cloud
wordcloud(names(spam_freq), spam_freq,
          max.words = 100,
          scale = c(3, 0.5),
          colors = brewer.pal(8, "Reds"),
          main = "Spam Messages")
title("Spam Word Cloud")

# Create bar plots of top words
plot_top_words <- function(word_freq, title, color) {
  top_words <- head(word_freq, 15)
  barplot(top_words,
          las = 2,
          col = color,
          main = title,
          cex.names = 0.7,
          ylab = "Frequency")
}

par(mfrow = c(1, 2))
plot_top_words(ham_freq, "Top Ham Words", "skyblue")
plot_top_words(spam_freq, "Top Spam Words", "salmon")

# Create document term matrix for all messages
create_dtm <- function(text) {
  corpus <- Corpus(VectorSource(text))
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removeWords, stopwords("english"))
  corpus <- tm_map(corpus, stripWhitespace)

  # Create DTM with minimal sparsity
  dtm <- DocumentTermMatrix(corpus,
                            control = list(
                              weighting = weightTfIdf,
                              wordLengths = c(3, 20),
                              bounds = list(global = c(2, Inf))
                            ))

  # Remove sparse terms
  dtm <- removeSparseTerms(dtm, 0.99)
  return(dtm)
}

# Create feature matrix
dtm <- create_dtm(sms_data$CleanText)
dtm_matrix <- as.matrix(dtm)
sms_features <- as.data.frame(dtm_matrix)

# Add response variable
sms_features$Type <- factor(sms_data$Type)

# Check for empty documents and remove them
sms_features <- sms_features[rowSums(sms_features[, -ncol(sms_features)]) > 0, ]
str(sms_features)
sum(is.na(sms_features))



#sms_features_balanced <- ROSE(Type ~ ., data = sms_features, seed = 123)$data

# Apply SMOTE
sms_features_balanced <- sms_features ##step_smote(Type ~ ., data = sms_features, over_ratio = 1)

table(sms_features_balanced$Type)
#sms_features_balanced <- SMOTE(Type ~ ., data = sms_features, perc.over = 200, perc.under = 200)

# Split data maintaining class proportions
set.seed(123)
train_index <- createDataPartition(sms_features_balanced$Type, p = 0.7,
                                   list = FALSE,
                                   times = 1)
train_data <- sms_features_balanced[train_index, ]
test_data <- sms_features_balanced[-train_index, ]

# Train Naive Bayes with Laplace smoothing
nb_model <- naiveBayes(Type ~ .,
                       data = train_data,
                       laplace = 1)

# Make predictions
train_pred <- predict(nb_model, train_data)
test_pred <- predict(nb_model, test_data)

# Evaluate performance
print("Training Set Performance:")
train_cm <- confusionMatrix(train_pred, train_data$Type)
print(train_cm)

print("\nTest Set Performance:")
test_cm <- confusionMatrix(test_pred, test_data$Type)
print(test_cm)

# Cross-validation
ctrl <- trainControl(method = "cv",
                     number = 10,
                     verboseIter = FALSE)

nb_cv <- train(Type ~ .,
               data = train_data,
               method = "nb",
               trControl = ctrl,
               tuneGrid = data.frame(fL = 1, usekernel = FALSE))

print("\nCross-validation Results:")
print(nb_cv)

# Calculate and print additional metrics
precision <- posPredValue(test_pred, test_data$Type)
recall <- sensitivity(test_pred, test_data$Type)
f1_score <- 2 * (precision * recall) / (precision + recall)

print("\nModel Metrics:")
cat(sprintf("Precision: %.3f\n", precision))
cat(sprintf("Recall: %.3f\n", recall))
cat(sprintf("F1 Score: %.3f\n", f1_score))

# ROC-AUC
roc_curve <- roc(test_data$Type, as.numeric(test_pred))
auc_value <- auc(roc_curve)
cat(sprintf("ROC-AUC: %.3f\n", auc_value))

# Save important terms
important_terms <- tail(sort(colSums(dtm_matrix)), 20)
print("\nTop 20 Important Terms:")
print(important_terms)
