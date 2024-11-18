# install.packages("wordcloud")
# install.packages("ggplot2")
# install.packages("syuzhet")
# install.packages("e1071")
# install.packages("caret")
# install.packages("dplyr")
# install.packages("stringr")

# Load required libraries
library(tm)
library(wordcloud)
library(ggplot2)
library(syuzhet)
library(e1071)
library(caret)
library(dplyr)
library(stringr)

# Step 1: Load and prepare the SMS data
#sms_data <- read.csv("D:\\datasets\\SMS DATA.csv")
sms_data <- read.csv("D:\\datasets\\SMS DATA.csv", encoding = "UTF-8", stringsAsFactors = FALSE)
# attach(sms_data)
# summary(sms_data)


# Enhanced text cleaning function
clean_text <- function(text) {
  # Replace smart quotes and apostrophes with standard ones
  text <- str_replace_all(text, "['']", "'")
  ##text <- str_replace_all(text, "[""]", '"')

  # Remove currency symbols and special characters
  text <- str_replace_all(text, "£|€|\\$", " ")
  text <- str_replace_all(text, "[^[:ascii:]]", " ")

  # Fix common encoding issues
  text <- str_replace_all(text, "\\s+", " ")  # Replace multiple spaces with single space
  text <- str_replace_all(text, "[^[:alnum:][:space:]'.,!?]", " ")

  # Clean up any resulting double spaces
  text <- str_trim(text)

  return(text)
}

# First clean the data before creating corpus
sms_data$v2 <- clean_text(sms_data$v2)


# Part 1: Sentiment Analysis and Visualization
# Create corpus for ham and spam separately
create_corpus <- function(text) {
  corpus <- Corpus(VectorSource(text))
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removeWords, stopwords("english"))
  corpus <- tm_map(corpus, stripWhitespace)
  return(corpus)
}

# Prepare separate corpora
ham_corpus <- create_corpus(sms_data$v2[sms_data$v1 == "ham"])
spam_corpus <- create_corpus(sms_data$v2[sms_data$v1 == "spam"])

# Create document term matrices
ham_dtm <- TermDocumentMatrix(ham_corpus)
spam_dtm <- TermDocumentMatrix(spam_corpus)

# Convert to matrices and calculate word frequencies
ham_m <- as.matrix(ham_dtm)
spam_m <- as.matrix(spam_dtm)
ham_word_freq <- sort(rowSums(ham_m), decreasing = TRUE)
spam_word_freq <- sort(rowSums(spam_m), decreasing = TRUE)

# Create word clouds
par(mfrow=c(1,2))
wordcloud(words = names(ham_word_freq), freq = ham_word_freq,
          max.words = 100, random.order = FALSE, rot.per = 0.35,
          colors = brewer.pal(8, "Dark2"), main = "Ham Messages")
wordcloud(words = names(spam_word_freq), freq = spam_word_freq,
          max.words = 100, random.order = FALSE, rot.per = 0.35,
          colors = brewer.pal(8, "Dark2"), main = "Spam Messages")

# Sentiment Analysis
get_sentiment_scores <- function(text) {
  sentiments <- get_nrc_sentiment(text)
  colSums(sentiments)
}

ham_sentiments <- get_sentiment_scores(sms_data$v2[sms_data$v1 == "ham"])
spam_sentiments <- get_sentiment_scores(sms_data$v2[sms_data$v1 == "spam"])

# Create sentiment comparison plot
sentiment_df <- data.frame(
  sentiment = rep(names(ham_sentiments), 2),
  count = c(ham_sentiments, spam_sentiments),
  type = rep(c("Ham", "Spam"), each = length(ham_sentiments))
)

ggplot(sentiment_df, aes(x = sentiment, y = count, fill = type)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Sentiment Analysis of Ham vs Spam Messages",
       x = "Sentiment", y = "Count", fill = "Message Type")




# Part 2: Naive Bayes Classification
# Prepare the data
corpus <- create_corpus(sms_data$v2)
dtm <- DocumentTermMatrix(corpus)

# Convert sparse matrix to dense matrix
dtm_matrix <- as.matrix(dtm)

# Remove terms that appear less than 5 times
dtm_matrix <- dtm_matrix[, colSums(dtm_matrix) >= 5]

# Split the data
set.seed(123)
train_index <- createDataPartition(sms_data$v1, p = 0.7, list = FALSE)
train_data <- dtm_matrix[train_index, ]
test_data <- dtm_matrix[-train_index, ]
train_labels <- sms_data$v1[train_index]
test_labels <- sms_data$v1[-train_index]

# Train Naive Bayes model
nb_model <- naiveBayes(train_data, factor(train_labels))

# Make predictions
train_pred <- predict(nb_model, train_data)
test_pred <- predict(nb_model, test_data)

# Calculate performance metrics
train_cm <- confusionMatrix(factor(train_pred), factor(train_labels))
test_cm <- confusionMatrix(factor(test_pred), factor(test_labels))

# Print results
print("Training Set Performance:")
print(train_cm)
print("\nTest Set Performance:")
print(test_cm)
