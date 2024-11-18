library(tm)
library(wordcloud)
library(RColorBrewer)
library(e1071)
library(caret)
library(DMwR2)
library(pROC)

install.packages("themis")
library(themis)

# Load the SMS data
sms_data <- read.csv("D:\\datasets\\SMS DATA.csv", stringsAsFactors = FALSE)
names(sms_data) <- c("Type", "Text")
sms_data$Type <- factor(tolower(sms_data$Type))

# Clean and preprocess text data
sms_data$CleanText <- sapply(sms_data$Text, function(x) tolower(gsub("[^[:alnum:]]", " ", x)))

# Create corpus with enhanced preprocessing
create_corpus <- function(text) {
  corpus <- Corpus(VectorSource(text))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removeWords, stopwords("english"))
  corpus <- tm_map(corpus, stemDocument)
  corpus <- tm_map(corpus, stripWhitespace)
  return(corpus)
}


# Word Clouds
ham_words <- sms_data$CleanText[sms_data$Type == "ham"]
spam_words <- sms_data$CleanText[sms_data$Type == "spam"]

# Process corpora
ham_words <- create_corpus(ham_words)
spam_words <- create_corpus(spam_words)



par(mfrow = c(1, 2))
wordcloud(ham_words, max.words = 100, colors = brewer.pal(8, "Dark2"))
title("Word Cloud for Ham Messages")
wordcloud(spam_words, max.words = 100, colors = brewer.pal(8, "Reds"))
title("Word Cloud for Spam Messages")


# Spam word cloud
wordcloud(names(spam_freq), spam_freq,
          max.words = 100,
          scale = c(3, 0.5),
          colors = brewer.pal(8, "Reds"),
          main = "Spam Messages")
title("Spam Word Cloud")

# # Create bar plots of top words
# plot_top_words <- function(word_freq, title, color) {
#   top_words <- head(word_freq, 15)
#   barplot(top_words,
#           las = 2,
#           col = color,
#           main = title,
#           cex.names = 0.7,
#           ylab = "Frequency")
# }

# par(mfrow = c(1, 2))
# plot_top_words(ham_freq, "Top Ham Words", "skyblue")
# plot_top_words(spam_freq, "Top Spam Words", "salmon")
# Define the number of top words to display
top_n <- 20

# Assuming `ham_freq` and `spam_freq` are frequency tables of ham and spam words
ham_df <- data.frame(word = names(ham_freq), freq = ham_freq)
spam_df <- data.frame(word = names(spam_freq), freq = spam_freq)

# Sort by frequency for visualization
ham_df <- ham_df[order(-ham_df$freq), ]
spam_df <- spam_df[order(-spam_df$freq), ]

# Plotting Top Ham Words with Enhanced Aesthetics
#

par(mfrow = c(1, 2), mar = c(5, 4, 4, 2) + 0.1)  # Adding the bottom margin to 5 lines

barplot(ham_df$freq[1:top_n],
        las = 2,
        names.arg = ham_df$word[1:top_n],
        col = colorRampPalette(brewer.pal(9, "PuBu"))(top_n),
        main = "Top 20 Ham Words",
        ylab = "Frequency",
        border = "blue",
        space = 0.8)

# Plotting Top Spam Words with Enhanced Aesthetics
barplot(spam_df$freq[1:top_n],
        las = 2,
        names.arg = spam_df$word[1:top_n],
        col = colorRampPalette(brewer.pal(9, "OrRd"))(top_n),
        main = "Top 20 Spam Words",
        ylab = "Frequency",
        border = "red",
        space = 0.8)


# Create Document Term Matrix (DTM) and Data Balancing
dtm <- DocumentTermMatrix(Corpus(VectorSource(sms_data$CleanText)))
dtm <- removeSparseTerms(dtm, 0.99)
sms_matrix <- as.data.frame(as.matrix(dtm))
sms_matrix$Type <- sms_data$Type

# Apply SMOTE to balance the dataset
# sms_balanced <- SMOTE(Type ~ ., data = sms_matrix, perc.over = 200, perc.under = 150)


install.packages("themis")
library(themis)



# sms_balanced <- recipe(Type ~ ., data = sms_matrix) %>%
# step_smote(Type, over_ratio = 1) %>%
# prep() %>%
# juice()

# Split data into training and testing sets
set.seed(123)
train_index <- createDataPartition(sms_balanced$Type, p = 0.7, list = FALSE)
train_data <- sms_balanced[train_index, ]
test_data <- sms_balanced[-train_index, ]

# Train Naive Bayes Model
nb_model <- naiveBayes(Type ~ ., data = train_data)
train_pred <- predict(nb_model, train_data)
test_pred <- predict(nb_model, test_data)

# Evaluate performance on both training and testing sets
train_cm <- confusionMatrix(train_pred, train_data$Type)
test_cm <- confusionMatrix(test_pred, test_data$Type)

# Display results
cat("Training Set Performance:\n")
print(train_cm)
cat("Test Set Performance:\n")
print(test_cm)
# Plot ROC curve and calculate AUC
test_pred_probs <- predict(nb_model, test_data, type = "raw")[, "ham"]
roc_curve <- roc(test_data$Type, test_pred_probs, levels = c("ham", "spam"))
# Display AUC
cat(sprintf("ROC-AUC: %.3f\n", auc(roc_curve)))
#Plot
plot(roc_curve, col = "blue", main = "ROC Curve for Naive Bayes Model (Test Data)")
