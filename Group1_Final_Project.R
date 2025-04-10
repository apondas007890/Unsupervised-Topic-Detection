TextData1 <- read.csv("F:/AIUB  All Sem/Aiub Sem - 10/INTRODUCTION TO DATA SCIENCE [A]/Final/Lab/Final_Project/Final_Project.csv")
TextData2 <- read.csv("F:/AIUB  All Sem/Aiub Sem - 10/INTRODUCTION TO DATA SCIENCE [A]/Final/Lab/Final_Project/New_1.csv")
TextData <- rbind(TextData1, TextData2)
print(TextData)

install.packages("textclean")
library(textclean)

TextData$Data <- replace_contraction(TextData$Data)
TextData$Data <- replace_emoji(TextData$Data)
TextData$Data <- tolower(TextData$Data)
TextData$Data <- gsub("[[:punct:]]", "", TextData$Data)
TextData$Data <- gsub("[0-9]+", "", TextData$Data)
TextData$Data <- gsub("<.*?>", "", TextData$Data)
TextData$Data <- gsub("[^a-zA-Z0-9 ]", "", TextData$Data)

install.packages("tokenizers")
library(tokenizers)

TextData$Tokens <- tokenize_words(TextData$Data)
print(TextData$Tokens)

install.packages("tm")
library(tm)

TextData$No_Stop_Words <- lapply(TextData$Tokens, function(tokens) {
  tokens[!tokens %in% stopwords("en")]  
})

install.packages("SnowballC")
library(SnowballC)

TextData$Stemmed_Words <- lapply(TextData$No_Stop_Words, function(tokens) {
  wordStem(tokens, language = "en")
})

install.packages("textstem")
library(textstem)

TextData$Lemmatize_Words <- lapply(TextData$No_Stop_Words, function(tokens) {
  lemmatize_words(tokens)
})

install.packages("hunspell")
library(hunspell)

correct_spelling <- function(tokens) {
  corrected <- sapply(tokens, function(word) {
    if (hunspell_check(word)[1]) {
      return(word)
    } else {
      suggestions <- hunspell_suggest(word)
      if (length(suggestions[[1]]) > 0) {
        return(suggestions[[1]][1])
      } else {
        return(word)
      }
    }
  })
  return(corrected)
}

TextData$Corrected_Words <- lapply(TextData$Lemmatize_Words, correct_spelling)
print(TextData$Corrected_Words)

Corrected_DataFrame <- data.frame(
  Text = sapply(TextData$Corrected_Words, paste, collapse = " "),
  stringsAsFactors = FALSE
)
print(Corrected_DataFrame)

corpus <- Corpus(VectorSource(Corrected_DataFrame$Text))
dtm <- DocumentTermMatrix(corpus)
inspect(dtm)

tfidf_dtm <- weightTfIdf(dtm)
inspect(tfidf_dtm)
tfidf_matrix <- as.matrix(tfidf_dtm)
tfidf_df <- as.data.frame(tfidf_matrix)
print(tfidf_df)

install.packages("topicmodels")
library(topicmodels)

lda_model <- LDA(dtm, k = 2, control = list(seed = 1234))

topics <- terms(lda_model, 35)
print(topics)

topic_probabilities <- posterior(lda_model)$topics
print(topic_probabilities)

TextData <- cbind(TextData, topics)
TextData <- cbind(TextData, topic_probabilities)

install.packages("openxlsx")
library(openxlsx)
write.xlsx(TextData, "F:/AIUB  All Sem/Aiub Sem - 10/INTRODUCTION TO DATA SCIENCE [A]/Final/Lab/Final_Project/Try_Dataset.xlsx", rowNames = FALSE)






# Define a function to label topics based on words
topic_labeling <- function(topic_words) {
  sports_keywords <- c("club", "city", "league", "contract", "sign", "goal", "champion", "player", "season", "win", "manchester")
  health_keywords <- c("virus", "case", "health", "disease", "infect", "kill", "hospital", "pandemic", "outbreak", "minister", "organization")
  crime_keywords <- c("suspect", "burg", "police", "crime", "murder", "arrest", "violence", "prison", "criminal", "report")
  food_keywords <- c("restaurant", "chef", "recipe", "cooking", "dish", "taste", "delicious", "meal", "food", "flavor")
  travel_keywords <- c("tourism", "hotel", "flight", "destination", "journey", "adventure", "explore", "visit", "travel", "vacation")
  politics_keywords <- c("government", "election", "minister", "policy", "president", "vote", "law", "senate", "congress", "leader")
  
  sports_score <- sum(topic_words %in% sports_keywords)
  health_score <- sum(topic_words %in% health_keywords)
  crime_score <- sum(topic_words %in% crime_keywords)
  food_score <- sum(topic_words %in% food_keywords)
  travel_score <- sum(topic_words %in% travel_keywords)
  politics_score <- sum(topic_words %in% politics_keywords)
  
  scores <- c(Sports = sports_score, Health = health_score, Crime = crime_score, Food = food_score, Travel = travel_score, Politics = politics_score)
  
  best_match <- names(which.max(scores))
  return(best_match)
}

# Assign topic labels
topic_1_label <- topic_labeling(topics[, 1])
topic_2_label <- topic_labeling(topics[, 2])
cat("Topic 1 is labeled as:", topic_1_label, "\n")
cat("Topic 2 is labeled as:", topic_2_label, "\n")

# Assign labels to each document based on the highest topic probability
TextData$Topic_Label <- ifelse(topic_probabilities[, 1] > topic_probabilities[, 2], topic_1_label, topic_2_label)


# Save the labeled dataset
write.xlsx(TextData, "F:/AIUB  All Sem/Aiub Sem - 10/INTRODUCTION TO DATA SCIENCE [A]/Final/Lab/Final_Project/Labeled_Dataset.xlsx", rowNames = FALSE)



install.packages("ggplot2")
library(ggplot2)
# Visualization of topic distribution
ggplot(data = data.frame(Topic = factor(c(topic_1_label, topic_2_label)), Count = c(nrow(TextData[TextData$Topic_Label == topic_1_label, ]), nrow(TextData[TextData$Topic_Label == topic_2_label, ]))), aes(x = Topic, y = Count, fill = Topic)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Topic Distribution", x = "Topic", y = "Count")





