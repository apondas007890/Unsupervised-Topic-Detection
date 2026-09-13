# Unsupervised Topic Detection

Unsupervised Topic Detection is a Natural Language Processing (NLP) project built with R.

The main goal of this project is to find hidden topics in text data without manually assigning a topic to every document.

The project takes text data, cleans and processes it, converts the text into numerical form, and then uses LDA (Latent Dirichlet Allocation) to find the main topics in the dataset.

After finding the topics, keyword-based labeling is used to give each topic a meaningful name such as Sports, Health, Crime, Food, Travel, or Politics.

---

## Project Overview

Working with a large amount of text can make it difficult to understand what each document is about.

This project uses Natural Language Processing techniques to process text and automatically find common topics.

The main steps of the project are:

1. Load the text dataset
2. Clean the text
3. Tokenize the text
4. Remove stop words
5. Apply stemming
6. Apply lemmatization
7. Correct spelling
8. Create a Document-Term Matrix
9. Calculate TF-IDF
10. Apply LDA topic modeling
11. Find the most important words for each topic
12. Assign topic labels
13. Save the processed and labeled dataset
14. Visualize the topic distribution

---

## Dataset

The project uses text data collected from online news articles.

The current dataset contains news text from different categories.

Example sources include:

- The Daily Star
- BBC News

The original article links and titles are stored in:

```text
Text_Data_Link.txt
````

The project also includes the processed datasets:

```text
Final_Project.csv
Labeled_Dataset.xlsx
```

---

## Text Preprocessing

Before applying topic detection, the text needs to be cleaned.

The project performs several preprocessing steps.

### Contraction Replacement

Common contractions are expanded using the `textclean` package.

For example:

```text
can't → cannot
```

### Emoji Removal

Emojis are removed from the text.

### Lowercase Conversion

All text is converted to lowercase so that words such as:

```text
Football
football
FOOTBALL
```

are treated as the same word.

### Removing Punctuation

Punctuation marks are removed from the text.

### Removing Numbers

Numbers are removed from the text.

### Removing HTML Tags

HTML tags are removed if they are present in the text.

### Tokenization

The text is divided into individual words using the `tokenizers` package.

For example:

```text
"Manchester City won the match"

→

"Manchester"
"City"
"won"
"the"
"match"
```

### Stop Word Removal

Common English words that do not provide much information are removed.

Examples include:

```text
the
is
and
of
to
```

### Stemming

Stemming is used to reduce words to their basic word form.

This is done using the `SnowballC` package.

### Lemmatization

Lemmatization is also applied to reduce words to their meaningful base form.

The project uses the `textstem` package for this step.

### Spelling Correction

The `hunspell` package is used to check words and suggest corrections when possible.

---

## Feature Creation

After text preprocessing, the project creates a Document-Term Matrix.

The Document-Term Matrix represents the relationship between documents and the words that appear in them.

The project also calculates TF-IDF values.

TF-IDF helps measure how important a word is within a document compared with the complete collection of documents.

---

## Topic Modeling

The main topic detection method used in this project is:

**LDA (Latent Dirichlet Allocation)**

The project uses the `topicmodels` package in R.

The LDA model is created with:

```r
lda_model <- LDA(dtm, k = 2, control = list(seed = 1234))
```

Here:

```text
k = 2
```

means that the model is asked to find two topics from the dataset.

The model then returns the important words associated with each topic.

The project extracts the top 35 words for each topic.

---

## Topic Labeling

LDA gives topics as groups of words, but it does not automatically give them names such as "Sports" or "Health".

To make the results easier to understand, this project uses keyword-based topic labeling.

The project contains keyword groups for:

* Sports
* Health
* Crime
* Food
* Travel
* Politics

For example, the Sports keyword group contains words such as:

```text
club
city
league
contract
goal
champion
player
season
win
manchester
```

The Health keyword group contains words such as:

```text
virus
case
health
disease
infect
hospital
pandemic
outbreak
```

The words found by the LDA model are compared with these keyword groups.

The category with the highest matching score is selected as the topic label.

---

## Document Topic Assignment

After labeling the topics, each document is assigned to the topic with the highest probability.

For example, if a document has:

```text
Topic 1 Probability = 0.82
Topic 2 Probability = 0.18
```

the document will be assigned to Topic 1.

This allows the project to create a labeled dataset based on the results of the unsupervised model.

---

## Visualization

The project uses `ggplot2` to create a bar chart showing the number of documents assigned to each topic.

The visualization is titled:

```text
Topic Distribution
```

This makes it easier to understand how the detected topics are distributed across the dataset.

---

## Technologies Used

* R
* Natural Language Processing (NLP)
* LDA Topic Modeling
* TF-IDF
* ggplot2
* textclean
* tokenizers
* tm
* SnowballC
* textstem
* hunspell
* topicmodels
* openxlsx

---

## R Packages Used

The project uses the following R packages:

```text
textclean
tokenizers
tm
SnowballC
textstem
hunspell
topicmodels
openxlsx
ggplot2
```

If a package is not installed, it can be installed using:

```r
install.packages("package_name")
```

For example:

```r
install.packages("topicmodels")
```

---

## Project Structure

```text
Unsupervised-Topic-Detection/
│
├── Final_Project.csv
├── Group1_Final_Project.R
├── Labeled_Dataset.xlsx
├── Report.docx
├── Text_Data_Link.txt
└── README.md
```

### Group1_Final_Project.R

Contains the complete R code for:

* Data loading
* Text cleaning
* Tokenization
* Stop word removal
* Stemming
* Lemmatization
* Spelling correction
* Document-Term Matrix creation
* TF-IDF calculation
* LDA topic modeling
* Topic labeling
* Document classification
* Data export
* Visualization

### Final_Project.csv

Contains the text data used for the project.

### Labeled_Dataset.xlsx

Contains the processed dataset along with the topic information and assigned topic labels.

### Text_Data_Link.txt

Contains the source links and titles of the text data used in the project.

### Report.docx

Contains the project report and additional project information.

---

## How to Run

### Requirements

To run this project, you need:

* R
* RStudio (recommended)
* Internet connection for installing packages

### Steps

1. Clone or download this repository.

2. Open the project in RStudio.

3. Open:

```text
Group1_Final_Project.R
```

4. Make sure the required packages are installed.

5. Update the CSV file paths in the R script.

The original script contains local file paths such as:

```text
F:/AIUB All Sem/...
```

These paths need to be changed to the location of the dataset on your computer.

6. Run the R script.

7. The processed and labeled dataset will be created as an Excel file.

---

## Main Workflow

The complete workflow of the project can be summarized as:

```text
Raw Text Data
      |
      v
Text Cleaning
      |
      v
Tokenization
      |
      v
Stop Word Removal
      |
      v
Stemming + Lemmatization
      |
      v
Spelling Correction
      |
      v
Document-Term Matrix
      |
      v
TF-IDF
      |
      v
LDA Topic Modeling
      |
      v
Topic Words
      |
      v
Topic Labeling
      |
      v
Document Topic Assignment
      |
      v
Labeled Dataset
      |
      v
Topic Distribution Visualization
```

---

## What I Learned

This project helped me understand how text data can be processed and analyzed using R.

Through this project, I learned about:

* Natural Language Processing
* Text cleaning
* Tokenization
* Stop word removal
* Stemming
* Lemmatization
* Spelling correction
* Document-Term Matrix
* TF-IDF
* Topic modeling
* LDA
* Topic probability
* Unsupervised learning
* Data visualization
* Working with text data in R

---

## Challenges

Some of the main challenges in this project were:

* Cleaning raw text data
* Handling different forms of the same word
* Removing unnecessary words
* Correcting spelling mistakes
* Converting text into a format that can be used by machine learning models
* Understanding the topics generated by LDA
* Giving meaningful names to automatically generated topics

---

## Future Improvements

Some possible improvements for this project are:

* Use a larger text dataset
* Detect more than two topics
* Improve automatic topic labeling
* Compare LDA with other topic modeling methods
* Add more visualizations
* Improve spelling correction
* Use better keyword matching
* Create an interactive dashboard
* Compare the predicted topics with manually labeled data
* Build a simple web interface for topic detection

---

## Conclusion

This project demonstrates how unsupervised learning can be used to find hidden topics in text data.

Instead of manually assigning a topic to every document, the project uses text preprocessing and LDA to discover groups of related words.

The detected topics are then given meaningful labels using keyword matching, and each document is assigned to the most likely topic.

This project provides a practical introduction to Natural Language Processing, topic modeling, and unsupervised learning using R.
