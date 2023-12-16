# Load required packages
library(dplyr)
library(tm)
library(tidyverse)
library(stringr)
library(tidytext)
library(textstem)
library(koRpus)
library(koRpus.lang.en)

# Define a function to clean HTML tags
clean_html <- function(text) {
  gsub("<[^>]+>", " ", text)
}

# Read CSV file
filename <- "Awards.csv"
data <- read.csv(filename, header = TRUE, sep = ",", encoding = "UTF-8", stringsAsFactors = FALSE)

# Function to read a valid percentage from the user
read_percent <- function() {
  n <- as.integer(readline(prompt = "Enter a percentage (positive for head, negative for tail) [-100-100]: "))
  if (is.na(n) || !grepl("^-?[0-9]+$", n) || n > 100 || n < -100) {
    cat("Invalid percentage. Please enter a valid percentage between -100 and 100.\n")
    return(read_percent())
  }
  n
}

# User input for percentage
stated_percent <- read_percent()

# Clean and convert AwardedAmountToDate to numeric
data$AwardedAmountToDate <- as.numeric(gsub("[^0-9.]", "", data$AwardedAmountToDate))

# Function to generate and save a histogram
generate_and_save_histogram <- function(funding, percent, num_rows, topic_dir) {
  color1 <- ifelse(percent < 0, "gray", "gray40")
  color2 <- ifelse(percent < 0, "gray40", "gray")
  head_percent <- abs(percent) + ifelse(percent < 0, 100, 0)
  tail_percent <- 100 - head_percent
  n_in_head <- as.integer((abs(percent) / 100) * num_rows)
  n_in_tail <- num_rows - n_in_head
  cols <- rep(c(color1, color2), c(n_in_head, n_in_tail))
  
  # Save histogram as PNG image
  png(paste(topic_dir, "/award_amount_histogram.png", sep = ""), width = 5, height = 4, units = 'in', res = 300)
  hist(funding, breaks = 20, col = cols, main = NULL, xpd = FALSE, las = 1)
  legend("right", legend = c(paste("Head (", head_percent, "%)", sep = ""), paste("Tail (", tail_percent, "%)", sep = "")),
         fill = c(color1, color2), cex = 0.9)
  title("Distribution of Award Amounts", cex.main = 0.95, line = -2)
  title(xlab = "Award Amount (in millions of dollars)", line = 0)
  title(ylab = "Frequency", line = 2)
  dev.off()
}

# Call the function to generate and save the histogram
#generate_and_save_histogram(data$AwardedAmountToDate/1000000, stated_percent, nrow(data), "/Users/animeshjha/Downloads")

# Convert text to UTF-8 encoding
data$Abstract <- iconv(data$Abstract, to = "UTF-8")

# Create a corpus for text processing
text_corpus <- Corpus(VectorSource(data$Abstract))

# Create a list of custom stop words
custom_stop_words <- c(
  'use', 'novel', 'br', 'studied', 'human', 'hungry', 'recent', 'enormous', 'science', 'work', 'also', 'past', 'witnessed', 'award',
  'nsfs', 'merit', 'deemed', 'mission', 'worthy', 'decades', 'much', 'shown', 'non', 'project', 'research', 'systems', 'using', 'students', 'new',
  'support', 'nsf', 'reflects', 'learning', 'based', 'high', 'from', 'subject', 're', 'edu', 'use', 'br', 'project', 'research', 'systems', 'using',
  'students', 'new', 'support', 'nsf', 'reflects', 'learning', 'based', 'high'
)

# Text preprocessing
text_corpus <- tm_map(text_corpus, content_transformer(tolower))
text_corpus <- tm_map(text_corpus, removePunctuation)
text_corpus <- tm_map(text_corpus, removeNumbers)
text_corpus <- tm_map(text_corpus, removeWords, custom_stop_words)
text_corpus <- tm_map(text_corpus, stripWhitespace)

# View processed text
processed_text <- sapply(text_corpus, as.character)

# Save copy of abstracts for bigram work
nsf_funding <- tibble(id = as.character(data$AwardNumber),
                      abstract = data$Abstract,
                      amount = data$AwardedAmountToDate)

# Unnest bigrams and remove stop words
nsf_funding_bigram <- nsf_funding %>%
  unnest_tokens(bigram, abstract, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word, !word2 %in% stop_words$word) %>%
  unite(bigram, word1, word2, sep = " ")

# Unnest tokens and remove stop words
nsf_funding <- nsf_funding %>% 
  unnest_tokens(word, abstract)  %>% 
  anti_join(stop_words)

# Prepare for lemmatization
lemma_unique <- nsf_funding %>%
  select(word) %>%
  mutate(word_clean = str_replace_all(word, "\u2019s|'s", "")) %>%
  filter(!duplicated(word_clean), !is.na(word_clean)) %>%
  arrange(word)

# Lemmatize the words
lemma_unique$lemma <- lemmatize_words(lemma_unique$word_clean)
lemma_unique$lemma[is.na(lemma_unique$lemma)] <- lemma_unique$word_clean[is.na(lemma_unique$lemma)]

# Replace original words in nsf_funding with lemmas
lemma_new <- left_join(nsf_funding, lemma_unique, by = "word") %>%
  mutate(word = ifelse(is.na(lemma), word_clean, lemma)) %>%
  subset(select = c(id, word))

# Create a text corpus from lemmatized words
my_corpus <- Corpus(VectorSource(lemma_new$word))

# Strip digits and create document-term matrix
my_dtm <- DocumentTermMatrix(tm_map(my_corpus, removeNumbers))

# Sample terms
sampled_indices <- sample(seq_len(ncol(my_dtm)), round(ncol(my_dtm) * 0.15))
my_dtm <- my_dtm[, sampled_indices]

# Calculate term frequency
term_freq <- colSums(as.matrix(my_dtm))
freq_order <- order(term_freq, decreasing = TRUE)

# Inspect most and least frequently occurring terms
term_freq[head(freq_order)]
term_freq[tail(freq_order)]

# Load required packages
library(ggplot2)
library(wordcloud)
library(RColorBrewer)
library(dplyr)
library(tidyr)
library(tm)
library(tidytext)
library(ldatuning)
library(topicmodels)

# Function to create and save bar graph
create_and_save_bar_graph <- function(data, threshold, filename, x_size = 12, title_size = 14) {
  p <- ggplot(subset(data, occurrences > threshold), aes(term, occurrences)) +
    geom_bar(stat = "identity") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = x_size),
      axis.text.y = element_text(size = 12),
      plot.title = element_text(size = title_size, hjust = 0.5)
    )
  
  print(p)
  ggsave(filename)
}

# Create and save bar graphs
create_and_save_bar_graph(wf, 500, "WordFreqBarGraph.png")
create_and_save_bar_graph(wf, 1000, "WordFreqBarGraphHigh.png", 12, 14)
create_and_save_bar_graph(wf, 1000, "WordFreqBarGraph101.png", 12, 14)

# Set seed for reproducibility
set.seed(42)

# Create and save word cloud
wordcloud(names(freq), freq, max.words = 100, scale = c(4, .2), colors = brewer.pal(6, "Dark2"))
png("FreqWordCloud.png", width = 1280, height = 800)
dev.off()

# Load required packages
library(dplyr)
library(tidyr)
library(tm)
library(tidytext)

# Count words
word_counts <- lemma_new %>%
  count(id, word, sort = TRUE) %>%
  ungroup()

# Create document-term matrix
desc_dtm <- word_counts %>%
  cast_dtm(id, word, n)

# Calculate tf-idf
desc_tf_idf <- lemma_new %>% 
  count(id, word, sort = TRUE) %>%
  ungroup() %>%
  bind_tf_idf(word, id, n)

# Filter words with low idf
low_idf <- desc_tf_idf %>%
  filter(idf > 0.38 & idf < 2)

# Save low idf words to CSV file
write.csv(low_idf, "low_idf.csv")

# Display top 28 low idf words
print(head(low_idf, 28))

# Calculate term frequency
freq <- colSums(as.matrix(desc_dtm))

# Create sort order (descending)
ord <- order(freq, decreasing = TRUE)

# Display most and least frequently occurring terms
most_frequent <- freq[head(ord)]
print(most_frequent)

least_frequent <- freq[tail(ord)]
print(least_frequent)

# Load required packages
library(wordcloud)
library(RColorBrewer)

# Set seed for reproducibility
set.seed(42)

# Create and save TF-IDF word cloud
wordcloud(names(freq), freq, min.freq = 1000, scale = c(4, .2), colors = brewer.pal(6, "Dark2"))
png("TF-IDFWordCloud.png", width = 1280, height = 800)
dev.off()

# Find optimal number of topics
result <- FindTopicsNumber(
  desc_dtm,
  topics = seq(from = 10, to = 20, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 1234),
  mc.cores = 8L,
  verbose = TRUE
)

# Save plot to PNG file
png("FindTopicNumber.png", width = 1280, height = 800)
FindTopicsNumber_plot(result)
dev.off()

# Display plot
FindTopicsNumber_plot(result)

# Define a function to calculate the harmonic mean
calculate_harmonic_mean <- function(log_likelihoods, precision = 2000L) {
  median_ll <- median(log_likelihoods)
  as.double(median_ll - log(mean(exp(-mpfr(log_likelihoods, prec = precision) + median_ll))))
}

# Load required packages
library(tm)
library(topicmodels)
library(Rmpfr)
library(ggplot2)
library(scales)

# Assuming 'desc_dtm' is your document-term matrix

# Create a subset of data for faster testing
subset_size <- 100  # Adjust the subset size as needed
desc_dtm_subset <- desc_dtm[1:subset_size, ]

seqk <- seq(2, 40, 1)  # Use a smaller range for faster testing
burnin <- 100
iter <- 100
keep <- 10

# To measure overall execution time
start_time <- Sys.time()

fitted_models <- lapply(seqk, function(k) {
  # Display a message
  print(paste("Fitting model for k =", k))
  
  # Record start time for the current k
  k_start_time <- Sys.time()
  
  # Fit LDA model with adjusted parameters
  lda_model <- LDA(desc_dtm_subset, k = k, method = "Gibbs", control = list(burnin = burnin, iter = iter, keep = keep))
  
  # Calculate elapsed time for the current k
  k_elapsed_time <- Sys.time() - k_start_time
  print(paste("Elapsed time for k =", k_elapsed_time))
  
  # Return the fitted model
  return(lda_model)
})

# Calculate overall elapsed time
overall_elapsed_time <- Sys.time() - start_time
print(paste("Total elapsed time:", overall_elapsed_time))

# Extract the log-likelihoods from each topic
log_likelihoods_list <- lapply(fitted_models, function(model) model@logLiks[-c(1:(burnin/keep))])

# Compute the harmonic means
harmonic_means <- sapply(log_likelihoods_list, function(ll) calculate_harmonic_mean(ll))

# Create a plot of the harmonic means
harmonic_mean_plot <- ggplot(data.frame(seqk, harmonic_means), aes(x = seqk, y = harmonic_means)) +
  geom_path(lwd = 1.5) +
  theme(
    text = element_text(family = NULL),
    axis.title.y = element_text(vjust = 1, size = 16),
    axis.title.x = element_text(vjust = -.5, size = 16),
    axis.text = element_text(size = 16),
    plot.title = element_text(size = 20)
  ) +
  scale_y_continuous(labels = comma) +
  xlab('Number of Topics') +
  ylab('Harmonic Mean') +
  annotate("text", x = 25, y = max(harmonic_means) + 10000, 
           label = paste("The optimal number of topics is", seqk[which.max(harmonic_means)])) +
  ggtitle(expression(atop("LDA Analysis of NSF Program", 
                          atop(italic("Harmonic Mean"), ""))))

png("LDAHarmonicMean.png", width = 1280, height = 800)
print(harmonic_mean_plot)
dev.off()

# Visualize topics as word clouds
for (topic_num in 1:NumTopics){
  set.seed(42)
  topic_to_viz <- topic_num
  top_terms <- sort(lda_model$terms[topic_to_viz, ], decreasing = TRUE)[1:100]
  words <- names(top_terms)
  probabilities <- sort(lda_model$terms[topic_to_viz, ], decreasing = TRUE)[1:100]
  my_colors <- brewer.pal(8, "Dark2")
  
  # Save the word cloud to a PNG file
  png(paste0("WordCloud", topic_num, ".png"), width = 1280, height = 800)
  wordcloud(words, probabilities, scale = c(4, .2), random.order = FALSE, color = my_colors)
  dev.off()
  
  # Display the word cloud
  wordcloud(words, probabilities, scale = c(4, .2), random.order = FALSE, color = my_colors)
}

# Save and display the top terms in each LDA topic
png("TopTermsLDAHistogram.png", width = 1280, height = 1600)
top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  group_by(topic, term) %>%
  arrange(desc(beta)) %>%
  ungroup() %>%
  mutate(term = factor(paste(term, topic, sep = "__"), 
                       levels = rev(paste(term, topic, sep = "__")))) %>%
  ggplot(aes(term, beta, fill = as.factor(topic))) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +
  labs(title = "Top terms in each LDA topic",
       x = NULL, y = expression(beta)) +
  facet_wrap(~ topic, ncol = 3, scales = "free")
dev.off()

# Display the top 10 terms in each model
lda_terms <- terms(lda_model, 10)
print(lda_terms)

lda_gamma <- tidy(lda_model, matrix = "gamma")
print(lda_gamma)
library(ggplot2)
library(tidyverse)
library(data.table)
library(scales)

# Set row names and rename columns in MyData
setDT(MyData, keep.rownames = TRUE)[]
colnames(MyData)[1] <- "FundingRank"
MyData$AwardNumber <- as.character(MyData$AwardNumber)

# Print the structure and head of lda_gamma
str(lda_gamma)
head(lda_gamma)

# Left join lda_gamma with MyData
lda_gamma_joined <- lda_gamma %>%
  left_join(MyData, by = c("document" = "AwardNumber")) %>%
  mutate(FundingRank = as.numeric(FundingRank),
         AwardedAmountToDate = as.numeric(gsub("[$,]", "", AwardedAmountToDate)),
         worth = gamma * AwardedAmountToDate)

# Aggregate worth for each topic
topic_worth <- lda_gamma_joined %>%
  group_by(topic) %>%
  summarize(WorthSum = sum(worth)) %>%
  arrange(desc(WorthSum))

# Save topic_worth as CSV
write.csv(topic_worth, "TopicWorthRank.csv", row.names = FALSE)

# Left join lda_gamma_joined with topic_worth
lda_gamma_worth <- lda_gamma_joined %>%
  left_join(topic_worth, by = "topic") %>%
  na.omit()

# Create a scatter plot
p <- ggplot(lda_gamma_worth, aes(x = WorthSum, y = FundingRank, size = gamma)) +
  geom_point(shape = 23) +
  scale_size_continuous(limits = c(0, 0.01)) +
  ylim(0, nrow(lda_gamma_worth)) +
  xlim(1, max(lda_gamma_worth$WorthSum)) +
  labs(subtitle = "Topic by Document",
       x = "Worth Sum",
       y = "Funding Rank",
       title = "Gamma Level sorted by Funding Level",
       caption = "Topic Analysis")

# Save the plot
ggsave("DocTopic.png", height = 10, device = "png")

# Check for NA values in FundingRank and TopicWorthRank
print(summary(lda_gamma_worth$FundingRank))
print(any(is.na(lda_gamma_worth$FundingRank)))
print(summary(lda_gamma_worth$WorthSum))
print(any(is.na(lda_gamma_worth$WorthSum)))

# Create a data frame for nsf_funding
nsf_funding <- data.frame(
  id = as.character(MyData$AwardNumber),
  abstract = MyData$Abstract,
  amount = as.numeric(gsub("[$,]", "", MyData$AwardedAmountToDate))
)

# Perform necessary operations on nsf_funding
nsf_funding_join <- nsf_funding %>%
  mutate(document = as.character(id))

# Check if 'abstract' column exists in funding_gamma
if ('abstract' %in% colnames(funding_gamma)) {
  funding_gamma <- subset(funding_gamma, select = -c(id, abstract))
} else {
  print("Column 'abstract' not found in funding_gamma dataframe.")
}

# Merge lda_gamma with nsf_funding_join
funding_gamma <- merge(lda_gamma, nsf_funding_join, by = "document", all.x = TRUE)
funding_gamma <- subset(funding_gamma, select = -c(id, abstract))

# Top 5 topics for each document
doc_top <- funding_gamma %>%
  group_by(document) %>%
  top_n(5, gamma) %>%
  ungroup() %>%
  arrange(document, desc(gamma))

# Save top topics to CSV
write.csv(doc_top, "topTopics5Documents.csv", row.names = FALSE)

# Consensus document for each topic
document_topics <- doc_top %>%
  count(document, topic, amount) %>%
  group_by(topic) %>%
  top_n(1, n) %>%
  ungroup() %>%
  transmute(consensus = document, topic, amount)

# Print document_topics
print(document_topics)

# Visualization of document gamma probabilities across topics
p_gamma_map <- funding_gamma %>%
  mutate(document = factor(document, levels = rev(unique(document)))) %>%
  group_by(document) %>%
  top_n(1) %>%
  ungroup() %>%
  ggplot(aes(reorder(document, amount), y = gamma, label = NA, fill = as.factor(topic))) +
  geom_col() +
  geom_text(aes(document, 0.01), hjust = 0, color = "white", size = 0.5) +
  scale_fill_manual(values = c("#F48024", "#0077CC", "#5FBA7D", 
                               "#8C60A7", "#89C5DA", "#DA5724", "#74D944", "#CE50CA", "#3F4921", "#C0717C", "#CBD588", "#5F7FC7", 
                               "#673770", "#D3D93E", "#38333E", "#508578", "#D7C1B1", "#689030", "#AD6F3B", "#CD9BCD", 
                               "#D14285", "#6DDE88", "#652926", "#7FDCC0", "#C84248", "#8569D5", "#5E738F", "#D1A33D", 
                               "#8A7C64", "#599861", "#A48024", "#B077CC", "#CFBA7D", "#DFBA7D")) +
  scale_y_continuous(expand = c(0, 0), labels = percent_format()) +
  coord_flip() +
  theme_minimal() +
  theme(axis.text.y = element_blank()) +
  labs(x = NULL, y = expression(gamma), fill = "Topic")

# Save the gamma map plot
ggsave("TopicGammaMap.png", height = 10, device = "png")

# Heatmap to visualize gamma probabilities across topics for each document
p_heatmap <- ggplot(funding_gamma, aes(x = topic, y = document, fill = gamma)) +
  geom_tile() +
  scale_fill_gradientn(colors = c("#FFFFFF", "#3182BD", "#31A354", "#FDD0A2", "#FDAE6B", "#E6550D"),
                       na.value = "grey50", guide = "colorbar") +
  theme_minimal() +
  labs(x = "Topic", y = "Document", fill = expression(gamma)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        legend.position = "right") +
  ggtitle("Gamma Probabilities of Documents across Topics (Heatmap)")

# Save the heatmap plot
ggsave("TopicGammaHeatmap.png", height = 10, device = "png")
library(topicmodels)
library(tidytext)
library(magrittr)
library(dplyr)
library(slam)
library(wordcloud)
library(ggplot2)
library(LDAvis)

# Check and install missing libraries
required_libraries <- c("topicmodels", "slam", "LDAvis")
loaded_libraries <- sapply(required_libraries, function(lib) lib %in% installed.packages())

if (!all(loaded_libraries)) {
  install.packages(required_libraries[!loaded_libraries])
  sapply(required_libraries, require, character.only = TRUE)
}

# Perform LDA with a specified number of topics
num_topics <- 11
desc_lda <- LDA(desc_dtm, method = "Gibbs", k = num_topics, control = list(seed = 1234))
saveRDS(desc_lda, file = "desc_lda.rds")
tidy_lda <- tidy(desc_lda)

# Examine the top 10 terms for each topic
top_terms <- tidy_lda %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, desc(beta))

# Visualize topics as word clouds and save as PNG files
tm_result <- posterior(desc_lda)
for (topic_num in 1:num_topics) {
  set.seed(42)
  words <- names(sort(tm_result$terms[topic_num, ], decreasing = TRUE)[1:100])
  probabilities <- sort(tm_result$terms[topic_num, ], decreasing = TRUE)[1:100]
  my_colors <- brewer.pal(8, "Dark2")
  
  # Save the word cloud to a PNG file
  png(paste0("WordCloud", topic_num, ".png"), width = 1280, height = 800)
  wordcloud(words, probabilities, scale = c(4, .2), random.order = FALSE, colors = my_colors)
  dev.off()
}

# Save the top terms in each LDA topic as a histogram in PNG format
png("TermsLDAHistogram.png", width = 1280, height = 1600)
top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  group_by(topic, term) %>%
  arrange(desc(beta)) %>%
  ungroup() %>%
  mutate(term = factor(paste(term, topic, sep = "__"), levels = rev(paste(term, topic, sep = "__")))) %>%
  ggplot(aes(term, beta, fill = as.factor(topic))) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +
  labs(title = "Top terms in each LDA topic",
       x = NULL, y = expression(beta)) +
  facet_wrap(~ topic, ncol = 3, scales = "free")
dev.off()

# Function to convert LDA model to LDAvis
lda_model_to_ldavis <- function(lda_model) {
  post <- topicmodels::posterior(lda_model)
  vocab <- colnames(post$terms)
  vocab <- vocab[!(is.na(vocab) | nchar(vocab) == 0)]
  
  mat <- lda_model@wordassignments
  
  # Calculate term frequencies
  term_freq <- colSums(mat)
  
  # Ensure vocab and term freq match 
  if (length(vocab) != length(term_freq)) {
    stop("Vocab and term frequencies must have the same length") 
  }
  
  LDAvis::createJSON(
    phi = post$terms[, vocab, drop = FALSE],  
    theta = post$topics,
    vocab = vocab,
    doc.length = rowSums(mat, na.rm = TRUE),
    term.frequency = term_freq
  )
}