# NSF Grant Data Analysis Readme

## Overview
This repository contains a comprehensive analysis of National Science Foundation (NSF) grant data from the Division of Information and Intelligent Systems Organization, spanning the years 2018 to 2022. The analysis employs a meticulous methodology that combines data preprocessing and advanced Natural Language Processing (NLP) techniques to extract meaningful insights from grant abstracts.

## Methodology
### Data Preprocessing
1. **Data Selection:**
   - Focus on NSF grant fund data from 2018 to 2022 within the Division of Information and Intelligent Systems Organization.

2. **Encoding Conversion:**
   - Convert dataset encoding to UTF-8 for uniformity in character representation.

3. **Creating a Corpus:**
   - Build a corpus from the Abstract column for NLP analyses.

4. **Removing Stopwords:**
   - Enhance relevance by removing common stopwords from the corpus.

5. **Cleaning Abstracts:**
   - Remove special characters, numbers, and extra white spaces. Convert characters to lowercase.

6. **Creating a Tibble:**
   - Structure data into a tibble for organized handling.

7. **Bigram Creation:**
   - Generate bigrams from the Abstract column for insights into co-occurring terms.

8. **Stopword Removal from Bigrams:**
   - Refine the dataset by removing stopwords from generated bigrams.

9. **Tokens Unnesting:**
   - Split bigrams and extract tokens from the Abstract column for detailed examination.

10. **Lemmatization:**
   - Transform words to their base form for more accurate analysis.

11. **Updating Text Corpus:**
   - Update the text corpus with lemmatized terms for consistency.

12. **Document Term Matrix (DTM) Creation:**
   - Construct DTM using the bag of words method for structured term frequency representation.

### NLP Techniques
1. **Determining Optimal Number of Topics:**
   - Use Ldatuning methods and Harmonic Mean to identify the optimal number of topics based on different evaluation metrics.

2. **LDA Model Implementation:**
   - Implement Latent Dirichlet Allocation (LDA) model with the determined optimal number of topics.

3. **Top Words Extraction:**
   - Extract the top 10 words for each of the identified topics.

### Explanation of NLP Terms
- Detailed explanations of Bag of Words, Bigrams, Stopwords, Tokens, Lemmatization, Document Term Matrix (DTM), and Latent Dirichlet Allocation (LDA) Present in the Report.

## Results
- Visual representations including Word Clouds for each topic, an Intertopic Distance Map, and insights derived from keyword overlaps and distinctive topics.

## Conclusion
- Summary of key findings and the significance of visualizations in unraveling the diverse realms explored by NSF-supported Big Data research.