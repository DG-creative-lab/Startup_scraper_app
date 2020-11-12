#############################
#                           #
#  UK Based Startup Scraper #
#                           #
#############################


# load the libraries 

library(rvest) # for the actual scraping from the html
library(purrr) # to build functions for out scraper
library(tidyverse) # for all the data manipulation we will need
library(tidytext) # for text mining
library(text2vec) # for tokenisation
#ibrary(quanteda) # for quantitative operations on text data 
#library(RColorBrewer) # to get some nice colour for visualisations
#library(igraph) # for building a graph visualisation fro bigrams in the 
                # companies` description
#library(ggraph) # for visualising the the word combinations in the companies` descriptions
#library(wordcloud2) # for visualising the word frequency in the company description
library(topicmodels) # for defining the topics of the 
library(SnowballC)    # for stemming the company description text before analysis



#################### Build a function that scrapes all the data we want at once #######################################

get_startups <- function(conpany_name = Company_name, revenue =1000000:2000000000, city = City ) {
  
  # get url from input and read html
  
  "https://www.seedtable.com/startups-uk" %>% 
    read_html()-> URL
    
##### scrape the data
    
    # Company Name
    
    URL %>%
        html_nodes(".bg-white .mt-0 a") %>% 
      html_text() -> Company_name 
    
    # Company Description
    
    URL %>%
      html_nodes(".bg-white .mt-0+ p") %>% 
      html_text() -> Description 
    
    # City
    
    URL %>%
      html_nodes(".p-2:nth-child(1)") %>% 
      html_text() -> City 
    
    # Size
    
    URL %>%
      html_nodes(".p-2:nth-child(2)") %>% 
      html_text()->Size
      
    
    # Revenue
    
    URL %>%
      html_nodes(".p-2:nth-last-child(2)") %>% 
      html_text() -> Revenue
  
    # Website
    URL %>%
      html_nodes(".bg-white .mt-0 a") %>% 
      html_attr("href") -> Website
    
    # Linkedin
    
    URL %>%
      html_nodes(".meta a") %>% 
      html_attr("href") -> Linkedin
   
## clean the data a little
    
    Description%>%
    str_remove_all("\n")%>%
      str_squish()%>%
      str_trim()-> Description
    
    Size%>%
      str_remove_all("\n")%>%
      str_squish()%>%
      str_trim()-> Size
    
    City%>%
      str_remove_all("\n")%>%
      str_squish()%>%
      str_trim()-> City
    
    Revenue%>%
      str_remove_all("[\n $ ,]")%>%
      str_squish()%>%
      str_trim()%>%
    as.numeric-> Revenue
  
#### create dataframe, remove nas and return result
    
startup_df <- data.frame(Company_name,Description,City, Size, Revenue,Website, Linkedin )
  
  
startup_df
  
}

### test the function

london_startups<- get_startups(city = "London")
glimpse (london_startups)

####################################### Companies' Description Keywords ##########################

### the second function we will build will be a text mining function into the description of the companies;
### the idea is to provide keywords search option in the app and also to visualise the most repeated words, which presumably will give us an idea 
### of the most repeated industry, business value, mission and so on.. among the most successful start ups for 2020

# 1. Tokenise the text in the description, remove the stopwords (pereform 2 types of tekenisation  - by word and by bigrams. You will use the bigrams later 
# to build a word graph.) When cleaning the bigrams from stop words/ common phrases we apply the "separate, filter, unite"logic
# For both the bigrams and the unigrams tokenised text we will remove only the stop words and general statements ignoring the context specific general words and phrases
# like company and business. The reason is that we will use tfidf function to visualise the keywords specific to different companies and create a keyword search engine 
#functionality for our app.
# You can get a quick understanding on how the TF-Idf works here http://www.tfidf.com/ and a bit more on the R funcion here https://www.tidytextmining.com/tfidf.html
# In short Tf-idf stands for term frequency-inverse document frequency. It defines the importance of a word for a document by weighting it. 
# The weight of the word is defined by counting how often the word appears in the document and offsetting this count by the frequency of the word in the corpus of documents. 
# In our case tfidf will remove general words and phrases that are specific to the context of all companies description like the wowrd "business" or "company"


# Tokenise th text by word, remove the stop words, count the occurrences of the words per document and finally apply the bind_tf_idf() function to convert 
# the company description into keywords that are specific for the company

stop_words_cust<- rbind(stop_words,data.frame(word= c("business", "company", "platform","uk"), lexicon = "custom"))

tidy_description <- get_startups() %>%
  unnest_tokens(word,Description,token="words")%>%
  mutate(word = lemmatize_words(word))%>%  # lematise the words in th description to facilitate he cleaning of the corpusand to narrow the number of derivative words
  anti_join(stop_words_cust)%>%
  group_by(Company_name, word) %>%
  count()%>%
  bind_tf_idf(word, Company_name, n)%>%
  arrange(Company_name)


# the inverse document frequency will be higher number for words that are rare in the corpus

# visualise the word frequency with a wordcloud

fix(stop_words_cust)

write_csv(tidy_description,"tidy_description.csv")

############## Perform teh same tokenisation with bigrams ####################################


tidy_description_bi <- get_startups() %>%
  unest_tokens(bigram, Description, token = "ngrams", n = 2)%>%
  mutate(word = lemmatize_strings(bigram))%>%
  separate(bigram, c("word1", "word2"), sep = " ")%>%
  filter(!word1 %in% stop_words_cust$word) %>%
  filter(!word2 %in% stop_words_cust$word)%>%
  unite(bigram, word1, word2, sep = " ")%>%
  group_by(Company_name, bigram) %>%
  count()%>%
  bind_tf_idf(bigram, Company_name, n)%>%
  arrange(Company_name)# When cleaning the bigrams from stop words/ common phrases we apply the "separate, filter, unite" logic. This means that we will split the bigram into 2 words and use
# the stop-words data frame to match and remove the stop words. once cleaned we will unite back the bigram.




## using bigrams seems to be more informative
## build a visualisation of the bigram occurancies

fix(tidy_description_bi)

############ Perform Topic Models of the company descriptions ###############

# the topic modelling will help us categorise th companies. For the topic modelling we will use Latent Dirichlet allocation (LDA)
# LDA is one of th most commonly used algorithms for topic modelling in text analysis. The algorithm is guided by 2 
# priciples - every document is a mixture of topics and every topic is a mixture of terms.
# on topic modelling https://towardsdatascience.com/latent-dirichlet-allocation-lda-9d1cd064ffa2
# the topic model package that contains the algorithm requires document term matrix.
# we can cast a one-token-per-row table into a DocumentTermMatrix with tidytext's cast_dtm().

description_topic<- tidy_description %>%
  cast_dfm(document = Company_name, term=word, value=n)%>%
  LDA(k = 8, control = list(seed = 1234))%>%    # choosing 6 topics here is a suggestion, experiment to find the rigt number
  tidy( matrix = "beta")%>%                     # examine per-topic-per-word probability, e.g. the probability for the term to be generated by this topic
  group_by(topic) %>%                           # organise he data for better visibility of your topics
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)%>%
  mutate(term = reorder_within(term, beta, topic)) %>%   # build a visualisation of the topics
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()


# We can examine the per-document-per-topic probabilities, called ?? ("gamma"), with the matrix = "gamma" argument to tidy()

description_topic_doc<- tidy_description %>%
  cast_dfm(document = Company_name, term=word, value=n)%>%
  LDA(k = 8, control = list(seed = 1234))%>%    # choosing 6 topics here is a suggestion, experiment to find the rigt number
  tidy( matrix = "gamma")%>%                     # examine  per-document-per-topic probability, e.g. the probability for the term to be generated by this topic
  group_by(topic) %>%                           # organise he data for better visibility of your topics
  top_n(15, gamma) %>%
  ungroup() %>%
  arrange(topic, -gamma)%>%
  mutate(term = reorder_within(document, gamma, topic)) %>%   # build a visualisation of the topics
  ggplot(aes(gamma,document, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()


description_topic_bi<- tidy_description_bi %>%
  cast_dfm(document = Company_name, term=bigram, value=n)%>%
  LDA(k = 6, control = list(seed = 1234))%>%    # choosing 6 topics here is a suggestion, experiment to find the rigt number
  tidy( matrix = "beta")%>%                     # examine per-topic-per-word probability, e.g. the probability for the term to be generated by this topic
  group_by(topic) %>%                           # organse he data for better visibility of your topics
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)%>%
  mutate(term = reorder_within(term, beta, topic)) %>%   # build a visualisation of the topics
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()


# now we can use the LDA() function to create a 6 topic model. the 6 topic model is just a guess



# we will try to narrow a little the number of unique words by applying crude stemming mechanism utilising the Porter algorithm 
# https://nlp.stanford.edu/IR-book/html/htmledition/stemming-and-lemmatization-1.html ;
# the function we will use is the dfm_wordstem() from the quanteda package


# ncol(description_dtm ) #698

# description_dtm <- description_dtm %>%
#  dfm_wordstem()

# ncol(description_dtm ) # 598 we managed to narrow the number of words by 100


## 2. Visualise the words frequency  and build a graph  


# get an idea of the most used words by sorting the data according to 
# its rank, the higher the rank, the less used is the word

textstat_frequency(description_dtm)%>%
  arrange(rank)


features_dfm<- textstat_frequency(description_dtm, n=200)

# Sort by reverse frequency order

features_dfm$feature <- with(features_dfm, reorder(feature, -frequency))

ggplot(features_dfm, aes(x = feature, y = frequency)) +
  geom_point() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



# plot the 50 most common words

textplot_wordcloud(description_dtm, min_count = 2,
                   color = brewer.pal(5,"Accent"))

## visualise the bigram graph

# - build a tf-idf of bigrams per company to find out which bigrams define the companies the most
# - more info on tf-idf https://cran.r-project.org/web/packages/tidytext/vignettes/tf_idf.html 


tidy_description_bi_tf_idf <- tidy_description_bi %>%
  count(Company_name, bigram) %>%
  bind_tf_idf(bigram, Company_name, n) %>%
  arrange(desc(tf_idf))

# apart from the tf-idf (getting the relationships of the top word configurations), we can also visualiwse the relationships among all of the words simultaneously
# first we will arrange the words into a network as a combination of connected nodes.In R the function for building this network refers too it as a graph.
# a graph can be build out of any tidy object as far as we have 3 variables:
# from: the node an edge is coming from
# to: the node an edge is going towards
# weight: A numeric value associated with each edge


tidy_description_bi_graph <- get_startups() %>%
  unnest_tokens(bigram, Description, token = "ngrams", n = 2)%>%
  separate(bigram, c("word1", "word2"), sep = " ")%>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  count(word1, word2, sort = TRUE)%>%
  filter(n > 1) %>%
  graph_from_data_frame()



set.seed(123)

ggraph(tidy_description_bi_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)


  



