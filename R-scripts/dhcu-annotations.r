# so we get the package that lets us interface with the hypothesis API
devtools::install_github("mdlincoln/hypothesisr")

# then we switch it on
library(hypothesisr)

# let's create a variable to hold the annotaitons on the course website
dhcu_annotations <- hs_search_all( custom = list(uri.parts = "dhcu"))

# we can see how stuffed that variable is: this command will tell you how many rows, how many columns
dim(dhcu_annotations)

# you can view all of that rich data like this:
View(dhcu_annotations)

# so now lets do some more cool stuff. Remember, if you haven't already got
# these packages installed, you install with
# install.packages("name of the package goes here")
library(dplyr)
library(magrittr)
library(purrr)

# Display the top 5 most-annotated pages under this domain
slice(count(dhcu_annotations, uri, sort = TRUE), 1:5)

# so now let's go get *all* the annotations
# the line below ought to work, but doesnt'. I'm working on that.
dhcu_annotations2 <- hs_search_all(custom = list(group = "9ZoPkNzN"))

# trying something else
# going to have to just load things in from the csv from jonudell's page i guess
# download as csv, put column headers in, delete empty column.

dhcu_annotations2 <- read.csv("/Users/shawngraham/dhcu-teaching/annotations.csv")

# remember how we viewed all of the columns? If you want to see one particular column, you
# add the $columnname to the end of the variable, like so:

dhcu_annotations2$text
dhcu_annotations2$user

dim(dhcu_annotations2)

# So let's write all of the text to a csv file which you can then load into
# other tools like the topic modeling tool, voyant, or antconc etc:
library(readr)
# get rid of list cols, since we can't write those to csv
list_cols <- map_lgl(dhcu_annotations2, ~is.list(.x))
dhcu_annotations2_nolist <- dhcu_annotations2[, !list_cols]
write_csv(dhcu_annotations2_nolist, "dhcu_annotations2.csv")
#############


# let's make a graph. First, let's count the number of annotations made
# we create a new variable, and feed it the table command from the $user column


counts <- table(dhcu_annotations2_nolist$user)


# this line will make a bar chart, but it looks awful
barplot(counts, main="Prolific Annotators", xlab="Number of Annotations")

# so let's redo it, and we'll use the ggplot2 library (google this for options)
# but in our case, we're going to reorder the bars from most to least
library(ggplot2)
ggplot(as_data_frame(counts),
       aes(reorder(Var1, n),
           n)) +
  geom_col() +
  coord_flip() +
  theme_bw()

# and now we'll do a topic model using the tidy approach (which isn't in module 4)
library(tidytext)
library(tidyverse)



documents <- read_csv("dhcu_annotations2.csv")


# we need to break up the text into lists of words. The %>% is a 'pipe', which feeds the results of one command into the next one

word_counts <- documents %>%
  group_by(user) %>%
  unnest_tokens(word, text) %>% 
  count(user, word, sort = TRUE) %>%
  ungroup()

# we're going to define a list of common words that occlude 

custom_stop_words <- c(stop_words$word, 
                       "craftingdigitalhistory.ca",
                       "workbook.craftingdigitalhistory.ca",
                       "https",
                       "github",
                       "http",
                       "gt",
                       "git",
                       "lt",
                       "1")

# and now we're going to add that list to the stopwords
hist_words <- data_frame(word = custom_stop_words,
                         lexicon = c(stop_words$lexicon,
                                     rep(NA, length(custom_stop_words) - length(stop_words$word))))


# and now we're going to remove the stopwords from our words
word_counts <- word_counts %>%
  anti_join(hist_words)

# take a look at the words... you might spot things that you think should be added to our custom list. Go ahead, and then redo.
word_counts

# make document term matrix
dtm <- word_counts %>%
  cast_dtm(user, word, n)

# make topic model
library(topicmodels)
# how many topics to find?
k <- 6
username_lda <- LDA(dtm, k = k, control = list(seed = 1234))
username_lda

# tidy form
username_lda_td <- tidy(username_lda)
username_lda_td

# top five word per topic
top_terms <- username_lda_td %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms

# now let's make a really nifty chart
library(ggplot2)
theme_set(theme_bw())

top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ topic, scales = "free") +
  theme(axis.text.x = element_text(size = 8, angle = 90, hjust = 1))

# let's measure the sentiment of the words, using the 'afinn' mapping of word sentiment
#sentiment
tt_s <- word_counts %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  group_by(word) %>%
  summarize(occurences = n(),
            contribution = sum(score))

# run the next line, and you'll see that 'abandoned' scores -2 while 'abilities' scores 6...
tt_s

# let's chart the top 25 words and their contribution to positive/negative sentiment in the annotations
tt_s %>%
  top_n(40, abs(contribution)) %>%
  mutate(word = reorder(word, contribution)) %>%
  ggplot(aes(word, contribution, fill = contribution > 0)) +
  geom_col(show.legend = FALSE) +
  coord_flip()

# who's the most positive?
sentiment_messages <- word_counts %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  group_by(user) %>%
  summarize(sentiment = mean(score),
            words = n()) %>%
  ungroup() %>%
  filter(words >= 5) # filters out messages that have fewer words than this that contribute to sentinment score


#now see the most positive messages/ scores closer to 1 most positive, according to affin
sentiment_messages %>%
  arrange(desc(sentiment))

# tf-idf
# now let's do term-frequency inverse distribution frequencey (this is what overviewdocs.com does btw)
tf_idf <- word_counts %>%
  bind_tf_idf(word, user, n) %>%
  arrange(desc(tf_idf))

# so we can see which words are most characteristic of a particular author
tf_idf

# now, we graph! you might have to hit the 'zoom' button in order to see the resulting chart more clearly.
library(stringr)

tf_idf %>%
  group_by(user) %>%
  top_n(4, tf_idf) %>%
  ungroup() %>%
  mutate(word = reorder(word, tf_idf)) %>%
  ggplot(aes(word, tf_idf, fill = user)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ user, scales = "free") +
  ylab("tf-idf") +
  coord_flip()

# google 'text analysis tidyverse for more things you might do.

# plot for topics in all docs by a subset of authors
username_lda_gamma <- tidy(username_lda, matrix = "gamma")

username_lda_gamma %>% 
  left_join(documents, by = c('document' = 'user')) %>% 
  # get authors with at least ten documents
  semi_join(., documents %>% count(user) %>% filter(n > 20), 
            by=  c('document' = 'user')) %>% 
  mutate(doc = 1:nrow(.)) %>% 
  ggplot(aes(doc, gamma, fill = as.character(topic))) +
  geom_col() +
  theme(axis.text.x = element_text(size = 10, angle = 90, hjust = 1)) +
  facet_wrap( ~ document, scales = "free_y")
