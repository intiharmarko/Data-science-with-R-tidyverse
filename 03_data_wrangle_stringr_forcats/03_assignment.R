# 3 Assignment - Data Wrangle: strings - factors (stringr & forcats)


rm(list = ls())
graphics.off()


# Libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(tibble)
library(stringr)
library(forcats)


# Data
corpus <- readLines("./data/corpus.txt")

#-------------------------------------------------------------------#
# If your PC takes too much time when running code below,
# maybe you should use a sample of given corpus?

# Un-comment two lines below - to get corpus sample!
#sample.size <- .05 # % of corpus lines sampled
#corpus <- sample(x = corpus, size = length(corpus) * sample.size, replace = F)
#-------------------------------------------------------------------#


# Exercise 1

# Do some basic inspection of corpus:

#  - check number of lines
length(corpus)

#  - check number of characters
str_length(corpus) %>%  # per each row
  sum(.)                # all together

#  - inspect first and last 6 lines
corpus %>% head()
corpus %>% tail()


# Do some basic matching and string manipulations

# - count how many lines include at least one punctuation
corpus %>% str_detect(pattern = "[:punct:]") %>% sum()

# - show first 20 lines without any punctuation
corpus %>% str_subset(pattern = "[:punct:]", negate = T) %>% head(20)

# - count how many lines include at least one number / digit
corpus %>% str_detect(pattern = "[:digit:]") %>% sum()

# - inspect first 10 lines with digit present
corpus %>% str_subset(pattern = "[:digit:]") %>% head(10) %>% str_view_all("[:digit:]")

# - find string patterns that resemble phone numbers
#   search for patterns: ddd-dddd where d = digit 0-9
pattern <- "\\d\\d\\d-\\d\\d\\d\\d"
corpus %>% str_subset(pattern = pattern) %>% str_view_all(pattern)

pattern <- "\\d{3}-\\d{4}"
corpus %>% str_subset(pattern = pattern) %>% str_view_all(pattern)
corpus %>% str_subset(pattern = pattern) %>% length() # in how many lines pattern appears

pattern <- "[:digit:]{3}-[:digit:]{4}"
corpus %>% str_subset(pattern = pattern) %>% str_view_all(pattern)

# - find string patterns that resemble dollar signs "$" (escaping needed)
pattern <- "\\$"
corpus %>% str_subset(pattern = pattern) %>% str_view_all(pattern)

# - how many lines starts with word "The"?
pattern <- "^The"
corpus %>% str_subset(pattern = pattern) %>% str_view_all(pattern)
corpus %>% str_subset(pattern = pattern) %>% length()



# Exercise 2

# - use corpus and try to figure out which words usually come before comma ","
pattern <- "\\w+," # {word},  =  {word character appears one time or more} + {comma ,}  
corpus %>% str_subset(pattern = pattern) %>% str_view_all(pattern)

counts <- corpus %>%  # frequency counts
  str_extract(pattern = pattern) %>% # find patterns
  str_to_lower(.) %>%           # convert to lower case
  str_remove(pattern = ",") %>% # remove comma
  tibble(pattern = .) %>%       # create tibble
  filter(!is.na(pattern)) %>%   # remove NAs
  count(pattern) %>%            # count frequencies - occurrence
  arrange(desc(n))


# - if you consider first 5 letters at the beginning of each line, what are the top patterns
#   that lines start with?
corpus %>% 
  str_to_lower(.) %>% 
  str_sub(start = 1, end = 5) %>% 
  tibble(pattern = .) %>% 
  count(pattern) %>% 
  arrange(desc(n))


# - find words where: 
#   a) a vowel is followed by a vowel
#   b) a vowel is followed by two or more vowels
#   c) 2 vowels are not followed by a vowel
vowels <- "a|e|i|o|u"

pattern_a <- paste0("(", vowels, ")", "(?=", "(", vowels, ")", ")")
corpus %>% str_to_lower(.) %>% str_subset(pattern = pattern_a) %>% str_view_all(pattern_a)

pattern_b <- paste0("(", vowels, ")", "(?=", "(", vowels, ")", "{2,}", ")")
corpus %>% str_to_lower(.) %>% str_subset(pattern = pattern_b) %>% str_view_all(pattern_b)

pattern_c <- paste0("(", vowels, ")", "{2,}", "(?!", "(", vowels, ")", ")")
corpus %>% str_to_lower(.) %>% str_subset(pattern = pattern_c) %>% str_view_all(pattern_c)


# - check occurrence of words "the", "be", "to", "of", "and" ~ most common words
# - sort words by their frequency
# - before pattern match convert corpus to lower case

corpus.low <- corpus %>% str_to_lower() # convert to lower

# function for counting occurrence
fcount <- function(word){
  return(corpus.low %>% str_count(pattern = word) %>% sum(.))
}

most.common.words <- tribble(
  ~word,   ~count,
  
  "the",   fcount("the"),  
  "be",    fcount("be"),
  "to",    fcount("to"),
  "of",    fcount("of"),
  "and",   fcount("and")
) %>% 
  arrange(desc(count))


# - for top 3 most common words check:
#    a) number of lines only one word is present
#    b) number of lines 2 words are present
#    c) number of lines all three words are present
#    also add percentage % of lines for each scenario!

tibble(text = corpus.low) %>%  # create tibble with flags which word is present
  mutate(the = str_detect(text, pattern = "the"),
         be = str_detect(text, pattern = "to"),
         to = str_detect(text, pattern = "and"),
         `how many present?` = the + be + to) %>% # how many present
  group_by(`how many present?`, the, be , to) %>% # count all possible scenarios
  summarise(`nr lines` = n()) %>% 
  ungroup() %>% 
  mutate(`% of lines` = round(`nr lines` / sum(`nr lines`) * 100, 1)) %>%  # add percentages
  arrange(desc(`how many present?`))



# Exercise 3

# clean our corpus (corpus.clean):
#  - convert all characters to lower case
#  - remove punctuation(s)
#  - remove extra white spaces (more than one white space)
#  - replace tabs or new lines with a white space
#  - remove all digits
corpus.clean <- corpus %>% 
  str_to_lower(.) %>%     # to lower-case
  str_remove_all(pattern = "[:punct:]") %>% # remove punctuation(s)
  str_remove_all(pattern = "[:digit:]") %>% # remove digits
  str_replace_all(pattern = "\\t|\\n", replacement = " ") %>% # replace tabs & new lines
  str_trim(side = "both") %>%  # trim white spaces from both sides
  str_replace_all(pattern = "\\s{2,}", replacement = " ") # replace multiple white spaces (more than one) with single white space

# now use clean corpus and create:
#  - a table called "corpus.words" with 2 columns
#  - first column: "word"   - word from corpus
#  - second column: "count" - frequency occurrence count in corpus

# - DEMONSTRATION: how to collapse lines into single line and then collapse single line into one vector of words
corpus.clean %>% 
  head(3) %>%    # just select first 3 lines
  str_c(sep = " ", collapse = " ") %>%  # collapse line into a single line
  str_split(pattern = " ") %>% # split line by white spaces into list of words
  unlist() # convert list to vector

# - now for real  
corpus.words <- corpus.clean %>% 
  str_c(sep = " ", collapse = " ") %>%  # collapse line into a single line
  str_split(pattern = " ") %>% # split line by white spaces into list of words
  unlist() %>%  # convert list to vector
  tibble(word = .) %>%  # create tibble
  # create word counts
  group_by(word) %>% 
  summarise(count = n()) %>% 
  ungroup() %>% 
  arrange(desc(count))
  

# now add column "% coverage"
#  - column tells the percentage of text in corpus covered by given word
#  - % coverage = count / sum(count) * 100
corpus.words <- corpus.words %>% 
  mutate(`% coverage` = count / sum(count) * 100)

# now try to answer following questions:

# - how many different words were found in corpus
corpus.words %>% nrow()

# - how much text is covered with the most frequent word
corpus.words %>% slice(1)

# - how much text is covered with the top 10 most frequent words
corpus.words %>% slice(1:10) %>% pull(3) %>% sum(.)

# - how many words we need to cover 50% or 70% or 90% of corpus?
corpus.words <- corpus.words %>% 
  mutate(`% coverage cumsum` = cumsum(`% coverage`)) # add running total

corpus.words %>% filter(`% coverage cumsum` <= 50) %>% nrow() # 50%
corpus.words %>% filter(`% coverage cumsum` <= 70) %>% nrow() # 70%
corpus.words %>% filter(`% coverage cumsum` <= 90) %>% nrow() # 90%

# ggplot for visualizing coverage
corpus.words %>% 
  mutate(position = row_number()) %>% # add word position
  ggplot(aes(x = position,
             y = `% coverage cumsum`)) +
  geom_area(color = "black", fill = "gray80") +
  scale_x_continuous(breaks = seq(0,100000,5000)) +
  scale_y_continuous(breaks = seq(0,100,10)) +
  xlab("Word count") +
  ylab("Text-corpus % covered")


# Exercise 4

# we will create another table:
#  - first do word / lines sampling out of top 100 most frequent words in corpus
#  - repeat sampling 10000 times
#  - add columns prob = count / sum(count)   / probability for sampling a line / word
#  - for sampling use sample_n : replace = T and use probability in weight argument
#  - at the end only keep column word
#  - name the table corpus.words.top100
set.seed(567)

corpus.words.top100 <- corpus.words %>% 
  .[1:100,] %>% # keep only first 100 words
  mutate(prob = count / sum(count)) %>% # add probability column
  sample_n(tbl = ., size = 10000, replace = T, weight = prob) %>% # sampling
  select(word)

# convert word to factor variable  
corpus.words.top100 <- corpus.words.top100 %>% 
  mutate(word = as.factor(word))

# extract unique factor levels
corpus.words.top100 %>% pull(word) %>% fct_unique()

# count factor occurrence
corpus.words.top100 %>% pull(word) %>% fct_count()

