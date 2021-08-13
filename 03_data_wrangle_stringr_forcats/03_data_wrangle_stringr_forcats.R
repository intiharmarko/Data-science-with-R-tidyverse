# 3 Data Wrangle: strings - factors (stringr & forcats)

rm(list = ls())
graphics.off()

# Install packages and load package
install.packages("stringr")
install.packages("forcats")     

library(dplyr)
library(tidyr)
library(ggplot2)
library(tibble)
library(stringr)
library(forcats)


# 3.1 Strings inside tidyverse

# Create a string
s1 <- "Double quotes string"
s1

s2 <- 'Single quotes string'
s2

s3 <- "Double quotes outside, 'single quotes inside' a string"
s3

s4 <- 'Single quotes outside, "double quotes inside" a string'
s4

# Be careful of mixing quotes (hit ESC when R is stuck - code below!)
# Not possible!
s5 <- "Double quotes inside "double" quotes"
s6 <- 'Single quotes inside 'single' quotes'
# s7 <-  "not "working" 
# s8 <-  'not 'working'
# s9 <- "Missing closing quote


# Create a vector of strings
vec <- c("a", "b", "c")
vec  


# Character vector inside a tibble
df <- tibble(letters = vec)
df  


# Special characters
# How to escape a character ~ Regular expressions

#   Literal single or double quotes
"\""   # escape a special charatcer with back slash - \
'\''

#   Some other special characters (will be seen later at regular expressions)

#   New line
"\n"

#   Tabulator
"\t"

#   Unicode non-english characters
"\u03B1"

# See raw content of the string (omiited escape characters and outside quotes)
s <- "string"
s
writeLines(s)

s <- "\""
s
writeLines(s)

s <- "line 1 \nline 2"
s
writeLines(s)

rm(list =ls())

# 3.2 Strings matching

# load strings dataset
load("./data/strings.RData")
ls()

# str_detect() - Detect s pattern
# similar base R: grepl()

#   Find a fruit containing letter "a" (anywhere in word)
fruit
ind <- str_detect(string = fruit, pattern = "a") # returns TRUE / FALSE
fruit[ind]

fruit[grepl(pattern = "a", x = fruit)]

#   Find a fruit not containing any letter "a" !
#   we use negation
fruit[str_detect(fruit, "a", negate = T)]
fruit[!str_detect(fruit, "a")]

#   Inside tibble add flag if fruit contains letter "a" or if it doesn't contain letter "a"
fruit.df %>% 
  mutate(flag = case_when(str_detect(fruit, pattern = "a") ~ "contains 'a'",
                          T ~ "does not contain 'a'"))

#   Find fruits starting or ending with letter "a"
ind.start.a <- str_detect(fruit, pattern = "^a")
fruit[ind.start.a]

ind.end.a <- str_detect(fruit, pattern = "a$")
fruit[ind.end.a]


# str_which() - Detect s pattern return index position

# similar base R: grep()
#   Find a fruit containing letter "a" (anywhere in word)
ind <- str_which(string = fruit, pattern = "a") # returns index position
fruit[ind]

fruit[grep(pattern = "a", x = fruit)]


# str_count() - Count number of pattern matches in string

#   Add count of letter "a" in each fruit (use table)
fruit.df1 <- fruit.df %>% 
  mutate(`count a` = str_count(fruit, pattern = "a"))

#   Show counts of letter "a" in fruits
fruit.df1 %>% 
  count(`count a`)

#   Show fruit with 3 "a" letters
fruit.df1 %>% 
  filter(`count a` == 3)


# str_locate() / str_locate_all() - Locate position(s) of pattern match in string

#   Locate position of first letter "a" in each fruit (matrix is returned)
str_locate(fruit, pattern = "a") 

fruit.df1 <- str_locate(fruit, pattern = "a") %>% 
  as_tibble() %>% # convert matrix of positions to tibble
  mutate(fruit = fruit) %>% # add fruit name column
  select(fruit, start, end) # re-arrange columns

#   Locate position of all letters "a" in each fruit (list is returned)
str_locate_all(fruit, pattern = "a") 



# 3.3 Strings subsetting

# str_sub() - Extract part of s string
# similar base R: substr()

#   Extract first 3 letters of a fruit
str_sub(string = fruit, start = 1, end = 3)
substr(x = fruit, start = 1, stop = 3)

#   Extract first letter of common word and count word frequency by first word letter
words.df %>% 
  mutate(`first letter` = str_sub(word, 1, 1)) %>% # extract first letter
  count(`first letter`) %>% # count frequencies
  arrange(desc(n)) # sort from high to low frequency

#   Extract middle part of the word
str_sub(fruit, start = 3, end = 5) # from 3rd to 5th letter

#   Extract last letter / last 3 letters (use negative counters - for counting backward)
str_sub(fruit, start = -1, end = -1) # last letter
str_sub(fruit, start = -3, end = -1) # last 3 letters


# str_subset() - Return only strings that match pattern

#   Return fruit containing letter "c"
str_subset(string = fruit, pattern = "c")

#   Return fruit starting with letter "c"
str_subset(string = fruit, pattern = "^c")


# str_extract() / str_extract_all() - Return first or every pattern match

#   Return fruit containing "a" first occurence
str_extract(string = fruit, pattern = "a")  # vector is returned

#   Return fruit containing "a" all occurences
str_extract_all(string = fruit, pattern = "a") #  list is returned


# str_match() / str_match_all() - Return first or every pattern match (as a matrx)

#   Return fruit containing "a" first occurence
str_match(string = fruit, pattern = "a")  # matrix is returned

#   Return fruit containing "a" all occurences
str_match_all(string = fruit, pattern = "a") #  matrix inside list is returned



# 3.4 String lengths

# str_length() - Width of a string
# similar base R: nchar()
str_length("word")
nchar("word")

#   Find all fruits with length 10 or more characters
fruit[str_length(fruit) >= 10]


# str_pad() - String padding

#   Pad fruit names with symbol "X" to get a string with width = 20
str_pad(string = fruit, width = 20, side = "left", pad = "X")  # left side padding
str_pad(string = fruit, width = 20, side = "right", pad = "X") # right side padding
str_pad(string = fruit, width = 20, side = "both", pad = "X")  # both side padding

#   Where padding is very useful in practice (ID numbers)
set.seed(123)
id.numbers <- sample(x = 1:1000, size = 25, replace = F) # generate some ID numbers
id.numbers
str_pad(id.numbers, width = 5, side = "left", pad = "0") # add leading zeros


# str_trunc() - String truncating

#   Truncate fruit names with symbol "..." to get a string with width = 5
str_trunc(string = fruit, width = 5, side = "left", ellipsis = "...")   # left side truncating
str_trunc(string = fruit, width = 5, side = "right", ellipsis = "...")  # right side truncating
str_trunc(string = fruit, width = 5, side = "center", ellipsis = "...") # center side truncating


# str_trim() - Trim whitespaces

#   Create a string with white spaces
whitespace <- c("nospaces",
                " leftspace",
                "      leftspaces",
                "rightspace ",
                "rightspaces     ",
                " bothspace ",
                "   bothspaces   ",
                "middle space",
                " mix space ")
whitespace

#   Trim left white space(s)
whitespace.trim.left <- str_trim(string = whitespace, side = "left")
whitespace
whitespace.trim.left

#   Trim right white space(s)
whitespace.trim.right <- str_trim(string = whitespace, side = "right")
whitespace
whitespace.trim.right


#   Trim both side white space(s)
whitespace.trim.both <- str_trim(string = whitespace, side = "both")
whitespace
whitespace.trim.both



# 3.5 Strings mutating


# str_sub() - Replace a part of given string
#   Replace first 3 letters of each fruit with string "FRU"
fruit
fruit.sub <- fruit
fruit.sub
str_sub(fruit.sub, start = 1, end = 3) <- "FRU"
fruit.sub


# str_replace() - Replace the first matched pattern in a string
#   Replace first occurence of letter "a" with "A" in each fruit
str_replace(string = fruit, pattern = "a", replacement = "A")


# str_replace_all() - Replace all matched patterns in a string
#   Replace all occurences of letter "a" with "A" in each fruit
str_replace_all(string = fruit, pattern = "a", replacement = "A")


# str_to_lower() - Convert string to lower case
string.upper <- "THIS IS A STRING"
string.lower <- str_to_lower(string = string.upper)
string.lower


# str_to_upper() - Convert string to upper case
string.upper <- str_to_lower(string = string.lower)
string.upper


# str_to_title() - Convert string to "upper"title" case
string.title <- str_to_title(string = string.lower)
string.title



# 3.6 Joining and splitting strings

# str_c() - Join multiple strings into a single string
#   Let's split vector "fruit" into 4 equal in size smaller vectors
fruit1 <- fruit[1:20]
fruit2 <- fruit[21:40]
fruit3 <- fruit[41:60]
fruit4 <- fruit[61:80]

#   Create one vector of strings using all 4 smaller vectors
str_c(fruit1, fruit2, fruit3, fruit4, sep = "-")

#   Create vector of alphabet letters: one lower and one upper case
letters
Letters
str_c(letters, Letters)


# str_c() - Colapse a vector of strings into  single string
#   Collapse a vector of letters into a single string containing all letters
str_c(letters, collapse = "")
str_c(letters, collapse = " ")


# str_dup() - Repeat a string multiple times

#   Repeat one string 5 times
str_dup(string = "string", times = 5)

#   Repeat a vector of strings 2 times
str_dup(string = fruit1, times = 2)


# str_split_fixed() - Split a vector of strings into a matrix of substrings base on pattern

#   Split fruit by " " white space
str_split_fixed(string = fruit, pattern = " ", n = 2) # n - number of pieces to return!

#   Split first 5 sentences by " " white space - increase n
str_split_fixed(sentences[1:5], pattern = " ", n = 10) 


# str_split() - Split a vector of strings into a list / matrix of substrings base on pattern

#   Split first 5 sentences by " " white space
str_split(sentences[1:5], pattern = " ") # return a list
str_split(sentences[1:5], pattern = " ", simplify = T) # return a matrix


# str_glue() - Glue/merge together string and expression

#   Merge string and evaluated mathematical symbol   
str_glue("What is the value of sqrt(2), it is {sqrt(2)}.")

#   Merge fixed string and assigned string to a variable
name <- "Marko"
str_glue("Hi my name is {name}")


# str_glue_data() - Use data.frame / list or environment to create strings from string and expression

#   Merge string and values from a data.frame
mtcars
str_glue_data(mtcars, "The car {rownames(mtcars)}: {hp} horsepower, {cyl} number of cylinders and consumption {mpg} miles per gallon")



# 3.7 Other string helper functions

# str_order() - Return a vector of indexes after character vector is sorted.

#   Let's first shuffle fruits (to get random order)
set.seed(456)
fruit.shuf <- sample(x = fruit, size = length(fruit), replace = F)
fruit.shuf

#   Now get order index and use to sort shuffled fruits
str_order(x = fruit.shuf) # get index
fruit.shuf[str_order(x = fruit.shuf)] # use it for sort


# str_sort() - Sort character vector

#   Let' sort shuffled fruits
str_sort(x = fruit.shuf)
str_sort(x = fruit.shuf, decreasing = T)

#   Sorting numbers stored as strings!
set.seed(567)
numbers.s <- sample(1:250, size = 20, replace = F) # generate some numbers
numbers.s <- as.character(numbers.s) # conver numbers to charatcer
numbers.s

str_sort(numbers.s) # not sorted as numbers but as strings
str_sort(numbers.s, numeric = T) # sorted as numbers 


# str_view() / str_view_all() - Useful HTML rendering function

# very useful in the context of regular expressions 
# (given context will be shown later)

#   View first match
str_view(string = fruit, pattern = "a") # displays all
str_view(string = fruit, pattern = "a", match = T) # display only matched
str_view(string = fruit, pattern = "^a", match = T)

#   View all matches
str_view_all(string = fruit, pattern = "a", match = T) 



# 3.8 Regular expressions (regex)

# Get list of some special characters
?"'"

# Escaping paradox
string <- c("string", "word", "letter", "word.letter", "character/letter") 

#   Match "tr"
str_view(string, "tr")

#   Match ".t." - any character before t and any character after t
str_view(string, ".t.")
str_view_all(string, ".t.")


#   Match "." as a dot not as a metacharacter meaning:

#   1) wrong way: since . is interpreted as metacharater ~ any charatcer
str_view(string, ".")
#   2) wrong way (single backslash \ ): escaping is applied on . but \ is not escaped!
str_view(string, "\.")
#   3) correct way (double backslash \\ ): escaping is applied on . and \ !
str_view(string, "\\.")


#   Match "\" as a backslash character not as a metacharacter meaning:

writeLines("\\") # \ must be escaped when written as a string

str_view("\\", "\\\\") # double escaping is applied int he pattern ~ four \ in total at the end! 



# 3.9 Regex: Special characters & Classes

# Digits VS non-digits
string <- c(letters, "123", "1-5-6", "598642")
string

#   Find strings with digits
str_subset(string, "\\d")
str_view_all(string, "\\d", match = T)

#   Find strings without digits
str_subset(string, "\\D")
str_view_all(string, "\\D", match = T)

#   Strings with pattern "digit-digit-digit"
str_subset(string, "\\d-\\d-\\d")
str_view_all(string, "\\d-\\d-\\d", match = T)

#   Locate whitespace(s)
set.seed(123)
string <- c(sample(sentences, 5), 
            sample(fruit, 5),
            sample(words, 5),
            "This is \nnewline",
            "String with a tab \t")
string
writeLines(string)

str_subset(string, "\\s") # only strings with white spaces
str_view_all(string, "\\s")

#   Locate string with new lines or tabs
str_subset(string, "\\n") # only strings with new lines
str_subset(string, "\\t") # only strings with tabs


# Different classes
string <- c("123abc", "abc", "123", ".,?", "ABC", "\nABC", "\tabc")
string

#   Strings with digits
str_subset(string, "[:digit:]")
str_view_all(string, "[:digit:]", match = T)

#   Strings with letters
str_subset(string, "[:alpha:]")
str_view_all(string, "[:alpha:]", match = T)

#   Strings with upper / lower case letters
str_subset(string, "[:lower:]")
str_view_all(string, "[:lower:]", match = T)
str_subset(string, "[:upper:]")
str_view_all(string, "[:upper:]", match = T)

#   Strings with letters or numbers
str_subset(string, "[:alnum:]")
str_view_all(string, "[:alnum:]", match = T)

#   Strings with punctuation
str_subset(string, "[:punct:]")
str_view_all(string, "[:punct:]", match = T)

#   Strings with letters, numbers or punctuation
str_subset(string, "[:graph:]")
str_view_all(string, "[:graph:]", match = T)

#   Strings with space characters
str_subset(string, "[:blank:]")
str_view_all(string, "[:blank:]", match = T)



# 3.10 Regex: Alternates, anchors & groups

# Anchors

#   Find a word starting with letter "a"
str_subset(words, "^a")
str_view_all(words, "^a", match = T)

#   Find a word ending with letter "a"
str_subset(words, "a$")
str_view_all(words, "a$", match = T)

#   Find exact word using ^....$
str_subset(words, "^actor$")
str_view_all(words, "^actor$", match = T)
str_subset(fruit, "^lemon$")
str_view_all(fruit, "^lemon$", match = T)


# Alternates

#   Find words that starts with "af" or "ag"
str_subset(words, "^af|^ag")
str_view_all(words, "^af|^ag", match = T)

#   Find words containing letters "x" or "y" or "z"
str_subset(words, "[xyz]")

#   Find words not containing letters from "a" to "x"
str_subset(words %>% str_to_lower(), "[^[a-y]]")
str_view_all(words %>% str_to_lower(), "[^[a-y]]", match = T)

#   Find all country names beginning with letter "A" or "E"
str_subset(countries, "^A|^E")

#   Find all country names ending with letter "a" or "e"
str_subset(countries, "a$|e$")


# Groups

#   Find all sentences that include words: "the", "a" or "an"
str_subset(sentences, "(\\sthe\\s|\\sa\\s|\\san\\s)")
str_view_all(sentences, "(\\sthe\\s|\\sa\\s|\\san\\s)", match = T)

#   Find words with repeated pair of letters (two letters must be repeated): use back references
str_subset(words, "(..)\\1")   # \1 is a group reference 1st group, double backslash ~ escaping
str_view_all(words, "(..)\\1", match = T)

str_subset(fruit, "(..)\\1")   
str_view_all(fruit, "(..)\\1", match = T)

#   Mor ethan one greoup in back reference
string <- c("abc", "abcabc", "ababcc", "abababccc")
string
str_view_all(string, "(a)(b)", match = T)             # ab
str_view_all(string, "(a)(b)\\1", match = T)          # aba
str_view_all(string, "(a)(b)\\1\\2", match = T)       # abab
str_view_all(string, "(a)(b)\\1\\2\\1\\2", match = T) # ababab



# 3.11 Regex: Look arounds & quantifiers

# Look arounds

#   Find a word where letter "w" is followed by letter "a"
str_subset(words, "w(?=a)")   
str_view_all(words, "w(?=a)", match = T)

#   Find a word where letter "w" is not followed by letter "a"
str_subset(words, "w(?!a)")   
str_view_all(words, "w(?!a)", match = T)

#   Find a word where letter "a" is preceded by letter "w"
str_subset(words, "(?<=w)a")   
str_view_all(words, "(?<=w)a", match = T)

#   Find a word where letter "a" is not preceded by letter "w"
str_subset(words, "(?<!w)a")   
str_view_all(words, "(?<!w)a", match = T)


# Quantifiers
string <- " .A.AA.AAA.AAAA"

#   zero or one "A"
str_view_all(string, "A?")

#   zero or more "A"
str_view_all(string, "A*")

#   one or more "A"
str_view_all(string, "A+")

#   exactly 2 "A"
str_view_all(string, "A{2}")

#   2 or more "A"
str_view_all(string, "A{2,}")

#   between 2 and 3 "A"
str_view_all(string, "A{2,3}")


# Exercise with sentences
#   count the number of words in each sentence
#   first remove all punctuation and convert all to lower case
#   then count the number of words and show results
sentences.df1 <- sentences.df %>% 
  mutate(sentence = str_remove_all(sentence, "[:punct:]"), # remove punctuation
         sentence = str_to_lower(sentence)) %>%  # convert to lower case
  mutate(`nr words` = str_count(string = sentence, "\\s+") + 1) # counts number of spaces between words, + 1 added to get number of words

sentences.df1 %>% count(`nr words`) # show frequencies

# Countries with more than 3 words in a country name
countries.df %>% 
  mutate(`nr words` = str_count(string = country, "\\s+") + 1) %>% 
  filter(`nr words` > 3)



# 3.13 Factors ~ forcats

# First lets' create a factor variables
df <- mpg %>% 
  mutate_at(.vars = c("manufacturer", "model", "trans", "class"), .funs = as_factor)
str(df)

# Check factor levels
df$manufacturer %>% levels()


# fct_count() - Count factor values

#   Check car manufacturer (frequencies count)
df %>% .$manufacturer %>%  fct_count()
df %>% count(manufacturer)

#   Let's visualize frequencies (ggplot2 section is coming later!)
df %>% 
  count(manufacturer) %>% 
  ggplot(aes(x = manufacturer,
             y = n)) +
  geom_col()


# fct_unique() - Extract unique levels

#   Car manufacturer unique levels
df %>% .$manufacturer %>% fct_unique() 
df %>% .$manufacturer %>% fct_unique() %>% as.character()



# 3.14 Factors combine and order levels

# fct_c() - Combine factors

#   First lets split cars into 2 data frames
manufacturers <- df %>% .$manufacturer %>% fct_unique() %>% as.character() # unique manufacturers

df1 <- df %>%  # first subset
  filter(manufacturer %in% manufacturers[1:8])
df2 <- df %>%  # second subset
  filter(manufacturer %in% manufacturers[9:15])

#   Extract only factor vectors
f1 <- df1 %>% pull(manufacturer)
f2 <- df2 %>% pull(manufacturer)

#   Combine factors
c(f1, f2)    # with classical vector bind we lose factor level labels!
fct_c(f1,f2) # This way levels are preserved!


# fct_relevel() - Manually reorder levels

#   Lets randomly shuffle levels - manufacturers
set.seed(123)
manufacturers.rnd <- sample(manufacturers, size = length(manufacturers), replace = F)

#   Now count frequencies &  create another bar plot with manually reordered levels
df %>% 
  mutate(manufacturer = fct_relevel(manufacturer, manufacturers.rnd)) %>% 
  count(manufacturer)

df %>% 
  mutate(manufacturer = fct_relevel(manufacturer, manufacturers.rnd)) %>% 
  count(manufacturer) %>% 
  ggplot(aes(x = manufacturer,
             y = n)) +
  geom_col()


# fct_infreq() - Levels by frequency

#   Order manufacturers based on car count
df %>% 
  mutate(manufacturer = fct_infreq(manufacturer)) %>% 
  count(manufacturer)

df %>% 
  mutate(manufacturer = fct_infreq(manufacturer)) %>% 
  count(manufacturer) %>% 
  ggplot(aes(x = manufacturer,
             y = n)) +
  geom_col()


# fct_inorder() - Levels by order of appearance

#   Order manufacturers based how they appear in the data
df %>% 
  mutate(manufacturer = fct_inorder(manufacturer)) %>% 
  count(manufacturer)

df %>% 
  mutate(manufacturer = fct_inorder(manufacturer)) %>% 
  count(manufacturer) %>% 
  ggplot(aes(x = manufacturer,
             y = n)) +
  geom_col()


# fct_rev() - Reverse level of order

#   Order manufacturers based on reverse appereance in the data
df %>% 
  mutate(manufacturer = fct_rev(manufacturer)) %>% 
  count(manufacturer)

df %>% 
  mutate(manufacturer = fct_rev(manufacturer)) %>% 
  count(manufacturer) %>% 
  ggplot(aes(x = manufacturer,
             y = n)) +
  geom_col()

#   To get reverse frequency count: fct_infreq + fct_rev
df %>% 
  mutate(manufacturer = fct_infreq(manufacturer),
         manufacturer = fct_rev(manufacturer)) %>% 
  count(manufacturer) %>% 
  ggplot(aes(x = manufacturer,
             y = n)) +
  geom_col()



# 3.15 Factors change levels value and add / drop levels

# fct_recode() - Manually change levels

#   First lets pull levels and add county of origin
df %>% pull(manufacturer) %>% fct_count()

levels.country <- tribble( # table of: company & country of origin
  ~company,    ~country,
   "audi",          "Germany",
   "chevrolet",     "USA",
   "dodge",         "USA",
   "ford",          "USA",
   "honda",         "Japan",
   "hyundai",       "South Korea",
   "jeep",          "USA",
   "land rover",    "England",
   "lincoln",       "USA",
   "mercury",       "USA",
   "nissan",       "Japan",
   "pontiac",       "USA",
   "subaru",        "Japan",
   "toyota",        "Japan",
   "volkswagen",    "Germany")

#   Prepare pairs for re-coding factor levels
levels.country %>% 
  mutate(recode = str_c(country, " = ", "'", company, "'", sep = "")) %>% 
  pull(recode) %>% 
  str_c(., collapse = ", ")

#   Now re-code factor levels: companies -> countries
df.recode <- df %>% 
  mutate(manufacturer = fct_recode(manufacturer, 
                                   Germany = 'audi', 
                                   USA = 'chevrolet', 
                                   USA = 'dodge', 
                                   USA = 'ford', 
                                   Japan = 'honda', 
                                   `South Korea` = 'hyundai', 
                                   USA = 'jeep', 
                                   England = 'land rover', 
                                   USA = 'lincoln', 
                                   USA = 'mercury', 
                                   Japan = 'nissan', 
                                   USA = 'pontiac', 
                                   Japan = 'subaru', 
                                   Japan = 'toyota', 
                                   Germany = 'volkswagen'))

#   Check recoded factor levels
df.recode %>% 
  count(manufacturer)


# fct_collapse() - Collapse levels into manually defined groups

#   Let's keep only USA manufacturers, others are collapsed
non.us.manufacturers <- levels.country %>% filter(country != "USA") %>% pull(company) # vector of non-US manufacturers

df.collapse <- df %>% 
  mutate(manufacturer = fct_collapse(manufacturer, `non US` = non.us.manufacturers))

#   Check collapsed factor levels
df.collapse %>% 
  count(manufacturer)


# fct_other() - Replace levels with other

#   All non-US companies -> Other
df.other <- df %>% 
  mutate(manufacturer = fct_other(manufacturer, drop = non.us.manufacturers))

#   Check other factor levels
df.other %>% 
  count(manufacturer)


# fct_drop() - Drop factor levels

#   Drop other level
#    - first filter out rows with "Other"
df.drop <- df.other %>% 
  filter(manufacturer != "Other")

#   Check levels - "Other" still present!
df.drop %>% pull(manufacturer) %>%  fct_unique() 

#   Now drop "Other" level with no more data  
df.drop <- df.drop %>% 
  mutate(manufacturer = fct_drop(manufacturer))

#   Check levels - "Other" removed!
df.drop %>% pull(manufacturer) %>%  fct_unique()


# fct_expand() - Add additional levels to factor

#   Lets add some additional manufacturers
df.expand <- df %>% 
  mutate(manufacturer = fct_expand(manufacturer, c("Ferrari", "Lamborghini")))

#   Check levels - "New levels added
df.expand %>% pull(manufacturer) %>% fct_unique()
df.expand %>% pull(manufacturer) %>% levels()

#   But at the moment there aren't any cars from "Ferrari" or "Lamborghini"
#   - just levels are prepared in advance
df.expand %>% filter(manufacturer %in% c("Ferrari", "Lamborghini"))

