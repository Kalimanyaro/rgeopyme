#ejercicios de texto


head(USArrests)
states = rownames(USArrests)
substr(x = states, start = 1, stop = 4)
states2 = abbreviate(states)
states2
names(states2) = NULL

# size (in characters) of each name
state_chars = nchar(states)
# longest name
states[which(state_chars == max(state_chars))]
# get states names with 'k'
grep(pattern = "k", x = states, value = TRUE)
grep(pattern = "[wW]", x = states, value = TRUE)
# get states names with 'w'
grep(pattern = "w", x = tolower(states), value = TRUE)
grep(pattern = "w", x = states, value = TRUE, ignore.case = TRUE)


# position of a's
positions_a = gregexpr(pattern = "a", text = states, ignore.case = TRUE)
positions_a
# how many a's?
num_a = sapply(positions_a, function(x) ifelse(x[1] > 0, length(x), 0))
num_a
# load stringr (remember to install it first)
library(stringr)
# total number of a's
str_count(states, "a")

# total number of a's
str_count(tolower(states), "a")


# vector of vowels
vowels = c("a", "e", "i", "o", "u")
# vector for storing results
num_vowels = vector(mode = "integer", length = 5)
# calculate number of vowels in each name
for (j in seq_along(vowels)) {
  num_aux = str_count(tolower(states), vowels[j])
  num_vowels[j] = sum(num_aux)
}
# add vowel names
names(num_vowels) = vowels
# total number of vowels
num_vowels
# sort them in decreasing order
sort(num_vowels, decreasing = TRUE)

df2 = data.frame(numbers = 1:5, letters = letters[1:5],
                 stringsAsFactors = FALSE)


# abc radio stations data URL
abc = "http://www.abc.net.au/local/data/public/stations/abc-local-radio.csv"
# read data from URL
radio = read.table(abc, header = TRUE, sep = ",", stringsAsFactors = FALSE)
head(radio)
dim(radio)
# structure of columns
str(radio, vec.len = 1)


# read 'ktop100.txt' file
top105 = readLines("http://www.textfiles.com/music/ktop100.txt")
# how many lines
length(top105)
head(top105)


# paste
PI = paste("The life of", pi)
PI
IloveR = paste("I", "love", "R", sep = "-")
IloveR
# paste with objects of different lengths
paste("X", 1:5, sep = ".")


# paste with collapsing
paste(1:3, c("!", "?", "+"), sep = "", collapse = "")

# paste without collapsing
paste(1:3, c("!", "?", "+"), sep = "")
my_string = "programming with data is fun"
cat(my_string, "with R")
# especifying 'sep'
cat(my_string, "with R", sep = " =) ")
rep("x", 4)

# installing 'stringr'
install.packages("stringr")
# load 'stringr'
library(stringr)
# some factor
some_factor = factor(c(1, 1, 1, 2, 2, 2), labels = c("good", "bad"))
some_factor
nchar(some_factor)
str_length(some_factor)



some_quote = c(
  "I may not have gone",
  "where I intended to go,",
  "but I think I have ended up",
  "where I needed to be")
some_quote = paste(some_quote, collapse = " ")

cat(str_wrap(some_quote, width = 30))

cat(str_wrap(some_quote, width = 30, indent = 2), "\n")

cat(str_wrap(some_quote, width = 30, exdent = 3), "\n")


#regex



