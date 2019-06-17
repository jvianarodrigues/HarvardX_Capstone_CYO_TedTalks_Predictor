# Module 9 Capstone 2 CYO
# Ted Talks Views Predictor

#v1.18
# SECTION 1: DATA EXTRACTION ############################################################################################
library(tidyverse)
library(knitr)

#load 2 core datasets - initially sources from kaggle (https://www.kaggle.com/rounakbanik/ted-talks); saved to local drive to avoid repeated downloads (12Mb)
setwd("Y:\\JRodrigues\\Knowledge\\Data Science\\Harvardx\\Course examples\\Module 9 - capstone 2")
ted_main_data = read.csv("./ted-talks/ted_main.csv",header=TRUE,stringsAsFactors = TRUE)
ted_transcripts_data=read.csv("./ted-talks/transcripts.csv",header=TRUE,stringsAsFactors = FALSE)

#SECTION 2: PRE-PROCESSING ##############################################################################################
# 2.1. explore and dimension the ted_main_data and ted_transcripts_data sets
glimpse(ted_main_data)
dim(ted_main_data)

glimpse(ted_transcripts_data)
dim(ted_transcripts_data)

# ensure we have transcripts for all talks in the ted_main_data set (use URL as unique identifier)
ted_main_data <- semi_join(ted_main_data, ted_transcripts_data, by = "url")

# remove duplicate entries in transcripts
n_distinct(ted_transcripts_data)
nrow(ted_transcripts_data)
ted_transcripts_data <- distinct(ted_transcripts_data)
sum(ted_transcripts_data$url != ted_main_data$url)

#sort both datasets by url to allow direct row matching
ted_transcripts_data <- ted_transcripts_data[order(match(ted_transcripts_data$url, ted_main_data$url)), ]

# check for complete cases on all data, if evaluated to 0 then no incomplete cases 
sum(complete.cases(ted_main_data))-nrow(ted_main_data)

# 2.2. pre-process tag descriptors (concatenated string) into matrix of tags
# calc max_no_of_tags
tags_tidy <- ted_main_data$tags %>% 
  as.character() %>% 
  strsplit(., ", ") %>% 
  unlist() %>% 
  gsub("[[:punct:]]", "", .) %>% 
  enframe()

n_distinct(tags_tidy)
tags_distinct <- tags_tidy %>% group_by(value) %>% summarize(n = n()) %>% arrange(desc(n))
n_distinct(tags_distinct)
tags_matrix <- matrix(nrow = nrow(ted_main_data), ncol = nrow(tags_distinct)) # initialise a null matrix

#populate the tags_matrix by detecting tag names in the tags string and creating a boolean matrix that flags tags included for a given row
for (i in 1:nrow(ted_main_data)) {
  for (j in 1:nrow(tags_distinct)){
        tags_matrix[i, j] <- str_detect(as.character(ted_main_data$tags[i]), paste("\'", as.character(tags_distinct[j, 1]), "\'", sep = ""))  
  }
}

dim(tags_matrix)
colnames(tags_matrix) <- unlist(tags_distinct[1:nrow(tags_distinct), "value"])

# 2.3. preprocess the ratings scores (concatenated string) into a tidy matrix
#extract a 14 row x nrow(ted_main_data) col list of the the count of ratings scores for each ted talk, in a string of format "id number, description, number of ratings"
list_summary_ratings <- sapply(ted_main_data$ratings[1:nrow(ted_main_data)], function(x) {
    x <- x %>%
    str_replace(., "^\\[", "") %>%
    str_replace(., "\\]$", "") %>%
    str_replace_all(., "\\'id\': |\'name\': |\'count\': ", "") %>%
    str_replace_all(., "[[:blank:]]", "") %>%
    str_split(., "\\}") %>%
    unlist() %>%
    str_replace(., ",\\{|\\{", "")
})
list_summary_ratings <- list_summary_ratings %>% t()
dim(list_summary_ratings)
rownames(list_summary_ratings) <- ted_main_data$url[1:nrow(ted_main_data)]

#create a function to transform the string of ratings scores into a dataframe with id, desc, no of ratings
rating_df <- function(x) {
      temp_str_vector <- unlist(str_split(x, ","))
      data.frame (id = as.integer(temp_str_vector[1]), 
      rating_type = str_replace_all(temp_str_vector[2], "[[:punct:]]", ""),
      no_of_ratings = as.numeric(temp_str_vector[3]))
    }

#initialise a blank dataframe
ted_talks_ratings_all <- data.frame(id = integer(), 
                                    rating_type = character(), 
                                    no_of_ratings = numeric())

#create a loop that applies the function to the ratings listing (string format) and converts to a long dataframe with all the data
for (i in 1:nrow(ted_main_data)) { #loop for each ted talk
 for (j in 1:length(list_summary_ratings[1, ])-1) #loop for each rating entry per ted talk
  {
  ted_talks_ratings_all <- rbind(ted_talks_ratings_all, rating_df(list_summary_ratings[i, j]))
  }
}

ted_talks_ratings_all %>%
  group_by(rating_type) %>%
  summarize(no_of_ratings = sum(no_of_ratings)) %>%
  mutate(percent_of_ratings = no_of_ratings/sum(no_of_ratings)) %>%
  arrange(desc(no_of_ratings)) 
  
ted_talks_ratings_all %>% 
  group_by(rating_type) %>%
  mutate(tot_no_of_ratings = sum(no_of_ratings)) %>%
  arrange(desc(tot_no_of_ratings)) %>%
  ggplot(aes(reorder(x = rating_type, -tot_no_of_ratings), y = tot_no_of_ratings)) + 
    geom_col(color = "cornflowerblue") + 
    ggtitle("Total number of ratings by rating category") +
    xlab("Rating category") + 
    ylab("No of ratings")
    
n_distinct(ted_talks_ratings_all$rating_type)
rating_types <- levels(ted_talks_ratings_all$rating_type)
# after interim processing, restore punctuation to 1 of the key ratings categories
rating_types[11] <- "Jaw-dropping"

#create a matrix of nrow(ted_main_data) rows x n_distinct_rating types columns and populate with the views data  
ted_main_data_ratings_matrix <- matrix(nrow = nrow(ted_main_data), ncol = n_distinct(ted_talks_ratings_all$rating_type))
colnames(ted_main_data_ratings_matrix) <- rating_types

for (i in 1:nrow(ted_main_data)) {
  for (j in 1:n_distinct(ted_talks_ratings_all$rating_type))
       {
         #add code here to populate matrix of rating number by rating type from list_summary_ratings
         ted_main_data_ratings_matrix[i, j] <- ifelse(str_detect(ted_main_data$ratings[i], rating_types[j]), 
                                                      as.numeric(str_extract(str_extract(ted_main_data$ratings[i], paste("\'name\': \'", rating_types[j], "\', \'count\': \\d+\\}", sep="")), "\\d+")), 
                                                      0)
       }
}

head(ted_main_data_ratings_matrix, n = 5)

# 2.4. convert time stamps into UTC format
library(lubridate)
ted_main_data$film_date <- as.Date(structure(ted_main_data$film_date, class = c("POSIXct", "POSIXt")))
ted_main_data$published_date <- as.Date(structure(ted_main_data$published_date, class = c("POSIXct", "POSIXt")))

#2.5. sentiment analysis on transcripts
library(tidytext)

#create a subset of the large dataset of transcripts to test coding before full implementation across all
sub_set_to_analyse = nrow(ted_transcripts_data)
ted_transcripts_data_sample <- ted_transcripts_data[sample(1:nrow(ted_transcripts_data), size = sub_set_to_analyse, replace = FALSE), ]
#unnest individual words in the transcript, remove commonly used stop words to reduce dataset size, summarize for repeat words
tidy_transcript <- ted_transcripts_data_sample %>% 
  unnest_tokens(word, transcript) %>% 
  anti_join(stop_words, by = "word") %>%
  group_by(url, word) %>%
  summarize(n = n()) %>%
  filter(word != "â")
  
#display top word counts
tidy_transcript %>%
  group_by(word) %>%
  tally %>%
  arrange(desc(n)) %>%
  head(10)

# nrc lexicon
# inner join the sentiment associated with the matched words from the nrc lexicon 
tidy_transcript_nrc_words <- tidy_transcript %>%
  inner_join(get_sentiments("nrc"), by = "word")

# summarise the sentiment data by ted talk, with URL as the unique identifier
ted_transcript_sentiment_nrc <- tidy_transcript_nrc_words %>%
  group_by(url, sentiment) %>%
  summarize(n = n()) %>%
  spread(key = sentiment, value = n)

glimpse(ted_transcript_sentiment_nrc)
#save the sentiment data file, to avoid repeated re-runs of the dataset
write_excel_csv(as.data.frame(ted_transcript_sentiment_nrc), path = "results_nrc.csv")

#plot a wordcloud of the nrc sentiment words
#install.packages(c("tm", "SnowballC", "wordcloud", "RColorBrewer", "RCurl", "XML"))
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(RCurl)

wordcloud(tidy_transcript_nrc_words$word, min.freq = 1, max.words = 50, random.order = FALSE, random.color = FALSE, colors = "blue4")

# bing lexicon
# inner join the sentiment associated with the matched words from the nrc lexicon 
tidy_transcript_bing_words <- tidy_transcript %>%
  inner_join(get_sentiments("bing"), by = "word")

# summarise the sentiment data by ted talk, with URL as the unique identifier
ted_transcript_sentiment_bing <- tidy_transcript_bing_words %>%
  group_by(url, sentiment) %>%
  summarize(n = n()) %>%
  spread(key = sentiment, value = n)

glimpse(ted_transcript_sentiment_bing)
#save the sentiment data file, to avoid repeated re-runs of the dataset
write_excel_csv(as.data.frame(ted_transcript_sentiment_bing), path = "results_bing.csv")

wordcloud_data <- ted_transcripts_data_sample %>% 
unnest_tokens(word, transcript) %>% 
anti_join(stop_words, by = "word")

wordcloud(tidy_transcript_bing_words$word, min.freq = 1, max.words = 50, random.order = FALSE, random.color = FALSE, colors = "blue4")

#SECTION 3: EXPLORATORY ANALYSIS ########################################################################################
#3.0. Dimensioning the ted_main_data dataset, explore views on key parameters

glimpse(ted_main_data)
ted_main_data %>% ggplot(aes(x = views)) + geom_histogram(bins = 25, color = "blue4", fill = "cornflowerblue") + scale_x_log10() + xlab("log(10) of views") + ggtitle("Histogram of views")
ted_main_data %>% ggplot(aes(x = comments)) + geom_histogram(bins = 25, color = "blue4", fill = "cornflowerblue") + scale_x_log10() + xlab("log(10) of comments") + ggtitle("Histogram of comments")
ted_main_data %>% ggplot(aes(x = duration/60)) + geom_histogram(bins = 25, color = "blue4", fill = "cornflowerblue") + xlab("Talk duration (mins)") + ggtitle("Histogram of duration")
ted_main_data %>% ggplot(aes(x = languages)) + geom_histogram(bins = 25, color = "blue4", fill = "cornflowerblue") + ggtitle("Histogram of languages")
ted_main_data %>% ggplot(aes(x = published_date)) + geom_histogram(bins = 25, color = "blue4", fill = "cornflowerblue") + ggtitle("Histogram of published date")
ted_main_data %>% group_by(num_speaker) %>% summarize(number_of_talks = n(), percent_of_talks = round(n()/nrow(ted_main_data)*100, 1)) %>% knitr::kable()

#3.1. tags analysis - what are the most popular tags (by number, weighted by views per tag)?
sum_of_tags_matrix <- sort(colSums(tags_matrix), decreasing = TRUE) 
sum_of_tags_matrix_df <- data.frame(sum_of_tags = sum_of_tags_matrix, 
                                    tag_name = names(sum_of_tags_matrix)) %>% 
                                    mutate(cum_sum = cumsum(sum_of_tags), 
                                           cum_percent = cum_sum/sum(sum_of_tags))

head(sum_of_tags_matrix_df)
sum(sum_of_tags_matrix_df$cum_percent <= 0.5)

sum_of_tags_matrix_df %>%
  ggplot(aes(x = sum_of_tags)) + geom_histogram(binwidth = 25, color = "blue4", fill = "cornflowerblue") + ggtitle("Histogram of tag counts") + xlab("Count of tags")

sum_of_tags_matrix_df %>% 
    filter(cum_percent <= 0.5) %>% 
    arrange(desc(sum_of_tags)) %>% 
    ggplot(aes(reorder(x = tag_name, sum_of_tags), y = sum_of_tags)) + geom_col(colour = "blue4", fill = "cornflowerblue") + coord_flip() + xlab("Tag name")  + ylab("Occurance of tag name across all Ted Talks") + ggtitle("Plot of pareto of most common tag names, descending")

sum(sum_of_tags_matrix_df$sum_of_tags == 0)

sum_of_tags_matrix_df %>% 
  arrange(sum_of_tags) %>%
  top_n(50) %>%
  ggplot(aes(reorder(x = tag_name, sum_of_tags), y = sum_of_tags)) + geom_col(colour = "blue4", fill = "cornflowerblue") + coord_flip() + xlab("Tag name")  + ylab("Occurance of tag name across all Ted Talks") + ggtitle("Plot of pareto of least common tag names, descending")

# weighted tags by views
weighted_tags_matrix = tags_matrix * ted_main_data$views
weighted_sum_of_tags_matrix <- sort(colSums(weighted_tags_matrix), decreasing = TRUE) 
weighted_tags_matrix_df <- data.frame(wt_sum_of_tags = weighted_sum_of_tags_matrix, 
                                    tag_name = names(weighted_sum_of_tags_matrix)) %>% 
  mutate(cum_sum = cumsum(wt_sum_of_tags), 
         cum_percent = cum_sum/sum(wt_sum_of_tags))
head(weighted_tags_matrix_df)
sum(weighted_tags_matrix_df$cum_percent <= 0.5)

weighted_tags_matrix_df %>%
  ggplot(aes(x = wt_sum_of_tags)) + geom_histogram(bins = 100, color = "blue4", fill = "cornflowerblue") + ggtitle("Histogram of tags weighted by views") + xlab("Tags weighted by views")

sum(weighted_tags_matrix_df$wt_sum_of_tags == 0)

weighted_tags_matrix_df %>% 
  filter(cum_percent <= 0.5) %>% 
  arrange(desc(wt_sum_of_tags)) %>% 
  ggplot(aes(reorder(x = tag_name, wt_sum_of_tags), y = wt_sum_of_tags)) + geom_col(colour = "blue4", fill = "cornflowerblue") + coord_flip() + xlab("Tag name")  + ylab("tag count x number of views") + ggtitle("Plot of pareto of most common tag names, weighted by views, descending")

weighted_tags_matrix_df %>% 
  arrange(wt_sum_of_tags) %>%
  top_n(50) %>%
  ggplot(aes(reorder(x = tag_name, wt_sum_of_tags), y = wt_sum_of_tags)) + geom_col(colour = "blue4", fill = "cornflowerblue") + coord_flip() + xlab("Tag name")  + ylab("tag count x number of views") + ggtitle("Plot of pareto of least common tag names, weighted by views, descending")

# word cloud of tags
#confirm alignment with previous plots
wordcloud(as.vector(tags_tidy$value), min.freq = 50, max.words = 50, random.order = FALSE, colors = "blue4")

#3.2. EDA on ratings

# generate core statistics on total ratings distribution
data.frame(x = rowSums(ted_main_data_ratings_matrix)) %>% summary()
data.frame(x = rowSums(ted_main_data_ratings_matrix)) %>% summarize(mean = mean(x), median = median(x), sd = sd(x))

#plot histogram of number of ratings per views
data.frame(x = rowSums(ted_main_data_ratings_matrix)) %>% 
  ggplot(aes(x = x)) + 
  geom_histogram(bins = 25, color = "blue4", fill = "cornflowerblue") + 
  scale_x_log10() +
  xlab("log (10) of number of ratings per TED Talk") + 
  ylab("number of TED Talks") + 
  ggtitle("Histogram of ratings per TED Talk")

# assign a rating sentiment to each of the 14 rating types, i.e. positive (TRUE) or negative (FALSE)
colnames(ted_main_data_ratings_matrix)
ratings_sentiment = c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE)

#plot count of ratings, grouped by sentiment 
data.frame(num = colSums(ted_main_data_ratings_matrix), 
           names = colnames(ted_main_data_ratings_matrix), 
           sentiment = ratings_sentiment) %>%
    mutate(rating_sentiment = ifelse(sentiment, "Positive", "Negative")) %>%
    ggplot(aes(x = reorder(names, -num), y = num)) + geom_col(aes(fill = rating_sentiment)) +
    xlab("Rating category") + ylab("Number of ratings") + ggtitle("Summary of ratings per category")

#3.3. Testing for correlations between various parameters 
#recap key parameters in ted_main_data
glimpse(ted_main_data)

#3.3.1. comments
#plot scatterplot + linear smooth of views vs. comments
ted_main_data %>%
  ggplot(aes(x = comments, y = views)) + 
  geom_point() + 
  geom_smooth(method = "lm", show.legend = TRUE) + 
  ggtitle("Views vs. number of comments") + 
  xlab("number of comments per TED talk")

#plot scatterplot + linear smooth of views vs. comments, filtering out the top and bottom 1% 
ted_main_data %>%
  filter(between(views, quantile(views, 0.01), quantile(views, 0.99))) %>%
  filter(between(comments, quantile(comments, 0.01), quantile(comments, 0.99))) %>%
  ggplot(aes(x = comments, y = views)) + 
  geom_point() + 
  geom_smooth(method = "lm", show.legend = TRUE) + 
  ggtitle("Views vs. number of comments") + 
  xlab("number of comments per TED talk") +
  labs(subtitle = "filtering out top/bottom 1% of views and comments")

cor(ted_main_data$comments, ted_main_data$views)

#test "rate of comments" as % of total views, expecting that there's a stronger link btw # of comments and ratings
ted_main_data %>%
  mutate(comments_rate = comments/views*1000000) %>%
  ggplot(aes(x = comments_rate)) + 
  geom_histogram(bins = 30, color = "blue4", fill = "cornflowerblue") + 
  scale_x_log10() +
  ggtitle("Histogram of comments per million views")

ted_main_data %>%
  mutate(comments_rate = comments/views) %>%
  filter(between(views, quantile(views, 0.01), quantile(views, 0.99))) %>%
  filter(between(comments_rate, quantile(comments_rate, 0.01), quantile(comments_rate, 0.99))) %>%
  ggplot(aes(x = comments_rate, y = views)) + 
  geom_point() + 
  geom_smooth(method = "lm", show.legend = TRUE) +
  xlab("comments per million views") + 
  ggtitle("Views vs. comments/million views")

cor(ted_main_data$comments/ted_main_data$views, ted_main_data$views)

#3.3.2. duration
ted_main_data %>%
  mutate(duration_in_mins = duration/60) %>%
  ggplot(aes(x = duration_in_mins)) + 
  geom_histogram(bins = 25, color = "blue4", fill = "cornflowerblue") + 
  ggtitle("Histogram of duration of TED Talks")

ted_main_data %>%
  mutate(duration_in_mins = duration/60) %>%
  filter(between(views, quantile(views, 0.01), quantile(views, 0.99))) %>%
  filter(between(duration_in_mins, quantile(duration_in_mins, 0.01), quantile(duration_in_mins, 0.99))) %>%
  ggplot(aes(x = duration_in_mins, y = views)) + 
  geom_point() + 
  ggtitle("Plot of views vs. duration") + 
  labs(subtitle = "filtering out top and bottom 1% outliers of views and durations") +
  xlab("Duration (mins)") + 
  geom_smooth(method = "lm", show.legend = TRUE)

cor(ted_main_data$views, ted_main_data$duration)

#test for speed of talk - hypothesis: this is some gauge of the energy in the talk, maybe more important is the variability within a talk though
library(ngram)
ted_transcripts_data_wordcount <- sapply(ted_transcripts_data$transcript, wordcount)
names(ted_transcripts_data_wordcount) = NULL

ted_main_data %>%
  mutate(word_speed = ted_transcripts_data_wordcount/(duration/60)) %>%
  ggplot(aes(x = word_speed)) +
  geom_histogram(bins = 25, color = "blue4", fill = "cornflowerblue") + 
  ggtitle("Histogram of words per minute for TED Talks")

ted_main_data %>%
  mutate(word_speed = ted_transcripts_data_wordcount/(duration/60)) %>%
  summarize(mean_word_speed = mean(word_speed), median_word_speed = median(word_speed), sd_word_speed = sd(word_speed))

ted_main_data %>%
  mutate(word_speed = ted_transcripts_data_wordcount/(duration/60)) %>%
  ggplot(aes(x = word_speed, y = views)) +
  geom_point() + 
  geom_smooth(method = "lm", show.legend = TRUE)

cor(ted_transcripts_data_wordcount/(ted_main_data$duration/60), ted_main_data$views)

# 3.3.3. languages
#plot languages vs. views with a linear smooth
ted_main_data %>% 
  filter(views < 3e7) %>%
  ggplot(aes(x = languages, y = views)) + 
  geom_point() + 
  geom_smooth(method = "lm", show.legend = TRUE) + 
  ggtitle("Views vs. number of languages per TED talk") +
  labs(subtitle = "filtered out outlying views > 3e7")

cor(ted_main_data$views, ted_main_data$languages)

# 3.3.4. num_speakers
ted_main_data %>%
  group_by(num_speaker) %>%
  summarize(n = n(), sum_of_views = as.numeric(sum(views)), average_views = as.numeric(sum(views)/n()))

paste(round((ted_main_data %>% filter(num_speaker != 1) %>% nrow())/nrow(ted_main_data)*100, 2), "% of ted talks with > 1 speaker")

#such a small sample of num_speaker != 1 that we ignore

# 3.3.4b. specific speakers
ted_main_data %>%
  group_by(main_speaker) %>%
  summarize(number_of_talks_per_speaker = n(), views = sum(as.numeric(views)), average_views = views/number_of_talks_per_speaker) %>%
  arrange(desc(number_of_talks_per_speaker)) %>%
  group_by(number_of_talks_per_speaker) %>%
  summarize(Number_talks_per_speaker = mean(number_of_talks_per_speaker), Total_views = sum(views), Number_of_talks = n(), Average_views = sum(views)/Number_of_talks/Number_talks_per_speaker) %>%
  kable()

ted_main_data %>%
  group_by(main_speaker) %>%
  summarize(number_of_talks_per_speaker = n(), views = sum(as.numeric(views)), average_views = views/number_of_talks_per_speaker) %>%
  arrange(desc(number_of_talks_per_speaker)) %>%
  group_by(number_of_talks_per_speaker) %>%
  summarize(Number_talks_per_speaker = mean(number_of_talks_per_speaker), Total_views = sum(views), Number_of_talks = n(), Average_views = sum(views)/Number_of_talks/Number_talks_per_speaker) %>%
  ggplot(aes(x = Number_talks_per_speaker, y = Average_views)) + 
  geom_col(color = "blue4", fill = "cornflowerblue") + 
  geom_smooth(method = "lm") +
  ggtitle("Average views per number of talks per speaker") + 
  xlab("Number of talks per speaker") + 
  ylab("Average views") + 
  geom_label(aes(label = formatC(Average_views, format = "d", big.mark = ","))) +
  scale_x_continuous(breaks = seq(1, 9))

# 3.3.5. publish date
ted_main_data %>%
  ggplot(aes(x = published_date)) + 
  geom_histogram(binwidth = 365, color = "blue4", fill="cornflowerblue") + 
  ggtitle("Histogram of published dates")

ted_main_data %>%
  ggplot(aes(x = published_date, y = views)) + geom_point() + geom_smooth(method = "lm", show.legend = TRUE)

cor(as.numeric(ted_main_data$published_date), ted_main_data$views)

#3.3.6. Ratings
#plot correlation btw views and total ratings
ted_main_data %>%
  mutate(total_no_of_ratings = rowSums(ted_main_data_ratings_matrix)) %>%
  filter(total_no_of_ratings < 25000) %>%
  ggplot(aes(x = total_no_of_ratings, y = views)) + 
  geom_point() + 
  geom_smooth(method = "lm", show.legend = TRUE) + 
  ggtitle("Correlation of total ratings vs. views") + 
  labs(subtitle = "filtered for far outliers, ratings > 25000")

#test correlation with total ratings
cor(ted_main_data$views, rowSums(ted_main_data_ratings_matrix))

# correlation of views vs. specific rating categories
tmp <- cor(ted_main_data$views, ted_main_data_ratings_matrix) %>%
  sort(decreasing = TRUE)
names(tmp) <- colnames(ted_main_data_ratings_matrix)
tmp
knitr::kable(tmp, col.names = "Correlation")

#plot correlation btw views and net positive ratings
ted_main_data %>%
  mutate(net_no_of_positive_ratings = rowSums(ted_main_data_ratings_matrix %*% ifelse(ratings_sentiment, 1, -1))) %>%
  filter(between(views, quantile(views, 0.01), quantile(views, 0.99))) %>%
  filter(between(net_no_of_positive_ratings, quantile(net_no_of_positive_ratings, 0.01), quantile(net_no_of_positive_ratings, 0.99))) %>%
  ggplot(aes(x = net_no_of_positive_ratings, y = views)) + 
  geom_point() + 
  geom_smooth(method = "lm", show.legend = TRUE) +
  ggtitle("Views vs. count of net positive ratings per TED Talk") +
  labs(subtitle = "Filtered out top/bottom 1% of views and net ratings") +
  xlab("Net positive ratings, i.e. sum of positive ratings less sum of negative ratings")

#test correlation with net positive ratings
cor(ted_main_data$views, rowSums(ted_main_data_ratings_matrix %*% ifelse(ratings_sentiment, 1, -1)))

#test correlation with only sum of positive ratings
cor(ted_main_data$views, rowSums(ted_main_data_ratings_matrix %*% ifelse(ratings_sentiment, 1, 0)))

#3.3.7. Sentiment analysis vs. views

glimpse(ted_transcript_sentiment_bing)
ted_transcript_sentiment_bing %>% 
  ungroup() %>% 
  mutate(net_pos_neg = positive - negative) %>%
  arrange(desc(net_pos_neg)) %>%
  ggplot(aes(x = reorder(url, -net_pos_neg), y = net_pos_neg)) + geom_col()
  
# top 10 by net positive sentiment
ted_transcript_sentiment_bing %>% 
  ungroup() %>% 
  mutate(net_pos_neg = positive - negative) %>%
  arrange(desc(net_pos_neg)) %>%
  top_n(., 10) %>%
  inner_join(., ted_main_data, "url") %>%
  select(title, negative, positive, net_pos_neg, views)

# bottom 10 by net positive sentiment
ted_transcript_sentiment_bing %>% 
  ungroup() %>% 
  mutate(net_pos_neg = positive - negative) %>%
  arrange(desc(net_pos_neg)) %>%
  top_n(., -10) %>%
  inner_join(., ted_main_data, "url") %>%
  select(title, negative, positive, net_pos_neg, views)

# top 10 by range of sentiment
ted_transcript_sentiment_bing %>% 
  ungroup() %>% 
  mutate(spread_pos_neg = positive + negative) %>%
  arrange(desc(spread_pos_neg)) %>%
  top_n(., 10) %>%
  inner_join(., ted_main_data, "url") %>%
  select(title, negative, positive, spread_pos_neg, views)

# test correlation on net positive/negative
inner_join(ted_transcript_sentiment_bing, ted_main_data, by = "url") %>%
  filter(views < quantile(ted_main_data$views, 0.99)) %>%
  mutate(net_pos_neg = positive - negative) %>%
  ggplot(aes(x = net_pos_neg, y = views)) + 
  geom_point() + 
  geom_smooth(method = "lm", show.legend = TRUE) + 
  ggtitle("Views vs. net of positive and negative sentiment")

tmp <- (inner_join(ted_transcript_sentiment_bing, ted_main_data, by = "url") %>% 
          mutate(net_pos_neg = positive - negative) %>%
          na.omit())
cor(tmp$views, tmp$net_pos_neg)

# test correlation on spread of positive/negative
inner_join(ted_transcript_sentiment_bing, ted_main_data, by = "url") %>%
  filter(views < quantile(ted_main_data$views, 0.99)) %>% 
  mutate(spread_pos_neg = positive + negative) %>%
  ggplot(aes(x = spread_pos_neg, y = views)) + 
  geom_point() + 
  geom_smooth(method = "lm", show.legend = TRUE) +
  ggtitle("Views vs. net of positive and negative sentiment") + 
  labs(subtitle = "Filtered out top 1% of views")

tmp <- (inner_join(ted_transcript_sentiment_bing, ted_main_data, by = "url") %>% 
          mutate(spread_pos_neg = positive + negative) %>%
          na.omit())
cor(tmp$views, tmp$spread_pos_neg)

# repeating at a high level for the nrc lexicon
inner_join(ted_transcript_sentiment_nrc, ted_main_data, by = "url") %>%
  filter(views < quantile(ted_main_data$views, 0.99)) %>% 
  mutate(spread_pos_neg = positive + negative) %>%
  ggplot(aes(x = spread_pos_neg, y = views)) + 
  geom_point() + 
  geom_smooth(method = "lm", show.legend = TRUE) +
  ggtitle("Views vs. net of positive and negative sentiment") + 
  labs(subtitle = "Filtered out top 1% of views")

tmp <- (inner_join(ted_transcript_sentiment_bing, ted_main_data, by = "url") %>% 
          mutate(spread_pos_neg = positive + negative) %>%
          na.omit())
cor(tmp$views, tmp$spread_pos_neg)


#SECTION 4: PREDICTION MODELS ###########################################################################################

library(caret)
set.seed(1)

#prepare final dataset for training
final_data_set <- ted_main_data %>% 
  mutate(views_quartile = ntile(views, 4)) %>% # add quartiles of views as a new parameter
  mutate(number_of_ratings = rowSums(ted_main_data_ratings_matrix)) %>% #add total no. of ratings
  mutate(net_no_of_positive_ratings = rowSums(ted_main_data_ratings_matrix %*% ifelse(ratings_sentiment, 1, -1))) #add sum of net positive ratings

#join sentiment analysis to ted_main_data
names(ted_transcript_sentiment_bing)[-1] <- paste("bing_", names(ted_transcript_sentiment_bing)[-1], sep ="")
final_data_set <- inner_join(final_data_set, ted_transcript_sentiment_bing, "url") %>% glimpse()

names(ted_transcript_sentiment_nrc)[-1] <- paste("nrc_", names(ted_transcript_sentiment_nrc)[-1], sep = "")
final_data_set <- inner_join(final_data_set, ted_transcript_sentiment_nrc, "url") %>% glimpse()

final_data_set <- final_data_set %>% 
  mutate(bing_spread_sentiment = bing_positive+bing_negative) %>% #add sentiment spread per bing
  mutate(nrc_spread_sentiment = nrc_positive+nrc_negative) %>% #add sentiment spread per bing
  filter(complete.cases(.)) %>% #confirm complete cases for entire dataset
  glimpse()

# prepare training and test sets
training_sample_percent <- 1 # choose to first model off of a subset of ted_main_data (to prove algorithms and increase run speed whilst testing models)
final_data_subset <- final_data_set[sample(final_data_set$comments, size = floor(nrow(final_data_set) * training_sample_percent), replace = FALSE), ]
final_data_subset <- final_data_subset %>% filter(complete.cases(.))

train_index <- createDataPartition(final_data_subset$views, times = 1, p = 0.8, list = FALSE)
train_set <- final_data_subset[train_index, ]
test_set <- final_data_subset[-train_index, ]

#####################################################################################################
# model 1: knn

ctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 3, verboseIter = TRUE)
k_tune_df <- data.frame(k = seq(3, 15, 3))
train_knn <- train(form = views_quartile ~ ., 
                   data = train_set %>% select(views_quartile, comments, duration, languages, 
                                               num_speaker, number_of_ratings, 
                                               net_no_of_positive_ratings, bing_spread_sentiment, nrc_spread_sentiment),
                   method = "knn",
                   trControl = ctrl, 
                   preProcess = c("center", "scale"),
                   tuneGrid = k_tune_df)

conf_matrix_knn <- confusionMatrix(data = as.factor(round(predict(train_knn, test_set),0)), 
                                   reference = as.factor(test_set$views_quartile))

train_knn$bestTune[1]
ggplot(train_knn, highlight = TRUE) + ggtitle("knn training results")

conf_matrix_knn$overall[["Accuracy"]]
conf_matrix_knn$table
train_knn$results %>% kable()

accuracy_1_quartile <- function(x) {
  1-sum(x[1, 3:4], x[2, 4], x[3, 1], x[4, 1:2])/sum(x)
}
accuracy_1_quartile(conf_matrix_knn$table)

# model 2: rborist
ctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 3, verboseIter = TRUE) # test run
tune_Rborist <- data.frame(predFixed = 2, minNode = seq(3,15,3))
train_Rborist <- train(form = views_quartile ~ ., 
                       data = train_set %>% select(views_quartile, comments, duration, languages, 
                                                   num_speaker, number_of_ratings, 
                                                   net_no_of_positive_ratings, bing_spread_sentiment, nrc_spread_sentiment),
                     method = "Rborist",
                     trControl = ctrl, 
                     preProcess = c("center", "scale"),
                     tuneGrid = tune_Rborist)

conf_matrix_rborist <- confusionMatrix(data = as.factor(round(predict(train_Rborist, test_set),0)), 
                                       reference = as.factor(test_set$views_quartile))


ggplot(train_Rborist, highlight = TRUE) + ggtitle("Rborist training results, tuned for minNodes")

train_Rborist$bestTune[2]
conf_matrix_rborist$overall[["Accuracy"]]
conf_matrix_rborist$table
conf_matrix_rborist$byClass %>% kable
accuracy_1_quartile(conf_matrix_rborist$table)

# model 3: ranger
ctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 3, verboseIter = TRUE)
tune_ranger <- expand.grid(.mtry = 2:5, .splitrule = "extratrees", .min.node.size = seq(3,15,3))
train_ranger <- train(form = views_quartile ~ ., 
                       data = train_set %>% select(views_quartile, comments, duration, languages, 
                                                   num_speaker, number_of_ratings, 
                                                   net_no_of_positive_ratings, bing_spread_sentiment, nrc_spread_sentiment),
                       method = "ranger",
                       trControl = ctrl, 
                       preProcess = c("center", "scale"),
                       tuneGrid = tune_ranger, 
                       importance = "impurity")

conf_matrix_ranger <- confusionMatrix(data = as.factor(round(predict(train_ranger, test_set),0)), 
                                      reference = as.factor(test_set$views_quartile))

ggplot(train_ranger, highlight = TRUE) + ggtitle("Ranger training results, tuned for min.node.size")
conf_matrix_ranger$overall[["Accuracy"]]
conf_matrix_ranger$table
conf_matrix_ranger$byClass %>% kable
accuracy_1_quartile(conf_matrix_ranger$table)

#####################################################################################################

#Dataset 2: no a priori parameters to train on
set.seed(1)

#prepare final dataset for training
final_data_set <- ted_main_data %>% 
  mutate(views_quartile = ntile(views, 4)) # add quartiles of views as a new parameter

colnames(tags_matrix) <- paste("tag_", colnames(tags_matrix), sep = "") # rename columns in tags matrix
final_data_set <- cbind(final_data_set, tags_matrix) # add on tags matrix to ted_main_data

#join sentiment analysis to ted_main_data
#names(ted_transcript_sentiment_bing)[-1] <- paste("bing_", names(ted_transcript_sentiment_bing)[-1], sep ="")
final_data_set <- inner_join(final_data_set, ted_transcript_sentiment_bing, "url") %>% glimpse()

#names(ted_transcript_sentiment_nrc)[-1] <- paste("nrc_", names(ted_transcript_sentiment_nrc)[-1], sep = "")
final_data_set <- inner_join(final_data_set, ted_transcript_sentiment_nrc, "url") %>% glimpse()

final_data_set <- final_data_set %>% 
  mutate(bing_spread_sentiment = bing_positive+bing_negative) %>% #add sentiment spread per bing
  mutate(nrc_spread_sentiment = nrc_positive+nrc_negative) %>% #add sentiment spread per bing
  filter(complete.cases(.)) %>% #confirm complete cases for entire dataset
  glimpse()

#filter out parameters not used for training, incl. those that will only be known after talk release and feedback from viewers
final_data_set <- final_data_set %>% select(-languages, -ratings, -description, -related_talks, -event, -name, -tags, 
                                            -speaker_occupation, -main_speaker, -title, -url, -views)

# prepare training and test sets
training_sample_percent <- 1 # choose to first model off of a subset of ted_main_data (to prove algorithms and increase run speed whilst testing models)
final_data_subset <- final_data_set[sample(final_data_set$comments, size = floor(nrow(final_data_set) * training_sample_percent), replace = FALSE), ]
final_data_subset <- final_data_subset %>% filter(complete.cases(.))

train_index <- createDataPartition(final_data_subset$views, times = 1, p = 0.8, list = FALSE)
train_set <- final_data_subset[train_index, ]
test_set <- final_data_subset[-train_index, ]

# model 1: ranger
ctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 3, verboseIter = TRUE) #test run
tune_ranger <- expand.grid(.mtry = seq(5,25,5), .splitrule = "extratrees", .min.node.size = seq(3,15,3))

train_ranger2 <- train(form = views_quartile ~ ., 
                   data = train_set,
                   method = "ranger",
                   trControl = ctrl, 
                   preProcess = c("center", "scale"),
                   tuneGrid = tune_ranger)
train_ranger2$bestTune
ggplot(train_ranger2, highlight = TRUE)

conf_matrix_ranger2 <- confusionMatrix(data = as.factor(round(predict(train_ranger2, test_set),0)), 
                                    reference = as.factor(test_set$views_quartile))

conf_matrix_ranger2$overall[["Accuracy"]]
conf_matrix_ranger2$table
accuracy_1_quartile(conf_matrix_ranger2$table)

# 5. RESULTS
# consolidate and display results of various models

df_dataset1_results <- data.frame(name = character(), 
                                  accuracy = numeric())

df_dataset1_results <- rbind(df_dataset1_results,
                          data_frame(name = "knn", accuracy = conf_matrix_knn$overall[["Accuracy"]]),
                          data_frame(name = "rborist", accuracy = conf_matrix_rborist$overall[["Accuracy"]]),
                          data_frame(name = "ranger", accuracy = conf_matrix_ranger$overall[["Accuracy"]]))

tmp <- c(accuracy_1_quartile(conf_matrix_knn$table), 
         accuracy_1_quartile(conf_matrix_rborist$table),
         accuracy_1_quartile(conf_matrix_ranger$table))

df_dataset1_results <- cbind(df_dataset1_results, tmp)
names(df_dataset1_results)[3] <- "Accuracy to 1 quartile"
df_dataset1_results %>% kable
df_dataset1_results %>% ggplot(aes(x = name, y = accuracy)) + geom_col()

# dataset 2
df_dataset2_results <- data.frame(name = character(), 
                                  accuracy = numeric())
df_dataset2_results <- rbind(df_dataset2_results,
                             data_frame(name = "ranger", accuracy = conf_matrix_ranger2$overall[["Accuracy"]]))
tmp <- accuracy_1_quartile(conf_matrix_ranger2$table)
df_dataset2_results <- cbind(df_dataset2_results, tmp)
names(df_dataset2_results)[3] <- "Accuracy to 1 quartile"
df_dataset2_results %>% kable


# END ##########################################################################################################################
