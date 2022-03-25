# Install packages
install.packages("syuzhet")
install.packages("ggplot2")
install.packages("plotly")
install.packages("readr")

# Load packages
library(syuzhet)
library(ggplot2)
library(readr)
library(plotly)

# Load and view tweets
load("politicstweets.RData")
summary(politicstweets)
View(politicstweets)

# Calculate sentiment scores
politicstweets$sentiment_s <- get_sentiment(politicstweets$text, method="syuzhet")
politicstweets$sentiment_n <- get_sentiment(politicstweets$text, method="nrc")
politicstweets$sentiment_a <- get_sentiment(politicstweets$text, method="afinn")
politicstweets$sentiment_b <- get_sentiment(politicstweets$text, method="bing")

# Check output
politicstweets$sentiment_s[1:10]
politicstweets$sentiment_n[1:10]
politicstweets$sentiment_a[1:10]
politicstweets$sentiment_b[1:10]

## Comparing sentiment scores

# Set par
par(mfrow = c(2, 2)) # 2 rows with 2 plots

# First plot
hist(politicstweets$sentiment_s, 
     xlab="Syuzhet Score",
     main="Syuzhet Sentiment Scores for Political Tweets",
     cex.main=.7, col="blue",
     ylim = c(0, 800),
     xlim = c(-10, 10),
     border= F)
# Add line for the mean sentiment
abline(v=mean(politicstweets$sentiment_s), lwd=2)

# Second plot
hist(politicstweets$sentiment_n, 
     xlab="NRC Score",
     main="NRC Sentiment Scores for Political Tweets",
     cex.main = .7, col="purple",
     ylim = c(0, 800),
     xlim = c(-10, 10),
     border= F)
abline(v=mean(politicstweets$sentiment_n), lwd=2)

# Third plot
hist(politicstweets$sentiment_a, 
     xlab="Afinn Score",
     main="Afinn Sentiment Scores for Political Tweets",
     cex.main = .7, col="red",
     ylim = c(0, 800),
     xlim = c(-10, 10),
     border= F)
abline(v=mean(politicstweets$sentiment_a), lwd=2)

# Fourth plot
hist(politicstweets$sentiment_b, 
     xlab="Bing Score",
     main="Bing Sentiment Scores for Political Tweets",
     cex.main = .7, col="green",
     ylim = c(0, 800),
     xlim = c(-10, 10),
     border= F)
abline(v=mean(politicstweets$sentiment_b), lwd=2)

## Compare sentiments to activity
# retweets <- subset(politicstweets, is_retweet == "TRUE")
politicstweets$activity_count = politicstweets$favorite_count + politicstweets$retweet_count

p <- ggplot(data=politicstweets, mapping = aes(x=sentiment_b, y=activity_count)) +
  geom_point(na.rm=T)
p

p <- ggplot(data=politicstweets, mapping = aes(x=sentiment_b, y=activity_count, color=is_retweet)) +
  geom_point(na.rm=T, size=3, shape=4, alpha = .5, position = "jitter")+
  ggtitle("Activity vs. Sentiment in Political Twitter Posts")+
  ylab("Sum of Favorites and Retweets")+
  xlab("Sentiment (Bing)") +
  xlim(-4, 4)
ggplotly(p)


# Extra
bee_string <- read_file("the-full-bee-movie-script.txt")
bee_sentences <- get_sentences(bee_string)
bee_sentiments <- get_sentiment(bee_sentences, 
                               method="nrc")

par(mfrow = c(1, 1))
plot(bee_sentiments, type = "l",col="red", ylab="Sentiment (NRC)", xlab="Sentence", lwd=2, font.lab=2, main="Bee Movie")