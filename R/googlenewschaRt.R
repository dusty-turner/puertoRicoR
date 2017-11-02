#' Puerto Rico Google News Chart Making
#'
#' googlenewschaRt: This function takes the news data frame and makes the sentiment chart
#' @param filename "R_News_1NOV17.csv"
#' @keywords Puerto Rico
#' @export
#' @examples
#' googlenewschaRt

googlenewschaRt = function(filename = "R_News_1NOV17.csv"){


  library(lubridate)
  library(tidyverse)
  library(tidytext)
  library(ggplot2)

  news = read.csv("R_News_1NOV17.csv")
  news = news[which(complete.cases(news)),]
  news$Date = as_date(news$Date)
  news = news[which(!is.na(news$Date)),]


  text_df <- data_frame(line = 1:length(news$Title), text = as.character(news$Title), Date = as_date(news$Date), Source = rep("All_News", nrow(news)), Company = news$Source)
  text_df2 <- data_frame(line = 1:length(news$Title), text = as.character(news$Narrative), Date = as_date(news$Date), Source = rep("All_News", nrow(news)),Company = news$Source)

  text_df = bind_rows(text_df,text_df2)

  textcleaned = text_df %>%
    unnest_tokens(word, text)

  data("stop_words")
  cleanedarticle = anti_join(textcleaned,stop_words, by = "word")
  dropwords = data.frame(word = as.character(c("trump", "Rico", "rico", "comfort")))
  cleanedarticle = anti_join(cleanedarticle,dropwords, by = "word")


  tweetsentimentbing = cleanedarticle %>%
    inner_join(get_sentiments("bing"))
  tweetsentimentafinn = cleanedarticle %>%
    inner_join(get_sentiments("afinn"))
  nrc = get_sentiments("nrc")
  nrc = nrc %>%
    filter(sentiment=="negative") %>%
    full_join(nrc%>%
                filter(sentiment=="positive"))
  tweetsentimentnrc = cleanedarticle %>%
    inner_join(nrc)

  tweetsentimentbing$sentimentnum = ifelse(tweetsentimentbing$sentiment=="positive",1,-1)
  tweetsentimentnrc$sentimentnum = ifelse(tweetsentimentnrc$sentiment=="positive",1,-1)

  news.df.bing =
    tweetsentimentbing %>%
    mutate(windowday = as.POSIXct(trunc.POSIXt(tweetsentimentbing$Date, units = c("days")))) %>%
    group_by(windowday, Company) %>%
    arrange(windowday) %>%
    summarise(netsent = sum(sentimentnum), totalsent = sum(abs(sentimentnum))) %>%
    mutate(sentavg = netsent/totalsent) %>%
    mutate(lexicon = "Bing")

  news.df.afinn =
    tweetsentimentafinn %>%
    mutate(windowday = as.POSIXct(trunc.POSIXt(tweetsentimentafinn$Date, units = c("days")))) %>%
    group_by(windowday, Company) %>%
    arrange(windowday) %>%
    summarise(netsent = sum(score), totalsent = sum(abs(score))) %>%
    mutate(sentavg = netsent/totalsent) %>%
    mutate(lexicon = "Afinn")

  news.df.nrc =
    tweetsentimentnrc %>%
    mutate(windowday = as.POSIXct(trunc.POSIXt(tweetsentimentnrc$Date, units = c("days")))) %>%
    group_by(windowday, Company) %>%
    arrange(windowday) %>%
    summarise(netsent = sum(sentimentnum), totalsent = sum(abs(sentimentnum))) %>%
    mutate(sentavg = netsent/totalsent) %>%
    mutate(lexicon = "NRC")

  news.df = bind_rows(news.df.bing,news.df.afinn,news.df.nrc)

  #################
  top6 = count(news, Source) %>% arrange(desc(n)) %>% slice(1:5) %>% select(Source) %>%
    as.data.frame()
  top6 = as.character(top6$Source)
  top6 = top6 %>% paste(collapse=",")

  sources = length(news$Source)
  uniquesources = length(unique(news$Source))

plot = ggplot(data = news.df, aes(x = windowday, y = sentavg, color = lexicon)) +
    ylim(-1,1) +
    # geom_smooth(span = .5) +
    geom_smooth(method = loess, span = .01) +
    # geom_line()
    ggtitle("News Sentiment Score Over Time", subtitle = paste("From", substr(min(news.df$windowday),1,10),"through",substr(max(news.df$windowday),1,10))) +
    labs(caption=paste(sources, "Articles from", uniquesources,"different sources \n", "Top News Sources:", top6)) +
    xlab(NULL) + ylab("Sentiment")



  return(plot)
}
