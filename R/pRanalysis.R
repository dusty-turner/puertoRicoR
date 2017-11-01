#' Puerto Rico Sentiment Tracking Function
#'
#' pRanalysis: This function takes a twitter dataframe and produces the twitter sentiment graph over time
#' @param filename "Puerto Rico 23 Sept - 26 OCT best.csv"
#' @keywords Puerto Rico
#' @export
#' @examples
#' pRtwitter()

pRanalysis = function(filename = "Puerto Rico 23 Sept - 26 OCT best.csv"){


  require(tidyr)
  require(dplyr)
  require(tidytext)
  require(ggplot2)
  require(lubridate)
  # filename = "testingagainR.csv"
  df.tweet = read.csv(filename)
  df.tweet$created = as.POSIXct(df.tweet$created,format = "%Y-%m-%d %H:%M", tz = "UTC")
  # df.tweet$created = as.POSIXct(df.tweet$created,format = "%m/%d/%Y %H:%M", tz = "UTC")
  df.tweet = df.tweet[,-c(1)]
  max(df.tweet$created)
  df.tweet = df.tweet[which(!is.na(df.tweet$text)),]


  ######## tokenize

  text_df <- data_frame(line = 1:length(df.tweet$text), text = as.character(df.tweet$text), time = df.tweet$created)

  textcleaned = text_df %>%
    unnest_tokens(word, text)


  ##English Only
  cleanedarticle = anti_join(textcleaned,stop_words, by = "word")
  dropwords = data.frame(word = as.character(c("https","trump")))
  cleanedarticle = anti_join(cleanedarticle,dropwords, by = "word")

  #### English
  nrc = get_sentiments("nrc")
  nrc = nrc %>%
    filter(sentiment=="negative") %>%
    full_join(nrc%>%
                filter(sentiment=="positive"))
  bing = get_sentiments("bing")
  afinn = get_sentiments("afinn")

  df = system.file("extdata", "NRCSpanish.csv", package = "puertoRicoR")
  nrcspanish = read.csv(df)

  max(cleanedarticle$time)
  min(cleanedarticle$time)

  tweetsentimentbing <- cleanedarticle %>%
    inner_join(bing, by = "word")
  tweetsentimentafinn <- cleanedarticle %>%
    inner_join(afinn, by = "word")
  tweetsentimentnrc <- cleanedarticle %>%
    inner_join(nrc, by = "word")
  tweetsentimentnrcspanish <- textcleaned %>%
    inner_join(nrcspanish, by = c("word"="Spanish"))

  tweetsentimentbing$sentimentnum = ifelse(tweetsentimentbing$sentiment=="positive",1,-1)
  tweetsentimentnrc$sentimentnum = ifelse(tweetsentimentnrc$sentiment=="positive",1,-1)
  tweetsentimentnrcspanish$sentimentnum = ifelse(tweetsentimentnrcspanish$Sentiment=="Positive",1,-1)
  # tweetsentiment$sentimentnum = left_join(tweetsentiment)

  plottingsentimentbing = tweetsentimentbing %>%
    mutate(windowday = as.POSIXct(trunc.POSIXt(time, units = c("hours")))) %>%
    group_by(windowday) %>%
    summarise(netsent = sum(sentimentnum), totalsent = sum(abs(sentimentnum))) %>%
    mutate(sentavg = netsent/totalsent) %>%
    mutate(Lexicon = "Bing")

  plottingsentimentafinn = tweetsentimentafinn %>%
    mutate(windowday = as.POSIXct(trunc.POSIXt(time, units = c("hours")))) %>%
    group_by(windowday) %>%
    summarise(netsent = sum(score), totalsent = sum(abs(score))) %>%
    mutate(sentavg = netsent/totalsent) %>%
    mutate(Lexicon = "Afinn")

  plottingsentimentnrc = tweetsentimentnrc %>%
    mutate(windowday = as.POSIXct(trunc.POSIXt(time, units = c("hours")))) %>%
    group_by(windowday) %>%
    summarise(netsent = sum(sentimentnum), totalsent = sum(abs(sentimentnum))) %>%
    mutate(sentavg = netsent/totalsent) %>%
    mutate(Lexicon = "NRC English")

  plottingsentimentnrcspanish = tweetsentimentnrcspanish %>%
    mutate(windowday = as.POSIXct(trunc.POSIXt(time, units = c("hours")))) %>%
    group_by(windowday) %>%
    summarise(netsent = sum(sentimentnum), totalsent = sum(abs(sentimentnum))) %>%
    mutate(sentavg = netsent/totalsent) %>%
    mutate(Lexicon = "NRC Spanish")

  # plottingsentiment = bind_rows(plottingsentimentbing,plottingsentimentafinn,plottingsentimentnrc)
  # plottingsentiment = bind_rows(plottingsentimentbing,plottingsentimentafinn,plottingsentimentnrc,plottingsentimentnrcspanish)
  plottingsentiment = bind_rows(plottingsentimentnrc,plottingsentimentnrcspanish)

  plot =  ggplot(data=plottingsentiment, aes(x=windowday, y = sentavg, color = Lexicon)) +
    ylim(c(-1,1)) +
    geom_smooth(span = .5) +
    ggtitle("Twitter Sentiment Score Over Time", subtitle = paste("From", substr(min(plottingsentiment$windowday),1,10),"through",substr(max(plottingsentiment$windowday),1,10))) +
    labs(caption=paste("Plot created:", Sys.Date())) +
    labs(x="Date", y="Sentiment Score")

  return(plot)
}