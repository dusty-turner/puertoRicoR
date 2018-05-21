#' Puerto Rico Sentiment Tracking Function
#'
#' pRanalysis: This function takes a twitter dataframe and produces the twitter sentiment graph over time
#' @param filename "Puerto Rico 23 Sept - 26 OCT best.csv"
#' @keywords Puerto Rico
#' @export
#' @examples
#' pRtwitter()

pRanalysis = function(filename = "Puerto Rico 23S - 17MAY.csv"){


  require(dplyr)
  require(tidyr)
  require(readr)
  require(tidytext)
  require(ggplot2)
  require(zoo)
  require(scales)
  # filename = "Puerto Rico Latest.csv"
  df.tweet = read_csv(filename)
  # df.tweet = read_csv(file.choose())
  # df.tweet$created
  # range(df.tweet$created)
  # df.tweet$created = as.POSIXct(df.tweet$created,format = "%Y-%m-%d %H:%M", tz = "UTC")
  df.tweet$created = as.POSIXct(df.tweet$created,format = "%m/%d/%Y %H:%M", tz = "UTC")
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
  # bing = get_sentiments("bing")
  # afinn = get_sentiments("afinn")

  df = system.file("extdata", "NRCSpanish.csv", package = "puertoRicoR")
  nrcspanish = read.csv(df)

  max(cleanedarticle$time)
  min(cleanedarticle$time)

  # tweetsentimentbing <- cleanedarticle %>%
  #   inner_join(bing, by = "word")
  # tweetsentimentafinn <- cleanedarticle %>%
  #   inner_join(afinn, by = "word")
  tweetsentimentnrc <- cleanedarticle %>%
    inner_join(nrc, by = "word")
  tweetsentimentnrcspanish <- textcleaned %>%
    inner_join(nrcspanish, by = c("word"="Spanish"))

  # tweetsentimentbing$sentimentnum = ifelse(tweetsentimentbing$sentiment=="positive",1,-1)
  tweetsentimentnrc$sentimentnum = ifelse(tweetsentimentnrc$sentiment=="positive",1,-1)
  tweetsentimentnrcspanish$sentimentnum = ifelse(tweetsentimentnrcspanish$Sentiment=="Positive",1,-1)
  # tweetsentiment$sentimentnum = left_join(tweetsentiment)

  # plottingsentimentbing = tweetsentimentbing %>%
  #   mutate(windowday = as.POSIXct(trunc.POSIXt(time, units = c("hours")))) %>%
  #   group_by(windowday) %>%
  #   summarise(netsent = sum(sentimentnum), totalsent = sum(abs(sentimentnum))) %>%
  #   mutate(sentavg = netsent/totalsent) %>%
  #   mutate(Lexicon = "Bing") %>%
  #   mutate(rollingmean = rollmean(x = sentavg, 24, align = "right", fill = NA))
  #
  # plottingsentimentafinn = tweetsentimentafinn %>%
  #   mutate(windowday = as.POSIXct(trunc.POSIXt(time, units = c("hours")))) %>%
  #   group_by(windowday) %>%
  #   summarise(netsent = sum(score), totalsent = sum(abs(score))) %>%
  #   mutate(sentavg = netsent/totalsent) %>%
  #   mutate(Lexicon = "Afinn") %>%
  #   mutate(rollingmean = rollmean(x = sentavg, 24, align = "right", fill = NA))

  plottingsentimentnrc = tweetsentimentnrc %>%
    mutate(windowday = as.POSIXct(trunc.POSIXt(time, units = c("hours")))) %>%
    group_by(windowday) %>%
    summarise(netsent = sum(sentimentnum), totalsent = sum(abs(sentimentnum))) %>%
    mutate(sentavg = netsent/totalsent) %>%
    mutate(Lexicon = "NRC English") %>%
    mutate(rollingmean = rollmean(x = sentavg, 24, align = "right", fill = NA))

  plottingsentimentnrcspanish = tweetsentimentnrcspanish %>%
    mutate(windowday = as.POSIXct(trunc.POSIXt(time, units = c("hours")))) %>%
    group_by(windowday) %>%
    summarise(netsent = sum(sentimentnum), totalsent = sum(abs(sentimentnum))) %>%
    mutate(sentavg = netsent/totalsent) %>%
    mutate(Lexicon = "NRC Spanish") %>%
    mutate(rollingmean = rollmean(x = sentavg, 24, align = "right", fill = NA))

  # plottingsentiment = bind_rows(plottingsentimentbing,plottingsentimentafinn,plottingsentimentnrc)
  # plottingsentiment = bind_rows(plottingsentimentbing,plottingsentimentafinn,plottingsentimentnrc,plottingsentimentnrcspanish)
  plottingsentiment = bind_rows(plottingsentimentnrc,plottingsentimentnrcspanish)


  ## creates the right side
  addition = cleanedarticle %>%
    distinct(line, time)%>%
    mutate(windowday = as.POSIXct(trunc.POSIXt(time, units = c("days")))) %>%
    group_by(windowday) %>%
    count(windowday) %>%
    mutate(Lexicon = "Test")

  ## makes the scaling happen
  themax = max(addition$n)
  addition$scaledn = rescale(addition$n, to = c(-.5,1))

  plot = ggplot() +
    # ylim(c(-1,1)) +
    geom_smooth(data = plottingsentiment, method = "loess", span = .10, aes(x = windowday, y = sentavg, color = Lexicon)) +
    geom_line(data = addition, aes(x = windowday, y = scaledn), alpha = .5) +
    ggtitle("Twitter Sentiment Score Over Time", subtitle = paste("From", substr(min(plottingsentiment$windowday),1,10),"through",substr(max(plottingsentiment$windowday),1,10))) +
    labs(caption=paste("Plot created:", Sys.Date())) +
    labs(x="Date", y="Sentiment Score") +
      scale_y_continuous(
        "Net Sentiment",
        sec.axis = sec_axis(~ (.+.5) * max(addition$n)/1.5
                            , name = "Total Tweets Per Day")) +
    theme(legend.position="bottom")


## Sand Chart Creation

  nrcfacet =  tweetsentimentnrc %>%
    mutate(windowday = as.POSIXct(trunc.POSIXt(time, units = c("days")))) %>%
    group_by(windowday, sentiment) %>%
    summarise(netsent = sum(sentimentnum), totalsent = sum(abs(sentimentnum))) %>%
    mutate(sentavg = netsent/totalsent) %>%
    mutate(Lexicon = "NRC") %>%
    ungroup()

  nrcspanishfacet =  tweetsentimentnrcspanish %>%
    mutate(windowday = as.POSIXct(trunc.POSIXt(time, units = c("days")))) %>%
    group_by(windowday, Sentiment) %>%
    summarise(netsent = sum(sentimentnum), totalsent = sum(abs(sentimentnum))) %>%
    mutate(sentavg = netsent/totalsent) %>%
    mutate(Lexicon = "NRC Spanish")

  ## Make spanish sentiment words lower case
nrcspanishfacet = nrcspanishfacet %>%
  mutate(sentiment = tolower(Sentiment))

    ## Joins the english and spanish for total sentiment
    nrcengspan = nrcfacet %>%
    full_join(nrcspanishfacet) %>%
    group_by(windowday, sentiment) %>%
    summarise(dailysent = sum(netsent))

  ## creates the right side
  addition = cleanedarticle %>%
    distinct(line, time)%>%
    mutate(windowday = as.POSIXct(trunc.POSIXt(time, units = c("days")))) %>%
    group_by(windowday) %>%
    count(windowday)

  ## makes the scaling happen
  themax = max(data.frame(nrcengspan[nrcengspan$sentiment=="positive",3]+abs(nrcengspan[nrcengspan$sentiment=="negative",3])))
  addition$scaledn = rescale(addition$n, to = c(min(addition$n),max(themax)))

  nrcengspan = nrcengspan %>%
    left_join(addition, by = "windowday")

  ## Finally plots the data
  # plot2 = ggplot(nrcengspan, aes(x = windowday, y = abs(dailysent), fill = sentiment)) +
  plot2 = ggplot(nrcengspan, aes(x = windowday, y = abs(dailysent))) +
    geom_area(aes(group = sentiment, fill = sentiment)) +
    geom_line(aes(y = scaledn)) +
    ggtitle("Twitter Sentiment Over Time", subtitle = paste("From", substr(min(plottingsentiment$windowday),1,10),"through",substr(max(plottingsentiment$windowday),1,10))) +
    labs(caption=paste("Plot created:", Sys.Date())) +
    labs(x="Date", y="Net Sentiment Added") +
    scale_y_continuous(
      "Total Sentiment",
      sec.axis = sec_axis(~ . * max(addition$n) /max(data.frame(nrcengspan[nrcengspan$sentiment=="positive",3]+abs(nrcengspan[nrcengspan$sentiment=="negative",3])))
                          , name = "Total Tweets Per Day")
    )+
    theme(legend.position="bottom")

  plotlist = list(plot, plot2)

  return(plotlist)
}

