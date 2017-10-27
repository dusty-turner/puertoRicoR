#' Puerto Rico Function
#'
#' pRtwitter: This function scrapes twitter for the specified keyword, appends it to your dataset, and saves an updated CSV to the working directory
#' @param searchterm "Puerto Rico"
#' @param n "100000"
#' @param since '2017-10-25'
#' @param until '2017-10-26'
#' @param olddataname "Puerto Rico 23 Sept - 25 OCT best.csv"
#' @param newdataname "Puerto Rico 23 Sept - 26 OCT best.csv"
#' @keywords Puerto Rico
#' @export
#' @examples
#' pRtwitter()


pRtwitter = function(searchterm = "Puerto Rico", n = 100000, since = '2017-10-25', until = '2017-10-26', olddataname = "Puerto Rico 23 Sept - 25 OCT best.csv", newdataname = "Puerto Rico 23 Sept - 26 OCT best.csv"){

  require(twitteR)
  require(ROAuth)
  require(httr)
  require(tidyr)
  require(dplyr)
  require(tidytext)
  require(ggplot2)
  require(lubridate)


  # Set API Keys
  api_key <- "t2Z0bBeYOEKf57x1fwioYHWj5"
  api_secret <- "ogeOnkBpGscHH0HrEaZwnzKp4gnm5g38SiWqwZ8lHbDFHGv5QN"
  access_token <- "232263908-8JfnQTXlCnQzs0TiyJuLSB3rEl70B2CJvvPcpUvS"
  access_token_secret <- "dHvdONwFo2XMyTL2UYPI2WRo7Dvp7hG5A9YFR3Vq0858B"
  setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)


  latest <- searchTwitter(searchterm, n = n,
                          since = since,
                          until = until,
                          geocode = '18.221,-66.59,100mi')


  motherofalldataframes = latest %>%
    twListToDF()

  ################MERGE NEW DATA WITH OLD

  if(file.exists(olddataname)==TRUE){
# olddataname = "Puerto Rico 23 Sept - 26 OCT best.csv"
    df.tweet = read.csv(olddataname)
    # df.tweet$created = as.POSIXct(df.tweet$created,format = "%Y-%m-%d %H:%M", tz = "UTC")
    df.tweet$created = as.POSIXct(df.tweet$created,format = "%m/%d/%Y %H:%M", tz = "UTC")
    df.tweet = df.tweet[,-c(1)]

    #Adding newest data to old data in a csv
    df.tweet$replyToSID = as.character(df.tweet$replyToSID)
    df.tweet$id = as.character(df.tweet$id)
    df.tweet$replyToUID = as.character(df.tweet$replyToUID)
    df.tweet$text = iconv(df.tweet$text, from="UTF-8", to="ASCII", "byte")
    motherofalldataframes$longitude = as.numeric(motherofalldataframes$longitude)
    motherofalldataframes$latitude = as.numeric(motherofalldataframes$latitude)
    motherofalldataframes$created = as_datetime(motherofalldataframes$created)
    # motherofalldataframes$score = 0
    motherofalldataframes$text = iconv(motherofalldataframes$text, from="UTF-8", to="ASCII", "byte")

    df.tweet = rbind(df.tweet, motherofalldataframes)
    df.tweet = distinct(df.tweet)
    # df.tweet = distinct((full_join(df.tweet, motherofalldataframes, by = c("text", "created"))))
    df.tweet = df.tweet[which(!is.na(df.tweet$text)),]
    write.csv(df.tweet, newdataname)
    return(paste("Your New Data Has Been Added to your Old File", newdataname))

  } else if(file.exists(olddataname)==FALSE){
    df = system.file("extdata", "best.csv", package = "puertoRicoR")
    df.tweet = read.csv(df)
    df.tweet$created = as.POSIXct(df.tweet$created,format = "%m/%d/%Y %H:%M", tz = "UTC")
    df.tweet = df.tweet[,-c(1)]

    #Adding newest data to old data in a csv
    df.tweet$replyToSID = as.character(df.tweet$replyToSID)
    df.tweet$id = as.character(df.tweet$id)
    df.tweet$replyToUID = as.character(df.tweet$replyToUID)
    df.tweet$text = iconv(df.tweet$text, from="UTF-8", to="ASCII", "byte")


    motherofalldataframes$longitude = as.numeric(motherofalldataframes$longitude)
    motherofalldataframes$latitude = as.numeric(motherofalldataframes$latitude)
    motherofalldataframes$created = as_datetime(motherofalldataframes$created)
    # motherofalldataframes$score = 0
    motherofalldataframes$text = iconv(motherofalldataframes$text, from="UTF-8", to="ASCII", "byte")
    df.tweet = rbind(df.tweet, motherofalldataframes)
    df.tweet = distinct(df.tweet)
    # df.tweet = distinct((full_join(df.tweet, motherofalldataframes, by = c("text", "created"))))
    df.tweet = df.tweet[which(!is.na(df.tweet$text)),]
    write.csv(df.tweet, newdataname)
    return(paste("Your New Data Has Been Added to the basefile and saved as", newdataname))

  }



}



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

#' bar plot common words
#'
#' common words barplot:
#' @param filename "best.csv"
#' @keywords Puerto Rico
#' @export
#' @examples
#' twbarplot()

twbarplot = function(filename = "best.csv"){


  require(tidyr)
  require(dplyr)
  require(tidytext)
  require(ggplot2)
  require(lubridate)

  if(file.exists(filename)==TRUE){
    df.tweet = read.csv(filename)
    df.tweet$created = as.POSIXct(df.tweet$created,format = "%Y-%m-%d %H:%M", tz = "UTC")
    # df.tweet$created = as.POSIXct(df.tweet$created,format = "%m/%d/%Y %H:%M", tz = "UTC")
    df.tweet = df.tweet[,-c(1)]
    max(df.tweet$created)
    df.tweet = df.tweet[which(!is.na(df.tweet$text)),]
    text_df <- data_frame(line = 1:length(df.tweet$text), text = as.character(df.tweet$text), time = df.tweet$created)
    textcleaned = text_df %>%
      unnest_tokens(word, text)
    ##English Only
    cleanedarticle = anti_join(textcleaned,stop_words, by = "word")
    dropwords = data.frame(word = as.character(c("https","trump")))
    cleanedarticle = anti_join(cleanedarticle,dropwords, by = "word")

    bing_word_counts <- cleanedarticle %>%
      inner_join(get_sentiments("bing")) %>%
      count(word, sentiment, sort = TRUE) %>%
      ungroup()

    ##Bar plot common words

    plottss = bing_word_counts %>%
      group_by(sentiment) %>%
      top_n(10) %>%
      ungroup() %>%
      mutate(word = reorder(word, n)) %>%
      ggplot(aes(word, n, fill = sentiment)) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~sentiment, scales = "free_y") +
      labs(y = "Contribution to sentiment",
           x = NULL) +
      coord_flip()

    return(plottss)
      } else if(file.exists(filename)==FALSE){


  df = system.file("extdata", "best.csv", package = "puertoRicoR")
  df.tweet = read.csv(df)
  df.tweet$created = as.POSIXct(df.tweet$created,format = "%Y-%m-%d %H:%M", tz = "UTC")
  # df.tweet$created = as.POSIXct(df.tweet$created,format = "%m/%d/%Y %H:%M", tz = "UTC")
  df.tweet = df.tweet[,-c(1)]
  max(df.tweet$created)
  df.tweet = df.tweet[which(!is.na(df.tweet$text)),]
  text_df <- data_frame(line = 1:length(df.tweet$text), text = as.character(df.tweet$text), time = df.tweet$created)
  textcleaned = text_df %>%
    unnest_tokens(word, text)
  ##English Only
  cleanedarticle = anti_join(textcleaned,stop_words, by = "word")
  dropwords = data.frame(word = as.character(c("https","trump")))
  cleanedarticle = anti_join(cleanedarticle,dropwords, by = "word")

  bing_word_counts <- cleanedarticle %>%
    inner_join(get_sentiments("bing")) %>%
    count(word, sentiment, sort = TRUE) %>%
    ungroup()

  ##Bar plot common words

  plottss = bing_word_counts %>%
    group_by(sentiment) %>%
    top_n(10) %>%
    ungroup() %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(word, n, fill = sentiment)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~sentiment, scales = "free_y") +
    labs(y = "Contribution to sentiment",
         x = NULL) +
    coord_flip()

  return(plottss)

      }
}

#' helper function
#'
#' helper:
#' @param filename "Puerto Rico 23 Sept - 26 OCT best.csv"
#' @keywords Puerto Rico
#' @export
#' @examples
#' helper()

helpme = function(filename = "best.csv"){


  require(tidyr)
  require(dplyr)
  require(tidytext)
  require(ggplot2)
  require(lubridate)

  df = system.file("extdata", "best.csv", package = "puertoRicoR")
  df = read.csv(df)
  df = df$created
  return(df)
}

