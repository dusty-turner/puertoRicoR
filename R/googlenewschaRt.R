#' Puerto Rico Google News Chart Making
#'
#' googlenewschaRt: This function takes the news data frame and makes the sentiment chart
#' @param filename "R_News_1NOV17.csv"
#' @keywords Puerto Rico
#' @export
#' @examples
#' googlenewschaRt

googlenewschaRt = function(filename = "R_News_21NOV17.csv"){


  library(lubridate)
  library(tidyverse)
  library(tidytext)
  library(ggplot2)
  library(scales)

  news = read_csv(filename)
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


  # tweetsentimentbing = cleanedarticle %>%
  #   inner_join(get_sentiments("bing"))
  # tweetsentimentafinn = cleanedarticle %>%
  #   inner_join(get_sentiments("afinn"))
  nrc = get_sentiments("nrc")
  nrc = nrc %>%
    filter(sentiment=="negative") %>%
    full_join(nrc%>%
                filter(sentiment=="positive"))
  tweetsentimentnrc = cleanedarticle %>%
    inner_join(nrc)

  # tweetsentimentbing$sentimentnum = ifelse(tweetsentimentbing$sentiment=="positive",1,-1)
  tweetsentimentnrc$sentimentnum = ifelse(tweetsentimentnrc$sentiment=="positive",1,-1)

  # news.df.bing =
  #   tweetsentimentbing %>%
  #   mutate(windowday = as.POSIXct(trunc.POSIXt(tweetsentimentbing$Date, units = c("days")))) %>%
  #   group_by(windowday, Company) %>%
  #   arrange(windowday) %>%
  #   summarise(netsent = sum(sentimentnum), totalsent = sum(abs(sentimentnum))) %>%
  #   mutate(sentavg = netsent/totalsent) %>%
  #   mutate(lexicon = "Bing")
  #
  # news.df.afinn =
  #   tweetsentimentafinn %>%
  #   mutate(windowday = as.POSIXct(trunc.POSIXt(tweetsentimentafinn$Date, units = c("days")))) %>%
  #   group_by(windowday, Company) %>%
  #   arrange(windowday) %>%
  #   summarise(netsent = sum(score), totalsent = sum(abs(score))) %>%
  #   mutate(sentavg = netsent/totalsent) %>%
  #   mutate(lexicon = "Afinn")

  news.df.nrc =
    tweetsentimentnrc %>%
    mutate(windowday = as.POSIXct(trunc.POSIXt(tweetsentimentnrc$Date, units = c("days")))) %>%
    group_by(windowday, Company) %>%
    arrange(windowday) %>%
    summarise(netsent = sum(sentimentnum), totalsent = sum(abs(sentimentnum))) %>%
    mutate(sentavg = netsent/totalsent) %>%
    mutate(lexicon = "NRC")

  # news.df = bind_rows(news.df.bing,news.df.afinn,news.df.nrc)
  news.df = news.df.nrc

  #################
  top6 = count(news, Source) %>% arrange(desc(n)) %>% slice(1:5) %>% select(Source) %>%
    as.data.frame()
  top6 = as.character(top6$Source)
  top6 = top6 %>% paste(collapse=",")

  sources = length(news$Source)
  uniquesources = length(unique(news$Source))

  addition = news %>%
    select(Source, Date) %>%
    mutate(windowday = as.POSIXct(trunc.POSIXt(Date, units = c("days")))) %>%
    select(windowday, Source) %>%
    group_by(windowday) %>%
    summarise(n = n())

  addition$scaledn = rescale(addition$n, to = c(-.25,1))

plot = ggplot(data = news.df, aes(x = windowday, y = sentavg)) +
    # ylim(-1,1) +
    geom_smooth(method = loess, span = .01) +
    geom_line(data = addition, aes(y=scaledn)) +
    ggtitle("News Sentiment Score and Volume Over Time", subtitle = paste("From", substr(min(news.df$windowday),1,10),"through",substr(max(news.df$windowday),1,10))) +
    labs(caption=paste(sources, "Articles from", uniquesources,"different sources \n", "Top News Sources:", top6)) +
    xlab(NULL) + ylab("Sentiment") +
    scale_y_continuous(
      "Total Sentiment",
      sec.axis = sec_axis(~ (.+.25) * max(addition$n)/1.25
                          , name = "Total Articles"))

  nrcfacetneg = tweetsentimentnrc %>%
    group_by(Company) %>%
    summarise(totalsentbysource = sum(abs(sentimentnum))) %>%
    arrange(desc(totalsentbysource)) %>%
    top_n(10) %>%
    inner_join(tweetsentimentnrc) %>%
    select(Company, Date, word, sentiment, sentimentnum) %>%
    mutate(windowday = as.POSIXct(trunc.POSIXt(Date, units = c("days")))) %>%
    group_by(windowday, sentiment, Company) %>%
    summarise(netsent = sum(sentimentnum), totalsent = sum(abs(sentimentnum))) %>%
    arrange(Company, windowday) %>%
    mutate(sentavg = netsent/totalsent) %>%
    mutate(Lexicon = "NRC") %>%
    group_by(windowday, Company) %>%
    filter(sentiment=="negative") %>%
    arrange(windowday) %>%
    ungroup()

  nrcfacetpos = tweetsentimentnrc %>%
    group_by(Company) %>%
    summarise(totalsentbysource = sum(abs(sentimentnum))) %>%
    arrange(desc(totalsentbysource)) %>%
    top_n(10) %>%
    inner_join(tweetsentimentnrc) %>%
    select(Company, Date, word, sentiment, sentimentnum) %>%
    mutate(windowday = as.POSIXct(trunc.POSIXt(Date, units = c("days")))) %>%
    group_by(windowday, sentiment, Company) %>%
    summarise(netsent = sum(sentimentnum), totalsent = sum(abs(sentimentnum))) %>%
    arrange(Company, windowday) %>%
    mutate(sentavg = netsent/totalsent) %>%
    mutate(Lexicon = "NRC") %>%
    group_by(windowday, Company) %>%
    filter(sentiment=="positive") %>%
    ungroup()

  weeksneg <- unique(nrcfacetneg$windowday)
  plansneg <- unique(nrcfacetneg$Company)
  combinationsneg <- expand.grid(windowday = weeksneg, Company = plansneg)

  weekspos <- unique(nrcfacetpos$windowday)
  planspos <- unique(nrcfacetpos$Company)
  combinationspos <- expand.grid(windowday = weekspos, Company = planspos)

  nrcfacetneg = full_join(nrcfacetneg, combinationsneg, by = c("windowday" = "windowday", "Company" = "Company")) %>%
    arrange(Company,windowday) %>%
    mutate(netsent = ifelse(is.na(netsent), -.0001, netsent)) %>%
    arrange(windowday)

  nrcfacetpos = full_join(nrcfacetpos, combinationspos, by = c("windowday" = "windowday", "Company" = "Company")) %>%
    arrange(Company,windowday) %>%
    mutate(netsent = ifelse(is.na(netsent), .0001, netsent)) %>%
    arrange(windowday)

  # plot2 = ggplot() +
  #   geom_area(data = nrcfacetneg, aes(x = windowday, y = netsent, fill = Company), position = "stack") +
  #   geom_area(data = nrcfacetpos, aes(x = windowday, y = netsent, fill = Company), position = "stack") +
  #   # geom_area(data = nrcfacetpos, aes(x = windowday, y = netsent, fill = Company), position = 'stack')
  #   # scale_y_reverse() +
  #   # geom_area(data = nrcfacetpos, aes(x = windowday, y = netsent, fill = Company))) +
  #   ggtitle("Sentiment Added By Top News Sources") +
  #   labs(caption=paste("Plot created:", Sys.Date())) +
  #   labs(x="Date", y="Net Sentiment Added")

  ## makes the scaling happen
  addition = news %>%
    select(Source, Date) %>%
    mutate(windowday = as.POSIXct(trunc.POSIXt(Date, units = c("days")))) %>%
    select(windowday, Source) %>%
    group_by(windowday) %>%
    summarise(n = n())

  themaxhelper =
    # nrcfacetneg %>%
    nrcfacetpos %>%
    group_by(windowday) %>%
    summarise(dailysent = sum(totalsent, na.rm = TRUE))

  themax = max(themaxhelper$dailysent)
  addition$scaledn = rescale(addition$n, to = c(min(addition$n),themax))

plot2 = ggplot() +
    geom_area(data = nrcfacetneg, aes(x = windowday, y = netsent, fill = Company), position = "stack") +
    geom_area(data = nrcfacetpos, aes(x = windowday, y = netsent, fill = Company), position = "stack") +
    geom_line(data = addition, aes(x = windowday, y = scaledn)) +
    # geom_area(data = nrcfacetpos, aes(x = windowday, y = netsent, fill = Company), position = 'stack')
    # scale_y_reverse() +
    # geom_area(data = nrcfacetpos, aes(x = windowday, y = netsent, fill = Company))) +
    ggtitle("News Sentiment by Source and Volume Over Time") +
    labs(caption=paste("Plot created:", Sys.Date())) +
    labs(x="Date", y="Net Sentiment Added") +
    scale_y_continuous(
      "Total Sentiment",
      sec.axis = sec_axis(~ . * max(addition$n)/max(addition$scaledn)
                          , name = "Total Articles")
    )

  plotlist = list(plot,plot2)

  return(plotlist)
}
