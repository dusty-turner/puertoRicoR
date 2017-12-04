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


pRtwitter = function(searchterm = "Puerto Rico",
                     n = 100000,
                     since = '2017-10-25',
                     until = '2017-10-26',
                     olddataname = "Puerto Rico 23 Sept - 25 OCT best.csv",
                     newdataname = "Puerto Rico 23 Sept - 26 OCT best.csv"){

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
    df.tweet$created = as.POSIXct(df.tweet$created,format = "%Y-%m-%d %H:%M", tz = "UTC")
    # df.tweet$created = as.POSIXct(df.tweet$created,format = "%m/%d/%Y %H:%M", tz = "UTC")
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
    df.tweet = df.tweet %>% distinct(text, created, screenName, .keep_all = TRUE)
    # df.tweet = distinct((full_join(df.tweet, motherofalldataframes, by = c("text", "created"))))
    df.tweet = df.tweet[which(!is.na(df.tweet$text)),]
    write.csv(df.tweet, newdataname)
    return(paste("Your New Data Has Been Added to your Old File", newdataname))

  } else if(file.exists(olddataname)==FALSE){
    df = system.file("extdata", "best.csv", package = "puertoRicoR")
    df.tweet = read.csv(df)
    # df.tweet$created = as.POSIXct(df.tweet$created,format = "%m/%d/%Y %H:%M", tz = "UTC")
    df.tweet$created = as.POSIXct(df.tweet$created,format = "%Y-%m-%d %H:%M", tz = "UTC")
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
