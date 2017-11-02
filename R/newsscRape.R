#' update news
#'
#' updatenews: This function updates your news search for "Puerto Rico" and saves it to the working directory
#' @keywords Google News
#' @export
#' @examples
#' newspull

newsscRape = function(oldfile = "R_News_1NOV17.csv", newfile = "R_News_1NOV17.csv"){

library(rvest)
library(lubridate)
library(stringi)
library(stringr)
library(tidyverse)

google <- c("https://www.google.com/search?q=Puerto+Rico&num=100&tbs=sbd:1,qdr:m&tbm=nws&source=lnt&sa=X&ved=0ahUKEwj6wKTm6YnXAhUIHJQKHTU1CzsQpwUIHw&biw=1922&bih=951&dpr=1",
            "https://www.google.com/search?q=Puerto+Rico&num=100&tbs=sbd:1,qdr:m&tbm=nws&ei=tW_wWcPNNIKXwQSE9bHQDg&start=100&sa=N&biw=1914&bih=947&dpr=1",
            "https://www.google.com/search?q=Puerto+Rico&num=100&tbs=sbd:1,qdr:m&tbm=nws&ei=yG_wWez2I4GWwATqoaRw&start=200&sa=N&biw=1914&bih=947&dpr=1",
            "https://www.google.com/search?q=Puerto+Rico&num=100&tbs=sbd:1,qdr:m&tbm=nws&ei=HXDwWfzwJ8SDwQSA06joDg&start=300&sa=N&biw=1914&bih=947&dpr=1",
            "https://www.google.com/search?q=Puerto+Rico&num=100&tbs=sbd:1,qdr:m&tbm=nws&ei=T3DwWZMux43CBJTEogg&start=400&sa=N&biw=1914&bih=947&dpr=1",
            "https://www.google.com/search?q=Puerto+Rico&num=100&tbs=sbd:1,qdr:m&tbm=nws&ei=YHDwWZMggv7ABNj7udAE&start=500&sa=N&biw=1914&bih=947&dpr=1",
            "https://www.google.com/search?q=Puerto+Rico&num=100&tbs=sbd:1,qdr:m&tbm=nws&ei=cXDwWc7AOML8wQTjnbDYBg&start=600&sa=N&biw=1914&bih=947&dpr=1",
            "https://www.google.com/search?q=Puerto+Rico&num=100&tbs=sbd:1,qdr:m&tbm=nws&ei=hnDwWeeKCoP0wASPoImgCQ&start=700&sa=N&biw=1914&bih=947&dpr=1")
            # "https://www.google.com/search?q=Puerto+Rico&num=100&tbs=sbd:1,qdr:m&tbm=nws&ei=kHDwWcGnF8K3wATP1JvADg&start=800&sa=N&biw=1914&bih=947&dpr=1",
            # "https://www.google.com/search?q=Puerto+Rico&num=100&tbs=sbd:1,qdr:m&tbm=nws&ei=kHDwWcGnF8K3wATP1JvADg&start=800&sa=N&biw=1914&bih=947&dpr=1")

# google <- c("https://www.google.com/search?q=Puerto+Rico&num=100&tbs=sbd:1,qdr:m&tbm=nws&ei=ES71We7sHMre0gLFlZHYCQ&start=0&sa=N&biw=1366&bih=637&dpr=1",
#             "https://www.google.com/search?q=Puerto+Rico&num=100&tbs=sbd:1,qdr:m&tbm=nws&ei=Fy71WfOBLYKJ0wLlkp2gCg&start=100&sa=N&biw=1366&bih=637&dpr=1",
#             "https://www.google.com/search?q=Puerto+Rico&num=100&tbs=sbd:1,qdr:m&tbm=nws&ei=QS71WYKqKIiW0gLT_Y64Cg&start=200&sa=N&biw=1366&bih=637&dpr=1")


##headlines
headlinelist = list()
blurblist = list()
originslist = list()

for(i in seq_along(google)){
  headlines <- google[i] %>%
    read_html() %>%
    html_nodes(".r") %>%
    html_text()
  Sys.sleep(sample(10, 1) * 0.1)
  blurb <- google[i] %>%
    read_html() %>%
    html_nodes(".st") %>%
    html_text()
  Sys.sleep(sample(10, 1) * 0.1)
  origins <- google[i] %>%
    read_html() %>%
    html_nodes(".slp , ._gJs") %>%
    html_text()
  headlinelist[[i]] = headlines
  blurblist[[i]] = blurb
  originslist[[i]] = origins
  Sys.sleep(sample(10, 1) * 1)
}
headlines = unlist(headlinelist)
blurb = unlist(blurblist)
origins = unlist(originslist)


dateloc = unlist(str_split(origins, " - "))
Date1 = dateloc[c(FALSE,TRUE)]
Source = dateloc[c(TRUE,FALSE)]

# vec = rep(as_date(format(Sys.time(), "%m %d %Y %Z", tz = "UTC"), "%m %d %Y"),length(headlines))
vec = rep(Sys.time(), length(headlines))

number1 = ifelse(stri_detect_fixed(Date1, "minute"),gsub("([0-9]+).*$", "\\1", Date1),
                 ifelse(stri_detect_fixed(Date1, "hour"),gsub("([0-9]+).*$", "\\1", Date1),
                        ifelse(stri_detect_fixed(Date1, "day"),gsub("([0-9]+).*$", "\\1", Date1),NA)))

number = ifelse(stri_detect_fixed(Date1, "minute"),as.numeric(number1)*60,
                ifelse(stri_detect_fixed(Date1, "hour"),as.numeric(number1)*60*60,
                       ifelse(stri_detect_fixed(Date1, "day"),as.numeric(number1)*24*60*60,number1)))


Date = trunc.POSIXt(vec[1:length(number)] - as.numeric(number), "days")
Date = as.POSIXct(Date)

Date[is.na(Date)] = as.Date(Date1[is.na(Date)], "%b %d, %Y") + days(1)
Date = trunc.POSIXt(Date, "days")
Date = as.POSIXct(Date)
googleheadlines = data_frame(Source = Source, Date = Date, Title = headlines, Narrative = blurb)

tail(dateloc)

news = read.csv(oldfile)



news$Date = as_date(news$Date)

news = news[,-1]

test = full_join(news, googleheadlines, by = c("Source", "Title", "Narrative"))
test$Date = as.POSIXlt(ifelse(is.na(test$Date.x),as.character(test$Date.y),as.character(test$Date.x)))
test = test[,c(1,2,3,6)]

test = unique(test)

write.csv(test, newfile)

return(paste("google scrape complete", newfile, "saved with", nrow(test), "rows"))

}

