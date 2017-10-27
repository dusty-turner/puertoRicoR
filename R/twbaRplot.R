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
