#' this helper function
#'
#' helpers:
#' @param filename "Puerto Rico 23 Sept - 26 OCT best.csv"
#' @keywords Puerto Ricos
#' @export
#' @examples
#' helpers()

helpmes = function(filename = "best.csv"){


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
