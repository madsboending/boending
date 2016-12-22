#' Hurtig udregning af Net Promotor Score
#'
#' Funktionen udregner net promotor score.
#' @param x er en integer som udelukkende indeholder nps værdier 1-10 .
#' @keywords net promotor score
#' @export
#' @examples
#' #eksempel
#' dat <- c(1,3,2,4,1,4,8,9,10,10,9,9,8,10,6,10)
#' 
#' nps(dat)
#' 

nps <- function(data=x, y){
  promotors <- length(x[y>8,])/length(x$y)
  
  detractors <- length(x[y<7,])/length(x$y)
  
  result <- promotors-detractors
  return(result)
}

