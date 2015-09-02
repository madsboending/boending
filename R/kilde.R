#' Kildeangivelse på diagrammer
#'
#' Funktionen gør det nemt at angive kilde på et diagram.
#' @param x er et ggplot2 diagram.
#' @param y er en tekst string indeholdende kildeangivelsen.
#' @keywords kildeangivelse
#' @export
#' @examples
#' #data eksempel (fra ggplot2 examples)
#' df <- data.frame(gp = factor(rep(letters[1:3], each = 10)), y = rnorm(30))
#' 
#' #plot eksempel (fra ggplot2 examples)
#' myplot <- ggplot(df, aes(x = gp, y = y))+geom_point()
#' 
#' kilde(myplot, 'Kilde: Væksthus Nordjylland, September 2015')

kilde <- function(x, y) {
  txt <- gridExtra::arrangeGrob(sub = grid::textGrob(y, x = 0, hjust = -0.12, vjust=-1.1, gp = grid::gpar(fontface = "italic", fontsize = 11)))
  
  kilde <- gridExtra::grid.arrange(bottom=txt, x, ncol=1)
  return(kilde)
}

