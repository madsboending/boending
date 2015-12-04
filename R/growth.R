#' Udregning af vækstkategori baseret på udvikling i omsætning og ansatte
#'
#' Funktionen udregner vækstkategorien for henholdsvis omsætningsvækst, samt beskæftigelsesvækst.
#' @param x er en numerisk variabel.
#' @param y er en integer, der angiver antallet år, som vækstkategoriseringen skal foretages på .
#' @keywords vækstkategori
#' @export
#' @examples
#' #data eksempel (fra ggplot2 examples)
#' df <- data.frame(gp = factor(rep(letters[1:3], each = 10)), y = rnorm(30))
#' 
#' #plot eksempel (fra ggplot2 examples)
#' myplot <- ggplot(df, aes(x = gp, y = y))+geom_point()
#' 
#' kilde(myplot, 'Kilde: Væksthus Nordjylland, September 2015')

growth <- function(x, y, z){
abs.v <- x-y 
  
prop.v <- dplyr::mutate(z, vk=if(abs.v>1000) {(x-y)/y} else {NA})
return(prop.v)
}

growth(v.dat1$oms_1, v.dat1$oms_2, v.dat1)


abs.v <- 'vkst'
if(v.dat1$oms_1-v.dat1$oms_2>1000){v.dat1$oms_1-v.dat1$oms_2/v.dat1$oms_2 
}

v.dat1$oms_1-v.dat1$oms_2




growth <- function(x, y) {
  txt <- gridExtra::arrangeGrob(sub = grid::textGrob(y, x = 0, hjust = -0.12, vjust=-1.1, gp = grid::gpar(fontface = "italic", fontsize = 11)))
  
  growth <- gridExtra::grid.arrange(bottom=txt, x, ncol=1)
  return(growth)
}

