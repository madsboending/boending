
#' Plot flere diagrammer på én side.
#'
#' Funktionen gør det muligt at plotte flere diagrammer på samme side. Funktionen er skamløst kopieret fra http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
#' @param ..., ggplot objects
#' @param plotlist,  en liste af ggplot objects
#' @param cols, antallet af kolonner i layoutet
#' @param layout, matrix som specificere layoutet. Hvis layout benyttes ignoreres 'cols'
#' @keywords multiplot
#' @export
#' @examples
#' #data eksempel (fra ggplot2 examples)
#' df <- data.frame(gp = factor(rep(letters[1:3], each = 10)), y = rnorm(30))
#' 
#' #plot eksempel (fra ggplot2 examples)
#' plot1 <- ggplot(df, aes(x = gp, y = y))+geom_point()
#' 
#' plot2 <- ggplot(df, aes(x = gp, y = y))+geom_boxplot()
#'  
#' 
#' multiplot(plot1, plot2, ncols=2)
#' 
#'Hvis layoutet  f.eks. er matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
#' så vil plot 1 være til venstre øverst, 2 vil være til højre øverst, og
#' 3 vil sprede sig nederst.
#' 

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

