globalVariables(c("img1", "img2", "icon", "img"))
#' Probability represented by a single bar
#'
#' @import ggplot2
#' @param probabilities Vector of probabilities
#' @param categories Vector of category names
#' @param colours Colours of the categories
#'
#' @return GGPlot object
#' @export
#'
#' @examples
#'
#' bar_prob(c("Yes", "No"), c(80,20),
#'         colours=c("green", "red"))
#' bar_prob(c("Yes", "Maybe", "No"), c(50,30,20),
#'         colours=c("green", "orange", "red"))
#'

bar_prob = function(categories, probabilities, colours=NULL){
  df = data.frame(fill = categories, y=probabilities, x=1)
  df$fill = factor(df$fill, levels=categories)
  plt = ggplot(df, aes(x=x, y=y))+
    geom_col(aes(fill=fill), show.legend = TRUE)+
    coord_flip()+
    scale_y_continuous(trans="reverse")+
    # annotate("text", x = 1, y = probabilities[1]/2,
    #          label = paste0(probabilities[1], "%"))+
    # annotate("text", x = 1, y = 100-probabilities[2]/2,
    #          label = paste0(probabilities[2], "%"))+
    theme_void()+
    theme(legend.position = "bottom")+
    labs(fill="")
  if(!is.null(colours)){
    plt=plt+scale_fill_manual(values=colours)
  }
  plt
}
