globalVariables(c("fill", "x", "y"))
#' Icons showing n in N people
#'
#' @import ggplot2
#' @import ggimage
#' @import utils
#'
#' @param n Number with the disease or condition
#' @param N Total number of people
#' @param icon Path to the icon (file or url)
#' @param additional (Optional) Number of additional patients with the risk due to a factor, for example
#' @param plt_size Size of the icon
#' @param icon_lib Library to fetch the icons from (One of 'file', 'RiskCom', or 'internet')
#' @param export (Optional) File to export the image to.
#' @param colours Colours for the icons
#'
#' @return GGPlot object
#' @export
#'
#' @examples
#' # Example showing 2 in 5 people with a disease.
#' ninN(n=2, N=5, icon="person", plt_size=.3)
#' # Example with a risk factor increasing the risk from 2 in 5 to 3 in 5
#' ninN(n=2, N=5, icon="person", additional=1, plt_size=.3)
ninN = function(n, N, icon, additional, plt_size=0.5, icon_lib="RiskCom", export, colours){
  if (icon_lib=="RiskCom"){
    icon = paste0(icon, ".png")
    icon = system.file("extdata", icon, package="RiskCom")
  }

  theme_set(theme_gray(base_family = 'mono' ))

  img = rep(icon, N)
  if (missing(additional)){
    if (missing(colours)){
      colours = c("black", "lightgrey")
    }
    colour = c(rep("group1", n),rep("group2", N-n))
    df <- data.frame(x = 1:N, y = 1, img = img, fill = colour)
    plt = ggplot(df) +
      geom_image(aes(x = x, y = y, image = img, colour=fill), size=plt_size)+
      scale_colour_manual(values=colours)+
      xlim(c(0.5,N+.5))+
      theme_void() +
      theme(legend.position = "none")
  } else{
    if (missing(colours)){
      colours = c("black", "red", "lightgrey")
    }
    colour = c(rep("group1", n), rep("group2", additional), rep("group3", N-n-additional))
    df <- data.frame(x = 1:N, y = 1, img = img, fill = colour)
    plt = ggplot(df) +
      geom_image(aes(x = x, y = y, image = img, colour=fill), size=plt_size)+
      scale_colour_manual(values=colours)+
      xlim(c(0.5,N+.5))+
      theme_void() +
      theme(legend.position = "none", text=element_text(size=16))
  }
  if (!missing(export)){
    ggsave(export, height=1,width=N)
  }
  plt
}
