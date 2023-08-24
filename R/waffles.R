globalVariables(c("img1", "img2", "icon", "img"))
#' Waffle plot creator
#'
#' @import ggplot2
#' @import ggimage
#' @import flextable
#' @import grid
#' @import gridExtra
#' @import patchwork
#' @import utils
#'
#' @param icons Vector of icon paths
#' @param legend Vector of icon descriptions corresponding to icons
#' @param percentages Vector of percentages of each icon (must sum to 100)
#' @param icon_lib Library to fetch the icons from (One of 'file', 'RiskCom', or 'internet')
#' @param icons2 Optional additional level of icons
#' @param legend2 Optional additional legends for the second icons
#' @param icon_size Size of the main icons
#' @param icon_size2 Size of the additional icons
#' @param legend_size Size of the icon legends
#' @param dir_size Direction of the size ('height' or 'width')
#' @param offset_x Offset for the additional icons
#' @param offset_y Offset for the additional icons
#' @param export (Optional) File path and name to export to
#'
#' @return GGPlot object
#' @export
#'
#' @examples
#'
#' # Single waffle plot
#' icons = c("happy",
#'   "neutral",
#'   "sad")
#' legend = c("Happy", "Neutral", "Sad")
#' percentages = c(35, 45, 20)
#' waffles(icons, legend, percentages)
#'
#' # Double waffle plot using icons from the internet
#' # Icons2 can be different but the same are used here
#' icons1 = c("person", "person_grey")
#' icons2 = c("happy", "neutral", "sad")
#' legend1 = c("No disease", "Disease")
#' legend2 = c("Happy", "Neutral", "Sad")
#' percentages = c(70,9,1, 3, 5, 12)
#' waffles(icons=icons1, legend=legend1, percentages,
#'         icons2=icons2, legend2=legend2)
waffles = function(icons, legend, percentages, icon_lib='RiskCom',
                   icons2, legend2, export,
                   icon_size=0.1, icon_size2=0.04, legend_size=0.5, dir_size="height",
                   offset_x=-0.25, offset_y=-0.25){

  # The next line is very important. R seems to struggle with fonts and without
  # this line flextable tried to use the 'Ariel' font. On my machine, this was
  # 'TT Ariel'. This line tells R to just use whichever is the sans font on the
  # machine. This should hopefully work across platforms and machines.
  set_flextable_defaults(font.family="sans")

  if (sum(percentages)!=100){ # This makes sure that the percentages sum to 100.
    stop("The percentages must be a vector of integers which sum to 100.
         Each percent will be an individual icon on the plot.")
  }
  if (missing(icons2)){ # If 'icons2' is not specified then use the single waffle plot
    if (icon_lib=="RiskCom"){ # If using the built-in library
      icons = paste0(icons, ".png")
      icons = system.file("extdata", icons, package="RiskCom")
    }
    if (icon_lib == "internet"){ # If using images from the internet
      icons = unname(sapply(icons, save_temp_images))
    }
    plt = waffles1(icons, legend, percentages,
             icon_size, legend_size, dir_size) # Call the single waffle plot function
  } else {
    if (icon_lib == "internet"){ # If using images from the internet
      icons = unname(sapply(icons, save_temp_images))
      icons2 = unname(sapply(icons2, save_temp_images))
    }
    if (icon_lib=="RiskCom"){ # If using the built-in library
      icons = paste0(icons, ".png")
      icons = system.file("extdata", icons, package="RiskCom")
      icons2 = paste0(icons2, ".png")
      icons2 = system.file("extdata", icons2, package="RiskCom")
    }
    plt = waffles2(icons=icons, legend=legend, percentages=percentages,
             icons2=icons2, legend2=legend2,
             icon_size=icon_size, icon_size2, legend_size, dir_size,
             offset_x, offset_y) # Call the double waffle plot function
  }
  if (!missing(export)){ # If a file name to export to is specified
    ggsave(export, height=5,width=7) # Save
  }
  plt = plt +
    theme(text=element_text(size=16)) +
    theme_void()


  plt # Return the plot
}

# The single waffle plot function
waffles1 = function(icons, legend, percentages,
                    icon_size, legend_size, dir_size){
  y = rep(1:10,10) # Creating a grid of 10-by-1
  x = c(rep(1,10), rep(2,10), rep(3,10), rep(4,10), rep(5,10),
        rep(6,10), rep(7,10), rep(8,10), rep(9,10), rep(10,10))
  df = c()
  for (i in c(1:length(icons))){
    df = c(df, rep(icons[i], percentages[i]))
  }
  df <- data.frame(x = x, y = y, img = df)
  plt = ggplot(df) +
    geom_image(aes(x = x, y = y, image = img), size=icon_size, by=dir_size)+   scale_y_reverse()+
    theme_void()

  ft_legend = data.frame(
    icon=icons,
    label=legend
  )
  ft = flextable(ft_legend, col_keys=c("empty","icon", "label"))
  ft = compose(ft,
               j="icon",
               value=as_paragraph(
                 as_image(src=icon, width=legend_size, height=legend_size)
               ))
  ft = set_header_labels(ft, values=c("empty"="","icon"="", "label"=""))
  ft = border_remove(ft)
  plt+
    gen_grob(ft)+
    plot_layout(ncol=2, widths=c(5,2))+
    theme(text=element_text(size=16))+
    theme_void()

}

waffles2 = function(icons, legend, percentages, icons2, legend2,
                    icon_size, icon_size2, legend_size, dir_size,
                    offset_x, offset_y){
  y = rep(1:10,10)
  x = c(rep(1,10), rep(2,10), rep(3,10), rep(4,10), rep(5,10),
        rep(6,10), rep(7,10), rep(8,10), rep(9,10), rep(10,10))
  icons_list = list(icons2, icons)
  icons_grid = expand.grid(icons_list)
  legend_list = list(legend2, legend)
  legend_grid = expand.grid(legend_list)
  df_icon1 = c()
  df_icon2 = c()
  for (i in c(1:length(icons_grid$Var1))){
    df_icon1 = c(df_icon1, as.character(unlist(rep(icons_grid$Var2[i], percentages[i]))))
    df_icon2 = c(df_icon2, as.character(unlist(rep(icons_grid$Var1[i], percentages[i]))))
  }
  df1 <- data.frame(x = x, y = y, img1 = df_icon1, img2 = df_icon2)
  plt = ggplot(df1) +
    geom_image(aes(x = x, y = y, image = img1), size=icon_size, by=dir_size)+   scale_y_reverse()+
    geom_image(aes(x = x+offset_x, y = y+offset_y, image = img2), size=icon_size2, by=dir_size)+   scale_y_reverse()+
    theme_void()
  ft_legend = data.frame(
    icon=unlist(icons_list),
    label=unlist(legend_list)
  )
  ft = flextable(ft_legend, col_keys=c("empty","icon", "label"))
  ft = compose(ft,
               j="icon",
               value=as_paragraph(
                 as_image(src=icon, width=legend_size, height=legend_size)
               ))
  ft = set_header_labels(ft, values=c("empty"="","icon"="", "label"=""))
  ft = border_remove(ft)
  plt+
    gen_grob(ft)+
    plot_layout(ncol=2, widths=c(5,2))+

    theme(text=element_text(size=16))
}

save_temp_images <- function(img){
  f <- tempfile(fileext=".png")
  download.file(img, f, mode = 'wb')
  f
}
