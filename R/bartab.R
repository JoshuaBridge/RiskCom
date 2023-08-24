globalVariables(c("freq"))
#' Bar plot with a table below
#'
#' @import ggplot2
#' @import ggimage
#' @import flextable
#' @import grid
#' @import gridExtra
#' @import patchwork
#' @import stringr
#' @import utils
#'
#' @param vals Vector of the x-axis factors.
#' @param xlab Label for the x-axis
#' @param ylab Label for the y-axis
#' @param vals2 (Optional) Vector of the additional factors.
#' @param flab (Optional) Labels for the extra factors.
#' @param missing Whether a column for missing values is needed.
#'
#' @return GGPlot object
#' @export
#'
#' @examples
#' data(mtcars)
#' cyl = factor(mtcars$cyl)
#' gears = factor(mtcars$gear)
#' bartab(vals = cyl, xlab = "Cylinders", ylab = "Frequency")
#' bartab(vals = cyl, xlab = "Cylinders", ylab = "Frequency",
#'        vals2 = gears, flab = "Gears")
bartab = function(vals, xlab, ylab,
                   vals2, flab, missing=FALSE){

  set_flextable_defaults(font.family = "sans")

  if (missing(vals2)){
    bartab1(vals, xlab, ylab)
  }
  else {
    bartab2(vals, vals2, xlab, ylab, flab, missing=missing)
  }
}

bartab1 = function(vals, xlab, ylab){
  if (!is.factor(vals)){
    vals = factor(vals)
  }
  tab = data.frame("vals" = vals)
  tab = proc_freq(tab, "vals")
  tab = set_header_labels(tab, vals=xlab, Count=ylab)
  df = data.frame("vals"=tab$body$dataset$vals, "freq"=tab$body$dataset$count)
  df = df[df$vals!="Total",]
  ggplot(data=df, aes(x=vals, y=freq))+
    geom_bar(stat="identity", fill=(1:length(unique(vals)))+1)+
    ylab(ylab)+
    xlab(xlab)+
    scale_x_discrete(labels=function(x) str_wrap(x,width=10))+
    gen_grob(tab)+
    plot_layout(nrow=2, heights=c(2,1))
}

bartab2 = function(vals1, vals2, xlab, ylab, flab, missing=TRUE){
  set_flextable_defaults(na_str="0", nan_str="0")
  if (!is.factor(vals1)){
    vals1 = factor(vals1)
  }
  if (!is.factor(vals2)){
    vals1 = factor(vals2)
  }
  tab = data.frame(x=vals1, y=vals2)
  colnames(tab)=c(xlab, flab)
  tab = proc_freq(tab, row=xlab, col=flab,
                  include.row_percent = F,
                  include.column_percent = F,
                  include.table_percent = F)
  if (missing){
    bar_x = rep(c(levels(vals1), "Missing"), each=length(unique(vals2)))
    tab2 = table(vals1, vals2, useNA = 'ifany')
    bar_y = as.numeric(t(tab2))
    bar_f = rep(c(levels(vals2),"Missing"), times=length(unique(vals1)))
    dat=data.frame("vals1"=factor(bar_x, levels=c(levels(vals1), "Missing")),
                   "vals2"=factor(bar_f, levels=c(levels(vals2), "Missing")),
                   "freq"=bar_y)
  }
  else {
    bar_x = rep(levels(vals1), each=length(unique(vals2)))
    tab2 = table(vals1, vals2, useNA = 'ifany')
    bar_y = as.numeric(t(tab2))
    bar_f = rep(levels(vals2), times=length(unique(vals1)))
    dat=data.frame("vals1"=factor(bar_x, levels=levels(vals1)),
                   "vals2"=factor(bar_f, levels=levels(vals2)),
                   "freq"=bar_y)
  }
  ggplot(dat, aes(fill=vals2, y=freq, x=vals1))+
    geom_bar(position = "dodge", stat="identity")+
    labs(x=xlab, y=ylab, fill=flab)+
    scale_x_discrete(labels=function(x) str_wrap(x,width=10))+
    gen_grob(tab)+
    plot_layout(nrow=2, heights=c(2,1))
}
