% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/waffles.R
\name{waffles}
\alias{waffles}
\title{Waffle plot creator}
\usage{
waffles(
  icons,
  legend,
  percentages,
  icon_lib = "RiskCom",
  icons2,
  legend2,
  export,
  icon_size = 0.1,
  icon_size2 = 0.04,
  legend_size = 0.5,
  dir_size = "height",
  offset_x = -0.25,
  offset_y = -0.25
)
}
\arguments{
\item{icons}{Vector of icon paths}

\item{legend}{Vector of icon descriptions corresponding to icons}

\item{percentages}{Vector of percentages of each icon (must sum to 100)}

\item{icon_lib}{Library to fetch the icons from (One of 'file', 'RiskCom', or 'internet')}

\item{icons2}{Optional additional level of icons}

\item{legend2}{Optional additional legends for the second icons}

\item{export}{(Optional) File path and name to export to}

\item{icon_size}{Size of the main icons}

\item{icon_size2}{Size of the additional icons}

\item{legend_size}{Size of the icon legends}

\item{dir_size}{Direction of the size ('height' or 'width')}

\item{offset_x}{Offset for the additional icons}

\item{offset_y}{Offset for the additional icons}
}
\value{
GGPlot object
}
\description{
Waffle plot creator
}
\examples{

# Single waffle plot
icons = c("happy",
  "neutral",
  "sad")
legend = c("Happy", "Neutral", "Sad")
percentages = c(35, 45, 20)
waffles(icons, legend, percentages)

# Double waffle plot using icons from the internet
# Icons2 can be different but the same are used here
icons1 = c("person", "person_grey")
icons2 = c("happy", "neutral", "sad")
legend1 = c("No disease", "Disease")
legend2 = c("Happy", "Neutral", "Sad")
percentages = c(70,9,1, 3, 5, 12)
waffles(icons=icons1, legend=legend1, percentages,
        icons2=icons2, legend2=legend2)
}
