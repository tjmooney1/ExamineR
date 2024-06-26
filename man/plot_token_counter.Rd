% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_token_counter.R
\name{plot_token_counter}
\alias{plot_token_counter}
\title{plot_token_counter}
\usage{
plot_token_counter(data, text_var, n, text_size, fill = "")
}
\arguments{
\item{data}{A tibble or data frame object}

\item{text_var}{The text or message variable to analyse}

\item{n}{Amount of top terms to visualize}

\item{text_size}{How large should the y axis text be, this should also depend on the number of top terms selected}

\item{fill}{What color should the bar chart plot be}
}
\value{
Returns a simple bar chart displaying the top terms mentioned throughout any one message variable
}
\description{
Renders a simple bar chart showing the most frequent words in a text variable. Along with being able to choose how many of the top terms to visualize, the user has the option to change some aesthetics, such as y axis text size and fill color.
}
\examples{
plot_token_counter(data = data, text_var = message, n = 10, text_size = 15, fill = "steelblue")
}
