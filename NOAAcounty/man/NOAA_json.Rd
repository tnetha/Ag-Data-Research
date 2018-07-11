\name{NOAA_json}
\alias{NOAA_json}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Download weather data from NOAA and import it as a dataframe
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
NOAA_json(fips_codes, Start_date, End_date, Weather_element)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fips_codes}{
%%     ~~Describe \code{fips_codes} here~~
A vector of county FIPS codes
}
  \item{Start_date}{
%%     ~~Describe \code{Start_date} here~~
First day of data to download
}
  \item{End_date}{
%%     ~~Describe \code{End_date} here~~
Last day of data to download
}
  \item{Weather_element}{
%%     ~~Describe \code{Weather_element} here~~
Weather element to download, mean is taken if multiple stations exist in a county
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
Tyler Netherly
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
NOAA_json(c(48001, 48003), '2005-01-01', '2005-12-31', 'pcpn')


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
