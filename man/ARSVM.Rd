\name{ARSVM}
\alias{ARSVM}
\title{
Auto-Regressive Support Vector Machine
}

\description{
The ARSVM function fit Auto-Regressive Support Vector Machine  for univariate
time series data.
}

\usage{
ARSVM(data,h)
}

\arguments{
  \item{data}{
Input univariate time series (ts) data.}
\item{h}{
The forecast horizon.
}
}

\details{
This package allows you to fit the Auto-Regressive Support Vector Machine for
univariate time series.
}

\value{
  \item{Optimum lag}{Optimum lag of the considered data}
  \item{Model Summary}{Summary of the fitted SVM}
  \item{Weights}{weights of the fitted SVM }
  \item{Constant}{Constant of the fitted SVM}
  \item{MAPE}{Mean Absolute Percentage Error (MAPE) of the SVM}
  \item{RMSE}{Root Mean Square Error (RMSE) of fitted SVM}
  \item{fitted }{Fitted values of SVM}
   \item{forecasted.values}{h step ahead forecasted values employing SVM}
}

\references{
Kim, K.(2003). Financial time series forecasting using support vector machines,
55(1-2), 307-319.

}
\author{
Mrinmoy Ray,Samir Barman, Kanchan Sinha, K. N. Singh
}

\seealso{
SVM
}

\examples{
data=lynx
ARSVM(data,5)
}

\keyword{SVM}
