\name{superpc.listfeatures}
\alias{superpc.listfeatures}
\title{ Return a list of the important predictors }
\description{
 Return a list of the important predictor
}
\usage{
superpc.listfeatures(data, train.obj, fitred, component.number, shrinkage)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{Data object }
  \item{train.obj}{Object returned by superpc.train}
  \item{fitred}{Object returned by superpc.predict.red}
  \item{component.number}{ Index of principal component to use (1,2, or 3) }
  \item{shrinkage}{Amount of shrinkage  }
 }
}
\details{
}
\value{
Returns matrix of features and their importance scores, in order
of decreasing absolute value of importance score. The importance score
is the correlation of the reduced predictor and the full supervised PC
predictor. It also lists the raw score- for survival data, this is the Cox score
for that feature; for regression, it is the standardized regression coefficient.
}
\author{Eric Bair and Rob Tibshirani}


\examples{
#generate some data

x<-matrix(rnorm(1000*40),ncol=40)
y<-10+svd(x[1:60,])$v[,1]+ .1*rnorm(40)
ytest<-10+svd(x[1:60,])$v[,1]+ .1*rnorm(40)
status<- sample(c(rep(1,30),rep(0,10)))
status.test<- sample(c(rep(1,30),rep(0,10)))
featurenames <- paste("feature",as.character(1:1000),sep="")
data<-list(x=x,y=y, status=status, featurenames=featurenames)
data.test<-list(x=x,y=ytest, status=status.test, featurenames= featurenames)

a<- superpc.train(data, type="survival")

fit<- superpc.predict(a, data, data.test, threshold=1.0, n.components=1, prediction.type="continuous")

fit.red<- superpc.predict.red(a,data, data.test, .6)
superpc.listfeatures(data, a,  fit.red,1,shrinkage=0.3)

}
\keyword{regression}% at least one, from doc/KEYWORDS
\keyword{survival}% __ONLY ONE__ keyword per line