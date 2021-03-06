% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/removeSparseTerms.R
\name{removeSparseTerms}
\alias{removeSparseTerms}
\title{removeSparseTerms Remove Sparse Terms from a
Term-Document Matrix function original pakage tm}
\usage{
removeSparseTerms(x, sparse)
}
\arguments{
\item{x}{A \code{DocumentTermMatrix} or a \code{TermDocumentMatrix}}

\item{sparse}{A numeric for the maximal allowed sparsity in the range
from bigger zero to smaller one.}
}
\value{
term-document matrix where those terms from \code{x} are removed
which have at least a sparse percentage of empty
(i.e., terms occurring 0 times in a document) elements. I.e.,
the resulting matrix contains only terms with a sparse factor
of less than \code{sparse}.
}
\description{
removeSparseTerms Remove Sparse Terms from a
Term-Document Matrix function original pakage tm
}
\examples{
# data("crude")
#library(tm)
# tdm <- TermDocumentMatrix(crude)
# removeSparseTerms(tdm, 0.3)

}
