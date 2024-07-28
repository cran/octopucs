\name{glzcluster2}
\alias{glzcluster2}
\title{
Generalized Cluster
}
\description{
Generates a cluster hypothesis based on the transformation of 
the 1-prop ratio of the cells in the matrix's body into their corresponding 
column averages.
}
\usage{
glzcluster2(data=data, n = NULL, prop=NULL, distance =NULL, method=NULL, 
Name=NULL)
}

\arguments{
 \item{data}{
A matrix object of r rows that will be gropued by c columns.
}
  \item{n}{
The number of subsets to be generated is based on which the generalized 
cluster will be generated.
}
  \item{prop}{
Either the proportion of the c columns used in every run to produce a new 
grouping array or the 1-prop number of cells in the body of the matrix whose 
values are replaced by the corresponding column average. This will be 
automatically decided based on the total number of columns in the matrix. 
When there are less than 19 columns in the matrix, the procedure will replace 
cell values with column averages even if the resample is TRUE.
}

  \item{distance}{
Distance method to be used (default="bray"). Options include those of the 
function "vegdist" ("vegan"): "manhattan", "euclidean", "canberra", "bray", 
"kulczynski", "gower", "morisita", "horn", "mountford", "jaccard", "raup", 
"binomial", "chao", "altGower", "cao" and "mahalanobis", as well as those 
referred in Koleff et al. (2003), available either by name or number: 1 or "w", 
2 or "-1", 3 or "c", 4 or "wb", 5 or "r", 6 or "I", 7 or "e", 8 or "t", 9 or 
"me", 10 or "j", 11 or "sor", 12 or "m", 13 or "-2", 14 or "co", 15 or "cc", 
16 or "g", 17 or "-3", 18 or "1", 19 or "", 20 or "hk", 21 or "rlb", 22 or 
"sim", 23 or "gl", 24 or "z". For open distance metrics (not restricted to 
zero-one ranges), the axis is re-scaled to cover a zero-one range. If so, the 
axis will be labeled a "Similarity (rescaled)"; if no re-scaling is necessary, 
the label will appear as "Similarity".
}
  \item{method}{
Agglomerative procedure for hierarchical clustering, including: "average" 
(=UPGMA), "ward.D", "ward.D2", "single", "complete", "mcquitty" (=WPGMA), 
"median" (=WPGMC) or "centroid" (=UPGMC).
}
  \item{Name}{
Name of the  matrix object to be analyzed
}
}

\value{
Generates a list object that contains the n cluster assembled with 
random subsets of the data, and in the n+1 position the generalized cluster, 
an hclust output
}
\references{
Koleff, P., Gaston, K.J., Lennon, J. J. 2003. Measuring beta diversity for 
presence-absence data. Journal of Animal Ecology 71:367.
https://doi.org/10.1046/j.1365-2656.2003.00710.x
}
\author{
Roger Guevara (roger.guevara@inecol.mx) & 
Ian MacGregor-Fors (ian.macgregor@helsinki.fi)
}
\examples{
data(dune)
M2 <- glzcluster(dune, n=100,
prop=0.8, distance="bray",
method="average")
length(M2)
ls(M2[[length(M2)]])
}
