\name{octopucs}
\alias{octopucs}
\title{
Support for Hierarchical Clusters
}
\description{
The global function of the "octopucs" package is to estimate support for 
hierarchical clusters throughout the dendrogram's topology, considering their 
repeatability, integrity, and contamination based on bootstrapping procedures.
}
\usage{
octopucs(data, n=NULL, prop=NULL, resample = TRUE, distance =NULL, 
method=NULL, orientation=NULL, disp = NULL, lab.pos=NULL, 
sep.lab=NULL, xlim=NULL, ylim=NULL, verbose=NULL)
}

\arguments{
  \item{data}{
A matrix whose rows (sites/conditions)  will be grouped based on the columns 
(species/elements).
}
  \item{n}{
The number of bootstrap repetitions.
}
  \item{prop}{
Proportion of information used in the bootstrapping procedures. When matrices 
contain more than 18 columns, it refers to (i) the proportion of columns used 
in every run to produce a new grouping array when "resample=TRUE" and (ii) the 
number of cells in the body of the matrix (1–prop; e.g., 1–0.8=0.2) that will 
be replaced with column averages when "resample=FALSE". When matrices contain 
less than 18 columns, the procedure with "resample=FALSE" will be forced. In 
such a scenario, "prop" corresponds to the number of cells in the body of the 
matrix (1–prop; e.g., 1–0.8=0.2) that will be replaced in the bootstrapping 
procedures.
}
  \item{resample}{
When TRUE, "octopucs" will select random subsets of columns in every bootstrap 
run to produce a new grouping array. When FALSE, "octopucs" will replace cells 
in the body of the matrix with column averages instead of entire columns (see 
description for "prop" for further details).
}
  \item{distance}{
Distance method to be used (default="bray"). Options include those of the 
function "vegdist" ("vegan"): "manhattan", "euclidean", "canberra", "bray", 
"kulczynski", "gower", "morisita", "horn", "mountford", "jaccard", "raup", 
"binomial", "chao", "altGower", "cao" and "mahalanobis", as well as those 
referred in Koleff et al. (2003), available either by name or number: 1 or "w", 
2 or "-1", 3 or "c", 4 or "wb", 5 or "r", 6 or "I", 7 or "e", 8 or "t", 9 or 
"me", 10 or "j", 11 or "sor", 12 or "m", 13 or "-2", 14 or "co", 15 or "cc", 16 
or "g", 17 or "-3", 18 or "1", 19 or "", 20 or "hk", 21 or "rlb", 22 or "sim", 
23 or "gl", 24 or "z". For open distance metrics (not restricted to zero-one 
ranges), the axis is re-scaled to cover a zero-one range. If so, the axis will 
be labeled a "Similarity (rescaled)"; if no re-scaling is necessary, the label 
will appear as "Similarity".
}
  \item{method}{
Agglomerative procedure for hierarchical clustering, including: "average" 
(=UPGMA), "ward.D", "ward.D2", "single", "complete", "mcquitty" (=WPGMA), 
"median" (=WPGMC) or "centroid" (=UPGMC).
}

  \item{orientation}{
Orientation of the plot (default="r": right; metric in the x-axis is 
similarity). "l" = left-wise orientation (metric in the x-axis is 
dissimilarity). "d" = down-wise orientation (metric in the y-axis is 
dissimilarity).
}
 \item{disp}{
Result display in the dendrogram (default="sig"; only supported nodes will 
populated with given results). "all" = all nodes will be populated with support 
metrics, with significantly supported nodes in blue/red and non-significantly 
supported nodes in black.
}
  \item{lab.pos}{
This argument modifies the position of the labels relative to the tips of the 
dendrogram (sites/conditions).
}
  \item{sep.lab}{
This argument modifies the position of supporting values from the nodes and 
lines in the dendrogram.
}
  \item{xlim}{
Limits of the x-axis.
}
  \item{ylim}{
Limits of the y-axis.
}
  \item{verbose}{
TRUE = display all relevant setup values used in the analysis. FALSE = display 
only the dendrogram with marked significantly grouped clusters.
}
}

\value{
"octopucs" generates a list object that contains:
\item{$gcluster}{hclust output of the generalized clustered dendrogram.}
\item{$support}{Table with the support value for every group and subgroup 
across the topology of the generalized clustered dendrogram.}
\item{$grps}{List with the memberships of every group and subgroup in the 
topology.}
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
octopucs(dune, n=100, prop=0.8,
resample=TRUE,distance="bray",
method="average", disp = "sig",
verbose=TRUE)
}

\keyword{community ecology}
\keyword{cluster}
