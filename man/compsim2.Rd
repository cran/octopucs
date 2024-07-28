\name{compsim2}
\alias{compsim2}
\title{
Complementarity and disimilarity
}
\description{
Calculates the complementaritry and dissimilarity between two 
or more communities base on .
}
\usage{
compsim2(M)
}

\arguments{
 \item{M}{
A species times site matrix.
}
 
}

\value{
Generates a disimilarity matrix.
}
\references{
Aguirre, A., Guevara, R., García, M., & López, J. C. (2010). Fate of
epiphytes on phorophytes with different architectural characteristics
 along the perturbation gradient of Sabal mexicana forests in Veracruz, 
 Mexico. Journal of Vegetation Science, 21(1), 6-15.
 https://doi.org/10.1111/j.1654-1103.2009.01131.x
}
\author{
Roger Guevara (roger.guevara@inecol.mx)
}

\examples{
data(dune)
M2 <- compsim2(t(dune[1:2,]))
M2
}