external_dependency_EXPLICIT <- function(mat){

  matrixStats::colMins(mat)

}


# LIBARY NON EXPLICIT CALL

library(matrixStats)
external_dependency_NON_explicit <- function(mat){

  colMins(mat)

}


#external_dependency(mat = matrix(1:9, nrow = 3))
