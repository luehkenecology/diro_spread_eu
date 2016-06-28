matrix_to_raster <- function(x, aim){
  x2 <- t(x)
  
  if(nrow(x2) == 1){
    x3 <- t(x2)
    (setValues(aim, x3))
  } else {
    x3 <- lapply(1:ncol(x2), function(y) 
      setValues(aim, x2[,y]))
    (brick(unlist(x3)))
  }
}