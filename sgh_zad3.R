#http://michal.ramsza.org/lectures/2_r_programming/proj2/project_2.html?fbclid=IwAR3A51c-gC2Pjma0s7iQEThnLQKpWtsiEpeTkOojQij454BYAyCheArCi1Y


######### given funcs
f1 <- function(x){
  c( sin( x[1] + x[2]), cos( x[2]) + sin( x[1]))
}
f2 <- function( x){
  c(
    log( 1/2 + x[1]^2),
    sqrt( abs( x[2] * x[1]))
  )
}

f3 <- function( x){
  c(
    sqrt( 1/2 + x[1]^2) + rnorm( 2, 0, 1/10),
    sin( x[2] + x[1])
  )
}

f4 <- function( x){
  z <- x[1] + x[2] * ( 0 + 1i)
  phi <- Arg( z)
  r <- Mod( z)
  nphi <- phi + pi/36
  if( nphi > pi){
    nphi <- -2 * pi + nphi
  }
  r * rnorm( 1, 1, 1/50)  * c( cos( nphi), sin( nphi))
}
##############
createPath <- function(f1, x, iter){
  
  list_of_points = list()
  list_of_points[[1]] = x
  
  # easier?
  for( i in 2:iter){
    temp = f1(x)
    list_of_points[[i]] = temp
    x = temp
  }
  
  rownames = 1:iter
  colnames = c("x", "y")
  d = list(rownames, colnames)
  list_of_points = data.frame(matrix(unlist(list_of_points), ncol = 2, byrow = TRUE,dimnames = d),stringsAsFactors=FALSE)
  
  return(list_of_points)
}


x = c(1, 1)
o = createPath(f1, x, 20)
plot(o$x, o$y, type="l")


check2 = createPath( f2, c( 1, 1), 100)
plot(check2$x, check2$y, type="l")

check3 = createPath( f3, c( 1, 1), 300)####### wtf?!
plot(check3$x, check3$y, type="l")


check4 = createPath( f4, c( 1, 0), 1000)
plot(check4$x, check4$y, type="l")
