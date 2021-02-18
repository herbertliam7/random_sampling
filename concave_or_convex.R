#Here is an adaptation on the original polygon generating function, it has an additional output which states
# if the generated shape is convex or concave.

#The majority of the shapes generated are concave, unless the distances to the origin are very close or the same.

#This is the function seen before, but this one has been modified to include check.convex.
polygon.generator.2 <- function(number_of_points,  min_distance_to_origin, max_distance_to_origin) {
  max_distance_to_origin
  r <- runif(number_of_points,min_distance_to_origin,max_distance_to_origin)
  theta <- runif(number_of_points,-pi,pi)
  theta1 <- sort(theta)
  x <- r*cos(theta1)
  y <- r*sin(theta1)
  print(check.convex(x,y))
  plot(x,y,type='n')
  polygon(x,y)
}

#This function takes the sum of all the acute angles of the shape, and then compares this to what the sum
# of interior angles should be for tis particular shape.
check.convex <- function(x_vertices,y_vertices){
  x <- x_vertices
  y <- y_vertices
  i <- 1
  x <- c(x, x[1], x[2])
  y <- c(y, y[1], y[2])
  angles <- c()
  repeat{
    a <- distance.finder(x[i],y[i],x[i+2],y[i+2])
    b <- distance.finder(x[i],y[i],x[i+1],y[i+1])
    c <- distance.finder(x[i+1],y[i+1],x[i+2],y[i+2])
    p.1 <- b + c - a
    p.2 <- 2 * sqrt(b) * sqrt(c)
    angles <- c(angles, acos(p.1/p.2))
    i <- i+1
    if(i == length(x)-1)break
  }
  X <- abs(sum(angles)-((length(x)-4) * pi))
  if (X <= 0.001){
    return("Convex")
  }
  else{
    return("concave")
  }
}

#This function simply finds the length between any two points.
distance.finder <- function(x.1,y.1,x.2,y.2){
  return(((y.2-y.1)^2)+((x.2-x.1)^2))
}

#This shape will always be convex.
polygon.generator.2(10,10,10)

#This shape may or may not be convex.
polygon.generator.2(10,9,10)

#This shape is very unlikely to be convex.
polygon.generator.2(10,5,10)

