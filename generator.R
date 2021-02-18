#This function generates a random shape using polar coordinates, the center is always the origin.
polygon.generator <- function(number_of_points,  min_distance_to_origin, max_distance_to_origin) {
  max_distance_to_origin
  r <- runif(number_of_points,min_distance_to_origin,max_distance_to_origin)
  theta <- runif(number_of_points,-pi,pi)
  theta1 <- sort(theta)
  x <- r*cos(theta1)
  y <- r*sin(theta1)
  plot(x,y,type='n')
  polygon(x,y)

  
}

#The paramaters are as follows:
# number_of_points, the amount of vertices you want the shape to have.
# min_distance_to_origin, the closest you want a point to be to the origin.
# max_distance_to_origin, the furthest you want a point to be to the origin.

#Some observations to make:
#If you keep the min_distance and max_distance the same/similar, the shape you get will be convex.
#As you increase the number of points, the shape will begin to resemble a circle.

#Wide range (5-10)
polygon.generator(10,5,10)
#Smaller range (9-10)
polygon.generator(10,9,10)
#Range is (9-10), points much higher. (kind of a circle)
polygon.generator(100,9,10)
#Range = 10
polygon.generator(10,10,10)
#Range = 10, number of vertices much higher.
polygon.generator(100,10,10)