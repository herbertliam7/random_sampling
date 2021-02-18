#This is the function that will check to see if a point (p.x,p.y) is inside a shape.
#The parameters x_vertices and y_vertices are the ones of the polygon you are testing.
#Note: The points must be given in the order you want the shape to be drawn
# and you must include the first coordinate at the end too. An example is given below.
inside.polygon <- function(x_vertices,y_vertices,p.x,p.y) {
#Initialise crossings and vertices
count <- 0
#Check Plot Works
#plot(x_vertices,y_vertices)
#polygon(x_vertices,y_vertices)

y_c <- 0
i <- 1
while(i <= (length(x_vertices)-1)){
repeat {
if (x_vertices[i] < p.x && x_vertices[i+1] < p.x){
  i <- i + 1
  break
}
if (x_vertices[i] > p.x && x_vertices[i+1] > p.x){
  i <- i + 1
  break
} 
if (y_vertices[i] < p.y && y_vertices[i+1] < p.y){
  i <- i + 1
  break
}
if (y_vertices[i] > p.y && y_vertices[i+1] > p.y){
  count <- count + 1
  i <- i + 1
  break
}
if (y_vertices[i] > p.y && y_vertices[i+1] < p.y){
  y_c <- y_vertices[i+1] + (((y_vertices[i]-y_vertices[i+1])/(x_vertices[i]-x_vertices[i+1]))*(p.x-x_vertices[i+1]))
  if (y_c > p.y){
    count <- count + 1
    i <- i + 1
    break
  }
  else {
    i <- i + 1
    break
  }
}
if (y_vertices[i] < p.y && y_vertices[i+1] > p.y){
  y_c <- y_vertices[i] + (((y_vertices[i+1]-y_vertices[i])/(x_vertices[i+1]-x_vertices[i]))*(p.x-x_vertices[i]))
  if (y_c > p.y){
    count <- count + 1
    i <- i + 1
    break  
  }
  else {
    i <- i + 1
    break
  }
}
}
}
if (count %% 2 == 0){
  #print("point is NOT INSIDE")
  return(0)
}
else {
  #print("point is INSIDE")
  return(1)
}
}
#The function will return 1 if the point is inside, 0 if it is not.

#Here is a shape, drawn and connected up using the polygon function.
plot(c(0,0,5,3,5), c(0,5,5,2,2))
polygon(c(0,0,5,3,5,5), c(0,5,5,2,2,0))

#Note that here, when calling the function, the first coordinate is included again at the end.
inside.polygon(c(0,0,5,3,5,5),c(0,5,5,2,2,0),2,4)
inside.polygon(c(0,0,5,3,5,5),c(0,5,5,2,2,0),5,3)


#Here an example of the monte carlo estimate is given, and how to use it with the function to obtain the area of the shape.
x_coords <- runif(50000,0,5)
y_coords <- runif(50000,0,5)
#We use 0-5 because the total area of the square the shape is enclosed in is 5x5.

counter <- 0
#Here we check how many of the random coordinates are inside the shape.
for (i in 1:50000) {
  if (inside.polygon(c(0,0,5,3,5,5),c(0,5,5,2,2,0),x_coords[i],y_coords[i]) == 1)
  {
    counter <- counter + 1
  }
}
#Here we calculate the percentage of points that have fallen inside the shape drawn, to obtain an estimate of its area
# in relation to the overarching 5x5 square.
(counter/50000) * 25
