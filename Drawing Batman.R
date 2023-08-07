f1a <- function(x){
  #3*sqrt(1-(x/7)^2)
  3 * sqrt(1-(x/7)^2)
}

f1b <- function(x){
  #-3*sqrt(1-(x/7)^2)
  -3 * sqrt(1-(x/7)^2)
}

f2 <- function(x){
  abs(x/2)-0.09137221*x^2-3+sqrt(1-(abs(abs(x)-2)-1)^2)
}

f3 <- function(x){
  9-8*abs(x)
}

f4 <- function(x){
  3*abs(x)+0.75
}

f5 <- function(x){
  rep(2.25, times=length(x))
}

f6 <- function(x){
  2.710524+(1.5-0.5*abs(x))-1.355262*sqrt(4-(abs(x)-1)^2)
}

x1 <- c(seq(-3,-7,length.out=1000), seq(3,7,length.out=1000))
y1 <- c(f1a(x1), f1b(x1))
x1 <- c(x1, x1)
x1 <- x1[which(y1>(-3*sqrt(33)/7))]
y1 <- y1[which(y1>(-3*sqrt(33)/7))]


x2 <- seq(from=-4,to=4,length.out=1000)
y2 <- f2(x2)

x3 <- c(seq(-1,-0.75, length.out=100),seq(0.75,1, length.out=100))
y3 <- f3(x3)

x4 <- c(seq(-0.75,-0.5, length.out=100), seq(0.5, 0.75, length.out=100))
y4 <- f4(x4)

x5 <- seq(-0.5, 0.5, length.out=200)
y5 <- f5(x5)

x6 <- c(seq(-3, -1, length.out=1000), seq(1,3,length.out=1000))
y6 <- f6(x6)


plot(x1,y1, xlim=c(-8,8), ylim=c(-4,4))
#points(c(seq(-7,-3,length.out=length(y1b)/2), seq(3,7,length.out=length(y1b)/2)),y1b)
points(x2,y2)
points(x3,y3)
points(x4,y4)
points(x5,y5)
points(x6,y6)