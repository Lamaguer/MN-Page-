#Adam Bashforth de dos pasos
#Primer paso sera Runge-Kutta 4th Orden.

# y' = 5yt - 1
f <- function(t,y){return(5*y*t - 1)
}
t0 <- 0
tf <- 1
y0 <- 2
n <- 10
if(n <2){
    stop("Se requieren al menos n=2 pasos")
}

h <- (tf-t0)/n
t <- seq(t0,tf, by=h)
y <- numeric(n+1)

y[1] <- y0

#RK4 Para obtener y1 en t1
k1 <- f(t[1], y[1])
k2 <- f(t[1] + h/2, y[1]+ (h/2) * k1)
k3 <- f(t[1] + h/2, y[1]+ (h/2) * k2)
k4 <- f(t[1] + h, y[1] + h * k3)

y[2] <- y[1] + (h/6) * (k1 * 2 * k2 * 2 * k3 + k4)

# Adam Bashforth
for( k in 2:n){
    fk <- f(t[k], y[k])
    fkant <- f(t[k-1], y[k-1])

    y[k+1] <- y[k] + (h/2) * (3 * fk - fkant)
}

res <- data.frame(t=t, y=y)
cat("Resultado de los pasos hasta t = 1.0: \n")
print(res)