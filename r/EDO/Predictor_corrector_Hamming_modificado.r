# Predictor corrector de Hamming modificado
# y1, y2 y 3 obtenidas por Runge-Kutta 4to Orden.

# y' = 5yt - 1
f <- function(t,y){return(5*y*t - 1)}
t0 <- 0
tf <- 1
y0 <- 2
n <- 10

if(n<4){
    stop("Hamming modificado requiere al menos n=4 pasos")
}

h <-(tf-t0)/n
t <- seq(t0,tf,by =h)
y <- numeric(n+1)
y[1] <- y0

#Runge-Kutta 4t0 Orden
for(k in 1:3){
    tk <- t[k]
    yk <- y[k]

    k1 <- f(tk, yk)
    k2 <- f(tk+h / 2, yk + (h/2) * k1)
    k3 <- f(tk+h / 2, yk + (h/2) * k2)
    k4 <- f(tk + h, yk + h * k3)
    y[k+1] <- yk + (h/6) * (k1 +  2*k2 + 2*k3 + k4)
}

p <- numeric(n+1)
c <- numeric(n+1)
#La diferencia previa inicial siempre inicia en 0 
#al ser el primer recorrimiento del metodo.
diff_prev <- 0 

#Predictor corrector de Hamming modificado
for(k in 4:n){
    fk <- f(t[k],y[k])
    fkm1 <- f(t[k-1],y[k-1])
    fkm2 <- f(t[k-2],y[k-2])
    
    #Predictor
    pk <- y[k-3] + (4*h /3) * (2*fk - fkm1 + 2*fkm2)
    #Predictor modificado
    mk <- pk + (112/121) * diff_prev
    #Corrector
    fm <- f(t[k+1], mk)
    ck <- (9 * y[k] - y[k-2])/ 8 + (3*h / 8) * (2*fk - fkm1 + fm)

    y[k+1] <- ck + (9/121) * (pk - ck)

    #Actualizar para el siguiente ciclo.
    p[k+1] <- pk
    c[k+1] <- ck
    diff_prev <- ck - pk
}

res <- data.frame(t=t, y=y)
cat("Resultado de los pasos hasta t = 1.0: \n")
print(res)