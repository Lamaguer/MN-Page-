#Reduccion ciclica

# 2x1 -x2 + 0x3 = 1
# -x1 +2x2 -x3 = 0
# 0x1+ -x2 + 2x3 = 1

a <- c(0,-1,-1)
d <- c(2,2,2)
c <- c(-1,-1,0)
b <- c(1,0,1)
n<- length(d)
if(length(a)!= n || length(b) !=n || length(c) !=n ){
    stop("Los vectores necesitan ser del a,d,c y b deben de tener el mismo tamaño que n.")
}
#Estos valores deben de ser 0 porque el metodo necesita un sistema tridiagonal
a[1] <- 0
c[n] <- 0

#inicializacion
d_red <- numeric(n)
b_red <- numeric(n)
a_red <- numeric(n)
c_red <- numeric(n)

for (i in seq(1,n, by=2)){
    if(i-1>=1){ #Checa si hay vecino izquierda
        alpha <- -a[i]/d[i-1]
        c_prev <- c[i-1]
        b_prev <- b[i-1]
        a_prev <- a[i-1]
    }else{
        alpha <-  0
        c_prev <- 0
        b_prev <- 0
        a_prev <- 0
    }
    if(i+1 <=n){ #NCheca si no hay vecino derecho
        beta <- -c[i] / d[i+1]
        a_next <- a[i+1]
        b_next <- b[i+1]
        c_next <- c[i+1]
    }else{
        beta <- 0
        a_next <- 0
        b_next <- 0
        c_next <- 0
    }
    #red de reducida 
    d_red[i] <- d[i] + alpha * c_prev + beta * a_next
    b_red[i] <- b[i] + alpha * b_prev + beta * b_next
    a_red[i] <- alpha * a_prev
    c_red[i] <- beta * c_next
}

x <- numeric(n)
#Regla de Cramer
det <- d_red[1] * d_red[3] - c_red[1] * a_red[3]
x[1] <- (b_red[1]*d_red[3] - c_red[1] * b_red[3])/det
x[3] <- (d_red[1]*b_red[3] - b_red[1] * a_red[3])/det

#Reconstruccion regresiva
for(i in seq(2,n,by=2)){
    x[i] <- (b[i]-a[i] * x[i-1] - c[i]*x[i+1]) / d[i]
}

cat("Solucion x:", sprintf("%.4f",x ),"\n")