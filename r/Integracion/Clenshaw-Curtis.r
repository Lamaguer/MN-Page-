#Metodo Clenshaw-Curtis
#Funcion f(x):
# n=5
fx <- function(x){ return(x^4)}
a<- -1 #Limite inferior
b<- 1 #Limite superior
n <- 5

if (n<3){
    stop("Clenshaw Curtis requiere n=3 nodos como minimo.")
}

N <- n-1
j <- 0:N

x <- cos(j*pi/ N)
w <- numeric(n)
kmax <- floor((N-1)/2)

for (i in 1:(n)){
    j_val <- j[i]
    #Si es esquina es 0.5, si no 1.
    deltaj <- if(j_val ==0 || j_val==N) 0.5 else 1.0 

    suma <- 0
    if(kmax >= 1 ){
        for(k in 1:kmax){
            suma <- suma + (2*cos(2*k * j_val * pi / N)) / (4*k^2 - 1)
        }
    }
    Ej <- if(N %% 2 ==0) cos(j_val * pi)/(N^2 - 1) else 0

    w[i] <- (2*deltaj / N) * (1 - suma - Ej)
}
#Transformacion de nodos al intervalo a,b
t <- ((a+b)/2) + ((b-a)/2)*x

res <- ((b-a)/2) * sum(w* fx(t))

cat("Resultado del metodo con n=",n,": ",sprintf("%.8f",res),"\n")