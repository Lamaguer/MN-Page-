#Gauss Chebyshev de primera especie
#Funcion g(x)=x^2
# n=2

gx <- function(x){ return(x^2)}
n <- 2

if (n <= 0) {
  stop("n Necesita ser un entero positivo.")
}

k <- 1:n
angulos <- ((2*k -1)*pi)/(2*n)
nodos <- cos(angulos)
evaluaciones <- gx(nodos)
res <- (pi/n) * sum(evaluaciones)

cat("Resultado del metodo con n=",n,": ",sprintf("%.6f",res),"\n")