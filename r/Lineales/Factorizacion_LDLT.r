#Factorizacion LDL T 

A <- matrix(c(4,1,
              1,3),nrow=2, byrow=TRUE)
B <- c(1,2)

n<-nrow(A)
if(n!=ncol(A) || !isSymmetric(A)){
    stop("Matriz A tiene que ser cuadrada y simétrica.")
}

L <- diag(1,n) #Crea una matriz de tamaño n con 1 en la diagonal y 0 en todo lo demoas
D <- numeric(n) #Crea un vector de n tamaño vacio.

for(j in 1:n){
    #Diagonal D
    suma_d <- 0
    if(j >1){
     for(k in 1:(j-1)){ #D se construye usando la sumatoria
        suma_d <- suma_d + (L[j,k]^2)*D[k] 
     }   
    }
    D[j] <- A[j,j] - suma_d
    if(D[j]==0){
        stop("Pivote nulo encontrado en D[",j,"]. No se puede tener pivote nulo en este metodo.")
    }
    #Elementos inferiores L
    if(j<n){
        for(i in (j+1):n){
            sumal <- 0
            if(j> 1){
                for(k in 1:(j-1)){
                    sumal<- sumal + L[i,k]*L[j,k] * D[k]
                }
            }
            L[i,j] <- (A[i,j]- sumal)/ D[j]
        }
    }
}
#Lz =B
z <- numeric(n)
for(i in 1:n){
    suma <-0
    if(i >1){
        for(j in 1:(i-1)){
            suma <- suma + L[i,j]*z[j]
        }
    }
    z[i] <- B[i]-suma
}   
#Dw = z
w<- z/D

#LTx = w
x<-numeric(n)
for(i in n:1){
    suma <-0
    if(i <n){
        for(j in (i+1):n){
            suma <- suma + L[j,i]*x[j]
        }
    }
    x[i] <- w[i] - suma
}

cat("Solucion x:", sprintf("%.6f", x), "\n")
cat("Diagonal D:", sprintf("%.6f", D), "\n")
