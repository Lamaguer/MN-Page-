#Metodo de Laguerre
coef <- c(1,0,1) #Es la formula (x2+1, pero solo en coeficientes.)
x0 <-1 #Punto de partida
iter <- 0
maxi<-100
tol <- 1e-6

n <- length(coef)-1  #Grado polinomial
x <- as.complex(x0) #Complex añade +0i para que no genere error al dar imaginario

while(iter<maxi){
    iter<-iter+1

    P <- as.complex(coef[1])
    dP <- as.complex(0)
    ddP <- as.complex(0)

    for(i in 2:(n+1)){ #Hacerlo al reves previene generar variables temporales
        ddP <- ddP * x +2 *dP
        dP <- dP*x + P
        P <- P*x + coef[i]
    }

    if(Mod(P)< tol){    #Se usa Mod para tomar en cuenta el numero imaginario
        break
    }

    G <- dP/ P
    H <- G^2 - (ddP/P)
    radicando<- (n-1)*(n*H -G^2)
    raiz_rad <- sqrt(radicando)

    den1 <- G+ raiz_rad
    den2 <- G- raiz_rad
    denominador <- if(Mod(den1)>= Mod(den2)){
        den1
    }else{ 
        den2
    }
    paso <- n/denominador
    x <- x - paso
    #Se compara paso ya que paso no es un numero complejo (i)
    if(Mod(paso) < tol){
        break
    }
}
    cat("Iteraciones totales: ",iter, "\n")
    cat("Valor de X:", sprintf("%.10f + %.10fi",Re(x), Im(x)), "\n")
