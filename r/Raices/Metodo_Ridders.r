    f <- function(x) x^3 - x -2
    # a <- as.numeric(readline(prompt="Ingrese el valor de Extremo A: "))
    # b <- as.numeric(readline(prompt="Ingrese el valor de Extremo B: "))
    a <- 1
    b <- 2
    fa <- f(a)
    fb <- f(b)
    maxi <-100
    if((fa*fb)>=0){stop("El intervalo [a,b] no encierra un cambio de signo.")}
    tol <- 1e-6 #Margen de error
    iter <- 0
    while(iter<maxi){
        iter <- iter+1
        m <- (a+b)/2
        fm <- f(m)

        s <- sqrt(fm^2 - (fa*fb))
        if (s==0) {break} #Evita inf al dividir por 0 

        signo <- sign(fa - fb)
        x <- m + (m-a)*((signo*fm)/s)
        fx <- f(x)

        #Detener antes del maximo numero de iteraciones
        if(abs(fx)< tol || abs(b-a) < tol){
            break
        }

        if(fa*fx <0){ #Hay cambio entre a y x? [a,x]
            b <- x
            fb <- fx
        } else if (fm*fx< 0) { #Hay cambio entre punto medio y x? [m,x]
        a <-m
        fa <- fm
        b <- x
        fb <- fx

        }else{ #Descarte, cambio tiene que estar entre x y b [x,b]
            a <-x
            fa <-fx
        }
    }
        cat("Iteraciones totales: ",iter, "\n")
        cat("Valor de X:", sprintf("%.10f",x), "\n")
        cat("Valor de fx", sprintf("%.10f",fx), "\n")