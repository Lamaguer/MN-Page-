z<-0
dats<-0
z<- as.numeric(readline(prompt="Ingrese el valor de z: "))
dats<- as.numeric(readline(prompt="Ingrese la cantidad de datos a interpolar: "))
if(dats<3){
    cat("ERROR, El número de datos debe ser mayor o igual a 3 para interpolar con este metodo.")
}else{
    vectorx <- vector("numeric", dats)
    vectory <- vector("numeric", dats)
    for(i in 1:dats){
        vectorx[i] <- as.numeric(readline(prompt = paste("Ingrese el valor de x",i, ": ")))
        vectory[i] <- as.numeric(readline(prompt = paste("Ingrese el valor de y",i, ": ")))
    }
    h <- diff(vectorx)
    d <- diff(vectory) / h
    n_int <- dats-2
    A <- matrix(0, nrow = n_int, ncol = n_int)
    B <- numeric(n_int)

    for(i in 1:n_int){
        A[i,i] <- 2 * (h[i]+h[i+1])
        if(i > 1) A[i, i-1 ] <- h[i]
        if(i < n_int) A[i, i+1] <- h[i+1]

        B[i] <- 6 * (d[i+1] - d[i])
    }

    M_int <- solve(A,B)
    M <- c(0, M_int, 0)

    #En que subintervalo se encuentra z
    i <- max(which(vectorx <= z))
    if(i==dats){
        i <- dats-1
    }

    h_i <- vectorx[i+1] - vectorx[i]
    A_rel <- (vectorx[i+1]- z)/h_i
    B_rel <- (z - vectorx[i])/h_i

    Sz <- A_rel*vectory[i] + B_rel*vectory[i+1] + (h_i^2 / 6)*((A_rel^3 - A_rel)* M[i] + (B_rel^3 - B_rel)*M[i+1])
    
    cat("Valor interpolado de Z:", Sz)
}