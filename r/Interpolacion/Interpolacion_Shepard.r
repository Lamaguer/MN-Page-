# Metodo de Interpolación de Shepard

j <- as.numeric(readline(prompt="Ingrese cantidad de puntos a interpolar"))
print("Ingrese las coordenadas del punto de consulta:")
x_consulta <- as.numeric(readline(prompt="Eje X"))
y_consulta <- as.numeric(readline(prompt="Eje Y"))
p <- as.numeric(readline(prompt="Exponente (P)"))

x_pts <- numeric(j)
y_pts <- numeric(j)
z_pts <- numeric(j)

for(i in 1:j){
    cat("Punto [",i,"]")
    x_pts[i] <- as.numeric(readline(prompt= paste("X del punto [",i,"]")))
    y_pts[i] <- as.numeric(readline(prompt= paste("Y del punto [",i,"]")))
    z_pts[i] <- as.numeric(readline(prompt= paste("Z del punto [",i,"]")))
}

dists <- sqrt((x_consulta-x_pts)^2 + (y_consulta-y_pts)^2)

coincidencia <- which(dists==0)
if (length(coincidencia) >0){
    return(z_pts[coincidencia])
}

pesos <- 1/(dists^p)

Sz <- sum(pesos*z_pts)/sum(pesos)

cat("El valor interpolado de todos los puntos es: ", Sz)