#Algortimo de descenso de gradiente

#Función objetivo
f = function(x, y)(0.5*(x**2 +y**2))

#Definimos el vector gradiente
dfx = function(x,y)(x)
dfy = function(x,y)(y)

#Fijamos el paso t
t=0.5

#Valor inicial
x0 = 1
y0 = 1
f0 = f(x0, y0)

#Bucle general del algoritmo
niter= 10
iter = 0
criterio = 0

while ((iter < niter) & (criterio==0)){
  iter = iter + 1
  deltax=dfx(x0, y0)
  deltay=dfy(x0, y0)
  
  x1=x0-t*deltax
  y1=y0-t*deltay
  
  f1 = f(x1, y1)
  cat("iter", iter, "x1", x1, "y1", y1,"x0", x0, "y0", y0, "f0", f0, "f1", f1, "\n" )
  
  if (abs(f0-f1)<1/(10**9))(criterio=1)
  
  
  x0=x1
  y0=y1
  f0=f1

}
  
  

  



