df = read.csv2('titanic_es.csv')

#1. ¿Cuántos pasajeros aparecen registrados en el conjunto de datos?

num_pasajeros = length(df$clase)
print(num_pasajeros)

#2. ¿Cuántos pasajeros sobrevivieron al naufragio?

sobrevivientes = sum(df$sobreviviente)
print(sobrevivientes)

#3. Calcula el porcentaje de pasajeros que sobrevivió al naufragio.
porcentaje_sobrevivientes = sobrevivientes/num_pasajeros*100
print(porcentaje_sobrevivientes)

#4. ¿Cuántos pasajeros viajaban en primera clase?
pasajeros_primera = sum(df$clase=="primera")
print(pasajeros_primera)

#5. ¿Cuántos niños (menores de 12 años) aparecen registrados?
pasajeros_niños = sum(df$edad < 12)
print(pasajeros_niños)

#6. Calcula las frecuencias relativas para las distintas clases de pasajeros.
fabs = table(df$clase) # Frecuencia absoluta
frel = fabs/sum(fabs) # Frecuencia relativa
print (frel)



#7. Representa con un gráfico adecuado la distribución de las distintas clases de pasajeros.
barplot(frel, xlab = 'Clase', ylab = 'Frecuencia relativa', ylim = c(0,0.5), main="Frecuencias relativas para las distintas clases")

#8. Representa con un gráfico adecuado la distribución de la edad de los pasajeros registrados.
hist(df$edad)

#9. ¿Cuál era la edad media de los pasajeros? ¿Y la mediana?
edad_media = mean(df$edad)
print (edad_media)

edad_mediana = median(df$edad)
print (edad_mediana)

#10. ¿Cuál era el precio medio de un pasaje en el Titanic?
precio_medio = mean(df$tarifa)
print(precio_medio)

#11 a. ¿qué porcentaje de mujeres adultas sobrevivió al naufragio?
mujeres_adultas_sobrevivientes = sum((df$edad >= 12) & (df$sexo == "mujer") & (df$sobreviviente == 1))
mujeres_adultas =  sum((df$edad >= 12) & (df$sexo == "mujer"))
porcentaje_mujeres_sobrevivientes = mujeres_adultas_sobrevivientes/mujeres_adultas*100

print(mujeres_adultas_sobrevivientes)
print(mujeres_adultas)
print (porcentaje_mujeres_sobrevivientes)

#11 b. ¿qué porcentaje de hombres adultos sobrevivió al naufragio?
hombres_adultos_sobrevivientes = sum((df$edad >= 12) & (df$sexo == "hombre") & (df$sobreviviente == 1))
hombres_adultos =  sum((df$edad >= 12) & (df$sexo == "hombre"))
porcentaje_hombres_sobrevivientes = hombres_adultos_sobrevivientes/hombres_adultos*100

print(hombres_adultos_sobrevivientes)
print(hombres_adultos)
print (porcentaje_hombres_sobrevivientes)

#11 c. ¿qué porcentaje de niños/as sobrevivió al naufragio?
niños_sobrevivientes = sum((df$edad < 12) & (df$sobreviviente == 1))
niños =  sum(df$edad < 12)
porcentaje_niños_sobrevivientes = niños_sobrevivientes/niños*100

print(niños_sobrevivientes)
print(niños)
print (porcentaje_niños_sobrevivientes)

#12. ¿Es la tasa de supervivencia de los hombres que viajaban en primera clase superior a la de los hombres
#que viajaban en segunda? ¿Es superior a las de los hombres que viajaban en tercera clase?

hombres_adultos_primera = sum((df$edad >= 12) & (df$sexo == "hombre") & (df$clase == "primera"))
hombres_adultos_primera_sobrevivientes = sum((df$edad >= 12) & (df$sexo == "hombre") & (df$clase == "primera") & (df$sobreviviente == 1))
print (hombres_adultos_primera_sobrevivientes/hombres_adultos_primera*100)  

hombres_adultos_segunda = sum((df$edad >= 12) & (df$sexo == "hombre") & (df$clase == "segunda"))
hombres_adultos_segunda_sobrevivientes = sum((df$edad >= 12) & (df$sexo == "hombre") & (df$clase == "segunda") & (df$sobreviviente == 1))
print (hombres_adultos_segunda_sobrevivientes/hombres_adultos_segunda*100)

hombres_adultos_tercera = sum((df$edad >= 12) & (df$sexo == "hombre") & (df$clase == "tercera"))
hombres_adultos_tercera_sobrevivientes = sum((df$edad >= 12) & (df$sexo == "hombre") & (df$clase == "tercera") & (df$sobreviviente == 1))
print (hombres_adultos_tercera_sobrevivientes/hombres_adultos_tercera*100)

#13. Representa mediante un diagrama de cajas las edades de los pasajeros agrupadas por clase. ¿Qué
#observas?
boxplot(df$edad ~ df$clase)

#14. Calcula la media y la varianza de las edades en cada clase.
tapply(df$edad, df$clase, mean)
tapply(df$edad, df$clase, var)

#15. En el barco se proporcionaba asistencia especial al 5% de los pasajeros de mayor edad. ¿Qué edad
#debía tener un pasajero para recibir asistencia especial? 
  
pdec = seq(0, 1, by = 0.05)
quantile(df$edad, pdec)

#¿Qué edad tenía el pasajero más mayor?
max (df$edad)

#16. Dibuja un histograma para el precio del pasaje en primera clase.
df_primera = subset(df, clase == "primera", select = c("tarifa"))
hist(df_primera$tarifa )

#17. ¿Cuál es la moda para el puerto de embarque?
frec_embarque = table(df$embarque)
barplot(frec_embarque)

#18. Los pasajeros que pagaban menos por su billete debían dormir en los camarotes más modestos del barco.
#En concreto, el 5% de los pasajeros que menos pagaban viajaban en dichos camarotes. ¿Cuál era el
#precio máximo que se pagaba por uno de estos camarotes?

prdec = seq(0, 1, by = 0.05)
quantile(df$tarifa, prdec)

#19. Construye una tabla de contingencia para la distribución de la clase del pasajero por sexo.
tabla = table(df$clase, df$sexo)
tabla_prop = prop.table(tabla, margin= 1)
tabla_sin_prop = prop.table(tabla)

#20. Representa en un mismo gráfico la distribución de la clase del pasajero por sexo.

mosaicplot(tabla, cex = 1.1, col = 3:5)
barplot(tabla, beside = TRUE, col = 3:5)
legend("topright", legend = c("primera", "segunda", "tercera"), fill = 3:5)

#21. Calcula los cuartiles para la edad
pedaddec = seq(0, 1, by = 0.25)
quantile(df$edad, pedaddec)

#22. ¿Pagaban billete los menores de un año?
df_menores_1=subset(df, edad < 1, select=c("tarifa", "edad") )

sum(df_menores_1$tarifa)
