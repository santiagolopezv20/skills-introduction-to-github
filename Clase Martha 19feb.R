require(insuranceData)
require(tidyverse)
require(dplyr)
require(skimr)
require(ggplot2)

datos<-data("dataCar")
datos

table(dataCar$numclaims)

# Porcenyaje de polizas con reclamaciones 
reclamaciones<-sum(dataCar$numclaims>=0)
reclamaciones1<-sum(dataCar$numclaims>=1)

Porcentaje<-reclamaciones1/reclamaciones
Porcentaje*100

# TOP 5 vehiculos con mayor numero de reclamaciones 
sedan_coches<- subset(dataCar, veh_body== 'SEDAN')
count(sedan_coches)

num_sedan <- sum(dataCar$veh_body == "SEDAN")
num_sedan

reclamaciones_sedan<-num_sedan>=1

table(dataCar$numclaims)


bus_coches<- subset(dataCar, veh_body== 'BUS')
count(bus_coches)

convt_coches<- subset(dataCar, veh_body== 'CONVT')
count(convt_coches)


num_sedan1 <- (dataCar$veh_body == "SEDAN")
num_sedan1
dataCar[[num_sedan1]]


# En esta parte, filtramos los vehiculos que tienen las reclamaciones.
sedan<-filter(dataCar, num_sedan1)
sum(sedan$numclaims)

# Ahora, podemos sacar el top 5 de todos los vehiculos.

# Agrupar por tipo de vehículo y sumar el número de reclamaciones
top_vehiculos <- aggregate(numclaims ~ veh_body, data = dataCar, sum)

# Ordenar de mayor a menor número de reclamaciones
top_vehiculos <- top_vehiculos[order(-top_vehiculos$numclaims), ]

# Seleccionar el Top 5
top_5_vehiculos <- head(top_vehiculos, 5)

# Mostrar el Top 5
top_5_vehiculos



# Respuesta de Martha
claims_tipo <- dataCar%>%
  group_by(veh_body)%>%
  summarise(totclaims=sum(numclaims))%>%
  arrange(desc(totclaims))
# Para mostrar el top 5 que tenemos es
head(claims_tipo,5)

# Ahora, por el tipo de poliza que nos muestra, vamos a tener todos los coches.
pol_tipo <- dataCar%>%
  group_by(veh_body)%>%
  summarise(totpol=n())%>%
  arrange(desc(totpol))

# Pasemos a ggplot2, jugamos con los elementos
library(ggplot2)

ggplot(claims_tipo, aes(x = reorder(veh_body, -totclaims), y = totclaims)) +
  geom_bar(stat = "identity", fill = "#F0E68C", color = "#CD1076") +
  labs(title = "Grafica por el numero de reclamaciones", x = "Tipo de vehículo", y = "Total de Reclamaciones") +  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Top vehiculos con el mayor monto de reclamaciones
montoclaims_tipo <- dataCar%>%
  group_by(veh_body)%>%
  summarise(totmontoclaims=sum(claimcst0))%>%
  arrange(desc(totmontoclaims))

ggplot(montoclaims_tipo, aes(x = reorder(veh_body, -totmontoclaims), y = totmontoclaims)) +
  geom_bar(stat = "identity", fill = "#F0E68C", color = "#CD1076") +
  labs(title = "Grafica por el monto total de reclamaciones", x = "Tipo de vehículo", y = "Monto Total de Reclamaciones") +  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

head(montoclaims_tipo, 10)

#Para saber el numero de siniestros por genero
claims_genero <- dataCar%>%
  group_by(gender)%>%
  summarise(claimsgenero=sum(numclaims))%>%
  arrange(desc(claimsgenero))

#Para saber el numero de siniestros por genero y tipo de vehiculo
claims_genero <- dataCar%>%
  group_by(gender, veh_body)%>%
  summarise(claimsgeneroportipo=sum(numclaims))%>%
  arrange(desc(claimsgeneroportipo))

#Obtener la grafica para el numero de reclamaciones por genero y tipo de vehiculo
ggplot(claims_genero, aes(x = reorder(veh_body, -claimsgeneroportipo), y = claimsgeneroportipo, fill= gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Grafica por el total de reclamaciones por tipo de vehiculo", x = "Tipo de vehículo", y = "Total de Reclamaciones") +  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
