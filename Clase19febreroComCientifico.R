require(insuranceData)
require(tidyverse)
require(dplyr)
require(skimr)
require(ggplot2)
install.packages("gridExtra")
install.packages("grid")
library(gridExtra)
library(grid)

datos<-data("dataCar")

table(dataCar$numclaims) #Hacemos una tabla para saber el numero de polizas que tenemos por numero de reclamaciones

# Porcentaje de polizas con reclamaciones 
totalPolizas<-sum(dataCar$numclaims>=0)
reclamaciones<-sum(dataCar$numclaims>=1)

Porcentaje<-reclamaciones/totalPolizas #Saber el porcentaje de polizas que tuvieron reclamaciones
Porcentaje*100

#Vamos a obtener el numero de reclamaciones por tipo de vehiculo mediante pipes(TOP 5)
claims_tipo <- dataCar%>%
  group_by(veh_body)%>%
  summarise(totclaims=sum(numclaims))%>%
  arrange(desc(totclaims))
# Para mostrar el top 5 que tenemos es
head(claims_tipo,5)

# Vamos a utilizar la libreria ggplot2 para poder graficar los resultados obtenidos.
library(ggplot2)

ggplot(claims_tipo, aes(x = reorder(veh_body, -totclaims), y = totclaims)) +
  geom_bar(stat = "identity", fill = "#F0E68C", color = "#CD1076") +
  labs(title = "Numero de reclamaciones por tipo de vehiculo", x = "Tipo de vehículo", y = "Total de Reclamaciones") +  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Top tipo de vehiculos con el mayor monto de reclamaciones
montoclaims_tipo <- dataCar%>%
  group_by(veh_body)%>%
  summarise(totmontoclaims=sum(claimcst0))%>%
  arrange(desc(totmontoclaims))

head(montoclaims_tipo, 5)

#Graficamos los resultados mediante la libreria ggplot
ggplot(montoclaims_tipo, aes(x = reorder(veh_body, -totmontoclaims), y = totmontoclaims)) +
  geom_bar(stat = "identity", fill = "#F0E68C", color = "#CD1076") +
  labs(title = "Grafica por el monto total de reclamaciones", x = "Tipo de vehículo", y = "Monto Total de Reclamaciones") +  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

head(montoclaims_tipo, 10)

#Ahora vamos a obtener el numero de siniestros por genero
claims_genero <- dataCar%>%
  group_by(gender)%>%
  summarise(claimsgenero=sum(numclaims))%>%
  arrange(desc(claimsgenero))

ggplot(claims_genero, aes(x = gender, y = claimsgenero)) +
  geom_bar(stat = "identity", fill = "#7FFFD4", color = "#00FFFF") +
  labs(title = "Gráfica por el número de reclamaciones según género", 
       x = "Género", 
       y = "Total de Reclamaciones") +  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))

head(claims_genero)

#Mediante la siguiente funcion vamos a obtener el numero de siniestros por genero y tipo de vehiculo
claims_genero_tipo <- dataCar%>%
  group_by(gender, veh_body)%>%
  summarise(claimsgeneroportipo=sum(numclaims))%>%
  arrange(desc(claimsgeneroportipo))


#Obtener la grafica para el numero de reclamaciones por genero y tipo de vehiculo
ggplot(claims_genero_tipo, aes(x = reorder(veh_body, -claimsgeneroportipo), y = claimsgeneroportipo, fill= gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Grafica por el Total de Reclamaciones por Tipo de Vehiculo y Genero", x = "Tipo de vehículo", y = "Total de Reclamaciones") +  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



