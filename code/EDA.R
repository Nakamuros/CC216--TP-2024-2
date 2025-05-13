df<- read.csv("data/hotel_bookings.csv")

install.packages("psych")
library("psych")
library("ggplot2")
#carga de datos volviendo los NULL en NA
data_hotel <- read.csv("data/hotel_bookings.csv", na.strings = "NULL", header = TRUE, stringsAsFactors = FALSE)

summary(data_hotel)

str(data_hotel)
#sondeo de valores faltantes dentro del dataset
valores_faltantes<- colSums(is.na(data_hotel))
valores_faltantes[valores_faltantes>0]

#tratamiento de datos faltantes

#imputacion para el dato country, puesto que no tiene muchos datos faltantes
#extraccion de moda
moda<-function(x){
  uv<-unique(na.omit(x))#uv = unique value
  return(uv[which.max(tabulate(match(x, uv)))])
}
data_hotel$country[is.na(data_hotel$country)]<-moda(data_hotel$country)

#eliminacion de columnas con muchos valores faltantes(agent y company)
data_hotel_clean<-data_hotel[,!(names(data_hotel)%in% c("agent", "company"))]
#validacion de datos post imputacion y eliminacion
summary(data_hotel_clean)

#identificacion de datos atipicos mediante el uso de boxplot
#lead time
ggplot(data_hotel, aes(y = lead_time))+ geom_boxplot(fill = "green")+labs(title = "boxplot de lead time", y = "lead time (dias)")+ theme_minimal()

#adr
ggplot(data_hotel, aes(y = adr))+ geom_boxplot(fill = "blue")+labs(title = "boxplot de ADR", y = "ADR(tarifa diaria promedio")+ theme_minimal()

#stays in week nights
ggplot(data_hotel, aes(y = stays_in_week_nights))+ geom_boxplot(fill = "red")+labs(title = "boxplot de stays in week night", 
                                                                                   y = "stays in week night")+ theme_minimal()
#tratamiento de datos atipicos

#winsorizacion con un rango 5% a 95%
#lead time
data_hotel_clean$lead_time<-data_hotel_clean$lead_time_winsorizado <- winsor(data_hotel_clean$lead_time, trim = 0.01)
#adr
data_hotel_clean$adr<-data_hotel_clean$adr_winsorizado <- winsor(data_hotel_clean$adr, trim = 0.01)
#stays in week nights
data_hotel_clean$stays_in_week_nights<-data_hotel_clean$stays_in_week_nights_winsorizado <- winsor(data_hotel_clean$stays_in_week_nights, trim = 0.01)
#verificamos los resultados
summary(data_hotel_clean$lead_time)
summary(data_hotel_clean$adr)
summary(data_hotel_clean$stays_in_week_nights)
data_hotel_clean <-data_hotel_clean[, !names(data_hotel_limpia) %in% c("lead_time_winsorizado","adr_winsorizado", "stays_in_week_nights_winsorizado" )]
#creamos un csv con data limpia
write.csv(data_hotel_clean, file = "hotel_bookings_clean.csv")

#cargamos el csv creado con la data limpia
data_hotel_limpia <- read.csv("data/hotel_bookings_clean.csv")


data_hotel_limpia <-data_hotel_limpia[, !names(data_hotel_limpia) %in% "X"]

#analissi en df limpio

#reservas por mes de cada hotel

ggplot(data_hotel_limpia, aes(x = hotel))+ geom_bar(fill = "blue")+ 
  labs(title = "Reservas por tipo de hotel", x = "hotel", y = "numero de reservas")+ theme_minimal()

#resumen de las reservas en forma de tabla
table(data_hotel_limpia$hotel)

#demanda a lo largo de los años
ggplot(data_hotel_limpia, aes(x = arrival_date_year)) + 
  geom_line(stat = "count", color = "orange", size = 1)+
  labs(title = "Evolucion de la demanda de los hoteles a lo largo de los años", 
       x = "Año", y = "cantidad de reservas")  + theme_minimal()

#resumen de la demanda a lo largo de los años
table(data_hotel_limpia$arrival_date_year)

#temporadas alta media y baja
ggplot(data_hotel_limpia, aes(x = arrival_date_month))+ 
  geom_bar(fill = "green")+ 
  labs(title = "Reservas por mes", x = "Mes", y = "Cantidad de reservas")+ 
  theme_minimal()
#resumen de las reservas por mes
table(data_hotel_limpia$arrival_date_month)

#niños y bebes en reserva
#creacion de una columna con la sumatoria de niños y bebes en cada reserva
data_hotel_limpia$children_babies <- 
  data_hotel_limpia$children + data_hotel_limpia$babies

ggplot(data_hotel_limpia, aes(x = children_babies))+
  geom_bar(fill = "red")+
  labs(title = "Reservas que incluyen niñoss y/o bebes", 
       x = "Cantidad de niños y bebes",
       y = "Cantidad de reservas")+ theme_minimal()

#resumen de niños y bebes
table(data_hotel_limpia$children_babies)

#decision de estacionamiento

ggplot(data_hotel_limpia, aes(x = factor(required_car_parking_spaces))) +
  geom_bar(fill = "purple")+
  labs(title = "Reservas que requieren de un estacionamiento",
       x = "Espacios de estacionamiento", y = "Cantidad de reservas") + theme_minimal()
#resumen numerico de espacios de estacionamiento
table(data_hotel_limpia$required_car_parking_spaces)

#cantidad de reservas canceladas
canceladas <- subset(data_hotel_limpia, is_canceled == 1)

ggplot(canceladas, aes(x = arrival_date_month)) +
  geom_bar(fill = "red") +
  labs(title = "Reservas canceladas por mes", x = "Mes", y = "Cantidad de cancelaciones") +
  theme_minimal()

table(canceladas$arrival_date_month)
