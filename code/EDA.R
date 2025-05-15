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
data_hotel_limpia$arrival_date_month <- factor(data_hotel_limpia$arrival_date_month,
                                               levels = month.name)

# Gráfico
ggplot(data_hotel_limpia, aes(x = arrival_date_month)) + 
  geom_bar(fill = "green") + 
  labs(title = "Reservas por mes", 
       x = "Mes", 
       y = "Cantidad de reservas") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#resumen de las reservas por mes
table(data_hotel_limpia$arrival_date_month)
reservas<- data_hotel_limpia$arrival_date_month

table(data_hotel_limpia$market_segment, data_hotel_limpia$arrival_date_month)


#niños y bebes en reserva
#creacion de una columna con la sumatoria de niños y bebes en cada reserva
data_hotel_limpia$children_babies <- 
  data_hotel_limpia$children + data_hotel_limpia$babies

ggplot(data_hotel_limpia, aes(x = children_babies))+
  geom_bar(fill = "blue")+
  labs(title = "Reservas que incluyen niñoss y/o bebes", 
       x = "Cantidad de niños y bebes",
       y = "Cantidad de reservas")+ theme_minimal()

#resumen de niños y bebes
table(data_hotel_limpia$children_babies)

#decision de estacionamiento
data_parqueo <- data_hotel_limpia %>%
  filter( required_car_parking_spaces & data_hotel_limpia$arrival_date_year == 2016)
ggplot(data_parqueo, aes(x = factor(required_car_parking_spaces))) +
  geom_bar(fill = "purple")+
  labs(title = "Reservas que requieren de un estacionamiento en el año 2016",
       x = "Espacios de estacionamiento", y = "Cantidad de reservas") + theme_minimal()
#resumen numerico de espacios de estacionamiento
table(data_parqueo$required_car_parking_spaces)

#parqueo<-data_hotel_limpia$required_car_parking_spaces


parqueo_menores <- data_parqueo %>%
  filter(children_babies > 0)
tapply(data_parqueo$required_car_parking_spaces,
       data_parqueo$market_segment,
       sum,
       na.rm = TRUE)
library(dplyr)

# Resumen por hotel
resumen_hotel <- parqueo_menores %>%
  group_by(hotel) %>%
  summarise(cantidad_reservas = n())

# Gráfico
#data reserva con estacionamiento por hotel de menores
ggplot(resumen_hotel, aes(x = hotel, y = cantidad_reservas, fill = hotel)) +
  geom_bar(stat = "identity") +
  labs(title = "Reservas con hijos que requieren estacionamiento por tipo de hotel",
       x = "Tipo de hotel", y = "Cantidad de reservas") +
  theme_minimal()

data_hotel_limpia %>%
  group_by(market_segment, hotel) %>%
  summarise(total_parqueo = sum(required_car_parking_spaces, na.rm = TRUE))
table(parqueo_menores$required_car_parking_spaces)



#hijos parqueo segmento
resumen_segmento_hijos_parqueo <- parqueo_menores %>%
  group_by(market_segment) %>%
  summarise(cantidad_reservas = n())

ggplot(resumen_segmento_hijos_parqueo, aes(x = market_segment, y = cantidad_reservas, fill = market_segment)) +
  geom_bar(stat = "identity") +
  labs(title = "Reservas con hijos y estacionamiento por segmento de mercado",
       x = "Segmento de mercado", y = "Cantidad de reservas") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
table(resumen_segmento_hijos_parqueo)

#por segmento en general

# Agrupar por hotel y segmento
resumen_hotel_segmento <- data_parqueo %>%
  group_by(hotel, market_segment) %>%
  summarise(cantidad_reservas = n()) %>%
  ungroup()

ggplot(resumen_hotel_segmento, aes(x = market_segment, y = cantidad_reservas, fill = hotel)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Reservas con parqueo por segmento de mercado y tipo de hotel",
       x = "Segmento de mercado",
       y = "Cantidad de reservas",
       fill = "Tipo de hotel") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

resumen_segmento_resort <- data_hotel_limpia %>%
  filter(required_car_parking_spaces > 0, hotel == "Resort Hotel") %>%
  group_by(market_segment) %>%
  summarise(cantidad_reservas = n()) %>%
  arrange(desc(cantidad_reservas))
resumen_segmento_resort
#cantidad de reservas canceladas
# Convertir a factor con orden cronológico
canceladas$arrival_date_month <- factor(canceladas$arrival_date_month,
                                        levels = month.name)

#reservas canceladas con parqueo

# Filtrar reservas canceladas con estacionamiento
canceladas_parqueo <- data_hotel_limpia %>%
  filter(is_canceled == 1, required_car_parking_spaces > 0)

# Agrupar por hotel
resumen_canceladas_hotel <- canceladas_parqueo %>%
  group_by(hotel) %>%
  summarise(cantidad_canceladas = n())
table(resumen_canceladas_hotel)
# Gráfico
ggplot(resumen_canceladas_hotel, aes(x = hotel, y = cantidad_canceladas, fill = hotel)) +
  geom_bar(stat = "identity") +
  labs(title = "Reservas canceladas con estacionamiento por tipo de hotel",
       x = "Tipo de hotel",
       y = "Cantidad de reservas canceladas") +
  theme_minimal()


# Filtrar reservas que requieren estacionamiento
reservas_parqueo <- data_hotel_limpia %>%
  filter(required_car_parking_spaces > 0)

# Agrupar por hotel y estado de cancelación
resumen_cancelacion <- reservas_parqueo %>%
  group_by(hotel, is_canceled) %>%
  summarise(cantidad_reservas = n()) %>%
  ungroup()

# Convertir is_canceled a factor con etiquetas descriptivas
resumen_cancelacion$is_canceled <- factor(resumen_cancelacion$is_canceled,
                                          levels = c(0, 1),
                                          labels = c("No Cancelada", "Cancelada"))

# Crear el gráfico de barras agrupadas
ggplot(resumen_cancelacion, aes(x = hotel, y = cantidad_reservas, fill = is_canceled)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Reservas con estacionamiento por tipo de hotel y estado de cancelación",
       x = "Tipo de hotel",
       y = "Cantidad de reservas",
       fill = "Estado de cancelación") +
  theme_minimal()

# Gráfico
ggplot(canceladas, aes(x = arrival_date_month)) +
  geom_bar(fill = "yellow") +
  labs(title = "Reservas canceladas por mes", 
       x = "Mes", 
       y = "Cantidad de cancelaciones") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


table(canceladas$arrival_date_month)


#parqueo data estacionamiento por segmento

resumen_parqueo <- data_hotel_limpia %>%
  filter(!is.na(required_car_parking_spaces) & required_car_parking_spaces > 0) %>%
  group_by(market_segment, hotel) %>%
  summarise(total_parqueo = sum(required_car_parking_spaces)) %>%
  ungroup()


















