# CC216--TP-2025-1
Caso de Análisis: Hotel Booking Demand Datasets

# 📊 Análisis Exploratorio de Datos – Demanda Hotelera

## 🎯 Objetivo del trabajo

Realizar un análisis exploratorio de un conjunto de datos (*Exploratory Data Analysis, EDA*), generando visualizaciones, preparando los datos y extrayendo conclusiones iniciales utilizando **R / RStudio** como herramienta principal.

---

## 👥 Alumnos participantes

- Rodrigo Alonso Gamero Vera  
- Jairo Luis Orihuela Paredes  
- Marcelo Matías Hernández Rengifo  
- Ángel Gabriel Díaz Chávez

---

## 📁 Descripción del dataset

Este artículo presenta **dos conjuntos de datos reales** sobre reservas hoteleras:

- **Hotel H1**: Resort hotel, ubicado en la región del Algarve.  
- **Hotel H2**: Hotel urbano, ubicado en Lisboa.

Ambos conjuntos comparten la misma estructura, compuesta por **31 variables**. Las observaciones incluyen un total de:

- **40,060 reservas** para el hotel H1  
- **79,330 reservas** para el hotel H2

Cada fila del dataset representa una **reserva individual** e incluye tanto reservas efectivas como canceladas. Los datos abarcan el período entre el **1 de julio de 2015 y el 31 de agosto de 2017**. Para preservar la privacidad, se eliminaron los elementos relacionados con la identificación de los clientes y de los hoteles.

🔍 Este conjunto de datos es útil para tareas de:
- Predicción de cancelaciones de reservas
- Análisis de comportamiento de clientes
- Gestión de ingresos
- Minería de datos
- Enseñanza de aprendizaje automático

> Fuente del dataset: [Hotel booking demand datasets (ScienceDirect)](https://doi.org/10.1016/j.dib.2018.11.126)

---

## 📌 Conclusiones

Posterior a un analisis exhaustivo se puede deducir que:

- El City Hotel presenta una mayor preferencia por parte de los clientes en comparación con el Resort Hotel.

- Se observa una disminución progresiva en la demanda de reservas a partir del año 2016.

- Existen claras temporadas de alta, media y baja demanda:

     - **Alta demanda:** mayo, julio y agosto.

     - **Demanda media:** marzo, abril, junio, septiembre y octubre.

     - **Baja demanda:** enero, febrero, noviembre y diciembre.

- Se registraron 9,232 reservas que incluyen niños y/o bebés, lo cual evidencia la importancia de considerar a las familias como un segmento relevante.

- Solo 7,416 reservas requerían espacios de estacionamiento, lo que indica una baja necesidad general de este servicio.

- Los meses con mayor cantidad de cancelaciones de reservas fueron abril, mayo, junio, julio, septiembre y octubre, cada uno con más de 4,000 cancelaciones.

**Recomendaciones**

- **Estrategia de precios y campañas:**

Aplicar tarifas dinámicas que incrementen en temporada alta para maximizar ingresos.

Implementar promociones específicas durante las temporadas bajas para fomentar las reservas.

- **Gestión de cancelaciones:**

Fortalecer las políticas de cancelación, especialmente en los meses de alta cancelación.

Utilizar recordatorios automáticos y ofrecer beneficios por cumplimiento de reserva para reducir las anulaciones.

- **Segmentación y marketing:**

Desarrollar ofertas familiares y paquetes especiales para reservas con niños y/o bebés.

Enfocar campañas de marketing digital principalmente en el City Hotel, dada su popularidad.

- **Infraestructura:**

No es necesario ampliar los espacios de estacionamiento en este momento, dado su bajo nivel de requerimiento.

- **Seguimiento de tendencias:**

Continuar con el monitoreo y análisis de datos anualmente para adaptar las decisiones comerciales y operativas a los cambios de comportamiento del cliente.

---

## 📄 Licencia

Este trabajo esta licenciado bajo el estandar CC BY 4.0

> https://creativecommons.org/licenses/by/4.0/

