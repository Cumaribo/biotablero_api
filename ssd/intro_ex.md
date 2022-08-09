
## Guía de uso

Bienvenido a *Chapman*, un sistema de recomendación para identificar los sitios que complementan en mayor medida inventarios de biodiversidad previos.

Esta versión de prueba de *Chapman*  está enfocada en identificar los sitios que tienen la mayor probabilidad de agregar especies al listado del Global Big Day realizado en el 2017. Consulte *Acerca de* para una explicación más completa de *Chapman*. 

#### Ver resultados

Para ver las sugerencias precalculadas, seleccione el área de su interés del menú desplegable y pulse el botón "Ver resultados". Podrá descargar los resultados del análisis de disimilitud en formato raster y las sugerencias de muestreo como shapefile o archivo separado por comas (.csv).

#### Personalizar análisis (beta)

Si usted esta interesado en evaluar iterativamente las sugerencias de *Chapman*, seleccione el área de su interés del menú desplegable y pulse el botón “Personalizar (beta)”.

Una vez sea desplegado el mapa de disimilitud en la pestaña  "Personalizar" podrá agregar sitios a muestrear iterativamente a través de los botones:

*  *Sugerir*: Identifica automáticamente el sitio más complementario de acuerdo al modelo de disimilitud. Esta acción desplegará una capa en el visor con la complementariedad ambiental en el área de interés seleccionada y un marcador del sitio sugerido. Podra desplegar la capa de complementarierdad seleccionando esta opción de visualización en la parte inferior izquierda del visor.

*  *Añadir*: Agrega manualmente un sitio sobre el mapa. Deberá ingresar la latitud y longitud del sitio que desea añadir y pulsar este botón para ingresar manualmente una localidad.

*  *Eliminar*: Descarta el punto añadido (automática o manualmente) más reciente.

En la medida que sean añadidas sugerencias de muestreo se desplegará una tabla en la parte inferior de la pestaña "Mapa" con información de contexto de las sugerencias, como las coordenadas, departamento, municipio y altura del sitio, asi como la presencia de áreas protegidas, distancia a carreteras (en km) y coberturas terrestres principales. La ultima columna (EDTotal) es una medida de la complementariedad del muestreo en el área de interés; si este valor se mantiene estable entre iteraciones es posible que se haya alcanzado un punto de saturación en el muestreo en el que sitios adicionales no incorporan nuevas especies a los listados. Utilice esta información como base para decidir la conveniencia de incluir un sitio en el muestreo o no. En caso que una sugerencia no se considere viable o pertinente, el mejor curso de acción es eliminarla para encontrar el siguiente sitio más complementario. **Note que si su proposito es identificar secuencialmente las sugerencias de *Chapman*, la opción "Ver resultados" es mucho más rápida**.


#### ADVERTENCIA DE DESEMPEÑO

La opción "Personalizar (beta)" de *Chapman* calcula las sugerencias de muestreo en tiempo real. Al ser una aplicación de ámbito nacional, la resolución es relativamente gruesa (20 km nacional, 5 km departamental), con el fin de obtener sugerencias en un periodo razonable de tiempo. Actualmente, una sugerencia de ámbito nacional o en departamentos grandes toma aproximadamente 1.5 minutos. En áreas más pequeñas (e.g. Atlántico), los tiempos de respuesta son considerablemente más rápidos. Por lo tanto, para evitar caidas en el servicio, es importante despues de pulsar el botón "sugerir"  esperar a que el proceso termine antes de interactuar nuevamente con el sistema. Igualmente, al ser una aplicación en desarrollo, *Chapman* es susceptible de sufrir caidas en el servicio, causadas por actualizaciones del sistema y especialmente el número de usuarios activos en la plataforma, por lo que si en algún momento no se puede conectar lo invitamos a contactarnos o a conectarse en otro momento. 


#### Contacto
Para cualquier inquietud comuníquese con Jorge Velásquez en jvelasquez@humboldt.org.co.


#### Cite esta aplicación como
Velásquez-Tibatá, J. I. & González, I. 2018. Chapman: sistema de recomendación de muestreo. Laboratorio de Biogeografía Aplicada, Instituto de Investigaciones Biológicas Alexander von Humboldt. http://indicadores.humboldt.org.co/muestreo/
