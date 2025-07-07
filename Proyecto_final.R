library(tidyverse)
library(modelr)
library(dplyr)


df_sueldos<-read.csv('sueldos_it_argentina_2025.csv')#df_original
glimpse(df_sueldos)#Variables originales

unique(df_nuevo$maximo_nivel_de_estudios)
df_nuevo%>%count(cantidad_de_personas_en_tu_organizacion)

#FILTRADO DEL DATASET:
#X_sal se dividio por mil asi en los graficos posteriores se entiende la cantidad de la que se habla, refiere a ultimo salario mensual bruto en pesos
#Se eliminaron los NA de nivel de estudios, reemplazo los Doctorado y Posdoctorado por NA por ser pocos
#Reemplazo de valores sin sentido de edad, antiguedad_en_la_empresa_actual, anos_en_el_puesto_actual por NA
#Reduccion de valores en personas de tu organizacion (de 11 valores a 3)

df_nuevo<- df_sueldos%>% mutate(X_sal=X_sal/1000, 
                                'contas_con_beneficios_adicionales'=replace(contas_con_beneficios_adicionales,contas_con_beneficios_adicionales!='Ninguna de las anteriores','Si'),
                                'contas_con_beneficios_adicionales'=replace(contas_con_beneficios_adicionales,contas_con_beneficios_adicionales=='Ninguna de las anteriores','No'),
                                'recibis_algun_tipo_de_bono'=replace(recibis_algun_tipo_de_bono,recibis_algun_tipo_de_bono=='No','Ninguno'),
                                'cantidad_de_personas_en_tu_organizacion'=replace(cantidad_de_personas_en_tu_organizacion,cantidad_de_personas_en_tu_organizacion=='1 (solamente yo)','De 1 a 50 personas'),
                                'cantidad_de_personas_en_tu_organizacion'=replace(cantidad_de_personas_en_tu_organizacion,cantidad_de_personas_en_tu_organizacion=='De 2 a 10 personas','De 1 a 50 personas'),
                                'cantidad_de_personas_en_tu_organizacion'=replace(cantidad_de_personas_en_tu_organizacion,cantidad_de_personas_en_tu_organizacion=='De 11  a 50  personas','De 1 a 50 personas'),
                                'cantidad_de_personas_en_tu_organizacion'=replace(cantidad_de_personas_en_tu_organizacion,cantidad_de_personas_en_tu_organizacion=='De 51 a 100 personas','De 51 a 1000 personas'),
                                'cantidad_de_personas_en_tu_organizacion'=replace(cantidad_de_personas_en_tu_organizacion,cantidad_de_personas_en_tu_organizacion=='De 101 a 200 personas','De 51 a 1000 personas'),
                                'cantidad_de_personas_en_tu_organizacion'=replace(cantidad_de_personas_en_tu_organizacion,cantidad_de_personas_en_tu_organizacion=='De 201 a 500 personas','De 51 a 1000 personas'),
                                'cantidad_de_personas_en_tu_organizacion'=replace(cantidad_de_personas_en_tu_organizacion,cantidad_de_personas_en_tu_organizacion=='De 501 a 1000 personas','De 51 a 1000 personas'),
                                'cantidad_de_personas_en_tu_organizacion'=replace(cantidad_de_personas_en_tu_organizacion,cantidad_de_personas_en_tu_organizacion=='De 1001 a 2000 personas','Más de 1000 personas'),
                                'cantidad_de_personas_en_tu_organizacion'=replace(cantidad_de_personas_en_tu_organizacion,cantidad_de_personas_en_tu_organizacion=='De 2001a 5000 personas','Más de 1000 personas'),
                                'cantidad_de_personas_en_tu_organizacion'=replace(cantidad_de_personas_en_tu_organizacion,cantidad_de_personas_en_tu_organizacion=='De 5001 a 10000 personas','Más de 1000 personas'),
                                'cantidad_de_personas_en_tu_organizacion'=replace(cantidad_de_personas_en_tu_organizacion,cantidad_de_personas_en_tu_organizacion=='Más de 10000 personas','Más de 1000 personas'),
                                'tengo_edad'=replace(tengo_edad,tengo_edad>85,NA),
                                'antiguedad_en_la_empresa_actual'=replace(antiguedad_en_la_empresa_actual,antiguedad_en_la_empresa_actual>60,NA),
                                'anos_en_el_puesto_actual'=replace(anos_en_el_puesto_actual,anos_en_el_puesto_actual>60,NA),
                                'maximo_nivel_de_estudios'=replace(maximo_nivel_de_estudios,maximo_nivel_de_estudios %in% c('Doctorado','Posdoctorado'),NA))%>% 
  select(c("donde_estas_trabajando",'dedicacion','recibis_algun_tipo_de_bono','contas_con_beneficios_adicionales',"carrera",
           trabajo_de:cuantas_personas_tenes_a_cargo,
           'cantidad_de_personas_en_tu_organizacion','modalidad_de_trabajo',maximo_nivel_de_estudios,
           'tengo_edad','genero','sueldo_dolarizado', 'seniority','X_sal'))%>%drop_na()

df_nuevo$maximo_nivel_de_estudios <- relevel(factor(df_nuevo$maximo_nivel_de_estudios), ref = "Secundario")
df_nuevo$recibis_algun_tipo_de_bono <- relevel(factor(df_nuevo$recibis_algun_tipo_de_bono), ref = "Ninguno")

#GRAFICOS PARA PRESENTACION 
#Usar como ejemplo el pdf Diapositivas Presentacion TP Final y que se cumplan los puntos del pdf Rubrica TP FINAL
#En el pdf Diapositivas Presentacion TP Final a partir de la diapo 21, se muestran como ejemplo una que esta mal hecha y la siguiente bien y asi sucesivamente
#Ver ultima clase virtual

#2 diapositivas introduccion (caratula e introduccion de pregunta a responder)

#1 diapositiva o 2 diapositivas(limpieza de datasets, presentacion de variables)
#Variables definitivas (no pegar screenshot)
glimpse(df_nuevo)

#Rangos de variables numericas (no pegar screenshot)
range(df_nuevo$X_sal) #[150.000, 12.960.000]
range(df_nuevo$antiguedad_en_la_empresa_actual) #[0,40]
range(df_nuevo$anos_en_el_puesto_actual) #[0,40]
range(df_nuevo$tengo_edad) #[20,74]
range(df_nuevo$anos_de_experiencia) #[0,45]
range(df_nuevo$cuantas_personas_tenes_a_cargo) #[0,130]

#Distribucion variable target
df_nuevo%>%ggplot(aes(y=X_sal))+
  geom_boxplot()+
  labs(title = 'Distribución de salario bruto', y='Salario (miles de pesos)')



#2 diapositivas (graficos sobre el dataset)
#Se podria agregar el grafico de la pagina del informe para mostrar de donde votaron la mayoria

#Cantidad de empleados segun genero (puede estar y mencionarse rapido)
df_nuevo%>%filter(genero  %in% c('Hombre Cis', 'Mujer Cis'))%>%
  ggplot(aes(x=genero, fill=genero))+
  geom_bar()+
  labs(title = 'Cantidad de empleados según género',x='Géneros',y='Cantidad de empleados')+
  theme(legend.position = "none",
        axis.text.x = element_text(size = 10.5),
        axis.text.y = element_text(size = 10.5),
        axis.title.y = element_text(size = 12),
        axis.title.x = element_text(size = 13),
        plot.title = element_text(size = 16))

#Proporcion de part time/full time (puede estar y mencionarse rapido)
df_nuevo%>%ggplot(aes(x='',fill = dedicacion))+
  geom_bar(position = 'fill')+
  labs(title = 'Proporción de empleados según dedicación',x='',y='Proporción de empleados', fill='Dedicación')+
  theme(axis.text.x = element_text(size = 10.5),
        axis.text.y = element_text(size = 10.5),
        axis.title.y = element_text(size = 12),
        axis.title.x = element_text(size = 13),
        plot.title = element_text(size = 16),
        legend.title = element_text(size = 13.5),     # Tamaño del título de la leyenda
        legend.text = element_text(size = 12),      # Tamaño del texto de los items de la leyenda
        legend.key.size = unit(0.9, "cm")           # Tamaño de los símbolos (cajas, puntos) en la leyenda)
        )
 
#Cantidad de empleados segun nivel de estudios
df_nuevo%>%
  ggplot(aes(x=maximo_nivel_de_estudios, fill = maximo_nivel_de_estudios))+
  geom_bar()+
  theme(legend.position = "none",
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10.5),
        axis.text.y = element_text(size = 10.5))+
  labs(title = 'Cantidad de empleados según máximo nivel de estudios',x='Niveles de estudio',y='Cantidad de empleados')

#Distribucion edades 
df_nuevo%>%ggplot(aes(x=tengo_edad))+
  geom_histogram(bins = 30, fill='red',color='black')+
  labs(title = "Distribución de edades de los empleados", x='Años',y='Cantidad de empleados')+
  theme(
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        axis.title.y = element_text(size = 13),
        axis.title.x = element_text(size = 14),
        plot.title = element_text(size = 15))


#1 diapositiva o 2 diapositivas (relaciones sueldo con variables numericas)
#RELACIONES NUMERICAS DEBILES:
#Sueldo segun antiguedad en la empresa
df_nuevo%>%
  ggplot(aes(x=antiguedad_en_la_empresa_actual, y=X_sal))+
  geom_point()+geom_smooth(method = "lm", color="red")+
  labs(title = 'Salario bruto según años en la empresa actual',x='Años en actual empresa', y='Salario (miles de pesos)')+
  theme(
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size = 11),
    axis.title.y = element_text(size = 14),
    axis.title.x = element_text(size = 14),
    plot.title = element_text(size = 15))

#Sueldo segun años en el puesto actual
df_nuevo%>%
  ggplot(aes(x=anos_en_el_puesto_actual, y=X_sal))+
  geom_point()+geom_smooth(method = "lm", color="red")+
  labs(title = 'Salario bruto según años en el puesto actual',x='Años en el puesto actual', y='Salario (miles de pesos)')+
  theme(
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size = 11),
    axis.title.y = element_text(size = 14),
    axis.title.x = element_text(size = 14),
    plot.title = element_text(size = 15))


#RELACIONES NUMERICAS FUERTES (he):
#Sueldo segun edad
df_nuevo%>%
  ggplot(aes(x=tengo_edad, y=X_sal))+
  geom_point()+geom_smooth(method = "lm", color="red")+
  labs(title = 'Salario bruto según edad',x='Edad', y='Salario (miles de pesos)')+
  theme(
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size = 11),
    axis.title.y = element_text(size = 14),
    axis.title.x = element_text(size = 14),
    plot.title = element_text(size = 15))

#Sueldo segun años de experiencia
df_nuevo%>%
  ggplot(aes(x=anos_de_experiencia, y=X_sal))+
  geom_point()+geom_smooth(method = "lm", color="red")+
  labs(title = 'Salario bruto según años de experiencia',x='Años de experiencia', y='Salario (miles de pesos)')+
  theme(
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size = 11),
    axis.title.y = element_text(size = 14),
    axis.title.x = element_text(size = 14),
    plot.title = element_text(size = 15))

#Sueldo segun personas a cargo
df_nuevo%>%filter(cuantas_personas_tenes_a_cargo<=100)%>%
  ggplot(aes(x=cuantas_personas_tenes_a_cargo, y=X_sal))+
  geom_point()+geom_smooth(method = "lm", color="red")+
  labs(title = 'Salario bruto según personas a cargo',x='Personas a cargo', y='Salario (miles de pesos)')+
  theme(
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size = 11),
    axis.title.y = element_text(size = 14),
    axis.title.x = element_text(size = 14),
    plot.title = element_text(size = 15))

#2 diapositivas (relaciones sueldo con variables categoricas)
#BOXPLOTS 
#Seniority
df_nuevo%>%ggplot(aes(x=seniority,y=X_sal, fill = seniority))+
  geom_boxplot()+
  labs(title = 'Distribución de salario bruto según seniority',x='Seniority', y='Salario (miles de pesos)', fill='Seniority')+
  theme(
    axis.title.x = element_blank(),    # Quitar título eje X
    axis.text.x = element_blank(),     # Quitar etiquetas eje X (números o texto)
    axis.ticks.x = element_blank(),     # Quitar las marcas (ticks) del eje X
    axis.title.y = element_text(size = 12),   # tamaño etiquetas ejes Y   
    axis.text.y = element_text(size = 11),    # tamaño números de eje Y
    plot.title = element_text(size = 16),      # tamaño título
    legend.title = element_text(size = 12),     # Tamaño del título de la leyenda
    legend.text = element_text(size = 12),      # Tamaño del texto de los items de la leyenda
    legend.key.size = unit(1, "cm")           # Tamaño de los símbolos (cajas, puntos) en la leyenda
  )
  

#Bono adicional 
df_nuevo%>%ggplot(aes(x = fct_reorder(recibis_algun_tipo_de_bono, X_sal),y=X_sal, fill = recibis_algun_tipo_de_bono))+
  geom_boxplot()+
  labs(title = 'Distribución de salario bruto según bono adicional',x='Tipo de bono', y='Salario (miles de pesos)',fill='Tipos de bono')+
  theme(
    axis.title.x = element_blank(),    # Quitar título eje X
    axis.text.x = element_blank(),     # Quitar etiquetas eje X (números o texto)
    axis.ticks.x = element_blank(),     # Quitar las marcas (ticks) del eje X
    axis.title.y = element_text(size = 12),   # tamaño etiquetas ejes Y   
    axis.text.y = element_text(size = 11),    # tamaño números de eje Y
    plot.title = element_text(size = 16),      # tamaño título
    legend.title = element_text(size = 13),     # Tamaño del título de la leyenda
    legend.text = element_text(size = 11),      # Tamaño del texto de los items de la leyenda
    legend.key.size = unit(0.8, "cm")           # Tamaño de los símbolos (cajas, puntos) en la leyenda
  )

#Nivel de estudios (agregar directamente por encima la cantidad de gente que tiene cada nivel, ver diapo 29 del ejemplo)
df_nuevo%>%count(maximo_nivel_de_estudios)#n por nivel
df_nuevo%>%
  ggplot(aes(x=fct_reorder(maximo_nivel_de_estudios, X_sal),y=X_sal, fill = maximo_nivel_de_estudios))+
  geom_boxplot()+
  labs(title = 'Distribución de salario bruto según máximo nivel de estudios',x='Niveles de estudio', y='Salario (miles de pesos)', fill='Niveles de estudio')+
  theme(
    axis.title.x = element_blank(),    # Quitar título eje X
    axis.text.x = element_blank(),     # Quitar etiquetas eje X (números o texto)
    axis.ticks.x = element_blank(),     # Quitar las marcas (ticks) del eje X
    axis.title.y = element_text(size = 12),   # tamaño etiquetas ejes Y   
    axis.text.y = element_text(size = 11),    # tamaño números de eje Y
    plot.title = element_text(size = 16),      # tamaño título
    legend.title = element_text(size = 11),     # Tamaño del título de la leyenda
    legend.text = element_text(size = 11),      # Tamaño del texto de los items de la leyenda
    legend.key.size = unit(0.8, "cm")           # Tamaño de los símbolos (cajas, puntos) en la leyenda
  )

#Sueldo dolarizado (puede estar)
df_nuevo%>%ggplot(aes(x=sueldo_dolarizado,y=X_sal, fill = sueldo_dolarizado))+
  geom_boxplot()+
  labs(title = 'Distribución de salario bruto según sueldo dolarizado',x='Sueldo dolarizado', y='Salario (miles de pesos)', fill= 'Sueldo dolarizado')+
  theme(
    axis.title.x = element_blank(),    # Quitar título eje X
    axis.text.x = element_blank(),     # Quitar etiquetas eje X (números o texto)
    axis.ticks.x = element_blank(),     # Quitar las marcas (ticks) del eje X
    axis.title.y = element_text(size = 12),   # tamaño etiquetas ejes Y   
    axis.text.y = element_text(size = 11),    # tamaño números de eje Y
    plot.title = element_text(size = 16),      # tamaño título
    legend.title = element_text(size = 12),     # Tamaño del título de la leyenda
    legend.text = element_text(size = 12),      # Tamaño del texto de los items de la leyenda
    legend.key.size = unit(1, "cm")           # Tamaño de los símbolos (cajas, puntos) en la leyenda
  )


#1 diapositiva o 2 diapositivas (modelos)
#Mostrar en una tabla que variables se agregan, cuantas hay por modelo, no mostrar modelos enteros. Completo solo mostrar el 5
mod1<-lm(data=df_nuevo, X_sal ~ (poly(anos_de_experiencia,2,raw = TRUE)+cuantas_personas_tenes_a_cargo+tengo_edad)*sueldo_dolarizado)
mod2<-lm(data=df_nuevo, X_sal ~ (poly(anos_de_experiencia,2,raw = TRUE)+cuantas_personas_tenes_a_cargo+tengo_edad)*sueldo_dolarizado+maximo_nivel_de_estudios)
mod3<-lm(data=df_nuevo, X_sal ~ (poly(anos_de_experiencia,2,raw = TRUE)+cuantas_personas_tenes_a_cargo+tengo_edad)*sueldo_dolarizado+factor(maximo_nivel_de_estudios)+factor(cantidad_de_personas_en_tu_organizacion))
mod4<-lm(data=df_nuevo, X_sal ~ (poly(anos_de_experiencia,2,raw = TRUE)+cuantas_personas_tenes_a_cargo+tengo_edad)*sueldo_dolarizado+factor(maximo_nivel_de_estudios)+factor(cantidad_de_personas_en_tu_organizacion)+factor(seniority))
mod5<-lm(data=df_nuevo, X_sal ~ (poly(anos_de_experiencia,2,raw = TRUE)+cuantas_personas_tenes_a_cargo+tengo_edad)*sueldo_dolarizado+factor(maximo_nivel_de_estudios)+factor(cantidad_de_personas_en_tu_organizacion)+factor(seniority)+factor(recibis_algun_tipo_de_bono))

anova(mod1,mod2) #Quedarse con p-valores

summary(mod5) #Quedarse solo con adjusted R2, RMSE y p-valor 
rmse <- sqrt(mean(resid(mod1)^2))


#1 diapositiva o 2 diapositivas (graficos sobre el modelo final)
#Prediccion vs valor real
df_nuevo %>% add_predictions(mod5) %>% 
  ggplot(aes(x = pred, y = X_sal)) +
  geom_point(alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, color = "red" )+
  labs(title = "Predicción vs Valor real", x = "Predicción del modelo", y = "Salario bruto real")+
  theme(
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size = 11),
    axis.title.y = element_text(size = 13.5),
    axis.title.x = element_text(size = 13.5),
    plot.title = element_text(size = 15))

#Residuos vs predicciones
df_nuevo %>% add_predictions(mod5) %>% add_residuals(mod5) %>% 
  ggplot(aes(x=pred, y=resid)) + geom_point()+geom_hline(aes(yintercept=0), color='red')+
  labs(title = "Residuos vs predicción en funcion del salario", x = "Predicción", y = "Residuos")+
  theme(
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size = 11),
    axis.title.y = element_text(size = 13),
    axis.title.x = element_text(size = 13),
    plot.title = element_text(size = 15))


#Grafico de salario vs experiencia con linea de prediccion basado en modelo 1
media_personas <- mean(df_nuevo$cuantas_personas_tenes_a_cargo, na.rm = TRUE)
media_edad <- mean(df_nuevo$tengo_edad, na.rm = TRUE)

grid <- df_nuevo %>%
  distinct(anos_de_experiencia, sueldo_dolarizado) %>%
  mutate(cuantas_personas_tenes_a_cargo = mean(df_nuevo$cuantas_personas_tenes_a_cargo, na.rm = TRUE),
    tengo_edad = mean(df_nuevo$tengo_edad, na.rm = TRUE))%>%
  add_predictions(mod1, var = "pred_cuad")

ggplot(df_nuevo, aes(x = anos_de_experiencia, y = X_sal, color = factor(sueldo_dolarizado))) +
  geom_point(alpha = 0.4) +
  geom_line(data = grid, aes(y = pred_cuad), color = "black") +
  facet_wrap(~sueldo_dolarizado) +
  labs(title = "Relación entre experiencia y salario bruto",
       x='Años de experiencia', y='Salario (miles de pesos)',
       color='Sueldo dolarizado')+
  theme(
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size = 11),
    axis.title.y = element_text(size = 12),
    axis.title.x = element_text(size = 13),
    plot.title = element_text(size = 15),
    legend.title = element_text(size = 13),     # Tamaño del título de la leyenda
    legend.text = element_text(size = 11),      # Tamaño del texto de los items de la leyenda
    legend.key.size = unit(0.8, "cm")           # Tamaño de los símbolos (cajas, puntos) en la leyenda
  )

#2 diapositivas finales (conclusiones y agradecimientos)





df_nuevo %>% add_predictions(mod1) %>% 
  ggplot(aes(x = pred, y = X_sal)) +
  geom_point(alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, color = "red" )+
  labs(title = "Predicción vs Valor real", x = "Predicción del modelo", y = "Salario bruto real")+
  theme(
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size = 11),
    axis.title.y = element_text(size = 13.5),
    axis.title.x = element_text(size = 13.5),
    plot.title = element_text(size = 15))







#Residuos vs predicciones
df_nuevo %>% add_predictions(mod1) %>% add_residuals(mod1) %>% 
  ggplot(aes(x=pred, y=resid)) + geom_point()+geom_hline(aes(yintercept=0), color='red')+
  labs(title = "Residuos vs predicción en funcion del salario", x = "Predicción", y = "Residuos")+
  theme(
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size = 11),
    axis.title.y = element_text(size = 13),
    axis.title.x = element_text(size = 13),
    plot.title = element_text(size = 15))







anova(mod3,mod4) #Quedarse con p-valores

summary(mod5) #Quedarse solo con adjusted R2, RMSE y p-valor 
rmse <- sqrt(mean(resid(mod4)^2))
















