setwd("~/Documents/DatosEPF/FicherosCSV")
library(dplyr)
library(tidyr)

##### Variable declarations 
lista <- list.files(pattern = "Fichero *")
lista.hogar <- lista[grep("Hogar", lista)]
lista.gasto <- lista[grep("Gastos", lista)]

año <- 2006:2015
columnas <- c("Media.pob", "Total.pob", "Media.mue", "Total.mue", "SD.mue")
columnas_porcentaje <- c("Hogares.pob", "Hogares.mue", "Porcentaje.pob", "Porcentaje.mue")

nombres_df <- paste("ConsumoIngreso", 2006:2015, sep = "")
nombres_porcentajes <- paste("Porcentaje", 2006:2015, sep = "")

for(i in 1:length(lista.hogar)) {
  hogar <- read.csv(lista.hogar[i], header = TRUE, stringsAsFactors = FALSE) ## Cambiar por [i].
  hogar_subset <- select(hogar, INTERIN, NUMERO, FACTOR) 
    
    ## Group income categories.
  hogar_subset$INTERIN[hogar_subset$INTERIN == 2] <- 1
  hogar_subset$INTERIN[hogar_subset$INTERIN == 3 | hogar_subset$INTERIN == 4] <- 2
  hogar_subset$INTERIN[hogar_subset$INTERIN == 5 | hogar_subset$INTERIN == 6] <- 3
  hogar_subset$INTERIN[hogar_subset$INTERIN == 7] <- 4
  hogar_subset$INTERIN[hogar_subset$INTERIN == 8 | hogar_subset$INTERIN == 9 | hogar_subset$INTERIN == 10] <- 5
    
    
  gastos <- read.csv(lista.gasto[i], header = TRUE, stringsAsFactors = FALSE)
  gastos_subset <- gastos %>% select(NUMERO, CODIGO, CANTIDAD, FACTOR) %>% mutate(CANT_MUESTRA = CANTIDAD/FACTOR)
    
  
  ################# Get total population and sample sizes, and home percentages ##################
  poblacion_total <- sum(hogar_subset$FACTOR)
  muestra_total <- length(hogar_subset$NUMERO)
  pob_per_income <- hogar_subset %>% group_by(INTERIN) %>% summarize(Hogares_pob = sum(FACTOR), Hogares_muestra = length(unique(NUMERO)))
  porcentaje_hogares <- pob_per_income %>% mutate(pob_porcentaje = (Hogares_pob/sum(Hogares_pob))*100, mue_porcentaje = (Hogares_muestra/sum(Hogares_muestra))*100)
  
  colnames(porcentaje_hogares)[2:5] <- paste(columnas_porcentaje, año[i], sep = "")
  assign(nombres_porcentajes[i], porcentaje_hogares)
  ################################################################################################

  
  # Merge both dataframes by the Identification Number. 
  
  hogar_gastos <- merge(hogar_subset, gastos_subset, by = "NUMERO", all.y = T) 
  
  # Create a variable (SUBGRUPO) with only the first three numbers of every product CODE.
  # Prepend a "0" to the code to make them all of 5 digits. 
  hogar_gastos$CODIGO <- formatC(hogar_gastos$CODIGO, width = 5, format = "d", flag = "0") 
  hogar_gastos$SUBGRUPO <- substr(hogar_gastos$CODIGO, 1, 3) 
  
  # Filter products by subgroup (only the ones that have physical quantity)
  hogar_gastos <- hogar_gastos %>% filter(
    SUBGRUPO == "011" | SUBGRUPO == "012" | SUBGRUPO == "021" | SUBGRUPO == "022" | SUBGRUPO == "044" | SUBGRUPO == "045" | SUBGRUPO == "072"
  ) 
  hogar_gastos <- filter(hogar_gastos, CODIGO != "01116", CODIGO != "01115", CODIGO != "01117")
  
  hogar_gastos <- merge(hogar_gastos, pob_per_income, by = "INTERIN", all.y = T)
  

  cantidad_ingreso <- hogar_gastos %>% na.omit(hogar_gastos) %>% group_by(INTERIN, CODIGO) %>% summarize(
    pob_media = sum(CANTIDAD)/unique(Hogares_pob), 
    pob_total = sum(CANTIDAD),
    mue_media = sum(CANT_MUESTRA)/unique(Hogares_muestra),
    mue_total = sum(CANT_MUESTRA),
    HOGARES_CERO = unique(Hogares_muestra) - n(),
    suma_cant = sum((CANT_MUESTRA - mue_media)^2),
    suma_ceros = HOGARES_CERO * (mue_media^2), 
    suma_total = suma_cant + suma_ceros,
    sd = sqrt(suma_total/unique(Hogares_muestra))
    
  ) %>% select(-(HOGARES_CERO:suma_total))

  ####### Assign column and dataframe names to obtain one dataframe per year ########
  colnames(cantidad_ingreso)[3:7] <- paste(columnas, año[i], sep = "")
  assign(nombres_df[i], cantidad_ingreso)
  
}



lista_por_año_ingreso <- list(ConsumoIngreso2006, ConsumoIngreso2007, ConsumoIngreso2008, ConsumoIngreso2009, ConsumoIngreso2010, ConsumoIngreso2011, ConsumoIngreso2012, ConsumoIngreso2013, ConsumoIngreso2014, ConsumoIngreso2015)

merge.all <- function(x, y) {
  merge(x, y, by=c("INTERIN", "CODIGO"), all = TRUE)
}
consumo_ingreso_final <- Reduce(merge.all, lista_por_año_ingreso)


##Create two variables (INGRESO and TIPO_PRODUCTO) with the name of every code value for home type and product.
numeros_ingresos <- c(1,2,3,4,5)


nombres_ingresos <- c("Menos de 1000","De 1000 a menos de 2000", "De 2000 a menos de 3000", 
                      "De 3000 a menos de 5000", "5000 euros o más")
consumo_ingreso_final$INGRESO <- nombres_ingresos[match(consumo_ingreso_final$INTERIN, numeros_ingresos)]

consumo_ingreso_final$INGRESO <- paste(consumo_ingreso_final$INTERIN, consumo_ingreso_final$INGRESO)


numeros_codigos <- unique(consumo_ingreso_final$CODIGO)
nombres_codigos <- c("Arroz (kilo)", "Pan (kilo)", "Otros productos de panadería (kilo)", "Pastas alimenticias (kilo)", 
                     "Carne de bovino fresca, refrigerada o congelada (kilo)", "Carne de porcino fresca, refrigerada o congelada (kilo)", 
                     "Carne de ovino y caprino fresca, refrigerada o congelada (kilo)", "Carne de ave fresca, refrigerada o congelada (kilo)", 
                     "Charcutería y carne seca, salada o ahumada (kilo)", "Carnes preparadas y otros productos conteniendo carne (kilo)", 
                     "Otras carnes comestibles (frescas y congeladas) (kilo)", "Despojos, menudillos y casquería (kilo)", "Pescados frescos o refrigerados (kilo)", 
                     "Pescados congelados (kilo)", "Crustáceos y moluscos frescos, refrigerados o congelados (kilo)", "Pescados y mariscos secos, ahumados o salados (kilo)", 
                     "Otros pescados y mariscos procesados o conservados y preparados de pescados y mariscos (kilo)", "Leche entera (litro)", "Leche semidescremada y descremada (litro)", 
                     "Leche conservada (kilo)", "Yogures y leches fermentadas (kilo)", "Queso y requesón (kilo)", "Otros productos a base de leche (kilo)", "Huevos (unidad)", 
                     "Mantequilla (kilo)", "Margarina y otras grasas vegetales (kilo)", "Aceite de oliva (litro)", "Otros aceites comestibles (litro)", "Otras grasas animales (kilo)", 
                     "Cítricos (frescos, refrigerados o congelados) (kilo)", "Plátanos (frescos, refrigerados o congelados) (kilo)", "Manzanas (frescas, refrigeradas o congeladas) (kilo)", 
                     "Peras (frescas, refrigeradas o congeladas) (kilo)", "Frutas con hueso (frescas, refrigeradas o congeladas) (kilo)", "Aceitunas (kilo)", 
                     "Bayas (frescas, refrigeradas o congeladas) (kilo)", "Otras frutas (frescas, refrigeradas o congeladas) (kilo)", "Frutos secos (kilo)", 
                     "Frutas preparadas y en conserva (kilo)", "Hortalizas de hoja o de tallo (frescas o refrigeradas) (kilo)", "Coles (frescas o refrigeradas) (kilo)", 
                     "Hortalizas cultivadas por su fruto (frescas o refrigeradas) (kilo)", "Hortalizas con raíz o bulbo y setas (frescas o refrigeradas) (kilo)", "Legumbres y hortalizas secas (kilo)", 
                     "Verduras congeladas (kilo)", "Legumbres y hortalizas en conserva o preparadas y otros productos a base de legumbres y hortalizas (kilo)", "Patatas (kilo)", 
                     "Productos derivados de las patatas, mandioca y otros tubérculos (kilo)", "Azúcar (kilo)", "Confitura, mermelada y miel (kilo)", "Chocolate en barra o en tableta (kilo)", 
                     "Helados (kilo)", "Otros productos a base de azúcar (kilo)", "Café (kilo)", "Té e infusiones (kilo)", "Cacao (kilo)", "Agua mineral (litro)", "Bebidas refrescantes con o sin gas (litro)", 
                     "Zumos de frutas (litro)", "Zumos de vegetales (litro)", "Espirituosos y licores (litro)", "Vinos de uva y otras frutas fermentadas (litro)", "Otros vinos (litro)", 
                     "Cerveza (litro)", "Cigarrillos (cajetilla de 20)", "Puros y pequeños cigarros (unidad)", "Distribución de agua fría (vivienda principal) (m3)", 
                     "Distribución de agua fría (otras viviendas) (m3)", "Electricidad (vivienda principal) (Kwh)", "Electricidad (otras viviendas) (m3)", 
                     "Gas ciudad y natural (vivienda principal) (m3)", "Gas ciudad y natural (otras viviendas) (m3)", "Gas licuado (vivienda principal) (kilo)", 
                     "Gas licuado (otras viviendas) (kilo)", "Combustibles líquidos (vivienda principal) (litro)", "Combustibles líquidos (otras viviendas) (litro)", 
                     "Combustibles sólidos (vivienda principal) (kilo)", "Combustibles sólidos (otras viviendas) (kilo)", 
                     "Carburantes y lubricantes (litro)")
consumo_ingreso_final$TIPO_PRODUCTO <- nombres_codigos[match(consumo_ingreso_final$CODIGO, numeros_codigos)]
consumo_ingreso_final$TIPO_PRODUCTO <- paste(consumo_ingreso_final$CODIGO, consumo_ingreso_final$TIPO_PRODUCTO)


#Reorder the columns and write file with dataframe
consumo_ingreso_final <- consumo_ingreso_final[c(53,54,3:52)]
write.table(consumo_ingreso_final, "ConsumoIngreso2006a2015_pob.csv", sep=";", col.names=NA)


############### CREATE FILE WITH HOME PERCENTAGES PER YEAR ###########################

porcentaje_por_año <- list(Porcentaje2006, Porcentaje2007, Porcentaje2008, Porcentaje2009, Porcentaje2010, Porcentaje2011, Porcentaje2012, Porcentaje2013, Porcentaje2014, Porcentaje2015)
merge.all <- function(x, y) {
  merge(x, y, by=c("INTERIN"), all = TRUE)
}
porcentaje_final <- Reduce(merge.all, porcentaje_por_año)

write.table(porcentaje_final, "ConsumoIngreso2006a2015Porcentajes_pob.csv", sep=";", col.names= NA)


######################################################################################


