
### Indicadores Familia y composición de los hogares
### Unidad de Métodos y Acceso a datos
### Observatorio Uruguay
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

library(rio)
library(survey)
library(srvyr)
library(tidyverse)
library(laeken)
library(statar)
library(rmarkdown)
library(xlsx)
library(readxl)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


### REFERENCIAS BASE MOTOR
BASE_AUX <- data.frame("CODIND"=c(""),
                       "SEXO"=c(""),	
                       "ASCENDENCIA"=c(""),	
                       "QUINTIL"=c(""),
                       "DEPARTAMENTOUY"=c(""),	
                       "URBANORURALUY"=c(""),	
                       "PAÍS"=c(""),	
                       "ANIO"=c(""),	
                       "RESPONSABLE"=c(""),	
                       "EDAD"=c(""),
                       "POBRE"=c(""), 
                       "NSE"=c(""),
                       "NIVELEDU"=c(""))

### Carga de bases ###

ech            <- rio::import("Bases/ECH_2023.CSV")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

ech <- ech %>% dplyr::mutate(bc_correlat = ID)

ech <- ech %>% dplyr::mutate(jefe = case_when(e30==1 ~ 1, e30!=1 ~ 0))

ech <- ech %>% group_by(bc_correlat) %>% mutate(sumjefe=sum(jefe)) %>% as.data.frame()


### Generación de nuevas variables ###

# Ingresos
ech <- ech %>% dplyr::mutate(y_pc       =  HT11 / HT19 )                      #Ingreso per-cápita
ech <- ech %>% dplyr::mutate(y_pc_svl   =  (HT11 - HT13) / HT19)              #Ingreso per-cápita sin valor locativo

# Quintil de ingresos
ech_h <- ech %>% distinct(ID, .keep_all = TRUE)
ech_h <- ech_h %>% dplyr::mutate(quintilesy = statar::xtile(y_pc, n=5, wt = W_ANO))
ech_h <- ech_h[,c("ID","quintilesy")]
ech <- merge(ech, ech_h, by = "ID")

# Pobreza auxiliar
ech <- ech %>% dplyr::mutate(pobre_aux = case_when(pobre06 == 0 ~ 2,
                                                   pobre06 == 1 ~ 1))
                                                   

# Región
ech <- ech %>% dplyr::mutate(bd_region = case_when(REGION_4 == 1 | REGION_4 == 2 ~ 1,
                                                   REGION_4 == 3 ~ 2,
                                                   REGION_4 == 4 ~ 3))

# Renombro edad
ech <- ech %>% dplyr::mutate(bc_pe3 = e27)

# Sexo
ech <- ech %>% dplyr::mutate(bc_pe2 = e26)

# Ascendencia afro
ech <- ech %>% dplyr::mutate(bd_e29_1 = e29_1)


# Sexo del jefe/a de hogar
ech <- ech[order(ech$ID, ech$e30, decreasing = FALSE), ]
ech_h <- ech %>% distinct(ID, .keep_all = TRUE)
ech_h <- ech_h %>% dplyr::mutate(sexojefe = case_when(e30 == 1 & bc_pe2 == 1 ~ 1,
                                                        e30 == 1 & bc_pe2 == 2 ~ 2,
                                                        e30 != 1 ~ 99))
ech_h <- ech_h[,c("ID","sexojefe")]

ech <- merge(ech, ech_h, by = "ID")
ech <- ech %>% dplyr::mutate(sexojefe1 = case_when(sexojefe==1 ~ 1, sexojefe!=1 ~ 0))
ech <- ech %>% dplyr::mutate(sexojefe2 = case_when(sexojefe==2 ~ 1, sexojefe!=2 ~ 0))

#Tipo de hogar

ech <- ech %>% dplyr::mutate(bc_pe4 = case_when(e30==1 ~ 1, e30==2 ~ 2, e30 ==3|e30==4|e30==5 ~ 3, e30==6|e30==7 ~ 4, 
                                                  e30==8|e30==9|e30==10|e30==11|e30==12 ~ 5, e30==13 ~ 6, e30==14 ~ 7))
ech <- ech %>% dplyr::mutate(jefe = case_when(bc_pe4==1 ~ 1, bc_pe4!=1 ~ 0))
ech <- ech %>% dplyr::mutate(esposo_companero = case_when(bc_pe4==2 ~ 1, bc_pe4!=2 ~ 0))
ech <- ech %>% dplyr::mutate(hijos = case_when(bc_pe4==3 ~ 1, bc_pe4!=3 ~ 0))
ech <- ech %>% dplyr::mutate(otro_pariente = case_when(bc_pe4==4|bc_pe4==5 ~ 1, bc_pe4!=4&bc_pe4!=5 ~ 0))
ech <- ech %>% dplyr::mutate(otro_nopariente = case_when(bc_pe4==6 ~ 1, bc_pe4!=6 ~ 0))
ech <- ech %>% dplyr::mutate(servicio_domestico = case_when(bc_pe4==7 ~ 1, bc_pe4!=7 ~ 0))
ech <- ech %>% dplyr::mutate(jefatura_femenina = case_when(bc_pe4==1&bc_pe2==2 ~ 1, bc_pe4!=1|bc_pe2!=2 ~ 0))

ech <- ech %>% group_by(bc_correlat) %>% mutate(sumjefe=sum(jefe)) %>% as.data.frame()
ech <- ech %>% group_by(bc_correlat) %>% mutate(sumesposo_companero=sum(esposo_companero)) %>% as.data.frame()
ech <- ech %>% group_by(bc_correlat) %>% mutate(sumhijo=sum(hijos)) %>% as.data.frame()
ech <- ech %>% group_by(bc_correlat) %>% mutate(sumotro_pariente=sum(otro_pariente)) %>% as.data.frame()
ech <- ech %>% group_by(bc_correlat) %>% mutate(sumno_pariente=sum(otro_nopariente)) %>% as.data.frame()
ech <- ech %>% group_by(bc_correlat) %>% mutate(sumservicio_domestico=sum(servicio_domestico)) %>% as.data.frame()
ech <- ech %>% group_by(bc_correlat) %>% mutate(sumjefatura_femenina=sum(jefatura_femenina)) %>% as.data.frame()

#Constuyendo tipo de hogar#

#1.UNIPERSONAL 
#2. PAREJA SIN HIJOS
#3. BIPARENTAL 
#4. MONOPARENTAL FEMENINO 
#5. MONOPARENTAL MASCULINO 
#6. EXTENDIDO O COMPUESTO 
#7. EXTENDIDO O COMPUESTO CON NÚCLEO MONOPARENTAL
#8. SIN NÚCLEO CONYUGAL

ech <- ech %>% dplyr::mutate(tipo_hogar = case_when(sumesposo_companero==0 & sumhijo==0 & sumno_pariente==0 & sumotro_pariente==0 ~ 1, 
                                                      sumesposo_companero>=1 & sumhijo==0 & sumotro_pariente==0 & sumno_pariente==0 ~ 2, 
                                                      (sumesposo_companero>=1 & sumhijo>=1)& sumotro_pariente==0 & sumno_pariente==0 ~ 3, 
                                                      sumesposo_companero==0 & sumhijo>=1 & sumotro_pariente==0 & sumjefatura_femenina==1 & sumno_pariente==0 ~ 4, 
                                                      sumesposo_companero==0 & sumhijo>=1 & sumotro_pariente==0 & sumjefatura_femenina==0 & sumno_pariente==0 ~ 5, 
                                                      ((sumotro_pariente>=1 | sumno_pariente>=1)&(sumesposo_companero>=1 | sumhijo>=1)&sumjefatura_femenina==0)|((sumotro_pariente>=1 | sumno_pariente>=1)&sumesposo_companero>0&sumjefatura_femenina==1)|((sumotro_pariente>=1 | sumno_pariente>=1)&sumhijo==0&sumjefatura_femenina==1&sumesposo_companero>=1) ~ 6, 
                                                      (sumesposo_companero==0 &sumhijo>0&sumjefatura_femenina==1&sumotro_pariente>=1) | (sumesposo_companero==0 &sumhijo>0&sumjefatura_femenina==1&sumno_pariente>=1) ~ 7,
                                                      (sumotro_pariente >=1 | sumno_pariente >=1) & (sumesposo_companero==0 & sumhijo==0) ~ 8))

ech <- ech %>% dplyr::mutate(tipo_hogar1 = case_when(tipo_hogar==1 ~ 1)) 
ech <- ech %>% dplyr::mutate(tipo_hogar2 = case_when(tipo_hogar==2 ~ 1))
ech <- ech %>% dplyr::mutate(tipo_hogar3 = case_when(tipo_hogar==3 ~ 1)) 
ech <- ech %>% dplyr::mutate(tipo_hogar4 = case_when(tipo_hogar==4 ~ 1)) 
ech <- ech %>% dplyr::mutate(tipo_hogar5 = case_when(tipo_hogar==5 ~ 1)) 
ech <- ech %>% dplyr::mutate(tipo_hogar6 = case_when(tipo_hogar==6 ~ 1)) 
ech <- ech %>% dplyr::mutate(tipo_hogar7 = case_when(tipo_hogar==7 ~ 1))
ech <- ech %>% dplyr::mutate(tipo_hogar8 = case_when(tipo_hogar==8 ~ 1))
ech <- mutate_at(ech, c("tipo_hogar1", "tipo_hogar2", "tipo_hogar3", "tipo_hogar4", "tipo_hogar5","tipo_hogar6", "tipo_hogar7", "tipo_hogar8"), ~replace(., is.na(.), 0))


#Ciclo de vida del hogar
ech <- ech %>% dplyr::mutate(mujmen40 = case_when((bc_pe4==1 | bc_pe4==2) & bc_pe2==2 & bc_pe3<40 ~ 1, 
                                                     (bc_pe4!=1 & bc_pe4!=2) | bc_pe2!=2 | bc_pe3>=40 ~ 0))
ech <- ech %>% dplyr::mutate(mujmay40 = case_when((bc_pe4==1 | bc_pe4==2) & bc_pe2==2 & bc_pe3>=40 ~ 1, 
                                                     (bc_pe4!=1 & bc_pe4!=2) | bc_pe2!=2 | bc_pe3<40 ~ 0))
ech <- ech %>% dplyr::mutate(hijo_men5 = case_when(hijos==1 & bc_pe3<6 ~ 1, 
                                                      (hijos!=1 | bc_pe3>=6 ~ 0)))
ech <- ech %>% dplyr::mutate(hijo_6a12 = case_when(hijos==1 & bc_pe3>5&bc_pe3<13 ~ 1, 
                                                     (hijos!=1 | (bc_pe3<6 | bc_pe3>12) ~ 0)))
ech <- ech %>% dplyr::mutate(hijo_may12 = case_when(hijos==1 & bc_pe3>12 ~ 1, 
                                                      (hijos!=1 | (bc_pe3<13) ~ 0)))
ech <- ech %>% dplyr::mutate(hijo_menos12 = case_when(hijos==1 & (hijo_6a12==1 | hijo_men5==1) ~ 1, 
                                                        (hijos!=1 | (hijo_6a12!=1 & hijo_men5!=1) ~ 0)))
ech <- ech %>% dplyr::mutate(hijo_13y18 = case_when(hijos==1 & (bc_pe3>12 & bc_pe3<19) ~ 1, 
                                                      (hijos!=1 | (bc_pe3<13 | bc_pe3>18) ~ 0)))
ech <- ech %>% dplyr::mutate(hijo_may18 = case_when(hijos==1 & bc_pe3>18 ~ 1, 
                                                      (hijos!=1 | bc_pe3<19 ~ 0)))
ech <- ech %>% group_by(bc_correlat) %>% mutate(sumhijo_men5 =sum(hijo_men5)) %>% as.data.frame()
ech <- ech %>% group_by(bc_correlat) %>% mutate(sumhijo_6a12 =sum(hijo_6a12)) %>% as.data.frame()
ech <- ech %>% group_by(bc_correlat) %>% mutate(sumhijo_may12 =sum(hijo_may12)) %>% as.data.frame()
ech <- ech %>% group_by(bc_correlat) %>% mutate(sumhijo_menos12 =sum(hijo_menos12)) %>% as.data.frame()
ech <- ech %>% group_by(bc_correlat) %>% mutate(sumhijo_13y18 =sum(hijo_13y18)) %>% as.data.frame()
ech <- ech %>% group_by(bc_correlat) %>% mutate(sumhijo_may18 =sum(hijo_may18)) %>% as.data.frame()
ech <- ech %>% group_by(bc_correlat) %>% mutate(summujmen40 =sum(mujmen40)) %>% as.data.frame()
ech <- ech %>% group_by(bc_correlat) %>% mutate(summujmay40 =sum(mujmay40)) %>% as.data.frame()

#Constuyendo ciclo de vida del hogar#

#1. Pareja joven sin hijos 
#2. Etapa inicial
#3. Etapa expansión o crecimiento
#4. Etapa consolidación 
#5. Etapa de salida 
#6. Nido vacío (pareja mayor sin hijos) 
#7. Hogares no familaires

ech <- ech %>% dplyr::mutate(ciclo_vida = case_when(sumesposo_companero>=1 & sumhijo==0 & summujmen40==1 ~ 1, 
                                                      (sumhijo>0 ) & (sumhijo_men5>0) & sumhijo_6a12==0 & sumhijo_may12==0 ~ 2, 
                                                      (sumhijo>0 ) & ((sumhijo_men5>0) & (sumhijo_6a12>0)) | (sumhijo_men5==0 & (sumhijo_6a12>0))& sumhijo_may12==0 ~ 3, 
                                                      (sumhijo>0 ) & (sumhijo_13y18>0) ~ 4, 
                                                      (sumhijo>0 ) & (sumhijo_men5==0 & sumhijo_6a12==0 & sumhijo_13y18==0 & (sumhijo_may18>0)) ~ 5, 
                                                      sumesposo_companero>=1 & sumhijo==0 & summujmay40==1 ~ 6, 
                                                      ((sumotro_pariente >=1 | sumno_pariente >=1) & (sumesposo_companero==0 & sumhijo==0))|(sumesposo_companero==0 & sumhijo==0 & sumno_pariente==0 & sumotro_pariente==0) ~ 7))

ech <- ech %>% dplyr::mutate(ciclo_vida1 = case_when(ciclo_vida==1 ~ 1, ciclo_vida!=1 ~ 0)) 
ech <- ech %>% dplyr::mutate(ciclo_vida2 = case_when(ciclo_vida==2 ~ 1, ciclo_vida!=2 ~ 0)) 
ech <- ech %>% dplyr::mutate(ciclo_vida3 = case_when(ciclo_vida==3 ~ 1, ciclo_vida!=3 ~ 0)) 
ech <- ech %>% dplyr::mutate(ciclo_vida4 = case_when(ciclo_vida==4 ~ 1, ciclo_vida!=4 ~ 0)) 
ech <- ech %>% dplyr::mutate(ciclo_vida5 = case_when(ciclo_vida==5 ~ 1, ciclo_vida!=5 ~ 0)) 
ech <- ech %>% dplyr::mutate(ciclo_vida6 = case_when(ciclo_vida==6 ~ 1, ciclo_vida!=6 ~ 0)) 
ech <- ech %>% dplyr::mutate(ciclo_vida7 = case_when(ciclo_vida==7 ~ 1, ciclo_vida!=7 ~ 0)) 
ech <- mutate_at(ech, c("ciclo_vida1", "ciclo_vida2", "ciclo_vida3", "ciclo_vida4", "ciclo_vida5","ciclo_vida6", "ciclo_vida7"), ~replace(., is.na(.), 0))

### Modelo de pareja
ech <- ech %>% dplyr::mutate(bc_pobp = POBPCOAC)
ech <- ech %>% dplyr::mutate(bc_horas = f85+f98)
ech <- ech %>% dplyr::mutate(bc_horas_1 = f85)

#generamos la variable que identifica hogares en donde hay una pareja 

ech <- ech %>% dplyr::mutate(biparentales = case_when(sumesposo_companero>0 ~ 1, sumesposo_companero==0 ~ 0))

#Nos quedamos solo con los hogares biparentales*


#generamos variables a nivel agregado
#Primero generamos variable a nivel personas*

#JEFE Ocupado tiempo completo_varón*
ech <- ech %>% dplyr::mutate(jefeocup_tc_v = case_when(bc_pobp==2 & (bc_horas>39 | bc_horas_1>39) & jefe==1 & bc_pe2==1&biparentales==1 ~ 1))

#JEFE Ocupado tiempo completo_mujer*
ech <- ech %>% dplyr::mutate(jefeocup_tc_m = case_when(bc_pobp==2 & (bc_horas>39 | bc_horas_1>39) & jefe==1 & bc_pe2==2&biparentales==1 ~ 1))

#CONYUGE Ocupado tiempo completo*
ech <- ech %>% dplyr::mutate(conyocup_tc_v = case_when(bc_pobp==2 & (bc_horas>39 | bc_horas_1>39) & esposo_companero==1 & bc_pe2==1&biparentales==1 ~ 1))
ech <- ech %>% dplyr::mutate(conyocup_tc_m = case_when(bc_pobp==2 & (bc_horas>39 | bc_horas_1>39) & esposo_companero==1 & bc_pe2==2&biparentales==1 ~ 1))

#JEFE Ocupado medio tiempo_varón*
ech <- ech %>% dplyr::mutate(jefeocup_mt_v = case_when(bc_pobp==2 & bc_horas<40 & jefe==1 & bc_pe2==1&biparentales==1 ~ 1))

#JEFE Ocupado medio tiempo_mujer*
ech <- ech %>% dplyr::mutate(jefeocup_mt_m = case_when(bc_pobp==2 & bc_horas<40& jefe==1 & bc_pe2==2&biparentales==1 ~ 1))

#CONYUGE Ocupado medio tiempo_varón*
ech <- ech %>% dplyr::mutate(conyocup_mt_v = case_when(bc_pobp==2 & bc_horas<40& esposo_companero==1 & bc_pe2==1&biparentales==1 ~ 1))

#CONYUGE Ocupado medio tiempo_mujer*
ech <- ech %>% dplyr::mutate(conyocup_mt_m = case_when(bc_pobp==2 & bc_horas<40& esposo_companero==1 & bc_pe2==2&biparentales==1 ~ 1))

#JEFE Inactivo o desempleado_varón*
ech <- ech %>% dplyr::mutate(jefeinact_desoc_v = case_when(bc_pobp>2 & bc_pobp<12 & jefe==1 & bc_pe2==1&biparentales==1 ~ 1))

#JEFE Inactivo o desempleado_mujer*
ech <- ech %>% dplyr::mutate(jefeinact_desoc_m = case_when(bc_pobp>2 & bc_pobp<12 & jefe==1 & bc_pe2==2&biparentales==1 ~ 1))

#CONYUGE Inactivo o desempleado_varón*
ech <- ech %>% dplyr::mutate(conyinact_desoc_v = case_when(bc_pobp>2 & bc_pobp<12 & esposo_companero==1 & bc_pe2==1&biparentales==1 ~ 1))

#CONYUGE Inactivo o desempleado_mujer*
ech <- ech %>% dplyr::mutate(conyinact_desoc_m = case_when(bc_pobp>2 & bc_pobp<12 & esposo_companero==1 & bc_pe2==2&biparentales==1 ~ 1))

ech <- mutate_at(ech, c("jefeocup_tc_v", "jefeocup_tc_m", "conyocup_tc_v", "conyocup_tc_m", "jefeocup_mt_v", "jefeocup_mt_m", 
                          "conyocup_mt_v", "conyocup_mt_m", "jefeinact_desoc_v", "jefeinact_desoc_m", "conyinact_desoc_v", "conyinact_desoc_m"), ~replace(., is.na(.), 0))

#### Segundo, variables agregadas a nivel hogar
ech <- ech %>% group_by(bc_correlat) %>% mutate(sumjefeocup_tc_m=sum(jefeocup_tc_m)) %>% as.data.frame()
ech <- ech %>% group_by(bc_correlat) %>% mutate(sumjefeocup_tc_v=sum(jefeocup_tc_v)) %>% as.data.frame()
ech <- ech %>% group_by(bc_correlat) %>% mutate(sumconyocup_tc_m=sum(conyocup_tc_m)) %>% as.data.frame()
ech <- ech %>% group_by(bc_correlat) %>% mutate(sumconyocup_tc_v=sum(conyocup_tc_v)) %>% as.data.frame()
ech <- ech %>% group_by(bc_correlat) %>% mutate(sumjefeocup_mt_m=sum(jefeocup_mt_m)) %>% as.data.frame()
ech <- ech %>% group_by(bc_correlat) %>% mutate(sumjefeocup_mt_v=sum(jefeocup_mt_v)) %>% as.data.frame()
ech <- ech %>% group_by(bc_correlat) %>% mutate(sumconyocup_mt_v=sum(conyocup_mt_v)) %>% as.data.frame()
ech <- ech %>% group_by(bc_correlat) %>% mutate(sumconyocup_mt_m=sum(conyocup_mt_m)) %>% as.data.frame()
ech <- ech %>% group_by(bc_correlat)  %>% mutate(sumjefeinact_desoc_m=sum(jefeinact_desoc_m)) %>% as.data.frame()
ech <- ech %>% group_by(bc_correlat) %>% mutate(sumjefeinact_desoc_v=sum(jefeinact_desoc_v)) %>% as.data.frame()
ech <- ech %>% group_by(bc_correlat) %>% mutate(sumconyinact_desoc_m=sum(conyinact_desoc_m)) %>% as.data.frame()
ech <- ech %>% group_by(bc_correlat) %>% mutate(sumconyinact_desoc_v=sum(conyinact_desoc_v)) %>% as.data.frame()

#### Generamos la variable de modelos de pareja con las categorías de arriba
#1. PROVEEDOR TRADICIONAL (solo hombre trabaja, mujer inactiva o desempleada)*
#2 PROVEEDOR MODIFICADO: pareja donde ambos trabajan para el mercado pero el hombre trabaja a tiempo completo y la mujer a tiempo parcial. 
#3. DOBLE CARRERA:  pareja donde ambos trabajan para el mercado, ambos a tiempo completo o ambos a tiempo parcial. 
#4. INVERSIÓN DE ROLES: pareja donde sÓlo la mujer trabaja para el mercado y el hombre es inactivo o desocupado. 
#5. INVERSIÓN DE ROLES MODIFICADO: ambos trabajan en el mercado laboral pero hombre ocupado a tiempo parcial y mujer ocupada a tiempo completo.
#6. MODELO RESIDUAL: Ambos no trabajan (desocupados o inactivos)

ech <- ech %>% dplyr::mutate(modelo_pareja = case_when(((sumjefeocup_tc_v==1 & sumconyinact_desoc_m==1)|(sumconyocup_tc_v==1 & sumjefeinact_desoc_m==1)|(sumjefeocup_mt_v==1 & sumconyinact_desoc_m==1)|(sumconyocup_mt_v==1 & sumjefeinact_desoc_m==1))&biparentales==1 ~ 1, 
                                                         ((sumjefeocup_tc_v==1 & sumconyocup_mt_m==1) | (sumconyocup_tc_v==1 & sumjefeocup_mt_m==1))&biparentales==1 ~ 2,
                                                         ((sumjefeocup_tc_v==1 & sumconyocup_tc_m==1) | (sumjefeocup_tc_m==1 & sumconyocup_tc_v==1)| (sumjefeocup_mt_m==1 & sumconyocup_mt_v==1) | (sumjefeocup_mt_v==1 & sumconyocup_mt_m==1))&biparentales==1 ~3,
                                                         ((sumjefeocup_tc_m==1 & sumconyinact_desoc_v==1) | (sumconyocup_tc_m==1 & sumjefeinact_desoc_v==1)| (sumjefeocup_mt_m==1 & sumconyinact_desoc_v==1) | (sumconyocup_mt_m==1 & sumjefeinact_desoc_v==1))&biparentales==1 ~4,
                                                         ((sumjefeocup_tc_m==1 & sumconyocup_mt_v==1) | (sumconyocup_tc_m==1 & sumjefeocup_mt_v==1))&biparentales==1 ~ 5, 
                                                         ((sumjefeinact_desoc_m==1 & sumconyinact_desoc_v==1) | (sumjefeinact_desoc_v==1 & sumconyinact_desoc_m==1))&biparentales==1 ~6))

ech <- ech %>% dplyr::mutate(modelo_pareja1 = case_when(modelo_pareja==1 ~ 1, modelo_pareja!=1 ~ 0)) 
ech <- ech %>% dplyr::mutate(modelo_pareja2 = case_when(modelo_pareja==2 ~ 1, modelo_pareja!=2 ~ 0)) 
ech <- ech %>% dplyr::mutate(modelo_pareja3 = case_when(modelo_pareja==3 ~ 1, modelo_pareja!=3 ~ 0)) 
ech <- ech %>% dplyr::mutate(modelo_pareja4 = case_when(modelo_pareja==4 ~ 1, modelo_pareja!=4 ~ 0)) 
ech <- ech %>% dplyr::mutate(modelo_pareja5 = case_when(modelo_pareja==5 ~ 1, modelo_pareja!=5 ~ 0)) 
ech <- ech %>% dplyr::mutate(modelo_pareja6 = case_when(modelo_pareja==6 ~ 1, modelo_pareja!=6 ~ 0)) 
ech <- mutate_at(ech, c("modelo_pareja1", "modelo_pareja2", "modelo_pareja3", "modelo_pareja4", "modelo_pareja5","modelo_pareja6"), ~replace(., is.na(.), 0))

####Estado civil
ech <- ech %>% dplyr::mutate(bc_pe5 = case_when(e35==2 | e35==3 | e35==6 | e35==7 ~ 1, 
                                                    e35==4 | e35==5 ~ 2,
                                                    e35==0 & (e36==1 | e36==2 | e36==3) ~ 3,
                                                    e35==0 & (e36==4 | e36==6) ~ 4,
                                                    e35==0 & e36==5 ~ 5)) 

ech <- ech %>% dplyr::mutate(estado_civil1 = case_when(bc_pe5==1 ~ 1, bc_pe5!=1 ~ 0)) 
ech <- ech %>% dplyr::mutate(estado_civil2 = case_when(bc_pe5==2 ~ 1, bc_pe5!=2 ~ 0)) 
ech <- ech %>% dplyr::mutate(estado_civil3 = case_when(bc_pe5==3 ~ 1, bc_pe5!=3 ~ 0)) 
ech <- ech %>% dplyr::mutate(estado_civil4 = case_when(bc_pe5==4 ~ 1, bc_pe5!=4 ~ 0)) 
ech <- ech %>% dplyr::mutate(estado_civil5 = case_when(bc_pe5==5 ~ 1, bc_pe5!=5 ~ 0)) 
ech <- mutate_at(ech, c("estado_civil1", "estado_civil2", "estado_civil3", "estado_civil4", "estado_civil5"), ~replace(., is.na(.), 0))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#Asistencia a centros educativos
ech <- ech %>% dplyr::mutate(asiste = case_when(e49==3 ~ 1, e49!=3 ~ 0)) 


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
###Menores de 15 
ech <- ech %>% dplyr::mutate(men15 = case_when(bc_pe3<15 ~ 1, bc_pe3>=15 ~ 0)) 
ech <- ech %>% group_by(bc_correlat) %>% mutate(men15h=max(men15)) %>% as.data.frame()

###Menores de 18 
ech <- ech %>% dplyr::mutate(men0a4 = case_when(bc_pe3<5 ~ 1, bc_pe3>=5 ~ 0)) 
ech <- ech %>% group_by(bc_correlat) %>% mutate(men0a4h=max(men0a4)) %>% as.data.frame()

ech <- ech %>% dplyr::mutate(men5a12 = case_when(bc_pe3>4&bc_pe3<13 ~ 1, bc_pe3>=12|bc_pe3<5 ~ 0)) 
ech <- ech %>% group_by(bc_correlat) %>% mutate(men5a12h=max(men5a12)) %>% as.data.frame()

ech <- ech %>% dplyr::mutate(men13a17 = case_when(bc_pe3>12&bc_pe3<18 ~ 1, bc_pe3<13|bc_pe3>=18 ~ 0)) 
ech <- ech %>% group_by(bc_correlat) %>% mutate(men13a17h=max(men13a17)) %>% as.data.frame()

ech <- ech %>% dplyr::mutate(men18 = case_when(bc_pe3<18 ~ 1, bc_pe3>=18 ~ 0)) 
ech <- ech %>% group_by(bc_correlat) %>% mutate(men18h=max(men18)) %>% as.data.frame()

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
### Menores de 13 en el hogar
ech <- ech %>% dplyr::mutate(menor13 = case_when(bc_pe3<13 ~ 1, bc_pe3>=13 ~ 0))
ech <- ech %>% group_by(bc_correlat) %>% mutate(menor13H=max(menor13)) %>% as.data.frame()

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
### Menores de 4 en el hogar
ech <- ech %>% dplyr::mutate(menor4 = case_when(bc_pe3<4 ~ 1, bc_pe3>=4 ~ 0))
ech <- ech %>% group_by(bc_correlat) %>% mutate(menor4H=max(menor4)) %>% as.data.frame()

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
### Menores de 7 en el hogar
ech <- ech %>% dplyr::mutate(menor7 = case_when(bc_pe3<7 ~ 1, bc_pe3>=7 ~ 0))
ech <- ech %>% group_by(bc_correlat) %>% mutate(menor7H=max(menor7)) %>% as.data.frame()

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
###Mayores de 64  
ech <- ech %>% dplyr::mutate(may64 = case_when(bc_pe3>64 ~ 1, bc_pe3<=64 ~ 0)) 
ech <- ech %>% group_by(bc_correlat) %>% mutate(may64h=max(may64)) %>% as.data.frame()


# Tramo de edad
ech <- ech %>% dplyr::mutate(tramo_edad = case_when(bc_pe3>=0  & bc_pe3<=5  ~ 1,
                                                      bc_pe3>=6  & bc_pe3<=12 ~ 2,
                                                      bc_pe3>=13 & bc_pe3<=18 ~ 3,
                                                      bc_pe3>=19 & bc_pe3<=24 ~ 4,
                                                      bc_pe3>=25 & bc_pe3<=29 ~ 5,
                                                      bc_pe3>=30 & bc_pe3<=64 ~ 6,
                                                      bc_pe3>=65 ~ 7))
ech <- ech %>% dplyr::mutate(tramo_edad2 = case_when(bc_pe3>=0  & bc_pe3<=29  ~ 1,
                                                       bc_pe3>=30  & bc_pe3<=44 ~ 2,
                                                       bc_pe3>=45 & bc_pe3<=64 ~ 3,
                                                       bc_pe3>=65 ~ 4))
ech <- ech %>% dplyr::mutate(tramo_edad3 = case_when(bc_pe3<=29  ~ 1,
                                                       bc_pe3>=30 & bc_pe3<=49 ~ 2))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
###Activos
ech <- ech %>% dplyr::mutate(activos1 = case_when(bc_pe2==1&bc_pobp>5 ~ 0, bc_pobp>=2&bc_pobp<=5&bc_pe2==1 ~ 1))
ech <- ech %>% dplyr::mutate(activos2 = case_when(bc_pe2==2&bc_pobp>5 ~ 0, bc_pobp>=2&bc_pobp<=5&bc_pe2==2 ~ 1))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
###Cobertura de salud
ech <- ech %>% dplyr::mutate(nocobert=case_when(e45_cv==7 ~ 1, e45_cv!=7 ~ 0)) 


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
###Cantidad de integrantes en el hogar
ech <- ech %>% dplyr::mutate(identif = case_when(bc_pe4!=7 ~ 1, bc_pe4==7 ~ 0))
ech <- ech %>% group_by(bc_correlat) %>% mutate(n_int=sum(identif)) %>% as.data.frame()
ech <- ech %>% dplyr::mutate(nint1 = case_when(n_int==1~1))
ech <- ech %>% dplyr::mutate(nint2 = case_when(n_int==2~1))
ech <- ech %>% dplyr::mutate(nint3 = case_when(n_int==3~1))
ech <- ech %>% dplyr::mutate(nint4 = case_when(n_int==4~1))
ech <- ech %>% dplyr::mutate(nint5 = case_when(n_int>=5~1))

ech <- mutate_at(ech, c("nint1", "nint2", "nint3", "nint4", "nint5"), ~replace(., is.na(.), 0))


### Bases menores de 18 ###                                
ech_men <- ech %>%  filter(men18h >0)

### Bases a nivel hogar ###                               

ech_h <- ech %>% distinct(ID, .keep_all = TRUE)
ech_h_men <- ech_men %>% distinct(ID, .keep_all = TRUE)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

### Información de muestreo ###

ech_svy           <- srvyr::as_survey_design(ech, ids = bc_correlat, weights = W_ANO)
ech_h_svy         <- srvyr::as_survey_design(ech_h, ids = bc_correlat, weights = W_ANO)
ech_h_men_svy         <- srvyr::as_survey_design(ech_h_men, ids = bc_correlat, weights = W_ANO)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

### Estimación de indicadores ###


### 500 Distribución porcentual de los hogares según tipo de hogar


## País Urbano

ech_h_svy_1 <- ech_h_svy %>%
  filter(bd_region == 1 & tipo_hogar>0)

# Total

NOMINDICADOR <- 
c("Distribución porcentual de los hogares según tipo de hogar (Unipersonal)",
"Distribución porcentual de los hogares según tipo de hogar (Pareja sin hijos)", 
"Distribución porcentual de los hogares según tipo de hogar (Biparental)",
"Distribución porcentual de los hogares según tipo de hogar (Monoparental femenino)",
"Distribución porcentual de los hogares según tipo de hogar (Monoparental masculino)",
"Distribución porcentual de los hogares según tipo de hogar (Extendido o compuesto)", 
"Distribución porcentual de los hogares según tipo de hogar (Extendido con núcleo monoparental)", 
"Distribución porcentual de los hogares según tipo de hogar (Sin núcleo)" )

var_int <- c("tipo_hogar1", "tipo_hogar2", "tipo_hogar3", "tipo_hogar4", "tipo_hogar5", "tipo_hogar6", "tipo_hogar7", "tipo_hogar8")
a_sem <- sapply(ech_h_svy_1$variables %>% select(all_of(var_int)), function(x){
  ech_h_svy_1 %>%
        srvyr::summarise(stat = srvyr::survey_mean(x, na.rm = TRUE)) 
})

c_ano <- matrix(,8,1)
for (i in 1:8) {
  c_ano[i,1] <- c(as.numeric(c(a_sem[1,i])))
}

colnames(c_ano) <- c("VALOR")
c_ano <- cbind(c_ano, NOMINDICADOR)
c_ano <- as.data.frame(c_ano)
BASE_MOTOR <- cbind(c_ano,BASE_AUX)

# Sexo del jefe

a_jefe <- function(y) {
  base <- subset(ech_h_svy_1, sexojefe == y) 
  resultados  <-  sapply(base$variables %>% select(all_of(var_int)), function(x){
    base %>%
      srvyr::summarise(stat1 = srvyr::survey_mean(x, na.rm = TRUE))
    
  })
  y <- as.numeric(resultados[1,])
}

a_e_jefe = matrix(, nrow = 8, ncol = 2)

for(i in 1:2){
  a_e_jefe[,i] <- a_jefe(y = i)
}

c_jefe <- as.data.frame(a_e_jefe)
c_jefe_1 <- cbind(as.data.frame(c_jefe[,1]), NOMINDICADOR, BASE_AUX)
c_jefe_1 <- c_jefe_1 %>% dplyr::mutate(SEXO = "VARONES")
c_jefe_2 <- cbind(as.data.frame(c_jefe[,2]), NOMINDICADOR, BASE_AUX)
c_jefe_2 <- c_jefe_2 %>% dplyr::mutate(SEXO = "MUJERES")
colnames(c_jefe_1) <- colnames(BASE_MOTOR)
colnames(c_jefe_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_jefe_1,c_jefe_2)


# Tramo de edad

a_edad <- function(y) {
  base <- subset(ech_h_svy_1, tramo_edad2 == y) 
  resultados  <-  sapply(base$variables %>% select(all_of(var_int)), function(x){
    base %>%
      srvyr::summarise(stat1 = srvyr::survey_mean(x, na.rm = TRUE))
    
  })
  y <- as.numeric(resultados[1,])
}

a_e_edad = matrix(, nrow = 8, ncol = 4)

for(i in 1:4){
  a_e_edad[,i] <- a_edad(y = i)
}

c_edad <- as.data.frame(a_e_edad)
c_edad_1 <- cbind(as.data.frame(c_edad[,1]), NOMINDICADOR, BASE_AUX)
c_edad_1 <- c_edad_1 %>% dplyr::mutate(EDAD = "Menor de 30 años")
c_edad_2 <- cbind(as.data.frame(c_edad[,2]), NOMINDICADOR, BASE_AUX)
c_edad_2 <- c_edad_2 %>% dplyr::mutate(EDAD = "Entre 30 y 44 años")
c_edad_3 <- cbind(as.data.frame(c_edad[,3]), NOMINDICADOR, BASE_AUX)
c_edad_3 <- c_edad_3 %>% dplyr::mutate(EDAD = "Entre 45 y 64 años")
c_edad_4 <- cbind(as.data.frame(c_edad[,4]), NOMINDICADOR, BASE_AUX)
c_edad_4 <- c_edad_4 %>% dplyr::mutate(EDAD = "65 años o más")
colnames(c_edad_1) <- colnames(BASE_MOTOR)
colnames(c_edad_2) <- colnames(BASE_MOTOR)
colnames(c_edad_3) <- colnames(BASE_MOTOR)
colnames(c_edad_4) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_edad_1,c_edad_2, c_edad_3, c_edad_4)


# Quintil de ingreso del hogar
a_quintil <- function(y) {
  base <- subset(ech_h_svy_1, quintilesy == y) 
  resultados  <-  sapply(base$variables %>% select(all_of(var_int)), function(x){
    base %>%
      srvyr::summarise(stat1 = srvyr::survey_mean(x, na.rm = TRUE))
    
  })
  y <- as.numeric(resultados[1,])
}

a_e_quintil = matrix(, nrow = 8, ncol = 5)

for(i in 1:5){
  a_e_quintil[,i] <- a_quintil(y = i)
}

c_quintil <- as.data.frame(a_e_quintil)
c_quintil_1 <- cbind(as.data.frame(c_quintil[,1]), NOMINDICADOR, BASE_AUX)
c_quintil_1 <- c_quintil_1 %>% dplyr::mutate(QUINTIL = "Quintil 1")
c_quintil_2 <- cbind(as.data.frame(c_quintil[,2]), NOMINDICADOR, BASE_AUX)
c_quintil_2 <- c_quintil_2 %>% dplyr::mutate(QUINTIL = "Quintil 2")
c_quintil_3 <- cbind(as.data.frame(c_quintil[,3]), NOMINDICADOR, BASE_AUX)
c_quintil_3 <- c_quintil_3 %>% dplyr::mutate(QUINTIL = "Quintil 3")
c_quintil_4 <- cbind(as.data.frame(c_quintil[,4]), NOMINDICADOR, BASE_AUX)
c_quintil_4 <- c_quintil_4 %>% dplyr::mutate(QUINTIL = "Quintil 4")
c_quintil_5 <- cbind(as.data.frame(c_quintil[,5]), NOMINDICADOR, BASE_AUX)
c_quintil_5 <- c_quintil_5 %>% dplyr::mutate(QUINTIL = "Quintil 5")
colnames(c_quintil_1) <- colnames(BASE_MOTOR)
colnames(c_quintil_2) <- colnames(BASE_MOTOR)
colnames(c_quintil_3) <- colnames(BASE_MOTOR)
colnames(c_quintil_4) <- colnames(BASE_MOTOR)
colnames(c_quintil_5) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_quintil_1,c_quintil_2, c_quintil_3, c_quintil_4, c_quintil_5)




# Base motor

BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(CODIND = "500")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(CATEGORIA = "Composición y estructura de los hogares")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(PESTAÑA = "País urbano")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(PAÍS = "Uruguay")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(ANIO = 2023)
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(RESPONSABLE = "Sharon Katzkowicz")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(ASCENDENCIA = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(DEPARTAMENTOUY = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(URBANORURALUY = "Urbano (más de 5.000 habitantes)")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(POBRE = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(NSE = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(NIVELEDU = "Todos")
BASE_MOTOR <- as.data.frame(BASE_MOTOR)

BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(SEXO = case_when(SEXO == "" ~ "Todos", SEXO == "MUJERES"~"Mujeres", SEXO=="VARONES"~"Varones"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(QUINTIL = case_when(QUINTIL == "" ~ "Todos",
                                                            QUINTIL == "Quintil 1"~"Quintil 1", 
                                                            QUINTIL == "Quintil 2"~"Quintil 2",
                                                            QUINTIL == "Quintil 3"~"Quintil 3", 
                                                            QUINTIL == "Quintil 4"~"Quintil 4",
                                                            QUINTIL == "Quintil 5"~"Quintil 5"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(EDAD = case_when(EDAD == "" ~ "Todos",
                                                               EDAD == "Menor de 30 años"~"Menor de 30 años", 
                                                               EDAD == "Entre 30 y 44 años"~"Entre 30 y 44 años",
                                                               EDAD == "Entre 45 y 64 años"~"Entre 45 y 64 años", 
                                                               EDAD == "65 años o más"~"65 años o más"))


m500_pu <- BASE_MOTOR %>% select(CODIND,	
                        NOMINDICADOR,	
                        CATEGORIA,	
                        PESTAÑA,	
                        SEXO,	
                        ASCENDENCIA,	
                        QUINTIL,	
                        DEPARTAMENTOUY,	
                        URBANORURALUY,	
                        EDAD,	
                        POBRE,
                        NSE,
                        NIVELEDU,
                        PAÍS,	
                        ANIO,	
                        VALOR,	
                        RESPONSABLE)	


## Total país

# Total
ech_h_svy_1 <- ech_h_svy %>%
  filter(tipo_hogar>0)

NOMINDICADOR <- 
  c("Distribución porcentual de los hogares según tipo de hogar (Unipersonal)",
    "Distribución porcentual de los hogares según tipo de hogar (Pareja sin hijos)", 
    "Distribución porcentual de los hogares según tipo de hogar (Biparental)",
    "Distribución porcentual de los hogares según tipo de hogar (Monoparental femenino)",
    "Distribución porcentual de los hogares según tipo de hogar (Monoparental masculino)",
    "Distribución porcentual de los hogares según tipo de hogar (Extendido o compuesto)", 
    "Distribución porcentual de los hogares según tipo de hogar (Extendido con núcleo monoparental)", 
    "Distribución porcentual de los hogares según tipo de hogar (Sin núcleo)" )

var_int <- c("tipo_hogar1", "tipo_hogar2", "tipo_hogar3", "tipo_hogar4", "tipo_hogar5", "tipo_hogar6", "tipo_hogar7", "tipo_hogar8")
a_sem <- sapply(ech_h_svy_1$variables %>% select(all_of(var_int)), function(x){
  ech_h_svy_1 %>%
    srvyr::summarise(stat = srvyr::survey_mean(x, na.rm = TRUE)) 
})

c_ano <- matrix(,8,1)
for (i in 1:8) {
  c_ano[i,1] <- c(as.numeric(c(a_sem[1,i])))
}

colnames(c_ano) <- c("VALOR")
c_ano <- cbind(c_ano, NOMINDICADOR)
c_ano <- as.data.frame(c_ano)
BASE_MOTOR <- cbind(c_ano,BASE_AUX)


# Región

a_reg <- function(y) {
  base <- subset(ech_h_svy_1, bd_region == y) 
  resultados  <-  sapply(base$variables %>% select(all_of(var_int)), function(x){
    base %>%
      srvyr::summarise(stat1 = srvyr::survey_mean(x, na.rm = TRUE))
    
  })
  y <- as.numeric(resultados[1,])
}

a_e_reg = matrix(, nrow = 8, ncol = 3)

for(i in 1:3){
  a_e_reg[,i] <- a_reg(y = i)
}

c_reg <- as.data.frame(a_e_reg[,2:3])
c_reg_1 <- cbind(as.data.frame(c_reg[,1]), NOMINDICADOR, BASE_AUX)
c_reg_1 <- c_reg_1 %>% dplyr::mutate(URBANORURALUY = "URBANO (MENOS DE 5.000 HABITANTES)")
c_reg_2 <- cbind(as.data.frame(c_reg[,2]), NOMINDICADOR, BASE_AUX)
c_reg_2 <- c_reg_2 %>% dplyr::mutate(URBANORURALUY = "RURAL DISPERSO")
colnames(c_reg_1) <- colnames(BASE_MOTOR)
colnames(c_reg_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_reg_1,c_reg_2)


# Sexo del jefe

a_jefe <- function(y) {
  base <- subset(ech_h_svy_1, sexojefe == y) 
  resultados  <-  sapply(base$variables %>% select(all_of(var_int)), function(x){
    base %>%
      srvyr::summarise(stat1 = srvyr::survey_mean(x, na.rm = TRUE))
    
  })
  y <- as.numeric(resultados[1,])
}

a_e_jefe = matrix(, nrow = 8, ncol = 2)

for(i in 1:2){
  a_e_jefe[,i] <- a_jefe(y = i)
}

c_jefe <- as.data.frame(a_e_jefe)
c_jefe_1 <- cbind(as.data.frame(c_jefe[,1]), NOMINDICADOR, BASE_AUX)
c_jefe_1 <- c_jefe_1 %>% dplyr::mutate(SEXO = "VARONES")
c_jefe_2 <- cbind(as.data.frame(c_jefe[,2]), NOMINDICADOR, BASE_AUX)
c_jefe_2 <- c_jefe_2 %>% dplyr::mutate(SEXO = "MUJERES")
colnames(c_jefe_1) <- colnames(BASE_MOTOR)
colnames(c_jefe_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_jefe_1,c_jefe_2)


# Tramo de edad

a_edad <- function(y) {
  base <- subset(ech_h_svy_1, tramo_edad2 == y) 
  resultados  <-  sapply(base$variables %>% select(all_of(var_int)), function(x){
    base %>%
      srvyr::summarise(stat1 = srvyr::survey_mean(x, na.rm = TRUE))
    
  })
  y <- as.numeric(resultados[1,])
}

a_e_edad = matrix(, nrow = 8, ncol = 4)

for(i in 1:4){
  a_e_edad[,i] <- a_edad(y = i)
}

c_edad <- as.data.frame(a_e_edad)

c_edad_1 <- cbind(as.data.frame(c_edad[,1]), NOMINDICADOR, BASE_AUX)
c_edad_1 <- c_edad_1 %>% dplyr::mutate(EDAD = "Menor de 30 años")
c_edad_2 <- cbind(as.data.frame(c_edad[,2]), NOMINDICADOR, BASE_AUX)
c_edad_2 <- c_edad_2 %>% dplyr::mutate(EDAD = "Entre 30 y 44 años")
c_edad_3 <- cbind(as.data.frame(c_edad[,3]), NOMINDICADOR, BASE_AUX)
c_edad_3 <- c_edad_3 %>% dplyr::mutate(EDAD = "Entre 45 y 64 años")
c_edad_4 <- cbind(as.data.frame(c_edad[,4]), NOMINDICADOR, BASE_AUX)
c_edad_4 <- c_edad_4 %>% dplyr::mutate(EDAD = "65 años o más")
colnames(c_edad_1) <- colnames(BASE_MOTOR)
colnames(c_edad_2) <- colnames(BASE_MOTOR)
colnames(c_edad_3) <- colnames(BASE_MOTOR)
colnames(c_edad_4) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_edad_1,c_edad_2, c_edad_3, c_edad_4)



# Ascendencia

a_ascen <- function(y) {
  base <- subset(ech_h_svy_1, bd_e29_1 == y) 
  resultados  <-  sapply(base$variables %>% select(all_of(var_int)), function(x){
    base %>%
      srvyr::summarise(stat1 = srvyr::survey_mean(x, na.rm = TRUE))
    
  })
  y <- as.numeric(resultados[1,])
}

a_e_ascen = matrix(, nrow = 8, ncol = 2)

for(i in 1:2){
  a_e_ascen[,i] <- a_ascen(y = i)
}

c_ascen <- as.data.frame(a_e_ascen)
c_ascen_1 <- cbind(as.data.frame(c_ascen[,1]), NOMINDICADOR, BASE_AUX)
c_ascen_1 <- c_ascen_1 %>% dplyr::mutate(ASCENDENCIA = "AFRO")
c_ascen_2 <- cbind(as.data.frame(c_ascen[,2]), NOMINDICADOR, BASE_AUX)
c_ascen_2 <- c_ascen_2 %>% dplyr::mutate(ASCENDENCIA = "NO AFRO")
colnames(c_ascen_1) <- colnames(BASE_MOTOR)
colnames(c_ascen_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_ascen_1,c_ascen_2)





# Quintil de ingreso del hogar
a_quintil <- function(y) {
  base <- subset(ech_h_svy_1, quintilesy == y) 
  resultados  <-  sapply(base$variables %>% select(all_of(var_int)), function(x){
    base %>%
      srvyr::summarise(stat1 = srvyr::survey_mean(x, na.rm = TRUE))
    
  })
  y <- as.numeric(resultados[1,])
}

a_e_quintil = matrix(, nrow = 8, ncol = 5)

for(i in 1:5){
  a_e_quintil[,i] <- a_quintil(y = i)
}

c_quintil <- as.data.frame(a_e_quintil)

c_quintil_1 <- cbind(as.data.frame(c_quintil[,1]), NOMINDICADOR, BASE_AUX)
c_quintil_1 <- c_quintil_1 %>% dplyr::mutate(QUINTIL = "Quintil 1")
c_quintil_2 <- cbind(as.data.frame(c_quintil[,2]), NOMINDICADOR, BASE_AUX)
c_quintil_2 <- c_quintil_2 %>% dplyr::mutate(QUINTIL = "Quintil 2")
c_quintil_3 <- cbind(as.data.frame(c_quintil[,3]), NOMINDICADOR, BASE_AUX)
c_quintil_3 <- c_quintil_3 %>% dplyr::mutate(QUINTIL = "Quintil 3")
c_quintil_4 <- cbind(as.data.frame(c_quintil[,4]), NOMINDICADOR, BASE_AUX)
c_quintil_4 <- c_quintil_4 %>% dplyr::mutate(QUINTIL = "Quintil 4")
c_quintil_5 <- cbind(as.data.frame(c_quintil[,5]), NOMINDICADOR, BASE_AUX)
c_quintil_5 <- c_quintil_5 %>% dplyr::mutate(QUINTIL = "Quintil 5")
colnames(c_quintil_1) <- colnames(BASE_MOTOR)
colnames(c_quintil_2) <- colnames(BASE_MOTOR)
colnames(c_quintil_3) <- colnames(BASE_MOTOR)
colnames(c_quintil_4) <- colnames(BASE_MOTOR)
colnames(c_quintil_5) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_quintil_1,c_quintil_2, c_quintil_3, c_quintil_4, c_quintil_5)


# Pobreza

a_pobre <- function(y) {
  base <- subset(ech_h_svy_1, pobre_aux == y) 
  resultados  <-  sapply(base$variables %>% select(all_of(var_int)), function(x){
    base %>%
      srvyr::summarise(stat1 = srvyr::survey_mean(x, na.rm = TRUE))
    
  })
  y <- as.numeric(resultados[1,])
}

a_e_pobre = matrix(, nrow = 8, ncol = 2)

for(i in 1:2){
  a_e_pobre[,i] <- a_pobre(y = i)
}

c_pobre <- as.data.frame(a_e_pobre)
c_pobre_1 <- cbind(as.data.frame(c_pobre[,1]), NOMINDICADOR, BASE_AUX)
c_pobre_1 <- c_pobre_1 %>% dplyr::mutate(POBRE = "POBRE")
c_pobre_2 <- cbind(as.data.frame(c_pobre[,2]), NOMINDICADOR, BASE_AUX)
c_pobre_2 <- c_pobre_2 %>% dplyr::mutate(POBRE = "NO POBRE")
colnames(c_pobre_1) <- colnames(BASE_MOTOR)
colnames(c_pobre_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_pobre_1,c_pobre_2)


# Base motor

BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(CODIND = "500")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(CATEGORIA = "Composición y estructura de los hogares")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(PESTAÑA = "Total país")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(PAÍS = "Uruguay")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(ANIO = 2023)
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(RESPONSABLE = "Sharon Katzkowicz")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(DEPARTAMENTOUY = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(NSE = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(NIVELEDU = "Todos")
BASE_MOTOR <- as.data.frame(BASE_MOTOR)


BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(ASCENDENCIA = case_when(ASCENDENCIA == "AFRO" ~ "Afro", 
                                                                   ASCENDENCIA == "NO AFRO" ~ "No afro",
                                                                   ASCENDENCIA == "" ~ "Todos"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(URBANORURALUY = case_when(URBANORURALUY == "URBANO (MENOS DE 5.000 HABITANTES)" ~ "Urbano (menos de 5.000 habitantes)",
                                                                     URBANORURALUY == "RURAL DISPERSO" ~ "Rural disperso", 
                                                                     URBANORURALUY == "" ~ "Total país"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(SEXO = case_when(SEXO == "MUJERES" ~ "Mujeres", 
                                                            SEXO=="VARONES" ~ "Varones",
                                                            SEXO == "" ~ "Todos"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(QUINTIL = case_when(QUINTIL == "Quintil 1"~"Quintil 1", 
                                                               QUINTIL == "Quintil 2"~"Quintil 2",
                                                               QUINTIL == "Quintil 3"~"Quintil 3", 
                                                               QUINTIL == "Quintil 4"~"Quintil 4",
                                                               QUINTIL == "Quintil 5"~"Quintil 5",
                                                               QUINTIL == "" ~ "Todos"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(EDAD = case_when(EDAD == "Menor de 30 años"~"Menor de 30 años", 
                                                            EDAD == "Entre 30 y 44 años"~"Entre 30 y 44 años",
                                                            EDAD == "Entre 45 y 64 años"~"Entre 45 y 64 años", 
                                                            EDAD == "65 años o más"~"65 años o más",
                                                            EDAD == "" ~ "Todos"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(POBRE = case_when(POBRE == "POBRE" ~ "Pobre", 
                                                             POBRE == "NO POBRE" ~ "No pobre",
                                                             POBRE == "" ~ "Todos"))
                                           

m500_tp <- BASE_MOTOR %>% select(CODIND,	
                                  NOMINDICADOR,	
                                  CATEGORIA,	
                                  PESTAÑA,	
                                  SEXO,	
                                  ASCENDENCIA,	
                                  QUINTIL,	
                                  DEPARTAMENTOUY,	
                                  URBANORURALUY,	
                                  EDAD,	
                                  POBRE,
                                  NSE,
                                  NIVELEDU,
                                  PAÍS,	
                                  ANIO,	
                                  VALOR,	
                                  RESPONSABLE)







### 501 Distribución porcentual de hogares con menores según tipo de hogar


## País Urbano

ech_h_svy_1_men <- ech_h_men_svy %>%
  filter(bd_region == 1 & tipo_hogar>0)

# Total

NOMINDICADOR <- 
  c("Distribución porcentual de hogares con menores según tipo de hogar (Unipersonal)",
    "Distribución porcentual de hogares con menores según tipo de hogar (Pareja sin hijos)", 
    "Distribución porcentual de hogares con menores según tipo de hogar (Biparental)",
    "Distribución porcentual de hogares con menores según tipo de hogar (Monoparental femenino)",
    "Distribución porcentual de hogares con menores según tipo de hogar (Monoparental masculino)",
    "Distribución porcentual de hogares con menores según tipo de hogar (Extendido o compuesto)", 
    "Distribución porcentual de hogares con menores según tipo de hogar (Extendido con núcleo monoparental)", 
    "Distribución porcentual de hogares con menores según tipo de hogar (Sin núcleo)" )

var_int <- c("tipo_hogar1", "tipo_hogar2", "tipo_hogar3", "tipo_hogar4", "tipo_hogar5", "tipo_hogar6", "tipo_hogar7", "tipo_hogar8")
a_sem <- sapply(ech_h_svy_1_men$variables %>% select(all_of(var_int)), function(x){
  ech_h_svy_1_men %>%
    srvyr::summarise(stat = srvyr::survey_mean(x, na.rm = TRUE)) 
})

c_ano <- matrix(,8,1)
for (i in 1:8) {
  c_ano[i,1] <- c(as.numeric(c(a_sem[1,i])))
}

colnames(c_ano) <- c("VALOR")
c_ano <- cbind(c_ano, NOMINDICADOR)
c_ano <- as.data.frame(c_ano)
BASE_MOTOR <- cbind(c_ano,BASE_AUX)

# Sexo del jefe

a_jefe <- function(y) {
  base <- subset(ech_h_svy_1_men, sexojefe == y) 
  resultados  <-  sapply(base$variables %>% select(all_of(var_int)), function(x){
    base %>%
      srvyr::summarise(stat1 = srvyr::survey_mean(x, na.rm = TRUE))
    
  })
  y <- as.numeric(resultados[1,])
}

a_e_jefe = matrix(, nrow = 8, ncol = 2)

for(i in 1:2){
  a_e_jefe[,i] <- a_jefe(y = i)
}

c_jefe <- as.data.frame(a_e_jefe)
c_jefe_1 <- cbind(as.data.frame(c_jefe[,1]), NOMINDICADOR, BASE_AUX)
c_jefe_1 <- c_jefe_1 %>% dplyr::mutate(SEXO = "VARONES")
c_jefe_2 <- cbind(as.data.frame(c_jefe[,2]), NOMINDICADOR, BASE_AUX)
c_jefe_2 <- c_jefe_2 %>% dplyr::mutate(SEXO = "MUJERES")
colnames(c_jefe_1) <- colnames(BASE_MOTOR)
colnames(c_jefe_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_jefe_1,c_jefe_2)


# Tramo de edad

a_edad <- function(y) {
  base <- subset(ech_h_svy_1_men, tramo_edad2 == y) 
  resultados  <-  sapply(base$variables %>% select(all_of(var_int)), function(x){
    base %>%
      srvyr::summarise(stat1 = srvyr::survey_mean(x, na.rm = TRUE))
    
  })
  y <- as.numeric(resultados[1,])
}

a_e_edad = matrix(, nrow = 8, ncol = 4)

for(i in 1:4){
  a_e_edad[,i] <- a_edad(y = i)
}


c_edad <- as.data.frame(a_e_edad)
c_edad_1 <- cbind(as.data.frame(c_edad[,1]), NOMINDICADOR, BASE_AUX)
c_edad_1 <- c_edad_1 %>% dplyr::mutate(EDAD = "Menor de 30 años")
c_edad_2 <- cbind(as.data.frame(c_edad[,2]), NOMINDICADOR, BASE_AUX)
c_edad_2 <- c_edad_2 %>% dplyr::mutate(EDAD = "Entre 30 y 44 años")
c_edad_3 <- cbind(as.data.frame(c_edad[,3]), NOMINDICADOR, BASE_AUX)
c_edad_3 <- c_edad_3 %>% dplyr::mutate(EDAD = "Entre 45 y 64 años")
c_edad_4 <- cbind(as.data.frame(c_edad[,4]), NOMINDICADOR, BASE_AUX)
c_edad_4 <- c_edad_4 %>% dplyr::mutate(EDAD = "65 años o más")
colnames(c_edad_1) <- colnames(BASE_MOTOR)
colnames(c_edad_2) <- colnames(BASE_MOTOR)
colnames(c_edad_3) <- colnames(BASE_MOTOR)
colnames(c_edad_4) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_edad_1,c_edad_2, c_edad_3, c_edad_4)


# Quintil de ingreso del hogar
a_quintil <- function(y) {
  base <- subset(ech_h_svy_1_men, quintilesy == y) 
  resultados  <-  sapply(base$variables %>% select(all_of(var_int)), function(x){
    base %>%
      srvyr::summarise(stat1 = srvyr::survey_mean(x, na.rm = TRUE))
    
  })
  y <- as.numeric(resultados[1,])
}

a_e_quintil = matrix(, nrow = 8, ncol = 5)

for(i in 1:5){
  a_e_quintil[,i] <- a_quintil(y = i)
}

c_quintil <- as.data.frame(a_e_quintil)

c_quintil_1 <- cbind(as.data.frame(c_quintil[,1]), NOMINDICADOR, BASE_AUX)
c_quintil_1 <- c_quintil_1 %>% dplyr::mutate(QUINTIL = "Quintil 1")
c_quintil_2 <- cbind(as.data.frame(c_quintil[,2]), NOMINDICADOR, BASE_AUX)
c_quintil_2 <- c_quintil_2 %>% dplyr::mutate(QUINTIL = "Quintil 2")
c_quintil_3 <- cbind(as.data.frame(c_quintil[,3]), NOMINDICADOR, BASE_AUX)
c_quintil_3 <- c_quintil_3 %>% dplyr::mutate(QUINTIL = "Quintil 3")
c_quintil_4 <- cbind(as.data.frame(c_quintil[,4]), NOMINDICADOR, BASE_AUX)
c_quintil_4 <- c_quintil_4 %>% dplyr::mutate(QUINTIL = "Quintil 4")
c_quintil_5 <- cbind(as.data.frame(c_quintil[,5]), NOMINDICADOR, BASE_AUX)
c_quintil_5 <- c_quintil_5 %>% dplyr::mutate(QUINTIL = "Quintil 5")
colnames(c_quintil_1) <- colnames(BASE_MOTOR)
colnames(c_quintil_2) <- colnames(BASE_MOTOR)
colnames(c_quintil_3) <- colnames(BASE_MOTOR)
colnames(c_quintil_4) <- colnames(BASE_MOTOR)
colnames(c_quintil_5) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_quintil_1,c_quintil_2, c_quintil_3, c_quintil_4, c_quintil_5)




# Base motor

BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(CODIND = "501")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(CATEGORIA = "Composición y estructura de los hogares")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(PESTAÑA = "País urbano")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(PAÍS = "Uruguay")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(ANIO = 2023)
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(RESPONSABLE = "Sharon Katzkowicz")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(ASCENDENCIA = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(DEPARTAMENTOUY = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(URBANORURALUY = "Urbano (más de 5.000 habitantes)")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(POBRE = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(NSE = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(NIVELEDU = "Todos")

BASE_MOTOR <- as.data.frame(BASE_MOTOR)

BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(SEXO = case_when(SEXO == "" ~ "Todos", SEXO == "MUJERES"~"Mujeres", SEXO=="VARONES"~"Varones"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(QUINTIL = case_when(QUINTIL == "" ~ "Todos",
                                                               QUINTIL == "Quintil 1"~"Quintil 1", 
                                                               QUINTIL == "Quintil 2"~"Quintil 2",
                                                               QUINTIL == "Quintil 3"~"Quintil 3", 
                                                               QUINTIL == "Quintil 4"~"Quintil 4",
                                                               QUINTIL == "Quintil 5"~"Quintil 5"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(EDAD = case_when(EDAD == "" ~ "Todos",
                                                            EDAD == "Menor de 30 años"~"Menor de 30 años", 
                                                            EDAD == "Entre 30 y 44 años"~"Entre 30 y 44 años",
                                                            EDAD == "Entre 45 y 64 años"~"Entre 45 y 64 años", 
                                                            EDAD == "65 años o más"~"65 años o más"))


m501_pu <- BASE_MOTOR %>% select(CODIND,	
                                 NOMINDICADOR,	
                                 CATEGORIA,	
                                 PESTAÑA,	
                                 SEXO,	
                                 ASCENDENCIA,	
                                 QUINTIL,	
                                 DEPARTAMENTOUY,	
                                 URBANORURALUY,	
                                 EDAD,	
                                 POBRE,
                                 NSE,
                                 NIVELEDU,
                                 PAÍS,	
                                 ANIO,	
                                 VALOR,	
                                 RESPONSABLE)	


## Total país

ech_h_men_svy_1 <- ech_h_men_svy %>%
  filter(tipo_hogar>0)

# Total

NOMINDICADOR <- 
  c("Distribución porcentual de hogares con menores según tipo de hogar (Unipersonal)",
    "Distribución porcentual de hogares con menores según tipo de hogar (Pareja sin hijos)", 
    "Distribución porcentual de hogares con menores según tipo de hogar (Biparental)",
    "Distribución porcentual de hogares con menores según tipo de hogar (Monoparental femenino)",
    "Distribución porcentual de hogares con menores según tipo de hogar (Monoparental masculino)",
    "Distribución porcentual de hogares con menores según tipo de hogar (Extendido o compuesto)", 
    "Distribución porcentual de hogares con menores según tipo de hogar (Extendido con núcleo monoparental)", 
    "Distribución porcentual de hogares con menores según tipo de hogar (Sin núcleo)" )

var_int <- c("tipo_hogar1", "tipo_hogar2", "tipo_hogar3", "tipo_hogar4", "tipo_hogar5", "tipo_hogar6", "tipo_hogar7", "tipo_hogar8")
a_sem <- sapply(ech_h_men_svy_1$variables %>% select(all_of(var_int)), function(x){
  ech_h_men_svy_1 %>%
    srvyr::summarise(stat = srvyr::survey_mean(x, na.rm = TRUE)) 
})

c_ano <- matrix(,8,1)
for (i in 1:8) {
  c_ano[i,1] <- c(as.numeric(c(a_sem[1,i])))
}

colnames(c_ano) <- c("VALOR")
c_ano <- cbind(c_ano, NOMINDICADOR)
c_ano <- as.data.frame(c_ano)
BASE_MOTOR <- cbind(c_ano,BASE_AUX)


# Región

a_reg <- function(y) {
  base <- subset(ech_h_men_svy_1, bd_region == y) 
  resultados  <-  sapply(base$variables %>% select(all_of(var_int)), function(x){
    base %>%
      srvyr::summarise(stat1 = srvyr::survey_mean(x, na.rm = TRUE))
    
  })
  y <- as.numeric(resultados[1,])
}

a_e_reg = matrix(, nrow = 8, ncol = 3)

for(i in 1:3){
  a_e_reg[,i] <- a_reg(y = i)
}

c_reg <- as.data.frame(a_e_reg[,2:3])
c_reg_1 <- cbind(as.data.frame(c_reg[,1]), NOMINDICADOR, BASE_AUX)
c_reg_1 <- c_reg_1 %>% dplyr::mutate(URBANORURALUY = "URBANO (MENOS DE 5.000 HABITANTES)")
c_reg_2 <- cbind(as.data.frame(c_reg[,2]), NOMINDICADOR, BASE_AUX)
c_reg_2 <- c_reg_2 %>% dplyr::mutate(URBANORURALUY = "RURAL DISPERSO")
colnames(c_reg_1) <- colnames(BASE_MOTOR)
colnames(c_reg_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_reg_1,c_reg_2)


# Sexo del jefe

a_jefe <- function(y) {
  base <- subset(ech_h_men_svy_1, sexojefe == y) 
  resultados  <-  sapply(base$variables %>% select(all_of(var_int)), function(x){
    base %>%
      srvyr::summarise(stat1 = srvyr::survey_mean(x, na.rm = TRUE))
    
  })
  y <- as.numeric(resultados[1,])
}

a_e_jefe = matrix(, nrow = 8, ncol = 2)

for(i in 1:2){
  a_e_jefe[,i] <- a_jefe(y = i)
}

c_jefe <- as.data.frame(a_e_jefe)
c_jefe_1 <- cbind(as.data.frame(c_jefe[,1]), NOMINDICADOR, BASE_AUX)
c_jefe_1 <- c_jefe_1 %>% dplyr::mutate(SEXO = "VARONES")
c_jefe_2 <- cbind(as.data.frame(c_jefe[,2]), NOMINDICADOR, BASE_AUX)
c_jefe_2 <- c_jefe_2 %>% dplyr::mutate(SEXO = "MUJERES")
colnames(c_jefe_1) <- colnames(BASE_MOTOR)
colnames(c_jefe_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_jefe_1,c_jefe_2)


# Tramo de edad

a_edad <- function(y) {
  base <- subset(ech_h_men_svy_1, tramo_edad2 == y) 
  resultados  <-  sapply(base$variables %>% select(all_of(var_int)), function(x){
    base %>%
      srvyr::summarise(stat1 = srvyr::survey_mean(x, na.rm = TRUE))
    
  })
  y <- as.numeric(resultados[1,])
}

a_e_edad = matrix(, nrow = 8, ncol = 4)

for(i in 1:4){
  a_e_edad[,i] <- a_edad(y = i)
}

c_edad <- as.data.frame(a_e_edad)

c_edad_1 <- cbind(as.data.frame(c_edad[,1]), NOMINDICADOR, BASE_AUX)
c_edad_1 <- c_edad_1 %>% dplyr::mutate(EDAD = "Menor de 30 años")
c_edad_2 <- cbind(as.data.frame(c_edad[,2]), NOMINDICADOR, BASE_AUX)
c_edad_2 <- c_edad_2 %>% dplyr::mutate(EDAD = "Entre 30 y 44 años")
c_edad_3 <- cbind(as.data.frame(c_edad[,3]), NOMINDICADOR, BASE_AUX)
c_edad_3 <- c_edad_3 %>% dplyr::mutate(EDAD = "Entre 45 y 64 años")
c_edad_4 <- cbind(as.data.frame(c_edad[,4]), NOMINDICADOR, BASE_AUX)
c_edad_4 <- c_edad_4 %>% dplyr::mutate(EDAD = "65 años o más")
colnames(c_edad_1) <- colnames(BASE_MOTOR)
colnames(c_edad_2) <- colnames(BASE_MOTOR)
colnames(c_edad_3) <- colnames(BASE_MOTOR)
colnames(c_edad_4) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_edad_1,c_edad_2, c_edad_3, c_edad_4)



# Ascendencia

a_ascen <- function(y) {
  base <- subset(ech_h_men_svy_1, bd_e29_1 == y) 
  resultados  <-  sapply(base$variables %>% select(all_of(var_int)), function(x){
    base %>%
      srvyr::summarise(stat1 = srvyr::survey_mean(x, na.rm = TRUE))
    
  })
  y <- as.numeric(resultados[1,])
}

a_e_ascen = matrix(, nrow = 8, ncol = 2)

for(i in 1:2){
  a_e_ascen[,i] <- a_ascen(y = i)
}

c_ascen <- as.data.frame(a_e_ascen)
c_ascen_1 <- cbind(as.data.frame(c_ascen[,1]), NOMINDICADOR, BASE_AUX)
c_ascen_1 <- c_ascen_1 %>% dplyr::mutate(ASCENDENCIA = "AFRO")
c_ascen_2 <- cbind(as.data.frame(c_ascen[,2]), NOMINDICADOR, BASE_AUX)
c_ascen_2 <- c_ascen_2 %>% dplyr::mutate(ASCENDENCIA = "NO AFRO")
colnames(c_ascen_1) <- colnames(BASE_MOTOR)
colnames(c_ascen_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_ascen_1,c_ascen_2)





# Quintil de ingreso del hogar
a_quintil <- function(y) {
  base <- subset(ech_h_men_svy_1, quintilesy == y) 
  resultados  <-  sapply(base$variables %>% select(all_of(var_int)), function(x){
    base %>%
      srvyr::summarise(stat1 = srvyr::survey_mean(x, na.rm = TRUE))
    
  })
  y <- as.numeric(resultados[1,])
}

a_e_quintil = matrix(, nrow = 8, ncol = 5)

for(i in 1:5){
  a_e_quintil[,i] <- a_quintil(y = i)
}

c_quintil <- as.data.frame(a_e_quintil)

c_quintil_1 <- cbind(as.data.frame(c_quintil[,1]), NOMINDICADOR, BASE_AUX)
c_quintil_1 <- c_quintil_1 %>% dplyr::mutate(QUINTIL = "Quintil 1")
c_quintil_2 <- cbind(as.data.frame(c_quintil[,2]), NOMINDICADOR, BASE_AUX)
c_quintil_2 <- c_quintil_2 %>% dplyr::mutate(QUINTIL = "Quintil 2")
c_quintil_3 <- cbind(as.data.frame(c_quintil[,3]), NOMINDICADOR, BASE_AUX)
c_quintil_3 <- c_quintil_3 %>% dplyr::mutate(QUINTIL = "Quintil 3")
c_quintil_4 <- cbind(as.data.frame(c_quintil[,4]), NOMINDICADOR, BASE_AUX)
c_quintil_4 <- c_quintil_4 %>% dplyr::mutate(QUINTIL = "Quintil 4")
c_quintil_5 <- cbind(as.data.frame(c_quintil[,5]), NOMINDICADOR, BASE_AUX)
c_quintil_5 <- c_quintil_5 %>% dplyr::mutate(QUINTIL = "Quintil 5")
colnames(c_quintil_1) <- colnames(BASE_MOTOR)
colnames(c_quintil_2) <- colnames(BASE_MOTOR)
colnames(c_quintil_3) <- colnames(BASE_MOTOR)
colnames(c_quintil_4) <- colnames(BASE_MOTOR)
colnames(c_quintil_5) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_quintil_1,c_quintil_2, c_quintil_3, c_quintil_4, c_quintil_5)


# Pobreza

a_pobre <- function(y) {
  base <- subset(ech_h_men_svy_1, pobre_aux == y) 
  resultados  <-  sapply(base$variables %>% select(all_of(var_int)), function(x){
    base %>%
      srvyr::summarise(stat1 = srvyr::survey_mean(x, na.rm = TRUE))
    
  })
  y <- as.numeric(resultados[1,])
}

a_e_pobre = matrix(, nrow = 8, ncol = 2)

for(i in 1:2){
  a_e_pobre[,i] <- a_pobre(y = i)
}

c_pobre <- as.data.frame(a_e_pobre)
c_pobre_1 <- cbind(as.data.frame(c_pobre[,1]), NOMINDICADOR, BASE_AUX)
c_pobre_1 <- c_pobre_1 %>% dplyr::mutate(POBRE = "POBRE")
c_pobre_2 <- cbind(as.data.frame(c_pobre[,2]), NOMINDICADOR, BASE_AUX)
c_pobre_2 <- c_pobre_2 %>% dplyr::mutate(POBRE = "NO POBRE")
colnames(c_pobre_1) <- colnames(BASE_MOTOR)
colnames(c_pobre_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_pobre_1,c_pobre_2)



# Base motor

BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(CODIND = "501")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(CATEGORIA = "Composición y estructura de los hogares")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(PESTAÑA = "Total país")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(PAÍS = "Uruguay")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(ANIO = 2023)
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(RESPONSABLE = "Sharon Katzkowicz")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(DEPARTAMENTOUY = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(NSE = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(NIVELEDU = "Todos")

BASE_MOTOR <- as.data.frame(BASE_MOTOR)


BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(ASCENDENCIA = case_when(ASCENDENCIA == "AFRO" ~ "Afro", 
                                                                   ASCENDENCIA == "NO AFRO" ~ "No afro",
                                                                   ASCENDENCIA == "" ~ "Todos"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(URBANORURALUY = case_when(URBANORURALUY == "URBANO (MENOS DE 5.000 HABITANTES)" ~ "Urbano (menos de 5.000 habitantes)",
                                                                     URBANORURALUY == "RURAL DISPERSO" ~ "Rural disperso", 
                                                                     URBANORURALUY == "" ~ "Total país"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(SEXO = case_when(SEXO == "MUJERES" ~ "Mujeres", 
                                                            SEXO=="VARONES" ~ "Varones",
                                                            SEXO == "" ~ "Todos"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(QUINTIL = case_when(QUINTIL == "Quintil 1"~"Quintil 1", 
                                                               QUINTIL == "Quintil 2"~"Quintil 2",
                                                               QUINTIL == "Quintil 3"~"Quintil 3", 
                                                               QUINTIL == "Quintil 4"~"Quintil 4",
                                                               QUINTIL == "Quintil 5"~"Quintil 5",
                                                               QUINTIL == "" ~ "Todos"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(EDAD = case_when(EDAD == "Menor de 30 años"~"Menor de 30 años", 
                                                            EDAD == "Entre 30 y 44 años"~"Entre 30 y 44 años",
                                                            EDAD == "Entre 45 y 64 años"~"Entre 45 y 64 años", 
                                                            EDAD == "65 años o más"~"65 años o más",
                                                            EDAD == "" ~ "Todos"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(POBRE = case_when(POBRE == "POBRE" ~ "Pobre", 
                                                             POBRE == "NO POBRE" ~ "No pobre",
                                                             POBRE == "" ~ "Todos"))


m501_tp <- BASE_MOTOR %>% select(CODIND,	
                                 NOMINDICADOR,	
                                 CATEGORIA,	
                                 PESTAÑA,	
                                 SEXO,	
                                 ASCENDENCIA,	
                                 QUINTIL,	
                                 DEPARTAMENTOUY,	
                                 URBANORURALUY,	
                                 EDAD,	
                                 POBRE, 
                                 NSE,
                                 NIVELEDU,
                                 PAÍS,	
                                 ANIO,	
                                 VALOR,	
                                 RESPONSABLE)






### 502 Distribución porcentual de las personas según tipo de hogar en el que residen


## País Urbano

ech_svy_1 <- ech_svy %>%
  filter(bd_region == 1 & tipo_hogar>0)

# Total

NOMINDICADOR <- 
  c("Distribución porcentual de las personas según tipo de hogar en el que residen (Unipersonal)",
    "Distribución porcentual de las personas según tipo de hogar en el que residen (Pareja sin hijos)", 
    "Distribución porcentual de las personas según tipo de hogar en el que residen (Biparental)",
    "Distribución porcentual de las personas según tipo de hogar en el que residen (Monoparental femenino)",
    "Distribución porcentual de las personas según tipo de hogar en el que residen (Monoparental masculino)",
    "Distribución porcentual de las personas según tipo de hogar en el que residen (Extendido o compuesto)", 
    "Distribución porcentual de las personas según tipo de hogar en el que residen (Extendido con núcleo monoparental)", 
    "Distribución porcentual de las personas según tipo de hogar en el que residen (Sin núcleo)" )

var_int <- c("tipo_hogar1", "tipo_hogar2", "tipo_hogar3", "tipo_hogar4", "tipo_hogar5", "tipo_hogar6", "tipo_hogar7", "tipo_hogar8")
a_sem <- sapply(ech_svy_1$variables %>% select(all_of(var_int)), function(x){
  ech_svy_1 %>%
    srvyr::summarise(stat = srvyr::survey_mean(x, na.rm = TRUE)) 
})

c_ano <- matrix(,8,1)
for (i in 1:8) {
  c_ano[i,1] <- c(as.numeric(c(a_sem[1,i])))
}

colnames(c_ano) <- c("VALOR")
c_ano <- cbind(c_ano, NOMINDICADOR)
c_ano <- as.data.frame(c_ano)
BASE_MOTOR <- cbind(c_ano,BASE_AUX)

# Sexo


a_sexo <- function(y) {
  base <- subset(ech_svy_1,  bc_pe2 == y) 
  resultados  <-  sapply(base$variables %>% select(all_of(var_int)), function(x){
    base %>%
      srvyr::summarise(stat1 = srvyr::survey_mean(x, na.rm = TRUE))
    
  })
  y <- as.numeric(resultados[1,])
}

a_e_sexo = matrix(, nrow = 8, ncol = 2)

for(i in 1:2){
  a_e_sexo[,i] <- a_sexo(y = i)
}

c_sexo <- as.data.frame(a_e_sexo)
c_sexo_1 <- cbind(as.data.frame(c_sexo[,1]), NOMINDICADOR, BASE_AUX)
c_sexo_1 <- c_sexo_1 %>% dplyr::mutate(SEXO = "VARONES")
c_sexo_2 <- cbind(as.data.frame(c_sexo[,2]), NOMINDICADOR, BASE_AUX)
c_sexo_2 <- c_sexo_2 %>% dplyr::mutate(SEXO = "MUJERES")
colnames(c_sexo_1) <- colnames(BASE_MOTOR)
colnames(c_sexo_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_sexo_1,c_sexo_2)


# Tramo de edad

a_edad <- function(y) {
  base <- subset(ech_svy_1, tramo_edad2 == y) 
  resultados  <-  sapply(base$variables %>% select(all_of(var_int)), function(x){
    base %>%
      srvyr::summarise(stat1 = srvyr::survey_mean(x, na.rm = TRUE))
    
  })
  y <- as.numeric(resultados[1,])
}

a_e_edad = matrix(, nrow = 8, ncol = 4)

for(i in 1:4){
  a_e_edad[,i] <- a_edad(y = i)
}

c_edad <- as.data.frame(a_e_edad)

c_edad_1 <- cbind(as.data.frame(c_edad[,1]), NOMINDICADOR, BASE_AUX)
c_edad_1 <- c_edad_1 %>% dplyr::mutate(EDAD = "Menor de 30 años")
c_edad_2 <- cbind(as.data.frame(c_edad[,2]), NOMINDICADOR, BASE_AUX)
c_edad_2 <- c_edad_2 %>% dplyr::mutate(EDAD = "Entre 30 y 44 años")
c_edad_3 <- cbind(as.data.frame(c_edad[,3]), NOMINDICADOR, BASE_AUX)
c_edad_3 <- c_edad_3 %>% dplyr::mutate(EDAD = "Entre 45 y 64 años")
c_edad_4 <- cbind(as.data.frame(c_edad[,4]), NOMINDICADOR, BASE_AUX)
c_edad_4 <- c_edad_4 %>% dplyr::mutate(EDAD = "65 años o más")
colnames(c_edad_1) <- colnames(BASE_MOTOR)
colnames(c_edad_2) <- colnames(BASE_MOTOR)
colnames(c_edad_3) <- colnames(BASE_MOTOR)
colnames(c_edad_4) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_edad_1,c_edad_2, c_edad_3, c_edad_4)


# Quintil de ingreso del hogar
a_quintil <- function(y) {
  base <- subset(ech_svy_1, quintilesy == y) 
  resultados  <-  sapply(base$variables %>% select(all_of(var_int)), function(x){
    base %>%
      srvyr::summarise(stat1 = srvyr::survey_mean(x, na.rm = TRUE))
    
  })
  y <- as.numeric(resultados[1,])
}

a_e_quintil = matrix(, nrow = 8, ncol = 5)

for(i in 1:5){
  a_e_quintil[,i] <- a_quintil(y = i)
}

c_quintil <- as.data.frame(a_e_quintil)

c_quintil_1 <- cbind(as.data.frame(c_quintil[,1]), NOMINDICADOR, BASE_AUX)
c_quintil_1 <- c_quintil_1 %>% dplyr::mutate(QUINTIL = "Quintil 1")
c_quintil_2 <- cbind(as.data.frame(c_quintil[,2]), NOMINDICADOR, BASE_AUX)
c_quintil_2 <- c_quintil_2 %>% dplyr::mutate(QUINTIL = "Quintil 2")
c_quintil_3 <- cbind(as.data.frame(c_quintil[,3]), NOMINDICADOR, BASE_AUX)
c_quintil_3 <- c_quintil_3 %>% dplyr::mutate(QUINTIL = "Quintil 3")
c_quintil_4 <- cbind(as.data.frame(c_quintil[,4]), NOMINDICADOR, BASE_AUX)
c_quintil_4 <- c_quintil_4 %>% dplyr::mutate(QUINTIL = "Quintil 4")
c_quintil_5 <- cbind(as.data.frame(c_quintil[,5]), NOMINDICADOR, BASE_AUX)
c_quintil_5 <- c_quintil_5 %>% dplyr::mutate(QUINTIL = "Quintil 5")
colnames(c_quintil_1) <- colnames(BASE_MOTOR)
colnames(c_quintil_2) <- colnames(BASE_MOTOR)
colnames(c_quintil_3) <- colnames(BASE_MOTOR)
colnames(c_quintil_4) <- colnames(BASE_MOTOR)
colnames(c_quintil_5) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_quintil_1,c_quintil_2, c_quintil_3, c_quintil_4, c_quintil_5)




# Base motor

BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(CODIND = "502")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(CATEGORIA = "Composición y estructura de los hogares")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(PESTAÑA = "País urbano")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(PAÍS = "Uruguay")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(ANIO = 2023)
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(RESPONSABLE = "Sharon Katzkowicz")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(ASCENDENCIA = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(DEPARTAMENTOUY = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(URBANORURALUY = "Urbano (más de 5.000 habitantes)")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(POBRE = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(NSE = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(NIVELEDU = "Todos")

BASE_MOTOR <- as.data.frame(BASE_MOTOR)

BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(SEXO = case_when(SEXO == "" ~ "Todos", SEXO == "MUJERES"~"Mujeres", SEXO=="VARONES"~"Varones"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(QUINTIL = case_when(QUINTIL == "" ~ "Todos",
                                                               QUINTIL == "Quintil 1"~"Quintil 1", 
                                                               QUINTIL == "Quintil 2"~"Quintil 2",
                                                               QUINTIL == "Quintil 3"~"Quintil 3", 
                                                               QUINTIL == "Quintil 4"~"Quintil 4",
                                                               QUINTIL == "Quintil 5"~"Quintil 5"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(EDAD = case_when(EDAD == "" ~ "Todos",
                                                            EDAD == "Menor de 30 años"~"Menor de 30 años", 
                                                            EDAD == "Entre 30 y 44 años"~"Entre 30 y 44 años",
                                                            EDAD == "Entre 45 y 64 años"~"Entre 45 y 64 años", 
                                                            EDAD == "65 años o más"~"65 años o más"))


m502_pu <- BASE_MOTOR %>% select(CODIND,	
                                 NOMINDICADOR,	
                                 CATEGORIA,	
                                 PESTAÑA,	
                                 SEXO,	
                                 ASCENDENCIA,	
                                 QUINTIL,	
                                 DEPARTAMENTOUY,	
                                 URBANORURALUY,	
                                 EDAD,	
                                 POBRE,
                                 NSE,
                                 NIVELEDU,
                                 PAÍS,	
                                 ANIO,	
                                 VALOR,	
                                 RESPONSABLE)	


## Total país
ech_svy_1 <- ech_svy %>%
  filter(tipo_hogar>0)

# Total

NOMINDICADOR <- 
  c("Distribución porcentual de las personas según tipo de hogar en el que residen (Unipersonal)",
    "Distribución porcentual de las personas según tipo de hogar en el que residen (Pareja sin hijos)", 
    "Distribución porcentual de las personas según tipo de hogar en el que residen (Biparental)",
    "Distribución porcentual de las personas según tipo de hogar en el que residen (Monoparental femenino)",
    "Distribución porcentual de las personas según tipo de hogar en el que residen (Monoparental masculino)",
    "Distribución porcentual de las personas según tipo de hogar en el que residen (Extendido o compuesto)", 
    "Distribución porcentual de las personas según tipo de hogar en el que residen (Extendido con núcleo monoparental)", 
    "Distribución porcentual de las personas según tipo de hogar en el que residen (Sin núcleo)" )

var_int <- c("tipo_hogar1", "tipo_hogar2", "tipo_hogar3", "tipo_hogar4", "tipo_hogar5", "tipo_hogar6", "tipo_hogar7", "tipo_hogar8")
a_sem <- sapply(ech_svy_1$variables %>% select(all_of(var_int)), function(x){
  ech_svy_1 %>%
    srvyr::summarise(stat = srvyr::survey_mean(x, na.rm = TRUE)) 
})

c_ano <- matrix(,8,1)
for (i in 1:8) {
  c_ano[i,1] <- c(as.numeric(c(a_sem[1,i])))
}

colnames(c_ano) <- c("VALOR")
c_ano <- cbind(c_ano, NOMINDICADOR)
c_ano <- as.data.frame(c_ano)
BASE_MOTOR <- cbind(c_ano,BASE_AUX)


# Región

a_reg <- function(y) {
  base <- subset(ech_svy_1, bd_region == y) 
  resultados  <-  sapply(base$variables %>% select(all_of(var_int)), function(x){
    base %>%
      srvyr::summarise(stat1 = srvyr::survey_mean(x, na.rm = TRUE))
    
  })
  y <- as.numeric(resultados[1,])
}

a_e_reg = matrix(, nrow = 8, ncol = 3)

for(i in 1:3){
  a_e_reg[,i] <- a_reg(y = i)
}

c_reg <- as.data.frame(a_e_reg[,2:3])
c_reg_1 <- cbind(as.data.frame(c_reg[,1]), NOMINDICADOR, BASE_AUX)
c_reg_1 <- c_reg_1 %>% dplyr::mutate(URBANORURALUY = "URBANO (MENOS DE 5.000 HABITANTES)")
c_reg_2 <- cbind(as.data.frame(c_reg[,2]), NOMINDICADOR, BASE_AUX)
c_reg_2 <- c_reg_2 %>% dplyr::mutate(URBANORURALUY = "RURAL DISPERSO")
colnames(c_reg_1) <- colnames(BASE_MOTOR)
colnames(c_reg_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_reg_1,c_reg_2)


# Sexo

a_sexo <- function(y) {
  base <- subset(ech_svy_1,  bc_pe2 == y) 
  resultados  <-  sapply(base$variables %>% select(all_of(var_int)), function(x){
    base %>%
      srvyr::summarise(stat1 = srvyr::survey_mean(x, na.rm = TRUE))
    
  })
  y <- as.numeric(resultados[1,])
}

a_e_sexo = matrix(, nrow = 8, ncol = 2)

for(i in 1:2){
  a_e_sexo[,i] <- a_sexo(y = i)
}

c_sexo <- as.data.frame(a_e_sexo)
c_sexo_1 <- cbind(as.data.frame(c_sexo[,1]), NOMINDICADOR, BASE_AUX)
c_sexo_1 <- c_sexo_1 %>% dplyr::mutate(SEXO = "VARONES")
c_sexo_2 <- cbind(as.data.frame(c_sexo[,2]), NOMINDICADOR, BASE_AUX)
c_sexo_2 <- c_sexo_2 %>% dplyr::mutate(SEXO = "MUJERES")
colnames(c_sexo_1) <- colnames(BASE_MOTOR)
colnames(c_sexo_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_sexo_1,c_sexo_2)


# Tramo de edad

a_edad <- function(y) {
  base <- subset(ech_svy_1, tramo_edad2 == y) 
  resultados  <-  sapply(base$variables %>% select(all_of(var_int)), function(x){
    base %>%
      srvyr::summarise(stat1 = srvyr::survey_mean(x, na.rm = TRUE))
    
  })
  y <- as.numeric(resultados[1,])
}

a_e_edad = matrix(, nrow = 8, ncol = 4)

for(i in 1:4){
  a_e_edad[,i] <- a_edad(y = i)
}


c_edad <- as.data.frame(a_e_edad)
c_edad_1 <- cbind(as.data.frame(c_edad[,1]), NOMINDICADOR, BASE_AUX)
c_edad_1 <- c_edad_1 %>% dplyr::mutate(EDAD = "Menor de 30 años")
c_edad_2 <- cbind(as.data.frame(c_edad[,2]), NOMINDICADOR, BASE_AUX)
c_edad_2 <- c_edad_2 %>% dplyr::mutate(EDAD = "Entre 30 y 44 años")
c_edad_3 <- cbind(as.data.frame(c_edad[,3]), NOMINDICADOR, BASE_AUX)
c_edad_3 <- c_edad_3 %>% dplyr::mutate(EDAD = "Entre 45 y 64 años")
c_edad_4 <- cbind(as.data.frame(c_edad[,4]), NOMINDICADOR, BASE_AUX)
c_edad_4 <- c_edad_4 %>% dplyr::mutate(EDAD = "65 años o más")
colnames(c_edad_1) <- colnames(BASE_MOTOR)
colnames(c_edad_2) <- colnames(BASE_MOTOR)
colnames(c_edad_3) <- colnames(BASE_MOTOR)
colnames(c_edad_4) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_edad_1,c_edad_2, c_edad_3, c_edad_4)



# Ascendencia

a_ascen <- function(y) {
  base <- subset(ech_svy_1, bd_e29_1 == y) 
  resultados  <-  sapply(base$variables %>% select(all_of(var_int)), function(x){
    base %>%
      srvyr::summarise(stat1 = srvyr::survey_mean(x, na.rm = TRUE))
    
  })
  y <- as.numeric(resultados[1,])
}

a_e_ascen = matrix(, nrow = 8, ncol = 2)

for(i in 1:2){
  a_e_ascen[,i] <- a_ascen(y = i)
}

c_ascen <- as.data.frame(a_e_ascen)
c_ascen_1 <- cbind(as.data.frame(c_ascen[,1]), NOMINDICADOR, BASE_AUX)
c_ascen_1 <- c_ascen_1 %>% dplyr::mutate(ASCENDENCIA = "AFRO")
c_ascen_2 <- cbind(as.data.frame(c_ascen[,2]), NOMINDICADOR, BASE_AUX)
c_ascen_2 <- c_ascen_2 %>% dplyr::mutate(ASCENDENCIA = "NO AFRO")
colnames(c_ascen_1) <- colnames(BASE_MOTOR)
colnames(c_ascen_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_ascen_1,c_ascen_2)





# Quintil de ingreso del hogar
a_quintil <- function(y) {
  base <- subset(ech_svy_1, quintilesy == y) 
  resultados  <-  sapply(base$variables %>% select(all_of(var_int)), function(x){
    base %>%
      srvyr::summarise(stat1 = srvyr::survey_mean(x, na.rm = TRUE))
    
  })
  y <- as.numeric(resultados[1,])
}

a_e_quintil = matrix(, nrow = 8, ncol = 5)

for(i in 1:5){
  a_e_quintil[,i] <- a_quintil(y = i)
}

c_quintil <- as.data.frame(a_e_quintil)

c_quintil_1 <- cbind(as.data.frame(c_quintil[,1]), NOMINDICADOR, BASE_AUX)
c_quintil_1 <- c_quintil_1 %>% dplyr::mutate(QUINTIL = "Quintil 1")
c_quintil_2 <- cbind(as.data.frame(c_quintil[,2]), NOMINDICADOR, BASE_AUX)
c_quintil_2 <- c_quintil_2 %>% dplyr::mutate(QUINTIL = "Quintil 2")
c_quintil_3 <- cbind(as.data.frame(c_quintil[,3]), NOMINDICADOR, BASE_AUX)
c_quintil_3 <- c_quintil_3 %>% dplyr::mutate(QUINTIL = "Quintil 3")
c_quintil_4 <- cbind(as.data.frame(c_quintil[,4]), NOMINDICADOR, BASE_AUX)
c_quintil_4 <- c_quintil_4 %>% dplyr::mutate(QUINTIL = "Quintil 4")
c_quintil_5 <- cbind(as.data.frame(c_quintil[,5]), NOMINDICADOR, BASE_AUX)
c_quintil_5 <- c_quintil_5 %>% dplyr::mutate(QUINTIL = "Quintil 5")
colnames(c_quintil_1) <- colnames(BASE_MOTOR)
colnames(c_quintil_2) <- colnames(BASE_MOTOR)
colnames(c_quintil_3) <- colnames(BASE_MOTOR)
colnames(c_quintil_4) <- colnames(BASE_MOTOR)
colnames(c_quintil_5) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_quintil_1,c_quintil_2, c_quintil_3, c_quintil_4, c_quintil_5)


# Pobreza

a_pobre <- function(y) {
  base <- subset(ech_svy_1, pobre_aux == y) 
  resultados  <-  sapply(base$variables %>% select(all_of(var_int)), function(x){
    base %>%
      srvyr::summarise(stat1 = srvyr::survey_mean(x, na.rm = TRUE))
    
  })
  y <- as.numeric(resultados[1,])
}

a_e_pobre = matrix(, nrow = 8, ncol = 2)

for(i in 1:2){
  a_e_pobre[,i] <- a_pobre(y = i)
}


c_pobre <- as.data.frame(a_e_pobre)
c_pobre_1 <- cbind(as.data.frame(c_pobre[,1]), NOMINDICADOR, BASE_AUX)
c_pobre_1 <- c_pobre_1 %>% dplyr::mutate(POBRE = "POBRE")
c_pobre_2 <- cbind(as.data.frame(c_pobre[,2]), NOMINDICADOR, BASE_AUX)
c_pobre_2 <- c_pobre_2 %>% dplyr::mutate(POBRE = "NO POBRE")
colnames(c_pobre_1) <- colnames(BASE_MOTOR)
colnames(c_pobre_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_pobre_1,c_pobre_2)


# Base motor

BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(CODIND = "502")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(CATEGORIA = "Composición y estructura de los hogares")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(PESTAÑA = "Total país")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(PAÍS = "Uruguay")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(ANIO = 2023)
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(RESPONSABLE = "Sharon Katzkowicz")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(DEPARTAMENTOUY = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(NSE = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(NIVELEDU = "Todos")

BASE_MOTOR <- as.data.frame(BASE_MOTOR)


BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(ASCENDENCIA = case_when(ASCENDENCIA == "AFRO" ~ "Afro", 
                                                                   ASCENDENCIA == "NO AFRO" ~ "No afro",
                                                                   ASCENDENCIA == "" ~ "Todos"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(URBANORURALUY = case_when(URBANORURALUY == "URBANO (MENOS DE 5.000 HABITANTES)" ~ "Urbano (menos de 5.000 habitantes)",
                                                                     URBANORURALUY == "RURAL DISPERSO" ~ "Rural disperso", 
                                                                     URBANORURALUY == "" ~ "Total país"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(SEXO = case_when(SEXO == "MUJERES" ~ "Mujeres", 
                                                            SEXO=="VARONES" ~ "Varones",
                                                            SEXO == "" ~ "Todos"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(QUINTIL = case_when(QUINTIL == "Quintil 1"~"Quintil 1", 
                                                               QUINTIL == "Quintil 2"~"Quintil 2",
                                                               QUINTIL == "Quintil 3"~"Quintil 3", 
                                                               QUINTIL == "Quintil 4"~"Quintil 4",
                                                               QUINTIL == "Quintil 5"~"Quintil 5",
                                                               QUINTIL == "" ~ "Todos"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(EDAD = case_when(EDAD == "Menor de 30 años"~"Menor de 30 años", 
                                                            EDAD == "Entre 30 y 44 años"~"Entre 30 y 44 años",
                                                            EDAD == "Entre 45 y 64 años"~"Entre 45 y 64 años", 
                                                            EDAD == "65 años o más"~"65 años o más",
                                                            EDAD == "" ~ "Todos"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(POBRE = case_when(POBRE == "POBRE" ~ "Pobre", 
                                                             POBRE == "NO POBRE" ~ "No pobre",
                                                             POBRE == "" ~ "Todos"))


m502_tp <- BASE_MOTOR %>% select(CODIND,	
                                 NOMINDICADOR,	
                                 CATEGORIA,	
                                 PESTAÑA,	
                                 SEXO,	
                                 ASCENDENCIA,	
                                 QUINTIL,	
                                 DEPARTAMENTOUY,	
                                 URBANORURALUY,	
                                 EDAD,	
                                 POBRE, 
                                 NSE, 
                                 NIVELEDU,
                                 PAÍS,	
                                 ANIO,	
                                 VALOR,	
                                 RESPONSABLE)





### 503 Distribución porcentual de los hogares según ciclo de vida del hogar


## País Urbano

ech_h_svy_1 <- ech_h_svy %>%
  filter(bd_region == 1 & ciclo_vida>0)

# Total


NOMINDICADOR <- 
  c("Distribución porcentual de los hogares según ciclo de vida del hogar (Pareja joven sin hijos)", 
    "Distribución porcentual de los hogares según ciclo de vida del hogar (Etapa inicial)",
    "Distribución porcentual de los hogares según ciclo de vida del hogar (Expansión o crecimiento)", 
    "Distribución porcentual de los hogares según ciclo de vida del hogar (Consolidación)",
    "Distribución porcentual de los hogares según ciclo de vida del hogar (Etapa de salida)",
    "Distribución porcentual de los hogares según ciclo de vida del hogar (Pareja mayor sin hijos)",
    "Distribución porcentual de los hogares según ciclo de vida del hogar (Hoagres no familiares)")

var_int <- c("ciclo_vida1", "ciclo_vida2", "ciclo_vida3", "ciclo_vida4", "ciclo_vida5", "ciclo_vida6", "ciclo_vida7")
a_sem <- sapply(ech_h_svy_1$variables %>% select(all_of(var_int)), function(x){
  ech_h_svy_1 %>%
    srvyr::summarise(stat = srvyr::survey_mean(x, na.rm = TRUE)) 
})

c_ano <- matrix(,7,1)
for (i in 1:7) {
  c_ano[i,1] <- c(as.numeric(c(a_sem[1,i])))
}

colnames(c_ano) <- c("VALOR")
c_ano <- cbind(c_ano, NOMINDICADOR)
c_ano <- as.data.frame(c_ano)
BASE_MOTOR <- cbind(c_ano,BASE_AUX)

# Sexo del jefe

a_jefe <- function(y) {
  base <- subset(ech_h_svy_1, sexojefe == y) 
  resultados  <-  sapply(base$variables %>% select(all_of(var_int)), function(x){
    base %>%
      srvyr::summarise(stat1 = srvyr::survey_mean(x, na.rm = TRUE))
    
  })
  y <- as.numeric(resultados[1,])
}

a_e_jefe = matrix(, nrow = 7, ncol = 2)

for(i in 1:2){
  a_e_jefe[,i] <- a_jefe(y = i)
}


c_jefe <- as.data.frame(a_e_jefe)
c_jefe_1 <- cbind(as.data.frame(c_jefe[,1]), NOMINDICADOR, BASE_AUX)
c_jefe_1 <- c_jefe_1 %>% dplyr::mutate(SEXO = "VARONES")
c_jefe_2 <- cbind(as.data.frame(c_jefe[,2]), NOMINDICADOR, BASE_AUX)
c_jefe_2 <- c_jefe_2 %>% dplyr::mutate(SEXO = "MUJERES")
colnames(c_jefe_1) <- colnames(BASE_MOTOR)
colnames(c_jefe_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_jefe_1,c_jefe_2)


# Tramo de edad

a_edad <- function(y) {
  base <- subset(ech_h_svy_1, tramo_edad2 == y) 
  resultados  <-  sapply(base$variables %>% select(all_of(var_int)), function(x){
    base %>%
      srvyr::summarise(stat1 = srvyr::survey_mean(x, na.rm = TRUE))
    
  })
  y <- as.numeric(resultados[1,])
}

a_e_edad = matrix(, nrow = 7, ncol = 4)

for(i in 1:4){
  a_e_edad[,i] <- a_edad(y = i)
}

c_edad <- as.data.frame(a_e_edad)

c_edad_1 <- cbind(as.data.frame(c_edad[,1]), NOMINDICADOR, BASE_AUX)
c_edad_1 <- c_edad_1 %>% dplyr::mutate(EDAD = "Menor de 30 años")
c_edad_2 <- cbind(as.data.frame(c_edad[,2]), NOMINDICADOR, BASE_AUX)
c_edad_2 <- c_edad_2 %>% dplyr::mutate(EDAD = "Entre 30 y 44 años")
c_edad_3 <- cbind(as.data.frame(c_edad[,3]), NOMINDICADOR, BASE_AUX)
c_edad_3 <- c_edad_3 %>% dplyr::mutate(EDAD = "Entre 45 y 64 años")
c_edad_4 <- cbind(as.data.frame(c_edad[,4]), NOMINDICADOR, BASE_AUX)
c_edad_4 <- c_edad_4 %>% dplyr::mutate(EDAD = "65 años o más")
colnames(c_edad_1) <- colnames(BASE_MOTOR)
colnames(c_edad_2) <- colnames(BASE_MOTOR)
colnames(c_edad_3) <- colnames(BASE_MOTOR)
colnames(c_edad_4) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_edad_1,c_edad_2, c_edad_3, c_edad_4)


# Quintil de ingreso del hogar
a_quintil <- function(y) {
  base <- subset(ech_h_svy_1, quintilesy == y) 
  resultados  <-  sapply(base$variables %>% select(all_of(var_int)), function(x){
    base %>%
      srvyr::summarise(stat1 = srvyr::survey_mean(x, na.rm = TRUE))
    
  })
  y <- as.numeric(resultados[1,])
}

a_e_quintil = matrix(, nrow = 7, ncol = 5)

for(i in 1:5){
  a_e_quintil[,i] <- a_quintil(y = i)
}

c_quintil <- as.data.frame(a_e_quintil)

c_quintil_1 <- cbind(as.data.frame(c_quintil[,1]), NOMINDICADOR, BASE_AUX)
c_quintil_1 <- c_quintil_1 %>% dplyr::mutate(QUINTIL = "Quintil 1")
c_quintil_2 <- cbind(as.data.frame(c_quintil[,2]), NOMINDICADOR, BASE_AUX)
c_quintil_2 <- c_quintil_2 %>% dplyr::mutate(QUINTIL = "Quintil 2")
c_quintil_3 <- cbind(as.data.frame(c_quintil[,3]), NOMINDICADOR, BASE_AUX)
c_quintil_3 <- c_quintil_3 %>% dplyr::mutate(QUINTIL = "Quintil 3")
c_quintil_4 <- cbind(as.data.frame(c_quintil[,4]), NOMINDICADOR, BASE_AUX)
c_quintil_4 <- c_quintil_4 %>% dplyr::mutate(QUINTIL = "Quintil 4")
c_quintil_5 <- cbind(as.data.frame(c_quintil[,5]), NOMINDICADOR, BASE_AUX)
c_quintil_5 <- c_quintil_5 %>% dplyr::mutate(QUINTIL = "Quintil 5")
colnames(c_quintil_1) <- colnames(BASE_MOTOR)
colnames(c_quintil_2) <- colnames(BASE_MOTOR)
colnames(c_quintil_3) <- colnames(BASE_MOTOR)
colnames(c_quintil_4) <- colnames(BASE_MOTOR)
colnames(c_quintil_5) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_quintil_1,c_quintil_2, c_quintil_3, c_quintil_4, c_quintil_5)




# Base motor

BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(CODIND = "503")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(CATEGORIA = "Composición y estructura de los hogares")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(PESTAÑA = "País urbano")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(PAÍS = "Uruguay")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(ANIO = 2023)
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(RESPONSABLE = "Sharon Katzkowicz")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(ASCENDENCIA = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(DEPARTAMENTOUY = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(URBANORURALUY = "Urbano (más de 5.000 habitantes)")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(POBRE = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(NSE = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(NIVELEDU = "Todos")
BASE_MOTOR <- as.data.frame(BASE_MOTOR)

BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(SEXO = case_when(SEXO == "" ~ "Todos", SEXO == "MUJERES"~"Mujeres", SEXO=="VARONES"~"Varones"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(QUINTIL = case_when(QUINTIL == "" ~ "Todos",
                                                               QUINTIL == "Quintil 1"~"Quintil 1", 
                                                               QUINTIL == "Quintil 2"~"Quintil 2",
                                                               QUINTIL == "Quintil 3"~"Quintil 3", 
                                                               QUINTIL == "Quintil 4"~"Quintil 4",
                                                               QUINTIL == "Quintil 5"~"Quintil 5"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(EDAD = case_when(EDAD == "" ~ "Todos",
                                                            EDAD == "Menor de 30 años"~"Menor de 30 años", 
                                                            EDAD == "Entre 30 y 44 años"~"Entre 30 y 44 años",
                                                            EDAD == "Entre 45 y 64 años"~"Entre 45 y 64 años", 
                                                            EDAD == "65 años o más"~"65 años o más"))


m503_pu <- BASE_MOTOR %>% select(CODIND,	
                                 NOMINDICADOR,	
                                 CATEGORIA,	
                                 PESTAÑA,	
                                 SEXO,	
                                 ASCENDENCIA,	
                                 QUINTIL,	
                                 DEPARTAMENTOUY,	
                                 URBANORURALUY,	
                                 EDAD,	
                                 POBRE,
                                 NSE,
                                 NIVELEDU,
                                 PAÍS,	
                                 ANIO,	
                                 VALOR,	
                                 RESPONSABLE)	


## Total país
ech_h_svy_1 <- ech_h_svy %>%
  filter(ciclo_vida>0)

# Total

NOMINDICADOR <- 
  c("Distribución porcentual de los hogares según ciclo de vida del hogar (Pareja joven sin hijos)", 
    "Distribución porcentual de los hogares según ciclo de vida del hogar (Etapa inicial)",
    "Distribución porcentual de los hogares según ciclo de vida del hogar (Expansión o crecimiento)", 
    "Distribución porcentual de los hogares según ciclo de vida del hogar (Consolidación)",
    "Distribución porcentual de los hogares según ciclo de vida del hogar (Etapa de salida)",
    "Distribución porcentual de los hogares según ciclo de vida del hogar (Pareja mayor sin hijos)",
    "Distribución porcentual de los hogares según ciclo de vida del hogar (Hoagres no familiares)")

var_int <- c("ciclo_vida1", "ciclo_vida2", "ciclo_vida3", "ciclo_vida4", "ciclo_vida5", "ciclo_vida6", "ciclo_vida7")
a_sem <- sapply(ech_h_svy_1$variables %>% select(all_of(var_int)), function(x){
  ech_h_svy_1 %>%
    srvyr::summarise(stat = srvyr::survey_mean(x, na.rm = TRUE)) 
})

c_ano <- matrix(,7,1)
for (i in 1:7) {
  c_ano[i,1] <- c(as.numeric(c(a_sem[1,i])))
}

colnames(c_ano) <- c("VALOR")
c_ano <- cbind(c_ano, NOMINDICADOR)
c_ano <- as.data.frame(c_ano)
BASE_MOTOR <- cbind(c_ano,BASE_AUX)


# Región

a_reg <- function(y) {
  base <- subset(ech_h_svy_1, bd_region == y) 
  resultados  <-  sapply(base$variables %>% select(all_of(var_int)), function(x){
    base %>%
      srvyr::summarise(stat1 = srvyr::survey_mean(x, na.rm = TRUE))
    
  })
  y <- as.numeric(resultados[1,])
}

a_e_reg = matrix(, nrow = 7, ncol = 3)

for(i in 1:3){
  a_e_reg[,i] <- a_reg(y = i)
}


c_reg <- as.data.frame(a_e_reg[,2:3])
c_reg_1 <- cbind(as.data.frame(c_reg[,1]), NOMINDICADOR, BASE_AUX)
c_reg_1 <- c_reg_1 %>% dplyr::mutate(URBANORURALUY = "URBANO (MENOS DE 5.000 HABITANTES)")
c_reg_2 <- cbind(as.data.frame(c_reg[,2]), NOMINDICADOR, BASE_AUX)
c_reg_2 <- c_reg_2 %>% dplyr::mutate(URBANORURALUY = "RURAL DISPERSO")
colnames(c_reg_1) <- colnames(BASE_MOTOR)
colnames(c_reg_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_reg_1,c_reg_2)


# Sexo del jefe

a_jefe <- function(y) {
  base <- subset(ech_h_svy_1, sexojefe == y) 
  resultados  <-  sapply(base$variables %>% select(all_of(var_int)), function(x){
    base %>%
      srvyr::summarise(stat1 = srvyr::survey_mean(x, na.rm = TRUE))
    
  })
  y <- as.numeric(resultados[1,])
}

a_e_jefe = matrix(, nrow = 7, ncol = 2)

for(i in 1:2){
  a_e_jefe[,i] <- a_jefe(y = i)
}

c_jefe <- as.data.frame(a_e_jefe)
c_jefe_1 <- cbind(as.data.frame(c_jefe[,1]), NOMINDICADOR, BASE_AUX)
c_jefe_1 <- c_jefe_1 %>% dplyr::mutate(SEXO = "VARONES")
c_jefe_2 <- cbind(as.data.frame(c_jefe[,2]), NOMINDICADOR, BASE_AUX)
c_jefe_2 <- c_jefe_2 %>% dplyr::mutate(SEXO = "MUJERES")
colnames(c_jefe_1) <- colnames(BASE_MOTOR)
colnames(c_jefe_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_jefe_1,c_jefe_2)


# Tramo de edad

a_edad <- function(y) {
  base <- subset(ech_h_svy_1, tramo_edad2 == y) 
  resultados  <-  sapply(base$variables %>% select(all_of(var_int)), function(x){
    base %>%
      srvyr::summarise(stat1 = srvyr::survey_mean(x, na.rm = TRUE))
    
  })
  y <- as.numeric(resultados[1,])
}

a_e_edad = matrix(, nrow = 7, ncol = 4)

for(i in 1:4){
  a_e_edad[,i] <- a_edad(y = i)
}


c_edad <- as.data.frame(a_e_edad)

c_edad_1 <- cbind(as.data.frame(c_edad[,1]), NOMINDICADOR, BASE_AUX)
c_edad_1 <- c_edad_1 %>% dplyr::mutate(EDAD = "Menor de 30 años")
c_edad_2 <- cbind(as.data.frame(c_edad[,2]), NOMINDICADOR, BASE_AUX)
c_edad_2 <- c_edad_2 %>% dplyr::mutate(EDAD = "Entre 30 y 44 años")
c_edad_3 <- cbind(as.data.frame(c_edad[,3]), NOMINDICADOR, BASE_AUX)
c_edad_3 <- c_edad_3 %>% dplyr::mutate(EDAD = "Entre 45 y 64 años")
c_edad_4 <- cbind(as.data.frame(c_edad[,4]), NOMINDICADOR, BASE_AUX)
c_edad_4 <- c_edad_4 %>% dplyr::mutate(EDAD = "65 años o más")
colnames(c_edad_1) <- colnames(BASE_MOTOR)
colnames(c_edad_2) <- colnames(BASE_MOTOR)
colnames(c_edad_3) <- colnames(BASE_MOTOR)
colnames(c_edad_4) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_edad_1,c_edad_2, c_edad_3, c_edad_4)



# Ascendencia

a_ascen <- function(y) {
  base <- subset(ech_h_svy_1, bd_e29_1 == y) 
  resultados  <-  sapply(base$variables %>% select(all_of(var_int)), function(x){
    base %>%
      srvyr::summarise(stat1 = srvyr::survey_mean(x, na.rm = TRUE))
    
  })
  y <- as.numeric(resultados[1,])
}

a_e_ascen = matrix(, nrow = 7, ncol = 2)

for(i in 1:2){
  a_e_ascen[,i] <- a_ascen(y = i)
}


c_ascen <- as.data.frame(a_e_ascen)
c_ascen_1 <- cbind(as.data.frame(c_ascen[,1]), NOMINDICADOR, BASE_AUX)
c_ascen_1 <- c_ascen_1 %>% dplyr::mutate(ASCENDENCIA = "AFRO")
c_ascen_2 <- cbind(as.data.frame(c_ascen[,2]), NOMINDICADOR, BASE_AUX)
c_ascen_2 <- c_ascen_2 %>% dplyr::mutate(ASCENDENCIA = "NO AFRO")
colnames(c_ascen_1) <- colnames(BASE_MOTOR)
colnames(c_ascen_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_ascen_1,c_ascen_2)





# Quintil de ingreso del hogar
a_quintil <- function(y) {
  base <- subset(ech_h_svy_1, quintilesy == y) 
  resultados  <-  sapply(base$variables %>% select(all_of(var_int)), function(x){
    base %>%
      srvyr::summarise(stat1 = srvyr::survey_mean(x, na.rm = TRUE))
    
  })
  y <- as.numeric(resultados[1,])
}

a_e_quintil = matrix(, nrow = 7, ncol = 5)

for(i in 1:5){
  a_e_quintil[,i] <- a_quintil(y = i)
}


c_quintil <- as.data.frame(a_e_quintil)

c_quintil_1 <- cbind(as.data.frame(c_quintil[,1]), NOMINDICADOR, BASE_AUX)
c_quintil_1 <- c_quintil_1 %>% dplyr::mutate(QUINTIL = "Quintil 1")
c_quintil_2 <- cbind(as.data.frame(c_quintil[,2]), NOMINDICADOR, BASE_AUX)
c_quintil_2 <- c_quintil_2 %>% dplyr::mutate(QUINTIL = "Quintil 2")
c_quintil_3 <- cbind(as.data.frame(c_quintil[,3]), NOMINDICADOR, BASE_AUX)
c_quintil_3 <- c_quintil_3 %>% dplyr::mutate(QUINTIL = "Quintil 3")
c_quintil_4 <- cbind(as.data.frame(c_quintil[,4]), NOMINDICADOR, BASE_AUX)
c_quintil_4 <- c_quintil_4 %>% dplyr::mutate(QUINTIL = "Quintil 4")
c_quintil_5 <- cbind(as.data.frame(c_quintil[,5]), NOMINDICADOR, BASE_AUX)
c_quintil_5 <- c_quintil_5 %>% dplyr::mutate(QUINTIL = "Quintil 5")
colnames(c_quintil_1) <- colnames(BASE_MOTOR)
colnames(c_quintil_2) <- colnames(BASE_MOTOR)
colnames(c_quintil_3) <- colnames(BASE_MOTOR)
colnames(c_quintil_4) <- colnames(BASE_MOTOR)
colnames(c_quintil_5) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_quintil_1,c_quintil_2, c_quintil_3, c_quintil_4, c_quintil_5)


# Pobreza

a_pobre <- function(y) {
  base <- subset(ech_h_svy_1, pobre_aux == y) 
  resultados  <-  sapply(base$variables %>% select(all_of(var_int)), function(x){
    base %>%
      srvyr::summarise(stat1 = srvyr::survey_mean(x, na.rm = TRUE))
    
  })
  y <- as.numeric(resultados[1,])
}

a_e_pobre = matrix(, nrow = 7, ncol = 2)

for(i in 1:2){
  a_e_pobre[,i] <- a_pobre(y = i)
}

c_pobre <- as.data.frame(a_e_pobre)
c_pobre_1 <- cbind(as.data.frame(c_pobre[,1]), NOMINDICADOR, BASE_AUX)
c_pobre_1 <- c_pobre_1 %>% dplyr::mutate(POBRE = "POBRE")
c_pobre_2 <- cbind(as.data.frame(c_pobre[,2]), NOMINDICADOR, BASE_AUX)
c_pobre_2 <- c_pobre_2 %>% dplyr::mutate(POBRE = "NO POBRE")
colnames(c_pobre_1) <- colnames(BASE_MOTOR)
colnames(c_pobre_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_pobre_1,c_pobre_2)


# Base motor

BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(CODIND = "503")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(CATEGORIA = "Composición y estructura de los hogares")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(PESTAÑA = "Total país")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(PAÍS = "Uruguay")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(ANIO = 2023)
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(RESPONSABLE = "Sharon Katzkowicz")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(DEPARTAMENTOUY = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(NSE = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(NIVELEDU = "Todos")
BASE_MOTOR <- as.data.frame(BASE_MOTOR)


BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(ASCENDENCIA = case_when(ASCENDENCIA == "AFRO" ~ "Afro", 
                                                                   ASCENDENCIA == "NO AFRO" ~ "No afro",
                                                                   ASCENDENCIA == "" ~ "Todos"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(URBANORURALUY = case_when(URBANORURALUY == "URBANO (MENOS DE 5.000 HABITANTES)" ~ "Urbano (menos de 5.000 habitantes)",
                                                                     URBANORURALUY == "RURAL DISPERSO" ~ "Rural disperso", 
                                                                     URBANORURALUY == "" ~ "Total país"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(SEXO = case_when(SEXO == "MUJERES" ~ "Mujeres", 
                                                            SEXO=="VARONES" ~ "Varones",
                                                            SEXO == "" ~ "Todos"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(QUINTIL = case_when(QUINTIL == "Quintil 1"~"Quintil 1", 
                                                               QUINTIL == "Quintil 2"~"Quintil 2",
                                                               QUINTIL == "Quintil 3"~"Quintil 3", 
                                                               QUINTIL == "Quintil 4"~"Quintil 4",
                                                               QUINTIL == "Quintil 5"~"Quintil 5",
                                                               QUINTIL == "" ~ "Todos"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(EDAD = case_when(EDAD == "Menor de 30 años"~"Menor de 30 años", 
                                                            EDAD == "Entre 30 y 44 años"~"Entre 30 y 44 años",
                                                            EDAD == "Entre 45 y 64 años"~"Entre 45 y 64 años", 
                                                            EDAD == "65 años o más"~"65 años o más",
                                                            EDAD == "" ~ "Todos"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(POBRE = case_when(POBRE == "POBRE" ~ "Pobre", 
                                                             POBRE == "NO POBRE" ~ "No pobre",
                                                             POBRE == "" ~ "Todos"))


m503_tp <- BASE_MOTOR %>% select(CODIND,	
                                 NOMINDICADOR,	
                                 CATEGORIA,	
                                 PESTAÑA,	
                                 SEXO,	
                                 ASCENDENCIA,	
                                 QUINTIL,	
                                 DEPARTAMENTOUY,	
                                 URBANORURALUY,	
                                 EDAD,	
                                 POBRE,
                                 NSE,
                                 NIVELEDU,
                                 PAÍS,	
                                 ANIO,	
                                 VALOR,	
                                 RESPONSABLE)






### 504 Distribución porcentual de los hogares biparentales según modelo de pareja


## País Urbano

ech_h_svy_1 <- ech_h_svy %>%
  filter(bd_region == 1 & modelo_pareja>0)

# Total


NOMINDICADOR <- 
  c("Distribución porcentual de los hogares biparentales según modelo de pareja (Proveedor tradicional)",
    "Distribución porcentual de los hogares biparentales según modelo de pareja (Proveedor modificado)",
    "Distribución porcentual de los hogares biparentales según modelo de pareja (Doble carrera)",
    "Distribución porcentual de los hogares biparentales según modelo de pareja (Inversión de roles)",
    "Distribución porcentual de los hogares biparentales según modelo de pareja (Inversión de roles modificado)",
    "Distribución porcentual de los hogares biparentales según modelo de pareja (Residual)" )



var_int <- c("modelo_pareja1", "modelo_pareja2", "modelo_pareja3", "modelo_pareja4", "modelo_pareja5", "modelo_pareja6")
a_sem <- sapply(ech_h_svy_1$variables %>% select(all_of(var_int)), function(x){
  ech_h_svy_1 %>%
    srvyr::summarise(stat = srvyr::survey_mean(x, na.rm = TRUE)) 
})

c_ano <- matrix(,6,1)
for (i in 1:6) {
  c_ano[i,1] <- c(as.numeric(c(a_sem[1,i])))
}

colnames(c_ano) <- c("VALOR")
c_ano <- cbind(c_ano, NOMINDICADOR)
c_ano <- as.data.frame(c_ano)
BASE_MOTOR <- cbind(c_ano,BASE_AUX)

# Sexo del jefe

a_jefe <- function(y) {
  base <- subset(ech_h_svy_1, sexojefe == y) 
  resultados  <-  sapply(base$variables %>% select(all_of(var_int)), function(x){
    base %>%
      srvyr::summarise(stat1 = srvyr::survey_mean(x, na.rm = TRUE))
    
  })
  y <- as.numeric(resultados[1,])
}

a_e_jefe = matrix(, nrow = 6, ncol = 2)

for(i in 1:2){
  a_e_jefe[,i] <- a_jefe(y = i)
}


c_jefe <- as.data.frame(a_e_jefe)
c_jefe_1 <- cbind(as.data.frame(c_jefe[,1]), NOMINDICADOR, BASE_AUX)
c_jefe_1 <- c_jefe_1 %>% dplyr::mutate(SEXO = "VARONES")
c_jefe_2 <- cbind(as.data.frame(c_jefe[,2]), NOMINDICADOR, BASE_AUX)
c_jefe_2 <- c_jefe_2 %>% dplyr::mutate(SEXO = "MUJERES")
colnames(c_jefe_1) <- colnames(BASE_MOTOR)
colnames(c_jefe_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_jefe_1,c_jefe_2)


# Tramo de edad

a_edad <- function(y) {
  base <- subset(ech_h_svy_1, tramo_edad2 == y) 
  resultados  <-  sapply(base$variables %>% select(all_of(var_int)), function(x){
    base %>%
      srvyr::summarise(stat1 = srvyr::survey_mean(x, na.rm = TRUE))
    
  })
  y <- as.numeric(resultados[1,])
}

a_e_edad = matrix(, nrow = 6, ncol = 4)

for(i in 1:4){
  a_e_edad[,i] <- a_edad(y = i)
}

c_edad <- as.data.frame(a_e_edad)

c_edad_1 <- cbind(as.data.frame(c_edad[,1]), NOMINDICADOR, BASE_AUX)
c_edad_1 <- c_edad_1 %>% dplyr::mutate(EDAD = "Menor de 30 años")
c_edad_2 <- cbind(as.data.frame(c_edad[,2]), NOMINDICADOR, BASE_AUX)
c_edad_2 <- c_edad_2 %>% dplyr::mutate(EDAD = "Entre 30 y 44 años")
c_edad_3 <- cbind(as.data.frame(c_edad[,3]), NOMINDICADOR, BASE_AUX)
c_edad_3 <- c_edad_3 %>% dplyr::mutate(EDAD = "Entre 45 y 64 años")
c_edad_4 <- cbind(as.data.frame(c_edad[,4]), NOMINDICADOR, BASE_AUX)
c_edad_4 <- c_edad_4 %>% dplyr::mutate(EDAD = "65 años o más")
colnames(c_edad_1) <- colnames(BASE_MOTOR)
colnames(c_edad_2) <- colnames(BASE_MOTOR)
colnames(c_edad_3) <- colnames(BASE_MOTOR)
colnames(c_edad_4) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_edad_1,c_edad_2, c_edad_3, c_edad_4)


# Quintil de ingreso del hogar
a_quintil <- function(y) {
  base <- subset(ech_h_svy_1, quintilesy == y) 
  resultados  <-  sapply(base$variables %>% select(all_of(var_int)), function(x){
    base %>%
      srvyr::summarise(stat1 = srvyr::survey_mean(x, na.rm = TRUE))
    
  })
  y <- as.numeric(resultados[1,])
}

a_e_quintil = matrix(, nrow = 6, ncol = 5)

for(i in 1:5){
  a_e_quintil[,i] <- a_quintil(y = i)
}


c_quintil <- as.data.frame(a_e_quintil)

c_quintil_1 <- cbind(as.data.frame(c_quintil[,1]), NOMINDICADOR, BASE_AUX)
c_quintil_1 <- c_quintil_1 %>% dplyr::mutate(QUINTIL = "Quintil 1")
c_quintil_2 <- cbind(as.data.frame(c_quintil[,2]), NOMINDICADOR, BASE_AUX)
c_quintil_2 <- c_quintil_2 %>% dplyr::mutate(QUINTIL = "Quintil 2")
c_quintil_3 <- cbind(as.data.frame(c_quintil[,3]), NOMINDICADOR, BASE_AUX)
c_quintil_3 <- c_quintil_3 %>% dplyr::mutate(QUINTIL = "Quintil 3")
c_quintil_4 <- cbind(as.data.frame(c_quintil[,4]), NOMINDICADOR, BASE_AUX)
c_quintil_4 <- c_quintil_4 %>% dplyr::mutate(QUINTIL = "Quintil 4")
c_quintil_5 <- cbind(as.data.frame(c_quintil[,5]), NOMINDICADOR, BASE_AUX)
c_quintil_5 <- c_quintil_5 %>% dplyr::mutate(QUINTIL = "Quintil 5")
colnames(c_quintil_1) <- colnames(BASE_MOTOR)
colnames(c_quintil_2) <- colnames(BASE_MOTOR)
colnames(c_quintil_3) <- colnames(BASE_MOTOR)
colnames(c_quintil_4) <- colnames(BASE_MOTOR)
colnames(c_quintil_5) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_quintil_1,c_quintil_2, c_quintil_3, c_quintil_4, c_quintil_5)




# Base motor

BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(CODIND = "504")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(CATEGORIA = "Composición y estructura de los hogares")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(PESTAÑA = "País urbano")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(PAÍS = "Uruguay")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(ANIO = 2023)
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(RESPONSABLE = "Sharon Katzkowicz")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(ASCENDENCIA = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(DEPARTAMENTOUY = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(URBANORURALUY = "Urbano (más de 5.000 habitantes)")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(POBRE = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(NSE = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(NIVELEDU = "Todos")
BASE_MOTOR <- as.data.frame(BASE_MOTOR)

BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(SEXO = case_when(SEXO == "" ~ "Todos", SEXO == "MUJERES"~"Mujeres", SEXO=="VARONES"~"Varones"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(QUINTIL = case_when(QUINTIL == "" ~ "Todos",
                                                               QUINTIL == "Quintil 1"~"Quintil 1", 
                                                               QUINTIL == "Quintil 2"~"Quintil 2",
                                                               QUINTIL == "Quintil 3"~"Quintil 3", 
                                                               QUINTIL == "Quintil 4"~"Quintil 4",
                                                               QUINTIL == "Quintil 5"~"Quintil 5"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(EDAD = case_when(EDAD == "" ~ "Todos",
                                                            EDAD == "Menor de 30 años"~"Menor de 30 años", 
                                                            EDAD == "Entre 30 y 44 años"~"Entre 30 y 44 años",
                                                            EDAD == "Entre 45 y 64 años"~"Entre 45 y 64 años", 
                                                            EDAD == "65 años o más"~"65 años o más"))


m504_pu <- BASE_MOTOR %>% select(CODIND,	
                                 NOMINDICADOR,	
                                 CATEGORIA,	
                                 PESTAÑA,	
                                 SEXO,	
                                 ASCENDENCIA,	
                                 QUINTIL,	
                                 DEPARTAMENTOUY,	
                                 URBANORURALUY,	
                                 EDAD,	
                                 POBRE,
                                 NSE,
                                 NIVELEDU,
                                 PAÍS,	
                                 ANIO,	
                                 VALOR,	
                                 RESPONSABLE)	


## Total país

ech_h_svy_1 <- ech_h_svy %>%
  filter(modelo_pareja>0)

# Total

NOMINDICADOR <- 
  c("Distribución porcentual de los hogares biparentales según modelo de pareja (Proveedor tradicional)",
    "Distribución porcentual de los hogares biparentales según modelo de pareja (Proveedor modificado)",
    "Distribución porcentual de los hogares biparentales según modelo de pareja (Doble carrera)",
    "Distribución porcentual de los hogares biparentales según modelo de pareja (Inversión de roles)",
    "Distribución porcentual de los hogares biparentales según modelo de pareja (Inversión de roles modificado)",
    "Distribución porcentual de los hogares biparentales según modelo de pareja (Residual)" )


var_int <- c("modelo_pareja1", "modelo_pareja2", "modelo_pareja3", "modelo_pareja4", "modelo_pareja5", "modelo_pareja6")
a_sem <- sapply(ech_h_svy_1$variables %>% select(all_of(var_int)), function(x){
  ech_h_svy_1 %>%
    srvyr::summarise(stat = srvyr::survey_mean(x, na.rm = TRUE)) 
})

c_ano <- matrix(,6,1)
for (i in 1:6) {
  c_ano[i,1] <- c(as.numeric(c(a_sem[1,i])))
}

colnames(c_ano) <- c("VALOR")
c_ano <- cbind(c_ano, NOMINDICADOR)
c_ano <- as.data.frame(c_ano)
BASE_MOTOR <- cbind(c_ano,BASE_AUX)


# Región

a_reg <- function(y) {
  base <- subset(ech_h_svy_1, bd_region == y) 
  resultados  <-  sapply(base$variables %>% select(all_of(var_int)), function(x){
    base %>%
      srvyr::summarise(stat1 = srvyr::survey_mean(x, na.rm = TRUE))
    
  })
  y <- as.numeric(resultados[1,])
}

a_e_reg = matrix(, nrow = 6, ncol = 3)

for(i in 1:3){
  a_e_reg[,i] <- a_reg(y = i)
}

c_reg <- as.data.frame(a_e_reg[,2:3])
c_reg_1 <- cbind(as.data.frame(c_reg[,1]), NOMINDICADOR, BASE_AUX)
c_reg_1 <- c_reg_1 %>% dplyr::mutate(URBANORURALUY = "URBANO (MENOS DE 5.000 HABITANTES)")
c_reg_2 <- cbind(as.data.frame(c_reg[,2]), NOMINDICADOR, BASE_AUX)
c_reg_2 <- c_reg_2 %>% dplyr::mutate(URBANORURALUY = "RURAL DISPERSO")
colnames(c_reg_1) <- colnames(BASE_MOTOR)
colnames(c_reg_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_reg_1,c_reg_2)


# Sexo del jefe

a_jefe <- function(y) {
  base <- subset(ech_h_svy_1, sexojefe == y) 
  resultados  <-  sapply(base$variables %>% select(all_of(var_int)), function(x){
    base %>%
      srvyr::summarise(stat1 = srvyr::survey_mean(x, na.rm = TRUE))
    
  })
  y <- as.numeric(resultados[1,])
}

a_e_jefe = matrix(, nrow = 6, ncol = 2)

for(i in 1:2){
  a_e_jefe[,i] <- a_jefe(y = i)
}


c_jefe <- as.data.frame(a_e_jefe)
c_jefe_1 <- cbind(as.data.frame(c_jefe[,1]), NOMINDICADOR, BASE_AUX)
c_jefe_1 <- c_jefe_1 %>% dplyr::mutate(SEXO = "VARONES")
c_jefe_2 <- cbind(as.data.frame(c_jefe[,2]), NOMINDICADOR, BASE_AUX)
c_jefe_2 <- c_jefe_2 %>% dplyr::mutate(SEXO = "MUJERES")
colnames(c_jefe_1) <- colnames(BASE_MOTOR)
colnames(c_jefe_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_jefe_1,c_jefe_2)


# Tramo de edad

a_edad <- function(y) {
  base <- subset(ech_h_svy_1, tramo_edad2 == y) 
  resultados  <-  sapply(base$variables %>% select(all_of(var_int)), function(x){
    base %>%
      srvyr::summarise(stat1 = srvyr::survey_mean(x, na.rm = TRUE))
    
  })
  y <- as.numeric(resultados[1,])
}

a_e_edad = matrix(, nrow = 6, ncol = 4)

for(i in 1:4){
  a_e_edad[,i] <- a_edad(y = i)
}

c_edad <- as.data.frame(a_e_edad)

c_edad_1 <- cbind(as.data.frame(c_edad[,1]), NOMINDICADOR, BASE_AUX)
c_edad_1 <- c_edad_1 %>% dplyr::mutate(EDAD = "Menor de 30 años")
c_edad_2 <- cbind(as.data.frame(c_edad[,2]), NOMINDICADOR, BASE_AUX)
c_edad_2 <- c_edad_2 %>% dplyr::mutate(EDAD = "Entre 30 y 44 años")
c_edad_3 <- cbind(as.data.frame(c_edad[,3]), NOMINDICADOR, BASE_AUX)
c_edad_3 <- c_edad_3 %>% dplyr::mutate(EDAD = "Entre 45 y 64 años")
c_edad_4 <- cbind(as.data.frame(c_edad[,4]), NOMINDICADOR, BASE_AUX)
c_edad_4 <- c_edad_4 %>% dplyr::mutate(EDAD = "65 años o más")
colnames(c_edad_1) <- colnames(BASE_MOTOR)
colnames(c_edad_2) <- colnames(BASE_MOTOR)
colnames(c_edad_3) <- colnames(BASE_MOTOR)
colnames(c_edad_4) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_edad_1,c_edad_2, c_edad_3, c_edad_4)



# Ascendencia

a_ascen <- function(y) {
  base <- subset(ech_h_svy_1, bd_e29_1 == y) 
  resultados  <-  sapply(base$variables %>% select(all_of(var_int)), function(x){
    base %>%
      srvyr::summarise(stat1 = srvyr::survey_mean(x, na.rm = TRUE))
    
  })
  y <- as.numeric(resultados[1,])
}

a_e_ascen = matrix(, nrow = 6, ncol = 2)

for(i in 1:2){
  a_e_ascen[,i] <- a_ascen(y = i)
}

c_ascen <- as.data.frame(a_e_ascen)
c_ascen_1 <- cbind(as.data.frame(c_ascen[,1]), NOMINDICADOR, BASE_AUX)
c_ascen_1 <- c_ascen_1 %>% dplyr::mutate(ASCENDENCIA = "AFRO")
c_ascen_2 <- cbind(as.data.frame(c_ascen[,2]), NOMINDICADOR, BASE_AUX)
c_ascen_2 <- c_ascen_2 %>% dplyr::mutate(ASCENDENCIA = "NO AFRO")
colnames(c_ascen_1) <- colnames(BASE_MOTOR)
colnames(c_ascen_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_ascen_1,c_ascen_2)





# Quintil de ingreso del hogar
a_quintil <- function(y) {
  base <- subset(ech_h_svy_1, quintilesy == y) 
  resultados  <-  sapply(base$variables %>% select(all_of(var_int)), function(x){
    base %>%
      srvyr::summarise(stat1 = srvyr::survey_mean(x, na.rm = TRUE))
    
  })
  y <- as.numeric(resultados[1,])
}

a_e_quintil = matrix(, nrow = 6, ncol = 5)

for(i in 1:5){
  a_e_quintil[,i] <- a_quintil(y = i)
}

c_quintil <- as.data.frame(a_e_quintil)

c_quintil_1 <- cbind(as.data.frame(c_quintil[,1]), NOMINDICADOR, BASE_AUX)
c_quintil_1 <- c_quintil_1 %>% dplyr::mutate(QUINTIL = "Quintil 1")
c_quintil_2 <- cbind(as.data.frame(c_quintil[,2]), NOMINDICADOR, BASE_AUX)
c_quintil_2 <- c_quintil_2 %>% dplyr::mutate(QUINTIL = "Quintil 2")
c_quintil_3 <- cbind(as.data.frame(c_quintil[,3]), NOMINDICADOR, BASE_AUX)
c_quintil_3 <- c_quintil_3 %>% dplyr::mutate(QUINTIL = "Quintil 3")
c_quintil_4 <- cbind(as.data.frame(c_quintil[,4]), NOMINDICADOR, BASE_AUX)
c_quintil_4 <- c_quintil_4 %>% dplyr::mutate(QUINTIL = "Quintil 4")
c_quintil_5 <- cbind(as.data.frame(c_quintil[,5]), NOMINDICADOR, BASE_AUX)
c_quintil_5 <- c_quintil_5 %>% dplyr::mutate(QUINTIL = "Quintil 5")
colnames(c_quintil_1) <- colnames(BASE_MOTOR)
colnames(c_quintil_2) <- colnames(BASE_MOTOR)
colnames(c_quintil_3) <- colnames(BASE_MOTOR)
colnames(c_quintil_4) <- colnames(BASE_MOTOR)
colnames(c_quintil_5) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_quintil_1,c_quintil_2, c_quintil_3, c_quintil_4, c_quintil_5)


# Pobreza

a_pobre <- function(y) {
  base <- subset(ech_h_svy_1, pobre_aux == y) 
  resultados  <-  sapply(base$variables %>% select(all_of(var_int)), function(x){
    base %>%
      srvyr::summarise(stat1 = srvyr::survey_mean(x, na.rm = TRUE))
    
  })
  y <- as.numeric(resultados[1,])
}

a_e_pobre = matrix(, nrow = 6, ncol = 2)

for(i in 1:2){
  a_e_pobre[,i] <- a_pobre(y = i)
}

c_pobre <- as.data.frame(a_e_pobre)
c_pobre_1 <- cbind(as.data.frame(c_pobre[,1]), NOMINDICADOR, BASE_AUX)
c_pobre_1 <- c_pobre_1 %>% dplyr::mutate(POBRE = "POBRE")
c_pobre_2 <- cbind(as.data.frame(c_pobre[,2]), NOMINDICADOR, BASE_AUX)
c_pobre_2 <- c_pobre_2 %>% dplyr::mutate(POBRE = "NO POBRE")
colnames(c_pobre_1) <- colnames(BASE_MOTOR)
colnames(c_pobre_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_pobre_1,c_pobre_2)


# Base motor

BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(CODIND = "504")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(CATEGORIA = "Composición y estructura de los hogares")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(PESTAÑA = "Total país")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(PAÍS = "Uruguay")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(ANIO = 2023)
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(RESPONSABLE = "Sharon Katzkowicz")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(DEPARTAMENTOUY = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(NSE = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(NIVELEDU = "Todos")
BASE_MOTOR <- as.data.frame(BASE_MOTOR)


BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(ASCENDENCIA = case_when(ASCENDENCIA == "AFRO" ~ "Afro", 
                                                                   ASCENDENCIA == "NO AFRO" ~ "No afro",
                                                                   ASCENDENCIA == "" ~ "Todos"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(URBANORURALUY = case_when(URBANORURALUY == "URBANO (MENOS DE 5.000 HABITANTES)" ~ "Urbano (menos de 5.000 habitantes)",
                                                                     URBANORURALUY == "RURAL DISPERSO" ~ "Rural disperso", 
                                                                     URBANORURALUY == "" ~ "Total país"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(SEXO = case_when(SEXO == "MUJERES" ~ "Mujeres", 
                                                            SEXO=="VARONES" ~ "Varones",
                                                            SEXO == "" ~ "Todos"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(QUINTIL = case_when(QUINTIL == "Quintil 1"~"Quintil 1", 
                                                               QUINTIL == "Quintil 2"~"Quintil 2",
                                                               QUINTIL == "Quintil 3"~"Quintil 3", 
                                                               QUINTIL == "Quintil 4"~"Quintil 4",
                                                               QUINTIL == "Quintil 5"~"Quintil 5",
                                                               QUINTIL == "" ~ "Todos"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(EDAD = case_when(EDAD == "Menor de 30 años"~"Menor de 30 años", 
                                                            EDAD == "Entre 30 y 44 años"~"Entre 30 y 44 años",
                                                            EDAD == "Entre 45 y 64 años"~"Entre 45 y 64 años", 
                                                            EDAD == "65 años o más"~"65 años o más",
                                                            EDAD == "" ~ "Todos"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(POBRE = case_when(POBRE == "POBRE" ~ "Pobre", 
                                                             POBRE == "NO POBRE" ~ "No pobre",
                                                             POBRE == "" ~ "Todos"))


m504_tp <- BASE_MOTOR %>% select(CODIND,	
                                 NOMINDICADOR,	
                                 CATEGORIA,	
                                 PESTAÑA,	
                                 SEXO,	
                                 ASCENDENCIA,	
                                 QUINTIL,	
                                 DEPARTAMENTOUY,	
                                 URBANORURALUY,	
                                 EDAD,	
                                 POBRE,
                                 NSE,
                                 NIVELEDU,
                                 PAÍS,	
                                 ANIO,	
                                 VALOR,	
                                 RESPONSABLE)






### 505 Distribución porcentual de las personas según estado civil


## País Urbano

ech_svy_1 <- ech_svy %>%
  filter(bd_region == 1 & bc_pe3>=14 & bc_pe5>0)

# Total


NOMINDICADOR <- 
  c("Distribución porcentual de las personas según estado conyugal (Unión libre)", 
    "Distribución porcentual de las personas según estado conyugal (Casada/o)",
    "Distribución porcentual de las personas según estado conyugal (Divorciada/o o separada/o)",
    "Distribución porcentual de las personas según estado conyugal (Viuda/o)",
    "Distribución porcentual de las personas según estado conyugal (Soltera/o)")



var_int <- c("estado_civil1", "estado_civil2", "estado_civil3", "estado_civil4", "estado_civil5")
b_sem <- sapply(ech_svy_1$variables %>% select(all_of(var_int)), function(x){
  ech_svy_1 %>%
    srvyr::summarise(stat = srvyr::survey_mean(x, na.rm = TRUE)) 
})

c_ano <- matrix(,5,1)
for (i in 1:5) {
  c_ano[i,1] <- c(as.numeric(b_sem[1,i]))
}

colnames(c_ano) <- c("VALOR")
c_ano <- cbind(c_ano, NOMINDICADOR)
c_ano <- as.data.frame(c_ano)
BASE_MOTOR <- cbind(c_ano,BASE_AUX)

# Sexo

b_sexo <- function(y) {
  base <- subset(ech_svy_1, bc_pe2 == y) 
  resultados  <-  sapply(base$variables %>% select(all_of(var_int)), function(x){
    base %>%
      srvyr::summarise(stat1 = srvyr::survey_mean(x, na.rm = TRUE))
    
  })
  y <- as.numeric(resultados[1,])
}

b_e_sexo = matrix(, nrow = 5, ncol = 2)

for(i in 1:2){
  b_e_sexo[,i] <- b_sexo(y = i)
}

c_sexo <- as.data.frame(b_e_sexo)
c_sexo_1 <- cbind(as.data.frame(c_sexo[,1]), NOMINDICADOR, BASE_AUX)
c_sexo_1 <- c_sexo_1 %>% dplyr::mutate(SEXO = "VARONES")
c_sexo_2 <- cbind(as.data.frame(c_sexo[,2]), NOMINDICADOR, BASE_AUX)
c_sexo_2 <- c_sexo_2 %>% dplyr::mutate(SEXO = "MUJERES")
colnames(c_sexo_1) <- colnames(BASE_MOTOR)
colnames(c_sexo_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_sexo_1,c_sexo_2)


# Tramo de edad

b_edad <- function(y) {
  base <- subset(ech_svy_1, tramo_edad2 == y) 
  resultados  <-  sapply(base$variables %>% select(all_of(var_int)), function(x){
    base %>%
      srvyr::summarise(stat1 = srvyr::survey_mean(x, na.rm = TRUE))
    
  })
  y <- as.numeric(resultados[1,])
}

b_e_edad = matrix(, nrow = 5, ncol = 4)

for(i in 1:4){
  b_e_edad[,i] <- b_edad(y = i)
}

c_edad <- as.data.frame(b_e_edad)
c_edad_1 <- cbind(as.data.frame(c_edad[,1]), NOMINDICADOR, BASE_AUX)
c_edad_1 <- c_edad_1 %>% dplyr::mutate(EDAD = "Menor de 30 años")
c_edad_2 <- cbind(as.data.frame(c_edad[,2]), NOMINDICADOR, BASE_AUX)
c_edad_2 <- c_edad_2 %>% dplyr::mutate(EDAD = "Entre 30 y 44 años")
c_edad_3 <- cbind(as.data.frame(c_edad[,3]), NOMINDICADOR, BASE_AUX)
c_edad_3 <- c_edad_3 %>% dplyr::mutate(EDAD = "Entre 45 y 64 años")
c_edad_4 <- cbind(as.data.frame(c_edad[,4]), NOMINDICADOR, BASE_AUX)
c_edad_4 <- c_edad_4 %>% dplyr::mutate(EDAD = "65 años o más")
colnames(c_edad_1) <- colnames(BASE_MOTOR)
colnames(c_edad_2) <- colnames(BASE_MOTOR)
colnames(c_edad_3) <- colnames(BASE_MOTOR)
colnames(c_edad_4) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_edad_1,c_edad_2, c_edad_3, c_edad_4)


# Quintil de ingreso del hogar
b_quintil <- function(y) {
  base <- subset(ech_svy_1, quintilesy == y) 
  resultados  <-  sapply(base$variables %>% select(all_of(var_int)), function(x){
    base %>%
      srvyr::summarise(stat1 = srvyr::survey_mean(x, na.rm = TRUE))
    
  })
  y <- as.numeric(resultados[1,])
}

b_e_quintil = matrix(, nrow = 5, ncol = 5)

for(i in 1:5){
  b_e_quintil[,i] <- b_quintil(y = i)
}

c_quintil <- as.data.frame(b_e_quintil)
c_quintil_1 <- cbind(as.data.frame(c_quintil[,1]), NOMINDICADOR, BASE_AUX)
c_quintil_1 <- c_quintil_1 %>% dplyr::mutate(QUINTIL = "Quintil 1")
c_quintil_2 <- cbind(as.data.frame(c_quintil[,2]), NOMINDICADOR, BASE_AUX)
c_quintil_2 <- c_quintil_2 %>% dplyr::mutate(QUINTIL = "Quintil 2")
c_quintil_3 <- cbind(as.data.frame(c_quintil[,3]), NOMINDICADOR, BASE_AUX)
c_quintil_3 <- c_quintil_3 %>% dplyr::mutate(QUINTIL = "Quintil 3")
c_quintil_4 <- cbind(as.data.frame(c_quintil[,4]), NOMINDICADOR, BASE_AUX)
c_quintil_4 <- c_quintil_4 %>% dplyr::mutate(QUINTIL = "Quintil 4")
c_quintil_5 <- cbind(as.data.frame(c_quintil[,5]), NOMINDICADOR, BASE_AUX)
c_quintil_5 <- c_quintil_5 %>% dplyr::mutate(QUINTIL = "Quintil 5")
colnames(c_quintil_1) <- colnames(BASE_MOTOR)
colnames(c_quintil_2) <- colnames(BASE_MOTOR)
colnames(c_quintil_3) <- colnames(BASE_MOTOR)
colnames(c_quintil_4) <- colnames(BASE_MOTOR)
colnames(c_quintil_5) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_quintil_1,c_quintil_2, c_quintil_3, c_quintil_4, c_quintil_5)




# Base motor

BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(CODIND = "505")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(CATEGORIA = "Composición y estructura de los hogares")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(PESTAÑA = "País urbano")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(PAÍS = "Uruguay")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(ANIO = 2023)
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(RESPONSABLE = "Sharon Katzkowicz")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(ASCENDENCIA = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(DEPARTAMENTOUY = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(URBANORURALUY = "Urbano (más de 5.000 habitantes)")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(POBRE = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(NSE = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(NIVELEDU = "Todos")
BASE_MOTOR <- as.data.frame(BASE_MOTOR)

BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(SEXO = case_when(SEXO == "" ~ "Todos", SEXO == "MUJERES"~"Mujeres", SEXO=="VARONES"~"Varones"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(QUINTIL = case_when(QUINTIL == "" ~ "Todos",
                                                               QUINTIL == "Quintil 1"~"Quintil 1", 
                                                               QUINTIL == "Quintil 2"~"Quintil 2",
                                                               QUINTIL == "Quintil 3"~"Quintil 3", 
                                                               QUINTIL == "Quintil 4"~"Quintil 4",
                                                               QUINTIL == "Quintil 5"~"Quintil 5"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(EDAD = case_when(EDAD == "" ~ "Todos",
                                                            EDAD == "Menor de 30 años"~"Menor de 30 años", 
                                                            EDAD == "Entre 30 y 44 años"~"Entre 30 y 44 años",
                                                            EDAD == "Entre 45 y 64 años"~"Entre 45 y 64 años", 
                                                            EDAD == "65 años o más"~"65 años o más"))


m505_pu <- BASE_MOTOR %>% select(CODIND,	
                                 NOMINDICADOR,	
                                 CATEGORIA,	
                                 PESTAÑA,	
                                 SEXO,	
                                 ASCENDENCIA,	
                                 QUINTIL,	
                                 DEPARTAMENTOUY,	
                                 URBANORURALUY,	
                                 EDAD,	
                                 POBRE,
                                 NSE,
                                 NIVELEDU,
                                 PAÍS,	
                                 ANIO,	
                                 VALOR,	
                                 RESPONSABLE)	


## Total país

# Total

ech_svy_1 <- ech_svy %>%
  filter(bc_pe3>=14 & bc_pe5>0)



NOMINDICADOR <- 
  c("Distribución porcentual de las personas según estado conyugal (Unión libre)", 
    "Distribución porcentual de las personas según estado conyugal (Casada/o)",
    "Distribución porcentual de las personas según estado conyugal (Divorciada/o o separada/o)",
    "Distribución porcentual de las personas según estado conyugal (Viuda/o)",
    "Distribución porcentual de las personas según estado conyugal (Soltera/o)")


var_int <- c("estado_civil1", "estado_civil2", "estado_civil3", "estado_civil4", "estado_civil5")
b_sem <- sapply(ech_svy_1$variables %>% select(all_of(var_int)), function(x){
  ech_svy_1 %>%
    srvyr::summarise(stat = srvyr::survey_mean(x, na.rm = TRUE)) 
})

c_ano <- matrix(,5,1)
for (i in 1:5) {
  c_ano[i,1] <- c(as.numeric(b_sem[1,i]))
}

colnames(c_ano) <- c("VALOR")
c_ano <- cbind(c_ano, NOMINDICADOR)
c_ano <- as.data.frame(c_ano)
BASE_MOTOR <- cbind(c_ano,BASE_AUX)


# Región
b_reg <- function(y) {
  base <- subset(ech_svy_1, bd_region == y) 
  resultados  <-  sapply(base$variables %>% select(all_of(var_int)), function(x){
    base %>%
      srvyr::summarise(stat1 = srvyr::survey_mean(x, na.rm = TRUE))
    
  })
  y <- as.numeric(resultados[1,])
}

b_e_reg = matrix(, nrow = 5, ncol = 3)

for(i in 1:3){
  b_e_reg[,i] <- b_reg(y = i)
}

c_reg <- as.data.frame(b_e_reg[,2:3])
c_reg_1 <- cbind(as.data.frame(c_reg[,1]), NOMINDICADOR, BASE_AUX)
c_reg_1 <- c_reg_1 %>% dplyr::mutate(URBANORURALUY = "URBANO (MENOS DE 5.000 HABITANTES)")
c_reg_2 <- cbind(as.data.frame(c_reg[,2]), NOMINDICADOR, BASE_AUX)
c_reg_2 <- c_reg_2 %>% dplyr::mutate(URBANORURALUY = "RURAL DISPERSO")
colnames(c_reg_1) <- colnames(BASE_MOTOR)
colnames(c_reg_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_reg_1,c_reg_2)


# Sexo 
b_sexo <- function(y) {
  base <- subset(ech_svy_1, bc_pe2 == y) 
  resultados  <-  sapply(base$variables %>% select(all_of(var_int)), function(x){
    base %>%
      srvyr::summarise(stat1 = srvyr::survey_mean(x, na.rm = TRUE))
    
  })
  y <- as.numeric(resultados[1,])
}

b_e_sexo = matrix(, nrow = 5, ncol = 2)

for(i in 1:2){
  b_e_sexo[,i] <- b_sexo(y = i)
}

c_sexo <- as.data.frame(b_e_sexo)
c_sexo_1 <- cbind(as.data.frame(c_sexo[,1]), NOMINDICADOR, BASE_AUX)
c_sexo_1 <- c_sexo_1 %>% dplyr::mutate(SEXO = "VARONES")
c_sexo_2 <- cbind(as.data.frame(c_sexo[,2]), NOMINDICADOR, BASE_AUX)
c_sexo_2 <- c_sexo_2 %>% dplyr::mutate(SEXO = "MUJERES")
colnames(c_sexo_1) <- colnames(BASE_MOTOR)
colnames(c_sexo_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_sexo_1,c_sexo_2)


# Tramo de edad

b_edad <- function(y) {
  base <- subset(ech_svy_1, tramo_edad2 == y) 
  resultados  <-  sapply(base$variables %>% select(all_of(var_int)), function(x){
    base %>%
      srvyr::summarise(stat1 = srvyr::survey_mean(x, na.rm = TRUE))
    
  })
  y <- as.numeric(resultados[1,])
}

b_e_edad = matrix(, nrow = 5, ncol = 4)

for(i in 1:4){
  b_e_edad[,i] <- b_edad(y = i)
}

c_edad <- as.data.frame(b_e_edad)
c_edad_1 <- cbind(as.data.frame(c_edad[,1]), NOMINDICADOR, BASE_AUX)
c_edad_1 <- c_edad_1 %>% dplyr::mutate(EDAD = "Menor de 30 años")
c_edad_2 <- cbind(as.data.frame(c_edad[,2]), NOMINDICADOR, BASE_AUX)
c_edad_2 <- c_edad_2 %>% dplyr::mutate(EDAD = "Entre 30 y 44 años")
c_edad_3 <- cbind(as.data.frame(c_edad[,3]), NOMINDICADOR, BASE_AUX)
c_edad_3 <- c_edad_3 %>% dplyr::mutate(EDAD = "Entre 45 y 64 años")
c_edad_4 <- cbind(as.data.frame(c_edad[,4]), NOMINDICADOR, BASE_AUX)
c_edad_4 <- c_edad_4 %>% dplyr::mutate(EDAD = "65 años o más")
colnames(c_edad_1) <- colnames(BASE_MOTOR)
colnames(c_edad_2) <- colnames(BASE_MOTOR)
colnames(c_edad_3) <- colnames(BASE_MOTOR)
colnames(c_edad_4) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_edad_1,c_edad_2, c_edad_3, c_edad_4)



# Ascendencia

b_ascen <- function(y) {
  base <- subset(ech_svy_1, bd_e29_1 == y) 
  resultados  <-  sapply(base$variables %>% select(all_of(var_int)), function(x){
    base %>%
      srvyr::summarise(stat1 = srvyr::survey_mean(x, na.rm = TRUE))
    
  })
  y <- as.numeric(resultados[1,])
}

b_e_ascen = matrix(, nrow = 5, ncol = 2)

for(i in 1:2){
  b_e_ascen[,i] <- b_ascen(y = i)
}

c_ascen <- as.data.frame(b_e_ascen)
c_ascen_1 <- cbind(as.data.frame(c_ascen[,1]), NOMINDICADOR, BASE_AUX)
c_ascen_1 <- c_ascen_1 %>% dplyr::mutate(ASCENDENCIA = "AFRO")
c_ascen_2 <- cbind(as.data.frame(c_ascen[,2]), NOMINDICADOR, BASE_AUX)
c_ascen_2 <- c_ascen_2 %>% dplyr::mutate(ASCENDENCIA = "NO AFRO")
colnames(c_ascen_1) <- colnames(BASE_MOTOR)
colnames(c_ascen_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_ascen_1,c_ascen_2)



# Quintil de ingreso del hogar
b_quintil <- function(y) {
  base <- subset(ech_svy_1, quintilesy == y) 
  resultados  <-  sapply(base$variables %>% select(all_of(var_int)), function(x){
    base %>%
      srvyr::summarise(stat1 = srvyr::survey_mean(x, na.rm = TRUE))
    
  })
  y <- as.numeric(resultados[1,])
}

b_e_quintil = matrix(, nrow = 5, ncol = 5)

for(i in 1:5){
  b_e_quintil[,i] <- b_quintil(y = i)
}

c_quintil <- as.data.frame(b_e_quintil)
c_quintil_1 <- cbind(as.data.frame(c_quintil[,1]), NOMINDICADOR, BASE_AUX)
c_quintil_1 <- c_quintil_1 %>% dplyr::mutate(QUINTIL = "Quintil 1")
c_quintil_2 <- cbind(as.data.frame(c_quintil[,2]), NOMINDICADOR, BASE_AUX)
c_quintil_2 <- c_quintil_2 %>% dplyr::mutate(QUINTIL = "Quintil 2")
c_quintil_3 <- cbind(as.data.frame(c_quintil[,3]), NOMINDICADOR, BASE_AUX)
c_quintil_3 <- c_quintil_3 %>% dplyr::mutate(QUINTIL = "Quintil 3")
c_quintil_4 <- cbind(as.data.frame(c_quintil[,4]), NOMINDICADOR, BASE_AUX)
c_quintil_4 <- c_quintil_4 %>% dplyr::mutate(QUINTIL = "Quintil 4")
c_quintil_5 <- cbind(as.data.frame(c_quintil[,5]), NOMINDICADOR, BASE_AUX)
c_quintil_5 <- c_quintil_5 %>% dplyr::mutate(QUINTIL = "Quintil 5")
colnames(c_quintil_1) <- colnames(BASE_MOTOR)
colnames(c_quintil_2) <- colnames(BASE_MOTOR)
colnames(c_quintil_3) <- colnames(BASE_MOTOR)
colnames(c_quintil_4) <- colnames(BASE_MOTOR)
colnames(c_quintil_5) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_quintil_1,c_quintil_2, c_quintil_3, c_quintil_4, c_quintil_5)


# Pobreza

b_pobre <- function(y) {
  base <- subset(ech_svy_1, pobre_aux == y) 
  resultados  <-  sapply(base$variables %>% select(all_of(var_int)), function(x){
    base %>%
      srvyr::summarise(stat1 = srvyr::survey_mean(x, na.rm = TRUE))
    
  })
  y <- as.numeric(resultados[1,])
}

b_e_pobre = matrix(, nrow = 5, ncol = 2)

for(i in 1:2){
  b_e_pobre[,i] <- b_pobre(y = i)
}

c_pobre <- as.data.frame(b_e_pobre)
c_pobre_1 <- cbind(as.data.frame(c_pobre[,1]), NOMINDICADOR, BASE_AUX)
c_pobre_1 <- c_pobre_1 %>% dplyr::mutate(POBRE = "POBRE")
c_pobre_2 <- cbind(as.data.frame(c_pobre[,2]), NOMINDICADOR, BASE_AUX)
c_pobre_2 <- c_pobre_2 %>% dplyr::mutate(POBRE = "NO POBRE")
colnames(c_pobre_1) <- colnames(BASE_MOTOR)
colnames(c_pobre_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_pobre_1,c_pobre_2)


# Base motor

BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(CODIND = "505")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(CATEGORIA = "Composición y estructura de los hogares")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(PESTAÑA = "Total país")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(PAÍS = "Uruguay")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(ANIO = 2023)
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(RESPONSABLE = "Sharon Katzkowicz")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(DEPARTAMENTOUY = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(NSE = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(NIVELEDU = "Todos")
BASE_MOTOR <- as.data.frame(BASE_MOTOR)


BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(ASCENDENCIA = case_when(ASCENDENCIA == "AFRO" ~ "Afro", 
                                                                   ASCENDENCIA == "NO AFRO" ~ "No afro",
                                                                   ASCENDENCIA == "" ~ "Todos"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(URBANORURALUY = case_when(URBANORURALUY == "URBANO (MENOS DE 5.000 HABITANTES)" ~ "Urbano (menos de 5.000 habitantes)",
                                                                     URBANORURALUY == "RURAL DISPERSO" ~ "Rural disperso", 
                                                                     URBANORURALUY == "" ~ "Total país"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(SEXO = case_when(SEXO == "MUJERES" ~ "Mujeres", 
                                                            SEXO=="VARONES" ~ "Varones",
                                                            SEXO == "" ~ "Todos"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(QUINTIL = case_when(QUINTIL == "Quintil 1"~"Quintil 1", 
                                                               QUINTIL == "Quintil 2"~"Quintil 2",
                                                               QUINTIL == "Quintil 3"~"Quintil 3", 
                                                               QUINTIL == "Quintil 4"~"Quintil 4",
                                                               QUINTIL == "Quintil 5"~"Quintil 5",
                                                               QUINTIL == "" ~ "Todos"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(EDAD = case_when(EDAD == "Menor de 30 años"~"Menor de 30 años", 
                                                            EDAD == "Entre 30 y 44 años"~"Entre 30 y 44 años",
                                                            EDAD == "Entre 45 y 64 años"~"Entre 45 y 64 años", 
                                                            EDAD == "65 años o más"~"65 años o más",
                                                            EDAD == "" ~ "Todos"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(POBRE = case_when(POBRE == "POBRE" ~ "Pobre", 
                                                             POBRE == "NO POBRE" ~ "No pobre",
                                                             POBRE == "" ~ "Todos"))


m505_tp <- BASE_MOTOR %>% select(CODIND,	
                                 NOMINDICADOR,	
                                 CATEGORIA,	
                                 PESTAÑA,	
                                 SEXO,	
                                 ASCENDENCIA,	
                                 QUINTIL,	
                                 DEPARTAMENTOUY,	
                                 URBANORURALUY,	
                                 EDAD,	
                                 POBRE,
                                 NSE,
                                 NIVELEDU,
                                 PAÍS,	
                                 ANIO,	
                                 VALOR,	
                                 RESPONSABLE)




### 508 Distribución porcentual de los hogares según sexo del jefe/a del hogar


## País Urbano

ech_h_svy_1 <- ech_h_svy %>%
  filter(bd_region == 1)

# Total

NOMINDICADOR <- 
  c("Distribución porcentual de los hogares según sexo del jefe/a (Varón)",
    "Distribución porcentual de los hogares según sexo del jefe/a (Mujer)")


var_int <- c("sexojefe1", "sexojefe2")
a_sem <- sapply(ech_h_svy_1$variables %>% select(all_of(var_int)), function(x){
  ech_h_svy_1 %>%
    srvyr::summarise(stat = srvyr::survey_mean(x, na.rm = TRUE)) 
})

c_ano <- matrix(,2,1)
for (i in 1:2) {
  c_ano[i,1] <- c(as.numeric(c(a_sem[1,i])))
}

colnames(c_ano) <- c("VALOR")
c_ano <- cbind(c_ano, NOMINDICADOR)
c_ano <- as.data.frame(c_ano)
BASE_MOTOR <- cbind(c_ano,BASE_AUX)

# Tramo de edad

a_edad <- function(y) {
  base <- subset(ech_h_svy_1, tramo_edad2 == y) 
  resultados  <-  sapply(base$variables %>% select(all_of(var_int)), function(x){
    base %>%
      srvyr::summarise(stat1 = srvyr::survey_mean(x, na.rm = TRUE))
    
  })
  y <- as.numeric(resultados[1,])
}

a_e_edad = matrix(, nrow = 2, ncol = 4)

for(i in 1:4){
  a_e_edad[,i] <- a_edad(y = i)
}


c_edad <- as.data.frame(a_e_edad)

c_edad_1 <- cbind(as.data.frame(c_edad[,1]), NOMINDICADOR, BASE_AUX)
c_edad_1 <- c_edad_1 %>% dplyr::mutate(EDAD = "Menor de 30 años")
c_edad_2 <- cbind(as.data.frame(c_edad[,2]), NOMINDICADOR, BASE_AUX)
c_edad_2 <- c_edad_2 %>% dplyr::mutate(EDAD = "Entre 30 y 44 años")
c_edad_3 <- cbind(as.data.frame(c_edad[,3]), NOMINDICADOR, BASE_AUX)
c_edad_3 <- c_edad_3 %>% dplyr::mutate(EDAD = "Entre 45 y 64 años")
c_edad_4 <- cbind(as.data.frame(c_edad[,4]), NOMINDICADOR, BASE_AUX)
c_edad_4 <- c_edad_4 %>% dplyr::mutate(EDAD = "65 años o más")
colnames(c_edad_1) <- colnames(BASE_MOTOR)
colnames(c_edad_2) <- colnames(BASE_MOTOR)
colnames(c_edad_3) <- colnames(BASE_MOTOR)
colnames(c_edad_4) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_edad_1,c_edad_2, c_edad_3, c_edad_4)


# Quintil de ingreso del hogar
a_quintil <- function(y) {
  base <- subset(ech_h_svy_1, quintilesy == y) 
  resultados  <-  sapply(base$variables %>% select(all_of(var_int)), function(x){
    base %>%
      srvyr::summarise(stat1 = srvyr::survey_mean(x, na.rm = TRUE))
    
  })
  y <- as.numeric(resultados[1,])
}

a_e_quintil = matrix(, nrow = 2, ncol = 5)

for(i in 1:5){
  a_e_quintil[,i] <- a_quintil(y = i)
}

c_quintil <- as.data.frame(a_e_quintil)

c_quintil_1 <- cbind(as.data.frame(c_quintil[,1]), NOMINDICADOR, BASE_AUX)
c_quintil_1 <- c_quintil_1 %>% dplyr::mutate(QUINTIL = "Quintil 1")
c_quintil_2 <- cbind(as.data.frame(c_quintil[,2]), NOMINDICADOR, BASE_AUX)
c_quintil_2 <- c_quintil_2 %>% dplyr::mutate(QUINTIL = "Quintil 2")
c_quintil_3 <- cbind(as.data.frame(c_quintil[,3]), NOMINDICADOR, BASE_AUX)
c_quintil_3 <- c_quintil_3 %>% dplyr::mutate(QUINTIL = "Quintil 3")
c_quintil_4 <- cbind(as.data.frame(c_quintil[,4]), NOMINDICADOR, BASE_AUX)
c_quintil_4 <- c_quintil_4 %>% dplyr::mutate(QUINTIL = "Quintil 4")
c_quintil_5 <- cbind(as.data.frame(c_quintil[,5]), NOMINDICADOR, BASE_AUX)
c_quintil_5 <- c_quintil_5 %>% dplyr::mutate(QUINTIL = "Quintil 5")
colnames(c_quintil_1) <- colnames(BASE_MOTOR)
colnames(c_quintil_2) <- colnames(BASE_MOTOR)
colnames(c_quintil_3) <- colnames(BASE_MOTOR)
colnames(c_quintil_4) <- colnames(BASE_MOTOR)
colnames(c_quintil_5) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_quintil_1,c_quintil_2, c_quintil_3, c_quintil_4, c_quintil_5)




# Base motor

BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(CODIND = "508")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(CATEGORIA = "Composición y estructura de los hogares")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(PESTAÑA = "País urbano")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(PAÍS = "Uruguay")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(ANIO = 2023)
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(RESPONSABLE = "Sharon Katzkowicz")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(ASCENDENCIA = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(DEPARTAMENTOUY = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(URBANORURALUY = "Urbano (más de 5.000 habitantes)")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(POBRE = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(NSE = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(NIVELEDU = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(SEXO = "Todos")
BASE_MOTOR <- as.data.frame(BASE_MOTOR)

BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(QUINTIL = case_when(QUINTIL == "" ~ "Todos",
                                                               QUINTIL == "Quintil 1"~"Quintil 1", 
                                                               QUINTIL == "Quintil 2"~"Quintil 2",
                                                               QUINTIL == "Quintil 3"~"Quintil 3", 
                                                               QUINTIL == "Quintil 4"~"Quintil 4",
                                                               QUINTIL == "Quintil 5"~"Quintil 5"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(EDAD = case_when(EDAD == "" ~ "Todos",
                                                            EDAD == "Menor de 30 años"~"Menor de 30 años", 
                                                            EDAD == "Entre 30 y 44 años"~"Entre 30 y 44 años",
                                                            EDAD == "Entre 45 y 64 años"~"Entre 45 y 64 años", 
                                                            EDAD == "65 años o más"~"65 años o más"))


m508_pu <- BASE_MOTOR %>% select(CODIND,	
                                 NOMINDICADOR,	
                                 CATEGORIA,	
                                 PESTAÑA,	
                                 SEXO,	
                                 ASCENDENCIA,	
                                 QUINTIL,	
                                 DEPARTAMENTOUY,	
                                 URBANORURALUY,	
                                 EDAD,	
                                 POBRE,
                                 NSE,
                                 NIVELEDU,
                                 PAÍS,	
                                 ANIO,	
                                 VALOR,	
                                 RESPONSABLE)	


## Total país

# Total

NOMINDICADOR <- 
  c("Distribución porcentual de los hogares según sexo del jefe/a (Varón)",
    "Distribución porcentual de los hogares según sexo del jefe/a (Mujer)")


var_int <- c("sexojefe1", "sexojefe2")
a_sem <- sapply(ech_h_svy$variables %>% select(all_of(var_int)), function(x){
  ech_h_svy %>%
    srvyr::summarise(stat = srvyr::survey_mean(x, na.rm = TRUE)) 
})

b_sem <- sapply(ech_h_svy$variables %>% select(all_of(var_int)), function(x){
  ech_h_svy %>%
    srvyr::summarise(stat = srvyr::survey_mean(x, na.rm = TRUE)) 
})

c_ano <- matrix(,2,1)
for (i in 1:2) {
  c_ano[i,1] <- c(as.numeric(c(a_sem[1,i])))
}

colnames(c_ano) <- c("VALOR")
c_ano <- cbind(c_ano, NOMINDICADOR)
c_ano <- as.data.frame(c_ano)
BASE_MOTOR <- cbind(c_ano,BASE_AUX)


# Región

a_reg <- function(y) {
  base <- subset(ech_h_svy, bd_region == y) 
  resultados  <-  sapply(base$variables %>% select(all_of(var_int)), function(x){
    base %>%
      srvyr::summarise(stat1 = srvyr::survey_mean(x, na.rm = TRUE))
    
  })
  y <- as.numeric(resultados[1,])
}

a_e_reg = matrix(, nrow = 2, ncol = 3)

for(i in 1:3){
  a_e_reg[,i] <- a_reg(y = i)
}


c_reg <- as.data.frame(a_e_reg[,2:3])
c_reg_1 <- cbind(as.data.frame(c_reg[,1]), NOMINDICADOR, BASE_AUX)
c_reg_1 <- c_reg_1 %>% dplyr::mutate(URBANORURALUY = "URBANO (MENOS DE 5.000 HABITANTES)")
c_reg_2 <- cbind(as.data.frame(c_reg[,2]), NOMINDICADOR, BASE_AUX)
c_reg_2 <- c_reg_2 %>% dplyr::mutate(URBANORURALUY = "RURAL DISPERSO")
colnames(c_reg_1) <- colnames(BASE_MOTOR)
colnames(c_reg_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_reg_1,c_reg_2)



# Tramo de edad

a_edad <- function(y) {
  base <- subset(ech_h_svy, tramo_edad2 == y) 
  resultados  <-  sapply(base$variables %>% select(all_of(var_int)), function(x){
    base %>%
      srvyr::summarise(stat1 = srvyr::survey_mean(x, na.rm = TRUE))
    
  })
  y <- as.numeric(resultados[1,])
}

a_e_edad = matrix(, nrow = 2, ncol = 4)

for(i in 1:4){
  a_e_edad[,i] <- a_edad(y = i)
}

c_edad <- as.data.frame(a_e_edad)

c_edad_1 <- cbind(as.data.frame(c_edad[,1]), NOMINDICADOR, BASE_AUX)
c_edad_1 <- c_edad_1 %>% dplyr::mutate(EDAD = "Menor de 30 años")
c_edad_2 <- cbind(as.data.frame(c_edad[,2]), NOMINDICADOR, BASE_AUX)
c_edad_2 <- c_edad_2 %>% dplyr::mutate(EDAD = "Entre 30 y 44 años")
c_edad_3 <- cbind(as.data.frame(c_edad[,3]), NOMINDICADOR, BASE_AUX)
c_edad_3 <- c_edad_3 %>% dplyr::mutate(EDAD = "Entre 45 y 64 años")
c_edad_4 <- cbind(as.data.frame(c_edad[,4]), NOMINDICADOR, BASE_AUX)
c_edad_4 <- c_edad_4 %>% dplyr::mutate(EDAD = "65 años o más")
colnames(c_edad_1) <- colnames(BASE_MOTOR)
colnames(c_edad_2) <- colnames(BASE_MOTOR)
colnames(c_edad_3) <- colnames(BASE_MOTOR)
colnames(c_edad_4) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_edad_1,c_edad_2, c_edad_3, c_edad_4)



# Ascendencia

a_ascen <- function(y) {
  base <- subset(ech_h_svy, bd_e29_1 == y) 
  resultados  <-  sapply(base$variables %>% select(all_of(var_int)), function(x){
    base %>%
      srvyr::summarise(stat1 = srvyr::survey_mean(x, na.rm = TRUE))
    
  })
  y <- as.numeric(resultados[1,])
}

a_e_ascen = matrix(, nrow = 2, ncol = 2)

for(i in 1:2){
  a_e_ascen[,i] <- a_ascen(y = i)
}

c_ascen <- as.data.frame(a_e_ascen)
c_ascen_1 <- cbind(as.data.frame(c_ascen[,1]), NOMINDICADOR, BASE_AUX)
c_ascen_1 <- c_ascen_1 %>% dplyr::mutate(ASCENDENCIA = "AFRO")
c_ascen_2 <- cbind(as.data.frame(c_ascen[,2]), NOMINDICADOR, BASE_AUX)
c_ascen_2 <- c_ascen_2 %>% dplyr::mutate(ASCENDENCIA = "NO AFRO")
colnames(c_ascen_1) <- colnames(BASE_MOTOR)
colnames(c_ascen_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_ascen_1,c_ascen_2)





# Quintil de ingreso del hogar
a_quintil <- function(y) {
  base <- subset(ech_h_svy, quintilesy == y) 
  resultados  <-  sapply(base$variables %>% select(all_of(var_int)), function(x){
    base %>%
      srvyr::summarise(stat1 = srvyr::survey_mean(x, na.rm = TRUE))
    
  })
  y <- as.numeric(resultados[1,])
}

a_e_quintil = matrix(, nrow = 2, ncol = 5)

for(i in 1:5){
  a_e_quintil[,i] <- a_quintil(y = i)
}


c_quintil <- as.data.frame(a_e_quintil)

c_quintil_1 <- cbind(as.data.frame(c_quintil[,1]), NOMINDICADOR, BASE_AUX)
c_quintil_1 <- c_quintil_1 %>% dplyr::mutate(QUINTIL = "Quintil 1")
c_quintil_2 <- cbind(as.data.frame(c_quintil[,2]), NOMINDICADOR, BASE_AUX)
c_quintil_2 <- c_quintil_2 %>% dplyr::mutate(QUINTIL = "Quintil 2")
c_quintil_3 <- cbind(as.data.frame(c_quintil[,3]), NOMINDICADOR, BASE_AUX)
c_quintil_3 <- c_quintil_3 %>% dplyr::mutate(QUINTIL = "Quintil 3")
c_quintil_4 <- cbind(as.data.frame(c_quintil[,4]), NOMINDICADOR, BASE_AUX)
c_quintil_4 <- c_quintil_4 %>% dplyr::mutate(QUINTIL = "Quintil 4")
c_quintil_5 <- cbind(as.data.frame(c_quintil[,5]), NOMINDICADOR, BASE_AUX)
c_quintil_5 <- c_quintil_5 %>% dplyr::mutate(QUINTIL = "Quintil 5")
colnames(c_quintil_1) <- colnames(BASE_MOTOR)
colnames(c_quintil_2) <- colnames(BASE_MOTOR)
colnames(c_quintil_3) <- colnames(BASE_MOTOR)
colnames(c_quintil_4) <- colnames(BASE_MOTOR)
colnames(c_quintil_5) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_quintil_1,c_quintil_2, c_quintil_3, c_quintil_4, c_quintil_5)


# Pobreza

a_pobre <- function(y) {
  base <- subset(ech_h_svy, pobre_aux == y) 
  resultados  <-  sapply(base$variables %>% select(all_of(var_int)), function(x){
    base %>%
      srvyr::summarise(stat1 = srvyr::survey_mean(x, na.rm = TRUE))
    
  })
  y <- as.numeric(resultados[1,])
}

a_e_pobre = matrix(, nrow = 2, ncol = 2)

for(i in 1:2){
  a_e_pobre[,i] <- a_pobre(y = i)
}


c_pobre <- as.data.frame(a_e_pobre)
c_pobre_1 <- cbind(as.data.frame(c_pobre[,1]), NOMINDICADOR, BASE_AUX)
c_pobre_1 <- c_pobre_1 %>% dplyr::mutate(POBRE = "POBRE")
c_pobre_2 <- cbind(as.data.frame(c_pobre[,2]), NOMINDICADOR, BASE_AUX)
c_pobre_2 <- c_pobre_2 %>% dplyr::mutate(POBRE = "NO POBRE")
colnames(c_pobre_1) <- colnames(BASE_MOTOR)
colnames(c_pobre_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_pobre_1,c_pobre_2)


# Base motor

BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(CODIND = "508")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(CATEGORIA = "Composición y estructura de los hogares")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(PESTAÑA = "Total país")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(PAÍS = "Uruguay")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(ANIO = 2023)
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(RESPONSABLE = "Sharon Katzkowicz")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(DEPARTAMENTOUY = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(NSE = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(NIVELEDU = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(SEXO = "Todos")
BASE_MOTOR <- as.data.frame(BASE_MOTOR)


BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(ASCENDENCIA = case_when(ASCENDENCIA == "AFRO" ~ "Afro", 
                                                                   ASCENDENCIA == "NO AFRO" ~ "No afro",
                                                                   ASCENDENCIA == "" ~ "Todos"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(URBANORURALUY = case_when(URBANORURALUY == "URBANO (MENOS DE 5.000 HABITANTES)" ~ "Urbano (menos de 5.000 habitantes)",
                                                                     URBANORURALUY == "RURAL DISPERSO" ~ "Rural disperso", 
                                                                     URBANORURALUY == "" ~ "Total país"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(QUINTIL = case_when(QUINTIL == "Quintil 1"~"Quintil 1", 
                                                               QUINTIL == "Quintil 2"~"Quintil 2",
                                                               QUINTIL == "Quintil 3"~"Quintil 3", 
                                                               QUINTIL == "Quintil 4"~"Quintil 4",
                                                               QUINTIL == "Quintil 5"~"Quintil 5",
                                                               QUINTIL == "" ~ "Todos"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(EDAD = case_when(EDAD == "Menor de 30 años"~"Menor de 30 años", 
                                                            EDAD == "Entre 30 y 44 años"~"Entre 30 y 44 años",
                                                            EDAD == "Entre 45 y 64 años"~"Entre 45 y 64 años", 
                                                            EDAD == "65 años o más"~"65 años o más",
                                                            EDAD == "" ~ "Todos"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(POBRE = case_when(POBRE == "POBRE" ~ "Pobre", 
                                                             POBRE == "NO POBRE" ~ "No pobre",
                                                             POBRE == "" ~ "Todos"))


m508_tp <- BASE_MOTOR %>% select(CODIND,	
                                 NOMINDICADOR,	
                                 CATEGORIA,	
                                 PESTAÑA,	
                                 SEXO,	
                                 ASCENDENCIA,	
                                 QUINTIL,	
                                 DEPARTAMENTOUY,	
                                 URBANORURALUY,	
                                 EDAD,	
                                 POBRE,
                                 NSE,
                                 NIVELEDU,
                                 PAÍS,	
                                 ANIO,	
                                 VALOR,	
                                 RESPONSABLE)




### 509 Porcentaje de hogares en los que residen menores entre 0 y 4 años de edad

ech_h_svy <- ech_h_svy %>% dplyr::mutate(menorH=men0a4h)


## País Urbano

# Total

NOMINDICADOR <- 
  c("Porcentaje de hogares en los que residen menores entre 0 y 4 años de edad")

a_mes <- ech_h_svy %>%
  srvyr::filter(bd_region == 1) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(menorH))
a_sem <- mean(as.numeric(a_mes$colname))

c_ano <- as.numeric(a_sem)

c_ano <- cbind(c_ano, NOMINDICADOR)
colnames(c_ano) <- c("VALOR", "NOMINDICADOR")
c_ano <- as.data.frame(c_ano)
BASE_MOTOR <- cbind(c_ano,BASE_AUX)


# Sexo del jefe

a_jefe <- function(x) {
  x <- ech_h_svy %>%
    filter(sexojefe == x & bd_region == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(menorH))
  x <- mean(x$colname)
}       

a_e_jefe <- numeric()

for(i in 1:2){
  a_e_jefe[i] <- a_jefe(x = i)
}     

c_jefe <- as.data.frame(a_e_jefe)
c_jefe_1 <- cbind(as.data.frame(c_jefe[1,1]), NOMINDICADOR, BASE_AUX)
c_jefe_1 <- c_jefe_1 %>% dplyr::mutate(SEXO = "VARONES")
c_jefe_2 <- cbind(as.data.frame(c_jefe[2,1]), NOMINDICADOR, BASE_AUX)
c_jefe_2 <- c_jefe_2 %>% dplyr::mutate(SEXO = "MUJERES")
colnames(c_jefe_1) <- colnames(BASE_MOTOR)
colnames(c_jefe_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_jefe_1,c_jefe_2)


# Tramo de edad


a_edad <- function(x) {
  x <- ech_h_svy %>%
    filter(tramo_edad2 == x & bd_region == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(menorH))
  x <- mean(x$colname)
}       

a_e_edad <- numeric()

for(i in 1:4){
  a_e_edad[i] <- a_edad(x = i)
}     

c_edad <- as.data.frame(a_e_edad)
c_edad_1 <- cbind(as.data.frame(c_edad[1,1]), NOMINDICADOR, BASE_AUX)
c_edad_1 <- c_edad_1 %>% dplyr::mutate(EDAD = "Menor de 30 años")
c_edad_2 <- cbind(as.data.frame(c_edad[2,1]), NOMINDICADOR, BASE_AUX)
c_edad_2 <- c_edad_2 %>% dplyr::mutate(EDAD = "Entre 30 y 44 años")
c_edad_3 <- cbind(as.data.frame(c_edad[3,1]), NOMINDICADOR, BASE_AUX)
c_edad_3 <- c_edad_3 %>% dplyr::mutate(EDAD = "Entre 45 y 64 años")
c_edad_4 <- cbind(as.data.frame(c_edad[4,1]), NOMINDICADOR, BASE_AUX)
c_edad_4 <- c_edad_4 %>% dplyr::mutate(EDAD = "65 años o más")
colnames(c_edad_1) <- colnames(BASE_MOTOR)
colnames(c_edad_2) <- colnames(BASE_MOTOR)
colnames(c_edad_3) <- colnames(BASE_MOTOR)
colnames(c_edad_4) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_edad_1,c_edad_2, c_edad_3, c_edad_4)


# Quintil de ingreso del hogar

a_quintil <- function(x) {
  x <- ech_h_svy %>%
    filter(quintilesy == x & bd_region == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(menorH))
  x <- mean(x$colname)
}       

a_e_quintil <- numeric()

for(i in 1:5){
  a_e_quintil[i] <- a_quintil(x = i)
}     

c_quintil <- as.data.frame(a_e_quintil)
c_quintil_1 <- cbind(as.data.frame(c_quintil[1,1]), NOMINDICADOR, BASE_AUX)
c_quintil_1 <- c_quintil_1 %>% dplyr::mutate(QUINTIL = "Quintil 1")
c_quintil_2 <- cbind(as.data.frame(c_quintil[2,1]), NOMINDICADOR, BASE_AUX)
c_quintil_2 <- c_quintil_2 %>% dplyr::mutate(QUINTIL = "Quintil 2")
c_quintil_3 <- cbind(as.data.frame(c_quintil[3,1]), NOMINDICADOR, BASE_AUX)
c_quintil_3 <- c_quintil_3 %>% dplyr::mutate(QUINTIL = "Quintil 3")
c_quintil_4 <- cbind(as.data.frame(c_quintil[4,1]), NOMINDICADOR, BASE_AUX)
c_quintil_4 <- c_quintil_4 %>% dplyr::mutate(QUINTIL = "Quintil 4")
c_quintil_5 <- cbind(as.data.frame(c_quintil[5,1]), NOMINDICADOR, BASE_AUX)
c_quintil_5 <- c_quintil_5 %>% dplyr::mutate(QUINTIL = "Quintil 5")
colnames(c_quintil_1) <- colnames(BASE_MOTOR)
colnames(c_quintil_2) <- colnames(BASE_MOTOR)
colnames(c_quintil_3) <- colnames(BASE_MOTOR)
colnames(c_quintil_4) <- colnames(BASE_MOTOR)
colnames(c_quintil_5) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_quintil_1,c_quintil_2, c_quintil_3, c_quintil_4, c_quintil_5)




# Base motor

BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(CODIND = "509")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(CATEGORIA = "Composición y estructura de los hogares")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(PESTAÑA = "País urbano")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(PAÍS = "Uruguay")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(ANIO = 2023)
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(RESPONSABLE = "Sharon Katzkowicz")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(ASCENDENCIA = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(DEPARTAMENTOUY = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(URBANORURALUY = "Urbano (más de 5.000 habitantes)")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(POBRE = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(NSE = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(NIVELEDU = "Todos")
BASE_MOTOR <- as.data.frame(BASE_MOTOR)

BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(SEXO = case_when(SEXO == "" ~ "Todos", SEXO == "MUJERES"~"Mujeres", SEXO=="VARONES"~"Varones"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(QUINTIL = case_when(QUINTIL == "" ~ "Todos",
                                                               QUINTIL == "Quintil 1"~"Quintil 1", 
                                                               QUINTIL == "Quintil 2"~"Quintil 2",
                                                               QUINTIL == "Quintil 3"~"Quintil 3", 
                                                               QUINTIL == "Quintil 4"~"Quintil 4",
                                                               QUINTIL == "Quintil 5"~"Quintil 5"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(EDAD = case_when(EDAD == "" ~ "Todos",
                                                            EDAD == "Menor de 30 años"~"Menor de 30 años", 
                                                            EDAD == "Entre 30 y 44 años"~"Entre 30 y 44 años",
                                                            EDAD == "Entre 45 y 64 años"~"Entre 45 y 64 años", 
                                                            EDAD == "65 años o más"~"65 años o más"))


m509_pu <- BASE_MOTOR %>% select(CODIND,	
                                 NOMINDICADOR,	
                                 CATEGORIA,	
                                 PESTAÑA,	
                                 SEXO,	
                                 ASCENDENCIA,	
                                 QUINTIL,	
                                 DEPARTAMENTOUY,	
                                 URBANORURALUY,	
                                 EDAD,	
                                 POBRE,
                                 NSE,
                                 NIVELEDU,
                                 PAÍS,	
                                 ANIO,	
                                 VALOR,	
                                 RESPONSABLE)	


## Total país

# Total


NOMINDICADOR <- 
  c("Porcentaje de hogares en los que residen menores entre 0 y 4 años de edad")

a_mes <- ech_h_svy %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(menorH))
a_sem <- mean(as.numeric(a_mes$colname))

c_ano <- as.numeric(a_sem)

c_ano <- cbind(c_ano, NOMINDICADOR)
colnames(c_ano) <- c("VALOR", "NOMINDICADOR")
c_ano <- as.data.frame(c_ano)
BASE_MOTOR <- cbind(c_ano,BASE_AUX)



# Región

a_reg <- function(x) {
  x <- ech_h_svy %>%
    filter(bd_region == x) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(menorH))
  x <- mean(x$colname)
}       

a_e_reg <- numeric()

for(i in 1:3){
  a_e_reg[i] <- a_reg(x = i)
}     

c_reg <- as.data.frame(a_e_reg)
c_reg_1 <- cbind(as.data.frame(c_reg[2,1]), NOMINDICADOR, BASE_AUX)
c_reg_1 <- c_reg_1 %>% dplyr::mutate(URBANORURALUY = "URBANO (MENOS DE 5.000 HABITANTES)")
c_reg_2 <- cbind(as.data.frame(c_reg[3,1]), NOMINDICADOR, BASE_AUX)
c_reg_2 <- c_reg_2 %>% dplyr::mutate(URBANORURALUY = "RURAL DISPERSO")
colnames(c_reg_1) <- colnames(BASE_MOTOR)
colnames(c_reg_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_reg_1,c_reg_2)


# Sexo del jefe

a_jefe <- function(x) {
  x <- ech_h_svy %>%
    filter(sexojefe == x) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(menorH))
  x <- mean(x$colname)
}       

a_e_jefe <- numeric()

for(i in 1:2){
  a_e_jefe[i] <- a_jefe(x = i)
}     

c_jefe <- as.data.frame(a_e_jefe)
c_jefe_1 <- cbind(as.data.frame(c_jefe[1,1]), NOMINDICADOR, BASE_AUX)
c_jefe_1 <- c_jefe_1 %>% dplyr::mutate(SEXO = "VARONES")
c_jefe_2 <- cbind(as.data.frame(c_jefe[2,1]), NOMINDICADOR, BASE_AUX)
c_jefe_2 <- c_jefe_2 %>% dplyr::mutate(SEXO = "MUJERES")
colnames(c_jefe_1) <- colnames(BASE_MOTOR)
colnames(c_jefe_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_jefe_1,c_jefe_2)


# Tramo de edad

a_edad <- function(x) {
  x <- ech_h_svy %>%
    filter(tramo_edad2 == x) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(menorH))
  x <- mean(x$colname)
}       

a_e_edad <- numeric()

for(i in 1:4){
  a_e_edad[i] <- a_edad(x = i)
}     


c_edad <- as.data.frame(a_e_edad)
c_edad_1 <- cbind(as.data.frame(c_edad[1,1]), NOMINDICADOR, BASE_AUX)
c_edad_1 <- c_edad_1 %>% dplyr::mutate(EDAD = "Menor de 30 años")
c_edad_2 <- cbind(as.data.frame(c_edad[2,1]), NOMINDICADOR, BASE_AUX)
c_edad_2 <- c_edad_2 %>% dplyr::mutate(EDAD = "Entre 30 y 44 años")
c_edad_3 <- cbind(as.data.frame(c_edad[3,1]), NOMINDICADOR, BASE_AUX)
c_edad_3 <- c_edad_3 %>% dplyr::mutate(EDAD = "Entre 45 y 64 años")
c_edad_4 <- cbind(as.data.frame(c_edad[4,1]), NOMINDICADOR, BASE_AUX)
c_edad_4 <- c_edad_4 %>% dplyr::mutate(EDAD = "65 años o más")
colnames(c_edad_1) <- colnames(BASE_MOTOR)
colnames(c_edad_2) <- colnames(BASE_MOTOR)
colnames(c_edad_3) <- colnames(BASE_MOTOR)
colnames(c_edad_4) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_edad_1,c_edad_2, c_edad_3, c_edad_4)



# Ascendencia

a_ascen <- function(x) {
  x <- ech_h_svy %>%
    filter(bd_e29_1 == x) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(menorH))
  x <- mean(x$colname)
}       

a_e_ascen <- numeric()

for(i in 1:2){
  a_e_ascen[i] <- a_ascen(x = i)
}     

c_ascen <- as.data.frame(a_e_ascen)
c_ascen_1 <- cbind(as.data.frame(c_ascen[1,1]), NOMINDICADOR, BASE_AUX)
c_ascen_1 <- c_ascen_1 %>% dplyr::mutate(ASCENDENCIA = "AFRO")
c_ascen_2 <- cbind(as.data.frame(c_ascen[2,1]), NOMINDICADOR, BASE_AUX)
c_ascen_2 <- c_ascen_2 %>% dplyr::mutate(ASCENDENCIA = "NO AFRO")
colnames(c_ascen_1) <- colnames(BASE_MOTOR)
colnames(c_ascen_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_ascen_1,c_ascen_2)


# Quintil de ingreso del hogar

a_quintil <- function(x) {
  x <- ech_h_svy %>%
    filter(quintilesy == x) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(menorH))
  x <- mean(x$colname)
}       

a_e_quintil <- numeric()

for(i in 1:5){
  a_e_quintil[i] <- a_quintil(x = i)
}     


c_quintil <- as.data.frame(a_e_quintil)
c_quintil_1 <- cbind(as.data.frame(c_quintil[1,1]), NOMINDICADOR, BASE_AUX)
c_quintil_1 <- c_quintil_1 %>% dplyr::mutate(QUINTIL = "Quintil 1")
c_quintil_2 <- cbind(as.data.frame(c_quintil[2,1]), NOMINDICADOR, BASE_AUX)
c_quintil_2 <- c_quintil_2 %>% dplyr::mutate(QUINTIL = "Quintil 2")
c_quintil_3 <- cbind(as.data.frame(c_quintil[3,1]), NOMINDICADOR, BASE_AUX)
c_quintil_3 <- c_quintil_3 %>% dplyr::mutate(QUINTIL = "Quintil 3")
c_quintil_4 <- cbind(as.data.frame(c_quintil[4,1]), NOMINDICADOR, BASE_AUX)
c_quintil_4 <- c_quintil_4 %>% dplyr::mutate(QUINTIL = "Quintil 4")
c_quintil_5 <- cbind(as.data.frame(c_quintil[5,1]), NOMINDICADOR, BASE_AUX)
c_quintil_5 <- c_quintil_5 %>% dplyr::mutate(QUINTIL = "Quintil 5")
colnames(c_quintil_1) <- colnames(BASE_MOTOR)
colnames(c_quintil_2) <- colnames(BASE_MOTOR)
colnames(c_quintil_3) <- colnames(BASE_MOTOR)
colnames(c_quintil_4) <- colnames(BASE_MOTOR)
colnames(c_quintil_5) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_quintil_1,c_quintil_2, c_quintil_3, c_quintil_4, c_quintil_5)


# Pobreza

a_pobre <- function(x) {
  x <- ech_h_svy %>%
    filter(pobre_aux == x) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(menorH))
  x <- mean(x$colname)
}       

a_e_pobre <- numeric()

for(i in 1:2){
  a_e_pobre[i] <- a_pobre(x = i)
}     

c_pobre <- as.data.frame(a_e_pobre)
c_pobre_1 <- cbind(as.data.frame(c_pobre[1,1]), NOMINDICADOR, BASE_AUX)
c_pobre_1 <- c_pobre_1 %>% dplyr::mutate(POBRE = "POBRE")
c_pobre_2 <- cbind(as.data.frame(c_pobre[2,1]), NOMINDICADOR, BASE_AUX)
c_pobre_2 <- c_pobre_2 %>% dplyr::mutate(POBRE = "NO POBRE")
colnames(c_pobre_1) <- colnames(BASE_MOTOR)
colnames(c_pobre_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_pobre_1,c_pobre_2)


# Base motor

BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(CODIND = "509")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(CATEGORIA = "Composición y estructura de los hogares")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(PESTAÑA = "Total país")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(PAÍS = "Uruguay")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(ANIO = 2023)
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(RESPONSABLE = "Sharon Katzkowicz")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(DEPARTAMENTOUY = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(NSE = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(NIVELEDU = "Todos")
BASE_MOTOR <- as.data.frame(BASE_MOTOR)


BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(ASCENDENCIA = case_when(ASCENDENCIA == "AFRO" ~ "Afro", 
                                                                   ASCENDENCIA == "NO AFRO" ~ "No afro",
                                                                   ASCENDENCIA == "" ~ "Todos"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(URBANORURALUY = case_when(URBANORURALUY == "URBANO (MENOS DE 5.000 HABITANTES)" ~ "Urbano (menos de 5.000 habitantes)",
                                                                     URBANORURALUY == "RURAL DISPERSO" ~ "Rural disperso", 
                                                                     URBANORURALUY == "" ~ "Total país"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(SEXO = case_when(SEXO == "MUJERES" ~ "Mujeres", 
                                                            SEXO=="VARONES" ~ "Varones",
                                                            SEXO == "" ~ "Todos"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(QUINTIL = case_when(QUINTIL == "Quintil 1"~"Quintil 1", 
                                                               QUINTIL == "Quintil 2"~"Quintil 2",
                                                               QUINTIL == "Quintil 3"~"Quintil 3", 
                                                               QUINTIL == "Quintil 4"~"Quintil 4",
                                                               QUINTIL == "Quintil 5"~"Quintil 5",
                                                               QUINTIL == "" ~ "Todos"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(EDAD = case_when(EDAD == "Menor de 30 años"~"Menor de 30 años", 
                                                            EDAD == "Entre 30 y 44 años"~"Entre 30 y 44 años",
                                                            EDAD == "Entre 45 y 64 años"~"Entre 45 y 64 años", 
                                                            EDAD == "65 años o más"~"65 años o más",
                                                            EDAD == "" ~ "Todos"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(POBRE = case_when(POBRE == "POBRE" ~ "Pobre", 
                                                             POBRE == "NO POBRE" ~ "No pobre",
                                                             POBRE == "" ~ "Todos"))


m509_tp <- BASE_MOTOR %>% select(CODIND,	
                                 NOMINDICADOR,	
                                 CATEGORIA,	
                                 PESTAÑA,	
                                 SEXO,	
                                 ASCENDENCIA,	
                                 QUINTIL,	
                                 DEPARTAMENTOUY,	
                                 URBANORURALUY,	
                                 EDAD,	
                                 POBRE,
                                 NSE,
                                 NIVELEDU,
                                 PAÍS,	
                                 ANIO,	
                                 VALOR,	
                                 RESPONSABLE)





### 510 Porcentaje de hogares en los que residen menores entre 5 y 12 años de edad

ech_h_svy <- ech_h_svy %>% dplyr::mutate(menorH=men5a12h)


## País Urbano

# Total

NOMINDICADOR <- 
  c("Porcentaje de hogares en los que residen menores entre 5 y 12 años de edad")

a_mes <- ech_h_svy %>%
  srvyr::filter(bd_region == 1) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(menorH))
a_sem <- mean(as.numeric(a_mes$colname))

c_ano <- as.numeric(a_sem)

c_ano <- cbind(c_ano, NOMINDICADOR)
colnames(c_ano) <- c("VALOR", "NOMINDICADOR")
c_ano <- as.data.frame(c_ano)
BASE_MOTOR <- cbind(c_ano,BASE_AUX)


# Sexo del jefe

a_jefe <- function(x) {
  x <- ech_h_svy %>%
    filter(sexojefe == x & bd_region == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(menorH))
  x <- mean(x$colname)
}       

a_e_jefe <- numeric()

for(i in 1:2){
  a_e_jefe[i] <- a_jefe(x = i)
}     

c_jefe <- as.data.frame(a_e_jefe)
c_jefe_1 <- cbind(as.data.frame(c_jefe[1,1]), NOMINDICADOR, BASE_AUX)
c_jefe_1 <- c_jefe_1 %>% dplyr::mutate(SEXO = "VARONES")
c_jefe_2 <- cbind(as.data.frame(c_jefe[2,1]), NOMINDICADOR, BASE_AUX)
c_jefe_2 <- c_jefe_2 %>% dplyr::mutate(SEXO = "MUJERES")
colnames(c_jefe_1) <- colnames(BASE_MOTOR)
colnames(c_jefe_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_jefe_1,c_jefe_2)


# Tramo de edad


a_edad <- function(x) {
  x <- ech_h_svy %>%
    filter(tramo_edad2 == x & bd_region == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(menorH))
  x <- mean(x$colname)
}       

a_e_edad <- numeric()

for(i in 1:4){
  a_e_edad[i] <- a_edad(x = i)
}     
c_edad <- as.data.frame(a_e_edad)
c_edad_1 <- cbind(as.data.frame(c_edad[1,1]), NOMINDICADOR, BASE_AUX)
c_edad_1 <- c_edad_1 %>% dplyr::mutate(EDAD = "Menor de 30 años")
c_edad_2 <- cbind(as.data.frame(c_edad[2,1]), NOMINDICADOR, BASE_AUX)
c_edad_2 <- c_edad_2 %>% dplyr::mutate(EDAD = "Entre 30 y 44 años")
c_edad_3 <- cbind(as.data.frame(c_edad[3,1]), NOMINDICADOR, BASE_AUX)
c_edad_3 <- c_edad_3 %>% dplyr::mutate(EDAD = "Entre 45 y 64 años")
c_edad_4 <- cbind(as.data.frame(c_edad[4,1]), NOMINDICADOR, BASE_AUX)
c_edad_4 <- c_edad_4 %>% dplyr::mutate(EDAD = "65 años o más")
colnames(c_edad_1) <- colnames(BASE_MOTOR)
colnames(c_edad_2) <- colnames(BASE_MOTOR)
colnames(c_edad_3) <- colnames(BASE_MOTOR)
colnames(c_edad_4) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_edad_1,c_edad_2, c_edad_3, c_edad_4)


# Quintil de ingreso del hogar

a_quintil <- function(x) {
  x <- ech_h_svy %>%
    filter(quintilesy == x & bd_region == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(menorH))
  x <- mean(x$colname)
}       

a_e_quintil <- numeric()

for(i in 1:5){
  a_e_quintil[i] <- a_quintil(x = i)
}     

c_quintil <- as.data.frame(a_e_quintil)
c_quintil_1 <- cbind(as.data.frame(c_quintil[1,1]), NOMINDICADOR, BASE_AUX)
c_quintil_1 <- c_quintil_1 %>% dplyr::mutate(QUINTIL = "Quintil 1")
c_quintil_2 <- cbind(as.data.frame(c_quintil[2,1]), NOMINDICADOR, BASE_AUX)
c_quintil_2 <- c_quintil_2 %>% dplyr::mutate(QUINTIL = "Quintil 2")
c_quintil_3 <- cbind(as.data.frame(c_quintil[3,1]), NOMINDICADOR, BASE_AUX)
c_quintil_3 <- c_quintil_3 %>% dplyr::mutate(QUINTIL = "Quintil 3")
c_quintil_4 <- cbind(as.data.frame(c_quintil[4,1]), NOMINDICADOR, BASE_AUX)
c_quintil_4 <- c_quintil_4 %>% dplyr::mutate(QUINTIL = "Quintil 4")
c_quintil_5 <- cbind(as.data.frame(c_quintil[5,1]), NOMINDICADOR, BASE_AUX)
c_quintil_5 <- c_quintil_5 %>% dplyr::mutate(QUINTIL = "Quintil 5")
colnames(c_quintil_1) <- colnames(BASE_MOTOR)
colnames(c_quintil_2) <- colnames(BASE_MOTOR)
colnames(c_quintil_3) <- colnames(BASE_MOTOR)
colnames(c_quintil_4) <- colnames(BASE_MOTOR)
colnames(c_quintil_5) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_quintil_1,c_quintil_2, c_quintil_3, c_quintil_4, c_quintil_5)




# Base motor

BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(CODIND = "510")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(CATEGORIA = "Composición y estructura de los hogares")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(PESTAÑA = "País urbano")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(PAÍS = "Uruguay")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(ANIO = 2023)
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(RESPONSABLE = "Sharon Katzkowicz")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(ASCENDENCIA = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(DEPARTAMENTOUY = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(URBANORURALUY = "Urbano (más de 5.000 habitantes)")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(POBRE = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(NSE = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(NIVELEDU = "Todos")
BASE_MOTOR <- as.data.frame(BASE_MOTOR)

BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(SEXO = case_when(SEXO == "" ~ "Todos", SEXO == "MUJERES"~"Mujeres", SEXO=="VARONES"~"Varones"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(QUINTIL = case_when(QUINTIL == "" ~ "Todos",
                                                               QUINTIL == "Quintil 1"~"Quintil 1", 
                                                               QUINTIL == "Quintil 2"~"Quintil 2",
                                                               QUINTIL == "Quintil 3"~"Quintil 3", 
                                                               QUINTIL == "Quintil 4"~"Quintil 4",
                                                               QUINTIL == "Quintil 5"~"Quintil 5"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(EDAD = case_when(EDAD == "" ~ "Todos",
                                                            EDAD == "Menor de 30 años"~"Menor de 30 años", 
                                                            EDAD == "Entre 30 y 44 años"~"Entre 30 y 44 años",
                                                            EDAD == "Entre 45 y 64 años"~"Entre 45 y 64 años", 
                                                            EDAD == "65 años o más"~"65 años o más"))


m510_pu <- BASE_MOTOR %>% select(CODIND,	
                                 NOMINDICADOR,	
                                 CATEGORIA,	
                                 PESTAÑA,	
                                 SEXO,	
                                 ASCENDENCIA,	
                                 QUINTIL,	
                                 DEPARTAMENTOUY,	
                                 URBANORURALUY,	
                                 EDAD,	
                                 POBRE,
                                 NSE,
                                 NIVELEDU,
                                 PAÍS,	
                                 ANIO,	
                                 VALOR,	
                                 RESPONSABLE)	


## Total país

# Total


NOMINDICADOR <- 
  c("Porcentaje de hogares en los que residen menores entre 5 y 12 años de edad")

a_mes <- ech_h_svy %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(menorH))
a_sem <- mean(as.numeric(a_mes$colname))

c_ano <- as.numeric(a_sem)

c_ano <- cbind(c_ano, NOMINDICADOR)
colnames(c_ano) <- c("VALOR", "NOMINDICADOR")
c_ano <- as.data.frame(c_ano)
BASE_MOTOR <- cbind(c_ano,BASE_AUX)



# Región

a_reg <- function(x) {
  x <- ech_h_svy %>%
    filter(bd_region == x) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(menorH))
  x <- mean(x$colname)
}       

a_e_reg <- numeric()

for(i in 1:3){
  a_e_reg[i] <- a_reg(x = i)
}     
c_reg <- as.data.frame(a_e_reg)
c_reg_1 <- cbind(as.data.frame(c_reg[2,1]), NOMINDICADOR, BASE_AUX)
c_reg_1 <- c_reg_1 %>% dplyr::mutate(URBANORURALUY = "URBANO (MENOS DE 5.000 HABITANTES)")
c_reg_2 <- cbind(as.data.frame(c_reg[3,1]), NOMINDICADOR, BASE_AUX)
c_reg_2 <- c_reg_2 %>% dplyr::mutate(URBANORURALUY = "RURAL DISPERSO")
colnames(c_reg_1) <- colnames(BASE_MOTOR)
colnames(c_reg_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_reg_1,c_reg_2)


# Sexo del jefe

a_jefe <- function(x) {
  x <- ech_h_svy %>%
    filter(sexojefe == x) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(menorH))
  x <- mean(x$colname)
}       

a_e_jefe <- numeric()

for(i in 1:2){
  a_e_jefe[i] <- a_jefe(x = i)
}     

c_jefe <- as.data.frame(a_e_jefe)
c_jefe_1 <- cbind(as.data.frame(c_jefe[1,1]), NOMINDICADOR, BASE_AUX)
c_jefe_1 <- c_jefe_1 %>% dplyr::mutate(SEXO = "VARONES")
c_jefe_2 <- cbind(as.data.frame(c_jefe[2,1]), NOMINDICADOR, BASE_AUX)
c_jefe_2 <- c_jefe_2 %>% dplyr::mutate(SEXO = "MUJERES")
colnames(c_jefe_1) <- colnames(BASE_MOTOR)
colnames(c_jefe_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_jefe_1,c_jefe_2)


# Tramo de edad

a_edad <- function(x) {
  x <- ech_h_svy %>%
    filter(tramo_edad2 == x) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(menorH))
  x <- mean(x$colname)
}       

a_e_edad <- numeric()

for(i in 1:4){
  a_e_edad[i] <- a_edad(x = i)
}     

c_edad <- as.data.frame(a_e_edad)
c_edad_1 <- cbind(as.data.frame(c_edad[1,1]), NOMINDICADOR, BASE_AUX)
c_edad_1 <- c_edad_1 %>% dplyr::mutate(EDAD = "Menor de 30 años")
c_edad_2 <- cbind(as.data.frame(c_edad[2,1]), NOMINDICADOR, BASE_AUX)
c_edad_2 <- c_edad_2 %>% dplyr::mutate(EDAD = "Entre 30 y 44 años")
c_edad_3 <- cbind(as.data.frame(c_edad[3,1]), NOMINDICADOR, BASE_AUX)
c_edad_3 <- c_edad_3 %>% dplyr::mutate(EDAD = "Entre 45 y 64 años")
c_edad_4 <- cbind(as.data.frame(c_edad[4,1]), NOMINDICADOR, BASE_AUX)
c_edad_4 <- c_edad_4 %>% dplyr::mutate(EDAD = "65 años o más")
colnames(c_edad_1) <- colnames(BASE_MOTOR)
colnames(c_edad_2) <- colnames(BASE_MOTOR)
colnames(c_edad_3) <- colnames(BASE_MOTOR)
colnames(c_edad_4) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_edad_1,c_edad_2, c_edad_3, c_edad_4)



# Ascendencia

a_ascen <- function(x) {
  x <- ech_h_svy %>%
    filter(bd_e29_1 == x) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(menorH))
  x <- mean(x$colname)
}       

a_e_ascen <- numeric()

for(i in 1:2){
  a_e_ascen[i] <- a_ascen(x = i)
}     

c_ascen <- as.data.frame(a_e_ascen)
c_ascen_1 <- cbind(as.data.frame(c_ascen[1,1]), NOMINDICADOR, BASE_AUX)
c_ascen_1 <- c_ascen_1 %>% dplyr::mutate(ASCENDENCIA = "AFRO")
c_ascen_2 <- cbind(as.data.frame(c_ascen[2,1]), NOMINDICADOR, BASE_AUX)
c_ascen_2 <- c_ascen_2 %>% dplyr::mutate(ASCENDENCIA = "NO AFRO")
colnames(c_ascen_1) <- colnames(BASE_MOTOR)
colnames(c_ascen_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_ascen_1,c_ascen_2)


# Quintil de ingreso del hogar

a_quintil <- function(x) {
  x <- ech_h_svy %>%
    filter(quintilesy == x) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(menorH))
  x <- mean(x$colname)
}       

a_e_quintil <- numeric()

for(i in 1:5){
  a_e_quintil[i] <- a_quintil(x = i)
}     

c_quintil <- as.data.frame(a_e_quintil)
c_quintil_1 <- cbind(as.data.frame(c_quintil[1,1]), NOMINDICADOR, BASE_AUX)
c_quintil_1 <- c_quintil_1 %>% dplyr::mutate(QUINTIL = "Quintil 1")
c_quintil_2 <- cbind(as.data.frame(c_quintil[2,1]), NOMINDICADOR, BASE_AUX)
c_quintil_2 <- c_quintil_2 %>% dplyr::mutate(QUINTIL = "Quintil 2")
c_quintil_3 <- cbind(as.data.frame(c_quintil[3,1]), NOMINDICADOR, BASE_AUX)
c_quintil_3 <- c_quintil_3 %>% dplyr::mutate(QUINTIL = "Quintil 3")
c_quintil_4 <- cbind(as.data.frame(c_quintil[4,1]), NOMINDICADOR, BASE_AUX)
c_quintil_4 <- c_quintil_4 %>% dplyr::mutate(QUINTIL = "Quintil 4")
c_quintil_5 <- cbind(as.data.frame(c_quintil[5,1]), NOMINDICADOR, BASE_AUX)
c_quintil_5 <- c_quintil_5 %>% dplyr::mutate(QUINTIL = "Quintil 5")
colnames(c_quintil_1) <- colnames(BASE_MOTOR)
colnames(c_quintil_2) <- colnames(BASE_MOTOR)
colnames(c_quintil_3) <- colnames(BASE_MOTOR)
colnames(c_quintil_4) <- colnames(BASE_MOTOR)
colnames(c_quintil_5) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_quintil_1,c_quintil_2, c_quintil_3, c_quintil_4, c_quintil_5)


# Pobreza

a_pobre <- function(x) {
  x <- ech_h_svy %>%
    filter(pobre_aux == x) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(menorH))
  x <- mean(x$colname)
}       

a_e_pobre <- numeric()

for(i in 1:2){
  a_e_pobre[i] <- a_pobre(x = i)
}     

c_pobre <- as.data.frame(a_e_pobre)
c_pobre_1 <- cbind(as.data.frame(c_pobre[1,1]), NOMINDICADOR, BASE_AUX)
c_pobre_1 <- c_pobre_1 %>% dplyr::mutate(POBRE = "POBRE")
c_pobre_2 <- cbind(as.data.frame(c_pobre[2,1]), NOMINDICADOR, BASE_AUX)
c_pobre_2 <- c_pobre_2 %>% dplyr::mutate(POBRE = "NO POBRE")
colnames(c_pobre_1) <- colnames(BASE_MOTOR)
colnames(c_pobre_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_pobre_1,c_pobre_2)


# Base motor

BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(CODIND = "510")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(CATEGORIA = "Composición y estructura de los hogares")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(PESTAÑA = "Total país")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(PAÍS = "Uruguay")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(ANIO = 2023)
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(RESPONSABLE = "Sharon Katzkowicz")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(DEPARTAMENTOUY = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(NSE = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(NIVELEDU = "Todos")
BASE_MOTOR <- as.data.frame(BASE_MOTOR)


BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(ASCENDENCIA = case_when(ASCENDENCIA == "AFRO" ~ "Afro", 
                                                                   ASCENDENCIA == "NO AFRO" ~ "No afro",
                                                                   ASCENDENCIA == "" ~ "Todos"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(URBANORURALUY = case_when(URBANORURALUY == "URBANO (MENOS DE 5.000 HABITANTES)" ~ "Urbano (menos de 5.000 habitantes)",
                                                                     URBANORURALUY == "RURAL DISPERSO" ~ "Rural disperso", 
                                                                     URBANORURALUY == "" ~ "Total país"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(SEXO = case_when(SEXO == "MUJERES" ~ "Mujeres", 
                                                            SEXO=="VARONES" ~ "Varones",
                                                            SEXO == "" ~ "Todos"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(QUINTIL = case_when(QUINTIL == "Quintil 1"~"Quintil 1", 
                                                               QUINTIL == "Quintil 2"~"Quintil 2",
                                                               QUINTIL == "Quintil 3"~"Quintil 3", 
                                                               QUINTIL == "Quintil 4"~"Quintil 4",
                                                               QUINTIL == "Quintil 5"~"Quintil 5",
                                                               QUINTIL == "" ~ "Todos"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(EDAD = case_when(EDAD == "Menor de 30 años"~"Menor de 30 años", 
                                                            EDAD == "Entre 30 y 44 años"~"Entre 30 y 44 años",
                                                            EDAD == "Entre 45 y 64 años"~"Entre 45 y 64 años", 
                                                            EDAD == "65 años o más"~"65 años o más",
                                                            EDAD == "" ~ "Todos"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(POBRE = case_when(POBRE == "POBRE" ~ "Pobre", 
                                                             POBRE == "NO POBRE" ~ "No pobre",
                                                             POBRE == "" ~ "Todos"))


m510_tp <- BASE_MOTOR %>% select(CODIND,	
                                 NOMINDICADOR,	
                                 CATEGORIA,	
                                 PESTAÑA,	
                                 SEXO,	
                                 ASCENDENCIA,	
                                 QUINTIL,	
                                 DEPARTAMENTOUY,	
                                 URBANORURALUY,	
                                 EDAD,	
                                 POBRE,
                                 NSE,
                                 NIVELEDU,
                                 PAÍS,	
                                 ANIO,	
                                 VALOR,	
                                 RESPONSABLE)



### 511 Porcentaje de hogares en los que residen menores entre 13 y 17 años de edad

ech_h_svy <- ech_h_svy %>% dplyr::mutate(menorH=men13a17h)

## País Urbano

# Total

NOMINDICADOR <- 
  c("Porcentaje de hogares en los que residen menores entre 13 y 17 años de edad")

a_mes <- ech_h_svy %>%
  srvyr::filter(bd_region == 1) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(menorH))
a_sem <- mean(as.numeric(a_mes$colname))

c_ano <- as.numeric(a_sem)

c_ano <- cbind(c_ano, NOMINDICADOR)
colnames(c_ano) <- c("VALOR", "NOMINDICADOR")
c_ano <- as.data.frame(c_ano)
BASE_MOTOR <- cbind(c_ano,BASE_AUX)


# Sexo del jefe

a_jefe <- function(x) {
  x <- ech_h_svy %>%
    filter(sexojefe == x & bd_region == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(menorH))
  x <- mean(x$colname)
}       

a_e_jefe <- numeric()

for(i in 1:2){
  a_e_jefe[i] <- a_jefe(x = i)
}     

c_jefe <- as.data.frame(a_e_jefe)
c_jefe_1 <- cbind(as.data.frame(c_jefe[1,1]), NOMINDICADOR, BASE_AUX)
c_jefe_1 <- c_jefe_1 %>% dplyr::mutate(SEXO = "VARONES")
c_jefe_2 <- cbind(as.data.frame(c_jefe[2,1]), NOMINDICADOR, BASE_AUX)
c_jefe_2 <- c_jefe_2 %>% dplyr::mutate(SEXO = "MUJERES")
colnames(c_jefe_1) <- colnames(BASE_MOTOR)
colnames(c_jefe_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_jefe_1,c_jefe_2)


# Tramo de edad


a_edad <- function(x) {
  x <- ech_h_svy %>%
    filter(tramo_edad2 == x & bd_region == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(menorH))
  x <- mean(x$colname)
}       

a_e_edad <- numeric()

for(i in 1:4){
  a_e_edad[i] <- a_edad(x = i)
}     
c_edad <- as.data.frame(a_e_edad)
c_edad_1 <- cbind(as.data.frame(c_edad[1,1]), NOMINDICADOR, BASE_AUX)
c_edad_1 <- c_edad_1 %>% dplyr::mutate(EDAD = "Menor de 30 años")
c_edad_2 <- cbind(as.data.frame(c_edad[2,1]), NOMINDICADOR, BASE_AUX)
c_edad_2 <- c_edad_2 %>% dplyr::mutate(EDAD = "Entre 30 y 44 años")
c_edad_3 <- cbind(as.data.frame(c_edad[3,1]), NOMINDICADOR, BASE_AUX)
c_edad_3 <- c_edad_3 %>% dplyr::mutate(EDAD = "Entre 45 y 64 años")
c_edad_4 <- cbind(as.data.frame(c_edad[4,1]), NOMINDICADOR, BASE_AUX)
c_edad_4 <- c_edad_4 %>% dplyr::mutate(EDAD = "65 años o más")
colnames(c_edad_1) <- colnames(BASE_MOTOR)
colnames(c_edad_2) <- colnames(BASE_MOTOR)
colnames(c_edad_3) <- colnames(BASE_MOTOR)
colnames(c_edad_4) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_edad_1,c_edad_2, c_edad_3, c_edad_4)


# Quintil de ingreso del hogar

a_quintil <- function(x) {
  x <- ech_h_svy %>%
    filter(quintilesy == x & bd_region == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(menorH))
  x <- mean(x$colname)
}       

a_e_quintil <- numeric()

for(i in 1:5){
  a_e_quintil[i] <- a_quintil(x = i)
}     

c_quintil <- as.data.frame(a_e_quintil)
c_quintil_1 <- cbind(as.data.frame(c_quintil[1,1]), NOMINDICADOR, BASE_AUX)
c_quintil_1 <- c_quintil_1 %>% dplyr::mutate(QUINTIL = "Quintil 1")
c_quintil_2 <- cbind(as.data.frame(c_quintil[2,1]), NOMINDICADOR, BASE_AUX)
c_quintil_2 <- c_quintil_2 %>% dplyr::mutate(QUINTIL = "Quintil 2")
c_quintil_3 <- cbind(as.data.frame(c_quintil[3,1]), NOMINDICADOR, BASE_AUX)
c_quintil_3 <- c_quintil_3 %>% dplyr::mutate(QUINTIL = "Quintil 3")
c_quintil_4 <- cbind(as.data.frame(c_quintil[4,1]), NOMINDICADOR, BASE_AUX)
c_quintil_4 <- c_quintil_4 %>% dplyr::mutate(QUINTIL = "Quintil 4")
c_quintil_5 <- cbind(as.data.frame(c_quintil[5,1]), NOMINDICADOR, BASE_AUX)
c_quintil_5 <- c_quintil_5 %>% dplyr::mutate(QUINTIL = "Quintil 5")
colnames(c_quintil_1) <- colnames(BASE_MOTOR)
colnames(c_quintil_2) <- colnames(BASE_MOTOR)
colnames(c_quintil_3) <- colnames(BASE_MOTOR)
colnames(c_quintil_4) <- colnames(BASE_MOTOR)
colnames(c_quintil_5) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_quintil_1,c_quintil_2, c_quintil_3, c_quintil_4, c_quintil_5)




# Base motor

BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(CODIND = "511")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(CATEGORIA = "Composición y estructura de los hogares")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(PESTAÑA = "País urbano")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(PAÍS = "Uruguay")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(ANIO = 2023)
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(RESPONSABLE = "Sharon Katzkowicz")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(ASCENDENCIA = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(DEPARTAMENTOUY = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(URBANORURALUY = "Urbano (más de 5.000 habitantes)")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(POBRE = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(NSE = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(NIVELEDU = "Todos")
BASE_MOTOR <- as.data.frame(BASE_MOTOR)

BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(SEXO = case_when(SEXO == "" ~ "Todos", SEXO == "MUJERES"~"Mujeres", SEXO=="VARONES"~"Varones"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(QUINTIL = case_when(QUINTIL == "" ~ "Todos",
                                                               QUINTIL == "Quintil 1"~"Quintil 1", 
                                                               QUINTIL == "Quintil 2"~"Quintil 2",
                                                               QUINTIL == "Quintil 3"~"Quintil 3", 
                                                               QUINTIL == "Quintil 4"~"Quintil 4",
                                                               QUINTIL == "Quintil 5"~"Quintil 5"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(EDAD = case_when(EDAD == "" ~ "Todos",
                                                            EDAD == "Menor de 30 años"~"Menor de 30 años", 
                                                            EDAD == "Entre 30 y 44 años"~"Entre 30 y 44 años",
                                                            EDAD == "Entre 45 y 64 años"~"Entre 45 y 64 años", 
                                                            EDAD == "65 años o más"~"65 años o más"))


m511_pu <- BASE_MOTOR %>% select(CODIND,	
                                 NOMINDICADOR,	
                                 CATEGORIA,	
                                 PESTAÑA,	
                                 SEXO,	
                                 ASCENDENCIA,	
                                 QUINTIL,	
                                 DEPARTAMENTOUY,	
                                 URBANORURALUY,	
                                 EDAD,	
                                 POBRE,
                                 NSE,
                                 NIVELEDU,
                                 PAÍS,	
                                 ANIO,	
                                 VALOR,	
                                 RESPONSABLE)	


## Total país

# Total


NOMINDICADOR <- 
  c("Porcentaje de hogares en los que residen menores entre 13 y 17 años de edad")

a_mes <- ech_h_svy %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(menorH))
a_sem <- mean(as.numeric(a_mes$colname))
c_ano <- as.numeric(a_sem)

c_ano <- cbind(c_ano, NOMINDICADOR)
colnames(c_ano) <- c("VALOR", "NOMINDICADOR")
c_ano <- as.data.frame(c_ano)
BASE_MOTOR <- cbind(c_ano,BASE_AUX)



# Región

a_reg <- function(x) {
  x <- ech_h_svy %>%
    filter(bd_region == x) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(menorH))
  x <- mean(x$colname)
}       

a_e_reg <- numeric()

for(i in 1:3){
  a_e_reg[i] <- a_reg(x = i)
}     
c_reg <- as.data.frame(a_e_reg)
c_reg_1 <- cbind(as.data.frame(c_reg[2,1]), NOMINDICADOR, BASE_AUX)
c_reg_1 <- c_reg_1 %>% dplyr::mutate(URBANORURALUY = "URBANO (MENOS DE 5.000 HABITANTES)")
c_reg_2 <- cbind(as.data.frame(c_reg[3,1]), NOMINDICADOR, BASE_AUX)
c_reg_2 <- c_reg_2 %>% dplyr::mutate(URBANORURALUY = "RURAL DISPERSO")
colnames(c_reg_1) <- colnames(BASE_MOTOR)
colnames(c_reg_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_reg_1,c_reg_2)


# Sexo del jefe

a_jefe <- function(x) {
  x <- ech_h_svy %>%
    filter(sexojefe == x) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(menorH))
  x <- mean(x$colname)
}       

a_e_jefe <- numeric()

for(i in 1:2){
  a_e_jefe[i] <- a_jefe(x = i)
}     


c_jefe <- as.data.frame(a_e_jefe)
c_jefe_1 <- cbind(as.data.frame(c_jefe[1,1]), NOMINDICADOR, BASE_AUX)
c_jefe_1 <- c_jefe_1 %>% dplyr::mutate(SEXO = "VARONES")
c_jefe_2 <- cbind(as.data.frame(c_jefe[2,1]), NOMINDICADOR, BASE_AUX)
c_jefe_2 <- c_jefe_2 %>% dplyr::mutate(SEXO = "MUJERES")
colnames(c_jefe_1) <- colnames(BASE_MOTOR)
colnames(c_jefe_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_jefe_1,c_jefe_2)


# Tramo de edad

a_edad <- function(x) {
  x <- ech_h_svy %>%
    filter(tramo_edad2 == x) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(menorH))
  x <- mean(x$colname)
}       

a_e_edad <- numeric()

for(i in 1:4){
  a_e_edad[i] <- a_edad(x = i)
}     
c_edad <- as.data.frame(a_e_edad)
c_edad_1 <- cbind(as.data.frame(c_edad[1,1]), NOMINDICADOR, BASE_AUX)
c_edad_1 <- c_edad_1 %>% dplyr::mutate(EDAD = "Menor de 30 años")
c_edad_2 <- cbind(as.data.frame(c_edad[2,1]), NOMINDICADOR, BASE_AUX)
c_edad_2 <- c_edad_2 %>% dplyr::mutate(EDAD = "Entre 30 y 44 años")
c_edad_3 <- cbind(as.data.frame(c_edad[3,1]), NOMINDICADOR, BASE_AUX)
c_edad_3 <- c_edad_3 %>% dplyr::mutate(EDAD = "Entre 45 y 64 años")
c_edad_4 <- cbind(as.data.frame(c_edad[4,1]), NOMINDICADOR, BASE_AUX)
c_edad_4 <- c_edad_4 %>% dplyr::mutate(EDAD = "65 años o más")
colnames(c_edad_1) <- colnames(BASE_MOTOR)
colnames(c_edad_2) <- colnames(BASE_MOTOR)
colnames(c_edad_3) <- colnames(BASE_MOTOR)
colnames(c_edad_4) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_edad_1,c_edad_2, c_edad_3, c_edad_4)



# Ascendencia

a_ascen <- function(x) {
  x <- ech_h_svy %>%
    filter(bd_e29_1 == x) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(menorH))
  x <- mean(x$colname)
}       

a_e_ascen <- numeric()

for(i in 1:2){
  a_e_ascen[i] <- a_ascen(x = i)
}     

c_ascen <- as.data.frame(a_e_ascen)
c_ascen_1 <- cbind(as.data.frame(c_ascen[1,1]), NOMINDICADOR, BASE_AUX)
c_ascen_1 <- c_ascen_1 %>% dplyr::mutate(ASCENDENCIA = "AFRO")
c_ascen_2 <- cbind(as.data.frame(c_ascen[2,1]), NOMINDICADOR, BASE_AUX)
c_ascen_2 <- c_ascen_2 %>% dplyr::mutate(ASCENDENCIA = "NO AFRO")
colnames(c_ascen_1) <- colnames(BASE_MOTOR)
colnames(c_ascen_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_ascen_1,c_ascen_2)


# Quintil de ingreso del hogar

a_quintil <- function(x) {
  x <- ech_h_svy %>%
    filter(quintilesy == x) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(menorH))
  x <- mean(x$colname)
}       

a_e_quintil <- numeric()

for(i in 1:5){
  a_e_quintil[i] <- a_quintil(x = i)
}     

c_quintil <- as.data.frame(a_e_quintil)
c_quintil_1 <- cbind(as.data.frame(c_quintil[1,1]), NOMINDICADOR, BASE_AUX)
c_quintil_1 <- c_quintil_1 %>% dplyr::mutate(QUINTIL = "Quintil 1")
c_quintil_2 <- cbind(as.data.frame(c_quintil[2,1]), NOMINDICADOR, BASE_AUX)
c_quintil_2 <- c_quintil_2 %>% dplyr::mutate(QUINTIL = "Quintil 2")
c_quintil_3 <- cbind(as.data.frame(c_quintil[3,1]), NOMINDICADOR, BASE_AUX)
c_quintil_3 <- c_quintil_3 %>% dplyr::mutate(QUINTIL = "Quintil 3")
c_quintil_4 <- cbind(as.data.frame(c_quintil[4,1]), NOMINDICADOR, BASE_AUX)
c_quintil_4 <- c_quintil_4 %>% dplyr::mutate(QUINTIL = "Quintil 4")
c_quintil_5 <- cbind(as.data.frame(c_quintil[5,1]), NOMINDICADOR, BASE_AUX)
c_quintil_5 <- c_quintil_5 %>% dplyr::mutate(QUINTIL = "Quintil 5")
colnames(c_quintil_1) <- colnames(BASE_MOTOR)
colnames(c_quintil_2) <- colnames(BASE_MOTOR)
colnames(c_quintil_3) <- colnames(BASE_MOTOR)
colnames(c_quintil_4) <- colnames(BASE_MOTOR)
colnames(c_quintil_5) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_quintil_1,c_quintil_2, c_quintil_3, c_quintil_4, c_quintil_5)


# Pobreza

a_pobre <- function(x) {
  x <- ech_h_svy %>%
    filter(pobre_aux == x) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(menorH))
  x <- mean(x$colname)
}       

a_e_pobre <- numeric()

for(i in 1:2){
  a_e_pobre[i] <- a_pobre(x = i)
}     

c_pobre <- as.data.frame(a_e_pobre)
c_pobre_1 <- cbind(as.data.frame(c_pobre[1,1]), NOMINDICADOR, BASE_AUX)
c_pobre_1 <- c_pobre_1 %>% dplyr::mutate(POBRE = "POBRE")
c_pobre_2 <- cbind(as.data.frame(c_pobre[2,1]), NOMINDICADOR, BASE_AUX)
c_pobre_2 <- c_pobre_2 %>% dplyr::mutate(POBRE = "NO POBRE")
colnames(c_pobre_1) <- colnames(BASE_MOTOR)
colnames(c_pobre_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_pobre_1,c_pobre_2)


# Base motor

BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(CODIND = "511")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(CATEGORIA = "Composición y estructura de los hogares")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(PESTAÑA = "Total país")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(PAÍS = "Uruguay")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(ANIO = 2023)
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(RESPONSABLE = "Sharon Katzkowicz")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(DEPARTAMENTOUY = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(NSE = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(NIVELEDU = "Todos")
BASE_MOTOR <- as.data.frame(BASE_MOTOR)


BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(ASCENDENCIA = case_when(ASCENDENCIA == "AFRO" ~ "Afro", 
                                                                   ASCENDENCIA == "NO AFRO" ~ "No afro",
                                                                   ASCENDENCIA == "" ~ "Todos"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(URBANORURALUY = case_when(URBANORURALUY == "URBANO (MENOS DE 5.000 HABITANTES)" ~ "Urbano (menos de 5.000 habitantes)",
                                                                     URBANORURALUY == "RURAL DISPERSO" ~ "Rural disperso", 
                                                                     URBANORURALUY == "" ~ "Total país"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(SEXO = case_when(SEXO == "MUJERES" ~ "Mujeres", 
                                                            SEXO=="VARONES" ~ "Varones",
                                                            SEXO == "" ~ "Todos"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(QUINTIL = case_when(QUINTIL == "Quintil 1"~"Quintil 1", 
                                                               QUINTIL == "Quintil 2"~"Quintil 2",
                                                               QUINTIL == "Quintil 3"~"Quintil 3", 
                                                               QUINTIL == "Quintil 4"~"Quintil 4",
                                                               QUINTIL == "Quintil 5"~"Quintil 5",
                                                               QUINTIL == "" ~ "Todos"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(EDAD = case_when(EDAD == "Menor de 30 años"~"Menor de 30 años", 
                                                            EDAD == "Entre 30 y 44 años"~"Entre 30 y 44 años",
                                                            EDAD == "Entre 45 y 64 años"~"Entre 45 y 64 años", 
                                                            EDAD == "65 años o más"~"65 años o más",
                                                            EDAD == "" ~ "Todos"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(POBRE = case_when(POBRE == "POBRE" ~ "Pobre", 
                                                             POBRE == "NO POBRE" ~ "No pobre",
                                                             POBRE == "" ~ "Todos"))


m511_tp <- BASE_MOTOR %>% select(CODIND,	
                                 NOMINDICADOR,	
                                 CATEGORIA,	
                                 PESTAÑA,	
                                 SEXO,	
                                 ASCENDENCIA,	
                                 QUINTIL,	
                                 DEPARTAMENTOUY,	
                                 URBANORURALUY,	
                                 EDAD,	
                                 POBRE,
                                 NSE,
                                 NIVELEDU,
                                 PAÍS,	
                                 ANIO,	
                                 VALOR,	
                                 RESPONSABLE)


### 512 Porcentaje de hogares en los que residen menores entre 0 y 17 años de edad

ech_h_svy <- ech_h_svy %>% dplyr::mutate(menorH=men18h)


## País Urbano

# Total

NOMINDICADOR <- 
  c("Porcentaje de hogares en los que residen menores entre 0 y 17 años de edad")

a_mes <- ech_h_svy %>%
  srvyr::filter(bd_region == 1) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(menorH))
a_sem <- mean(as.numeric(a_mes$colname))

c_ano <- as.numeric(a_sem)

c_ano <- cbind(c_ano, NOMINDICADOR)
colnames(c_ano) <- c("VALOR", "NOMINDICADOR")
c_ano <- as.data.frame(c_ano)
BASE_MOTOR <- cbind(c_ano,BASE_AUX)


# Sexo del jefe

a_jefe <- function(x) {
  x <- ech_h_svy %>%
    filter(sexojefe == x & bd_region == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(menorH))
  x <- mean(x$colname)
}       

a_e_jefe <- numeric()

for(i in 1:2){
  a_e_jefe[i] <- a_jefe(x = i)
}     


c_jefe <- as.data.frame(a_e_jefe)
c_jefe_1 <- cbind(as.data.frame(c_jefe[1,1]), NOMINDICADOR, BASE_AUX)
c_jefe_1 <- c_jefe_1 %>% dplyr::mutate(SEXO = "VARONES")
c_jefe_2 <- cbind(as.data.frame(c_jefe[2,1]), NOMINDICADOR, BASE_AUX)
c_jefe_2 <- c_jefe_2 %>% dplyr::mutate(SEXO = "MUJERES")
colnames(c_jefe_1) <- colnames(BASE_MOTOR)
colnames(c_jefe_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_jefe_1,c_jefe_2)


# Tramo de edad


a_edad <- function(x) {
  x <- ech_h_svy %>%
    filter(tramo_edad2 == x & bd_region == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(menorH))
  x <- mean(x$colname)
}       

a_e_edad <- numeric()

for(i in 1:4){
  a_e_edad[i] <- a_edad(x = i)
}     

c_edad <- as.data.frame(a_e_edad)
c_edad_1 <- cbind(as.data.frame(c_edad[1,1]), NOMINDICADOR, BASE_AUX)
c_edad_1 <- c_edad_1 %>% dplyr::mutate(EDAD = "Menor de 30 años")
c_edad_2 <- cbind(as.data.frame(c_edad[2,1]), NOMINDICADOR, BASE_AUX)
c_edad_2 <- c_edad_2 %>% dplyr::mutate(EDAD = "Entre 30 y 44 años")
c_edad_3 <- cbind(as.data.frame(c_edad[3,1]), NOMINDICADOR, BASE_AUX)
c_edad_3 <- c_edad_3 %>% dplyr::mutate(EDAD = "Entre 45 y 64 años")
c_edad_4 <- cbind(as.data.frame(c_edad[4,1]), NOMINDICADOR, BASE_AUX)
c_edad_4 <- c_edad_4 %>% dplyr::mutate(EDAD = "65 años o más")
colnames(c_edad_1) <- colnames(BASE_MOTOR)
colnames(c_edad_2) <- colnames(BASE_MOTOR)
colnames(c_edad_3) <- colnames(BASE_MOTOR)
colnames(c_edad_4) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_edad_1,c_edad_2, c_edad_3, c_edad_4)


# Quintil de ingreso del hogar

a_quintil <- function(x) {
  x <- ech_h_svy %>%
    filter(quintilesy == x & bd_region == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(menorH))
  x <- mean(x$colname)
}       

a_e_quintil <- numeric()

for(i in 1:5){
  a_e_quintil[i] <- a_quintil(x = i)
}     

c_quintil <- as.data.frame(a_e_quintil)
c_quintil_1 <- cbind(as.data.frame(c_quintil[1,1]), NOMINDICADOR, BASE_AUX)
c_quintil_1 <- c_quintil_1 %>% dplyr::mutate(QUINTIL = "Quintil 1")
c_quintil_2 <- cbind(as.data.frame(c_quintil[2,1]), NOMINDICADOR, BASE_AUX)
c_quintil_2 <- c_quintil_2 %>% dplyr::mutate(QUINTIL = "Quintil 2")
c_quintil_3 <- cbind(as.data.frame(c_quintil[3,1]), NOMINDICADOR, BASE_AUX)
c_quintil_3 <- c_quintil_3 %>% dplyr::mutate(QUINTIL = "Quintil 3")
c_quintil_4 <- cbind(as.data.frame(c_quintil[4,1]), NOMINDICADOR, BASE_AUX)
c_quintil_4 <- c_quintil_4 %>% dplyr::mutate(QUINTIL = "Quintil 4")
c_quintil_5 <- cbind(as.data.frame(c_quintil[5,1]), NOMINDICADOR, BASE_AUX)
c_quintil_5 <- c_quintil_5 %>% dplyr::mutate(QUINTIL = "Quintil 5")
colnames(c_quintil_1) <- colnames(BASE_MOTOR)
colnames(c_quintil_2) <- colnames(BASE_MOTOR)
colnames(c_quintil_3) <- colnames(BASE_MOTOR)
colnames(c_quintil_4) <- colnames(BASE_MOTOR)
colnames(c_quintil_5) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_quintil_1,c_quintil_2, c_quintil_3, c_quintil_4, c_quintil_5)

# Base motor

BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(CODIND = "512")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(CATEGORIA = "Composición y estructura de los hogares")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(PESTAÑA = "País urbano")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(PAÍS = "Uruguay")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(ANIO = 2023)
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(RESPONSABLE = "Sharon Katzkowicz")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(ASCENDENCIA = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(DEPARTAMENTOUY = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(URBANORURALUY = "Urbano (más de 5.000 habitantes)")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(POBRE = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(NSE = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(NIVELEDU = "Todos")
BASE_MOTOR <- as.data.frame(BASE_MOTOR)

BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(SEXO = case_when(SEXO == "" ~ "Todos", SEXO == "MUJERES"~"Mujeres", SEXO=="VARONES"~"Varones"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(QUINTIL = case_when(QUINTIL == "" ~ "Todos",
                                                               QUINTIL == "Quintil 1"~"Quintil 1", 
                                                               QUINTIL == "Quintil 2"~"Quintil 2",
                                                               QUINTIL == "Quintil 3"~"Quintil 3", 
                                                               QUINTIL == "Quintil 4"~"Quintil 4",
                                                               QUINTIL == "Quintil 5"~"Quintil 5"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(EDAD = case_when(EDAD == "" ~ "Todos",
                                                            EDAD == "Menor de 30 años"~"Menor de 30 años", 
                                                            EDAD == "Entre 30 y 44 años"~"Entre 30 y 44 años",
                                                            EDAD == "Entre 45 y 64 años"~"Entre 45 y 64 años", 
                                                            EDAD == "65 años o más"~"65 años o más"))


m512_pu <- BASE_MOTOR %>% select(CODIND,	
                                 NOMINDICADOR,	
                                 CATEGORIA,	
                                 PESTAÑA,	
                                 SEXO,	
                                 ASCENDENCIA,	
                                 QUINTIL,	
                                 DEPARTAMENTOUY,	
                                 URBANORURALUY,	
                                 EDAD,	
                                 POBRE,
                                 NSE,
                                 NIVELEDU,
                                 PAÍS,	
                                 ANIO,	
                                 VALOR,	
                                 RESPONSABLE)	


## Total país

# Total


NOMINDICADOR <- 
  c("Porcentaje de hogares en los que residen menores entre 0 y 17 años de edad")

a_mes <- ech_h_svy %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(menorH))
a_sem <- mean(as.numeric(a_mes$colname))

c_ano <- as.numeric(a_sem)

c_ano <- cbind(c_ano, NOMINDICADOR)
colnames(c_ano) <- c("VALOR", "NOMINDICADOR")
c_ano <- as.data.frame(c_ano)
BASE_MOTOR <- cbind(c_ano,BASE_AUX)



# Región

a_reg <- function(x) {
  x <- ech_h_svy %>%
    filter(bd_region == x) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(menorH))
  x <- mean(x$colname)
}       

a_e_reg <- numeric()

for(i in 1:3){
  a_e_reg[i] <- a_reg(x = i)
}     

c_reg <- as.data.frame(a_e_reg)
c_reg_1 <- cbind(as.data.frame(c_reg[2,1]), NOMINDICADOR, BASE_AUX)
c_reg_1 <- c_reg_1 %>% dplyr::mutate(URBANORURALUY = "URBANO (MENOS DE 5.000 HABITANTES)")
c_reg_2 <- cbind(as.data.frame(c_reg[3,1]), NOMINDICADOR, BASE_AUX)
c_reg_2 <- c_reg_2 %>% dplyr::mutate(URBANORURALUY = "RURAL DISPERSO")
colnames(c_reg_1) <- colnames(BASE_MOTOR)
colnames(c_reg_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_reg_1,c_reg_2)


# Sexo del jefe

a_jefe <- function(x) {
  x <- ech_h_svy %>%
    filter(sexojefe == x) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(menorH))
  x <- mean(x$colname)
}       

a_e_jefe <- numeric()

for(i in 1:2){
  a_e_jefe[i] <- a_jefe(x = i)
}     

c_jefe <- as.data.frame(a_e_jefe)
c_jefe_1 <- cbind(as.data.frame(c_jefe[1,1]), NOMINDICADOR, BASE_AUX)
c_jefe_1 <- c_jefe_1 %>% dplyr::mutate(SEXO = "VARONES")
c_jefe_2 <- cbind(as.data.frame(c_jefe[2,1]), NOMINDICADOR, BASE_AUX)
c_jefe_2 <- c_jefe_2 %>% dplyr::mutate(SEXO = "MUJERES")
colnames(c_jefe_1) <- colnames(BASE_MOTOR)
colnames(c_jefe_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_jefe_1,c_jefe_2)


# Tramo de edad

a_edad <- function(x) {
  x <- ech_h_svy %>%
    filter(tramo_edad2 == x) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(menorH))
  x <- mean(x$colname)
}       

a_e_edad <- numeric()

for(i in 1:4){
  a_e_edad[i] <- a_edad(x = i)
}     

c_edad <- as.data.frame(a_e_edad)
c_edad_1 <- cbind(as.data.frame(c_edad[1,1]), NOMINDICADOR, BASE_AUX)
c_edad_1 <- c_edad_1 %>% dplyr::mutate(EDAD = "Menor de 30 años")
c_edad_2 <- cbind(as.data.frame(c_edad[2,1]), NOMINDICADOR, BASE_AUX)
c_edad_2 <- c_edad_2 %>% dplyr::mutate(EDAD = "Entre 30 y 44 años")
c_edad_3 <- cbind(as.data.frame(c_edad[3,1]), NOMINDICADOR, BASE_AUX)
c_edad_3 <- c_edad_3 %>% dplyr::mutate(EDAD = "Entre 45 y 64 años")
c_edad_4 <- cbind(as.data.frame(c_edad[4,1]), NOMINDICADOR, BASE_AUX)
c_edad_4 <- c_edad_4 %>% dplyr::mutate(EDAD = "65 años o más")
colnames(c_edad_1) <- colnames(BASE_MOTOR)
colnames(c_edad_2) <- colnames(BASE_MOTOR)
colnames(c_edad_3) <- colnames(BASE_MOTOR)
colnames(c_edad_4) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_edad_1,c_edad_2, c_edad_3, c_edad_4)



# Ascendencia

a_ascen <- function(x) {
  x <- ech_h_svy %>%
    filter(bd_e29_1 == x) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(menorH))
  x <- mean(x$colname)
}       

a_e_ascen <- numeric()

for(i in 1:2){
  a_e_ascen[i] <- a_ascen(x = i)
}     

c_ascen <- as.data.frame(a_e_ascen)
c_ascen_1 <- cbind(as.data.frame(c_ascen[1,1]), NOMINDICADOR, BASE_AUX)
c_ascen_1 <- c_ascen_1 %>% dplyr::mutate(ASCENDENCIA = "AFRO")
c_ascen_2 <- cbind(as.data.frame(c_ascen[2,1]), NOMINDICADOR, BASE_AUX)
c_ascen_2 <- c_ascen_2 %>% dplyr::mutate(ASCENDENCIA = "NO AFRO")
colnames(c_ascen_1) <- colnames(BASE_MOTOR)
colnames(c_ascen_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_ascen_1,c_ascen_2)


# Quintil de ingreso del hogar

a_quintil <- function(x) {
  x <- ech_h_svy %>%
    filter(quintilesy == x) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(menorH))
  x <- mean(x$colname)
}       

a_e_quintil <- numeric()

for(i in 1:5){
  a_e_quintil[i] <- a_quintil(x = i)
}     

c_quintil <- as.data.frame(a_e_quintil)
c_quintil_1 <- cbind(as.data.frame(c_quintil[1,1]), NOMINDICADOR, BASE_AUX)
c_quintil_1 <- c_quintil_1 %>% dplyr::mutate(QUINTIL = "Quintil 1")
c_quintil_2 <- cbind(as.data.frame(c_quintil[2,1]), NOMINDICADOR, BASE_AUX)
c_quintil_2 <- c_quintil_2 %>% dplyr::mutate(QUINTIL = "Quintil 2")
c_quintil_3 <- cbind(as.data.frame(c_quintil[3,1]), NOMINDICADOR, BASE_AUX)
c_quintil_3 <- c_quintil_3 %>% dplyr::mutate(QUINTIL = "Quintil 3")
c_quintil_4 <- cbind(as.data.frame(c_quintil[4,1]), NOMINDICADOR, BASE_AUX)
c_quintil_4 <- c_quintil_4 %>% dplyr::mutate(QUINTIL = "Quintil 4")
c_quintil_5 <- cbind(as.data.frame(c_quintil[5,1]), NOMINDICADOR, BASE_AUX)
c_quintil_5 <- c_quintil_5 %>% dplyr::mutate(QUINTIL = "Quintil 5")
colnames(c_quintil_1) <- colnames(BASE_MOTOR)
colnames(c_quintil_2) <- colnames(BASE_MOTOR)
colnames(c_quintil_3) <- colnames(BASE_MOTOR)
colnames(c_quintil_4) <- colnames(BASE_MOTOR)
colnames(c_quintil_5) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_quintil_1,c_quintil_2, c_quintil_3, c_quintil_4, c_quintil_5)


# Pobreza

a_pobre <- function(x) {
  x <- ech_h_svy %>%
    filter(pobre_aux == x) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(menorH))
  x <- mean(x$colname)
}       

a_e_pobre <- numeric()

for(i in 1:2){
  a_e_pobre[i] <- a_pobre(x = i)
}     

c_pobre <- as.data.frame(a_e_pobre)
c_pobre_1 <- cbind(as.data.frame(c_pobre[1,1]), NOMINDICADOR, BASE_AUX)
c_pobre_1 <- c_pobre_1 %>% dplyr::mutate(POBRE = "POBRE")
c_pobre_2 <- cbind(as.data.frame(c_pobre[2,1]), NOMINDICADOR, BASE_AUX)
c_pobre_2 <- c_pobre_2 %>% dplyr::mutate(POBRE = "NO POBRE")
colnames(c_pobre_1) <- colnames(BASE_MOTOR)
colnames(c_pobre_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_pobre_1,c_pobre_2)


# Base motor

BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(CODIND = "512")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(CATEGORIA = "Composición y estructura de los hogares")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(PESTAÑA = "Total país")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(PAÍS = "Uruguay")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(ANIO = 2023)
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(RESPONSABLE = "Sharon Katzkowicz")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(DEPARTAMENTOUY = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(NSE = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(NIVELEDU = "Todos")
BASE_MOTOR <- as.data.frame(BASE_MOTOR)


BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(ASCENDENCIA = case_when(ASCENDENCIA == "AFRO" ~ "Afro", 
                                                                   ASCENDENCIA == "NO AFRO" ~ "No afro",
                                                                   ASCENDENCIA == "" ~ "Todos"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(URBANORURALUY = case_when(URBANORURALUY == "URBANO (MENOS DE 5.000 HABITANTES)" ~ "Urbano (menos de 5.000 habitantes)",
                                                                     URBANORURALUY == "RURAL DISPERSO" ~ "Rural disperso", 
                                                                     URBANORURALUY == "" ~ "Total país"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(SEXO = case_when(SEXO == "MUJERES" ~ "Mujeres", 
                                                            SEXO=="VARONES" ~ "Varones",
                                                            SEXO == "" ~ "Todos"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(QUINTIL = case_when(QUINTIL == "Quintil 1"~"Quintil 1", 
                                                               QUINTIL == "Quintil 2"~"Quintil 2",
                                                               QUINTIL == "Quintil 3"~"Quintil 3", 
                                                               QUINTIL == "Quintil 4"~"Quintil 4",
                                                               QUINTIL == "Quintil 5"~"Quintil 5",
                                                               QUINTIL == "" ~ "Todos"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(EDAD = case_when(EDAD == "Menor de 30 años"~"Menor de 30 años", 
                                                            EDAD == "Entre 30 y 44 años"~"Entre 30 y 44 años",
                                                            EDAD == "Entre 45 y 64 años"~"Entre 45 y 64 años", 
                                                            EDAD == "65 años o más"~"65 años o más",
                                                            EDAD == "" ~ "Todos"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(POBRE = case_when(POBRE == "POBRE" ~ "Pobre", 
                                                             POBRE == "NO POBRE" ~ "No pobre",
                                                             POBRE == "" ~ "Todos"))


m512_tp <- BASE_MOTOR %>% select(CODIND,	
                                 NOMINDICADOR,	
                                 CATEGORIA,	
                                 PESTAÑA,	
                                 SEXO,	
                                 ASCENDENCIA,	
                                 QUINTIL,	
                                 DEPARTAMENTOUY,	
                                 URBANORURALUY,	
                                 EDAD,	
                                 POBRE,
                                 NSE,
                                 NIVELEDU,
                                 PAÍS,	
                                 ANIO,	
                                 VALOR,	
                                 RESPONSABLE)




### 513 Porcentaje de hogares en los que residen mayores de 64 años

## País Urbano

# Total

NOMINDICADOR <- 
  c("Porcentaje de hogares en los que residen mayores de 64 años")

a_mes <- ech_h_svy %>%
  srvyr::filter(bd_region == 1) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(may64h))
a_sem <- mean(as.numeric(a_mes$colname))

c_ano <- as.numeric(a_sem)

c_ano <- cbind(c_ano, NOMINDICADOR)
colnames(c_ano) <- c("VALOR", "NOMINDICADOR")
c_ano <- as.data.frame(c_ano)
BASE_MOTOR <- cbind(c_ano,BASE_AUX)


# Sexo del jefe

a_jefe <- function(x) {
  x <- ech_h_svy %>%
    filter(sexojefe == x & bd_region == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(may64h))
  x <- mean(x$colname)
}       

a_e_jefe <- numeric()

for(i in 1:2){
  a_e_jefe[i] <- a_jefe(x = i)
}     


c_jefe <- as.data.frame(a_e_jefe)
c_jefe_1 <- cbind(as.data.frame(c_jefe[1,1]), NOMINDICADOR, BASE_AUX)
c_jefe_1 <- c_jefe_1 %>% dplyr::mutate(SEXO = "VARONES")
c_jefe_2 <- cbind(as.data.frame(c_jefe[2,1]), NOMINDICADOR, BASE_AUX)
c_jefe_2 <- c_jefe_2 %>% dplyr::mutate(SEXO = "MUJERES")
colnames(c_jefe_1) <- colnames(BASE_MOTOR)
colnames(c_jefe_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_jefe_1,c_jefe_2)


# Tramo de edad


a_edad <- function(x) {
  x <- ech_h_svy %>%
    filter(tramo_edad2 == x & bd_region == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(may64h))
  x <- mean(x$colname)
}       

a_e_edad <- numeric()

for(i in 1:4){
  a_e_edad[i] <- a_edad(x = i)
}     


c_edad <- as.data.frame(a_e_edad)
c_edad_1 <- cbind(as.data.frame(c_edad[1,1]), NOMINDICADOR, BASE_AUX)
c_edad_1 <- c_edad_1 %>% dplyr::mutate(EDAD = "Menor de 30 años")
c_edad_2 <- cbind(as.data.frame(c_edad[2,1]), NOMINDICADOR, BASE_AUX)
c_edad_2 <- c_edad_2 %>% dplyr::mutate(EDAD = "Entre 30 y 44 años")
c_edad_3 <- cbind(as.data.frame(c_edad[3,1]), NOMINDICADOR, BASE_AUX)
c_edad_3 <- c_edad_3 %>% dplyr::mutate(EDAD = "Entre 45 y 64 años")
c_edad_4 <- cbind(as.data.frame(c_edad[4,1]), NOMINDICADOR, BASE_AUX)
c_edad_4 <- c_edad_4 %>% dplyr::mutate(EDAD = "65 años o más")
colnames(c_edad_1) <- colnames(BASE_MOTOR)
colnames(c_edad_2) <- colnames(BASE_MOTOR)
colnames(c_edad_3) <- colnames(BASE_MOTOR)
colnames(c_edad_4) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_edad_1,c_edad_2, c_edad_3, c_edad_4)


# Quintil de ingreso del hogar

a_quintil <- function(x) {
  x <- ech_h_svy %>%
    filter(quintilesy == x & bd_region == 1) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(may64h))
  x <- mean(x$colname)
}       

a_e_quintil <- numeric()

for(i in 1:5){
  a_e_quintil[i] <- a_quintil(x = i)
}     
c_quintil <- as.data.frame(a_e_quintil)
c_quintil_1 <- cbind(as.data.frame(c_quintil[1,1]), NOMINDICADOR, BASE_AUX)
c_quintil_1 <- c_quintil_1 %>% dplyr::mutate(QUINTIL = "Quintil 1")
c_quintil_2 <- cbind(as.data.frame(c_quintil[2,1]), NOMINDICADOR, BASE_AUX)
c_quintil_2 <- c_quintil_2 %>% dplyr::mutate(QUINTIL = "Quintil 2")
c_quintil_3 <- cbind(as.data.frame(c_quintil[3,1]), NOMINDICADOR, BASE_AUX)
c_quintil_3 <- c_quintil_3 %>% dplyr::mutate(QUINTIL = "Quintil 3")
c_quintil_4 <- cbind(as.data.frame(c_quintil[4,1]), NOMINDICADOR, BASE_AUX)
c_quintil_4 <- c_quintil_4 %>% dplyr::mutate(QUINTIL = "Quintil 4")
c_quintil_5 <- cbind(as.data.frame(c_quintil[5,1]), NOMINDICADOR, BASE_AUX)
c_quintil_5 <- c_quintil_5 %>% dplyr::mutate(QUINTIL = "Quintil 5")
colnames(c_quintil_1) <- colnames(BASE_MOTOR)
colnames(c_quintil_2) <- colnames(BASE_MOTOR)
colnames(c_quintil_3) <- colnames(BASE_MOTOR)
colnames(c_quintil_4) <- colnames(BASE_MOTOR)
colnames(c_quintil_5) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_quintil_1,c_quintil_2, c_quintil_3, c_quintil_4, c_quintil_5)




# Base motor

BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(CODIND = "513")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(CATEGORIA = "Composición y estructura de los hogares")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(PESTAÑA = "País urbano")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(PAÍS = "Uruguay")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(ANIO = 2023)
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(RESPONSABLE = "Sharon Katzkowicz")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(ASCENDENCIA = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(DEPARTAMENTOUY = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(URBANORURALUY = "Urbano (más de 5.000 habitantes)")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(POBRE = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(NSE = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(NIVELEDU = "Todos")
BASE_MOTOR <- as.data.frame(BASE_MOTOR)

BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(SEXO = case_when(SEXO == "" ~ "Todos", SEXO == "MUJERES"~"Mujeres", SEXO=="VARONES"~"Varones"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(QUINTIL = case_when(QUINTIL == "" ~ "Todos",
                                                               QUINTIL == "Quintil 1"~"Quintil 1", 
                                                               QUINTIL == "Quintil 2"~"Quintil 2",
                                                               QUINTIL == "Quintil 3"~"Quintil 3", 
                                                               QUINTIL == "Quintil 4"~"Quintil 4",
                                                               QUINTIL == "Quintil 5"~"Quintil 5"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(EDAD = case_when(EDAD == "" ~ "Todos",
                                                            EDAD == "Menor de 30 años"~"Menor de 30 años", 
                                                            EDAD == "Entre 30 y 44 años"~"Entre 30 y 44 años",
                                                            EDAD == "Entre 45 y 64 años"~"Entre 45 y 64 años", 
                                                            EDAD == "65 años o más"~"65 años o más"))


m513_pu <- BASE_MOTOR %>% select(CODIND,	
                                 NOMINDICADOR,	
                                 CATEGORIA,	
                                 PESTAÑA,	
                                 SEXO,	
                                 ASCENDENCIA,	
                                 QUINTIL,	
                                 DEPARTAMENTOUY,	
                                 URBANORURALUY,	
                                 EDAD,	
                                 POBRE,
                                 NSE,
                                 NIVELEDU,
                                 PAÍS,	
                                 ANIO,	
                                 VALOR,	
                                 RESPONSABLE)	


## Total país

# Total


NOMINDICADOR <- 
  c("Porcentaje de hogares en los que residen mayores de 64 años")

a_mes <- ech_h_svy %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(may64h))
a_sem <- mean(as.numeric(a_mes$colname))

c_ano <- as.numeric(a_sem)

c_ano <- cbind(c_ano, NOMINDICADOR)
colnames(c_ano) <- c("VALOR", "NOMINDICADOR")
c_ano <- as.data.frame(c_ano)
BASE_MOTOR <- cbind(c_ano,BASE_AUX)



# Región

a_reg <- function(x) {
  x <- ech_h_svy %>%
    filter(bd_region == x) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(may64h))
  x <- mean(x$colname)
}       

a_e_reg <- numeric()

for(i in 1:3){
  a_e_reg[i] <- a_reg(x = i)
}     

c_reg <- as.data.frame(a_e_reg)
c_reg_1 <- cbind(as.data.frame(c_reg[2,1]), NOMINDICADOR, BASE_AUX)
c_reg_1 <- c_reg_1 %>% dplyr::mutate(URBANORURALUY = "URBANO (MENOS DE 5.000 HABITANTES)")
c_reg_2 <- cbind(as.data.frame(c_reg[3,1]), NOMINDICADOR, BASE_AUX)
c_reg_2 <- c_reg_2 %>% dplyr::mutate(URBANORURALUY = "RURAL DISPERSO")
colnames(c_reg_1) <- colnames(BASE_MOTOR)
colnames(c_reg_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_reg_1,c_reg_2)


# Sexo del jefe

a_jefe <- function(x) {
  x <- ech_h_svy %>%
    filter(sexojefe == x) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(may64h))
  x <- mean(x$colname)
}       

a_e_jefe <- numeric()

for(i in 1:2){
  a_e_jefe[i] <- a_jefe(x = i)
}     

c_jefe <- as.data.frame(a_e_jefe)
c_jefe_1 <- cbind(as.data.frame(c_jefe[1,1]), NOMINDICADOR, BASE_AUX)
c_jefe_1 <- c_jefe_1 %>% dplyr::mutate(SEXO = "VARONES")
c_jefe_2 <- cbind(as.data.frame(c_jefe[2,1]), NOMINDICADOR, BASE_AUX)
c_jefe_2 <- c_jefe_2 %>% dplyr::mutate(SEXO = "MUJERES")
colnames(c_jefe_1) <- colnames(BASE_MOTOR)
colnames(c_jefe_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_jefe_1,c_jefe_2)


# Tramo de edad

a_edad <- function(x) {
  x <- ech_h_svy %>%
    filter(tramo_edad2 == x) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(may64h))
  x <- mean(x$colname)
}       

a_e_edad <- numeric()

for(i in 1:4){
  a_e_edad[i] <- a_edad(x = i)
}     

c_edad <- as.data.frame(a_e_edad)
c_edad_1 <- cbind(as.data.frame(c_edad[1,1]), NOMINDICADOR, BASE_AUX)
c_edad_1 <- c_edad_1 %>% dplyr::mutate(EDAD = "Menor de 30 años")
c_edad_2 <- cbind(as.data.frame(c_edad[2,1]), NOMINDICADOR, BASE_AUX)
c_edad_2 <- c_edad_2 %>% dplyr::mutate(EDAD = "Entre 30 y 44 años")
c_edad_3 <- cbind(as.data.frame(c_edad[3,1]), NOMINDICADOR, BASE_AUX)
c_edad_3 <- c_edad_3 %>% dplyr::mutate(EDAD = "Entre 45 y 64 años")
c_edad_4 <- cbind(as.data.frame(c_edad[4,1]), NOMINDICADOR, BASE_AUX)
c_edad_4 <- c_edad_4 %>% dplyr::mutate(EDAD = "65 años o más")
colnames(c_edad_1) <- colnames(BASE_MOTOR)
colnames(c_edad_2) <- colnames(BASE_MOTOR)
colnames(c_edad_3) <- colnames(BASE_MOTOR)
colnames(c_edad_4) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_edad_1,c_edad_2, c_edad_3, c_edad_4)



# Ascendencia

a_ascen <- function(x) {
  x <- ech_h_svy %>%
    filter(bd_e29_1 == x) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(may64h))
  x <- mean(x$colname)
}       

a_e_ascen <- numeric()

for(i in 1:2){
  a_e_ascen[i] <- a_ascen(x = i)
}     

c_ascen <- as.data.frame(a_e_ascen)
c_ascen_1 <- cbind(as.data.frame(c_ascen[1,1]), NOMINDICADOR, BASE_AUX)
c_ascen_1 <- c_ascen_1 %>% dplyr::mutate(ASCENDENCIA = "AFRO")
c_ascen_2 <- cbind(as.data.frame(c_ascen[2,1]), NOMINDICADOR, BASE_AUX)
c_ascen_2 <- c_ascen_2 %>% dplyr::mutate(ASCENDENCIA = "NO AFRO")
colnames(c_ascen_1) <- colnames(BASE_MOTOR)
colnames(c_ascen_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_ascen_1,c_ascen_2)


# Quintil de ingreso del hogar

a_quintil <- function(x) {
  x <- ech_h_svy %>%
    filter(quintilesy == x) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(may64h))
  x <- mean(x$colname)
}       

a_e_quintil <- numeric()

for(i in 1:5){
  a_e_quintil[i] <- a_quintil(x = i)
}     

c_quintil <- as.data.frame(a_e_quintil)
c_quintil_1 <- cbind(as.data.frame(c_quintil[1,1]), NOMINDICADOR, BASE_AUX)
c_quintil_1 <- c_quintil_1 %>% dplyr::mutate(QUINTIL = "Quintil 1")
c_quintil_2 <- cbind(as.data.frame(c_quintil[2,1]), NOMINDICADOR, BASE_AUX)
c_quintil_2 <- c_quintil_2 %>% dplyr::mutate(QUINTIL = "Quintil 2")
c_quintil_3 <- cbind(as.data.frame(c_quintil[3,1]), NOMINDICADOR, BASE_AUX)
c_quintil_3 <- c_quintil_3 %>% dplyr::mutate(QUINTIL = "Quintil 3")
c_quintil_4 <- cbind(as.data.frame(c_quintil[4,1]), NOMINDICADOR, BASE_AUX)
c_quintil_4 <- c_quintil_4 %>% dplyr::mutate(QUINTIL = "Quintil 4")
c_quintil_5 <- cbind(as.data.frame(c_quintil[5,1]), NOMINDICADOR, BASE_AUX)
c_quintil_5 <- c_quintil_5 %>% dplyr::mutate(QUINTIL = "Quintil 5")
colnames(c_quintil_1) <- colnames(BASE_MOTOR)
colnames(c_quintil_2) <- colnames(BASE_MOTOR)
colnames(c_quintil_3) <- colnames(BASE_MOTOR)
colnames(c_quintil_4) <- colnames(BASE_MOTOR)
colnames(c_quintil_5) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_quintil_1,c_quintil_2, c_quintil_3, c_quintil_4, c_quintil_5)


# Pobreza

a_pobre <- function(x) {
  x <- ech_h_svy %>%
    filter(pobre_aux == x) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(may64h))
  x <- mean(x$colname)
}       

a_e_pobre <- numeric()

for(i in 1:2){
  a_e_pobre[i] <- a_pobre(x = i)
}     

c_pobre <- as.data.frame(a_e_pobre)
c_pobre_1 <- cbind(as.data.frame(c_pobre[1,1]), NOMINDICADOR, BASE_AUX)
c_pobre_1 <- c_pobre_1 %>% dplyr::mutate(POBRE = "POBRE")
c_pobre_2 <- cbind(as.data.frame(c_pobre[2,1]), NOMINDICADOR, BASE_AUX)
c_pobre_2 <- c_pobre_2 %>% dplyr::mutate(POBRE = "NO POBRE")
colnames(c_pobre_1) <- colnames(BASE_MOTOR)
colnames(c_pobre_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_pobre_1,c_pobre_2)


# Base motor

BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(CODIND = "513")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(CATEGORIA = "Composición y estructura de los hogares")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(PESTAÑA = "Total país")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(PAÍS = "Uruguay")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(ANIO = 2023)
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(RESPONSABLE = "Sharon Katzkowicz")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(DEPARTAMENTOUY = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(NSE = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(NIVELEDU = "Todos")
BASE_MOTOR <- as.data.frame(BASE_MOTOR)


BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(ASCENDENCIA = case_when(ASCENDENCIA == "AFRO" ~ "Afro", 
                                                                   ASCENDENCIA == "NO AFRO" ~ "No afro",
                                                                   ASCENDENCIA == "" ~ "Todos"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(URBANORURALUY = case_when(URBANORURALUY == "URBANO (MENOS DE 5.000 HABITANTES)" ~ "Urbano (menos de 5.000 habitantes)",
                                                                     URBANORURALUY == "RURAL DISPERSO" ~ "Rural disperso", 
                                                                     URBANORURALUY == "" ~ "Total país"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(SEXO = case_when(SEXO == "MUJERES" ~ "Mujeres", 
                                                            SEXO=="VARONES" ~ "Varones",
                                                            SEXO == "" ~ "Todos"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(QUINTIL = case_when(QUINTIL == "Quintil 1"~"Quintil 1", 
                                                               QUINTIL == "Quintil 2"~"Quintil 2",
                                                               QUINTIL == "Quintil 3"~"Quintil 3", 
                                                               QUINTIL == "Quintil 4"~"Quintil 4",
                                                               QUINTIL == "Quintil 5"~"Quintil 5",
                                                               QUINTIL == "" ~ "Todos"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(EDAD = case_when(EDAD == "Menor de 30 años"~"Menor de 30 años", 
                                                            EDAD == "Entre 30 y 44 años"~"Entre 30 y 44 años",
                                                            EDAD == "Entre 45 y 64 años"~"Entre 45 y 64 años", 
                                                            EDAD == "65 años o más"~"65 años o más",
                                                            EDAD == "" ~ "Todos"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(POBRE = case_when(POBRE == "POBRE" ~ "Pobre", 
                                                             POBRE == "NO POBRE" ~ "No pobre",
                                                             POBRE == "" ~ "Todos"))


m513_tp <- BASE_MOTOR %>% select(CODIND,	
                                 NOMINDICADOR,	
                                 CATEGORIA,	
                                 PESTAÑA,	
                                 SEXO,	
                                 ASCENDENCIA,	
                                 QUINTIL,	
                                 DEPARTAMENTOUY,	
                                 URBANORURALUY,	
                                 EDAD,	
                                 POBRE,
                                 NSE,
                                 NIVELEDU,
                                 PAÍS,	
                                 ANIO,	
                                 VALOR,	
                                 RESPONSABLE)



### 514 Tamaño medio de los hogares

## País Urbano

# Total

NOMINDICADOR <- 
  c("Tamaño medio de los hogares")

a_mes <- ech_h_svy %>%
  srvyr::filter(bd_region == 1 & is.na(n_int)==F) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(n_int))
a_sem <- mean(as.numeric(a_mes$colname))


c_ano <- as.numeric(a_sem)

c_ano <- cbind(c_ano, NOMINDICADOR)
colnames(c_ano) <- c("VALOR", "NOMINDICADOR")
c_ano <- as.data.frame(c_ano)
BASE_MOTOR <- cbind(c_ano,BASE_AUX)


# Sexo del jefe

a_jefe <- function(x) {
  x <- ech_h_svy %>%
    filter(sexojefe == x & bd_region == 1 & is.na(n_int)==F) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(n_int))
  x <- mean(x$colname)
}       

a_e_jefe <- numeric()

for(i in 1:2){
  a_e_jefe[i] <- a_jefe(x = i)
}     

c_jefe <- as.data.frame(a_e_jefe)
c_jefe_1 <- cbind(as.data.frame(c_jefe[1,1]), NOMINDICADOR, BASE_AUX)
c_jefe_1 <- c_jefe_1 %>% dplyr::mutate(SEXO = "VARONES")
c_jefe_2 <- cbind(as.data.frame(c_jefe[2,1]), NOMINDICADOR, BASE_AUX)
c_jefe_2 <- c_jefe_2 %>% dplyr::mutate(SEXO = "MUJERES")
colnames(c_jefe_1) <- colnames(BASE_MOTOR)
colnames(c_jefe_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_jefe_1,c_jefe_2)


# Tramo de edad


a_edad <- function(x) {
  x <- ech_h_svy %>%
    filter(tramo_edad2 == x & bd_region == 1 & is.na(n_int)==F) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(n_int))
  x <- mean(x$colname)
}       

a_e_edad <- numeric()

for(i in 1:4){
  a_e_edad[i] <- a_edad(x = i)
}     

c_edad <- as.data.frame(a_e_edad)
c_edad_1 <- cbind(as.data.frame(c_edad[1,1]), NOMINDICADOR, BASE_AUX)
c_edad_1 <- c_edad_1 %>% dplyr::mutate(EDAD = "Menor de 30 años")
c_edad_2 <- cbind(as.data.frame(c_edad[2,1]), NOMINDICADOR, BASE_AUX)
c_edad_2 <- c_edad_2 %>% dplyr::mutate(EDAD = "Entre 30 y 44 años")
c_edad_3 <- cbind(as.data.frame(c_edad[3,1]), NOMINDICADOR, BASE_AUX)
c_edad_3 <- c_edad_3 %>% dplyr::mutate(EDAD = "Entre 45 y 64 años")
c_edad_4 <- cbind(as.data.frame(c_edad[4,1]), NOMINDICADOR, BASE_AUX)
c_edad_4 <- c_edad_4 %>% dplyr::mutate(EDAD = "65 años o más")
colnames(c_edad_1) <- colnames(BASE_MOTOR)
colnames(c_edad_2) <- colnames(BASE_MOTOR)
colnames(c_edad_3) <- colnames(BASE_MOTOR)
colnames(c_edad_4) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_edad_1,c_edad_2, c_edad_3, c_edad_4)


# Quintil de ingreso del hogar

a_quintil <- function(x) {
  x <- ech_h_svy %>%
    filter(quintilesy == x & bd_region == 1 & is.na(n_int)==F) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(n_int))
  x <- mean(x$colname)
}       

a_e_quintil <- numeric()

for(i in 1:5){
  a_e_quintil[i] <- a_quintil(x = i)
}     

c_quintil <- as.data.frame(a_e_quintil)
c_quintil_1 <- cbind(as.data.frame(c_quintil[1,1]), NOMINDICADOR, BASE_AUX)
c_quintil_1 <- c_quintil_1 %>% dplyr::mutate(QUINTIL = "Quintil 1")
c_quintil_2 <- cbind(as.data.frame(c_quintil[2,1]), NOMINDICADOR, BASE_AUX)
c_quintil_2 <- c_quintil_2 %>% dplyr::mutate(QUINTIL = "Quintil 2")
c_quintil_3 <- cbind(as.data.frame(c_quintil[3,1]), NOMINDICADOR, BASE_AUX)
c_quintil_3 <- c_quintil_3 %>% dplyr::mutate(QUINTIL = "Quintil 3")
c_quintil_4 <- cbind(as.data.frame(c_quintil[4,1]), NOMINDICADOR, BASE_AUX)
c_quintil_4 <- c_quintil_4 %>% dplyr::mutate(QUINTIL = "Quintil 4")
c_quintil_5 <- cbind(as.data.frame(c_quintil[5,1]), NOMINDICADOR, BASE_AUX)
c_quintil_5 <- c_quintil_5 %>% dplyr::mutate(QUINTIL = "Quintil 5")
colnames(c_quintil_1) <- colnames(BASE_MOTOR)
colnames(c_quintil_2) <- colnames(BASE_MOTOR)
colnames(c_quintil_3) <- colnames(BASE_MOTOR)
colnames(c_quintil_4) <- colnames(BASE_MOTOR)
colnames(c_quintil_5) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_quintil_1,c_quintil_2, c_quintil_3, c_quintil_4, c_quintil_5)




# Base motor

BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(CODIND = "514")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(CATEGORIA = "Composición y estructura de los hogares")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(PESTAÑA = "País urbano")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(PAÍS = "Uruguay")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(ANIO = 2023)
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(RESPONSABLE = "Sharon Katzkowicz")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(ASCENDENCIA = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(DEPARTAMENTOUY = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(URBANORURALUY = "Urbano (más de 5.000 habitantes)")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(POBRE = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(NSE = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(NIVELEDU = "Todos")
BASE_MOTOR <- as.data.frame(BASE_MOTOR)

BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(SEXO = case_when(SEXO == "" ~ "Todos", SEXO == "MUJERES"~"Mujeres", SEXO=="VARONES"~"Varones"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(QUINTIL = case_when(QUINTIL == "" ~ "Todos",
                                                               QUINTIL == "Quintil 1"~"Quintil 1", 
                                                               QUINTIL == "Quintil 2"~"Quintil 2",
                                                               QUINTIL == "Quintil 3"~"Quintil 3", 
                                                               QUINTIL == "Quintil 4"~"Quintil 4",
                                                               QUINTIL == "Quintil 5"~"Quintil 5"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(EDAD = case_when(EDAD == "" ~ "Todos",
                                                            EDAD == "Menor de 30 años"~"Menor de 30 años", 
                                                            EDAD == "Entre 30 y 44 años"~"Entre 30 y 44 años",
                                                            EDAD == "Entre 45 y 64 años"~"Entre 45 y 64 años", 
                                                            EDAD == "65 años o más"~"65 años o más"))


m514_pu <- BASE_MOTOR %>% select(CODIND,	
                                 NOMINDICADOR,	
                                 CATEGORIA,	
                                 PESTAÑA,	
                                 SEXO,	
                                 ASCENDENCIA,	
                                 QUINTIL,	
                                 DEPARTAMENTOUY,	
                                 URBANORURALUY,	
                                 EDAD,	
                                 POBRE,
                                 NSE,
                                 NIVELEDU,
                                 PAÍS,	
                                 ANIO,	
                                 VALOR,	
                                 RESPONSABLE)	


## Total país

# Total


NOMINDICADOR <- 
  c("Tamaño medio de los hogares")

a_mes <- ech_h_svy %>%
  filter(is.na(n_int)==F) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(n_int))
a_sem <- mean(as.numeric(a_mes$colname))

c_ano <- as.numeric(a_sem)

c_ano <- cbind(c_ano, NOMINDICADOR)
colnames(c_ano) <- c("VALOR", "NOMINDICADOR")
c_ano <- as.data.frame(c_ano)
BASE_MOTOR <- cbind(c_ano,BASE_AUX)



# Región

a_reg <- function(x) {
  x <- ech_h_svy %>%
    filter(bd_region == x & is.na(n_int)==F) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(n_int))
  x <- mean(x$colname)
}       

a_e_reg <- numeric()

for(i in 1:3){
  a_e_reg[i] <- a_reg(x = i)
}     


c_reg <- as.data.frame(a_e_reg)
c_reg_1 <- cbind(as.data.frame(c_reg[2,1]), NOMINDICADOR, BASE_AUX)
c_reg_1 <- c_reg_1 %>% dplyr::mutate(URBANORURALUY = "URBANO (MENOS DE 5.000 HABITANTES)")
c_reg_2 <- cbind(as.data.frame(c_reg[3,1]), NOMINDICADOR, BASE_AUX)
c_reg_2 <- c_reg_2 %>% dplyr::mutate(URBANORURALUY = "RURAL DISPERSO")
colnames(c_reg_1) <- colnames(BASE_MOTOR)
colnames(c_reg_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_reg_1,c_reg_2)


# Sexo del jefe

a_jefe <- function(x) {
  x <- ech_h_svy %>%
    filter(sexojefe == x & is.na(n_int)==F) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(n_int))
  x <- mean(x$colname)
}       

a_e_jefe <- numeric()

for(i in 1:2){
  a_e_jefe[i] <- a_jefe(x = i)
}     

c_jefe <- as.data.frame(a_e_jefe)
c_jefe_1 <- cbind(as.data.frame(c_jefe[1,1]), NOMINDICADOR, BASE_AUX)
c_jefe_1 <- c_jefe_1 %>% dplyr::mutate(SEXO = "VARONES")
c_jefe_2 <- cbind(as.data.frame(c_jefe[2,1]), NOMINDICADOR, BASE_AUX)
c_jefe_2 <- c_jefe_2 %>% dplyr::mutate(SEXO = "MUJERES")
colnames(c_jefe_1) <- colnames(BASE_MOTOR)
colnames(c_jefe_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_jefe_1,c_jefe_2)


# Tramo de edad

a_edad <- function(x) {
  x <- ech_h_svy %>%
    filter(tramo_edad2 == x & is.na(n_int)==F) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(n_int))
  x <- mean(x$colname)
}       

a_e_edad <- numeric()

for(i in 1:4){
  a_e_edad[i] <- a_edad(x = i)
}     


c_edad <- as.data.frame(a_e_edad)
c_edad_1 <- cbind(as.data.frame(c_edad[1,1]), NOMINDICADOR, BASE_AUX)
c_edad_1 <- c_edad_1 %>% dplyr::mutate(EDAD = "Menor de 30 años")
c_edad_2 <- cbind(as.data.frame(c_edad[2,1]), NOMINDICADOR, BASE_AUX)
c_edad_2 <- c_edad_2 %>% dplyr::mutate(EDAD = "Entre 30 y 44 años")
c_edad_3 <- cbind(as.data.frame(c_edad[3,1]), NOMINDICADOR, BASE_AUX)
c_edad_3 <- c_edad_3 %>% dplyr::mutate(EDAD = "Entre 45 y 64 años")
c_edad_4 <- cbind(as.data.frame(c_edad[4,1]), NOMINDICADOR, BASE_AUX)
c_edad_4 <- c_edad_4 %>% dplyr::mutate(EDAD = "65 años o más")
colnames(c_edad_1) <- colnames(BASE_MOTOR)
colnames(c_edad_2) <- colnames(BASE_MOTOR)
colnames(c_edad_3) <- colnames(BASE_MOTOR)
colnames(c_edad_4) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_edad_1,c_edad_2, c_edad_3, c_edad_4)



# Ascendencia

a_ascen <- function(x) {
  x <- ech_h_svy %>%
    filter(bd_e29_1 == x & is.na(n_int)==F) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(n_int))
  x <- mean(x$colname)
}       

a_e_ascen <- numeric()

for(i in 1:2){
  a_e_ascen[i] <- a_ascen(x = i)
}     

c_ascen <- as.data.frame(a_e_ascen)
c_ascen_1 <- cbind(as.data.frame(c_ascen[1,1]), NOMINDICADOR, BASE_AUX)
c_ascen_1 <- c_ascen_1 %>% dplyr::mutate(ASCENDENCIA = "AFRO")
c_ascen_2 <- cbind(as.data.frame(c_ascen[2,1]), NOMINDICADOR, BASE_AUX)
c_ascen_2 <- c_ascen_2 %>% dplyr::mutate(ASCENDENCIA = "NO AFRO")
colnames(c_ascen_1) <- colnames(BASE_MOTOR)
colnames(c_ascen_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_ascen_1,c_ascen_2)


# Quintil de ingreso del hogar

a_quintil <- function(x) {
  x <- ech_h_svy %>%
    filter(quintilesy == x & is.na(n_int)==F) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(n_int))
  x <- mean(x$colname)
}       

a_e_quintil <- numeric()

for(i in 1:5){
  a_e_quintil[i] <- a_quintil(x = i)
}     

c_quintil <- as.data.frame(a_e_quintil)
c_quintil_1 <- cbind(as.data.frame(c_quintil[1,1]), NOMINDICADOR, BASE_AUX)
c_quintil_1 <- c_quintil_1 %>% dplyr::mutate(QUINTIL = "Quintil 1")
c_quintil_2 <- cbind(as.data.frame(c_quintil[2,1]), NOMINDICADOR, BASE_AUX)
c_quintil_2 <- c_quintil_2 %>% dplyr::mutate(QUINTIL = "Quintil 2")
c_quintil_3 <- cbind(as.data.frame(c_quintil[3,1]), NOMINDICADOR, BASE_AUX)
c_quintil_3 <- c_quintil_3 %>% dplyr::mutate(QUINTIL = "Quintil 3")
c_quintil_4 <- cbind(as.data.frame(c_quintil[4,1]), NOMINDICADOR, BASE_AUX)
c_quintil_4 <- c_quintil_4 %>% dplyr::mutate(QUINTIL = "Quintil 4")
c_quintil_5 <- cbind(as.data.frame(c_quintil[5,1]), NOMINDICADOR, BASE_AUX)
c_quintil_5 <- c_quintil_5 %>% dplyr::mutate(QUINTIL = "Quintil 5")
colnames(c_quintil_1) <- colnames(BASE_MOTOR)
colnames(c_quintil_2) <- colnames(BASE_MOTOR)
colnames(c_quintil_3) <- colnames(BASE_MOTOR)
colnames(c_quintil_4) <- colnames(BASE_MOTOR)
colnames(c_quintil_5) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_quintil_1,c_quintil_2, c_quintil_3, c_quintil_4, c_quintil_5)


# Pobreza

a_pobre <- function(x) {
  x <- ech_h_svy %>%
    filter(pobre_aux == x & is.na(n_int)==F) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(n_int))
  x <- mean(x$colname)
}       

a_e_pobre <- numeric()

for(i in 1:2){
  a_e_pobre[i] <- a_pobre(x = i)
}     

c_pobre <- as.data.frame(a_e_pobre)
c_pobre_1 <- cbind(as.data.frame(c_pobre[1,1]), NOMINDICADOR, BASE_AUX)
c_pobre_1 <- c_pobre_1 %>% dplyr::mutate(POBRE = "POBRE")
c_pobre_2 <- cbind(as.data.frame(c_pobre[2,1]), NOMINDICADOR, BASE_AUX)
c_pobre_2 <- c_pobre_2 %>% dplyr::mutate(POBRE = "NO POBRE")
colnames(c_pobre_1) <- colnames(BASE_MOTOR)
colnames(c_pobre_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_pobre_1,c_pobre_2)


# Base motor

BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(CODIND = "514")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(CATEGORIA = "Composición y estructura de los hogares")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(PESTAÑA = "Total país")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(PAÍS = "Uruguay")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(ANIO = 2023)
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(RESPONSABLE = "Sharon Katzkowicz")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(DEPARTAMENTOUY = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(NSE = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(NIVELEDU = "Todos")
BASE_MOTOR <- as.data.frame(BASE_MOTOR)


BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(ASCENDENCIA = case_when(ASCENDENCIA == "AFRO" ~ "Afro", 
                                                                   ASCENDENCIA == "NO AFRO" ~ "No afro",
                                                                   ASCENDENCIA == "" ~ "Todos"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(URBANORURALUY = case_when(URBANORURALUY == "URBANO (MENOS DE 5.000 HABITANTES)" ~ "Urbano (menos de 5.000 habitantes)",
                                                                     URBANORURALUY == "RURAL DISPERSO" ~ "Rural disperso", 
                                                                     URBANORURALUY == "" ~ "Total país"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(SEXO = case_when(SEXO == "MUJERES" ~ "Mujeres", 
                                                            SEXO=="VARONES" ~ "Varones",
                                                            SEXO == "" ~ "Todos"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(QUINTIL = case_when(QUINTIL == "Quintil 1"~"Quintil 1", 
                                                               QUINTIL == "Quintil 2"~"Quintil 2",
                                                               QUINTIL == "Quintil 3"~"Quintil 3", 
                                                               QUINTIL == "Quintil 4"~"Quintil 4",
                                                               QUINTIL == "Quintil 5"~"Quintil 5",
                                                               QUINTIL == "" ~ "Todos"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(EDAD = case_when(EDAD == "Menor de 30 años"~"Menor de 30 años", 
                                                            EDAD == "Entre 30 y 44 años"~"Entre 30 y 44 años",
                                                            EDAD == "Entre 45 y 64 años"~"Entre 45 y 64 años", 
                                                            EDAD == "65 años o más"~"65 años o más",
                                                            EDAD == "" ~ "Todos"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(POBRE = case_when(POBRE == "POBRE" ~ "Pobre", 
                                                             POBRE == "NO POBRE" ~ "No pobre",
                                                             POBRE == "" ~ "Todos"))


m514_tp <- BASE_MOTOR %>% select(CODIND,	
                                 NOMINDICADOR,	
                                 CATEGORIA,	
                                 PESTAÑA,	
                                 SEXO,	
                                 ASCENDENCIA,	
                                 QUINTIL,	
                                 DEPARTAMENTOUY,	
                                 URBANORURALUY,	
                                 EDAD,	
                                 POBRE,
                                 NSE,
                                 NIVELEDU,
                                 PAÍS,	
                                 ANIO,	
                                 VALOR,	
                                 RESPONSABLE)





### 515 Distribución porcentual de los hogares según cantidad de integrantes


## País Urbano

ech_h_svy_1 <- ech_h_svy %>%
  filter(bd_region == 1, n_int>0)

# Total


NOMINDICADOR <- 
  c("Distribución porcentual de los hogares según cantidad de integrantes (1 persona)",
    "Distribución porcentual de los hogares según cantidad de integrantes (2 personas)",
    "Distribución porcentual de los hogares según cantidad de integrantes (3 personas)",
    "Distribución porcentual de los hogares según cantidad de integrantes (4 personas)",
    "Distribución porcentual de los hogares según cantidad de integrantes (5 o más personas)")





var_int <- c("nint1", "nint2", "nint3", "nint4", "nint5")
a_sem <- sapply(ech_h_svy_1$variables %>% select(all_of(var_int)), function(x){
  ech_h_svy_1 %>%
    srvyr::summarise(stat = srvyr::survey_mean(x, na.rm = TRUE)) 
})


c_ano <- matrix(,5,1)
for (i in 1:5) {
  c_ano[i,1] <- c(as.numeric(c(a_sem[1,i])))
}

colnames(c_ano) <- c("VALOR")
c_ano <- cbind(c_ano, NOMINDICADOR)
c_ano <- as.data.frame(c_ano)
BASE_MOTOR <- cbind(c_ano,BASE_AUX)

# Sexo del jefe

a_jefe <- function(y) {
  base <- subset(ech_h_svy_1, sexojefe == y) 
  resultados  <-  sapply(base$variables %>% select(all_of(var_int)), function(x){
    base %>%
      srvyr::summarise(stat1 = srvyr::survey_mean(x, na.rm = TRUE))
    
  })
  y <- as.numeric(resultados[1,])
}

a_e_jefe = matrix(, nrow = 5, ncol = 2)

for(i in 1:2){
  a_e_jefe[,i] <- a_jefe(y = i)
}


c_jefe <- as.data.frame(a_e_jefe)
c_jefe_1 <- cbind(as.data.frame(c_jefe[,1]), NOMINDICADOR, BASE_AUX)
c_jefe_1 <- c_jefe_1 %>% dplyr::mutate(SEXO = "VARONES")
c_jefe_2 <- cbind(as.data.frame(c_jefe[,2]), NOMINDICADOR, BASE_AUX)
c_jefe_2 <- c_jefe_2 %>% dplyr::mutate(SEXO = "MUJERES")
colnames(c_jefe_1) <- colnames(BASE_MOTOR)
colnames(c_jefe_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_jefe_1,c_jefe_2)


# Tramo de edad

a_edad <- function(y) {
  base <- subset(ech_h_svy_1, tramo_edad2 == y) 
  resultados  <-  sapply(base$variables %>% select(all_of(var_int)), function(x){
    base %>%
      srvyr::summarise(stat1 = srvyr::survey_mean(x, na.rm = TRUE))
    
  })
  y <- as.numeric(resultados[1,])
}

a_e_edad = matrix(, nrow = 5, ncol = 4)

for(i in 1:4){
  a_e_edad[,i] <- a_edad(y = i)
}


c_edad <- as.data.frame(a_e_edad)

c_edad_1 <- cbind(as.data.frame(c_edad[,1]), NOMINDICADOR, BASE_AUX)
c_edad_1 <- c_edad_1 %>% dplyr::mutate(EDAD = "Menor de 30 años")
c_edad_2 <- cbind(as.data.frame(c_edad[,2]), NOMINDICADOR, BASE_AUX)
c_edad_2 <- c_edad_2 %>% dplyr::mutate(EDAD = "Entre 30 y 44 años")
c_edad_3 <- cbind(as.data.frame(c_edad[,3]), NOMINDICADOR, BASE_AUX)
c_edad_3 <- c_edad_3 %>% dplyr::mutate(EDAD = "Entre 45 y 64 años")
c_edad_4 <- cbind(as.data.frame(c_edad[,4]), NOMINDICADOR, BASE_AUX)
c_edad_4 <- c_edad_4 %>% dplyr::mutate(EDAD = "65 años o más")
colnames(c_edad_1) <- colnames(BASE_MOTOR)
colnames(c_edad_2) <- colnames(BASE_MOTOR)
colnames(c_edad_3) <- colnames(BASE_MOTOR)
colnames(c_edad_4) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_edad_1,c_edad_2, c_edad_3, c_edad_4)


# Quintil de ingreso del hogar
a_quintil <- function(y) {
  base <- subset(ech_h_svy_1, quintilesy == y) 
  resultados  <-  sapply(base$variables %>% select(all_of(var_int)), function(x){
    base %>%
      srvyr::summarise(stat1 = srvyr::survey_mean(x, na.rm = TRUE))
    
  })
  y <- as.numeric(resultados[1,])
}

a_e_quintil = matrix(, nrow = 5, ncol = 5)

for(i in 1:5){
  a_e_quintil[,i] <- a_quintil(y = i)
}


c_quintil <- as.data.frame(a_e_quintil)

c_quintil_1 <- cbind(as.data.frame(c_quintil[,1]), NOMINDICADOR, BASE_AUX)
c_quintil_1 <- c_quintil_1 %>% dplyr::mutate(QUINTIL = "Quintil 1")
c_quintil_2 <- cbind(as.data.frame(c_quintil[,2]), NOMINDICADOR, BASE_AUX)
c_quintil_2 <- c_quintil_2 %>% dplyr::mutate(QUINTIL = "Quintil 2")
c_quintil_3 <- cbind(as.data.frame(c_quintil[,3]), NOMINDICADOR, BASE_AUX)
c_quintil_3 <- c_quintil_3 %>% dplyr::mutate(QUINTIL = "Quintil 3")
c_quintil_4 <- cbind(as.data.frame(c_quintil[,4]), NOMINDICADOR, BASE_AUX)
c_quintil_4 <- c_quintil_4 %>% dplyr::mutate(QUINTIL = "Quintil 4")
c_quintil_5 <- cbind(as.data.frame(c_quintil[,5]), NOMINDICADOR, BASE_AUX)
c_quintil_5 <- c_quintil_5 %>% dplyr::mutate(QUINTIL = "Quintil 5")
colnames(c_quintil_1) <- colnames(BASE_MOTOR)
colnames(c_quintil_2) <- colnames(BASE_MOTOR)
colnames(c_quintil_3) <- colnames(BASE_MOTOR)
colnames(c_quintil_4) <- colnames(BASE_MOTOR)
colnames(c_quintil_5) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_quintil_1,c_quintil_2, c_quintil_3, c_quintil_4, c_quintil_5)




# Base motor

BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(CODIND = "515")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(CATEGORIA = "Composición y estructura de los hogares")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(PESTAÑA = "País urbano")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(PAÍS = "Uruguay")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(ANIO = 2023)
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(RESPONSABLE = "Sharon Katzkowicz")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(ASCENDENCIA = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(DEPARTAMENTOUY = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(URBANORURALUY = "Urbano (más de 5.000 habitantes)")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(POBRE = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(NSE = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(NIVELEDU = "Todos")
BASE_MOTOR <- as.data.frame(BASE_MOTOR)

BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(SEXO = case_when(SEXO == "" ~ "Todos", SEXO == "MUJERES"~"Mujeres", SEXO=="VARONES"~"Varones"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(QUINTIL = case_when(QUINTIL == "" ~ "Todos",
                                                               QUINTIL == "Quintil 1"~"Quintil 1", 
                                                               QUINTIL == "Quintil 2"~"Quintil 2",
                                                               QUINTIL == "Quintil 3"~"Quintil 3", 
                                                               QUINTIL == "Quintil 4"~"Quintil 4",
                                                               QUINTIL == "Quintil 5"~"Quintil 5"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(EDAD = case_when(EDAD == "" ~ "Todos",
                                                            EDAD == "Menor de 30 años"~"Menor de 30 años", 
                                                            EDAD == "Entre 30 y 44 años"~"Entre 30 y 44 años",
                                                            EDAD == "Entre 45 y 64 años"~"Entre 45 y 64 años", 
                                                            EDAD == "65 años o más"~"65 años o más"))


m515_pu <- BASE_MOTOR %>% select(CODIND,	
                                 NOMINDICADOR,	
                                 CATEGORIA,	
                                 PESTAÑA,	
                                 SEXO,	
                                 ASCENDENCIA,	
                                 QUINTIL,	
                                 DEPARTAMENTOUY,	
                                 URBANORURALUY,	
                                 EDAD,	
                                 POBRE,
                                 NSE,
                                 NIVELEDU,
                                 PAÍS,	
                                 ANIO,	
                                 VALOR,	
                                 RESPONSABLE)	


## Total país
ech_h_svy_1 <- ech_h_svy %>%
  filter(n_int>0)

# Total

NOMINDICADOR <- 
  c("Distribución porcentual de los hogares según cantidad de integrantes (1 persona)",
    "Distribución porcentual de los hogares según cantidad de integrantes (2 personas)",
    "Distribución porcentual de los hogares según cantidad de integrantes (3 personas)",
    "Distribución porcentual de los hogares según cantidad de integrantes (4 personas)",
    "Distribución porcentual de los hogares según cantidad de integrantes (5 o más personas)")


var_int <- c("nint1", "nint2", "nint3", "nint4", "nint5")
a_sem <- sapply(ech_h_svy_1$variables %>% select(all_of(var_int)), function(x){
  ech_h_svy_1 %>%
    srvyr::summarise(stat = srvyr::survey_mean(x, na.rm = TRUE)) 
})

c_ano <- matrix(,5,1)
for (i in 1:5) {
  c_ano[i,1] <- c(as.numeric(c(a_sem[1,i])))
}

colnames(c_ano) <- c("VALOR")
c_ano <- cbind(c_ano, NOMINDICADOR)
c_ano <- as.data.frame(c_ano)
BASE_MOTOR <- cbind(c_ano,BASE_AUX)


# Región

a_reg <- function(y) {
  base <- subset(ech_h_svy_1, bd_region == y) 
  resultados  <-  sapply(base$variables %>% select(all_of(var_int)), function(x){
    base %>%
      srvyr::summarise(stat1 = srvyr::survey_mean(x, na.rm = TRUE))
    
  })
  y <- as.numeric(resultados[1,])
}

a_e_reg = matrix(, nrow = 5, ncol = 3)

for(i in 1:3){
  a_e_reg[,i] <- a_reg(y = i)
}

c_reg <- as.data.frame(a_e_reg[,2:3])
c_reg_1 <- cbind(as.data.frame(c_reg[,1]), NOMINDICADOR, BASE_AUX)
c_reg_1 <- c_reg_1 %>% dplyr::mutate(URBANORURALUY = "URBANO (MENOS DE 5.000 HABITANTES)")
c_reg_2 <- cbind(as.data.frame(c_reg[,2]), NOMINDICADOR, BASE_AUX)
c_reg_2 <- c_reg_2 %>% dplyr::mutate(URBANORURALUY = "RURAL DISPERSO")
colnames(c_reg_1) <- colnames(BASE_MOTOR)
colnames(c_reg_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_reg_1,c_reg_2)


# Sexo del jefe

a_jefe <- function(y) {
  base <- subset(ech_h_svy_1, sexojefe == y) 
  resultados  <-  sapply(base$variables %>% select(all_of(var_int)), function(x){
    base %>%
      srvyr::summarise(stat1 = srvyr::survey_mean(x, na.rm = TRUE))
    
  })
  y <- as.numeric(resultados[1,])
}

a_e_jefe = matrix(, nrow = 5, ncol = 2)

for(i in 1:2){
  a_e_jefe[,i] <- a_jefe(y = i)
}

c_jefe <- as.data.frame(a_e_jefe)
c_jefe_1 <- cbind(as.data.frame(c_jefe[,1]), NOMINDICADOR, BASE_AUX)
c_jefe_1 <- c_jefe_1 %>% dplyr::mutate(SEXO = "VARONES")
c_jefe_2 <- cbind(as.data.frame(c_jefe[,2]), NOMINDICADOR, BASE_AUX)
c_jefe_2 <- c_jefe_2 %>% dplyr::mutate(SEXO = "MUJERES")
colnames(c_jefe_1) <- colnames(BASE_MOTOR)
colnames(c_jefe_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_jefe_1,c_jefe_2)


# Tramo de edad

a_edad <- function(y) {
  base <- subset(ech_h_svy_1, tramo_edad2 == y) 
  resultados  <-  sapply(base$variables %>% select(all_of(var_int)), function(x){
    base %>%
      srvyr::summarise(stat1 = srvyr::survey_mean(x, na.rm = TRUE))
    
  })
  y <- as.numeric(resultados[1,])
}

a_e_edad = matrix(, nrow = 5, ncol = 4)

for(i in 1:4){
  a_e_edad[,i] <- a_edad(y = i)
}

c_edad <- as.data.frame(a_e_edad)

c_edad_1 <- cbind(as.data.frame(c_edad[,1]), NOMINDICADOR, BASE_AUX)
c_edad_1 <- c_edad_1 %>% dplyr::mutate(EDAD = "Menor de 30 años")
c_edad_2 <- cbind(as.data.frame(c_edad[,2]), NOMINDICADOR, BASE_AUX)
c_edad_2 <- c_edad_2 %>% dplyr::mutate(EDAD = "Entre 30 y 44 años")
c_edad_3 <- cbind(as.data.frame(c_edad[,3]), NOMINDICADOR, BASE_AUX)
c_edad_3 <- c_edad_3 %>% dplyr::mutate(EDAD = "Entre 45 y 64 años")
c_edad_4 <- cbind(as.data.frame(c_edad[,4]), NOMINDICADOR, BASE_AUX)
c_edad_4 <- c_edad_4 %>% dplyr::mutate(EDAD = "65 años o más")
colnames(c_edad_1) <- colnames(BASE_MOTOR)
colnames(c_edad_2) <- colnames(BASE_MOTOR)
colnames(c_edad_3) <- colnames(BASE_MOTOR)
colnames(c_edad_4) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_edad_1,c_edad_2, c_edad_3, c_edad_4)



# Ascendencia

a_ascen <- function(y) {
  base <- subset(ech_h_svy_1, bd_e29_1 == y) 
  resultados  <-  sapply(base$variables %>% select(all_of(var_int)), function(x){
    base %>%
      srvyr::summarise(stat1 = srvyr::survey_mean(x, na.rm = TRUE))
    
  })
  y <- as.numeric(resultados[1,])
}

a_e_ascen = matrix(, nrow = 5, ncol = 2)

for(i in 1:2){
  a_e_ascen[,i] <- a_ascen(y = i)
}

c_ascen <- as.data.frame(a_e_ascen)
c_ascen_1 <- cbind(as.data.frame(c_ascen[,1]), NOMINDICADOR, BASE_AUX)
c_ascen_1 <- c_ascen_1 %>% dplyr::mutate(ASCENDENCIA = "AFRO")
c_ascen_2 <- cbind(as.data.frame(c_ascen[,2]), NOMINDICADOR, BASE_AUX)
c_ascen_2 <- c_ascen_2 %>% dplyr::mutate(ASCENDENCIA = "NO AFRO")
colnames(c_ascen_1) <- colnames(BASE_MOTOR)
colnames(c_ascen_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_ascen_1,c_ascen_2)


# Quintil de ingreso del hogar
a_quintil <- function(y) {
  base <- subset(ech_h_svy_1, quintilesy == y) 
  resultados  <-  sapply(base$variables %>% select(all_of(var_int)), function(x){
    base %>%
      srvyr::summarise(stat1 = srvyr::survey_mean(x, na.rm = TRUE))
    
  })
  y <- as.numeric(resultados[1,])
}

a_e_quintil = matrix(, nrow = 5, ncol = 5)

for(i in 1:5){
  a_e_quintil[,i] <- a_quintil(y = i)
}

c_quintil <- as.data.frame(a_e_quintil)

c_quintil_1 <- cbind(as.data.frame(c_quintil[,1]), NOMINDICADOR, BASE_AUX)
c_quintil_1 <- c_quintil_1 %>% dplyr::mutate(QUINTIL = "Quintil 1")
c_quintil_2 <- cbind(as.data.frame(c_quintil[,2]), NOMINDICADOR, BASE_AUX)
c_quintil_2 <- c_quintil_2 %>% dplyr::mutate(QUINTIL = "Quintil 2")
c_quintil_3 <- cbind(as.data.frame(c_quintil[,3]), NOMINDICADOR, BASE_AUX)
c_quintil_3 <- c_quintil_3 %>% dplyr::mutate(QUINTIL = "Quintil 3")
c_quintil_4 <- cbind(as.data.frame(c_quintil[,4]), NOMINDICADOR, BASE_AUX)
c_quintil_4 <- c_quintil_4 %>% dplyr::mutate(QUINTIL = "Quintil 4")
c_quintil_5 <- cbind(as.data.frame(c_quintil[,5]), NOMINDICADOR, BASE_AUX)
c_quintil_5 <- c_quintil_5 %>% dplyr::mutate(QUINTIL = "Quintil 5")
colnames(c_quintil_1) <- colnames(BASE_MOTOR)
colnames(c_quintil_2) <- colnames(BASE_MOTOR)
colnames(c_quintil_3) <- colnames(BASE_MOTOR)
colnames(c_quintil_4) <- colnames(BASE_MOTOR)
colnames(c_quintil_5) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_quintil_1,c_quintil_2, c_quintil_3, c_quintil_4, c_quintil_5)


# Pobreza

a_pobre <- function(y) {
  base <- subset(ech_h_svy_1, pobre_aux == y) 
  resultados  <-  sapply(base$variables %>% select(all_of(var_int)), function(x){
    base %>%
      srvyr::summarise(stat1 = srvyr::survey_mean(x, na.rm = TRUE))
    
  })
  y <- as.numeric(resultados[1,])
}

a_e_pobre = matrix(, nrow = 5, ncol = 2)

for(i in 1:2){
  a_e_pobre[,i] <- a_pobre(y = i)
}

c_pobre <- as.data.frame(a_e_pobre)
c_pobre_1 <- cbind(as.data.frame(c_pobre[,1]), NOMINDICADOR, BASE_AUX)
c_pobre_1 <- c_pobre_1 %>% dplyr::mutate(POBRE = "POBRE")
c_pobre_2 <- cbind(as.data.frame(c_pobre[,2]), NOMINDICADOR, BASE_AUX)
c_pobre_2 <- c_pobre_2 %>% dplyr::mutate(POBRE = "NO POBRE")
colnames(c_pobre_1) <- colnames(BASE_MOTOR)
colnames(c_pobre_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_pobre_1,c_pobre_2)


# Base motor

BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(CODIND = "515")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(CATEGORIA = "Composición y estructura de los hogares")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(PESTAÑA = "Total país")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(PAÍS = "Uruguay")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(ANIO = 2023)
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(RESPONSABLE = "Sharon Katzkowicz")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(DEPARTAMENTOUY = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(NSE = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(NIVELEDU = "Todos")
BASE_MOTOR <- as.data.frame(BASE_MOTOR)


BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(ASCENDENCIA = case_when(ASCENDENCIA == "AFRO" ~ "Afro", 
                                                                   ASCENDENCIA == "NO AFRO" ~ "No afro",
                                                                   ASCENDENCIA == "" ~ "Todos"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(URBANORURALUY = case_when(URBANORURALUY == "URBANO (MENOS DE 5.000 HABITANTES)" ~ "Urbano (menos de 5.000 habitantes)",
                                                                     URBANORURALUY == "RURAL DISPERSO" ~ "Rural disperso", 
                                                                     URBANORURALUY == "" ~ "Total país"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(SEXO = case_when(SEXO == "MUJERES" ~ "Mujeres", 
                                                            SEXO=="VARONES" ~ "Varones",
                                                            SEXO == "" ~ "Todos"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(QUINTIL = case_when(QUINTIL == "Quintil 1"~"Quintil 1", 
                                                               QUINTIL == "Quintil 2"~"Quintil 2",
                                                               QUINTIL == "Quintil 3"~"Quintil 3", 
                                                               QUINTIL == "Quintil 4"~"Quintil 4",
                                                               QUINTIL == "Quintil 5"~"Quintil 5",
                                                               QUINTIL == "" ~ "Todos"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(EDAD = case_when(EDAD == "Menor de 30 años"~"Menor de 30 años", 
                                                            EDAD == "Entre 30 y 44 años"~"Entre 30 y 44 años",
                                                            EDAD == "Entre 45 y 64 años"~"Entre 45 y 64 años", 
                                                            EDAD == "65 años o más"~"65 años o más",
                                                            EDAD == "" ~ "Todos"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(POBRE = case_when(POBRE == "POBRE" ~ "Pobre", 
                                                             POBRE == "NO POBRE" ~ "No pobre",
                                                             POBRE == "" ~ "Todos"))


m515_tp <- BASE_MOTOR %>% select(CODIND,	
                                 NOMINDICADOR,	
                                 CATEGORIA,	
                                 PESTAÑA,	
                                 SEXO,	
                                 ASCENDENCIA,	
                                 QUINTIL,	
                                 DEPARTAMENTOUY,	
                                 URBANORURALUY,	
                                 EDAD,	
                                 POBRE,
                                 NSE,
                                 NIVELEDU,
                                 PAÍS,	
                                 ANIO,	
                                 VALOR,	
                                 RESPONSABLE)




### 516 Distribución porcentual de hogares con menores de 15 años según tipo de hogar


## País Urbano

ech_h_svy_1_men <- ech_h_svy %>%
  filter(bd_region == 1 & men15h==1 & tipo_hogar>0)

# Total

NOMINDICADOR <- 
  c("Distribución porcentual de hogares con menores de 15 años según tipo de hogar (Unipersonal)",
    "Distribución porcentual de hogares con menores de 15 años según tipo de hogar (Pareja sin hijos)", 
    "Distribución porcentual de hogares con menores de 15 años según tipo de hogar (Biparental)",
    "Distribución porcentual de hogares con menores de 15 años según tipo de hogar (Monoparental femenino)",
    "Distribución porcentual de hogares con menores de 15 años según tipo de hogar (Monoparental masculino)",
    "Distribución porcentual de hogares con menores de 15 años según tipo de hogar (Extendido o compuesto)", 
    "Distribución porcentual de hogares con menores de 15 años según tipo de hogar (Extendido con núcleo monoparental)", 
    "Distribución porcentual de hogares con menores de 15 años según tipo de hogar (Sin núcleo)" )

var_int <- c("tipo_hogar1", "tipo_hogar2", "tipo_hogar3", "tipo_hogar4", "tipo_hogar5", "tipo_hogar6", "tipo_hogar7", "tipo_hogar8")
a_sem <- sapply(ech_h_svy_1_men$variables %>% select(all_of(var_int)), function(x){
  ech_h_svy_1_men %>%
    srvyr::summarise(stat = srvyr::survey_mean(x, na.rm = TRUE)) 
})

c_ano <- matrix(,8,1)
for (i in 1:8) {
  c_ano[i,1] <- c(as.numeric(c(a_sem[1,i])))
}

colnames(c_ano) <- c("VALOR")
c_ano <- cbind(c_ano, NOMINDICADOR)
c_ano <- as.data.frame(c_ano)
BASE_MOTOR <- cbind(c_ano,BASE_AUX)

# Sexo del jefe

a_jefe <- function(y) {
  base <- subset(ech_h_svy_1_men, sexojefe == y) 
  resultados  <-  sapply(base$variables %>% select(all_of(var_int)), function(x){
    base %>%
      srvyr::summarise(stat1 = srvyr::survey_mean(x, na.rm = TRUE))
    
  })
  y <- as.numeric(resultados[1,])
}

a_e_jefe = matrix(, nrow = 8, ncol = 2)

for(i in 1:2){
  a_e_jefe[,i] <- a_jefe(y = i)
}

c_jefe <- as.data.frame(a_e_jefe)
c_jefe_1 <- cbind(as.data.frame(c_jefe[,1]), NOMINDICADOR, BASE_AUX)
c_jefe_1 <- c_jefe_1 %>% dplyr::mutate(SEXO = "VARONES")
c_jefe_2 <- cbind(as.data.frame(c_jefe[,2]), NOMINDICADOR, BASE_AUX)
c_jefe_2 <- c_jefe_2 %>% dplyr::mutate(SEXO = "MUJERES")
colnames(c_jefe_1) <- colnames(BASE_MOTOR)
colnames(c_jefe_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_jefe_1,c_jefe_2)


# Tramo de edad

a_edad <- function(y) {
  base <- subset(ech_h_svy_1_men, tramo_edad2 == y) 
  resultados  <-  sapply(base$variables %>% select(all_of(var_int)), function(x){
    base %>%
      srvyr::summarise(stat1 = srvyr::survey_mean(x, na.rm = TRUE))
    
  })
  y <- as.numeric(resultados[1,])
}

a_e_edad = matrix(, nrow = 8, ncol = 4)

for(i in 1:4){
  a_e_edad[,i] <- a_edad(y = i)
}

c_edad <- as.data.frame(a_e_edad)

c_edad_1 <- cbind(as.data.frame(c_edad[,1]), NOMINDICADOR, BASE_AUX)
c_edad_1 <- c_edad_1 %>% dplyr::mutate(EDAD = "Menor de 30 años")
c_edad_2 <- cbind(as.data.frame(c_edad[,2]), NOMINDICADOR, BASE_AUX)
c_edad_2 <- c_edad_2 %>% dplyr::mutate(EDAD = "Entre 30 y 44 años")
c_edad_3 <- cbind(as.data.frame(c_edad[,3]), NOMINDICADOR, BASE_AUX)
c_edad_3 <- c_edad_3 %>% dplyr::mutate(EDAD = "Entre 45 y 64 años")
c_edad_4 <- cbind(as.data.frame(c_edad[,4]), NOMINDICADOR, BASE_AUX)
c_edad_4 <- c_edad_4 %>% dplyr::mutate(EDAD = "65 años o más")
colnames(c_edad_1) <- colnames(BASE_MOTOR)
colnames(c_edad_2) <- colnames(BASE_MOTOR)
colnames(c_edad_3) <- colnames(BASE_MOTOR)
colnames(c_edad_4) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_edad_1,c_edad_2, c_edad_3, c_edad_4)


# Quintil de ingreso del hogar
a_quintil <- function(y) {
  base <- subset(ech_h_svy_1_men, quintilesy == y) 
  resultados  <-  sapply(base$variables %>% select(all_of(var_int)), function(x){
    base %>%
      srvyr::summarise(stat1 = srvyr::survey_mean(x, na.rm = TRUE))
    
  })
  y <- as.numeric(resultados[1,])
}

a_e_quintil = matrix(, nrow = 8, ncol = 5)

for(i in 1:5){
  a_e_quintil[,i] <- a_quintil(y = i)
}

c_quintil <- as.data.frame(a_e_quintil)

c_quintil_1 <- cbind(as.data.frame(c_quintil[,1]), NOMINDICADOR, BASE_AUX)
c_quintil_1 <- c_quintil_1 %>% dplyr::mutate(QUINTIL = "Quintil 1")
c_quintil_2 <- cbind(as.data.frame(c_quintil[,2]), NOMINDICADOR, BASE_AUX)
c_quintil_2 <- c_quintil_2 %>% dplyr::mutate(QUINTIL = "Quintil 2")
c_quintil_3 <- cbind(as.data.frame(c_quintil[,3]), NOMINDICADOR, BASE_AUX)
c_quintil_3 <- c_quintil_3 %>% dplyr::mutate(QUINTIL = "Quintil 3")
c_quintil_4 <- cbind(as.data.frame(c_quintil[,4]), NOMINDICADOR, BASE_AUX)
c_quintil_4 <- c_quintil_4 %>% dplyr::mutate(QUINTIL = "Quintil 4")
c_quintil_5 <- cbind(as.data.frame(c_quintil[,5]), NOMINDICADOR, BASE_AUX)
c_quintil_5 <- c_quintil_5 %>% dplyr::mutate(QUINTIL = "Quintil 5")
colnames(c_quintil_1) <- colnames(BASE_MOTOR)
colnames(c_quintil_2) <- colnames(BASE_MOTOR)
colnames(c_quintil_3) <- colnames(BASE_MOTOR)
colnames(c_quintil_4) <- colnames(BASE_MOTOR)
colnames(c_quintil_5) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_quintil_1,c_quintil_2, c_quintil_3, c_quintil_4, c_quintil_5)




# Base motor

BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(CODIND = "516")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(CATEGORIA = "Composición y estructura de los hogares")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(PESTAÑA = "País urbano")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(PAÍS = "Uruguay")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(ANIO = 2023)
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(RESPONSABLE = "Sharon Katzkowicz")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(ASCENDENCIA = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(DEPARTAMENTOUY = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(URBANORURALUY = "Urbano (más de 5.000 habitantes)")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(POBRE = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(NSE = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(NIVELEDU = "Todos")

BASE_MOTOR <- as.data.frame(BASE_MOTOR)

BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(SEXO = case_when(SEXO == "" ~ "Todos", SEXO == "MUJERES"~"Mujeres", SEXO=="VARONES"~"Varones"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(QUINTIL = case_when(QUINTIL == "" ~ "Todos",
                                                               QUINTIL == "Quintil 1"~"Quintil 1", 
                                                               QUINTIL == "Quintil 2"~"Quintil 2",
                                                               QUINTIL == "Quintil 3"~"Quintil 3", 
                                                               QUINTIL == "Quintil 4"~"Quintil 4",
                                                               QUINTIL == "Quintil 5"~"Quintil 5"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(EDAD = case_when(EDAD == "" ~ "Todos",
                                                            EDAD == "Menor de 30 años"~"Menor de 30 años", 
                                                            EDAD == "Entre 30 y 44 años"~"Entre 30 y 44 años",
                                                            EDAD == "Entre 45 y 64 años"~"Entre 45 y 64 años", 
                                                            EDAD == "65 años o más"~"65 años o más"))


m516_pu <- BASE_MOTOR %>% select(CODIND,	
                                 NOMINDICADOR,	
                                 CATEGORIA,	
                                 PESTAÑA,	
                                 SEXO,	
                                 ASCENDENCIA,	
                                 QUINTIL,	
                                 DEPARTAMENTOUY,	
                                 URBANORURALUY,	
                                 EDAD,	
                                 POBRE,
                                 NSE,
                                 NIVELEDU,
                                 PAÍS,	
                                 ANIO,	
                                 VALOR,	
                                 RESPONSABLE)	


## Total país


ech_h_men_svy_1 <- ech_h_svy %>%
  filter(men15h==1 & tipo_hogar>0)

# Total

NOMINDICADOR <- 
  c("Distribución porcentual de hogares con menores de 15 años según tipo de hogar (Unipersonal)",
    "Distribución porcentual de hogares con menores de 15 años según tipo de hogar (Pareja sin hijos)", 
    "Distribución porcentual de hogares con menores de 15 años según tipo de hogar (Biparental)",
    "Distribución porcentual de hogares con menores de 15 años según tipo de hogar (Monoparental femenino)",
    "Distribución porcentual de hogares con menores de 15 años según tipo de hogar (Monoparental masculino)",
    "Distribución porcentual de hogares con menores de 15 años según tipo de hogar (Extendido o compuesto)", 
    "Distribución porcentual de hogares con menores de 15 años según tipo de hogar (Extendido con núcleo monoparental)", 
    "Distribución porcentual de hogares con menores de 15 años según tipo de hogar (Sin núcleo)" )

var_int <- c("tipo_hogar1", "tipo_hogar2", "tipo_hogar3", "tipo_hogar4", "tipo_hogar5", "tipo_hogar6", "tipo_hogar7", "tipo_hogar8")
a_sem <- sapply(ech_h_men_svy_1$variables %>% select(all_of(var_int)), function(x){
  ech_h_men_svy_1 %>%
    srvyr::summarise(stat = srvyr::survey_mean(x, na.rm = TRUE)) 
})

c_ano <- matrix(,8,1)
for (i in 1:8) {
  c_ano[i,1] <- c(as.numeric(c(a_sem[1,i])))
}

colnames(c_ano) <- c("VALOR")
c_ano <- cbind(c_ano, NOMINDICADOR)
c_ano <- as.data.frame(c_ano)
BASE_MOTOR <- cbind(c_ano,BASE_AUX)


# Región

a_reg <- function(y) {
  base <- subset(ech_h_men_svy_1, bd_region == y) 
  resultados  <-  sapply(base$variables %>% select(all_of(var_int)), function(x){
    base %>%
      srvyr::summarise(stat1 = srvyr::survey_mean(x, na.rm = TRUE))
    
  })
  y <- as.numeric(resultados[1,])
}

a_e_reg = matrix(, nrow = 8, ncol = 3)

for(i in 1:3){
  a_e_reg[,i] <- a_reg(y = i)
}

c_reg <- as.data.frame(a_e_reg[,2:3])
c_reg_1 <- cbind(as.data.frame(c_reg[,1]), NOMINDICADOR, BASE_AUX)
c_reg_1 <- c_reg_1 %>% dplyr::mutate(URBANORURALUY = "URBANO (MENOS DE 5.000 HABITANTES)")
c_reg_2 <- cbind(as.data.frame(c_reg[,2]), NOMINDICADOR, BASE_AUX)
c_reg_2 <- c_reg_2 %>% dplyr::mutate(URBANORURALUY = "RURAL DISPERSO")
colnames(c_reg_1) <- colnames(BASE_MOTOR)
colnames(c_reg_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_reg_1,c_reg_2)


# Sexo del jefe

a_jefe <- function(y) {
  base <- subset(ech_h_men_svy_1, sexojefe == y) 
  resultados  <-  sapply(base$variables %>% select(all_of(var_int)), function(x){
    base %>%
      srvyr::summarise(stat1 = srvyr::survey_mean(x, na.rm = TRUE))
    
  })
  y <- as.numeric(resultados[1,])
}

a_e_jefe = matrix(, nrow = 8, ncol = 2)

for(i in 1:2){
  a_e_jefe[,i] <- a_jefe(y = i)
}


c_jefe <- as.data.frame(a_e_jefe)
c_jefe_1 <- cbind(as.data.frame(c_jefe[,1]), NOMINDICADOR, BASE_AUX)
c_jefe_1 <- c_jefe_1 %>% dplyr::mutate(SEXO = "VARONES")
c_jefe_2 <- cbind(as.data.frame(c_jefe[,2]), NOMINDICADOR, BASE_AUX)
c_jefe_2 <- c_jefe_2 %>% dplyr::mutate(SEXO = "MUJERES")
colnames(c_jefe_1) <- colnames(BASE_MOTOR)
colnames(c_jefe_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_jefe_1,c_jefe_2)


# Tramo de edad

a_edad <- function(y) {
  base <- subset(ech_h_men_svy_1, tramo_edad2 == y) 
  resultados  <-  sapply(base$variables %>% select(all_of(var_int)), function(x){
    base %>%
      srvyr::summarise(stat1 = srvyr::survey_mean(x, na.rm = TRUE))
    
  })
  y <- as.numeric(resultados[1,])
}

a_e_edad = matrix(, nrow = 8, ncol = 4)

for(i in 1:4){
  a_e_edad[,i] <- a_edad(y = i)
}


c_edad <- as.data.frame(a_e_edad)

c_edad_1 <- cbind(as.data.frame(c_edad[,1]), NOMINDICADOR, BASE_AUX)
c_edad_1 <- c_edad_1 %>% dplyr::mutate(EDAD = "Menor de 30 años")
c_edad_2 <- cbind(as.data.frame(c_edad[,2]), NOMINDICADOR, BASE_AUX)
c_edad_2 <- c_edad_2 %>% dplyr::mutate(EDAD = "Entre 30 y 44 años")
c_edad_3 <- cbind(as.data.frame(c_edad[,3]), NOMINDICADOR, BASE_AUX)
c_edad_3 <- c_edad_3 %>% dplyr::mutate(EDAD = "Entre 45 y 64 años")
c_edad_4 <- cbind(as.data.frame(c_edad[,4]), NOMINDICADOR, BASE_AUX)
c_edad_4 <- c_edad_4 %>% dplyr::mutate(EDAD = "65 años o más")
colnames(c_edad_1) <- colnames(BASE_MOTOR)
colnames(c_edad_2) <- colnames(BASE_MOTOR)
colnames(c_edad_3) <- colnames(BASE_MOTOR)
colnames(c_edad_4) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_edad_1,c_edad_2, c_edad_3, c_edad_4)



# Ascendencia

a_ascen <- function(y) {
  base <- subset(ech_h_men_svy_1, bd_e29_1 == y) 
  resultados  <-  sapply(base$variables %>% select(all_of(var_int)), function(x){
    base %>%
      srvyr::summarise(stat1 = srvyr::survey_mean(x, na.rm = TRUE))
    
  })
  y <- as.numeric(resultados[1,])
}

a_e_ascen = matrix(, nrow = 8, ncol = 2)

for(i in 1:2){
  a_e_ascen[,i] <- a_ascen(y = i)
}

c_ascen <- as.data.frame(a_e_ascen)
c_ascen_1 <- cbind(as.data.frame(c_ascen[,1]), NOMINDICADOR, BASE_AUX)
c_ascen_1 <- c_ascen_1 %>% dplyr::mutate(ASCENDENCIA = "AFRO")
c_ascen_2 <- cbind(as.data.frame(c_ascen[,2]), NOMINDICADOR, BASE_AUX)
c_ascen_2 <- c_ascen_2 %>% dplyr::mutate(ASCENDENCIA = "NO AFRO")
colnames(c_ascen_1) <- colnames(BASE_MOTOR)
colnames(c_ascen_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_ascen_1,c_ascen_2)





# Quintil de ingreso del hogar
a_quintil <- function(y) {
  base <- subset(ech_h_men_svy_1, quintilesy == y) 
  resultados  <-  sapply(base$variables %>% select(all_of(var_int)), function(x){
    base %>%
      srvyr::summarise(stat1 = srvyr::survey_mean(x, na.rm = TRUE))
    
  })
  y <- as.numeric(resultados[1,])
}

a_e_quintil = matrix(, nrow = 8, ncol = 5)

for(i in 1:5){
  a_e_quintil[,i] <- a_quintil(y = i)
}

c_quintil <- as.data.frame(a_e_quintil)

c_quintil_1 <- cbind(as.data.frame(c_quintil[,1]), NOMINDICADOR, BASE_AUX)
c_quintil_1 <- c_quintil_1 %>% dplyr::mutate(QUINTIL = "Quintil 1")
c_quintil_2 <- cbind(as.data.frame(c_quintil[,2]), NOMINDICADOR, BASE_AUX)
c_quintil_2 <- c_quintil_2 %>% dplyr::mutate(QUINTIL = "Quintil 2")
c_quintil_3 <- cbind(as.data.frame(c_quintil[,3]), NOMINDICADOR, BASE_AUX)
c_quintil_3 <- c_quintil_3 %>% dplyr::mutate(QUINTIL = "Quintil 3")
c_quintil_4 <- cbind(as.data.frame(c_quintil[,4]), NOMINDICADOR, BASE_AUX)
c_quintil_4 <- c_quintil_4 %>% dplyr::mutate(QUINTIL = "Quintil 4")
c_quintil_5 <- cbind(as.data.frame(c_quintil[,5]), NOMINDICADOR, BASE_AUX)
c_quintil_5 <- c_quintil_5 %>% dplyr::mutate(QUINTIL = "Quintil 5")
colnames(c_quintil_1) <- colnames(BASE_MOTOR)
colnames(c_quintil_2) <- colnames(BASE_MOTOR)
colnames(c_quintil_3) <- colnames(BASE_MOTOR)
colnames(c_quintil_4) <- colnames(BASE_MOTOR)
colnames(c_quintil_5) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_quintil_1,c_quintil_2, c_quintil_3, c_quintil_4, c_quintil_5)


# Pobreza

a_pobre <- function(y) {
  base <- subset(ech_h_men_svy_1, pobre_aux == y) 
  resultados  <-  sapply(base$variables %>% select(all_of(var_int)), function(x){
    base %>%
      srvyr::summarise(stat1 = srvyr::survey_mean(x, na.rm = TRUE))
    
  })
  y <- as.numeric(resultados[1,])
}

a_e_pobre = matrix(, nrow = 8, ncol = 2)

for(i in 1:2){
  a_e_pobre[,i] <- a_pobre(y = i)
}

c_pobre <- as.data.frame(a_e_pobre)
c_pobre_1 <- cbind(as.data.frame(c_pobre[,1]), NOMINDICADOR, BASE_AUX)
c_pobre_1 <- c_pobre_1 %>% dplyr::mutate(POBRE = "POBRE")
c_pobre_2 <- cbind(as.data.frame(c_pobre[,2]), NOMINDICADOR, BASE_AUX)
c_pobre_2 <- c_pobre_2 %>% dplyr::mutate(POBRE = "NO POBRE")
colnames(c_pobre_1) <- colnames(BASE_MOTOR)
colnames(c_pobre_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_pobre_1,c_pobre_2)







# Base motor

BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(CODIND = "516")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(CATEGORIA = "Composición y estructura de los hogares")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(PESTAÑA = "Total país")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(PAÍS = "Uruguay")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(ANIO = 2023)
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(RESPONSABLE = "Sharon Katzkowicz")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(DEPARTAMENTOUY = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(NSE = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(NIVELEDU = "Todos")

BASE_MOTOR <- as.data.frame(BASE_MOTOR)


BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(ASCENDENCIA = case_when(ASCENDENCIA == "AFRO" ~ "Afro", 
                                                                   ASCENDENCIA == "NO AFRO" ~ "No afro",
                                                                   ASCENDENCIA == "" ~ "Todos"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(URBANORURALUY = case_when(URBANORURALUY == "URBANO (MENOS DE 5.000 HABITANTES)" ~ "Urbano (menos de 5.000 habitantes)",
                                                                     URBANORURALUY == "RURAL DISPERSO" ~ "Rural disperso", 
                                                                     URBANORURALUY == "" ~ "Total país"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(SEXO = case_when(SEXO == "MUJERES" ~ "Mujeres", 
                                                            SEXO=="VARONES" ~ "Varones",
                                                            SEXO == "" ~ "Todos"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(QUINTIL = case_when(QUINTIL == "Quintil 1"~"Quintil 1", 
                                                               QUINTIL == "Quintil 2"~"Quintil 2",
                                                               QUINTIL == "Quintil 3"~"Quintil 3", 
                                                               QUINTIL == "Quintil 4"~"Quintil 4",
                                                               QUINTIL == "Quintil 5"~"Quintil 5",
                                                               QUINTIL == "" ~ "Todos"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(EDAD = case_when(EDAD == "Menor de 30 años"~"Menor de 30 años", 
                                                            EDAD == "Entre 30 y 44 años"~"Entre 30 y 44 años",
                                                            EDAD == "Entre 45 y 64 años"~"Entre 45 y 64 años", 
                                                            EDAD == "65 años o más"~"65 años o más",
                                                            EDAD == "" ~ "Todos"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(POBRE = case_when(POBRE == "POBRE" ~ "Pobre", 
                                                             POBRE == "NO POBRE" ~ "No pobre",
                                                             POBRE == "" ~ "Todos"))


m516_tp <- BASE_MOTOR %>% select(CODIND,	
                                 NOMINDICADOR,	
                                 CATEGORIA,	
                                 PESTAÑA,	
                                 SEXO,	
                                 ASCENDENCIA,	
                                 QUINTIL,	
                                 DEPARTAMENTOUY,	
                                 URBANORURALUY,	
                                 EDAD,	
                                 POBRE, 
                                 NSE,
                                 NIVELEDU,
                                 PAÍS,	
                                 ANIO,	
                                 VALOR,	
                                 RESPONSABLE)




### 541 Tasa de participación de mujeres y varones entre 14 y 49 años de edad, sin menores en el hogar


## País Urbano


ech_svy_1 <- ech_svy %>%
  filter(bd_region == 1 & menor13H == 0 & (bc_pe3>13 | bc_pe3<50))

# Total


NOMINDICADOR <- 
  c("Tasa de participación de mujeres y varones entre 14 y 49 años sin menores en el hogar (Varón)",
    "Tasa de participación de mujeres y varones entre 14 y 49 años sin menores en el hogar (Mujer)")


var_int <- c("activos1", "activos2")
a_sem <- sapply(ech_svy_1$variables %>% select(all_of(var_int)), function(x){
  ech_svy_1 %>%
    srvyr::summarise(stat = srvyr::survey_mean(x, na.rm = TRUE)) 
})

c_ano <- matrix(,2,1)
for (i in 1:2) {
  c_ano[i,1] <- c(as.numeric(c(a_sem[1,i])))
}

colnames(c_ano) <- c("VALOR")
c_ano <- cbind(c_ano, NOMINDICADOR)
c_ano <- as.data.frame(c_ano)
BASE_MOTOR <- cbind(c_ano,BASE_AUX)

# Tramo de edad

a_edad <- function(y) {
  base <- subset(ech_svy_1, tramo_edad3 == y) 
  resultados  <-  sapply(base$variables %>% select(all_of(var_int)), function(x){
    base %>%
      srvyr::summarise(stat1 = srvyr::survey_mean(x, na.rm = TRUE))
    
  })
  y <- as.numeric(resultados[1,])
}

a_e_edad = matrix(, nrow = 2, ncol = 2)

for(i in 1:2){
  a_e_edad[,i] <- a_edad(y = i)
}


c_edad <- as.data.frame(a_e_edad)

c_edad_1 <- cbind(as.data.frame(c_edad[,1]), NOMINDICADOR, BASE_AUX)
c_edad_1 <- c_edad_1 %>% dplyr::mutate(EDAD = "Menor de 30 años")
c_edad_2 <- cbind(as.data.frame(c_edad[,2]), NOMINDICADOR, BASE_AUX)
c_edad_2 <- c_edad_2 %>% dplyr::mutate(EDAD = "Entre 30 y 49 años")
colnames(c_edad_1) <- colnames(BASE_MOTOR)
colnames(c_edad_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_edad_1,c_edad_2)


# Quintil de ingreso del hogar
a_quintil <- function(y) {
  base <- subset(ech_svy_1, quintilesy == y) 
  resultados  <-  sapply(base$variables %>% select(all_of(var_int)), function(x){
    base %>%
      srvyr::summarise(stat1 = srvyr::survey_mean(x, na.rm = TRUE))
    
  })
  y <- as.numeric(resultados[1,])
}

a_e_quintil = matrix(, nrow = 2, ncol = 5)

for(i in 1:5){
  a_e_quintil[,i] <- a_quintil(y = i)
}

c_quintil <- as.data.frame(a_e_quintil)

c_quintil_1 <- cbind(as.data.frame(c_quintil[,1]), NOMINDICADOR, BASE_AUX)
c_quintil_1 <- c_quintil_1 %>% dplyr::mutate(QUINTIL = "Quintil 1")
c_quintil_2 <- cbind(as.data.frame(c_quintil[,2]), NOMINDICADOR, BASE_AUX)
c_quintil_2 <- c_quintil_2 %>% dplyr::mutate(QUINTIL = "Quintil 2")
c_quintil_3 <- cbind(as.data.frame(c_quintil[,3]), NOMINDICADOR, BASE_AUX)
c_quintil_3 <- c_quintil_3 %>% dplyr::mutate(QUINTIL = "Quintil 3")
c_quintil_4 <- cbind(as.data.frame(c_quintil[,4]), NOMINDICADOR, BASE_AUX)
c_quintil_4 <- c_quintil_4 %>% dplyr::mutate(QUINTIL = "Quintil 4")
c_quintil_5 <- cbind(as.data.frame(c_quintil[,5]), NOMINDICADOR, BASE_AUX)
c_quintil_5 <- c_quintil_5 %>% dplyr::mutate(QUINTIL = "Quintil 5")
colnames(c_quintil_1) <- colnames(BASE_MOTOR)
colnames(c_quintil_2) <- colnames(BASE_MOTOR)
colnames(c_quintil_3) <- colnames(BASE_MOTOR)
colnames(c_quintil_4) <- colnames(BASE_MOTOR)
colnames(c_quintil_5) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_quintil_1,c_quintil_2, c_quintil_3, c_quintil_4, c_quintil_5)




# Base motor

BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(CODIND = "541")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(CATEGORIA = "Cuidados y educación")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(PESTAÑA = "País urbano")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(PAÍS = "Uruguay")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(ANIO = 2023)
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(RESPONSABLE = "Sharon Katzkowicz")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(ASCENDENCIA = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(DEPARTAMENTOUY = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(URBANORURALUY = "Urbano (más de 5.000 habitantes)")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(POBRE = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(NSE = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(NIVELEDU = "Todos")
BASE_MOTOR <- as.data.frame(BASE_MOTOR)

BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(SEXO = case_when(SEXO == "" ~ "Todos", SEXO == "MUJERES"~"Mujeres", SEXO=="VARONES"~"Varones"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(QUINTIL = case_when(QUINTIL == "" ~ "Todos",
                                                               QUINTIL == "Quintil 1"~"Quintil 1", 
                                                               QUINTIL == "Quintil 2"~"Quintil 2",
                                                               QUINTIL == "Quintil 3"~"Quintil 3", 
                                                               QUINTIL == "Quintil 4"~"Quintil 4",
                                                               QUINTIL == "Quintil 5"~"Quintil 5"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(EDAD = case_when(EDAD == "" ~ "Todos",
                                                            EDAD == "Menor de 30 años"~"Menor de 30 años", 
                                                            EDAD == "Entre 30 y 49 años"~"Entre 30 y 49 años"))


m541_pu <- BASE_MOTOR %>% select(CODIND,	
                                 NOMINDICADOR,	
                                 CATEGORIA,	
                                 PESTAÑA,	
                                 SEXO,	
                                 ASCENDENCIA,	
                                 QUINTIL,	
                                 DEPARTAMENTOUY,	
                                 URBANORURALUY,	
                                 EDAD,	
                                 POBRE,
                                 NSE,
                                 NIVELEDU,
                                 PAÍS,	
                                 ANIO,	
                                 VALOR,	
                                 RESPONSABLE)	


## Total país

ech_svy_1 <- ech_svy %>%
  filter(menor13H == 0 & (bc_pe3>13 | bc_pe3<50))

# Total

NOMINDICADOR <- 
  c(  c("Tasa de participación de mujeres y varones entre 14 y 49 años sin menores en el hogar (Varón)",
        "Tasa de participación de mujeres y varones entre 14 y 49 años sin menores en el hogar (Mujer)"))


var_int <- c("activos1", "activos2")
a_sem <- sapply(ech_svy_1$variables %>% select(all_of(var_int)), function(x){
  ech_svy_1 %>%
    srvyr::summarise(stat = srvyr::survey_mean(x, na.rm = TRUE)) 
})

c_ano <- matrix(,2,1)
for (i in 1:2) {
  c_ano[i,1] <- c(as.numeric(c(a_sem[1,i])))
}

colnames(c_ano) <- c("VALOR")
c_ano <- cbind(c_ano, NOMINDICADOR)
c_ano <- as.data.frame(c_ano)
BASE_MOTOR <- cbind(c_ano,BASE_AUX)


# Región

a_reg <- function(y) {
  base <- subset(ech_svy_1, bd_region == y) 
  resultados  <-  sapply(base$variables %>% select(all_of(var_int)), function(x){
    base %>%
      srvyr::summarise(stat1 = srvyr::survey_mean(x, na.rm = TRUE))
    
  })
  y <- as.numeric(resultados[1,])
}

a_e_reg = matrix(, nrow = 2, ncol = 3)

for(i in 1:3){
  a_e_reg[,i] <- a_reg(y = i)
}


c_reg <- as.data.frame(a_e_reg[,2:3])
c_reg_1 <- cbind(as.data.frame(c_reg[,1]), NOMINDICADOR, BASE_AUX)
c_reg_1 <- c_reg_1 %>% dplyr::mutate(URBANORURALUY = "URBANO (MENOS DE 5.000 HABITANTES)")
c_reg_2 <- cbind(as.data.frame(c_reg[,2]), NOMINDICADOR, BASE_AUX)
c_reg_2 <- c_reg_2 %>% dplyr::mutate(URBANORURALUY = "RURAL DISPERSO")
colnames(c_reg_1) <- colnames(BASE_MOTOR)
colnames(c_reg_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_reg_1,c_reg_2)


# Tramo de edad

a_edad <- function(y) {
  base <- subset(ech_svy_1, tramo_edad3 == y) 
  resultados  <-  sapply(base$variables %>% select(all_of(var_int)), function(x){
    base %>%
      srvyr::summarise(stat1 = srvyr::survey_mean(x, na.rm = TRUE))
    
  })
  y <- as.numeric(resultados[1,])
}

a_e_edad = matrix(, nrow = 2, ncol = 2)

for(i in 1:2){
  a_e_edad[,i] <- a_edad(y = i)
}


c_edad <- as.data.frame(a_e_edad)

c_edad_1 <- cbind(as.data.frame(c_edad[,1]), NOMINDICADOR, BASE_AUX)
c_edad_1 <- c_edad_1 %>% dplyr::mutate(EDAD = "Menor de 30 años")
c_edad_2 <- cbind(as.data.frame(c_edad[,2]), NOMINDICADOR, BASE_AUX)
c_edad_2 <- c_edad_2 %>% dplyr::mutate(EDAD = "Entre 30 y 49 años")
colnames(c_edad_1) <- colnames(BASE_MOTOR)
colnames(c_edad_2) <- colnames(BASE_MOTOR)
colnames(c_edad_3) <- colnames(BASE_MOTOR)
colnames(c_edad_4) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_edad_1,c_edad_2)



# Ascendencia

a_ascen <- function(y) {
  base <- subset(ech_svy_1, bd_e29_1 == y) 
  resultados  <-  sapply(base$variables %>% select(all_of(var_int)), function(x){
    base %>%
      srvyr::summarise(stat1 = srvyr::survey_mean(x, na.rm = TRUE))
    
  })
  y <- as.numeric(resultados[1,])
}

a_e_ascen = matrix(, nrow = 2, ncol = 2)

for(i in 1:2){
  a_e_ascen[,i] <- a_ascen(y = i)
}

c_ascen <- as.data.frame(a_e_ascen)
c_ascen_1 <- cbind(as.data.frame(c_ascen[,1]), NOMINDICADOR, BASE_AUX)
c_ascen_1 <- c_ascen_1 %>% dplyr::mutate(ASCENDENCIA = "AFRO")
c_ascen_2 <- cbind(as.data.frame(c_ascen[,2]), NOMINDICADOR, BASE_AUX)
c_ascen_2 <- c_ascen_2 %>% dplyr::mutate(ASCENDENCIA = "NO AFRO")
colnames(c_ascen_1) <- colnames(BASE_MOTOR)
colnames(c_ascen_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_ascen_1,c_ascen_2)





# Quintil de ingreso del hogar
a_quintil <- function(y) {
  base <- subset(ech_svy_1, quintilesy == y) 
  resultados  <-  sapply(base$variables %>% select(all_of(var_int)), function(x){
    base %>%
      srvyr::summarise(stat1 = srvyr::survey_mean(x, na.rm = TRUE))
    
  })
  y <- as.numeric(resultados[1,])
}

a_e_quintil = matrix(, nrow = 2, ncol = 5)

for(i in 1:5){
  a_e_quintil[,i] <- a_quintil(y = i)
}

c_quintil <- as.data.frame(a_e_quintil)

c_quintil_1 <- cbind(as.data.frame(c_quintil[,1]), NOMINDICADOR, BASE_AUX)
c_quintil_1 <- c_quintil_1 %>% dplyr::mutate(QUINTIL = "Quintil 1")
c_quintil_2 <- cbind(as.data.frame(c_quintil[,2]), NOMINDICADOR, BASE_AUX)
c_quintil_2 <- c_quintil_2 %>% dplyr::mutate(QUINTIL = "Quintil 2")
c_quintil_3 <- cbind(as.data.frame(c_quintil[,3]), NOMINDICADOR, BASE_AUX)
c_quintil_3 <- c_quintil_3 %>% dplyr::mutate(QUINTIL = "Quintil 3")
c_quintil_4 <- cbind(as.data.frame(c_quintil[,4]), NOMINDICADOR, BASE_AUX)
c_quintil_4 <- c_quintil_4 %>% dplyr::mutate(QUINTIL = "Quintil 4")
c_quintil_5 <- cbind(as.data.frame(c_quintil[,5]), NOMINDICADOR, BASE_AUX)
c_quintil_5 <- c_quintil_5 %>% dplyr::mutate(QUINTIL = "Quintil 5")
colnames(c_quintil_1) <- colnames(BASE_MOTOR)
colnames(c_quintil_2) <- colnames(BASE_MOTOR)
colnames(c_quintil_3) <- colnames(BASE_MOTOR)
colnames(c_quintil_4) <- colnames(BASE_MOTOR)
colnames(c_quintil_5) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_quintil_1,c_quintil_2, c_quintil_3, c_quintil_4, c_quintil_5)


# Pobreza

a_pobre <- function(y) {
  base <- subset(ech_svy_1, pobre_aux == y) 
  resultados  <-  sapply(base$variables %>% select(all_of(var_int)), function(x){
    base %>%
      srvyr::summarise(stat1 = srvyr::survey_mean(x, na.rm = TRUE))
    
  })
  y <- as.numeric(resultados[1,])
}

a_e_pobre = matrix(, nrow = 2, ncol = 2)

for(i in 1:2){
  a_e_pobre[,i] <- a_pobre(y = i)
}


c_pobre <- as.data.frame(a_e_pobre)
c_pobre_1 <- cbind(as.data.frame(c_pobre[,1]), NOMINDICADOR, BASE_AUX)
c_pobre_1 <- c_pobre_1 %>% dplyr::mutate(POBRE = "POBRE")
c_pobre_2 <- cbind(as.data.frame(c_pobre[,2]), NOMINDICADOR, BASE_AUX)
c_pobre_2 <- c_pobre_2 %>% dplyr::mutate(POBRE = "NO POBRE")
colnames(c_pobre_1) <- colnames(BASE_MOTOR)
colnames(c_pobre_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_pobre_1,c_pobre_2)


# Base motor

BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(CODIND = "541")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(CATEGORIA = "Cuidados y educación")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(PESTAÑA = "Total país")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(PAÍS = "Uruguay")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(ANIO = 2023)
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(RESPONSABLE = "Sharon Katzkowicz")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(DEPARTAMENTOUY = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(NSE = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(NIVELEDU = "Todos")
BASE_MOTOR <- as.data.frame(BASE_MOTOR)


BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(ASCENDENCIA = case_when(ASCENDENCIA == "AFRO" ~ "Afro", 
                                                                   ASCENDENCIA == "NO AFRO" ~ "No afro",
                                                                   ASCENDENCIA == "" ~ "Todos"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(URBANORURALUY = case_when(URBANORURALUY == "URBANO (MENOS DE 5.000 HABITANTES)" ~ "Urbano (menos de 5.000 habitantes)",
                                                                     URBANORURALUY == "RURAL DISPERSO" ~ "Rural disperso", 
                                                                     URBANORURALUY == "" ~ "Total país"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(SEXO = case_when(SEXO == "MUJERES" ~ "Mujeres", 
                                                            SEXO=="VARONES" ~ "Varones",
                                                            SEXO == "" ~ "Todos"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(QUINTIL = case_when(QUINTIL == "Quintil 1"~"Quintil 1", 
                                                               QUINTIL == "Quintil 2"~"Quintil 2",
                                                               QUINTIL == "Quintil 3"~"Quintil 3", 
                                                               QUINTIL == "Quintil 4"~"Quintil 4",
                                                               QUINTIL == "Quintil 5"~"Quintil 5",
                                                               QUINTIL == "" ~ "Todos"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(EDAD = case_when(EDAD == "Menor de 30 años"~"Menor de 30 años", 
                                                            EDAD == "Entre 30 y 49 años"~"Entre 30 y 49 años",
                                                            EDAD == "" ~ "Todos"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(POBRE = case_when(POBRE == "POBRE" ~ "Pobre", 
                                                             POBRE == "NO POBRE" ~ "No pobre",
                                                             POBRE == "" ~ "Todos"))


m541_tp <- BASE_MOTOR %>% select(CODIND,	
                                 NOMINDICADOR,	
                                 CATEGORIA,	
                                 PESTAÑA,	
                                 SEXO,	
                                 ASCENDENCIA,	
                                 QUINTIL,	
                                 DEPARTAMENTOUY,	
                                 URBANORURALUY,	
                                 EDAD,	
                                 POBRE,
                                 NSE,
                                 NIVELEDU,
                                 PAÍS,	
                                 ANIO,	
                                 VALOR,	
                                 RESPONSABLE)


### 542 Tasa de participación de mujeres y varones entre 14 y 49 años de edad, con presencia de menores de 4 en el hogar


## País Urbano


ech_svy_1 <- ech_svy %>%
  filter(bd_region == 1 & menor4H==1 & (bc_pe3>13 | bc_pe3<50))

# Total


NOMINDICADOR <- 
  c("Tasa de participación de mujeres y varones entre 14 y 49 años con presencia de menores de 4 en el hogar (Varón)",
    "Tasa de participación de mujeres y varones entre 14 y 49 años con presencia de menores de 4 en el hogar (Mujer)")


var_int <- c("activos1", "activos2")
a_sem <- sapply(ech_svy_1$variables %>% select(all_of(var_int)), function(x){
  ech_svy_1 %>%
    srvyr::summarise(stat = srvyr::survey_mean(x, na.rm = TRUE)) 
})

c_ano <- matrix(,2,1)
for (i in 1:2) {
  c_ano[i,1] <- c(as.numeric(c(a_sem[1,i])))
}

colnames(c_ano) <- c("VALOR")
c_ano <- cbind(c_ano, NOMINDICADOR)
c_ano <- as.data.frame(c_ano)
BASE_MOTOR <- cbind(c_ano,BASE_AUX)

# Tramo de edad

a_edad <- function(y) {
  base <- subset(ech_svy_1, tramo_edad3 == y) 
  resultados  <-  sapply(base$variables %>% select(all_of(var_int)), function(x){
    base %>%
      srvyr::summarise(stat1 = srvyr::survey_mean(x, na.rm = TRUE))
    
  })
  y <- as.numeric(resultados[1,])
}

a_e_edad = matrix(, nrow = 2, ncol = 2)

for(i in 1:2){
  a_e_edad[,i] <- a_edad(y = i)
}

c_edad <- as.data.frame(a_e_edad)

c_edad_1 <- cbind(as.data.frame(c_edad[,1]), NOMINDICADOR, BASE_AUX)
c_edad_1 <- c_edad_1 %>% dplyr::mutate(EDAD = "Menor de 30 años")
c_edad_2 <- cbind(as.data.frame(c_edad[,2]), NOMINDICADOR, BASE_AUX)
c_edad_2 <- c_edad_2 %>% dplyr::mutate(EDAD = "Entre 30 y 49 años")
colnames(c_edad_1) <- colnames(BASE_MOTOR)
colnames(c_edad_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_edad_1,c_edad_2)


# Quintil de ingreso del hogar
a_quintil <- function(y) {
  base <- subset(ech_svy_1, quintilesy == y) 
  resultados  <-  sapply(base$variables %>% select(all_of(var_int)), function(x){
    base %>%
      srvyr::summarise(stat1 = srvyr::survey_mean(x, na.rm = TRUE))
    
  })
  y <- as.numeric(resultados[1,])
}

a_e_quintil = matrix(, nrow = 2, ncol = 5)

for(i in 1:5){
  a_e_quintil[,i] <- a_quintil(y = i)
}

c_quintil <- as.data.frame(a_e_quintil)

c_quintil_1 <- cbind(as.data.frame(c_quintil[,1]), NOMINDICADOR, BASE_AUX)
c_quintil_1 <- c_quintil_1 %>% dplyr::mutate(QUINTIL = "Quintil 1")
c_quintil_2 <- cbind(as.data.frame(c_quintil[,2]), NOMINDICADOR, BASE_AUX)
c_quintil_2 <- c_quintil_2 %>% dplyr::mutate(QUINTIL = "Quintil 2")
c_quintil_3 <- cbind(as.data.frame(c_quintil[,3]), NOMINDICADOR, BASE_AUX)
c_quintil_3 <- c_quintil_3 %>% dplyr::mutate(QUINTIL = "Quintil 3")
c_quintil_4 <- cbind(as.data.frame(c_quintil[,4]), NOMINDICADOR, BASE_AUX)
c_quintil_4 <- c_quintil_4 %>% dplyr::mutate(QUINTIL = "Quintil 4")
c_quintil_5 <- cbind(as.data.frame(c_quintil[,5]), NOMINDICADOR, BASE_AUX)
c_quintil_5 <- c_quintil_5 %>% dplyr::mutate(QUINTIL = "Quintil 5")
colnames(c_quintil_1) <- colnames(BASE_MOTOR)
colnames(c_quintil_2) <- colnames(BASE_MOTOR)
colnames(c_quintil_3) <- colnames(BASE_MOTOR)
colnames(c_quintil_4) <- colnames(BASE_MOTOR)
colnames(c_quintil_5) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_quintil_1,c_quintil_2, c_quintil_3, c_quintil_4, c_quintil_5)




# Base motor

BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(CODIND = "542")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(CATEGORIA = "Cuidados y educación")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(PESTAÑA = "País urbano")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(PAÍS = "Uruguay")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(ANIO = 2023)
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(RESPONSABLE = "Sharon Katzkowicz")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(ASCENDENCIA = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(DEPARTAMENTOUY = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(URBANORURALUY = "Urbano (más de 5.000 habitantes)")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(POBRE = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(NSE = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(NIVELEDU = "Todos")
BASE_MOTOR <- as.data.frame(BASE_MOTOR)

BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(SEXO = case_when(SEXO == "" ~ "Todos", SEXO == "MUJERES"~"Mujeres", SEXO=="VARONES"~"Varones"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(QUINTIL = case_when(QUINTIL == "" ~ "Todos",
                                                               QUINTIL == "Quintil 1"~"Quintil 1", 
                                                               QUINTIL == "Quintil 2"~"Quintil 2",
                                                               QUINTIL == "Quintil 3"~"Quintil 3", 
                                                               QUINTIL == "Quintil 4"~"Quintil 4",
                                                               QUINTIL == "Quintil 5"~"Quintil 5"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(EDAD = case_when(EDAD == "" ~ "Todos",
                                                            EDAD == "Menor de 30 años"~"Menor de 30 años", 
                                                            EDAD == "Entre 30 y 49 años"~"Entre 30 y 49 años"))


m542_pu <- BASE_MOTOR %>% select(CODIND,	
                                 NOMINDICADOR,	
                                 CATEGORIA,	
                                 PESTAÑA,	
                                 SEXO,	
                                 ASCENDENCIA,	
                                 QUINTIL,	
                                 DEPARTAMENTOUY,	
                                 URBANORURALUY,	
                                 EDAD,	
                                 POBRE,
                                 NSE,
                                 NIVELEDU,
                                 PAÍS,	
                                 ANIO,	
                                 VALOR,	
                                 RESPONSABLE)	


## Total país

ech_svy_1 <- ech_svy %>%
  filter(menor4H==1 & (bc_pe3>13 | bc_pe3<50))

# Total

NOMINDICADOR <- 
  c(  c("Tasa de participación de mujeres y varones entre 14 y 49 años con presencia de menores de 4 en el hogar (Varón)",
        "Tasa de participación de mujeres y varones entre 14 y 49 años con presencia de menores de 4 en el hogar (Mujer)"))


var_int <- c("activos1", "activos2")
a_sem <- sapply(ech_svy_1$variables %>% select(all_of(var_int)), function(x){
  ech_svy_1 %>%
    srvyr::summarise(stat = srvyr::survey_mean(x, na.rm = TRUE)) 
})

c_ano <- matrix(,2,1)
for (i in 1:2) {
  c_ano[i,1] <- c(as.numeric(c(a_sem[1,i])))
}

colnames(c_ano) <- c("VALOR")
c_ano <- cbind(c_ano, NOMINDICADOR)
c_ano <- as.data.frame(c_ano)
BASE_MOTOR <- cbind(c_ano,BASE_AUX)


# Región

a_reg <- function(y) {
  base <- subset(ech_svy_1, bd_region == y) 
  resultados  <-  sapply(base$variables %>% select(all_of(var_int)), function(x){
    base %>%
      srvyr::summarise(stat1 = srvyr::survey_mean(x, na.rm = TRUE))
    
  })
  y <- as.numeric(resultados[1,])
}

a_e_reg = matrix(, nrow = 2, ncol = 3)

for(i in 1:3){
  a_e_reg[,i] <- a_reg(y = i)
}

c_reg <- as.data.frame(a_e_reg[,2:3])
c_reg_1 <- cbind(as.data.frame(c_reg[,1]), NOMINDICADOR, BASE_AUX)
c_reg_1 <- c_reg_1 %>% dplyr::mutate(URBANORURALUY = "URBANO (MENOS DE 5.000 HABITANTES)")
c_reg_2 <- cbind(as.data.frame(c_reg[,2]), NOMINDICADOR, BASE_AUX)
c_reg_2 <- c_reg_2 %>% dplyr::mutate(URBANORURALUY = "RURAL DISPERSO")
colnames(c_reg_1) <- colnames(BASE_MOTOR)
colnames(c_reg_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_reg_1,c_reg_2)


# Tramo de edad

a_edad <- function(y) {
  base <- subset(ech_svy_1, tramo_edad3 == y) 
  resultados  <-  sapply(base$variables %>% select(all_of(var_int)), function(x){
    base %>%
      srvyr::summarise(stat1 = srvyr::survey_mean(x, na.rm = TRUE))
    
  })
  y <- as.numeric(resultados[1,])
}

a_e_edad = matrix(, nrow = 2, ncol = 2)

for(i in 1:2){
  a_e_edad[,i] <- a_edad(y = i)
}

c_edad <- as.data.frame(a_e_edad)

c_edad_1 <- cbind(as.data.frame(c_edad[,1]), NOMINDICADOR, BASE_AUX)
c_edad_1 <- c_edad_1 %>% dplyr::mutate(EDAD = "Menor de 30 años")
c_edad_2 <- cbind(as.data.frame(c_edad[,2]), NOMINDICADOR, BASE_AUX)
c_edad_2 <- c_edad_2 %>% dplyr::mutate(EDAD = "Entre 30 y 49 años")
colnames(c_edad_1) <- colnames(BASE_MOTOR)
colnames(c_edad_2) <- colnames(BASE_MOTOR)
colnames(c_edad_3) <- colnames(BASE_MOTOR)
colnames(c_edad_4) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_edad_1,c_edad_2)



# Ascendencia

a_ascen <- function(y) {
  base <- subset(ech_svy_1, bd_e29_1 == y) 
  resultados  <-  sapply(base$variables %>% select(all_of(var_int)), function(x){
    base %>%
      srvyr::summarise(stat1 = srvyr::survey_mean(x, na.rm = TRUE))
    
  })
  y <- as.numeric(resultados[1,])
}

a_e_ascen = matrix(, nrow = 2, ncol = 2)

for(i in 1:2){
  a_e_ascen[,i] <- a_ascen(y = i)
}

c_ascen <- as.data.frame(a_e_ascen)
c_ascen_1 <- cbind(as.data.frame(c_ascen[,1]), NOMINDICADOR, BASE_AUX)
c_ascen_1 <- c_ascen_1 %>% dplyr::mutate(ASCENDENCIA = "AFRO")
c_ascen_2 <- cbind(as.data.frame(c_ascen[,2]), NOMINDICADOR, BASE_AUX)
c_ascen_2 <- c_ascen_2 %>% dplyr::mutate(ASCENDENCIA = "NO AFRO")
colnames(c_ascen_1) <- colnames(BASE_MOTOR)
colnames(c_ascen_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_ascen_1,c_ascen_2)





# Quintil de ingreso del hogar
a_quintil <- function(y) {
  base <- subset(ech_svy_1, quintilesy == y) 
  resultados  <-  sapply(base$variables %>% select(all_of(var_int)), function(x){
    base %>%
      srvyr::summarise(stat1 = srvyr::survey_mean(x, na.rm = TRUE))
    
  })
  y <- as.numeric(resultados[1,])
}

a_e_quintil = matrix(, nrow = 2, ncol = 5)

for(i in 1:5){
  a_e_quintil[,i] <- a_quintil(y = i)
}

c_quintil <- as.data.frame(a_e_quintil)

c_quintil_1 <- cbind(as.data.frame(c_quintil[,1]), NOMINDICADOR, BASE_AUX)
c_quintil_1 <- c_quintil_1 %>% dplyr::mutate(QUINTIL = "Quintil 1")
c_quintil_2 <- cbind(as.data.frame(c_quintil[,2]), NOMINDICADOR, BASE_AUX)
c_quintil_2 <- c_quintil_2 %>% dplyr::mutate(QUINTIL = "Quintil 2")
c_quintil_3 <- cbind(as.data.frame(c_quintil[,3]), NOMINDICADOR, BASE_AUX)
c_quintil_3 <- c_quintil_3 %>% dplyr::mutate(QUINTIL = "Quintil 3")
c_quintil_4 <- cbind(as.data.frame(c_quintil[,4]), NOMINDICADOR, BASE_AUX)
c_quintil_4 <- c_quintil_4 %>% dplyr::mutate(QUINTIL = "Quintil 4")
c_quintil_5 <- cbind(as.data.frame(c_quintil[,5]), NOMINDICADOR, BASE_AUX)
c_quintil_5 <- c_quintil_5 %>% dplyr::mutate(QUINTIL = "Quintil 5")
colnames(c_quintil_1) <- colnames(BASE_MOTOR)
colnames(c_quintil_2) <- colnames(BASE_MOTOR)
colnames(c_quintil_3) <- colnames(BASE_MOTOR)
colnames(c_quintil_4) <- colnames(BASE_MOTOR)
colnames(c_quintil_5) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_quintil_1,c_quintil_2, c_quintil_3, c_quintil_4, c_quintil_5)


# Pobreza

a_pobre <- function(y) {
  base <- subset(ech_svy_1, pobre_aux == y) 
  resultados  <-  sapply(base$variables %>% select(all_of(var_int)), function(x){
    base %>%
      srvyr::summarise(stat1 = srvyr::survey_mean(x, na.rm = TRUE))
    
  })
  y <- as.numeric(resultados[1,])
}

a_e_pobre = matrix(, nrow = 2, ncol = 2)

for(i in 1:2){
  a_e_pobre[,i] <- a_pobre(y = i)
}

c_pobre <- as.data.frame(a_e_pobre)
c_pobre_1 <- cbind(as.data.frame(c_pobre[,1]), NOMINDICADOR, BASE_AUX)
c_pobre_1 <- c_pobre_1 %>% dplyr::mutate(POBRE = "POBRE")
c_pobre_2 <- cbind(as.data.frame(c_pobre[,2]), NOMINDICADOR, BASE_AUX)
c_pobre_2 <- c_pobre_2 %>% dplyr::mutate(POBRE = "NO POBRE")
colnames(c_pobre_1) <- colnames(BASE_MOTOR)
colnames(c_pobre_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_pobre_1,c_pobre_2)


# Base motor

BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(CODIND = "542")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(CATEGORIA = "Cuidados y educación")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(PESTAÑA = "Total país")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(PAÍS = "Uruguay")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(ANIO = 2023)
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(RESPONSABLE = "Sharon Katzkowicz")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(DEPARTAMENTOUY = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(NSE = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(NIVELEDU = "Todos")
BASE_MOTOR <- as.data.frame(BASE_MOTOR)


BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(ASCENDENCIA = case_when(ASCENDENCIA == "AFRO" ~ "Afro", 
                                                                   ASCENDENCIA == "NO AFRO" ~ "No afro",
                                                                   ASCENDENCIA == "" ~ "Todos"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(URBANORURALUY = case_when(URBANORURALUY == "URBANO (MENOS DE 5.000 HABITANTES)" ~ "Urbano (menos de 5.000 habitantes)",
                                                                     URBANORURALUY == "RURAL DISPERSO" ~ "Rural disperso", 
                                                                     URBANORURALUY == "" ~ "Total país"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(SEXO = case_when(SEXO == "MUJERES" ~ "Mujeres", 
                                                            SEXO=="VARONES" ~ "Varones",
                                                            SEXO == "" ~ "Todos"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(QUINTIL = case_when(QUINTIL == "Quintil 1"~"Quintil 1", 
                                                               QUINTIL == "Quintil 2"~"Quintil 2",
                                                               QUINTIL == "Quintil 3"~"Quintil 3", 
                                                               QUINTIL == "Quintil 4"~"Quintil 4",
                                                               QUINTIL == "Quintil 5"~"Quintil 5",
                                                               QUINTIL == "" ~ "Todos"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(EDAD = case_when(EDAD == "Menor de 30 años"~"Menor de 30 años", 
                                                            EDAD == "Entre 30 y 49 años"~"Entre 30 y 49 años",
                                                            EDAD == "" ~ "Todos"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(POBRE = case_when(POBRE == "POBRE" ~ "Pobre", 
                                                             POBRE == "NO POBRE" ~ "No pobre",
                                                             POBRE == "" ~ "Todos"))


m542_tp <- BASE_MOTOR %>% select(CODIND,	
                                 NOMINDICADOR,	
                                 CATEGORIA,	
                                 PESTAÑA,	
                                 SEXO,	
                                 ASCENDENCIA,	
                                 QUINTIL,	
                                 DEPARTAMENTOUY,	
                                 URBANORURALUY,	
                                 EDAD,	
                                 POBRE,
                                 NSE,
                                 NIVELEDU,
                                 PAÍS,	
                                 ANIO,	
                                 VALOR,	
                                 RESPONSABLE)



### 543 Tasa de participación de mujeres y varones entre 14 y 49 años de edad, con presencia de menores de 7 en el hogar


## País Urbano


ech_svy_1 <- ech_svy %>%
  filter(bd_region == 1 & menor7H==1 & (bc_pe3>13 | bc_pe3<50))

# Total


NOMINDICADOR <- 
  c("Tasa de participación de mujeres y varones entre 14 y 49 años con presencia de menores de 7 en el hogar (Varón)",
    "Tasa de participación de mujeres y varones entre 14 y 49 años con presencia de menores de 7 en el hogar (Mujer)")


var_int <- c("activos1", "activos2")
a_sem <- sapply(ech_svy_1$variables %>% select(all_of(var_int)), function(x){
  ech_svy_1 %>%
    srvyr::summarise(stat = srvyr::survey_mean(x, na.rm = TRUE)) 
})

c_ano <- matrix(,2,1)
for (i in 1:2) {
  c_ano[i,1] <- c(as.numeric(c(a_sem[1,i])))
}

colnames(c_ano) <- c("VALOR")
c_ano <- cbind(c_ano, NOMINDICADOR)
c_ano <- as.data.frame(c_ano)
BASE_MOTOR <- cbind(c_ano,BASE_AUX)

# Tramo de edad

a_edad <- function(y) {
  base <- subset(ech_svy_1, tramo_edad3 == y) 
  resultados  <-  sapply(base$variables %>% select(all_of(var_int)), function(x){
    base %>%
      srvyr::summarise(stat1 = srvyr::survey_mean(x, na.rm = TRUE))
    
  })
  y <- as.numeric(resultados[1,])
}

a_e_edad = matrix(, nrow = 2, ncol = 2)

for(i in 1:2){
  a_e_edad[,i] <- a_edad(y = i)
}

c_edad <- as.data.frame(a_e_edad)

c_edad_1 <- cbind(as.data.frame(c_edad[,1]), NOMINDICADOR, BASE_AUX)
c_edad_1 <- c_edad_1 %>% dplyr::mutate(EDAD = "Menor de 30 años")
c_edad_2 <- cbind(as.data.frame(c_edad[,2]), NOMINDICADOR, BASE_AUX)
c_edad_2 <- c_edad_2 %>% dplyr::mutate(EDAD = "Entre 30 y 49 años")
colnames(c_edad_1) <- colnames(BASE_MOTOR)
colnames(c_edad_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_edad_1,c_edad_2)


# Quintil de ingreso del hogar
a_quintil <- function(y) {
  base <- subset(ech_svy_1, quintilesy == y) 
  resultados  <-  sapply(base$variables %>% select(all_of(var_int)), function(x){
    base %>%
      srvyr::summarise(stat1 = srvyr::survey_mean(x, na.rm = TRUE))
    
  })
  y <- as.numeric(resultados[1,])
}

a_e_quintil = matrix(, nrow = 2, ncol = 5)

for(i in 1:5){
  a_e_quintil[,i] <- a_quintil(y = i)
}

c_quintil <- as.data.frame(a_e_quintil)

c_quintil_1 <- cbind(as.data.frame(c_quintil[,1]), NOMINDICADOR, BASE_AUX)
c_quintil_1 <- c_quintil_1 %>% dplyr::mutate(QUINTIL = "Quintil 1")
c_quintil_2 <- cbind(as.data.frame(c_quintil[,2]), NOMINDICADOR, BASE_AUX)
c_quintil_2 <- c_quintil_2 %>% dplyr::mutate(QUINTIL = "Quintil 2")
c_quintil_3 <- cbind(as.data.frame(c_quintil[,3]), NOMINDICADOR, BASE_AUX)
c_quintil_3 <- c_quintil_3 %>% dplyr::mutate(QUINTIL = "Quintil 3")
c_quintil_4 <- cbind(as.data.frame(c_quintil[,4]), NOMINDICADOR, BASE_AUX)
c_quintil_4 <- c_quintil_4 %>% dplyr::mutate(QUINTIL = "Quintil 4")
c_quintil_5 <- cbind(as.data.frame(c_quintil[,5]), NOMINDICADOR, BASE_AUX)
c_quintil_5 <- c_quintil_5 %>% dplyr::mutate(QUINTIL = "Quintil 5")
colnames(c_quintil_1) <- colnames(BASE_MOTOR)
colnames(c_quintil_2) <- colnames(BASE_MOTOR)
colnames(c_quintil_3) <- colnames(BASE_MOTOR)
colnames(c_quintil_4) <- colnames(BASE_MOTOR)
colnames(c_quintil_5) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_quintil_1,c_quintil_2, c_quintil_3, c_quintil_4, c_quintil_5)




# Base motor

BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(CODIND = "543")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(CATEGORIA = "Cuidados y educación")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(PESTAÑA = "País urbano")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(PAÍS = "Uruguay")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(ANIO = 2023)
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(RESPONSABLE = "Sharon Katzkowicz")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(ASCENDENCIA = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(DEPARTAMENTOUY = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(URBANORURALUY = "Urbano (más de 5.000 habitantes)")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(POBRE = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(NSE = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(NIVELEDU = "Todos")
BASE_MOTOR <- as.data.frame(BASE_MOTOR)

BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(SEXO = case_when(SEXO == "" ~ "Todos", SEXO == "MUJERES"~"Mujeres", SEXO=="VARONES"~"Varones"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(QUINTIL = case_when(QUINTIL == "" ~ "Todos",
                                                               QUINTIL == "Quintil 1"~"Quintil 1", 
                                                               QUINTIL == "Quintil 2"~"Quintil 2",
                                                               QUINTIL == "Quintil 3"~"Quintil 3", 
                                                               QUINTIL == "Quintil 4"~"Quintil 4",
                                                               QUINTIL == "Quintil 5"~"Quintil 5"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(EDAD = case_when(EDAD == "" ~ "Todos",
                                                            EDAD == "Menor de 30 años"~"Menor de 30 años", 
                                                            EDAD == "Entre 30 y 49 años"~"Entre 30 y 49 años"))


m543_pu <- BASE_MOTOR %>% select(CODIND,	
                                 NOMINDICADOR,	
                                 CATEGORIA,	
                                 PESTAÑA,	
                                 SEXO,	
                                 ASCENDENCIA,	
                                 QUINTIL,	
                                 DEPARTAMENTOUY,	
                                 URBANORURALUY,	
                                 EDAD,	
                                 POBRE,
                                 NSE,
                                 NIVELEDU,
                                 PAÍS,	
                                 ANIO,	
                                 VALOR,	
                                 RESPONSABLE)	


## Total país

ech_svy_1 <- ech_svy %>%
  filter(menor7H==1 & (bc_pe3>13 | bc_pe3<50))

# Total

NOMINDICADOR <- 
  c(  c("Tasa de participación de mujeres y varones entre 14 y 49 años con presencia de menores de 7 en el hogar (Varón)",
        "Tasa de participación de mujeres y varones entre 14 y 49 años con presencia de menores de 7 en el hogar (Mujer)"))


var_int <- c("activos1", "activos2")
a_sem <- sapply(ech_svy_1$variables %>% select(all_of(var_int)), function(x){
  ech_svy_1 %>%
    srvyr::summarise(stat = srvyr::survey_mean(x, na.rm = TRUE)) 
})

c_ano <- matrix(,2,1)
for (i in 1:2) {
  c_ano[i,1] <- c(as.numeric(c(a_sem[1,i])))
}

colnames(c_ano) <- c("VALOR")
c_ano <- cbind(c_ano, NOMINDICADOR)
c_ano <- as.data.frame(c_ano)
BASE_MOTOR <- cbind(c_ano,BASE_AUX)


# Región

a_reg <- function(y) {
  base <- subset(ech_svy_1, bd_region == y) 
  resultados  <-  sapply(base$variables %>% select(all_of(var_int)), function(x){
    base %>%
      srvyr::summarise(stat1 = srvyr::survey_mean(x, na.rm = TRUE))
    
  })
  y <- as.numeric(resultados[1,])
}

a_e_reg = matrix(, nrow = 2, ncol = 3)

for(i in 1:3){
  a_e_reg[,i] <- a_reg(y = i)
}

c_reg <- as.data.frame(a_e_reg[,2:3])
c_reg_1 <- cbind(as.data.frame(c_reg[,1]), NOMINDICADOR, BASE_AUX)
c_reg_1 <- c_reg_1 %>% dplyr::mutate(URBANORURALUY = "URBANO (MENOS DE 5.000 HABITANTES)")
c_reg_2 <- cbind(as.data.frame(c_reg[,2]), NOMINDICADOR, BASE_AUX)
c_reg_2 <- c_reg_2 %>% dplyr::mutate(URBANORURALUY = "RURAL DISPERSO")
colnames(c_reg_1) <- colnames(BASE_MOTOR)
colnames(c_reg_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_reg_1,c_reg_2)


# Tramo de edad

a_edad <- function(y) {
  base <- subset(ech_svy_1, tramo_edad3 == y) 
  resultados  <-  sapply(base$variables %>% select(all_of(var_int)), function(x){
    base %>%
      srvyr::summarise(stat1 = srvyr::survey_mean(x, na.rm = TRUE))
    
  })
  y <- as.numeric(resultados[1,])
}

a_e_edad = matrix(, nrow = 2, ncol = 2)

for(i in 1:2){
  a_e_edad[,i] <- a_edad(y = i)
}


c_edad <- as.data.frame(a_e_edad)

c_edad_1 <- cbind(as.data.frame(c_edad[,1]), NOMINDICADOR, BASE_AUX)
c_edad_1 <- c_edad_1 %>% dplyr::mutate(EDAD = "Menor de 30 años")
c_edad_2 <- cbind(as.data.frame(c_edad[,2]), NOMINDICADOR, BASE_AUX)
c_edad_2 <- c_edad_2 %>% dplyr::mutate(EDAD = "Entre 30 y 49 años")
colnames(c_edad_1) <- colnames(BASE_MOTOR)
colnames(c_edad_2) <- colnames(BASE_MOTOR)
colnames(c_edad_3) <- colnames(BASE_MOTOR)
colnames(c_edad_4) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_edad_1,c_edad_2)



# Ascendencia

a_ascen <- function(y) {
  base <- subset(ech_svy_1, bd_e29_1 == y) 
  resultados  <-  sapply(base$variables %>% select(all_of(var_int)), function(x){
    base %>%
      srvyr::summarise(stat1 = srvyr::survey_mean(x, na.rm = TRUE))
    
  })
  y <- as.numeric(resultados[1,])
}

a_e_ascen = matrix(, nrow = 2, ncol = 2)

for(i in 1:2){
  a_e_ascen[,i] <- a_ascen(y = i)
}

c_ascen <- as.data.frame(a_e_ascen)
c_ascen_1 <- cbind(as.data.frame(c_ascen[,1]), NOMINDICADOR, BASE_AUX)
c_ascen_1 <- c_ascen_1 %>% dplyr::mutate(ASCENDENCIA = "AFRO")
c_ascen_2 <- cbind(as.data.frame(c_ascen[,2]), NOMINDICADOR, BASE_AUX)
c_ascen_2 <- c_ascen_2 %>% dplyr::mutate(ASCENDENCIA = "NO AFRO")
colnames(c_ascen_1) <- colnames(BASE_MOTOR)
colnames(c_ascen_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_ascen_1,c_ascen_2)





# Quintil de ingreso del hogar
a_quintil <- function(y) {
  base <- subset(ech_svy_1, quintilesy == y) 
  resultados  <-  sapply(base$variables %>% select(all_of(var_int)), function(x){
    base %>%
      srvyr::summarise(stat1 = srvyr::survey_mean(x, na.rm = TRUE))
    
  })
  y <- as.numeric(resultados[1,])
}

a_e_quintil = matrix(, nrow = 2, ncol = 5)

for(i in 1:5){
  a_e_quintil[,i] <- a_quintil(y = i)
}

c_quintil <- as.data.frame(a_e_quintil)

c_quintil_1 <- cbind(as.data.frame(c_quintil[,1]), NOMINDICADOR, BASE_AUX)
c_quintil_1 <- c_quintil_1 %>% dplyr::mutate(QUINTIL = "Quintil 1")
c_quintil_2 <- cbind(as.data.frame(c_quintil[,2]), NOMINDICADOR, BASE_AUX)
c_quintil_2 <- c_quintil_2 %>% dplyr::mutate(QUINTIL = "Quintil 2")
c_quintil_3 <- cbind(as.data.frame(c_quintil[,3]), NOMINDICADOR, BASE_AUX)
c_quintil_3 <- c_quintil_3 %>% dplyr::mutate(QUINTIL = "Quintil 3")
c_quintil_4 <- cbind(as.data.frame(c_quintil[,4]), NOMINDICADOR, BASE_AUX)
c_quintil_4 <- c_quintil_4 %>% dplyr::mutate(QUINTIL = "Quintil 4")
c_quintil_5 <- cbind(as.data.frame(c_quintil[,5]), NOMINDICADOR, BASE_AUX)
c_quintil_5 <- c_quintil_5 %>% dplyr::mutate(QUINTIL = "Quintil 5")
colnames(c_quintil_1) <- colnames(BASE_MOTOR)
colnames(c_quintil_2) <- colnames(BASE_MOTOR)
colnames(c_quintil_3) <- colnames(BASE_MOTOR)
colnames(c_quintil_4) <- colnames(BASE_MOTOR)
colnames(c_quintil_5) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_quintil_1,c_quintil_2, c_quintil_3, c_quintil_4, c_quintil_5)


# Pobreza

a_pobre <- function(y) {
  base <- subset(ech_svy_1, pobre_aux == y) 
  resultados  <-  sapply(base$variables %>% select(all_of(var_int)), function(x){
    base %>%
      srvyr::summarise(stat1 = srvyr::survey_mean(x, na.rm = TRUE))
    
  })
  y <- as.numeric(resultados[1,])
}

a_e_pobre = matrix(, nrow = 2, ncol = 2)

for(i in 1:2){
  a_e_pobre[,i] <- a_pobre(y = i)
}

c_pobre <- as.data.frame(a_e_pobre)
c_pobre_1 <- cbind(as.data.frame(c_pobre[,1]), NOMINDICADOR, BASE_AUX)
c_pobre_1 <- c_pobre_1 %>% dplyr::mutate(POBRE = "POBRE")
c_pobre_2 <- cbind(as.data.frame(c_pobre[,2]), NOMINDICADOR, BASE_AUX)
c_pobre_2 <- c_pobre_2 %>% dplyr::mutate(POBRE = "NO POBRE")
colnames(c_pobre_1) <- colnames(BASE_MOTOR)
colnames(c_pobre_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_pobre_1,c_pobre_2)


# Base motor

BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(CODIND = "543")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(CATEGORIA = "Cuidados y educación")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(PESTAÑA = "Total país")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(PAÍS = "Uruguay")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(ANIO = 2023)
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(RESPONSABLE = "Sharon Katzkowicz")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(DEPARTAMENTOUY = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(NSE = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(NIVELEDU = "Todos")
BASE_MOTOR <- as.data.frame(BASE_MOTOR)


BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(ASCENDENCIA = case_when(ASCENDENCIA == "AFRO" ~ "Afro", 
                                                                   ASCENDENCIA == "NO AFRO" ~ "No afro",
                                                                   ASCENDENCIA == "" ~ "Todos"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(URBANORURALUY = case_when(URBANORURALUY == "URBANO (MENOS DE 5.000 HABITANTES)" ~ "Urbano (menos de 5.000 habitantes)",
                                                                     URBANORURALUY == "RURAL DISPERSO" ~ "Rural disperso", 
                                                                     URBANORURALUY == "" ~ "Total país"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(SEXO = case_when(SEXO == "MUJERES" ~ "Mujeres", 
                                                            SEXO=="VARONES" ~ "Varones",
                                                            SEXO == "" ~ "Todos"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(QUINTIL = case_when(QUINTIL == "Quintil 1"~"Quintil 1", 
                                                               QUINTIL == "Quintil 2"~"Quintil 2",
                                                               QUINTIL == "Quintil 3"~"Quintil 3", 
                                                               QUINTIL == "Quintil 4"~"Quintil 4",
                                                               QUINTIL == "Quintil 5"~"Quintil 5",
                                                               QUINTIL == "" ~ "Todos"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(EDAD = case_when(EDAD == "Menor de 30 años"~"Menor de 30 años", 
                                                            EDAD == "Entre 30 y 49 años"~"Entre 30 y 49 años",
                                                            EDAD == "" ~ "Todos"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(POBRE = case_when(POBRE == "POBRE" ~ "Pobre", 
                                                             POBRE == "NO POBRE" ~ "No pobre",
                                                             POBRE == "" ~ "Todos"))


m543_tp <- BASE_MOTOR %>% select(CODIND,	
                                 NOMINDICADOR,	
                                 CATEGORIA,	
                                 PESTAÑA,	
                                 SEXO,	
                                 ASCENDENCIA,	
                                 QUINTIL,	
                                 DEPARTAMENTOUY,	
                                 URBANORURALUY,	
                                 EDAD,	
                                 POBRE,
                                 NSE,
                                 NIVELEDU,
                                 PAÍS,	
                                 ANIO,	
                                 VALOR,	
                                 RESPONSABLE)


### 544 Tasa de participación de mujeres y varones entre 14 y 49 años de edad, con presencia de menores de 13 en el hogar


## País Urbano


ech_svy_1 <- ech_svy %>%
  filter(bd_region == 1 & menor13H==1 & (bc_pe3>13 | bc_pe3<50))

# Total


NOMINDICADOR <- 
  c("Tasa de participación de mujeres y varones entre 14 y 49 años con presencia de menores de 13 en el hogar (Varón)",
    "Tasa de participación de mujeres y varones entre 14 y 49 años con presencia de menores de 13 en el hogar (Mujer)")


var_int <- c("activos1", "activos2")
a_sem <- sapply(ech_svy_1$variables %>% select(all_of(var_int)), function(x){
  ech_svy_1 %>%
    srvyr::summarise(stat = srvyr::survey_mean(x, na.rm = TRUE)) 
})

c_ano <- matrix(,2,1)
for (i in 1:2) {
  c_ano[i,1] <- c(as.numeric(c(a_sem[1,i])))
}

colnames(c_ano) <- c("VALOR")
c_ano <- cbind(c_ano, NOMINDICADOR)
c_ano <- as.data.frame(c_ano)
BASE_MOTOR <- cbind(c_ano,BASE_AUX)

# Tramo de edad

a_edad <- function(y) {
  base <- subset(ech_svy_1, tramo_edad3 == y) 
  resultados  <-  sapply(base$variables %>% select(all_of(var_int)), function(x){
    base %>%
      srvyr::summarise(stat1 = srvyr::survey_mean(x, na.rm = TRUE))
    
  })
  y <- as.numeric(resultados[1,])
}

a_e_edad = matrix(, nrow = 2, ncol = 2)

for(i in 1:2){
  a_e_edad[,i] <- a_edad(y = i)
}

c_edad <- as.data.frame(a_e_edad)

c_edad_1 <- cbind(as.data.frame(c_edad[,1]), NOMINDICADOR, BASE_AUX)
c_edad_1 <- c_edad_1 %>% dplyr::mutate(EDAD = "Menor de 30 años")
c_edad_2 <- cbind(as.data.frame(c_edad[,2]), NOMINDICADOR, BASE_AUX)
c_edad_2 <- c_edad_2 %>% dplyr::mutate(EDAD = "Entre 30 y 49 años")
colnames(c_edad_1) <- colnames(BASE_MOTOR)
colnames(c_edad_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_edad_1,c_edad_2)


# Quintil de ingreso del hogar
a_quintil <- function(y) {
  base <- subset(ech_svy_1, quintilesy == y) 
  resultados  <-  sapply(base$variables %>% select(all_of(var_int)), function(x){
    base %>%
      srvyr::summarise(stat1 = srvyr::survey_mean(x, na.rm = TRUE))
    
  })
  y <- as.numeric(resultados[1,])
}

a_e_quintil = matrix(, nrow = 2, ncol = 5)

for(i in 1:5){
  a_e_quintil[,i] <- a_quintil(y = i)
}

c_quintil <- as.data.frame(a_e_quintil)

c_quintil_1 <- cbind(as.data.frame(c_quintil[,1]), NOMINDICADOR, BASE_AUX)
c_quintil_1 <- c_quintil_1 %>% dplyr::mutate(QUINTIL = "Quintil 1")
c_quintil_2 <- cbind(as.data.frame(c_quintil[,2]), NOMINDICADOR, BASE_AUX)
c_quintil_2 <- c_quintil_2 %>% dplyr::mutate(QUINTIL = "Quintil 2")
c_quintil_3 <- cbind(as.data.frame(c_quintil[,3]), NOMINDICADOR, BASE_AUX)
c_quintil_3 <- c_quintil_3 %>% dplyr::mutate(QUINTIL = "Quintil 3")
c_quintil_4 <- cbind(as.data.frame(c_quintil[,4]), NOMINDICADOR, BASE_AUX)
c_quintil_4 <- c_quintil_4 %>% dplyr::mutate(QUINTIL = "Quintil 4")
c_quintil_5 <- cbind(as.data.frame(c_quintil[,5]), NOMINDICADOR, BASE_AUX)
c_quintil_5 <- c_quintil_5 %>% dplyr::mutate(QUINTIL = "Quintil 5")
colnames(c_quintil_1) <- colnames(BASE_MOTOR)
colnames(c_quintil_2) <- colnames(BASE_MOTOR)
colnames(c_quintil_3) <- colnames(BASE_MOTOR)
colnames(c_quintil_4) <- colnames(BASE_MOTOR)
colnames(c_quintil_5) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_quintil_1,c_quintil_2, c_quintil_3, c_quintil_4, c_quintil_5)




# Base motor

BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(CODIND = "544")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(CATEGORIA = "Cuidados y educación")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(PESTAÑA = "País urbano")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(PAÍS = "Uruguay")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(ANIO = 2023)
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(RESPONSABLE = "Sharon Katzkowicz")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(ASCENDENCIA = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(DEPARTAMENTOUY = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(URBANORURALUY = "Urbano (más de 5.000 habitantes)")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(POBRE = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(NSE = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(NIVELEDU = "Todos")
BASE_MOTOR <- as.data.frame(BASE_MOTOR)

BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(SEXO = case_when(SEXO == "" ~ "Todos", SEXO == "MUJERES"~"Mujeres", SEXO=="VARONES"~"Varones"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(QUINTIL = case_when(QUINTIL == "" ~ "Todos",
                                                               QUINTIL == "Quintil 1"~"Quintil 1", 
                                                               QUINTIL == "Quintil 2"~"Quintil 2",
                                                               QUINTIL == "Quintil 3"~"Quintil 3", 
                                                               QUINTIL == "Quintil 4"~"Quintil 4",
                                                               QUINTIL == "Quintil 5"~"Quintil 5"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(EDAD = case_when(EDAD == "" ~ "Todos",
                                                            EDAD == "Menor de 30 años"~"Menor de 30 años", 
                                                            EDAD == "Entre 30 y 49 años"~"Entre 30 y 49 años"))


m544_pu <- BASE_MOTOR %>% select(CODIND,	
                                 NOMINDICADOR,	
                                 CATEGORIA,	
                                 PESTAÑA,	
                                 SEXO,	
                                 ASCENDENCIA,	
                                 QUINTIL,	
                                 DEPARTAMENTOUY,	
                                 URBANORURALUY,	
                                 EDAD,	
                                 POBRE,
                                 NSE,
                                 NIVELEDU,
                                 PAÍS,	
                                 ANIO,	
                                 VALOR,	
                                 RESPONSABLE)	


## Total país

ech_svy_1 <- ech_svy %>%
  filter(menor13H==1 & (bc_pe3>13 | bc_pe3<50))

# Total

NOMINDICADOR <- 
  c(  c("Tasa de participación de mujeres y varones entre 14 y 49 años con presencia de menores de 13 en el hogar (Varón)",
        "Tasa de participación de mujeres y varones entre 14 y 49 años con presencia de menores de 13 en el hogar (Mujer)"))


var_int <- c("activos1", "activos2")
a_sem <- sapply(ech_svy_1$variables %>% select(all_of(var_int)), function(x){
  ech_svy_1 %>%
    srvyr::summarise(stat = srvyr::survey_mean(x, na.rm = TRUE)) 
})

c_ano <- matrix(,2,1)
for (i in 1:2) {
  c_ano[i,1] <- c(as.numeric(c(a_sem[1,i])))
}


colnames(c_ano) <- c("VALOR")
c_ano <- cbind(c_ano, NOMINDICADOR)
c_ano <- as.data.frame(c_ano)
BASE_MOTOR <- cbind(c_ano,BASE_AUX)


# Región

a_reg <- function(y) {
  base <- subset(ech_svy_1, bd_region == y) 
  resultados  <-  sapply(base$variables %>% select(all_of(var_int)), function(x){
    base %>%
      srvyr::summarise(stat1 = srvyr::survey_mean(x, na.rm = TRUE))
    
  })
  y <- as.numeric(resultados[1,])
}

a_e_reg = matrix(, nrow = 2, ncol = 3)

for(i in 1:3){
  a_e_reg[,i] <- a_reg(y = i)
}

c_reg <- as.data.frame(a_e_reg[,2:3])
c_reg_1 <- cbind(as.data.frame(c_reg[,1]), NOMINDICADOR, BASE_AUX)
c_reg_1 <- c_reg_1 %>% dplyr::mutate(URBANORURALUY = "URBANO (MENOS DE 5.000 HABITANTES)")
c_reg_2 <- cbind(as.data.frame(c_reg[,2]), NOMINDICADOR, BASE_AUX)
c_reg_2 <- c_reg_2 %>% dplyr::mutate(URBANORURALUY = "RURAL DISPERSO")
colnames(c_reg_1) <- colnames(BASE_MOTOR)
colnames(c_reg_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_reg_1,c_reg_2)


# Tramo de edad

a_edad <- function(y) {
  base <- subset(ech_svy_1, tramo_edad3 == y) 
  resultados  <-  sapply(base$variables %>% select(all_of(var_int)), function(x){
    base %>%
      srvyr::summarise(stat1 = srvyr::survey_mean(x, na.rm = TRUE))
    
  })
  y <- as.numeric(resultados[1,])
}

a_e_edad = matrix(, nrow = 2, ncol = 2)

for(i in 1:2){
  a_e_edad[,i] <- a_edad(y = i)
}


c_edad <- as.data.frame(a_e_edad)

c_edad_1 <- cbind(as.data.frame(c_edad[,1]), NOMINDICADOR, BASE_AUX)
c_edad_1 <- c_edad_1 %>% dplyr::mutate(EDAD = "Menor de 30 años")
c_edad_2 <- cbind(as.data.frame(c_edad[,2]), NOMINDICADOR, BASE_AUX)
c_edad_2 <- c_edad_2 %>% dplyr::mutate(EDAD = "Entre 30 y 49 años")
colnames(c_edad_1) <- colnames(BASE_MOTOR)
colnames(c_edad_2) <- colnames(BASE_MOTOR)
colnames(c_edad_3) <- colnames(BASE_MOTOR)
colnames(c_edad_4) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_edad_1,c_edad_2)



# Ascendencia

a_ascen <- function(y) {
  base <- subset(ech_svy_1, bd_e29_1 == y) 
  resultados  <-  sapply(base$variables %>% select(all_of(var_int)), function(x){
    base %>%
      srvyr::summarise(stat1 = srvyr::survey_mean(x, na.rm = TRUE))
    
  })
  y <- as.numeric(resultados[1,])
}

a_e_ascen = matrix(, nrow = 2, ncol = 2)

for(i in 1:2){
  a_e_ascen[,i] <- a_ascen(y = i)
}

c_ascen <- as.data.frame(a_e_ascen)
c_ascen_1 <- cbind(as.data.frame(c_ascen[,1]), NOMINDICADOR, BASE_AUX)
c_ascen_1 <- c_ascen_1 %>% dplyr::mutate(ASCENDENCIA = "AFRO")
c_ascen_2 <- cbind(as.data.frame(c_ascen[,2]), NOMINDICADOR, BASE_AUX)
c_ascen_2 <- c_ascen_2 %>% dplyr::mutate(ASCENDENCIA = "NO AFRO")
colnames(c_ascen_1) <- colnames(BASE_MOTOR)
colnames(c_ascen_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_ascen_1,c_ascen_2)





# Quintil de ingreso del hogar
a_quintil <- function(y) {
  base <- subset(ech_svy_1, quintilesy == y) 
  resultados  <-  sapply(base$variables %>% select(all_of(var_int)), function(x){
    base %>%
      srvyr::summarise(stat1 = srvyr::survey_mean(x, na.rm = TRUE))
    
  })
  y <- as.numeric(resultados[1,])
}

a_e_quintil = matrix(, nrow = 2, ncol = 5)

for(i in 1:5){
  a_e_quintil[,i] <- a_quintil(y = i)
}

c_quintil <- as.data.frame(a_e_quintil)

c_quintil_1 <- cbind(as.data.frame(c_quintil[,1]), NOMINDICADOR, BASE_AUX)
c_quintil_1 <- c_quintil_1 %>% dplyr::mutate(QUINTIL = "Quintil 1")
c_quintil_2 <- cbind(as.data.frame(c_quintil[,2]), NOMINDICADOR, BASE_AUX)
c_quintil_2 <- c_quintil_2 %>% dplyr::mutate(QUINTIL = "Quintil 2")
c_quintil_3 <- cbind(as.data.frame(c_quintil[,3]), NOMINDICADOR, BASE_AUX)
c_quintil_3 <- c_quintil_3 %>% dplyr::mutate(QUINTIL = "Quintil 3")
c_quintil_4 <- cbind(as.data.frame(c_quintil[,4]), NOMINDICADOR, BASE_AUX)
c_quintil_4 <- c_quintil_4 %>% dplyr::mutate(QUINTIL = "Quintil 4")
c_quintil_5 <- cbind(as.data.frame(c_quintil[,5]), NOMINDICADOR, BASE_AUX)
c_quintil_5 <- c_quintil_5 %>% dplyr::mutate(QUINTIL = "Quintil 5")
colnames(c_quintil_1) <- colnames(BASE_MOTOR)
colnames(c_quintil_2) <- colnames(BASE_MOTOR)
colnames(c_quintil_3) <- colnames(BASE_MOTOR)
colnames(c_quintil_4) <- colnames(BASE_MOTOR)
colnames(c_quintil_5) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_quintil_1,c_quintil_2, c_quintil_3, c_quintil_4, c_quintil_5)


# Pobreza

a_pobre <- function(y) {
  base <- subset(ech_svy_1, pobre_aux == y) 
  resultados  <-  sapply(base$variables %>% select(all_of(var_int)), function(x){
    base %>%
      srvyr::summarise(stat1 = srvyr::survey_mean(x, na.rm = TRUE))
    
  })
  y <- as.numeric(resultados[1,])
}

a_e_pobre = matrix(, nrow = 2, ncol = 2)

for(i in 1:2){
  a_e_pobre[,i] <- a_pobre(y = i)
}

c_pobre <- as.data.frame(a_e_pobre)
c_pobre_1 <- cbind(as.data.frame(c_pobre[,1]), NOMINDICADOR, BASE_AUX)
c_pobre_1 <- c_pobre_1 %>% dplyr::mutate(POBRE = "POBRE")
c_pobre_2 <- cbind(as.data.frame(c_pobre[,2]), NOMINDICADOR, BASE_AUX)
c_pobre_2 <- c_pobre_2 %>% dplyr::mutate(POBRE = "NO POBRE")
colnames(c_pobre_1) <- colnames(BASE_MOTOR)
colnames(c_pobre_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_pobre_1,c_pobre_2)


# Base motor

BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(CODIND = "544")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(CATEGORIA = "Cuidados y educación")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(PESTAÑA = "Total país")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(PAÍS = "Uruguay")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(ANIO = 2023)
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(RESPONSABLE = "Sharon Katzkowicz")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(DEPARTAMENTOUY = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(NSE = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(NIVELEDU = "Todos")
BASE_MOTOR <- as.data.frame(BASE_MOTOR)


BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(ASCENDENCIA = case_when(ASCENDENCIA == "AFRO" ~ "Afro", 
                                                                   ASCENDENCIA == "NO AFRO" ~ "No afro",
                                                                   ASCENDENCIA == "" ~ "Todos"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(URBANORURALUY = case_when(URBANORURALUY == "URBANO (MENOS DE 5.000 HABITANTES)" ~ "Urbano (menos de 5.000 habitantes)",
                                                                     URBANORURALUY == "RURAL DISPERSO" ~ "Rural disperso", 
                                                                     URBANORURALUY == "" ~ "Total país"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(SEXO = case_when(SEXO == "MUJERES" ~ "Mujeres", 
                                                            SEXO=="VARONES" ~ "Varones",
                                                            SEXO == "" ~ "Todos"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(QUINTIL = case_when(QUINTIL == "Quintil 1"~"Quintil 1", 
                                                               QUINTIL == "Quintil 2"~"Quintil 2",
                                                               QUINTIL == "Quintil 3"~"Quintil 3", 
                                                               QUINTIL == "Quintil 4"~"Quintil 4",
                                                               QUINTIL == "Quintil 5"~"Quintil 5",
                                                               QUINTIL == "" ~ "Todos"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(EDAD = case_when(EDAD == "Menor de 30 años"~"Menor de 30 años", 
                                                            EDAD == "Entre 30 y 49 años"~"Entre 30 y 49 años",
                                                            EDAD == "" ~ "Todos"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(POBRE = case_when(POBRE == "POBRE" ~ "Pobre", 
                                                             POBRE == "NO POBRE" ~ "No pobre",
                                                             POBRE == "" ~ "Todos"))


m544_tp <- BASE_MOTOR %>% select(CODIND,	
                                 NOMINDICADOR,	
                                 CATEGORIA,	
                                 PESTAÑA,	
                                 SEXO,	
                                 ASCENDENCIA,	
                                 QUINTIL,	
                                 DEPARTAMENTOUY,	
                                 URBANORURALUY,	
                                 EDAD,	
                                 POBRE,
                                 NSE,
                                 NIVELEDU,
                                 PAÍS,	
                                 ANIO,	
                                 VALOR,	
                                 RESPONSABLE)


### 560 Porcentaje de niños entre 0 y 5 años sin cobertura de salud

## País Urbano

# Total

NOMINDICADOR <- 
  c("Porcentaje de niños menores de 5 años sin cobertura de salud")

c_ano <- ech_svy %>%
  srvyr::filter(bd_region == 1 & bc_pe3< 6) %>%
  srvyr::summarise(colname = srvyr::survey_mean(nocobert))
c_ano <- as.numeric(c_ano$colname)

c_ano <- cbind(c_ano, NOMINDICADOR)
colnames(c_ano) <- c("VALOR", "NOMINDICADOR")
c_ano <- as.data.frame(c_ano)
BASE_MOTOR <- cbind(c_ano,BASE_AUX)

# Sexo 

b_sexo <- function(x) {
  x <- ech_svy %>%
    filter(bc_pe2 == x  & bd_region == 1  & bc_pe3< 6) %>%
    srvyr::summarise(colname = srvyr::survey_mean(nocobert))
  x <- mean(x$colname)
}       

b_e_sexo <- numeric()

for(i in 1:2){
  b_e_sexo[i] <- b_sexo(x = i)
}         

c_sexo <- as.data.frame(b_e_sexo)
c_sexo_1 <- cbind(as.data.frame(c_sexo[1,]), NOMINDICADOR, BASE_AUX)
c_sexo_1 <- c_sexo_1 %>% dplyr::mutate(SEXO = "VARONES")
c_sexo_2 <- cbind(as.data.frame(c_sexo[2,]), NOMINDICADOR, BASE_AUX)
c_sexo_2 <- c_sexo_2 %>% dplyr::mutate(SEXO = "MUJERES")
colnames(c_sexo_1) <- colnames(BASE_MOTOR)
colnames(c_sexo_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_sexo_1,c_sexo_2)

# Quintil de ingreso del hogar

b_quintil <- function(x) {
  x <- ech_svy %>%
    filter(quintilesy == x  & bd_region == 1  & bc_pe3< 6) %>%
    srvyr::summarise(colname = srvyr::survey_mean(nocobert))
  x <- mean(x$colname)
}       

b_e_quintil <- numeric()

for(i in 1:5){
  b_e_quintil[i] <- b_quintil(x = i)
}         

c_quintil <- as.data.frame(b_e_quintil)
c_quintil_1 <- cbind(as.data.frame(c_quintil[1,]), NOMINDICADOR, BASE_AUX)
c_quintil_1 <- c_quintil_1 %>% dplyr::mutate(QUINTIL = "Quintil 1")
c_quintil_2 <- cbind(as.data.frame(c_quintil[2,]), NOMINDICADOR, BASE_AUX)
c_quintil_2 <- c_quintil_2 %>% dplyr::mutate(QUINTIL = "Quintil 2")
c_quintil_3 <- cbind(as.data.frame(c_quintil[3,]), NOMINDICADOR, BASE_AUX)
c_quintil_3 <- c_quintil_3 %>% dplyr::mutate(QUINTIL = "Quintil 3")
c_quintil_4 <- cbind(as.data.frame(c_quintil[4,]), NOMINDICADOR, BASE_AUX)
c_quintil_4 <- c_quintil_4 %>% dplyr::mutate(QUINTIL = "Quintil 4")
c_quintil_5 <- cbind(as.data.frame(c_quintil[5,]), NOMINDICADOR, BASE_AUX)
c_quintil_5 <- c_quintil_5 %>% dplyr::mutate(QUINTIL = "Quintil 5")
colnames(c_quintil_1) <- colnames(BASE_MOTOR)
colnames(c_quintil_2) <- colnames(BASE_MOTOR)
colnames(c_quintil_3) <- colnames(BASE_MOTOR)
colnames(c_quintil_4) <- colnames(BASE_MOTOR)
colnames(c_quintil_5) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_quintil_1,c_quintil_2, c_quintil_3, c_quintil_4, c_quintil_5)


# Base motor

BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(CODIND = "560")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(CATEGORIA = "Salud infantil")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(PESTAÑA = "País urbano")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(PAÍS = "Uruguay")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(ANIO = 2023)
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(RESPONSABLE = "Sharon Katzkowicz")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(ASCENDENCIA = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(DEPARTAMENTOUY = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(URBANORURALUY = "Urbano (más de 5.000 habitantes)")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(POBRE = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(NSE = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(NIVELEDU = "Todos")
BASE_MOTOR <- as.data.frame(BASE_MOTOR)

BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(SEXO = case_when(SEXO == "" ~ "Todos", SEXO == "MUJERES"~"Mujeres", SEXO=="VARONES"~"Varones"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(QUINTIL = case_when(QUINTIL == "" ~ "Todos",
                                                               QUINTIL == "Quintil 1"~"Quintil 1", 
                                                               QUINTIL == "Quintil 2"~"Quintil 2",
                                                               QUINTIL == "Quintil 3"~"Quintil 3", 
                                                               QUINTIL == "Quintil 4"~"Quintil 4",
                                                               QUINTIL == "Quintil 5"~"Quintil 5"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(EDAD = case_when(EDAD == "" ~ "Todos"))


m560_pu <- BASE_MOTOR %>% select(CODIND,	
                                 NOMINDICADOR,	
                                 CATEGORIA,	
                                 PESTAÑA,	
                                 SEXO,	
                                 ASCENDENCIA,	
                                 QUINTIL,	
                                 DEPARTAMENTOUY,	
                                 URBANORURALUY,	
                                 EDAD,	
                                 POBRE,
                                 NSE,
                                 NIVELEDU,
                                 PAÍS,	
                                 ANIO,	
                                 VALOR,	
                                 RESPONSABLE)	


## Total país

# Total


NOMINDICADOR <- 
  c("Porcentaje de niños menores de 5 años sin cobertura de salud")

c_ano <- ech_svy %>%
  filter(bc_pe3< 6) %>%
  srvyr::summarise(colname = srvyr::survey_mean(nocobert))
c_ano <- as.numeric(c_ano$colname)

c_ano <- cbind(c_ano, NOMINDICADOR)
colnames(c_ano) <- c("VALOR", "NOMINDICADOR")
c_ano <- as.data.frame(c_ano)
BASE_MOTOR <- cbind(c_ano,BASE_AUX)



# Región

b_reg <- function(x) {
  x <- ech_svy %>%
    filter(bd_region == x  & bc_pe3< 6) %>%
    srvyr::summarise(colname = srvyr::survey_mean(nocobert))
  x <- mean(x$colname)
}       

b_e_reg <- numeric()

for(i in 1:3){
  b_e_reg[i] <- b_reg(x = i)
}         

c_reg <- as.data.frame(b_e_reg)
c_reg_1 <- cbind(as.data.frame(c_reg[2,]), NOMINDICADOR, BASE_AUX)
c_reg_1 <- c_reg_1 %>% dplyr::mutate(URBANORURALUY = "URBANO (MENOS DE 5.000 HABITANTES)")
c_reg_2 <- cbind(as.data.frame(c_reg[3,]), NOMINDICADOR, BASE_AUX)
c_reg_2 <- c_reg_2 %>% dplyr::mutate(URBANORURALUY = "RURAL DISPERSO")
colnames(c_reg_1) <- colnames(BASE_MOTOR)
colnames(c_reg_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_reg_1,c_reg_2)


# Sexo 

b_sexo <- function(x) {
  x <- ech_svy %>%
    filter(bc_pe2 == x  & bc_pe3< 6) %>%
    srvyr::summarise(colname = srvyr::survey_mean(nocobert))
  x <- mean(x$colname)
}       

b_e_sexo <- numeric()

for(i in 1:2){
  b_e_sexo[i] <- b_sexo(x = i)
}         

c_sexo <- as.data.frame(b_e_sexo)
c_sexo_1 <- cbind(as.data.frame(c_sexo[1,]), NOMINDICADOR, BASE_AUX)
c_sexo_1 <- c_sexo_1 %>% dplyr::mutate(SEXO = "VARONES")
c_sexo_2 <- cbind(as.data.frame(c_sexo[2,]), NOMINDICADOR, BASE_AUX)
c_sexo_2 <- c_sexo_2 %>% dplyr::mutate(SEXO = "MUJERES")
colnames(c_sexo_1) <- colnames(BASE_MOTOR)
colnames(c_sexo_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_sexo_1,c_sexo_2)



# Ascendencia

b_ascen <- function(x) {
  x <- ech_svy %>%
    filter(bd_e29_1 == x  & bc_pe3< 6) %>%
    srvyr::summarise(colname = srvyr::survey_mean(nocobert))
  x <- mean(x$colname)
}       

b_e_ascen <- numeric()

for(i in 1:2){
  b_e_ascen[i] <- b_ascen(x = i)
}         

c_ascen <- as.data.frame(b_e_ascen)
c_ascen_1 <- cbind(as.data.frame(c_ascen[1,]), NOMINDICADOR, BASE_AUX)
c_ascen_1 <- c_ascen_1 %>% dplyr::mutate(ASCENDENCIA = "AFRO")
c_ascen_2 <- cbind(as.data.frame(c_ascen[2,]), NOMINDICADOR, BASE_AUX)
c_ascen_2 <- c_ascen_2 %>% dplyr::mutate(ASCENDENCIA = "NO AFRO")
colnames(c_ascen_1) <- colnames(BASE_MOTOR)
colnames(c_ascen_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_ascen_1,c_ascen_2)


# Quintil de ingreso del hogar

b_quintil <- function(x) {
  x <- ech_svy %>%
    filter(quintilesy == x  & bc_pe3< 6) %>%
    srvyr::summarise(colname = srvyr::survey_mean(nocobert))
  x <- mean(x$colname)
}       

b_e_quintil <- numeric()

for(i in 1:5){
  b_e_quintil[i] <- b_quintil(x = i)
}         

c_quintil <- as.data.frame(b_e_quintil)
c_quintil_1 <- cbind(as.data.frame(c_quintil[1,]), NOMINDICADOR, BASE_AUX)
c_quintil_1 <- c_quintil_1 %>% dplyr::mutate(QUINTIL = "Quintil 1")
c_quintil_2 <- cbind(as.data.frame(c_quintil[2,]), NOMINDICADOR, BASE_AUX)
c_quintil_2 <- c_quintil_2 %>% dplyr::mutate(QUINTIL = "Quintil 2")
c_quintil_3 <- cbind(as.data.frame(c_quintil[3,]), NOMINDICADOR, BASE_AUX)
c_quintil_3 <- c_quintil_3 %>% dplyr::mutate(QUINTIL = "Quintil 3")
c_quintil_4 <- cbind(as.data.frame(c_quintil[4,]), NOMINDICADOR, BASE_AUX)
c_quintil_4 <- c_quintil_4 %>% dplyr::mutate(QUINTIL = "Quintil 4")
c_quintil_5 <- cbind(as.data.frame(c_quintil[5,]), NOMINDICADOR, BASE_AUX)
c_quintil_5 <- c_quintil_5 %>% dplyr::mutate(QUINTIL = "Quintil 5")
colnames(c_quintil_1) <- colnames(BASE_MOTOR)
colnames(c_quintil_2) <- colnames(BASE_MOTOR)
colnames(c_quintil_3) <- colnames(BASE_MOTOR)
colnames(c_quintil_4) <- colnames(BASE_MOTOR)
colnames(c_quintil_5) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_quintil_1,c_quintil_2, c_quintil_3, c_quintil_4, c_quintil_5)


# Pobreza

b_pobre <- function(x) {
  x <- ech_svy %>%
    filter(pobre_aux == x & bc_pe3< 6) %>%
    srvyr::summarise(colname = srvyr::survey_mean(nocobert))
  x <- mean(x$colname)
}       

b_e_pobre <- numeric()

for(i in 1:2){
  b_e_pobre[i] <- b_pobre(x = i)
}         

c_pobre <- as.data.frame(b_e_pobre)
c_pobre_1 <- cbind(as.data.frame(c_pobre[1,]), NOMINDICADOR, BASE_AUX)
c_pobre_1 <- c_pobre_1 %>% dplyr::mutate(POBRE = "POBRE")
c_pobre_2 <- cbind(as.data.frame(c_pobre[2,]), NOMINDICADOR, BASE_AUX)
c_pobre_2 <- c_pobre_2 %>% dplyr::mutate(POBRE = "NO POBRE")
colnames(c_pobre_1) <- colnames(BASE_MOTOR)
colnames(c_pobre_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_pobre_1,c_pobre_2)


# Base motor

BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(CODIND = "560")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(CATEGORIA = "Salud infantil")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(PESTAÑA = "Total país")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(PAÍS = "Uruguay")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(ANIO = 2023)
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(RESPONSABLE = "Sharon Katzkowicz")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(DEPARTAMENTOUY = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(NSE = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(NIVELEDU = "Todos")
BASE_MOTOR <- as.data.frame(BASE_MOTOR)


BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(ASCENDENCIA = case_when(ASCENDENCIA == "AFRO" ~ "Afro", 
                                                                   ASCENDENCIA == "NO AFRO" ~ "No afro",
                                                                   ASCENDENCIA == "" ~ "Todos"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(URBANORURALUY = case_when(URBANORURALUY == "URBANO (MENOS DE 5.000 HABITANTES)" ~ "Urbano (menos de 5.000 habitantes)",
                                                                     URBANORURALUY == "RURAL DISPERSO" ~ "Rural disperso", 
                                                                     URBANORURALUY == "" ~ "Total país"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(SEXO = case_when(SEXO == "MUJERES" ~ "Mujeres", 
                                                            SEXO=="VARONES" ~ "Varones",
                                                            SEXO == "" ~ "Todos"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(QUINTIL = case_when(QUINTIL == "Quintil 1"~"Quintil 1", 
                                                               QUINTIL == "Quintil 2"~"Quintil 2",
                                                               QUINTIL == "Quintil 3"~"Quintil 3", 
                                                               QUINTIL == "Quintil 4"~"Quintil 4",
                                                               QUINTIL == "Quintil 5"~"Quintil 5",
                                                               QUINTIL == "" ~ "Todos"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(EDAD = case_when(EDAD == "" ~ "Todos"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(POBRE = case_when(POBRE == "POBRE" ~ "Pobre", 
                                                             POBRE == "NO POBRE" ~ "No pobre",
                                                             POBRE == "" ~ "Todos"))


m560_tp <- BASE_MOTOR %>% select(CODIND,	
                                 NOMINDICADOR,	
                                 CATEGORIA,	
                                 PESTAÑA,	
                                 SEXO,	
                                 ASCENDENCIA,	
                                 QUINTIL,	
                                 DEPARTAMENTOUY,	
                                 URBANORURALUY,	
                                 EDAD,	
                                 POBRE,
                                 NSE,
                                 NIVELEDU,
                                 PAÍS,	
                                 ANIO,	
                                 VALOR,	
                                 RESPONSABLE)



### 561 Porcentaje de niños entre 6 y 12 años sin cobertura de salud

## País Urbano

# Total

NOMINDICADOR <- 
  c("Porcentaje de niños entre 6 y 12 años sin cobertura de salud")

c_ano <- ech_svy %>%
  srvyr::filter(bd_region == 1 & bc_pe3> 5 & bc_pe3<13) %>%
  srvyr::summarise(colname = srvyr::survey_mean(nocobert))
c_ano <- as.numeric(c_ano$colname)

c_ano <- cbind(c_ano, NOMINDICADOR)
colnames(c_ano) <- c("VALOR", "NOMINDICADOR")
c_ano <- as.data.frame(c_ano)
BASE_MOTOR <- cbind(c_ano,BASE_AUX)

# Sexo 

b_sexo <- function(x) {
  x <- ech_svy %>%
    filter(bc_pe2 == x  & bd_region == 1  & bc_pe3> 5 & bc_pe3<13) %>%
    srvyr::summarise(colname = srvyr::survey_mean(nocobert))
  x <- mean(x$colname)
}       

b_e_sexo <- numeric()

for(i in 1:2){
  b_e_sexo[i] <- b_sexo(x = i)
}         

c_sexo <- as.data.frame(b_e_sexo)
c_sexo_1 <- cbind(as.data.frame(c_sexo[1,]), NOMINDICADOR, BASE_AUX)
c_sexo_1 <- c_sexo_1 %>% dplyr::mutate(SEXO = "VARONES")
c_sexo_2 <- cbind(as.data.frame(c_sexo[2,]), NOMINDICADOR, BASE_AUX)
c_sexo_2 <- c_sexo_2 %>% dplyr::mutate(SEXO = "MUJERES")
colnames(c_sexo_1) <- colnames(BASE_MOTOR)
colnames(c_sexo_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_sexo_1,c_sexo_2)

# Quintil de ingreso del hogar

b_quintil <- function(x) {
  x <- ech_svy %>%
    filter(quintilesy == x  & bd_region == 1  & bc_pe3> 5 & bc_pe3<13) %>%
    srvyr::summarise(colname = srvyr::survey_mean(nocobert))
  x <- mean(x$colname)
}       

b_e_quintil <- numeric()

for(i in 1:5){
  b_e_quintil[i] <- b_quintil(x = i)
}         

c_quintil <- as.data.frame(b_e_quintil)
c_quintil_1 <- cbind(as.data.frame(c_quintil[1,]), NOMINDICADOR, BASE_AUX)
c_quintil_1 <- c_quintil_1 %>% dplyr::mutate(QUINTIL = "Quintil 1")
c_quintil_2 <- cbind(as.data.frame(c_quintil[2,]), NOMINDICADOR, BASE_AUX)
c_quintil_2 <- c_quintil_2 %>% dplyr::mutate(QUINTIL = "Quintil 2")
c_quintil_3 <- cbind(as.data.frame(c_quintil[3,]), NOMINDICADOR, BASE_AUX)
c_quintil_3 <- c_quintil_3 %>% dplyr::mutate(QUINTIL = "Quintil 3")
c_quintil_4 <- cbind(as.data.frame(c_quintil[4,]), NOMINDICADOR, BASE_AUX)
c_quintil_4 <- c_quintil_4 %>% dplyr::mutate(QUINTIL = "Quintil 4")
c_quintil_5 <- cbind(as.data.frame(c_quintil[5,]), NOMINDICADOR, BASE_AUX)
c_quintil_5 <- c_quintil_5 %>% dplyr::mutate(QUINTIL = "Quintil 5")
colnames(c_quintil_1) <- colnames(BASE_MOTOR)
colnames(c_quintil_2) <- colnames(BASE_MOTOR)
colnames(c_quintil_3) <- colnames(BASE_MOTOR)
colnames(c_quintil_4) <- colnames(BASE_MOTOR)
colnames(c_quintil_5) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_quintil_1,c_quintil_2, c_quintil_3, c_quintil_4, c_quintil_5)


# Base motor

BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(CODIND = "561")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(CATEGORIA = "Salud infantil")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(PESTAÑA = "País urbano")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(PAÍS = "Uruguay")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(ANIO = 2023)
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(RESPONSABLE = "Sharon Katzkowicz")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(ASCENDENCIA = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(DEPARTAMENTOUY = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(URBANORURALUY = "Urbano (más de 5.000 habitantes)")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(POBRE = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(NSE = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(NIVELEDU = "Todos")
BASE_MOTOR <- as.data.frame(BASE_MOTOR)

BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(SEXO = case_when(SEXO == "" ~ "Todos", SEXO == "MUJERES"~"Mujeres", SEXO=="VARONES"~"Varones"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(QUINTIL = case_when(QUINTIL == "" ~ "Todos",
                                                               QUINTIL == "Quintil 1"~"Quintil 1", 
                                                               QUINTIL == "Quintil 2"~"Quintil 2",
                                                               QUINTIL == "Quintil 3"~"Quintil 3", 
                                                               QUINTIL == "Quintil 4"~"Quintil 4",
                                                               QUINTIL == "Quintil 5"~"Quintil 5"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(EDAD = case_when(EDAD == "" ~ "Todos"))


m561_pu <- BASE_MOTOR %>% select(CODIND,	
                                 NOMINDICADOR,	
                                 CATEGORIA,	
                                 PESTAÑA,	
                                 SEXO,	
                                 ASCENDENCIA,	
                                 QUINTIL,	
                                 DEPARTAMENTOUY,	
                                 URBANORURALUY,	
                                 EDAD,	
                                 POBRE,
                                 NSE,
                                 NIVELEDU,
                                 PAÍS,	
                                 ANIO,	
                                 VALOR,	
                                 RESPONSABLE)	


## Total país

# Total


NOMINDICADOR <- 
  c("Porcentaje de niños entre 6 y 12 años sin cobertura de salud")

c_ano <- ech_svy %>%
  filter(bc_pe3> 5 & bc_pe3<13) %>%
  srvyr::summarise(colname = srvyr::survey_mean(nocobert))
c_ano <- as.numeric(c_ano$colname)

c_ano <- cbind(c_ano, NOMINDICADOR)
colnames(c_ano) <- c("VALOR", "NOMINDICADOR")
c_ano <- as.data.frame(c_ano)
BASE_MOTOR <- cbind(c_ano,BASE_AUX)



# Región

b_reg <- function(x) {
  x <- ech_svy %>%
    filter(bd_region == x  & bc_pe3> 5 & bc_pe3<13) %>%
    srvyr::summarise(colname = srvyr::survey_mean(nocobert))
  x <- mean(x$colname)
}       

b_e_reg <- numeric()

for(i in 1:3){
  b_e_reg[i] <- b_reg(x = i)
}         

c_reg <- as.data.frame(b_e_reg)
c_reg_1 <- cbind(as.data.frame(c_reg[2,]), NOMINDICADOR, BASE_AUX)
c_reg_1 <- c_reg_1 %>% dplyr::mutate(URBANORURALUY = "URBANO (MENOS DE 5.000 HABITANTES)")
c_reg_2 <- cbind(as.data.frame(c_reg[3,]), NOMINDICADOR, BASE_AUX)
c_reg_2 <- c_reg_2 %>% dplyr::mutate(URBANORURALUY = "RURAL DISPERSO")
colnames(c_reg_1) <- colnames(BASE_MOTOR)
colnames(c_reg_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_reg_1,c_reg_2)


# Sexo 

b_sexo <- function(x) {
  x <- ech_svy %>%
    filter(bc_pe2 == x  & bc_pe3> 5 & bc_pe3<13) %>%
    srvyr::summarise(colname = srvyr::survey_mean(nocobert))
  x <- mean(x$colname)
}       

b_e_sexo <- numeric()

for(i in 1:2){
  b_e_sexo[i] <- b_sexo(x = i)
}         

c_sexo <- as.data.frame(b_e_sexo)
c_sexo_1 <- cbind(as.data.frame(c_sexo[1,]), NOMINDICADOR, BASE_AUX)
c_sexo_1 <- c_sexo_1 %>% dplyr::mutate(SEXO = "VARONES")
c_sexo_2 <- cbind(as.data.frame(c_sexo[2,]), NOMINDICADOR, BASE_AUX)
c_sexo_2 <- c_sexo_2 %>% dplyr::mutate(SEXO = "MUJERES")
colnames(c_sexo_1) <- colnames(BASE_MOTOR)
colnames(c_sexo_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_sexo_1,c_sexo_2)



# Ascendencia

b_ascen <- function(x) {
  x <- ech_svy %>%
    filter(bd_e29_1 == x  & bc_pe3> 5 & bc_pe3<13) %>%
    srvyr::summarise(colname = srvyr::survey_mean(nocobert))
  x <- mean(x$colname)
}       

b_e_ascen <- numeric()

for(i in 1:2){
  b_e_ascen[i] <- b_ascen(x = i)
}         

c_ascen <- as.data.frame(b_e_ascen)
c_ascen_1 <- cbind(as.data.frame(c_ascen[1,]), NOMINDICADOR, BASE_AUX)
c_ascen_1 <- c_ascen_1 %>% dplyr::mutate(ASCENDENCIA = "AFRO")
c_ascen_2 <- cbind(as.data.frame(c_ascen[2,]), NOMINDICADOR, BASE_AUX)
c_ascen_2 <- c_ascen_2 %>% dplyr::mutate(ASCENDENCIA = "NO AFRO")
colnames(c_ascen_1) <- colnames(BASE_MOTOR)
colnames(c_ascen_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_ascen_1,c_ascen_2)


# Quintil de ingreso del hogar

b_quintil <- function(x) {
  x <- ech_svy %>%
    filter(quintilesy == x  & bc_pe3> 5 & bc_pe3<13) %>%
    srvyr::summarise(colname = srvyr::survey_mean(nocobert))
  x <- mean(x$colname)
}       

b_e_quintil <- numeric()

for(i in 1:5){
  b_e_quintil[i] <- b_quintil(x = i)
}         

c_quintil <- as.data.frame(b_e_quintil)
c_quintil_1 <- cbind(as.data.frame(c_quintil[1,]), NOMINDICADOR, BASE_AUX)
c_quintil_1 <- c_quintil_1 %>% dplyr::mutate(QUINTIL = "Quintil 1")
c_quintil_2 <- cbind(as.data.frame(c_quintil[2,]), NOMINDICADOR, BASE_AUX)
c_quintil_2 <- c_quintil_2 %>% dplyr::mutate(QUINTIL = "Quintil 2")
c_quintil_3 <- cbind(as.data.frame(c_quintil[3,]), NOMINDICADOR, BASE_AUX)
c_quintil_3 <- c_quintil_3 %>% dplyr::mutate(QUINTIL = "Quintil 3")
c_quintil_4 <- cbind(as.data.frame(c_quintil[4,]), NOMINDICADOR, BASE_AUX)
c_quintil_4 <- c_quintil_4 %>% dplyr::mutate(QUINTIL = "Quintil 4")
c_quintil_5 <- cbind(as.data.frame(c_quintil[5,]), NOMINDICADOR, BASE_AUX)
c_quintil_5 <- c_quintil_5 %>% dplyr::mutate(QUINTIL = "Quintil 5")
colnames(c_quintil_1) <- colnames(BASE_MOTOR)
colnames(c_quintil_2) <- colnames(BASE_MOTOR)
colnames(c_quintil_3) <- colnames(BASE_MOTOR)
colnames(c_quintil_4) <- colnames(BASE_MOTOR)
colnames(c_quintil_5) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_quintil_1,c_quintil_2, c_quintil_3, c_quintil_4, c_quintil_5)


# Pobreza

b_pobre <- function(x) {
  x <- ech_svy %>%
    filter(pobre_aux == x & bc_pe3> 5 & bc_pe3<13) %>%
    srvyr::summarise(colname = srvyr::survey_mean(nocobert))
  x <- mean(x$colname)
}       

b_e_pobre <- numeric()

for(i in 1:2){
  b_e_pobre[i] <- b_pobre(x = i)
}         

c_pobre <- as.data.frame(b_e_pobre)
c_pobre_1 <- cbind(as.data.frame(c_pobre[1,]), NOMINDICADOR, BASE_AUX)
c_pobre_1 <- c_pobre_1 %>% dplyr::mutate(POBRE = "POBRE")
c_pobre_2 <- cbind(as.data.frame(c_pobre[2,]), NOMINDICADOR, BASE_AUX)
c_pobre_2 <- c_pobre_2 %>% dplyr::mutate(POBRE = "NO POBRE")
colnames(c_pobre_1) <- colnames(BASE_MOTOR)
colnames(c_pobre_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_pobre_1,c_pobre_2)


# Base motor

BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(CODIND = "561")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(CATEGORIA = "Salud infantil")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(PESTAÑA = "Total país")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(PAÍS = "Uruguay")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(ANIO = 2023)
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(RESPONSABLE = "Sharon Katzkowicz")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(DEPARTAMENTOUY = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(NSE = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(NIVELEDU = "Todos")
BASE_MOTOR <- as.data.frame(BASE_MOTOR)


BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(ASCENDENCIA = case_when(ASCENDENCIA == "AFRO" ~ "Afro", 
                                                                   ASCENDENCIA == "NO AFRO" ~ "No afro",
                                                                   ASCENDENCIA == "" ~ "Todos"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(URBANORURALUY = case_when(URBANORURALUY == "URBANO (MENOS DE 5.000 HABITANTES)" ~ "Urbano (menos de 5.000 habitantes)",
                                                                     URBANORURALUY == "RURAL DISPERSO" ~ "Rural disperso", 
                                                                     URBANORURALUY == "" ~ "Total país"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(SEXO = case_when(SEXO == "MUJERES" ~ "Mujeres", 
                                                            SEXO=="VARONES" ~ "Varones",
                                                            SEXO == "" ~ "Todos"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(QUINTIL = case_when(QUINTIL == "Quintil 1"~"Quintil 1", 
                                                               QUINTIL == "Quintil 2"~"Quintil 2",
                                                               QUINTIL == "Quintil 3"~"Quintil 3", 
                                                               QUINTIL == "Quintil 4"~"Quintil 4",
                                                               QUINTIL == "Quintil 5"~"Quintil 5",
                                                               QUINTIL == "" ~ "Todos"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(EDAD = case_when(EDAD == "" ~ "Todos"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(POBRE = case_when(POBRE == "POBRE" ~ "Pobre", 
                                                             POBRE == "NO POBRE" ~ "No pobre",
                                                             POBRE == "" ~ "Todos"))


m561_tp <- BASE_MOTOR %>% select(CODIND,	
                                 NOMINDICADOR,	
                                 CATEGORIA,	
                                 PESTAÑA,	
                                 SEXO,	
                                 ASCENDENCIA,	
                                 QUINTIL,	
                                 DEPARTAMENTOUY,	
                                 URBANORURALUY,	
                                 EDAD,	
                                 POBRE,
                                 NSE,
                                 NIVELEDU,
                                 PAÍS,	
                                 ANIO,	
                                 VALOR,	
                                 RESPONSABLE)


### 562 Porcentaje de niños entre 13 y 17 años sin cobertura de salud

## País Urbano

# Total

NOMINDICADOR <- 
  c("Porcentaje de niños entre 13 y 17 años sin cobertura de salud")

c_ano <- ech_svy %>%
  srvyr::filter(bd_region == 1 & bc_pe3> 12 & bc_pe3<18) %>%
  srvyr::summarise(colname = srvyr::survey_mean(nocobert))
c_ano <- as.numeric(c_ano$colname)

c_ano <- cbind(c_ano, NOMINDICADOR)
colnames(c_ano) <- c("VALOR", "NOMINDICADOR")
c_ano <- as.data.frame(c_ano)
BASE_MOTOR <- cbind(c_ano,BASE_AUX)

# Sexo 

b_sexo <- function(x) {
  x <- ech_svy %>%
    filter(bc_pe2 == x  & bd_region == 1  & bc_pe3> 12 & bc_pe3<18) %>%
    srvyr::summarise(colname = srvyr::survey_mean(nocobert))
  x <- mean(x$colname)
}       

b_e_sexo <- numeric()

for(i in 1:2){
  b_e_sexo[i] <- b_sexo(x = i)
}         

c_sexo <- as.data.frame(b_e_sexo)
c_sexo_1 <- cbind(as.data.frame(c_sexo[1,]), NOMINDICADOR, BASE_AUX)
c_sexo_1 <- c_sexo_1 %>% dplyr::mutate(SEXO = "VARONES")
c_sexo_2 <- cbind(as.data.frame(c_sexo[2,]), NOMINDICADOR, BASE_AUX)
c_sexo_2 <- c_sexo_2 %>% dplyr::mutate(SEXO = "MUJERES")
colnames(c_sexo_1) <- colnames(BASE_MOTOR)
colnames(c_sexo_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_sexo_1,c_sexo_2)

# Quintil de ingreso del hogar

b_quintil <- function(x) {
  x <- ech_svy %>%
    filter(quintilesy == x  & bd_region == 1  & bc_pe3> 12 & bc_pe3<18) %>%
    srvyr::summarise(colname = srvyr::survey_mean(nocobert))
  x <- mean(x$colname)
}       

b_e_quintil <- numeric()

for(i in 1:5){
  b_e_quintil[i] <- b_quintil(x = i)
}         

c_quintil <- as.data.frame(b_e_quintil)
c_quintil_1 <- cbind(as.data.frame(c_quintil[1,]), NOMINDICADOR, BASE_AUX)
c_quintil_1 <- c_quintil_1 %>% dplyr::mutate(QUINTIL = "Quintil 1")
c_quintil_2 <- cbind(as.data.frame(c_quintil[2,]), NOMINDICADOR, BASE_AUX)
c_quintil_2 <- c_quintil_2 %>% dplyr::mutate(QUINTIL = "Quintil 2")
c_quintil_3 <- cbind(as.data.frame(c_quintil[3,]), NOMINDICADOR, BASE_AUX)
c_quintil_3 <- c_quintil_3 %>% dplyr::mutate(QUINTIL = "Quintil 3")
c_quintil_4 <- cbind(as.data.frame(c_quintil[4,]), NOMINDICADOR, BASE_AUX)
c_quintil_4 <- c_quintil_4 %>% dplyr::mutate(QUINTIL = "Quintil 4")
c_quintil_5 <- cbind(as.data.frame(c_quintil[5,]), NOMINDICADOR, BASE_AUX)
c_quintil_5 <- c_quintil_5 %>% dplyr::mutate(QUINTIL = "Quintil 5")
colnames(c_quintil_1) <- colnames(BASE_MOTOR)
colnames(c_quintil_2) <- colnames(BASE_MOTOR)
colnames(c_quintil_3) <- colnames(BASE_MOTOR)
colnames(c_quintil_4) <- colnames(BASE_MOTOR)
colnames(c_quintil_5) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_quintil_1,c_quintil_2, c_quintil_3, c_quintil_4, c_quintil_5)


# Base motor

BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(CODIND = "562")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(CATEGORIA = "Salud infantil")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(PESTAÑA = "País urbano")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(PAÍS = "Uruguay")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(ANIO = 2023)
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(RESPONSABLE = "Sharon Katzkowicz")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(ASCENDENCIA = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(DEPARTAMENTOUY = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(URBANORURALUY = "Urbano (más de 5.000 habitantes)")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(POBRE = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(NSE = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(NIVELEDU = "Todos")
BASE_MOTOR <- as.data.frame(BASE_MOTOR)

BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(SEXO = case_when(SEXO == "" ~ "Todos", SEXO == "MUJERES"~"Mujeres", SEXO=="VARONES"~"Varones"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(QUINTIL = case_when(QUINTIL == "" ~ "Todos",
                                                               QUINTIL == "Quintil 1"~"Quintil 1", 
                                                               QUINTIL == "Quintil 2"~"Quintil 2",
                                                               QUINTIL == "Quintil 3"~"Quintil 3", 
                                                               QUINTIL == "Quintil 4"~"Quintil 4",
                                                               QUINTIL == "Quintil 5"~"Quintil 5"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(EDAD = case_when(EDAD == "" ~ "Todos"))


m562_pu <- BASE_MOTOR %>% select(CODIND,	
                                 NOMINDICADOR,	
                                 CATEGORIA,	
                                 PESTAÑA,	
                                 SEXO,	
                                 ASCENDENCIA,	
                                 QUINTIL,	
                                 DEPARTAMENTOUY,	
                                 URBANORURALUY,	
                                 EDAD,	
                                 POBRE,
                                 NSE,
                                 NIVELEDU,
                                 PAÍS,	
                                 ANIO,	
                                 VALOR,	
                                 RESPONSABLE)	


## Total país

# Total


NOMINDICADOR <- 
  c("Porcentaje de niños entre 13 y 17 años sin cobertura de salud")

c_ano <- ech_svy %>%
  filter(bc_pe3> 12 & bc_pe3<18) %>%
  srvyr::summarise(colname = srvyr::survey_mean(nocobert))
c_ano <- as.numeric(c_ano$colname)

c_ano <- cbind(c_ano, NOMINDICADOR)
colnames(c_ano) <- c("VALOR", "NOMINDICADOR")
c_ano <- as.data.frame(c_ano)
BASE_MOTOR <- cbind(c_ano,BASE_AUX)



# Región

b_reg <- function(x) {
  x <- ech_svy %>%
    filter(bd_region == x  & bc_pe3> 12 & bc_pe3<18) %>%
    srvyr::summarise(colname = srvyr::survey_mean(nocobert))
  x <- mean(x$colname)
}       

b_e_reg <- numeric()

for(i in 1:3){
  b_e_reg[i] <- b_reg(x = i)
}         

c_reg <- as.data.frame(b_e_reg)
c_reg_1 <- cbind(as.data.frame(c_reg[2,]), NOMINDICADOR, BASE_AUX)
c_reg_1 <- c_reg_1 %>% dplyr::mutate(URBANORURALUY = "URBANO (MENOS DE 5.000 HABITANTES)")
c_reg_2 <- cbind(as.data.frame(c_reg[3,]), NOMINDICADOR, BASE_AUX)
c_reg_2 <- c_reg_2 %>% dplyr::mutate(URBANORURALUY = "RURAL DISPERSO")
colnames(c_reg_1) <- colnames(BASE_MOTOR)
colnames(c_reg_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_reg_1,c_reg_2)


# Sexo 

b_sexo <- function(x) {
  x <- ech_svy %>%
    filter(bc_pe2 == x  & bc_pe3> 12 & bc_pe3<18) %>%
    srvyr::summarise(colname = srvyr::survey_mean(nocobert))
  x <- mean(x$colname)
}       

b_e_sexo <- numeric()

for(i in 1:2){
  b_e_sexo[i] <- b_sexo(x = i)
}         

c_sexo <- as.data.frame(b_e_sexo)
c_sexo_1 <- cbind(as.data.frame(c_sexo[1,]), NOMINDICADOR, BASE_AUX)
c_sexo_1 <- c_sexo_1 %>% dplyr::mutate(SEXO = "VARONES")
c_sexo_2 <- cbind(as.data.frame(c_sexo[2,]), NOMINDICADOR, BASE_AUX)
c_sexo_2 <- c_sexo_2 %>% dplyr::mutate(SEXO = "MUJERES")
colnames(c_sexo_1) <- colnames(BASE_MOTOR)
colnames(c_sexo_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_sexo_1,c_sexo_2)



# Ascendencia

b_ascen <- function(x) {
  x <- ech_svy %>%
    filter(bd_e29_1 == x  & bc_pe3> 12 & bc_pe3<18) %>%
    srvyr::summarise(colname = srvyr::survey_mean(nocobert))
  x <- mean(x$colname)
}       

b_e_ascen <- numeric()

for(i in 1:2){
  b_e_ascen[i] <- b_ascen(x = i)
}         

c_ascen <- as.data.frame(b_e_ascen)
c_ascen_1 <- cbind(as.data.frame(c_ascen[1,]), NOMINDICADOR, BASE_AUX)
c_ascen_1 <- c_ascen_1 %>% dplyr::mutate(ASCENDENCIA = "AFRO")
c_ascen_2 <- cbind(as.data.frame(c_ascen[2,]), NOMINDICADOR, BASE_AUX)
c_ascen_2 <- c_ascen_2 %>% dplyr::mutate(ASCENDENCIA = "NO AFRO")
colnames(c_ascen_1) <- colnames(BASE_MOTOR)
colnames(c_ascen_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_ascen_1,c_ascen_2)


# Quintil de ingreso del hogar

b_quintil <- function(x) {
  x <- ech_svy %>%
    filter(quintilesy == x  & bc_pe3> 12 & bc_pe3<18) %>%
    srvyr::summarise(colname = srvyr::survey_mean(nocobert))
  x <- mean(x$colname)
}       

b_e_quintil <- numeric()

for(i in 1:5){
  b_e_quintil[i] <- b_quintil(x = i)
}         

c_quintil <- as.data.frame(b_e_quintil)
c_quintil_1 <- cbind(as.data.frame(c_quintil[1,]), NOMINDICADOR, BASE_AUX)
c_quintil_1 <- c_quintil_1 %>% dplyr::mutate(QUINTIL = "Quintil 1")
c_quintil_2 <- cbind(as.data.frame(c_quintil[2,]), NOMINDICADOR, BASE_AUX)
c_quintil_2 <- c_quintil_2 %>% dplyr::mutate(QUINTIL = "Quintil 2")
c_quintil_3 <- cbind(as.data.frame(c_quintil[3,]), NOMINDICADOR, BASE_AUX)
c_quintil_3 <- c_quintil_3 %>% dplyr::mutate(QUINTIL = "Quintil 3")
c_quintil_4 <- cbind(as.data.frame(c_quintil[4,]), NOMINDICADOR, BASE_AUX)
c_quintil_4 <- c_quintil_4 %>% dplyr::mutate(QUINTIL = "Quintil 4")
c_quintil_5 <- cbind(as.data.frame(c_quintil[5,]), NOMINDICADOR, BASE_AUX)
c_quintil_5 <- c_quintil_5 %>% dplyr::mutate(QUINTIL = "Quintil 5")
colnames(c_quintil_1) <- colnames(BASE_MOTOR)
colnames(c_quintil_2) <- colnames(BASE_MOTOR)
colnames(c_quintil_3) <- colnames(BASE_MOTOR)
colnames(c_quintil_4) <- colnames(BASE_MOTOR)
colnames(c_quintil_5) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_quintil_1,c_quintil_2, c_quintil_3, c_quintil_4, c_quintil_5)


# Pobreza

b_pobre <- function(x) {
  x <- ech_svy %>%
    filter(pobre_aux == x & bc_pe3> 12 & bc_pe3<18) %>%
    srvyr::summarise(colname = srvyr::survey_mean(nocobert))
  x <- mean(x$colname)
}       

b_e_pobre <- numeric()

for(i in 1:2){
  b_e_pobre[i] <- b_pobre(x = i)
}         

c_pobre <- as.data.frame(b_e_pobre)
c_pobre_1 <- cbind(as.data.frame(c_pobre[1,]), NOMINDICADOR, BASE_AUX)
c_pobre_1 <- c_pobre_1 %>% dplyr::mutate(POBRE = "POBRE")
c_pobre_2 <- cbind(as.data.frame(c_pobre[2,]), NOMINDICADOR, BASE_AUX)
c_pobre_2 <- c_pobre_2 %>% dplyr::mutate(POBRE = "NO POBRE")
colnames(c_pobre_1) <- colnames(BASE_MOTOR)
colnames(c_pobre_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_pobre_1,c_pobre_2)


# Base motor

BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(CODIND = "562")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(CATEGORIA = "Salud infantil")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(PESTAÑA = "Total país")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(PAÍS = "Uruguay")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(ANIO = 2023)
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(RESPONSABLE = "Sharon Katzkowicz")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(DEPARTAMENTOUY = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(NSE = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(NIVELEDU = "Todos")
BASE_MOTOR <- as.data.frame(BASE_MOTOR)


BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(ASCENDENCIA = case_when(ASCENDENCIA == "AFRO" ~ "Afro", 
                                                                   ASCENDENCIA == "NO AFRO" ~ "No afro",
                                                                   ASCENDENCIA == "" ~ "Todos"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(URBANORURALUY = case_when(URBANORURALUY == "URBANO (MENOS DE 5.000 HABITANTES)" ~ "Urbano (menos de 5.000 habitantes)",
                                                                     URBANORURALUY == "RURAL DISPERSO" ~ "Rural disperso", 
                                                                     URBANORURALUY == "" ~ "Total país"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(SEXO = case_when(SEXO == "MUJERES" ~ "Mujeres", 
                                                            SEXO=="VARONES" ~ "Varones",
                                                            SEXO == "" ~ "Todos"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(QUINTIL = case_when(QUINTIL == "Quintil 1"~"Quintil 1", 
                                                               QUINTIL == "Quintil 2"~"Quintil 2",
                                                               QUINTIL == "Quintil 3"~"Quintil 3", 
                                                               QUINTIL == "Quintil 4"~"Quintil 4",
                                                               QUINTIL == "Quintil 5"~"Quintil 5",
                                                               QUINTIL == "" ~ "Todos"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(EDAD = case_when(EDAD == "" ~ "Todos"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(POBRE = case_when(POBRE == "POBRE" ~ "Pobre", 
                                                             POBRE == "NO POBRE" ~ "No pobre",
                                                             POBRE == "" ~ "Todos"))


m562_tp <- BASE_MOTOR %>% select(CODIND,	
                                 NOMINDICADOR,	
                                 CATEGORIA,	
                                 PESTAÑA,	
                                 SEXO,	
                                 ASCENDENCIA,	
                                 QUINTIL,	
                                 DEPARTAMENTOUY,	
                                 URBANORURALUY,	
                                 EDAD,	
                                 POBRE,
                                 NSE,
                                 NIVELEDU,
                                 PAÍS,	
                                 ANIO,	
                                 VALOR,	
                                 RESPONSABLE)


### 563 Porcentaje de niños menores de 18 años sin cobertura de salud

## País Urbano

# Total

NOMINDICADOR <- 
  c("Porcentaje de niños menores de 18 años sin cobertura de salud")

c_ano <- ech_svy %>%
  srvyr::filter(bd_region == 1 & bc_pe3<18) %>%
  srvyr::summarise(colname = srvyr::survey_mean(nocobert))
c_ano <- as.numeric(c_ano$colname)

c_ano <- cbind(c_ano, NOMINDICADOR)
colnames(c_ano) <- c("VALOR", "NOMINDICADOR")
c_ano <- as.data.frame(c_ano)
BASE_MOTOR <- cbind(c_ano,BASE_AUX)

# Sexo 

b_sexo <- function(x) {
  x <- ech_svy %>%
    filter(bc_pe2 == x  & bd_region == 1  & bc_pe3<18) %>%
    srvyr::summarise(colname = srvyr::survey_mean(nocobert))
  x <- mean(x$colname)
}       

b_e_sexo <- numeric()

for(i in 1:2){
  b_e_sexo[i] <- b_sexo(x = i)
}         

c_sexo <- as.data.frame(b_e_sexo)
c_sexo_1 <- cbind(as.data.frame(c_sexo[1,]), NOMINDICADOR, BASE_AUX)
c_sexo_1 <- c_sexo_1 %>% dplyr::mutate(SEXO = "VARONES")
c_sexo_2 <- cbind(as.data.frame(c_sexo[2,]), NOMINDICADOR, BASE_AUX)
c_sexo_2 <- c_sexo_2 %>% dplyr::mutate(SEXO = "MUJERES")
colnames(c_sexo_1) <- colnames(BASE_MOTOR)
colnames(c_sexo_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_sexo_1,c_sexo_2)

# Quintil de ingreso del hogar

b_quintil <- function(x) {
  x <- ech_svy %>%
    filter(quintilesy == x  & bd_region == 1  & bc_pe3<18) %>%
    srvyr::summarise(colname = srvyr::survey_mean(nocobert))
  x <- mean(x$colname)
}       

b_e_quintil <- numeric()

for(i in 1:5){
  b_e_quintil[i] <- b_quintil(x = i)
}         

c_quintil <- as.data.frame(b_e_quintil)
c_quintil_1 <- cbind(as.data.frame(c_quintil[1,]), NOMINDICADOR, BASE_AUX)
c_quintil_1 <- c_quintil_1 %>% dplyr::mutate(QUINTIL = "Quintil 1")
c_quintil_2 <- cbind(as.data.frame(c_quintil[2,]), NOMINDICADOR, BASE_AUX)
c_quintil_2 <- c_quintil_2 %>% dplyr::mutate(QUINTIL = "Quintil 2")
c_quintil_3 <- cbind(as.data.frame(c_quintil[3,]), NOMINDICADOR, BASE_AUX)
c_quintil_3 <- c_quintil_3 %>% dplyr::mutate(QUINTIL = "Quintil 3")
c_quintil_4 <- cbind(as.data.frame(c_quintil[4,]), NOMINDICADOR, BASE_AUX)
c_quintil_4 <- c_quintil_4 %>% dplyr::mutate(QUINTIL = "Quintil 4")
c_quintil_5 <- cbind(as.data.frame(c_quintil[5,]), NOMINDICADOR, BASE_AUX)
c_quintil_5 <- c_quintil_5 %>% dplyr::mutate(QUINTIL = "Quintil 5")
colnames(c_quintil_1) <- colnames(BASE_MOTOR)
colnames(c_quintil_2) <- colnames(BASE_MOTOR)
colnames(c_quintil_3) <- colnames(BASE_MOTOR)
colnames(c_quintil_4) <- colnames(BASE_MOTOR)
colnames(c_quintil_5) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_quintil_1,c_quintil_2, c_quintil_3, c_quintil_4, c_quintil_5)


# Base motor

BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(CODIND = "563")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(CATEGORIA = "Salud infantil")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(PESTAÑA = "País urbano")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(PAÍS = "Uruguay")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(ANIO = 2023)
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(RESPONSABLE = "Sharon Katzkowicz")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(ASCENDENCIA = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(DEPARTAMENTOUY = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(URBANORURALUY = "Urbano (más de 5.000 habitantes)")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(POBRE = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(NSE = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(NIVELEDU = "Todos")
BASE_MOTOR <- as.data.frame(BASE_MOTOR)

BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(SEXO = case_when(SEXO == "" ~ "Todos", SEXO == "MUJERES"~"Mujeres", SEXO=="VARONES"~"Varones"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(QUINTIL = case_when(QUINTIL == "" ~ "Todos",
                                                               QUINTIL == "Quintil 1"~"Quintil 1", 
                                                               QUINTIL == "Quintil 2"~"Quintil 2",
                                                               QUINTIL == "Quintil 3"~"Quintil 3", 
                                                               QUINTIL == "Quintil 4"~"Quintil 4",
                                                               QUINTIL == "Quintil 5"~"Quintil 5"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(EDAD = case_when(EDAD == "" ~ "Todos"))


m563_pu <- BASE_MOTOR %>% select(CODIND,	
                                 NOMINDICADOR,	
                                 CATEGORIA,	
                                 PESTAÑA,	
                                 SEXO,	
                                 ASCENDENCIA,	
                                 QUINTIL,	
                                 DEPARTAMENTOUY,	
                                 URBANORURALUY,	
                                 EDAD,	
                                 POBRE,
                                 NSE,
                                 NIVELEDU,
                                 PAÍS,	
                                 ANIO,	
                                 VALOR,	
                                 RESPONSABLE)	


## Total país

# Total


NOMINDICADOR <- 
  c("Porcentaje de niños menores de 18 años sin cobertura de salud")

c_ano <- ech_svy %>%
  filter(bc_pe3<18) %>%
  srvyr::summarise(colname = srvyr::survey_mean(nocobert))
c_ano <- as.numeric(c_ano$colname)

c_ano <- cbind(c_ano, NOMINDICADOR)
colnames(c_ano) <- c("VALOR", "NOMINDICADOR")
c_ano <- as.data.frame(c_ano)
BASE_MOTOR <- cbind(c_ano,BASE_AUX)



# Región

b_reg <- function(x) {
  x <- ech_svy %>%
    filter(bd_region == x  & bc_pe3<18) %>%
    srvyr::summarise(colname = srvyr::survey_mean(nocobert))
  x <- mean(x$colname)
}       

b_e_reg <- numeric()

for(i in 1:3){
  b_e_reg[i] <- b_reg(x = i)
}         

c_reg <- as.data.frame(b_e_reg)
c_reg_1 <- cbind(as.data.frame(c_reg[2,]), NOMINDICADOR, BASE_AUX)
c_reg_1 <- c_reg_1 %>% dplyr::mutate(URBANORURALUY = "URBANO (MENOS DE 5.000 HABITANTES)")
c_reg_2 <- cbind(as.data.frame(c_reg[3,]), NOMINDICADOR, BASE_AUX)
c_reg_2 <- c_reg_2 %>% dplyr::mutate(URBANORURALUY = "RURAL DISPERSO")
colnames(c_reg_1) <- colnames(BASE_MOTOR)
colnames(c_reg_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_reg_1,c_reg_2)


# Sexo 

b_sexo <- function(x) {
  x <- ech_svy %>%
    filter(bc_pe2 == x  & bc_pe3<18) %>%
    srvyr::summarise(colname = srvyr::survey_mean(nocobert))
  x <- mean(x$colname)
}       

b_e_sexo <- numeric()

for(i in 1:2){
  b_e_sexo[i] <- b_sexo(x = i)
}         

c_sexo <- as.data.frame(b_e_sexo)
c_sexo_1 <- cbind(as.data.frame(c_sexo[1,]), NOMINDICADOR, BASE_AUX)
c_sexo_1 <- c_sexo_1 %>% dplyr::mutate(SEXO = "VARONES")
c_sexo_2 <- cbind(as.data.frame(c_sexo[2,]), NOMINDICADOR, BASE_AUX)
c_sexo_2 <- c_sexo_2 %>% dplyr::mutate(SEXO = "MUJERES")
colnames(c_sexo_1) <- colnames(BASE_MOTOR)
colnames(c_sexo_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_sexo_1,c_sexo_2)



# Ascendencia

b_ascen <- function(x) {
  x <- ech_svy %>%
    filter(bd_e29_1 == x  & bc_pe3<18) %>%
    srvyr::summarise(colname = srvyr::survey_mean(nocobert))
  x <- mean(x$colname)
}       

b_e_ascen <- numeric()

for(i in 1:2){
  b_e_ascen[i] <- b_ascen(x = i)
}         

c_ascen <- as.data.frame(b_e_ascen)
c_ascen_1 <- cbind(as.data.frame(c_ascen[1,]), NOMINDICADOR, BASE_AUX)
c_ascen_1 <- c_ascen_1 %>% dplyr::mutate(ASCENDENCIA = "AFRO")
c_ascen_2 <- cbind(as.data.frame(c_ascen[2,]), NOMINDICADOR, BASE_AUX)
c_ascen_2 <- c_ascen_2 %>% dplyr::mutate(ASCENDENCIA = "NO AFRO")
colnames(c_ascen_1) <- colnames(BASE_MOTOR)
colnames(c_ascen_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_ascen_1,c_ascen_2)


# Quintil de ingreso del hogar

b_quintil <- function(x) {
  x <- ech_svy %>%
    filter(quintilesy == x  & bc_pe3<18) %>%
    srvyr::summarise(colname = srvyr::survey_mean(nocobert))
  x <- mean(x$colname)
}       

b_e_quintil <- numeric()

for(i in 1:5){
  b_e_quintil[i] <- b_quintil(x = i)
}         

c_quintil <- as.data.frame(b_e_quintil)
c_quintil_1 <- cbind(as.data.frame(c_quintil[1,]), NOMINDICADOR, BASE_AUX)
c_quintil_1 <- c_quintil_1 %>% dplyr::mutate(QUINTIL = "Quintil 1")
c_quintil_2 <- cbind(as.data.frame(c_quintil[2,]), NOMINDICADOR, BASE_AUX)
c_quintil_2 <- c_quintil_2 %>% dplyr::mutate(QUINTIL = "Quintil 2")
c_quintil_3 <- cbind(as.data.frame(c_quintil[3,]), NOMINDICADOR, BASE_AUX)
c_quintil_3 <- c_quintil_3 %>% dplyr::mutate(QUINTIL = "Quintil 3")
c_quintil_4 <- cbind(as.data.frame(c_quintil[4,]), NOMINDICADOR, BASE_AUX)
c_quintil_4 <- c_quintil_4 %>% dplyr::mutate(QUINTIL = "Quintil 4")
c_quintil_5 <- cbind(as.data.frame(c_quintil[5,]), NOMINDICADOR, BASE_AUX)
c_quintil_5 <- c_quintil_5 %>% dplyr::mutate(QUINTIL = "Quintil 5")
colnames(c_quintil_1) <- colnames(BASE_MOTOR)
colnames(c_quintil_2) <- colnames(BASE_MOTOR)
colnames(c_quintil_3) <- colnames(BASE_MOTOR)
colnames(c_quintil_4) <- colnames(BASE_MOTOR)
colnames(c_quintil_5) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_quintil_1,c_quintil_2, c_quintil_3, c_quintil_4, c_quintil_5)


# Pobreza

b_pobre <- function(x) {
  x <- ech_svy %>%
    filter(pobre_aux == x & bc_pe3<18) %>%
    srvyr::summarise(colname = srvyr::survey_mean(nocobert))
  x <- mean(x$colname)
}       

b_e_pobre <- numeric()

for(i in 1:2){
  b_e_pobre[i] <- b_pobre(x = i)
}         

c_pobre <- as.data.frame(b_e_pobre)
c_pobre_1 <- cbind(as.data.frame(c_pobre[1,]), NOMINDICADOR, BASE_AUX)
c_pobre_1 <- c_pobre_1 %>% dplyr::mutate(POBRE = "POBRE")
c_pobre_2 <- cbind(as.data.frame(c_pobre[2,]), NOMINDICADOR, BASE_AUX)
c_pobre_2 <- c_pobre_2 %>% dplyr::mutate(POBRE = "NO POBRE")
colnames(c_pobre_1) <- colnames(BASE_MOTOR)
colnames(c_pobre_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_pobre_1,c_pobre_2)


# Base motor

BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(CODIND = "563")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(CATEGORIA = "Salud infantil")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(PESTAÑA = "Total país")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(PAÍS = "Uruguay")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(ANIO = 2023)
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(RESPONSABLE = "Sharon Katzkowicz")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(DEPARTAMENTOUY = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(NSE = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(NIVELEDU = "Todos")
BASE_MOTOR <- as.data.frame(BASE_MOTOR)


BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(ASCENDENCIA = case_when(ASCENDENCIA == "AFRO" ~ "Afro", 
                                                                   ASCENDENCIA == "NO AFRO" ~ "No afro",
                                                                   ASCENDENCIA == "" ~ "Todos"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(URBANORURALUY = case_when(URBANORURALUY == "URBANO (MENOS DE 5.000 HABITANTES)" ~ "Urbano (menos de 5.000 habitantes)",
                                                                     URBANORURALUY == "RURAL DISPERSO" ~ "Rural disperso", 
                                                                     URBANORURALUY == "" ~ "Total país"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(SEXO = case_when(SEXO == "MUJERES" ~ "Mujeres", 
                                                            SEXO=="VARONES" ~ "Varones",
                                                            SEXO == "" ~ "Todos"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(QUINTIL = case_when(QUINTIL == "Quintil 1"~"Quintil 1", 
                                                               QUINTIL == "Quintil 2"~"Quintil 2",
                                                               QUINTIL == "Quintil 3"~"Quintil 3", 
                                                               QUINTIL == "Quintil 4"~"Quintil 4",
                                                               QUINTIL == "Quintil 5"~"Quintil 5",
                                                               QUINTIL == "" ~ "Todos"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(EDAD = case_when(EDAD == "" ~ "Todos"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(POBRE = case_when(POBRE == "POBRE" ~ "Pobre", 
                                                             POBRE == "NO POBRE" ~ "No pobre",
                                                             POBRE == "" ~ "Todos"))


m563_tp <- BASE_MOTOR %>% select(CODIND,	
                                 NOMINDICADOR,	
                                 CATEGORIA,	
                                 PESTAÑA,	
                                 SEXO,	
                                 ASCENDENCIA,	
                                 QUINTIL,	
                                 DEPARTAMENTOUY,	
                                 URBANORURALUY,	
                                 EDAD,	
                                 POBRE,
                                 NSE,
                                 NIVELEDU,
                                 PAÍS,	
                                 ANIO,	
                                 VALOR,	
                                 RESPONSABLE)



### 551	Tasa de asistencia a la educación inicial de niños/as de 0 a 2 años

## País Urbano

# Total

NOMINDICADOR <- 
  c("Tasa de asistencia a la educación inicial de niños/as de 0 a 2 años")

c_ano <- ech_svy %>%
  srvyr::filter(bd_region == 1 & bc_pe3<3) %>%
  srvyr::summarise(colname = srvyr::survey_mean(asiste))
c_ano <- as.numeric(c_ano$colname)

c_ano <- cbind(c_ano, NOMINDICADOR)
colnames(c_ano) <- c("VALOR", "NOMINDICADOR")
c_ano <- as.data.frame(c_ano)
BASE_MOTOR <- cbind(c_ano,BASE_AUX)

# Sexo 

b_sexo <- function(x) {
  x <- ech_svy %>%
    filter(bc_pe2 == x  & bd_region == 1  & bc_pe3<3) %>%
    srvyr::summarise(colname = srvyr::survey_mean(asiste))
  x <- mean(x$colname)
}       

b_e_sexo <- numeric()

for(i in 1:2){
  b_e_sexo[i] <- b_sexo(x = i)
}         

c_sexo <- as.data.frame(b_e_sexo)
c_sexo_1 <- cbind(as.data.frame(c_sexo[1,]), NOMINDICADOR, BASE_AUX)
c_sexo_1 <- c_sexo_1 %>% dplyr::mutate(SEXO = "VARONES")
c_sexo_2 <- cbind(as.data.frame(c_sexo[2,]), NOMINDICADOR, BASE_AUX)
c_sexo_2 <- c_sexo_2 %>% dplyr::mutate(SEXO = "MUJERES")
colnames(c_sexo_1) <- colnames(BASE_MOTOR)
colnames(c_sexo_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_sexo_1,c_sexo_2)

# Quintil de ingreso del hogar

b_quintil <- function(x) {
  x <- ech_svy %>%
    filter(quintilesy == x  & bd_region == 1  & bc_pe3<3) %>%
    srvyr::summarise(colname = srvyr::survey_mean(asiste))
  x <- mean(x$colname)
}       

b_e_quintil <- numeric()

for(i in 1:5){
  b_e_quintil[i] <- b_quintil(x = i)
}         

c_quintil <- as.data.frame(b_e_quintil)
c_quintil_1 <- cbind(as.data.frame(c_quintil[1,]), NOMINDICADOR, BASE_AUX)
c_quintil_1 <- c_quintil_1 %>% dplyr::mutate(QUINTIL = "Quintil 1")
c_quintil_2 <- cbind(as.data.frame(c_quintil[2,]), NOMINDICADOR, BASE_AUX)
c_quintil_2 <- c_quintil_2 %>% dplyr::mutate(QUINTIL = "Quintil 2")
c_quintil_3 <- cbind(as.data.frame(c_quintil[3,]), NOMINDICADOR, BASE_AUX)
c_quintil_3 <- c_quintil_3 %>% dplyr::mutate(QUINTIL = "Quintil 3")
c_quintil_4 <- cbind(as.data.frame(c_quintil[4,]), NOMINDICADOR, BASE_AUX)
c_quintil_4 <- c_quintil_4 %>% dplyr::mutate(QUINTIL = "Quintil 4")
c_quintil_5 <- cbind(as.data.frame(c_quintil[5,]), NOMINDICADOR, BASE_AUX)
c_quintil_5 <- c_quintil_5 %>% dplyr::mutate(QUINTIL = "Quintil 5")
colnames(c_quintil_1) <- colnames(BASE_MOTOR)
colnames(c_quintil_2) <- colnames(BASE_MOTOR)
colnames(c_quintil_3) <- colnames(BASE_MOTOR)
colnames(c_quintil_4) <- colnames(BASE_MOTOR)
colnames(c_quintil_5) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_quintil_1,c_quintil_2, c_quintil_3, c_quintil_4, c_quintil_5)


# Base motor

BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(CODIND = "551")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(CATEGORIA = "Cuidados y educación")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(PESTAÑA = "País urbano")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(PAÍS = "Uruguay")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(ANIO = 2023)
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(RESPONSABLE = "Sharon Katzkowicz")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(ASCENDENCIA = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(DEPARTAMENTOUY = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(URBANORURALUY = "Urbano (más de 5.000 habitantes)")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(POBRE = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(NSE = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(NIVELEDU = "Todos")
BASE_MOTOR <- as.data.frame(BASE_MOTOR)

BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(SEXO = case_when(SEXO == "" ~ "Todos", SEXO == "MUJERES"~"Mujeres", SEXO=="VARONES"~"Varones"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(QUINTIL = case_when(QUINTIL == "" ~ "Todos",
                                                               QUINTIL == "Quintil 1"~"Quintil 1", 
                                                               QUINTIL == "Quintil 2"~"Quintil 2",
                                                               QUINTIL == "Quintil 3"~"Quintil 3", 
                                                               QUINTIL == "Quintil 4"~"Quintil 4",
                                                               QUINTIL == "Quintil 5"~"Quintil 5"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(EDAD = case_when(EDAD == "" ~ "Todos"))


m551_pu <- BASE_MOTOR %>% select(CODIND,	
                                 NOMINDICADOR,	
                                 CATEGORIA,	
                                 PESTAÑA,	
                                 SEXO,	
                                 ASCENDENCIA,	
                                 QUINTIL,	
                                 DEPARTAMENTOUY,	
                                 URBANORURALUY,	
                                 EDAD,	
                                 POBRE,
                                 NSE,
                                 NIVELEDU,
                                 PAÍS,	
                                 ANIO,	
                                 VALOR,	
                                 RESPONSABLE)	


## Total país

# Total


NOMINDICADOR <- 
  c("Tasa de asistencia a la educación inicial de niños/as de 0 a 2 años")

c_ano <- ech_svy %>%
  filter(bc_pe3<3) %>%
  srvyr::summarise(colname = srvyr::survey_mean(asiste))
c_ano <- as.numeric(c_ano$colname)

c_ano <- cbind(c_ano, NOMINDICADOR)
colnames(c_ano) <- c("VALOR", "NOMINDICADOR")
c_ano <- as.data.frame(c_ano)
BASE_MOTOR <- cbind(c_ano,BASE_AUX)



# Región

b_reg <- function(x) {
  x <- ech_svy %>%
    filter(bd_region == x  & bc_pe3<3) %>%
    srvyr::summarise(colname = srvyr::survey_mean(asiste))
  x <- mean(x$colname)
}       

b_e_reg <- numeric()

for(i in 1:3){
  b_e_reg[i] <- b_reg(x = i)
}         

c_reg <- as.data.frame(b_e_reg)
c_reg_1 <- cbind(as.data.frame(c_reg[2,]), NOMINDICADOR, BASE_AUX)
c_reg_1 <- c_reg_1 %>% dplyr::mutate(URBANORURALUY = "URBANO (MENOS DE 5.000 HABITANTES)")
c_reg_2 <- cbind(as.data.frame(c_reg[3,]), NOMINDICADOR, BASE_AUX)
c_reg_2 <- c_reg_2 %>% dplyr::mutate(URBANORURALUY = "RURAL DISPERSO")
colnames(c_reg_1) <- colnames(BASE_MOTOR)
colnames(c_reg_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_reg_1,c_reg_2)


# Sexo 

b_sexo <- function(x) {
  x <- ech_svy %>%
    filter(bc_pe2 == x  & bc_pe3<3) %>%
    srvyr::summarise(colname = srvyr::survey_mean(asiste))
  x <- mean(x$colname)
}       

b_e_sexo <- numeric()

for(i in 1:2){
  b_e_sexo[i] <- b_sexo(x = i)
}         

c_sexo <- as.data.frame(b_e_sexo)
c_sexo_1 <- cbind(as.data.frame(c_sexo[1,]), NOMINDICADOR, BASE_AUX)
c_sexo_1 <- c_sexo_1 %>% dplyr::mutate(SEXO = "VARONES")
c_sexo_2 <- cbind(as.data.frame(c_sexo[2,]), NOMINDICADOR, BASE_AUX)
c_sexo_2 <- c_sexo_2 %>% dplyr::mutate(SEXO = "MUJERES")
colnames(c_sexo_1) <- colnames(BASE_MOTOR)
colnames(c_sexo_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_sexo_1,c_sexo_2)



# Ascendencia

b_ascen <- function(x) {
  x <- ech_svy %>%
    filter(bd_e29_1 == x  & bc_pe3<3) %>%
    srvyr::summarise(colname = srvyr::survey_mean(asiste))
  x <- mean(x$colname)
}       

b_e_ascen <- numeric()

for(i in 1:2){
  b_e_ascen[i] <- b_ascen(x = i)
}         

c_ascen <- as.data.frame(b_e_ascen)
c_ascen_1 <- cbind(as.data.frame(c_ascen[1,]), NOMINDICADOR, BASE_AUX)
c_ascen_1 <- c_ascen_1 %>% dplyr::mutate(ASCENDENCIA = "AFRO")
c_ascen_2 <- cbind(as.data.frame(c_ascen[2,]), NOMINDICADOR, BASE_AUX)
c_ascen_2 <- c_ascen_2 %>% dplyr::mutate(ASCENDENCIA = "NO AFRO")
colnames(c_ascen_1) <- colnames(BASE_MOTOR)
colnames(c_ascen_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_ascen_1,c_ascen_2)


# Quintil de ingreso del hogar

b_quintil <- function(x) {
  x <- ech_svy %>%
    filter(quintilesy == x  & bc_pe3<3) %>%
    srvyr::summarise(colname = srvyr::survey_mean(asiste))
  x <- mean(x$colname)
}       

b_e_quintil <- numeric()

for(i in 1:5){
  b_e_quintil[i] <- b_quintil(x = i)
}         

c_quintil <- as.data.frame(b_e_quintil)
c_quintil_1 <- cbind(as.data.frame(c_quintil[1,]), NOMINDICADOR, BASE_AUX)
c_quintil_1 <- c_quintil_1 %>% dplyr::mutate(QUINTIL = "Quintil 1")
c_quintil_2 <- cbind(as.data.frame(c_quintil[2,]), NOMINDICADOR, BASE_AUX)
c_quintil_2 <- c_quintil_2 %>% dplyr::mutate(QUINTIL = "Quintil 2")
c_quintil_3 <- cbind(as.data.frame(c_quintil[3,]), NOMINDICADOR, BASE_AUX)
c_quintil_3 <- c_quintil_3 %>% dplyr::mutate(QUINTIL = "Quintil 3")
c_quintil_4 <- cbind(as.data.frame(c_quintil[4,]), NOMINDICADOR, BASE_AUX)
c_quintil_4 <- c_quintil_4 %>% dplyr::mutate(QUINTIL = "Quintil 4")
c_quintil_5 <- cbind(as.data.frame(c_quintil[5,]), NOMINDICADOR, BASE_AUX)
c_quintil_5 <- c_quintil_5 %>% dplyr::mutate(QUINTIL = "Quintil 5")
colnames(c_quintil_1) <- colnames(BASE_MOTOR)
colnames(c_quintil_2) <- colnames(BASE_MOTOR)
colnames(c_quintil_3) <- colnames(BASE_MOTOR)
colnames(c_quintil_4) <- colnames(BASE_MOTOR)
colnames(c_quintil_5) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_quintil_1,c_quintil_2, c_quintil_3, c_quintil_4, c_quintil_5)


# Pobreza

b_pobre <- function(x) {
  x <- ech_svy %>%
    filter(pobre_aux == x & bc_pe3<3) %>%
    srvyr::summarise(colname = srvyr::survey_mean(asiste))
  x <- mean(x$colname)
}       

b_e_pobre <- numeric()

for(i in 1:2){
  b_e_pobre[i] <- b_pobre(x = i)
}         

c_pobre <- as.data.frame(b_e_pobre)
c_pobre_1 <- cbind(as.data.frame(c_pobre[1,]), NOMINDICADOR, BASE_AUX)
c_pobre_1 <- c_pobre_1 %>% dplyr::mutate(POBRE = "POBRE")
c_pobre_2 <- cbind(as.data.frame(c_pobre[2,]), NOMINDICADOR, BASE_AUX)
c_pobre_2 <- c_pobre_2 %>% dplyr::mutate(POBRE = "NO POBRE")
colnames(c_pobre_1) <- colnames(BASE_MOTOR)
colnames(c_pobre_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_pobre_1,c_pobre_2)


# Base motor

BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(CODIND = "551")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(CATEGORIA = "Cuidados y educación")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(PESTAÑA = "Total país")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(PAÍS = "Uruguay")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(ANIO = 2023)
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(RESPONSABLE = "Sharon Katzkowicz")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(DEPARTAMENTOUY = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(NSE = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(NIVELEDU = "Todos")
BASE_MOTOR <- as.data.frame(BASE_MOTOR)


BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(ASCENDENCIA = case_when(ASCENDENCIA == "AFRO" ~ "Afro", 
                                                                   ASCENDENCIA == "NO AFRO" ~ "No afro",
                                                                   ASCENDENCIA == "" ~ "Todos"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(URBANORURALUY = case_when(URBANORURALUY == "URBANO (MENOS DE 5.000 HABITANTES)" ~ "Urbano (menos de 5.000 habitantes)",
                                                                     URBANORURALUY == "RURAL DISPERSO" ~ "Rural disperso", 
                                                                     URBANORURALUY == "" ~ "Total país"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(SEXO = case_when(SEXO == "MUJERES" ~ "Mujeres", 
                                                            SEXO=="VARONES" ~ "Varones",
                                                            SEXO == "" ~ "Todos"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(QUINTIL = case_when(QUINTIL == "Quintil 1"~"Quintil 1", 
                                                               QUINTIL == "Quintil 2"~"Quintil 2",
                                                               QUINTIL == "Quintil 3"~"Quintil 3", 
                                                               QUINTIL == "Quintil 4"~"Quintil 4",
                                                               QUINTIL == "Quintil 5"~"Quintil 5",
                                                               QUINTIL == "" ~ "Todos"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(EDAD = case_when(EDAD == "" ~ "Todos"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(POBRE = case_when(POBRE == "POBRE" ~ "Pobre", 
                                                             POBRE == "NO POBRE" ~ "No pobre",
                                                             POBRE == "" ~ "Todos"))


m551_tp <- BASE_MOTOR %>% select(CODIND,	
                                 NOMINDICADOR,	
                                 CATEGORIA,	
                                 PESTAÑA,	
                                 SEXO,	
                                 ASCENDENCIA,	
                                 QUINTIL,	
                                 DEPARTAMENTOUY,	
                                 URBANORURALUY,	
                                 EDAD,	
                                 POBRE,
                                 NSE,
                                 NIVELEDU,
                                 PAÍS,	
                                 ANIO,	
                                 VALOR,	
                                 RESPONSABLE)







### 552	Tasa de asistencia a la educación de niños/as de 3 a 5 años

## País Urbano

# Total

NOMINDICADOR <- 
  c("Tasa de asistencia a la educación de niños/as de 3 a 5 años")

c_ano <- ech_svy %>%
  srvyr::filter(bd_region == 1 & bc_pe3>2&bc_pe3<6) %>%
  srvyr::summarise(colname = srvyr::survey_mean(asiste))
c_ano <- as.numeric(c_ano$colname)

c_ano <- cbind(c_ano, NOMINDICADOR)
colnames(c_ano) <- c("VALOR", "NOMINDICADOR")
c_ano <- as.data.frame(c_ano)
BASE_MOTOR <- cbind(c_ano,BASE_AUX)

# Sexo 

b_sexo <- function(x) {
  x <- ech_svy %>%
    filter(bc_pe2 == x  & bd_region == 1  & bc_pe3>2&bc_pe3<6) %>%
    srvyr::summarise(colname = srvyr::survey_mean(asiste))
  x <- mean(x$colname)
}       

b_e_sexo <- numeric()

for(i in 1:2){
  b_e_sexo[i] <- b_sexo(x = i)
}         

c_sexo <- as.data.frame(b_e_sexo)
c_sexo_1 <- cbind(as.data.frame(c_sexo[1,]), NOMINDICADOR, BASE_AUX)
c_sexo_1 <- c_sexo_1 %>% dplyr::mutate(SEXO = "VARONES")
c_sexo_2 <- cbind(as.data.frame(c_sexo[2,]), NOMINDICADOR, BASE_AUX)
c_sexo_2 <- c_sexo_2 %>% dplyr::mutate(SEXO = "MUJERES")
colnames(c_sexo_1) <- colnames(BASE_MOTOR)
colnames(c_sexo_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_sexo_1,c_sexo_2)

# Quintil de ingreso del hogar

b_quintil <- function(x) {
  x <- ech_svy %>%
    filter(quintilesy == x  & bd_region == 1  & bc_pe3>2&bc_pe3<6) %>%
    srvyr::summarise(colname = srvyr::survey_mean(asiste))
  x <- mean(x$colname)
}       

b_e_quintil <- numeric()

for(i in 1:5){
  b_e_quintil[i] <- b_quintil(x = i)
}         

c_quintil <- as.data.frame(b_e_quintil)
c_quintil_1 <- cbind(as.data.frame(c_quintil[1,]), NOMINDICADOR, BASE_AUX)
c_quintil_1 <- c_quintil_1 %>% dplyr::mutate(QUINTIL = "Quintil 1")
c_quintil_2 <- cbind(as.data.frame(c_quintil[2,]), NOMINDICADOR, BASE_AUX)
c_quintil_2 <- c_quintil_2 %>% dplyr::mutate(QUINTIL = "Quintil 2")
c_quintil_3 <- cbind(as.data.frame(c_quintil[3,]), NOMINDICADOR, BASE_AUX)
c_quintil_3 <- c_quintil_3 %>% dplyr::mutate(QUINTIL = "Quintil 3")
c_quintil_4 <- cbind(as.data.frame(c_quintil[4,]), NOMINDICADOR, BASE_AUX)
c_quintil_4 <- c_quintil_4 %>% dplyr::mutate(QUINTIL = "Quintil 4")
c_quintil_5 <- cbind(as.data.frame(c_quintil[5,]), NOMINDICADOR, BASE_AUX)
c_quintil_5 <- c_quintil_5 %>% dplyr::mutate(QUINTIL = "Quintil 5")
colnames(c_quintil_1) <- colnames(BASE_MOTOR)
colnames(c_quintil_2) <- colnames(BASE_MOTOR)
colnames(c_quintil_3) <- colnames(BASE_MOTOR)
colnames(c_quintil_4) <- colnames(BASE_MOTOR)
colnames(c_quintil_5) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_quintil_1,c_quintil_2, c_quintil_3, c_quintil_4, c_quintil_5)


# Base motor

BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(CODIND = "552")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(CATEGORIA = "Cuidados y educación")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(PESTAÑA = "País urbano")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(PAÍS = "Uruguay")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(ANIO = 2023)
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(RESPONSABLE = "Sharon Katzkowicz")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(ASCENDENCIA = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(DEPARTAMENTOUY = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(URBANORURALUY = "Urbano (más de 5.000 habitantes)")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(POBRE = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(NSE = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(NIVELEDU = "Todos")
BASE_MOTOR <- as.data.frame(BASE_MOTOR)

BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(SEXO = case_when(SEXO == "" ~ "Todos", SEXO == "MUJERES"~"Mujeres", SEXO=="VARONES"~"Varones"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(QUINTIL = case_when(QUINTIL == "" ~ "Todos",
                                                               QUINTIL == "Quintil 1"~"Quintil 1", 
                                                               QUINTIL == "Quintil 2"~"Quintil 2",
                                                               QUINTIL == "Quintil 3"~"Quintil 3", 
                                                               QUINTIL == "Quintil 4"~"Quintil 4",
                                                               QUINTIL == "Quintil 5"~"Quintil 5"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(EDAD = case_when(EDAD == "" ~ "Todos"))


m552_pu <- BASE_MOTOR %>% select(CODIND,	
                                 NOMINDICADOR,	
                                 CATEGORIA,	
                                 PESTAÑA,	
                                 SEXO,	
                                 ASCENDENCIA,	
                                 QUINTIL,	
                                 DEPARTAMENTOUY,	
                                 URBANORURALUY,	
                                 EDAD,	
                                 POBRE,
                                 NSE,
                                 NIVELEDU,
                                 PAÍS,	
                                 ANIO,	
                                 VALOR,	
                                 RESPONSABLE)	


## Total país

# Total


NOMINDICADOR <- 
  c("Tasa de asistencia a la educación de niños/as de 3 a 5 años")

c_ano <- ech_svy %>%
  filter(bc_pe3>2&bc_pe3<6) %>%
  srvyr::summarise(colname = srvyr::survey_mean(asiste))
c_ano <- as.numeric(c_ano$colname)

c_ano <- cbind(c_ano, NOMINDICADOR)
colnames(c_ano) <- c("VALOR", "NOMINDICADOR")
c_ano <- as.data.frame(c_ano)
BASE_MOTOR <- cbind(c_ano,BASE_AUX)



# Región

b_reg <- function(x) {
  x <- ech_svy %>%
    filter(bd_region == x  & bc_pe3>2&bc_pe3<6) %>%
    srvyr::summarise(colname = srvyr::survey_mean(asiste))
  x <- mean(x$colname)
}       

b_e_reg <- numeric()

for(i in 1:3){
  b_e_reg[i] <- b_reg(x = i)
}         

c_reg <- as.data.frame(b_e_reg)
c_reg_1 <- cbind(as.data.frame(c_reg[2,]), NOMINDICADOR, BASE_AUX)
c_reg_1 <- c_reg_1 %>% dplyr::mutate(URBANORURALUY = "URBANO (MENOS DE 5.000 HABITANTES)")
c_reg_2 <- cbind(as.data.frame(c_reg[3,]), NOMINDICADOR, BASE_AUX)
c_reg_2 <- c_reg_2 %>% dplyr::mutate(URBANORURALUY = "RURAL DISPERSO")
colnames(c_reg_1) <- colnames(BASE_MOTOR)
colnames(c_reg_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_reg_1,c_reg_2)


# Sexo 

b_sexo <- function(x) {
  x <- ech_svy %>%
    filter(bc_pe2 == x  & bc_pe3>2&bc_pe3<6) %>%
    srvyr::summarise(colname = srvyr::survey_mean(asiste))
  x <- mean(x$colname)
}       

b_e_sexo <- numeric()

for(i in 1:2){
  b_e_sexo[i] <- b_sexo(x = i)
}         

c_sexo <- as.data.frame(b_e_sexo)
c_sexo_1 <- cbind(as.data.frame(c_sexo[1,]), NOMINDICADOR, BASE_AUX)
c_sexo_1 <- c_sexo_1 %>% dplyr::mutate(SEXO = "VARONES")
c_sexo_2 <- cbind(as.data.frame(c_sexo[2,]), NOMINDICADOR, BASE_AUX)
c_sexo_2 <- c_sexo_2 %>% dplyr::mutate(SEXO = "MUJERES")
colnames(c_sexo_1) <- colnames(BASE_MOTOR)
colnames(c_sexo_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_sexo_1,c_sexo_2)



# Ascendencia

b_ascen <- function(x) {
  x <- ech_svy %>%
    filter(bd_e29_1 == x  & bc_pe3>2&bc_pe3<6) %>%
    srvyr::summarise(colname = srvyr::survey_mean(asiste))
  x <- mean(x$colname)
}       

b_e_ascen <- numeric()

for(i in 1:2){
  b_e_ascen[i] <- b_ascen(x = i)
}         

c_ascen <- as.data.frame(b_e_ascen)
c_ascen_1 <- cbind(as.data.frame(c_ascen[1,]), NOMINDICADOR, BASE_AUX)
c_ascen_1 <- c_ascen_1 %>% dplyr::mutate(ASCENDENCIA = "AFRO")
c_ascen_2 <- cbind(as.data.frame(c_ascen[2,]), NOMINDICADOR, BASE_AUX)
c_ascen_2 <- c_ascen_2 %>% dplyr::mutate(ASCENDENCIA = "NO AFRO")
colnames(c_ascen_1) <- colnames(BASE_MOTOR)
colnames(c_ascen_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_ascen_1,c_ascen_2)


# Quintil de ingreso del hogar

b_quintil <- function(x) {
  x <- ech_svy %>%
    filter(quintilesy == x  & bc_pe3>2&bc_pe3<6) %>%
    srvyr::summarise(colname = srvyr::survey_mean(asiste))
  x <- mean(x$colname)
}       

b_e_quintil <- numeric()

for(i in 1:5){
  b_e_quintil[i] <- b_quintil(x = i)
}         

c_quintil <- as.data.frame(b_e_quintil)
c_quintil_1 <- cbind(as.data.frame(c_quintil[1,]), NOMINDICADOR, BASE_AUX)
c_quintil_1 <- c_quintil_1 %>% dplyr::mutate(QUINTIL = "Quintil 1")
c_quintil_2 <- cbind(as.data.frame(c_quintil[2,]), NOMINDICADOR, BASE_AUX)
c_quintil_2 <- c_quintil_2 %>% dplyr::mutate(QUINTIL = "Quintil 2")
c_quintil_3 <- cbind(as.data.frame(c_quintil[3,]), NOMINDICADOR, BASE_AUX)
c_quintil_3 <- c_quintil_3 %>% dplyr::mutate(QUINTIL = "Quintil 3")
c_quintil_4 <- cbind(as.data.frame(c_quintil[4,]), NOMINDICADOR, BASE_AUX)
c_quintil_4 <- c_quintil_4 %>% dplyr::mutate(QUINTIL = "Quintil 4")
c_quintil_5 <- cbind(as.data.frame(c_quintil[5,]), NOMINDICADOR, BASE_AUX)
c_quintil_5 <- c_quintil_5 %>% dplyr::mutate(QUINTIL = "Quintil 5")
colnames(c_quintil_1) <- colnames(BASE_MOTOR)
colnames(c_quintil_2) <- colnames(BASE_MOTOR)
colnames(c_quintil_3) <- colnames(BASE_MOTOR)
colnames(c_quintil_4) <- colnames(BASE_MOTOR)
colnames(c_quintil_5) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_quintil_1,c_quintil_2, c_quintil_3, c_quintil_4, c_quintil_5)


# Pobreza

b_pobre <- function(x) {
  x <- ech_svy %>%
    filter(pobre_aux == x & bc_pe3>2&bc_pe3<6) %>%
    srvyr::summarise(colname = srvyr::survey_mean(asiste))
  x <- mean(x$colname)
}       

b_e_pobre <- numeric()

for(i in 1:2){
  b_e_pobre[i] <- b_pobre(x = i)
}         

c_pobre <- as.data.frame(b_e_pobre)
c_pobre_1 <- cbind(as.data.frame(c_pobre[1,]), NOMINDICADOR, BASE_AUX)
c_pobre_1 <- c_pobre_1 %>% dplyr::mutate(POBRE = "POBRE")
c_pobre_2 <- cbind(as.data.frame(c_pobre[2,]), NOMINDICADOR, BASE_AUX)
c_pobre_2 <- c_pobre_2 %>% dplyr::mutate(POBRE = "NO POBRE")
colnames(c_pobre_1) <- colnames(BASE_MOTOR)
colnames(c_pobre_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_pobre_1,c_pobre_2)


# Base motor

BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(CODIND = "552")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(CATEGORIA = "Cuidados y educación")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(PESTAÑA = "Total país")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(PAÍS = "Uruguay")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(ANIO = 2023)
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(RESPONSABLE = "Sharon Katzkowicz")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(DEPARTAMENTOUY = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(NSE = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(NIVELEDU = "Todos")
BASE_MOTOR <- as.data.frame(BASE_MOTOR)


BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(ASCENDENCIA = case_when(ASCENDENCIA == "AFRO" ~ "Afro", 
                                                                   ASCENDENCIA == "NO AFRO" ~ "No afro",
                                                                   ASCENDENCIA == "" ~ "Todos"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(URBANORURALUY = case_when(URBANORURALUY == "URBANO (MENOS DE 5.000 HABITANTES)" ~ "Urbano (menos de 5.000 habitantes)",
                                                                     URBANORURALUY == "RURAL DISPERSO" ~ "Rural disperso", 
                                                                     URBANORURALUY == "" ~ "Total país"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(SEXO = case_when(SEXO == "MUJERES" ~ "Mujeres", 
                                                            SEXO=="VARONES" ~ "Varones",
                                                            SEXO == "" ~ "Todos"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(QUINTIL = case_when(QUINTIL == "Quintil 1"~"Quintil 1", 
                                                               QUINTIL == "Quintil 2"~"Quintil 2",
                                                               QUINTIL == "Quintil 3"~"Quintil 3", 
                                                               QUINTIL == "Quintil 4"~"Quintil 4",
                                                               QUINTIL == "Quintil 5"~"Quintil 5",
                                                               QUINTIL == "" ~ "Todos"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(EDAD = case_when(EDAD == "" ~ "Todos"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(POBRE = case_when(POBRE == "POBRE" ~ "Pobre", 
                                                             POBRE == "NO POBRE" ~ "No pobre",
                                                             POBRE == "" ~ "Todos"))


m552_tp <- BASE_MOTOR %>% select(CODIND,	
                                 NOMINDICADOR,	
                                 CATEGORIA,	
                                 PESTAÑA,	
                                 SEXO,	
                                 ASCENDENCIA,	
                                 QUINTIL,	
                                 DEPARTAMENTOUY,	
                                 URBANORURALUY,	
                                 EDAD,	
                                 POBRE,
                                 NSE,
                                 NIVELEDU,
                                 PAÍS,	
                                 ANIO,	
                                 VALOR,	
                                 RESPONSABLE)








BASE = rbind(m500_pu, m500_tp, m501_pu, m501_tp, m502_pu, 
             m502_tp, m503_pu, m503_tp, m504_pu, m504_tp, m505_pu, m505_tp, m508_pu, 
             m508_tp, m509_pu, m509_tp, m510_pu, m510_tp, m511_pu, m511_tp, m512_pu, 
             m512_tp, m513_pu, m513_tp, m514_pu, m514_tp, m515_pu, m515_tp, m516_pu, 
             m516_tp, m541_pu, m541_tp, m542_pu, m542_tp, m543_pu, m543_tp, m544_pu, 
             m544_tp, m551_pu, m551_tp, m552_pu, m552_tp, m560_pu, m560_tp, m561_pu, 
             m561_tp, m562_pu, m562_tp, m563_pu, m563_tp)
EDAD_HIJ <-"Todos"
BASE <- cbind(BASE, EDAD_HIJ)
colnames(BASE) <- c("CODIND",	"NOMINDICADOR",	"CATEGORIA",	"PESTAÑA",	"SEXO",	"ASCENDENCIA",	"QUINTIL",	"DEPARTAMENTOUY",	"URBANORURALUY",	"EDAD",	"POBRE",	"NSE",	"NIVELEDU",	"PAÍS",	"FECHA",	"VALOR",	"RESPONSABLE",	"EDAD_HIJ")


rio::export(BASE, "Data/PRUEBAS/ECH_2023.xlsx")


# #Base_Motor_Familia  <- rio::import("Versión pervia web/Base motor_familia_2023.xlsx")
# Base_Motor_Familia  <- rio::import("Data/Pruebas/Base_Motor_familia_23.xlsx"
# #Base_Motor_Familia_0524  <- rbind(Base_Motor_Familia, BASE)


#rio::export(Base_Motor_Familia_0524, "Export actualiza 2024/Base_Motor_familia_20052024.xlsx" )






