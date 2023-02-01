tostring bc_correlat, replace
cap drop jefe
gen jefe=0
replace jefe=1 if bc_pe4==1

cap drop esposo_companero
gen esposo_companero=0
replace esposo_compan=1 if bc_pe4==2

cap drop hijos
gen hijos=0
replace hijos=1 if bc_pe4==3

cap drop otro_pariente
gen otro_pariente=0
replace otro_pariente=1 if bc_pe4==4|bc_pe4==5

cap drop no_pariente
gen no_pariente=0
replace no_pariente=1 if bc_pe4==6

cap drop servicio_domestico
gen servicio_domestico=0
replace servicio_domestico=1 if bc_pe4==7

cap drop jefatura_femenina
gen jefatura_femenina=0
replace jefatura_femenina=1 if bc_pe4==1 & bc_pe2==2

cap drop correlat
g correlat=bc_correlat

cap drop sumjefe
cap drop sumesposo_companero
cap drop sumhijo
cap drop sumotro_pariente
cap drop sumno_pariente
cap drop sumservicio_domestico
cap drop sumjefatura_femenina

egen sumjefe= sum(jefe), by (correlat)
egen sumesposo_companero= sum(esposo_companero), by (correlat)
egen sumhijo= sum(hijos), by (correlat)
egen sumotro_pariente= sum(otro_pariente), by (correlat)
egen sumno_pariente= sum(no_pariente), by (correlat)
egen sumservicio_domestico= sum(servicio_domestico), by (correlat)
egen sumjefatura_femenina= sum(jefatura_femenina), by (correlat)


*** CLASIFICACIÓN HOGARES POR TIPO DE PAREJA 2011 ***

* a) Primero definimos tipo de hogar para poder clasificar a los hogares biparentales*

*** TIPO DE HOGAR ***
*b) generamos la variable que identifica hogares en donde hay una pareja 

g biparentales=0
replace biparentales=1 if (sumesposo_companero>0 & sumesposo_companero!=.)

*Nos quedamos solo con los hogares biparentales*


*c) generamos variables a nivel agregado

*Primero generamos variable a nivel personas*

*JEFE Ocupado tiempo completo_varón*
g jefeocup_tc_v=(bc_pobp==2 & (bc_horas>39 | bc_horas_1>39) & jefe==1 & bc_pe2==1&biparentales==1)

*JEFE Ocupado tiempo completo_mujer*
g jefeocup_tc_m=(bc_pobp==2 & (bc_horas>39 | bc_horas_1>39) & jefe==1 & bc_pe2==2&biparentales==1)

*CONYUGE Ocupado tiempo completo_varón*
g conyocup_tc_v=(bc_pobp==2 & (bc_horas>39 | bc_horas_1>39) & esposo_companero==1 & bc_pe2==1&biparentales==1)

g conyocup_tc_m=(bc_pobp==2 & (bc_horas>39 | bc_horas_1>39) & esposo_companero==1 & bc_pe2==2&biparentales==1)

*JEFE Ocupado medio tiempo_varón*
g jefeocup_mt_v=(bc_pobp==2 & bc_horas<40 & jefe==1 & bc_pe2==1&biparentales==1)

*JEFE Ocupado medio tiempo_mujer*
g jefeocup_mt_m=(bc_pobp==2 & bc_horas<40& jefe==1 & bc_pe2==2&biparentales==1)

*CONYUGE Ocupado medio tiempo_varón*
g conyocup_mt_v=(bc_pobp==2 & bc_horas<40& esposo_companero==1 & bc_pe2==1&biparentales==1)

*CONYUGE Ocupado medio tiempo_mujer*
g conyocup_mt_m=(bc_pobp==2 & bc_horas<40& esposo_companero==1 & bc_pe2==2&biparentales==1)
 
*JEFE Inactivo o desempleado_varón*
g jefeinact_desoc_v=(bc_pobp>2 & bc_pobp<12 & jefe==1 & bc_pe2==1&biparentales==1)

*JEFE Inactivo o desempleado_mujer*
g jefeinact_desoc_m=(bc_pobp>2 & bc_pobp<12 & jefe==1 & bc_pe2==2&biparentales==1)

*CONYUGE Inactivo o desempleado_varón*
g conyinact_desoc_v=(bc_pobp>2 & bc_pobp<12 & esposo_companero==1 & bc_pe2==1&biparentales==1)

*CONYUGE Inactivo o desempleado_mujer*
g conyinact_desoc_m=(bc_pobp>2 & bc_pobp<12 & esposo_companero==1 & bc_pe2==2&biparentales==1)

*** Segundo, variables agregadas a nivel hogar***

egen sumjefeocup_tc_m= sum(jefeocup_tc_m), by (correlat)
egen sumjefeocup_tc_v= sum(jefeocup_tc_v), by (correlat)

egen sumconyocup_tc_m= sum(conyocup_tc_m), by (correlat)
egen sumconyocup_tc_v= sum(conyocup_tc_v), by (correlat)

egen sumjefeocup_mt_m= sum(jefeocup_mt_m), by (correlat)
egen sumjefeocup_mt_v= sum(jefeocup_mt_v), by (correlat)

egen sumconyocup_mt_v= sum(conyocup_mt_v), by (correlat)
egen sumconyocup_mt_m= sum(conyocup_mt_m), by (correlat)

egen sumjefeinact_desoc_m= sum(jefeinact_desoc_m), by (correlat)
egen sumjefeinact_desoc_v= sum(jefeinact_desoc_v), by (correlat)

egen sumconyinact_desoc_m= sum(conyinact_desoc_m), by (correlat)
egen sumconyinact_desoc_v= sum(conyinact_desoc_v), by (correlat)

* d) generamos la variable de modelos de pareja con las categorías de arriba

g modelo_pareja=. 

*1. PROVEEDOR TRADICIONAL (solo hombre trabaja, mujer inactiva o desempleada)*

replace modelo_pareja=1 if ((sumjefeocup_tc_v==1 & sumconyinact_desoc_m==1) | (sumconyocup_tc_v==1 & sumjefeinact_desoc_m==1)/*
*/ | (sumjefeocup_mt_v==1 & sumconyinact_desoc_m==1) | (sumconyocup_mt_v==1 & sumjefeinact_desoc_m==1))&biparentales==1

*2 PROVEEDOR MODIFICADO: pareja donde ambos trabajan para el mercado pero el hombre trabaja a tiempo completo y la mujer a tiempo parcial. 

replace modelo_pareja=2 if ((sumjefeocup_tc_v==1 & sumconyocup_mt_m==1) | (sumconyocup_tc_v==1 & sumjefeocup_mt_m==1))&biparentales==1

*3. DOBLE CARRERA:  pareja donde ambos trabajan para el mercado, ambos a tiempo completo o ambos a tiempo parcial. 

replace modelo_pareja=3 if ((sumjefeocup_tc_v==1 & sumconyocup_tc_m==1) | (sumjefeocup_tc_m==1 & sumconyocup_tc_v==1)/*
*/ | (sumjefeocup_mt_m==1 & sumconyocup_mt_v==1) | (sumjefeocup_mt_v==1 & sumconyocup_mt_m==1))&biparentales==1

*4. INVERSIÓN DE ROLES: pareja donde sÓlo la mujer trabaja para el mercado y el hombre es inactivo o desocupado. 

replace modelo_pareja=4 if ((sumjefeocup_tc_m==1 & sumconyinact_desoc_v==1) | (sumconyocup_tc_m==1 & sumjefeinact_desoc_v==1)/*
*/ | (sumjefeocup_mt_m==1 & sumconyinact_desoc_v==1) | (sumconyocup_mt_m==1 & sumjefeinact_desoc_v==1))&biparentales==1

* 5. INVERSIÓN DE ROLES MODIFICADO: ambos trabajan en el mercado laboral pero hombre ocupado a tiempo parcial y mujer ocupada a tiempo completo.

replace modelo_pareja=5 if ((sumjefeocup_tc_m==1 & sumconyocup_mt_v==1) | (sumconyocup_tc_m==1 & sumjefeocup_mt_v==1))&biparentales==1

* 6. MODELO RESIDUAL: Ambos no trabajan (desocupados o inactivos)

replace modelo_pareja=6 if ((sumjefeinact_desoc_m==1 & sumconyinact_desoc_v==1) | (sumjefeinact_desoc_v==1 & sumconyinact_desoc_m==1))&biparentales==1

lab def modelo_pareja 1 "Modelo proveedor tradicional" 2" Modelo proveedor modificado" 3"Modelo doble carrera" 4"Modelo inversión de roles" /*
*/ 5 "Modelo de inversión de roles modificado" 6 "Modelo residual"

lab val modelo_pareja modelo_pareja
