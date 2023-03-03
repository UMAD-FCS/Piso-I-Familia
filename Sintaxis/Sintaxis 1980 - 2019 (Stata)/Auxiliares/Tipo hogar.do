*_Construimos tipo de hogar_*

*** TIPO DE HOGAR ***

/*Relación de parentesco e30

1	Jefe	
2	Esposo o compañero	
3	Hijo 
12	Otro pariente	
13	Otro no pariente	
14	Servicio dom괴ico o familiar del mismo*/

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

*Constuyendo tipo de hogar*

cap drop tipo_hogar


*1.UNIPERSONAL 

gen tipo_hogar=.

replace tipo_hogar=1 if sumesposo_companero==0 & sumhijo==0 & sumno_pariente==0 & sumotro_pariente==0 


*2. PAREJA SIN HIJOS

replace tipo_hogar=2 if sumesposo_companero>=1 & sumhijo==0 & sumotro_pariente==0 & sumno_pariente==0 


*3. BIPARENTAL 

replace tipo_hogar=3 if (sumesposo_companero>=1 & sumhijo>=1)& sumotro_pariente==0 & sumno_pariente==0  


*4. MONOPARENTAL FEMENINO 

replace tipo_hogar=4 if sumesposo_companero==0 & sumhijo>=1 & sumotro_pariente==0 & sumjefatura_femenina==1 /*
*/ & sumno_pariente==0 


*5. MONOPARENTAL MASCULINO 

replace tipo_hogar=5 if sumesposo_companero==0 & sumhijo>=1 & sumotro_pariente==0 & sumjefatura_femenina==0 /*
*/ & sumno_pariente==0 


*6. EXTENDIDO O COMPUESTO 

replace tipo_hogar=6 if sumotro_pariente>=1 & sumno_pariente==0
replace tipo_hogar=6 if sumno_pariente>=1


*7. EXTENDIDO O COMPUESTO CON NÚCLEO MONOPARENTAL

replace tipo_hogar=7 if (sumesposo_companero==0 &sumhijo>0&sumjefatura_femenina==1&sumotro_pariente>=1) 
replace tipo_hogar=7 if (sumesposo_companero==0 &sumhijo>0&sumjefatura_femenina==1&sumno_pariente>=1) 


*8. SIN NÚCLEO CONYUGAL

replace tipo_hogar=8 if  (sumotro_pariente >=1 | sumno_pariente >=1) & /*
*/(sumesposo_companero==0 & sumhijo==0)

