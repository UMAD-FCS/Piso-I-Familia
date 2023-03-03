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



cap drop mujmen40
gen mujmen40=0
replace mujmen40=1 if (bc_pe4==1 | bc_pe4==2) & bc_pe2==2 & bc_pe3<40

cap drop mujmay40
g mujmay40=0
replace mujmay40=1 if (bc_pe4==1 | bc_pe4==2) & bc_pe2==2 & bc_pe3>=40


*Edad de los hijos
cap drop hijo_men5
gen hijo_men5=0
replace hijo_men5=1 if (hijos==1)& bc_pe3<6

cap drop hijo_6a12
gen hijo_6a12=0
replace hijo_6a12=1 if (hijos==1) & (bc_pe3>5 & bc_pe3<13)

cap drop hijo_may12
gen hijo_may12=0
replace hijo_may12=1 if (hijos==1)& bc_pe3>12 

cap drop hijo_menos12
gen hijo_menos12=0
replace hijo_menos12=1 if (hijos==1) & (hijo_6a12==1 | hijo_men5==1)

cap drop hijo_13y18
gen hijo_13y18=0
replace hijo_13y18=1 if (hijos==1) & (bc_pe3>12 & bc_pe3<19)

cap drop hijo_may18
gen hijo_may18=0
replace hijo_may18=1 if (hijos==1)& bc_pe3>18 

*Genero variables agregadas de edad*
cap drop sumhijo_men5
cap drop sumhijo_6a12
cap drop sumhijo_may12
cap drop sumhijo_menos12
cap drop sumhijo_13y18
cap drop sumhijo_may18
egen sumhijo_men5=sum(hijo_men5), by (correlat) 
egen sumhijo_6a12= sum(hijo_6a12),by (correlat) 
egen sumhijo_may12=sum(hijo_may12), by (correlat) 
egen sumhijo_menos12=sum(hijo_menos12), by (correlat) 
egen sumhijo_13y18=sum(hijo_13y18), by (correlat)
egen sumhijo_may18=sum(hijo_may18), by (correlat) 

cap drop summujmen40
cap drop summujmay40_sum
egen summujmen40=sum(mujmen40), by (correlat) 
egen summujmay40_sum=sum(mujmay40), by (correlat) 

cap drop ciclo_vida
gen ciclo_vida=.

*1. PAREJA JOVEN SIN HIJOS (es la pareja que no ha tenido hijos y en la que la mujer tiene 40 años o menos)

replace ciclo_vida=1 if sumesposo_companero>=1 & sumhijo==0 & summujmen40==1

*2. ETAPA INICIAL (corresponde a las familias que solo tienen uno o más hijos de 5 años o menos)

replace ciclo_vida=2 if (sumhijo>0 & sumhijo!=.) & /*
*/(sumhijo_men5>0 & sumhijo_men5!=.) & sumhijo_6a12==0 & sumhijo_may12==0

*3. ETAPA DE EXPANSIÓN O CRECIMIENTO (familias cuyos hijos mayores tienen entre 6 y 12 años, independientemente de la edad del hijo menor)

replace ciclo_vida=3 if (sumhijo>0 & sumhijo!=.) & /*
*/((sumhijo_men5>0 & sumhijo_men5!=.) & (sumhijo_6a12>0 & sumhijo_6a12!=.)) | (sumhijo_men5==0 & (sumhijo_6a12>0 & sumhijo_6a12!=.))/*
*/ & sumhijo_may12==0

*4. ETAPA DE CONSOLIDACIÓN (familias cuyos hijos que tienen entre 13 y 18 años)

replace ciclo_vida=4 if  (sumhijo>0 & sumhijo!=.) & /*
*/(sumhijo_13y18>0 & sumhijo_13y18!=.)

*5. ETAPA DE SALIDA (familias cuyos hijos son mayores de 18 años)

replace ciclo_vida=5 if  (sumhijo>0 & sumhijo!=.) & /*
*/(sumhijo_men5==0 & sumhijo_6a12==0 & sumhijo_13y18==0 & (sumhijo_may18>0 & sumhijo_may18!=.))

*6. PAREJA MAYOR SIN HIJOS_ NIDO VACIO_ (No viven con los hijos y la mujer tiene más de 40 años)

replace ciclo_vida=6 if sumesposo_companero>=1 & sumhijo==0 & summujmay40==1

*7. NO FAMILIARES (sin núcleo conyugal y unipersonales)

replace ciclo_vida=7 if  (sumotro_pariente >=1 | sumno_pariente >=1) & /*
*/(sumesposo_companero==0 & sumhijo==0)

replace ciclo_vida=7 if sumesposo_companero==0 & sumhijo==0 & /*
*/ sumno_pariente==0 & sumotro_pariente==0 


label define ciclo_vida_label 7 "Hogares no familiares" 1 "Pareja joven sin hijos" 2 "Etapa inicial" 3 "Etapa expansión o crecimiento" /*
*/ 4 "Etapa consolidación" 5 "Etapa de salida" 6 "Nido vacío (pareja mayor sin hijos)", modify

lab val ciclo_vida ciclo_vida_label
