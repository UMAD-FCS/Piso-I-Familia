*======================================================================================*
*                     **   FAMILIA  **
*                    MATRIZ PARA BASE WEB
*
*
* Creación:      07/02/2022
* Institución:   UMAD, FCS
* Responsable:   Sharon Katzkowicz
* Descripción:   Construcción de matriz para base web (Piso II - Motores)para 
*				 indicadores de dimiensión temática "Familia"
*
*
*======================================================================================*


*======================================================================================*
*                           UBICACIÓN DE ARCHIVOS      
*======================================================================================*

	cd "C:\Users\Usuario\Dropbox\UMAD"
	global bases "C:\Users\Usuario\Dropbox\UMAD\Bases de datos\ENDIS"
	global tabulados "C:\Users\Usuario\Dropbox\UMAD\Sociodemografico\Familia\Tabulados"

/*	cd "C:\Users\Usuario\Dropbox\1. Unidad de Métodos y Acceso a Datos\1. Observatorio UMAD"
	global bases "C:\Users\Usuario\Dropbox\1. Unidad de Métodos y Acceso a Datos\1. Observatorio UMAD\PISO I_MICRODATOS\ECH\Microdatos\Compatibilizadas Iecon\En uso"
	global tabulados C:\Users\Usuario\Dropbox\1. Unidad de Métodos y Acceso a Datos\1. Observatorio UMAD\PISO II_MOTORES\Base web\3. Empleo, salarios y transferencias\Extracciones parciales
*/
	
*======================================================================================*
*              CARACTERÍSTICAS GENERALES DE LA MATRIZ DE EXPORTACAIÓN      
*======================================================================================*
  

*======================================================================================*
*                 3.4. Cuidados y educación
*======================================================================================*

* INDICADOR: Participación de personas en el cuidado de niños/as cuando enferma
* CÓDIGO:    548
* GRUPOS:    REgión / Tramo de edad
* FUENTE:    ENDIS
* PERÍODO:   2013, 2018

**
**MODIFICACIÓN MANUAL: CÓDIGOS Y LOCAL J PARA VARIABLES AUXILIARES**

    *PAÍS URBANO
	local i      = 1
	local codind = 548                  // Código de indicador
	local grupo  = 7 				    // Cantidad de grupos
	local canio  = 2                    // Cantidad de años de la serie

	local filas = (`grupo') * `canio'*6   // Cantidad de grupos más el total por cantidad de años de la serie
	matrix def       MATR= J(`filas',4,.)
	matrix colnames  MATR= VALOR AUXILIAR ANIO CUIDADORA


	use "$bases\endis 2013.dta", clear
*1: madre, 2: padre, 3: hermane, 4: abuele, 5: otro pariente o no pariente, 6: Persona remunerado	
g cuida1=1 if cf4a==1
replace cuida1=0 if cf4a!=1&cf4a!=.

g cuida2=1 if cf4b==1
replace cuida2=0 if cf4b!=1&cf4b!=.

g cuida3=1 if cf4f==1|cf4g==1
replace cuida3=0 if cf4f!=1&cf4f!=.&cf4g!=1&cf4g!=.

g cuida4=1 if cf4h==1
replace cuida4=0 if cf4h!=1&cf4h!=.

g cuida5=1 if cf4d==1|cf4e==1|cf4i==1|cf4j==1
replace cuida5=0 if cf4d!=1&cf4d!=.&cf4e!=1&cf4e!=.&cf4i!=1&cf4i!=.&cf4j!=1&cf4j!=.

g cuida6=1 if cf4k==1
replace cuida6=0 if cf4k!=1&cf4k!=.

local i=1 

foreach x of numlist 1/6 { 
	mean cuida`x' [aw=peso1] 
	matrix MATR  [`i',1]= e(b)
	matrix MATR  [`i',2]=  0
	matrix MATR  [`i',3]=  2013
	matrix MATR  [`i',4]=  `x'
	
	local i  = `i' + 1 
}

	local j  = 	1
	foreach val of numlist 1/2  {
	foreach x of numlist 1/6  {
	mean cuida`x' [aw=peso1] if region_n==`val'
	matrix MATR  [`i',1]=  e(b)
	matrix MATR  [`i',2]=  `j'
	matrix MATR  [`i',3]=  2013
	matrix MATR  [`i',4]=  `x'
	
	local i  = `i' + 1
}
    local j  = `j' + 1
}

	local j  = 	3
	foreach val of numlist 1/4  {
	foreach x of numlist 1/6  {
	mean cuida`x' [aw=peso1] if rango_ed==`val'
	matrix MATR  [`i',1]=  e(b)
	matrix MATR  [`i',2]=  `j'
	matrix MATR  [`i',3]=  2013
	matrix MATR  [`i',4]=  `x'
	
	local i  = `i' + 1
}
	local j  = `j' + 1
}

	use "$bases\endis 2018.dta", clear
rename ponderador peso1
rename region region_n
g rango_ed=1 if e27==0
replace rango_ed=2 if e27==1
replace rango_ed=3 if e27==2
replace rango_ed=4 if e27==3

*1: madre, 2: padre, 3: hermane, 4: abuele, 5: otro pariente o no pariente, 6: Persona remunerado	
g cuida1=1 if ih4_ne_1==1
replace cuida1=0 if ih4_ne_1!=1&ih4_ne_1!=.

g cuida2=1 if ih4_ne_2==1
replace cuida2=0 if ih4_ne_2!=1&ih4_ne_2!=.

g cuida3=1 if ih4_ne_6==1|ih4_ne_7==1
replace cuida3=0 if ih4_ne_6!=1&ih4_ne_6!=.&ih4_ne_7!=1&ih4_ne_7!=.

g cuida4=1 if ih4_ne_8==1
replace cuida4=0 if ih4_ne_8!=1&ih4_ne_8!=.

g cuida5=1 if ih4_ne_4==1|ih4_ne_5==1|ih4_ne_9==1|ih4_ne_10==1
replace cuida5=0 if ih4_ne_4!=1&ih4_ne_4!=.&ih4_ne_5!=1&ih4_ne_5!=.&ih4_ne_9!=1&ih4_ne_9!=.&ih4_ne_10!=1&ih4_ne_10!=.

g cuida6=1 if ih4_ne_11==1
replace cuida6=0 if ih4_ne_11!=1&ih4_ne_11!=.

local i=43 

foreach x of numlist 1/6 { 
	mean cuida`x' [aw=peso1] 
	matrix MATR  [`i',1]= e(b)
	matrix MATR  [`i',2]=  0
	matrix MATR  [`i',3]=  2018
	matrix MATR  [`i',4]=  `x'
	
	local i  = `i' + 1 
}

	local j  = 	1
	foreach val of numlist 1/2  {
	foreach x of numlist 1/6  {
	mean cuida`x' [aw=peso1] if region_n==`val'
	matrix MATR  [`i',1]=  e(b)
	matrix MATR  [`i',2]=  `j'
	matrix MATR  [`i',3]=  2018
	matrix MATR  [`i',4]=  `x'
	
	local i  = `i' + 1
}
    local j  = `j' + 1
}

	local j  = 	3
	foreach val of numlist 1/4  {
	foreach x of numlist 1/6  {
	mean cuida`x' [aw=peso1] if rango_ed==`val'
	matrix MATR  [`i',1]=  e(b)
	matrix MATR  [`i',2]=  `j'
	matrix MATR  [`i',3]=  2018
	matrix MATR  [`i',4]=  `x'
	
	local i  = `i' + 1
}
	local j  = `j' + 1
}


xml_tab MATR, save("$tabulados\Auxiliares\AUX`codind'_PU.xls") replace

**
/*PROBLEMA: GUARDAR COMO EXCEL 97, STATA NO ABRE CON ESTE COMANDO ARCHIVOS EXCEL 2003*/
**MODIFICACIÓN MANUAL: GUARDAR COMO LA BASE DE DATOS Y EDITAR NOMBRES DE INDICADORES E INFORMACIÓN DE METADATO**
**MODIFICACIÓN MANUAL: ACTUALIZAR LOCALS**

	local i      = 1
	local codind = 548                  // Código de indicador
	local grupo  = 7                    // Cantidad de grupos
	local canio  = 2                    // Cantidad de años de la serie


import  excel "$tabulados\Auxiliares\AUX`codind'_PU.xls", firstrow clear
destring ANIO, replace
sort ANIO AUXILIAR 
drop if ANIO==.
destring AUXILIAR, replace
destring CUIDADORA, replace

g CODIND                 = 548
g NOMINDICADOR           = "Participación de personas en el cuidado de niños/as cuando enferman (Madre)" if CUIDADORA==1 
replace NOMINDICADOR     = "Participación de personas en el cuidado de niños/as cuando enferman (Padre)" if CUIDADORA==2 
replace NOMINDICADOR     = "Participación de personas en el cuidado de niños/as cuando enferman (Hermano/a)" if CUIDADORA==3 
replace NOMINDICADOR     = "Participación de personas en el cuidado de niños/as cuando enferman (Abuelo/a)" if CUIDADORA==4 
replace NOMINDICADOR     = "Participación de personas en el cuidado de niños/as cuando enferman (Otro pariente o no pariente)" if CUIDADORA==5 
replace NOMINDICADOR     = "Participación de personas en el cuidado de niños/as cuando enferman (Persona remunerada)" if CUIDADORA==6 
g SEXO                   = "NA"
g ASCENDENCIA    		 = "NA"
g QUINTIL       		 = "NA"
g DEPARTAMENTOUY		 = "NA"
g URBANORURALUY 		 = "NA"
g PAÍS			 		 = "URUGUAY"
g RESPONSABLE			 = "SHARON KATZKOWICZ"

**

**
**MODIFICACIÓN MANUAL: CÓDIGOS Y TÍTULOS DE LA VARIABLES AUXILIARES CORRESPONDIENTES**

replace URBANORURALUY = "Montevideo"    if AUXILIAR==1
replace URBANORURALUY = "Interior urbano"   if AUXILIAR==2

replace NOMINDICADOR = NOMINDICADOR + ". 0 años."   if AUXILIAR==3
replace NOMINDICADOR = NOMINDICADOR + ". 1 año."   if AUXILIAR==4
replace NOMINDICADOR = NOMINDICADOR + ". 2 años."     if AUXILIAR==5
replace NOMINDICADOR = NOMINDICADOR + ". 3 años."     if AUXILIAR==6

/*
replace QUINTIL= "QUINTIL 1"   if AUXILIAR==7
replace QUINTIL= "QUINTIL 2"   if AUXILIAR==8
replace QUINTIL= "QUINTIL 3"     if AUXILIAR==9
replace QUINTIL= "QUINTIL 4"     if AUXILIAR==10
replace QUINTIL= "QUINTIL 5"     if AUXILIAR==11
*/
** 
save "$tabulados\Auxiliares\\`codind'_PU.dta", replace




************************************************************************************************
export excel  "$tabulados\\`codind'.xls", cell(A1) firstrow(varlabels) replace
