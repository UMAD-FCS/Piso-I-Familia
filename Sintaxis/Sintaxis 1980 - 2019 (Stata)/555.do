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
*                 3.4. Salud infantil
*======================================================================================*

* INDICADOR: Porcentaje de niños lactantes que reciben lactancia exclusiva durante los primeros 6 meses
* CÓDIGO:    555
* GRUPOS:    Región / Tramo de edad
* FUENTE:    ENDIS
* PERÍODO:   2013, 2018

**
**MODIFICACIÓN MANUAL: CÓDIGOS Y LOCAL J PARA VARIABLES AUXILIARES**

    *PAÍS URBANO
	local i      = 1
	local codind = 555                  // Código de indicador
	local grupo  = 7 				    // Cantidad de grupos
	local canio  = 2                    // Cantidad de años de la serie

	local filas = (`grupo') * `canio'   // Cantidad de grupos más el total por cantidad de años de la serie
	matrix def       MATR= J(`filas',3,.)
	matrix colnames  MATR= VALOR AUXILIAR ANIO 


	use "$bases\endis 2013.dta", clear
g lact_excl=1 if si8==1|si11==3
replace lact_exc=0 if si6==2|si11==1|si11==2

local i=1 

	mean lact_excl [aw=peso1] 
	matrix MATR  [`i',1]= e(b)
	matrix MATR  [`i',2]=  0
	matrix MATR  [`i',3]=  2013
	
	local i  = `i' + 1 


	local j  = 	1
	foreach val of numlist 1/2  {
	mean lact_excl [aw=peso1] if region_n==`val'
	matrix MATR  [`i',1]=  e(b)
	matrix MATR  [`i',2]=  `j'
	matrix MATR  [`i',3]=  2013
	
	local i  = `i' + 1
    local j  = `j' + 1
}

	local j  = 	3
	foreach val of numlist 1/4  {
	mean lact_excl [aw=peso1] if rango_ed==`val'
	matrix MATR  [`i',1]=  e(b)
	matrix MATR  [`i',2]=  `j'
	matrix MATR  [`i',3]=  2013
	
	local i  = `i' + 1
	local j  = `j' + 1
}

	use "$bases\endis 2018.dta", clear
rename ponderador peso1
rename region region_n
g rango_ed=1 if e27==0
replace rango_ed=2 if e27==1
replace rango_ed=3 if e27==2
replace rango_ed=4 if e27==3

g lact_excl=1 if si20_ne_1==98|(si20_ne_1_1>=6&si20_ne_1_1!=999)
replace lact_exc=0 if si6==2|(si20_ne_1_1<6&si20_ne_1_1!=.)

local i=8 

	mean lact_excl [aw=peso1] 
	matrix MATR  [`i',1]= e(b)
	matrix MATR  [`i',2]=  0
	matrix MATR  [`i',3]=  2018
	local i  = `i' + 1 


	local j  = 	1
	foreach val of numlist 1/2  {
	mean lact_excl [aw=peso1] if region_n==`val'
	matrix MATR  [`i',1]=  e(b)
	matrix MATR  [`i',2]=  `j'
	matrix MATR  [`i',3]=  2018
	
	local i  = `i' + 1
    local j  = `j' + 1
}

	local j  = 	3
	foreach val of numlist 1/4  {
	mean lact_excl [aw=peso1] if rango_ed==`val'
	matrix MATR  [`i',1]=  e(b)
	matrix MATR  [`i',2]=  `j'
	matrix MATR  [`i',3]=  2018
	
	local i  = `i' + 1
	local j  = `j' + 1
}


xml_tab MATR, save("$tabulados\Auxiliares\AUX`codind'_PU.xls") replace

**
/*PROBLEMA: GUARDAR COMO EXCEL 97, STATA NO ABRE CON ESTE COMANDO ARCHIVOS EXCEL 2003*/
**MODIFICACIÓN MANUAL: GUARDAR COMO LA BASE DE DATOS Y EDITAR NOMBRES DE INDICADORES E INFORMACIÓN DE METADATO**
**MODIFICACIÓN MANUAL: ACTUALIZAR LOCALS**

	local i      = 1
	local codind = 555                  // Código de indicador
	local grupo  = 7                    // Cantidad de grupos
	local canio  = 2                    // Cantidad de años de la serie


import  excel "$tabulados\Auxiliares\AUX`codind'_PU.xls", firstrow clear
destring ANIO, replace
sort ANIO AUXILIAR 
drop if ANIO==.
destring AUXILIAR, replace

g CODIND                 = 555
g NOMINDICADOR           = "Porcentaje de niños lactantes que reciben lactancia exclusiva durante los primeros 6 meses" 
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
