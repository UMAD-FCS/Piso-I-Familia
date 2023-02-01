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
	global bases "C:\Users\Usuario\Dropbox\UMAD\Bases de datos\ENCOR"
	global tabulados "C:\Users\Usuario\Dropbox\UMAD\Sociodemografico\Familia\Tabulados"

/*	cd "C:\Users\Usuario\Dropbox\1. Unidad de Métodos y Acceso a Datos\1. Observatorio UMAD"
	global bases "C:\Users\Usuario\Dropbox\1. Unidad de Métodos y Acceso a Datos\1. Observatorio UMAD\PISO I_MICRODATOS\ECH\Microdatos\Compatibilizadas Iecon\En uso"
	global tabulados C:\Users\Usuario\Dropbox\1. Unidad de Métodos y Acceso a Datos\1. Observatorio UMAD\PISO II_MOTORES\Base web\3. Empleo, salarios y transferencias\Extracciones parciales
*/
	
*======================================================================================*
*              CARACTERÍSTICAS GENERALES DE LA MATRIZ DE EXPORTACAIÓN      
*======================================================================================*
  

*======================================================================================*
*                 3.2. Planificación familiar
*======================================================================================*

* INDICADOR: Porcentaje de personas que en su primer embarazo o el de su pareja querían tener un hijo en ese momento, querían más adelante o no querían
* CÓDIGO:    525
* GRUPOS:    Región / Sexo / Tercil de ingresos / Tramo edad / Edad primer hijo 
* FUENTE:    ENCOR
* PERÍODO:   2015

**
**MODIFICACIÓN MANUAL: CÓDIGOS Y LOCAL J PARA VARIABLES AUXILIARES**

    *PAÍS URBANO
	local i      = 1
	local codind = 525                  // Código de indicador
	local grupo  = 14                   // Cantidad de grupos
	local canio  = 1                    // Cantidad de años de la serie

	local filas = (`grupo') * `canio'*3   // Cantidad de grupos más el total por cantidad de años de la serie
	matrix def       MATR= J(`filas',4,.)
	matrix colnames  MATR= VALOR AUXILIAR ANIO QUERIA

    use "$bases\ENCOR.dta", clear
 
g region=1 if region_4==1
replace region=2 if region_4==2

destring anio, replace
g edad_hij=2016-anio if anio>0&anio<2017
g edad_mp_hij=edad_act-edad_hij
 
g tramoed1=1 if edad_act>14&edad_act<20
replace tramoed1=2 if edad_act>19&edad_act<30
replace tramoed1=3 if edad_act>29&edad_act<46

g tramoed2=1 if edad_mp_hij<20
replace tramoed2=2 if edad_mp_hij>19&edad_mp_hij<30
replace tramoed2=3 if edad_mp_hij>29&edad_mp_hij<46

	g planif=1 if hr15==1
	replace planif=2 if hr15==2
	replace planif=3 if hr15==3
	replace planif=1 if hr15==.&ea22==1
	replace planif=2 if hr15==.&ea22==2
	replace planif=3 if hr15==.&ea22==3

	ta planif if planif>0&planif<4, gen(planif)	
	
	foreach x of numlist 1/3 {
	mean planif`x' [aw=peso] 
	matrix MATR  [`i',1]= e(b)
	matrix MATR  [`i',2]=  0
	matrix MATR  [`i',3]=  2015
	matrix MATR  [`i',4]=  `x'
	
	local i  = `i' + 1 
}
   	
	local j  = 	1
	foreach val of numlist 1/2  {
	foreach x of numlist 1/3 {
	mean planif`x' [aw=peso] if region==`val'
	matrix MATR  [`i',1]=  e(b)
	matrix MATR  [`i',2]=  `j'
	matrix MATR  [`i',3]=  2015
	matrix MATR  [`i',4]=  `x'
	
	local i  = `i' + 1
    }
	local j  = `j' + 1
}

	local j  = 	3
	foreach val of numlist 1/2  {
	foreach x of numlist 1/3 {
	mean planif`x' [aw=peso] if sexo==`val'
	matrix MATR  [`i',1]=  e(b)
	matrix MATR  [`i',2]=  `j'
	matrix MATR  [`i',3]=  2015
	matrix MATR  [`i',4]=  `x'
	
	local i  = `i' + 1
    }
	local j  = `j' + 1
}


    *
	local j  = 	5
	foreach val of numlist 1/3  {
	foreach x of numlist 1/3 {
	mean planif`x' [aw=peso] if bd_tercilesy==`val'
	matrix MATR  [`i',1]=  e(b)
	matrix MATR  [`i',2]=  `j'
	matrix MATR  [`i',3]=  2015
	matrix MATR  [`i',4]=  `x'
	
	local i  = `i' + 1
    }
	local j  = `j' + 1
}
    *
	local j  = 	8
	foreach val of numlist 1/3  {
	foreach x of numlist 1/3 {
	mean planif`x' [aw=peso] if tramoed1==`val'
	matrix MATR  [`i',1]=  e(b)
	matrix MATR  [`i',2]=  `j'
	matrix MATR  [`i',3]=  2015
	matrix MATR  [`i',4]=  `x'
	
	local i  = `i' + 1
    }
	local j  = `j' + 1
}
    *
	
	local j  = 	11
	foreach val of numlist 1/3  {
	foreach x of numlist 1/3 {
	mean planif`x' [aw=peso] if tramoed2==`val'
	matrix MATR  [`i',1]=  e(b)
	matrix MATR  [`i',2]=  `j'
	matrix MATR  [`i',3]=  2015
	matrix MATR  [`i',4]=  `x'
	
	local i  = `i' + 1
    }
	local j  = `j' + 1
}
    *
	

xml_tab MATR, save("$tabulados\Auxiliares\AUX`codind'_PU.xls") replace

**
/*PROBLEMA: GUARDAR COMO EXCEL 97, STATA NO ABRE CON ESTE COMANDO ARCHIVOS EXCEL 2003*/
**MODIFICACIÓN MANUAL: GUARDAR COMO LA BASE DE DATOS Y EDITAR NOMBRES DE INDICADORES E INFORMACIÓN DE METADATO**
**MODIFICACIÓN MANUAL: ACTUALIZAR LOCALS**

	local i      = 1
	local codind = 525                  // Código de indicador
	local grupo  = 14                   // Cantidad de grupos
	local canio  = 3                   // Cantidad de años de la serie


import  excel "$tabulados\Auxiliares\AUX`codind'_PU.xls", firstrow clear
destring ANIO, replace
sort ANIO AUXILIAR QUERIA
drop if ANIO==.
destring QUERIA, replace
destring AUXILIAR, replace

g CODIND                 = 525
g NOMINDICADOR           = "Porcentaje de personas que en su primer embarazo o el de su pareja querían tener un hijo en ese momento, querían más adelante o no querían (Quería)" if QUERIA==1
replace NOMINDICADOR     = "Porcentaje de personas que en su primer embarazo o el de su pareja querían tener un hijo en ese momento, querían más adelante o no querían (Quería más adelante)" if QUERIA==2
replace NOMINDICADOR     = "Porcentaje de personas que en su primer embarazo o el de su pareja querían tener un hijo en ese momento, querían más adelante o no querían (No quería)" if QUERIA==3
g SEXO                   = "NA"
g ASCENDENCIA    		 = "NA"
g QUINTIL       		 = "NA"
g DEPARTAMENTOUY		 = "NA"
g URBANORURALUY 		 = "NA"
g EDAD_HIJ				 = "NA"
g PAÍS			 		 = "URUGUAY"
g RESPONSABLE			 = "SHARON KATZKOWICZ"

**

**
**MODIFICACIÓN MANUAL: CÓDIGOS Y TÍTULOS DE LA VARIABLES AUXILIARES CORRESPONDIENTES**

replace URBANORURALUY = "Montevideo"   if AUXILIAR==1
replace URBANORURALUY = "Interior urbano"   if AUXILIAR==2

replace SEXO = "VARONES"   if AUXILIAR==3
replace SEXO = "MUJERES"   if AUXILIAR==4

replace QUINTIL = "Tercil 1"   if AUXILIAR==5
replace QUINTIL = "Tercil 2"   if AUXILIAR==6
replace QUINTIL = "Tercil 3"   if AUXILIAR==7


replace NOMINDICADOR = NOMINDICADOR + ". Entre 15 y 19 años."   if AUXILIAR==8
replace NOMINDICADOR = NOMINDICADOR + ". Entre 20 y 29 años."   if AUXILIAR==9
replace NOMINDICADOR = NOMINDICADOR + ". Mayor de 30 años."     if AUXILIAR==10

replace EDAD_HIJ = "Menor de 20 años."   if AUXILIAR==11
replace EDAD_HIJ = "Entre 20 y 29 años."   if AUXILIAR==12
replace EDAD_HIJ = "Entre 30 y 44 años."   if AUXILIAR==13


** 
save "$tabulados\Auxiliares\\`codind'_PU.dta", replace




************************************************************************************************
export excel  "$tabulados\\`codind'.xls", cell(A1) firstrow(varlabels) replace
