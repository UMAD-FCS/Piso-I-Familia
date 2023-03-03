
*======================================================================================*
*                     **   FAMILIA  **
*                    MATRIZ PARA BASE WEB
*
*
* Creación:      12/10/2020
* Institución:   UMAD, FCS
* Responsable:   Jimena Pandolfi
* Descripción:   Construcción de matriz para base web (Piso II - Motores)para 
*				 indicadores de dimiensión temática "Empleo, salarios y transferencias"
*
*
*======================================================================================*


*======================================================================================*
*                           UBICACIÓN DE ARCHIVOS      
*======================================================================================*

	cd "C:\Users\Usuario\Dropbox\UMAD"
	global bases "C:\Users\Usuario\Dropbox\UMAD\Bases de datos\Encuesta Continua de Hogares (ECH)\Compatibilizadas IECON\Bases DESCA"
	global tabulados "C:\Users\Usuario\Dropbox\UMAD\Sociodemografico\Familia\Tabulados"

/*	cd "C:\Users\Usuario\Dropbox\1. Unidad de Métodos y Acceso a Datos\1. Observatorio UMAD"
	global bases "C:\Users\Usuario\Dropbox\1. Unidad de Métodos y Acceso a Datos\1. Observatorio UMAD\PISO I_MICRODATOS\ECH\Microdatos\Compatibilizadas Iecon\En uso"
	global tabulados C:\Users\Usuario\Dropbox\1. Unidad de Métodos y Acceso a Datos\1. Observatorio UMAD\PISO II_MOTORES\Base web\3. Empleo, salarios y transferencias\Extracciones parciales
*/
	
*======================================================================================*
*              CARACTERÍSTICAS GENERALES DE LA MATRIZ DE EXPORTACAIÓN      
*======================================================================================*

	global seieTOT 12/19         // Años de serie para total país

	  

*======================================================================================*
*                 3.4. Cuidados y educación
*======================================================================================*

* INDICADOR: Tasa de asistencia a la educación de niños/as de 0 a 3 años
* CÓDIGO:    551
* GRUPOS:    Región / Ascendencia étnico-racial del jefe/a / Sexo del jefe/a / Tramo de edad del jefe/a / Quintil de ingreso / Pobreza
* FUENTE:    ECH (INE)
* PERÍODO:   2012-2020 Total país 

**
**MODIFICACIÓN MANUAL: CÓDIGOS Y LOCAL J PARA VARIABLES AUXILIARES**

    *TOTAL PAÍS
	local i      = 1
	local codind = 551                  // Código de indicador
	local grupo  = 15                   // Cantidad de grupos
	local canio  = 8                   // Cantidad de años de la serie

	local filas = (`grupo') * `canio'  // Cantidad de grupos más el total por cantidad de años de la serie
	matrix def       MATR= J(`filas',3,.)
	matrix colnames  MATR= VALOR AUXILIAR ANIO

	foreach anio of numlist $seieTOT  {
    use "$bases\fusionada_personasyhogares_`anio'.dta", clear
	
	cap drop bd_region
	g bd_region=.
	replace bd_region=1 if region_4== 1 | region_4== 2
	replace bd_region=2 if region_4== 3
	replace bd_region=3 if region_4== 4

	keep if bc_pe3!=.&bc_pe3<3
	rename asistemen3 asiste
	
	mean asiste [aw=bc_pesoan] 
	matrix MATR  [`i',1]= e(b)
	matrix MATR  [`i',2]=  0
	matrix MATR  [`i',3]=  `anio'
	
	local i  = `i' + 1 

   	
	local j  = 	1
	foreach val of numlist 1/3  {
	mean asiste [aw=bc_pesoan] if bd_region==`val'
	matrix MATR  [`i',1]=  e(b)
	matrix MATR  [`i',2]=  `j'
	matrix MATR  [`i',3]=  `anio'
	
	local i  = `i' + 1
	local j  = `j' + 1
}
    *
	
	local j  = 	3
	foreach val of numlist 1/2  {
	mean asiste [aw=bc_pesoan] if bc_pe2==`val'
	matrix MATR  [`i',1]=  e(b)
	matrix MATR  [`i',2]=  `j'
    matrix MATR  [`i',3]=  `anio'
	
	local i  = `i' + 1
	local j  = `j' + 1
}
    *
	
	local j  = 	5
	foreach val of numlist 1/2  {
	mean asiste [aw=bc_pesoan] if bd_e29_1==`val'
	matrix MATR  [`i',1]=  e(b)
	matrix MATR  [`i',2]=  `j'
    matrix MATR  [`i',3]=  `anio'
	
	local i  = `i' + 1
	local j  = `j' + 1
	}
    *
	
	local j  = 	7
	foreach val of numlist 1/5  {
	mean asiste [aw=bc_pesoan] if bd_quintilesy==`val'
	matrix MATR  [`i',1]=  e(b)
	matrix MATR  [`i',2]=  `j'
    matrix MATR  [`i',3]=  `anio'
	
	local i  = `i' + 1
	local j  = `j' + 1
	}
    *
	
	local j  = 	12
	foreach val of numlist 0/1  {
	mean asiste [aw=bc_pesoan] if pobre06==`val'
	matrix MATR  [`i',1]=  e(b)
	matrix MATR  [`i',2]=  `j'
    matrix MATR  [`i',3]=  `anio'
	
	local i  = `i' + 1
	local j  = `j' + 1
	}
    *

	}
    *

xml_tab MATR, save("$tabulados\Auxiliares\AUX`codind'_TP.xls") replace

**
/*PROBLEMA: GUARDAR COMO EXCEL 97, STATA NO ABRE CON ESTE COMANDO ARCHIVOS EXCEL 2003*/
**MODIFICACIÓN MANUAL: GUARDAR COMO LA BASE DE DATOS Y EDITAR NOMBRES DE INDICADORES E INFORMACIÓN DE METADATO**
**MODIFICACIÓN MANUAL: ACTUALIZAR LOCALS**

	local i      = 1
	local codind = 551                  // Código de indicador
	local grupo  = 14                   // Cantidad de grupos
	local canio  = 15                   // Cantidad de años de la serie


import  excel "$tabulados\Auxiliares\AUX`codind'_TP.xls", firstrow clear
rename ANIO ANIO_ANT
destring ANIO, replace
g ANIO=2000+ANIO_ANT if ANIO_ANT<=20
replace ANIO=1900+ANIO_ANT if ANIO_ANT>80
sort ANIO AUXILIAR 
drop if ANIO==.
drop ANIO_ANT
destring AUXILIAR, replace

g CODIND                 = 551
g NOMINDICADOR           = "Tasa de asistencia a la educación inicial de niños/as de 0 a 2 años" 
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

replace URBANORURALUY    = "URBANO (MAS DE 5.000 HABITANTES)"   if AUXILIAR==1
replace URBANORURALUY    = "URBANO (MENOS DE 5.000 HABITANTES)"   if AUXILIAR==2
replace URBANORURALUY    = "RURAL DISPERSO"                       if AUXILIAR==3

replace SEXO    = "VARONES"            if AUXILIAR==4
replace SEXO    = "MUJERES"            if AUXILIAR==5

replace ASCENDENCIA    = "AFRO"        if AUXILIAR==6
replace ASCENDENCIA    = "NO AFRO"     if AUXILIAR==7

replace QUINTIL			 = "QUINTIL 1"							  if AUXILIAR==8
replace QUINTIL			 = "QUINTIL 2"							  if AUXILIAR==9
replace QUINTIL			 = "QUINTIL 3"							  if AUXILIAR==10
replace QUINTIL			 = "QUINTIL 4"							  if AUXILIAR==11
replace QUINTIL			 = "QUINTIL 5"							  if AUXILIAR==12

replace NOMINDICADOR    = NOMINDICADOR+ ". Hogares no en situación de pobreza"    if AUXILIAR==13
replace NOMINDICADOR    = NOMINDICADOR+ ". Hogares en situación de pobreza"       if AUXILIAR==14
 

** 

** 
save "$tabulados\Auxiliares\\`codind'_TP.dta", replace

export excel  "$tabulados\\`codind'.xls", cell(A1) firstrow(varlabels) replace
