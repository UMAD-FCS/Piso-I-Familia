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
	global bases "C:\Users\Usuario\Dropbox\UMAD\Bases de datos\ENVBGG"
	global tabulados "C:\Users\Usuario\Dropbox\UMAD\Sociodemografico\Familia\Tabulados"

/*	cd "C:\Users\Usuario\Dropbox\1. Unidad de Métodos y Acceso a Datos\1. Observatorio UMAD"
	global bases "C:\Users\Usuario\Dropbox\1. Unidad de Métodos y Acceso a Datos\1. Observatorio UMAD\PISO I_MICRODATOS\ECH\Microdatos\Compatibilizadas Iecon\En uso"
	global tabulados C:\Users\Usuario\Dropbox\1. Unidad de Métodos y Acceso a Datos\1. Observatorio UMAD\PISO II_MOTORES\Base web\3. Empleo, salarios y transferencias\Extracciones parciales
*/
	
*======================================================================================*
*              CARACTERÍSTICAS GENERALES DE LA MATRIZ DE EXPORTACAIÓN      
*======================================================================================*
  

*======================================================================================*
*                 3.3. Violencia de género por parte de la familia
*======================================================================================*

* INDICADOR: Prevalencia de violencia basada en género de mujeres de 65 años o más por parte de la familia 
* CÓDIGO:    537
* GRUPOS:    -
* FUENTE:    ENVBGG
* PERÍODO:   2013, 2019

**
**MODIFICACIÓN MANUAL: CÓDIGOS Y LOCAL J PARA VARIABLES AUXILIARES**

    *PAÍS URBANO
	local i      = 1
	local codind = 537                  // Código de indicador
	local grupo  = 1                    // Cantidad de grupos
	local canio  = 2                    // Cantidad de años de la serie

	local filas = (`grupo') * `canio'    // Cantidad de grupos más el total por cantidad de años de la serie
	matrix def       MATR= J(`filas',3,.)
	matrix colnames  MATR= VALOR AUXILIAR ANIO


        use "$bases\EVBGG_2013.dta", clear


*======================================================================================*
*                           PREVALENCIA      
*======================================================================================*
drop if seleccio==0
*Indicador:	Porcentaje de mujeres de 15 años o más que vivieron situaciones de VBG en la infancia por parte de la familia según tipo de violencia.
recode VG_adultas (0=.) (2=0) (88=.) (99=.)

lab def dicotomica 0 "No" 1 "Si"
lab val VG_adultas dicotomica 

rename VG_adultas adultas


	mean adultas [aw=ponderador] 
	matrix MATR  [`i',1]= e(b)
	matrix MATR  [`i',2]=  0
	matrix MATR  [`i',3]=  2013
	local i  = `i' + 1 
   		
	
	use "$bases\EVBGG_2019.dta", clear

drop if region==5

*======================================================================================*
*                           PREVALENCIA      
*======================================================================================*

*Indicador:	Porcentaje de mujeres de 15 años o más que vivieron situaciones de VBG en la familia por parte de la familia según tipo de violencia.
recode VT_adultas (0=.) (2=0)

lab def dicotomica 0 "No" 1 "Si"
lab val VT_adultas dicotomica 

rename VT_adultas adultas
	
	mean adultas [aw=ponderador] 
	matrix MATR  [`i',1]= e(b)
	matrix MATR  [`i',2]=  0
	matrix MATR  [`i',3]=  2019

xml_tab MATR, save("$tabulados\Auxiliares\AUX`codind'_PU.xls") replace

**
/*PROBLEMA: GUARDAR COMO EXCEL 97, STATA NO ABRE CON ESTE COMANDO ARCHIVOS EXCEL 2003*/
**MODIFICACIÓN MANUAL: GUARDAR COMO LA BASE DE DATOS Y EDITAR NOMBRES DE INDICADORES E INFORMACIÓN DE METADATO**
**MODIFICACIÓN MANUAL: ACTUALIZAR LOCALS**

	local i      = 1
	local codind = 537                  // Código de indicador
	local grupo  = 1                   // Cantidad de grupos
	local canio  = 2                   // Cantidad de años de la serie


import  excel "$tabulados\Auxiliares\AUX`codind'_PU.xls", firstrow clear
destring ANIO, replace
sort ANIO AUXILIAR 
drop if ANIO==.
destring AUXILIAR, replace

g CODIND                 = 537
g NOMINDICADOR           = "Prevalencia de violencia basada en género de mujeres de 65 años o más por parte de la familia"
g SEXO                   = "NA"
g ASCENDENCIA    		 = "NA"
g QUINTIL       		 = "NA"
g DEPARTAMENTOUY		 = "NA"
g URBANORURALUY 		 = "NA"
g NSE					 = "NA"
g NIVELEDU				 = "NA"
g PAÍS			 		 = "URUGUAY"
g RESPONSABLE			 = "SHARON KATZKOWICZ"

**
** 
save "$tabulados\Auxiliares\\`codind'_PU.dta", replace




************************************************************************************************
export excel  "$tabulados\\`codind'.xls", cell(A1) firstrow(varlabels) replace
