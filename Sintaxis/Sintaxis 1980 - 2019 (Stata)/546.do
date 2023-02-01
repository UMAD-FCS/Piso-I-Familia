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

* INDICADOR: Porcentaje de personas según beneficios en el trabajo para la crianza
* CÓDIGO:    546
* GRUPOS:    -
* FUENTE:    ENDIS
* PERÍODO:   2018

**
**MODIFICACIÓN MANUAL: CÓDIGOS Y LOCAL J PARA VARIABLES AUXILIARES**

    *PAÍS URBANO
	local i      = 1
	local codind = 546                  // Código de indicador
	local grupo  = 3 				    // Cantidad de grupos
	local canio  = 1                    // Cantidad de años de la serie

	local filas = (`grupo') * `canio'*14   // Cantidad de grupos más el total por cantidad de años de la serie
	matrix def       MATR= J(`filas',4,.)
	matrix colnames  MATR= VALOR AUXILIAR ANIO BENEFICIO


	use "$bases\endis 2018.dta", clear
rename ponderador peso1


	foreach i of numlist 1/14 {
		foreach a of numlist 1/3 {
		g ih6_`i'_`a'=0 if ih6_ne_`i'!=`a'
		replace ih6_`i'_`a'=1 if ih6_ne_`i'==`a'
		}
	}

local i=1 
foreach i of numlist 1/14 { 
local j=0
foreach a of numlist 1/3{ 
	mean ih6_`i'_`a' [aw=peso1] 
	matrix MATR  [`i'+14*(`a'-1),1]= e(b)
	matrix MATR  [`i'+14*(`a'-1),2]=  `j'
	matrix MATR  [`i'+14*(`a'-1),3]=  2018
	matrix MATR  [`i'+14*(`a'-1),4]=  `i'
	
	local j  = `j' + 1 
}
}

xml_tab MATR, save("$tabulados\Auxiliares\AUX`codind'_PU.xls") replace

**
/*PROBLEMA: GUARDAR COMO EXCEL 97, STATA NO ABRE CON ESTE COMANDO ARCHIVOS EXCEL 2003*/
**MODIFICACIÓN MANUAL: GUARDAR COMO LA BASE DE DATOS Y EDITAR NOMBRES DE INDICADORES E INFORMACIÓN DE METADATO**
**MODIFICACIÓN MANUAL: ACTUALIZAR LOCALS**

	local i      = 1
	local codind = 546                  // Código de indicador
	local grupo  = 3                    // Cantidad de grupos
	local canio  = 2                    // Cantidad de años de la serie


import  excel "$tabulados\Auxiliares\AUX`codind'_PU.xls", firstrow clear
destring ANIO, replace
sort ANIO AUXILIAR 
drop if ANIO==.
destring AUXILIAR, replace
destring BENEFICIO, replace

g CODIND                 = 546
g NOMINDICADOR           = "Porcentaje de personas según beneficios en el trabajo para la crianza (Priorización horarios)" if BENEFICIO==1 
replace NOMINDICADOR     = "Porcentaje de personas según beneficios en el trabajo para la crianza (Flexibilidad horaria en períodos especiales)" if BENEFICIO==2 
replace NOMINDICADOR     = "Porcentaje de personas según beneficios en el trabajo para la crianza (Contabilización horas semanales o mensuales)" if BENEFICIO==3 
replace NOMINDICADOR     = "Porcentaje de personas según beneficios en el trabajo para la crianza (Reintegro gradual luego de permiso medio horario)" if BENEFICIO==4 
replace NOMINDICADOR     = "Porcentaje de personas según beneficios en el trabajo para la crianza (Trabajo a distancia)" if BENEFICIO==5 
replace NOMINDICADOR     = "Porcentaje de personas según beneficios en el trabajo para la crianza (Permiso acompañar familiares citas médicas o actividades educativas)" if BENEFICIO==6 
replace NOMINDICADOR     = "Porcentaje de personas según beneficios en el trabajo para la crianza (Centros de cuidados en la empresa)" if BENEFICIO==7 
replace NOMINDICADOR     = "Porcentaje de personas según beneficios en el trabajo para la crianza (Actividades especiales para perpiodos de vacaciones)" if BENEFICIO==8 
replace NOMINDICADOR     = "Porcentaje de personas según beneficios en el trabajo para la crianza (Centros de cuidado infantil en días específicos o de complemento del horario escolar)" if BENEFICIO==9 
replace NOMINDICADOR     = "Porcentaje de personas según beneficios en el trabajo para la crianza (Salas de lactancia)" if BENEFICIO==10 
replace NOMINDICADOR     = "Porcentaje de personas según beneficios en el trabajo para la crianza (Convenios con servicios de cuidados)" if BENEFICIO==11 
replace NOMINDICADOR     = "Porcentaje de personas según beneficios en el trabajo para la crianza (Vales o transferencias para servicios de cuidados)" if BENEFICIO==12 
replace NOMINDICADOR     = "Porcentaje de personas según beneficios en el trabajo para la crianza (Licencia especial para cuidado familiar)" if BENEFICIO==13 
replace NOMINDICADOR     = "Porcentaje de personas según beneficios en el trabajo para la crianza (Otro)" if BENEFICIO==14 
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

replace NOMINDICADOR = NOMINDICADOR + ". Solo padre."   if AUXILIAR==0
replace NOMINDICADOR = NOMINDICADOR + ". Solo madre."   if AUXILIAR==1
replace NOMINDICADOR = NOMINDICADOR + ". Ambos."     if AUXILIAR==2

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
