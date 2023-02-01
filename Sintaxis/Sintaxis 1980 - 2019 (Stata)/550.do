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
	global bases "C:\Users\Usuario\Dropbox\UMAD\Bases de datos\EUT"
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

* INDICADOR: Promedio de horas en las actividades de cuidados dentro del hogar
* CÓDIGO:    550
* GRUPOS:    Región / Tramo de edad / Terciles de ingresos
* FUENTE:    EUT
* PERÍODO:   2013

**
**MODIFICACIÓN MANUAL: CÓDIGOS Y LOCAL J PARA VARIABLES AUXILIARES**

    *PAÍS URBANO
	local i      = 1
	local codind = 550                  // Código de indicador
	local grupo  = 12 				    // Cantidad de grupos
	local canio  = 1                    // Cantidad de años de la serie

	local filas = (`grupo') * `canio'*6   // Cantidad de grupos más el total por cantidad de años de la serie
	matrix def       MATR= J(`filas',4,.)
	matrix colnames  MATR= VALOR AUXILIAR ANIO SUBPOBLACION


	use "$bases\eut 2013.dta", clear
*1: Disc, 2: 0 a 3, 3: 4 a 5, 4: 6 a 12, 5: 65 o mas
do "C:\Users\Usuario\Dropbox\UMAD\Sociodemografico\Familia\Sintaxis\Auxiliares\EUT.do"



destring region_3, replace
recode region_3 (3=2)
g rango_ed=1 if edad>=14&edad<30
replace rango_ed=2 if edad>=30&edad<45
replace rango_ed=3 if edad>=45&edad<65
replace rango_ed=4 if edad>=65


**Terciles de ingresos
g uno=1  						//Variabe auxiliar para construir cantidad de personas
replace uno=0 if e30==14 		//Excluyo de la suma al servicio doméstico

egen bd_ht19 = sum (uno), by (numero) 
label var bd_ht19 "Cantidad de pseronas (sin servicio doméstico)"

g ypc1=ht11/bd_ht19 if e30==1
xtile tercilesy_prev= ypc1 [aw=peso_eut], n(3)

egen bd_tercilesy= max (tercilesy_prev), by (numero)
label var bd_tercilesy "Quintil de ingreso per cápita del hogar"

drop uno ypc1 tercilesy_prev

local i=1 

foreach x of numlist 1/6  {
	mean horas`x' if horas`x'>0 [aw=peso_eut] 
	matrix MATR  [`i',1]= e(b)
	matrix MATR  [`i',2]=  0
	matrix MATR  [`i',3]=  2013
	matrix MATR  [`i',4]=  `x'
	
	local i  = `i' + 1 
}

	local j  = 	1
	foreach val of numlist 1/2  {
foreach x of numlist 1/6  {
	mean horas`x' [aw=peso_eut] if horas`x'>0 &region_3==`val'
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
	mean horas`x' [aw=peso_eut] if horas`x'>0 &rango_ed==`val'
	matrix MATR  [`i',1]=  e(b)
	matrix MATR  [`i',2]=  `j'
	matrix MATR  [`i',3]=  2013
	matrix MATR  [`i',4]=  `x'
	local i  = `i' + 1
}
    local j  = `j' + 1
}


	local j  = 	7
	foreach val of numlist 1/2  {
foreach x of numlist 1/6  {
	mean horas`x' [aw=peso_eut] if horas`x'>0 &sexo==`val'
	matrix MATR  [`i',1]=  e(b)
	matrix MATR  [`i',2]=  `j'
	matrix MATR  [`i',3]=  2013
	matrix MATR  [`i',4]=  `x'
	local i  = `i' + 1
}
    local j  = `j' + 1
}

	local j  = 	9
	foreach val of numlist 1/3  {
foreach x of numlist 1/6  {
	mean horas`x' [aw=peso_eut] if horas`x'>0 &bd_tercilesy==`val'
	matrix MATR  [`i',1]=  e(b)
	matrix MATR  [`i',2]=  `j'
	matrix MATR  [`i',3]=  2013
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
	local codind = 550                  // Código de indicador
	local grupo  = 12                   // Cantidad de grupos
	local canio  = 1                    // Cantidad de años de la serie


import  excel "$tabulados\Auxiliares\AUX`codind'_PU.xls", firstrow clear
destring ANIO, replace
sort ANIO AUXILIAR 
drop if ANIO==.
destring AUXILIAR, replace

g CODIND                 = 550
g NOMINDICADOR           = "Promedio de horas en las actividades de cuidados dentro del hogar (personas con discapacidad)" if SUBPOBLACION==1
replace NOMINDICADOR     = "Promedio de horas en las actividades de cuidados dentro del hogar (niños/as de 0 a 3 años)" if SUBPOBLACION==2
replace NOMINDICADOR     = "Promedio de horas en las actividades de cuidados dentro del hogar (niños/as de 4 a 5 años)" if SUBPOBLACION==3
replace NOMINDICADOR     = "Promedio de horas en las actividades de cuidados dentro del hogar (niños/as de 6 a 12 años)" if SUBPOBLACION==4
replace NOMINDICADOR     = "Promedio de horas en las actividades de cuidados dentro del hogar (niños/as de 0 a 12)" if SUBPOBLACION==5
replace NOMINDICADOR     = "Promedio de horas en las actividades de cuidados dentro del hogar (mayores de 65 años)" if SUBPOBLACION==6
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

replace SEXO = "Varones" if AUXILIAR==7
replace SEXO = "Mujeres" if AUXILIAR==8


replace NOMINDICADOR= NOMINDICADOR+"TERCIL 1"     if AUXILIAR==9
replace NOMINDICADOR= NOMINDICADOR+"TERCIL 2"     if AUXILIAR==10
replace NOMINDICADOR= NOMINDICADOR+"TERCIL 3"     if AUXILIAR==11

** 
save "$tabulados\Auxiliares\\`codind'_PU.dta", replace




************************************************************************************************
export excel  "$tabulados\\`codind'.xls", cell(A1) firstrow(varlabels) replace
