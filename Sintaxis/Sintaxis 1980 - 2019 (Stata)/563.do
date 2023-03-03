
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

    global seieURB 90/99 0/19   // Años de serie para país urbano
	global seieTOT 6/19         // Años de serie para total país

	  

*======================================================================================*
*                 3.5. Salud infantil
*======================================================================================*

* INDICADOR: Porcentaje de niños entre 0 y 17 años sin cobertura de salud
* CÓDIGO:    563
* GRUPOS:    Región / Ascendencia étnico-racial del jefe/a / Sexo del jefe/a / Tramo de edad del jefe/a / Quintil de ingreso / Pobreza
* FUENTE:    ECH (INE)
* PERÍODO:   1990-2020 País urbano / 2006-2020 Total país 

**
**MODIFICACIÓN MANUAL: CÓDIGOS Y LOCAL J PARA VARIABLES AUXILIARES**

    *TOTAL PAÍS
	local i      = 1
	local codind = 563                  // Código de indicador
	local grupo  = 14                   // Cantidad de grupos
	local canio  = 15                   // Cantidad de años de la serie

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

	keep if bc_pe3!=.&bc_pe3<18
	g nocobert=1 if bc_pe6a==1
	replace nocobert=0 if bc_pe6a!=1
	
	
	mean nocobert [aw=bc_pesoan] 
	matrix MATR  [`i',1]= e(b)
	matrix MATR  [`i',2]=  0
	matrix MATR  [`i',3]=  `anio'
	
	local i  = `i' + 1 

   	
/*NO se procesa país urbano para no duplicar valores*/
	
	local j  = 	1
	foreach val of numlist 2/3  {
	mean nocobert [aw=bc_pesoan] if bd_region==`val'
	matrix MATR  [`i',1]=  e(b)
	matrix MATR  [`i',2]=  `j'
	matrix MATR  [`i',3]=  `anio'
	
	local i  = `i' + 1
	local j  = `j' + 1
}
    *
	
	local j  = 	3
	foreach val of numlist 1/2  {
	mean nocobert [aw=bc_pesoan] if bc_pe2==`val'
	matrix MATR  [`i',1]=  e(b)
	matrix MATR  [`i',2]=  `j'
    matrix MATR  [`i',3]=  `anio'
	
	local i  = `i' + 1
	local j  = `j' + 1
}
    *
	
	local j  = 	5
	foreach val of numlist 1/2  {
	mean nocobert [aw=bc_pesoan] if bd_e29_1==`val'
	matrix MATR  [`i',1]=  e(b)
	matrix MATR  [`i',2]=  `j'
    matrix MATR  [`i',3]=  `anio'
	
	local i  = `i' + 1
	local j  = `j' + 1
	}
    *
	
	local j  = 	7
	foreach val of numlist 1/5  {
	mean nocobert [aw=bc_pesoan] if bd_quintilesy==`val'
	matrix MATR  [`i',1]=  e(b)
	matrix MATR  [`i',2]=  `j'
    matrix MATR  [`i',3]=  `anio'
	
	local i  = `i' + 1
	local j  = `j' + 1
	}
    *
	
	local j  = 	12
	foreach val of numlist 0/1  {
	mean nocobert [aw=bc_pesoan] if pobre06==`val'
	matrix MATR  [`i',1]=  e(b)
	matrix MATR  [`i',2]=  `j'
    matrix MATR  [`i',3]=  `anio'
	
	local i  = `i' + 1
	local j  = `j' + 1
	}
    *

	}
    *

**
use "$bases\fusionada_personasyhogares_20.dta", clear
g bc_pe2=e26
global anio 20
	mat def aux_2020 = J(12,1,.)
	
	g bc_pe3=e27
	

g uno=1  						//Variabe auxiliar para construir cantidad de personas
replace uno=0 if e30==14 		//Excluyo de la suma al servicio doméstico

egen bd_ht19 = sum (uno), by (numero) 
label var bd_ht19 "Cantidad de pseronas (sin servicio doméstico)"

g ypc1=ht11/bd_ht19 if e30==1
g quintilesy_prev=.
foreach i of numlist 1/12 {
xtile quintilesy_prev`i'= ypc1 [aw=pesomen] if mes==`i', n(5)
replace quintilesy_prev=quintilesy_prev`i' if mes==`i'
drop quintilesy_prev`i'
}

egen bd_quintilesy= max (quintilesy_prev), by (numero)
label var bd_quintilesy "Quintil de ingreso per cápita del hogar"

drop uno ypc1 quintilesy_prev

rename pobre_06 pobre06
	
	cap drop bd_region
	g bd_region=.
	replace bd_region=1 if region_4== 1 | region_4== 2
	replace bd_region=2 if region_4== 3
	replace bd_region=3 if region_4== 4

	keep if bc_pe3!=.&bc_pe3<18
	g nocobert=1 if e45_cv==7
	replace nocobert=0 if e45_cv!=7
	
	local i  =  197 // (1 + `grupo') * (`canio'-1) 
	
foreach mes of numlist 1/12 {
	mean nocobert if mes==`mes' [aw=pesomen] 
	matrix aux_2020 [`mes',1]=e(b)
}

	matrix MATR  [`i',1]=  (aux_2020[3,1]+aux_2020[4,1]+ /*
						*/ aux_2020[5,1]+aux_2020[6,1]+aux_2020[7,1]+aux_2020[8,1]+ /*
						*/ aux_2020[9,1]+aux_2020[10,1]+aux_2020[11,1]+aux_2020[12,1])/10
	matrix MATR  [`i',2]=  0
	matrix MATR  [`i',3]=  $anio
   	
	local i  = `i' + 1 /*NO se procesa país urbano para no duplicar valores*/
	local j  = 	1

	foreach val of numlist 2/3  {
foreach mes of numlist 1/12 {
	mean nocobert [aw=pesomen] if mes==`mes' & bd_region==`val'
	matrix aux_2020 [`mes',1]=e(b)
}
	matrix MATR  [`i',1]=  (aux_2020[3,1]+aux_2020[4,1]+ /*
						*/ aux_2020[5,1]+aux_2020[6,1]+aux_2020[7,1]+aux_2020[8,1]+ /*
						*/ aux_2020[9,1]+aux_2020[10,1]+aux_2020[11,1]+aux_2020[12,1])/10
	matrix MATR  [`i',2]=  `j'
	matrix MATR  [`i',3]=  $anio
	
	local i  = `i' + 1
	local j  = `j' + 1
    }
    *

	local j  = 	3
	foreach val of numlist 1/2  {
foreach mes of numlist 1/12 {
	mean nocobert [aw=pesomen] if mes==`mes' & e26==`val'
	matrix aux_2020 [`mes',1]=e(b)
}
	matrix MATR  [`i',1]=  (aux_2020[3,1]+aux_2020[4,1]+ /*
						*/ aux_2020[5,1]+aux_2020[6,1]+aux_2020[7,1]+aux_2020[8,1]+ /*
						*/ aux_2020[9,1]+aux_2020[10,1]+aux_2020[11,1]+aux_2020[12,1])/10
	matrix MATR  [`i',2]=  `j'
    matrix MATR  [`i',3]=  $anio
	
	local i  = `i' + 1
	local j  = `j' + 1
	}
    *
	
	local j  = 	5
	foreach val of numlist 1/2  {

foreach mes of numlist 1/12 {
	mean nocobert [aw=pesomen] if mes==`mes' & e29_1==`val'
	matrix aux_2020 [`mes',1]=e(b)
}
	matrix MATR  [`i',1]=  (aux_2020[3,1]+aux_2020[4,1]+ /*
						*/ aux_2020[5,1]+aux_2020[6,1]+aux_2020[7,1]+aux_2020[8,1]+ /*
						*/ aux_2020[9,1]+aux_2020[10,1]+aux_2020[11,1]+aux_2020[12,1])/10

	matrix MATR  [`i',2]=  `j'
    matrix MATR  [`i',3]=  $anio
	
	local i  = `i' + 1
	local j  = `j' + 1
	}
    *
	
	local j  = 	7
	foreach val of numlist 1/5  {
foreach mes of numlist 1/12 {
	mean nocobert [aw=pesomen] if mes==`mes' & bd_quintilesy==`val'
	matrix aux_2020 [`mes',1]=e(b)
}
	matrix MATR  [`i',1]=  (aux_2020[3,1]+aux_2020[4,1]+ /*
						*/ aux_2020[5,1]+aux_2020[6,1]+aux_2020[7,1]+aux_2020[8,1]+ /*
						*/ aux_2020[9,1]+aux_2020[10,1]+aux_2020[11,1]+aux_2020[12,1])/10
	
	matrix MATR  [`i',2]=  `j'
    matrix MATR  [`i',3]=  $anio
	
	local i  = `i' + 1
	local j  = `j' + 1
	}
    *
	
	local j  = 	16
	foreach val of numlist 0/1  {
foreach mes of numlist 1/12 {
	mean nocobert [aw=pesomen] if mes==`mes' & pobre06==`val'
	matrix aux_2020 [`mes',1]=e(b)
}
	matrix MATR  [`i',1]=  (aux_2020[3,1]+aux_2020[4,1]+ /*
						*/ aux_2020[5,1]+aux_2020[6,1]+aux_2020[7,1]+aux_2020[8,1]+ /*
						*/ aux_2020[9,1]+aux_2020[10,1]+aux_2020[11,1]+aux_2020[12,1])/10
	
	matrix MATR  [`i',2]=  `j'
    matrix MATR  [`i',3]=  $anio
	
	local i  = `i' + 1
	local j  = `j' + 1
	}
    *
    *

xml_tab MATR, save("$tabulados\Auxiliares\AUX`codind'_TP.xls") replace

**
/*PROBLEMA: GUARDAR COMO EXCEL 97, STATA NO ABRE CON ESTE COMANDO ARCHIVOS EXCEL 2003*/
**MODIFICACIÓN MANUAL: GUARDAR COMO LA BASE DE DATOS Y EDITAR NOMBRES DE INDICADORES E INFORMACIÓN DE METADATO**
**MODIFICACIÓN MANUAL: ACTUALIZAR LOCALS**

	local i      = 1
	local codind = 563                  // Código de indicador
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

g CODIND                 = 563
g NOMINDICADOR           = "Porcentaje de niños entre 0 y 17 años sin cobertura de salud" 
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

replace URBANORURALUY    = "URBANO (MENOS DE 5.000 HABITANTES)"   if AUXILIAR==1
replace URBANORURALUY    = "RURAL DISPERSO"                       if AUXILIAR==2

replace SEXO    = "VARONES"            if AUXILIAR==3
replace SEXO    = "MUJERES"            if AUXILIAR==4

replace ASCENDENCIA    = "AFRO"        if AUXILIAR==5
replace ASCENDENCIA    = "NO AFRO"     if AUXILIAR==6

replace QUINTIL			 = "QUINTIL 1"							  if AUXILIAR==7
replace QUINTIL			 = "QUINTIL 2"							  if AUXILIAR==8
replace QUINTIL			 = "QUINTIL 3"							  if AUXILIAR==9
replace QUINTIL			 = "QUINTIL 4"							  if AUXILIAR==10
replace QUINTIL			 = "QUINTIL 5"							  if AUXILIAR==11

replace NOMINDICADOR    = NOMINDICADOR+ ". Hogares no en situación de pobreza"    if AUXILIAR==12
replace NOMINDICADOR    = NOMINDICADOR+ ". Hogares en situación de pobreza"       if AUXILIAR==13
 

** 

** 
save "$tabulados\Auxiliares\\`codind'_TP.dta", replace




************************************************************************************************
************************************************************************************************

    *PAÍS URBANO
	local i      = 1
	local codind = 563                  // Código de indicador
	local grupo  = 8                   // Cantidad de grupos (se agrega un grupo para el total)
	local canio  = 30                   // Cantidad de años de la serie

	local filas = `grupo' * `canio'  // Cantidad de grupos más el total por cantidad de años de la serie
	matrix def       MATR= J(`filas',3,.)
	matrix colnames  MATR= VALOR AUXILIAR ANIO 

	foreach anio of numlist $seieURB  {
    use "$bases\Fusionada_personasyhogares_`anio'.dta", clear
	
	keep if bc_pe3!=.&bc_pe3<18
	g nocobert=1 if bc_pe6a==1
	replace nocobert=0 if bc_pe6a!=1

	mean nocobert [aw=bc_pesoan] if bc_filtloc==1
	matrix MATR  [`i',1]=  e(b)
	matrix MATR  [`i',2]=  0
	matrix MATR  [`i',3]=  `anio'
   	
	local i  = `i' + 1
	local j  = 	1

	foreach val of numlist 1/2  { 
	mean nocobert [aw=bc_pesoan] if bc_pe2==`val' & bc_filtloc==1
	matrix MATR  [`i',1]=  e(b)
	matrix MATR  [`i',2]=  `j'
    matrix MATR  [`i',3]=  `anio'
 	
	local i  = `i' + 1
	local j  = `j' + 1
	}
    *
	
	local j  = 	3
	foreach val of numlist 1/5  {
	mean nocobert [aw=bc_pesoan] if bd_quintilesy==`val' & bc_filtloc==1
	matrix MATR  [`i',1]=  e(b)
	matrix MATR  [`i',2]=  `j'
    matrix MATR  [`i',3]=  `anio'
 	
	local i  = `i' + 1
	local j  = `j' + 1
	}
    *
	}
    *

**
use "$bases\fusionada_personasyhogares_20.dta", clear
g bc_pe2=e26
global anio 20
	mat def aux_2020 = J(12,1,.)
	

	g bc_filtloc = region_4<3
	
	rename pobre_06 pobre06
	
	g uno=1  						//Variabe auxiliar para construir cantidad de personas
replace uno=0 if e30==14 		//Excluyo de la suma al servicio doméstico

egen bd_ht19 = sum (uno), by (numero) 
label var bd_ht19 "Cantidad de pseronas (sin servicio doméstico)"

g ypc1=ht11/bd_ht19 if e30==1
g quintilesy_prev=.
foreach i of numlist 1/12 {
xtile quintilesy_prev`i'= ypc1 [aw=pesomen] if mes==`i', n(5)
replace quintilesy_prev=quintilesy_prev`i' if mes==`i'
drop quintilesy_prev`i'
}

egen bd_quintilesy= max (quintilesy_prev), by (numero)
label var bd_quintilesy "Quintil de ingreso per cápita del hogar"

drop uno ypc1 quintilesy_prev

g bc_pe3=e27
	keep if bc_pe3!=.&bc_pe3<18
	g nocobert=1 if e45_cv==7
	replace nocobert=0 if e45_cv!=7

	local i  =  233 // (1 + `grupo') * (`canio'-1) 
	
foreach mes of numlist 1/12 {
	mean nocobert if mes==`mes'&bc_filtloc==1 [aw=pesomen] 
	matrix aux_2020 [`mes',1]=e(b)
}

	matrix MATR  [`i',1]=  (aux_2020[3,1]+aux_2020[4,1]+ /*
						*/ aux_2020[5,1]+aux_2020[6,1]+aux_2020[7,1]+aux_2020[8,1]+ /*
						*/ aux_2020[9,1]+aux_2020[10,1]+aux_2020[11,1]+aux_2020[12,1])/10
	matrix MATR  [`i',2]=  0
	matrix MATR  [`i',3]=  $anio
  	
	local i  = `i' + 1 
	local j  = 	1

	foreach val of numlist 1/2  {
	foreach mes of numlist 1/12 {
	mean nocobert [aw=pesomen] if mes==`mes' & e26==`val'&bc_filtloc==1
	matrix aux_2020 [`mes',1]=e(b)
}
	matrix MATR  [`i',1]=  (aux_2020[3,1]+aux_2020[4,1]+ /*
						*/ aux_2020[5,1]+aux_2020[6,1]+aux_2020[7,1]+aux_2020[8,1]+ /*
						*/ aux_2020[9,1]+aux_2020[10,1]+aux_2020[11,1]+aux_2020[12,1])/10
	matrix MATR  [`i',2]=  `j'
    matrix MATR  [`i',3]=  $anio
	
	local i  = `i' + 1
	local j  = `j' + 1
	}
    *
	
	local j  = 	3
	foreach val of numlist 1/5  {
foreach mes of numlist 1/12 {
	mean nocobert [aw=pesomen] if mes==`mes' & bd_quintilesy==`val'&bc_filtloc==1
	matrix aux_2020 [`mes',1]=e(b)
}
	matrix MATR  [`i',1]=  (aux_2020[3,1]+aux_2020[4,1]+ /*
						*/ aux_2020[5,1]+aux_2020[6,1]+aux_2020[7,1]+aux_2020[8,1]+ /*
						*/ aux_2020[9,1]+aux_2020[10,1]+aux_2020[11,1]+aux_2020[12,1])/10
	
	matrix MATR  [`i',2]=  `j'
    matrix MATR  [`i',3]=  $anio
	
	local i  = `i' + 1
	local j  = `j' + 1
	}
    *
 	*

xml_tab MATR, save("$tabulados\Auxiliares\AUX`codind'_PU.xls") replace

**
/*PROBLEMA: GUARDAR COMO EXCEL 97, STATA NO ABRE CON ESTE COMANDO ARCHIVOS EXCEL 2003*/
**MODIFICACIÓN MANUAL: GUARDAR COMO LA BASE DE DATOS Y EDITAR NOMBRES DE INDICADORES E INFORMACIÓN DE METADATO**
**MODIFICACIÓN MANUAL: ACTUALIZAR LOCALS**

	local i      = 1
	local codind = 563                // Código de indicador
	local grupo  = 8                  // Cantidad de grupos
	local canio  = 31                 // Cantidad de años de la serie

import  excel "$tabulados\Auxiliares\AUX`codind'_PU.xls", sheet("sheet1") firstrow clear
rename ANIO ANIO_ANT
destring ANIO, replace
g ANIO=2000+ANIO_ANT if ANIO_ANT<=20
replace ANIO=1900+ANIO_ANT if ANIO_ANT>80
sort ANIO AUXILIAR 
drop if ANIO==.
drop ANIO_ANT
destring AUXILIAR, replace

g CODIND                 = 563
g NOMINDICADOR           = "Porcentaje de niños entre 0 y 17 años sin cobertura de salud" 
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

replace URBANORURALUY    = "URBANO (MÁS DE 5.000 HABITANTES)"     

replace SEXO    = "VARONES"            if AUXILIAR==1
replace SEXO    = "MUJERES"            if AUXILIAR==2

replace QUINTIL			 = "QUINTIL 1"							  if AUXILIAR==3
replace QUINTIL			 = "QUINTIL 2"							  if AUXILIAR==4
replace QUINTIL			 = "QUINTIL 3"							  if AUXILIAR==5
replace QUINTIL			 = "QUINTIL 4"							  if AUXILIAR==6
replace QUINTIL			 = "QUINTIL 5"							  if AUXILIAR==7
** 

export excel  "$tabulados\Auxiliares\\`codind'_PU.dta", cell(A1) firstrow(varlabels) replace



************************************************************************************************
************************************************************************************************
*FUSIÓN PAÍS URBANO Y TOTAL PAÍS

append using "$tabulados\Auxiliares\\`codind'_TP.dta", force
export excel  "$tabulados\\`codind'.xls", cell(A1) firstrow(varlabels) replace
