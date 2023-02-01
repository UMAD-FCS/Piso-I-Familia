
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

	cd "C:\Users\sharo\Dropbox\UMAD"
	global bases "C:\Users\sharo\Dropbox\UMAD\Bases de datos\Encuesta Continua de Hogares (ECH)\Compatibilizadas IECON\Bases DESCA"
	global tabulados "C:\Users\sharo\Dropbox\UMAD\Sociodemografico\Familia\Tabulados"

/*	cd "C:\Users\sharo\Dropbox\1. Unidad de Métodos y Acceso a Datos\1. Observatorio UMAD"
	global bases "C:\Users\sharo\Dropbox\1. Unidad de Métodos y Acceso a Datos\1. Observatorio UMAD\PISO I_MICRODATOS\ECH\Microdatos\Compatibilizadas Iecon\En uso"
	global tabulados C:\Users\sharo\Dropbox\1. Unidad de Métodos y Acceso a Datos\1. Observatorio UMAD\PISO II_MOTORES\Base web\3. Empleo, salarios y transferencias\Extracciones parciales
*/
	
*======================================================================================*
*              CARACTERÍSTICAS GENERALES DE LA MATRIZ DE EXPORTACAIÓN      
*======================================================================================*

    global seieURB 90/99 0/19   // Años de serie para país urbano
	global seieTOT 6/19         // Años de serie para total país

	  

*======================================================================================*
*                 3.1. Composición y estructura de los hogares     
*======================================================================================*

* INDICADOR: Tasa de actividad de mujeres y varones de 14 a 49 años de edad, sin menores en el hogar
* CÓDIGO:    541
* GRUPOS:    Región / Ascendencia étnico-racial / Tramo de edad / Quintil de ingreso / Pobreza
* FUENTE:    ECH (INE)
* PERÍODO:   1990-2020 País urbano / 2006-2020 Total país 

**
**MODIFICACIÓN MANUAL: CÓDIGOS Y LOCAL J PARA VARIABLES AUXILIARES**

    *TOTAL PAÍS
	local i      = 1
	local codind = 541                  // Código de indicador
	local grupo  = 14                   // Cantidad de grupos
	local canio  = 15                   // Cantidad de años de la serie

	local filas = (`grupo') * `canio' * 2 // Cantidad de grupos más el total por cantidad de años de la serie
	matrix def       MATR= J(`filas',4,.)
	matrix colnames  MATR= VALOR AUXILIAR ANIO TIPO

	foreach anio of numlist $seieTOT  {
    use "$bases\fusionada_personasyhogares_`anio'.dta", clear
	
	g tramo_edad=.
	replace tramo_edad=1 if bc_pe3!=.&bc_pe3<=29
	replace tramo_edad=2 if bc_pe3>=30 & bc_pe3<=49
	
	cap drop bd_region
	g bd_region=.
	replace bd_region=1 if region_4== 1 | region_4== 2
	replace bd_region=2 if region_4== 3
	replace bd_region=3 if region_4== 4

cap drop menor13
g menor13=(bc_pe3<13)
egen menor13H= max(menor13), by (bc_correlat)
drop if menor13H!=0
drop if bc_pe3<14|bc_pe3>49

g activos1=0 if bc_pe2==1
replace activos1 = 1 if bc_pobp>=2 & bc_pobp<=5 & bc_pe2==1
replace activos1 = . if bc_pobp==1 & bc_pe2==1
g activos2=0 if bc_pe2==2
replace activos2 = 1 if bc_pobp>=2 & bc_pobp<=5 & bc_pe2==2
replace activos2 = . if bc_pobp==1 & bc_pe2==2
	
	foreach x of numlist 1/2 {
	mean activos`x' [aw=bc_pesoan] 
	matrix MATR  [`i',1]= e(b)
	matrix MATR  [`i',2]=  0
	matrix MATR  [`i',3]=  `anio'
   	matrix MATR  [`i',4]=  `x'
	
	local i  = `i' + 1 
}
   	
/*NO se procesa país urbano para no duplicar valores*/
	
	local j  = 	1
	foreach val of numlist 2/3  {
	foreach x of numlist 1/2 {
	mean activos`x' [aw=bc_pesoan] if bd_region==`val'
	matrix MATR  [`i',1]=  e(b)
	matrix MATR  [`i',2]=  `j'
	matrix MATR  [`i',3]=  `anio'
	matrix MATR  [`i',4]=  `x'
	
	local i  = `i' + 1
    }
	local j  = `j' + 1
}
    *
	
	local j  = 	3
	foreach val of numlist 1/2  {
	foreach x of numlist 1/2 {
	mean activos`x' [aw=bc_pesoan] if tramo_edad==`val'
	matrix MATR  [`i',1]=  e(b)
	matrix MATR  [`i',2]=  `j'
    matrix MATR  [`i',3]=  `anio'
    matrix MATR  [`i',4]=  `x'
	
	local i  = `i' + 1
	}
	local j  = `j' + 1
	}
    *
	
	local j  = 	5
	foreach val of numlist 1/2  {
	foreach x of numlist 1/2 {
	mean activos`x' [aw=bc_pesoan] if bd_e29_1==`val'
	matrix MATR  [`i',1]=  e(b)
	matrix MATR  [`i',2]=  `j'
    matrix MATR  [`i',3]=  `anio'
    matrix MATR  [`i',4]=  `x'
	
	local i  = `i' + 1
}
	local j  = `j' + 1
	}
    *
	
	local j  = 	7
	foreach val of numlist 1/5  {
	foreach x of numlist 1/2 {
	mean activos`x' [aw=bc_pesoan] if bd_quintilesy==`val'
	matrix MATR  [`i',1]=  e(b)
	matrix MATR  [`i',2]=  `j'
    matrix MATR  [`i',3]=  `anio'
    matrix MATR  [`i',4]=  `x'
	
	local i  = `i' + 1
}
	local j  = `j' + 1
	}
    *
	
	local j  = 	12
	foreach val of numlist 0/1  {
	foreach x of numlist 1/2 {
	mean activos`x' [aw=bc_pesoan] if pobre06==`val'
	matrix MATR  [`i',1]=  e(b)
	matrix MATR  [`i',2]=  `j'
    matrix MATR  [`i',3]=  `anio'
    matrix MATR  [`i',4]=  `x'
	
	local i  = `i' + 1
}
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
	g tramo_edad=.
	replace tramo_edad=1 if bc_pe3!=.&bc_pe3<=29
	replace tramo_edad=2 if bc_pe3>=30 & bc_pe3<=49

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

rename pobpcoac bc_pobpc
cap drop menor13
g menor13=(bc_pe3<13)
egen menor13H= max(menor13), by (bc_correlat)
drop if menor13H!=0
drop if bc_pe3<14|bc_pe3>49

g activos1=0 if bc_pe2==1
replace activos1 = 1 if bc_pobp>=2 & bc_pobp<=5 & bc_pe2==1
replace activos1 = . if bc_pobp==1 & bc_pe2==1
g activos2=0 if bc_pe2==2
replace activos2 = 1 if bc_pobp>=2 & bc_pobp<=5 & bc_pe2==2
replace activos2 = . if bc_pobp==1 & bc_pe2==2
	

rename pobre_06 pobre06
	
	cap drop bd_region
	g bd_region=.
	replace bd_region=1 if region_4== 1 | region_4== 2
	replace bd_region=2 if region_4== 3
	replace bd_region=3 if region_4== 4


	local i  =  393 // (1 + `grupo') * (`canio'-1) 
	
foreach x of numlist 1/2 {
foreach mes of numlist 1/12 {
	mean activos`x' if mes==`mes' [aw=pesomen] 
	matrix aux_2020 [`mes',1]=e(b)
}

	matrix MATR  [`i',1]=  (aux_2020[1,1]+aux_2020[2,1]+aux_2020[3,1]+aux_2020[4,1]+ /*
						*/ aux_2020[5,1]+aux_2020[6,1]+aux_2020[7,1]+aux_2020[8,1]+ /*
						*/ aux_2020[9,1]+aux_2020[10,1]+aux_2020[11,1]+aux_2020[12,1])/12
	matrix MATR  [`i',2]=  0
	matrix MATR  [`i',3]=  $anio
	matrix MATR  [`i',4]=  `x'
   	
	local i  = `i' + 1 /*NO se procesa país urbano para no duplicar valores*/
	local j  = 	1

	foreach val of numlist 2/3  {
foreach mes of numlist 1/12 {
	mean activos`x' [aw=pesomen] if mes==`mes' & bd_region==`val'
	matrix aux_2020 [`mes',1]=e(b)
}
	matrix MATR  [`i',1]=  (aux_2020[1,1]+aux_2020[2,1]+aux_2020[3,1]+aux_2020[4,1]+ /*
						*/ aux_2020[5,1]+aux_2020[6,1]+aux_2020[7,1]+aux_2020[8,1]+ /*
						*/ aux_2020[9,1]+aux_2020[10,1]+aux_2020[11,1]+aux_2020[12,1])/12
	matrix MATR  [`i',2]=  `j'
	matrix MATR  [`i',3]=  $anio
	matrix MATR  [`i',4]=  `x'
	
	local i  = `i' + 1
	local j  = `j' + 1
    }
    *


	local j  = 	3
	foreach val of numlist 1/2  {
foreach mes of numlist 1/12 {
	mean activos`x' [aw=pesomen] if mes==`mes' & tramo_edad==`val'
	matrix aux_2020 [`mes',1]=e(b)
}
	matrix MATR  [`i',1]=  (aux_2020[1,1]+aux_2020[2,1]+aux_2020[3,1]+aux_2020[4,1]+ /*
						*/ aux_2020[5,1]+aux_2020[6,1]+aux_2020[7,1]+aux_2020[8,1]+ /*
						*/ aux_2020[9,1]+aux_2020[10,1]+aux_2020[11,1]+aux_2020[12,1])/12
	matrix MATR  [`i',2]=  `j'
    matrix MATR  [`i',3]=  $anio
    matrix MATR  [`i',4]=  `x'
	
	local i  = `i' + 1
	local j  = `j' + 1
	}
    *
	
	local j  = 	5
	foreach val of numlist 1/2  {

foreach mes of numlist 1/12 {
	mean activos`x' [aw=pesomen] if mes==`mes' & e29_1==`val'
	matrix aux_2020 [`mes',1]=e(b)
}
	matrix MATR  [`i',1]=  (aux_2020[1,1]+aux_2020[2,1]+aux_2020[3,1]+aux_2020[4,1]+ /*
						*/ aux_2020[5,1]+aux_2020[6,1]+aux_2020[7,1]+aux_2020[8,1]+ /*
						*/ aux_2020[9,1]+aux_2020[10,1]+aux_2020[11,1]+aux_2020[12,1])/12

	matrix MATR  [`i',2]=  `j'
    matrix MATR  [`i',3]=  $anio
    matrix MATR  [`i',4]=  `x'
	
	local i  = `i' + 1
	local j  = `j' + 1
	}
    *
	
	local j  = 	7
	foreach val of numlist 1/5  {
foreach mes of numlist 1/12 {
	mean activos`x' [aw=pesomen] if mes==`mes' & bd_quintilesy==`val'
	matrix aux_2020 [`mes',1]=e(b)
}
	matrix MATR  [`i',1]=  (aux_2020[1,1]+aux_2020[2,1]+aux_2020[3,1]+aux_2020[4,1]+ /*
						*/ aux_2020[5,1]+aux_2020[6,1]+aux_2020[7,1]+aux_2020[8,1]+ /*
						*/ aux_2020[9,1]+aux_2020[10,1]+aux_2020[11,1]+aux_2020[12,1])/12
	
	matrix MATR  [`i',2]=  `j'
    matrix MATR  [`i',3]=  $anio
    matrix MATR  [`i',4]=  `x'
	
	local i  = `i' + 1
	local j  = `j' + 1
	}
    *
	
	local j  = 	12
	foreach val of numlist 0/1  {
foreach mes of numlist 1/12 {
	mean activos`x' [aw=pesomen] if mes==`mes' & pobre06==`val'
	matrix aux_2020 [`mes',1]=e(b)
}
	matrix MATR  [`i',1]=  (aux_2020[1,1]+aux_2020[2,1]+aux_2020[3,1]+aux_2020[4,1]+ /*
						*/ aux_2020[5,1]+aux_2020[6,1]+aux_2020[7,1]+aux_2020[8,1]+ /*
						*/ aux_2020[9,1]+aux_2020[10,1]+aux_2020[11,1]+aux_2020[12,1])/12
	
	matrix MATR  [`i',2]=  `j'
    matrix MATR  [`i',3]=  $anio
    matrix MATR  [`i',4]=  `x'
	
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
	local codind = 541                  // Código de indicador
	local grupo  = 18                   // Cantidad de grupos
	local canio  = 15                   // Cantidad de años de la serie


import  excel "$tabulados\Auxiliares\AUX`codind'_TP.xls", firstrow clear
rename ANIO ANIO_ANT
destring ANIO, replace
g ANIO=2000+ANIO_ANT if ANIO_ANT<=20
replace ANIO=1900+ANIO_ANT if ANIO_ANT>80
sort ANIO AUXILIAR TIPO
drop if ANIO==.
drop ANIO_ANT
destring TIPO, replace
destring AUXILIAR, replace

g CODIND                 = 541
g NOMINDICADOR           = "Tasa de participación de mujeres y varones entre 14 y 49 años sin menores en el hogar (Varón)" if TIPO==1
replace NOMINDICADOR     = "Tasa de participación de mujeres y varones entre 14 y 49 años sin menores en el hogar (Mujer)" if TIPO==2
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

replace NOMINDICADOR    = NOMINDICADOR+". Menor de 30 años"  if AUXILIAR==3
replace NOMINDICADOR    = NOMINDICADOR+". Entre 30 y 49 años"  if AUXILIAR==4

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
	local codind = 541                  // Código de indicador
	local grupo  = 8                   // Cantidad de grupos (se agrega un grupo para el total)
	local canio  = 31                   // Cantidad de años de la serie

	local filas = `grupo' * `canio' * 2 // Cantidad de grupos más el total por cantidad de años de la serie
	matrix def       MATR= J(`filas',4,.)
	matrix colnames  MATR= VALOR AUXILIAR ANIO TIPO

	foreach anio of numlist $seieURB  {
    use "$bases\Fusionada_personasyhogares_`anio'.dta", clear
	
	g tramo_edad=.
	replace tramo_edad=1 if bc_pe3!=.&bc_pe3<=29
	replace tramo_edad=2 if bc_pe3>=30 & bc_pe3<=44
	replace tramo_edad=3 if bc_pe3>=45 & bc_pe3<=64
	replace tramo_edad=4 if bc_pe3>=65 
	
cap drop menor13
g menor13=(bc_pe3<13)
egen menor13H= max(menor13), by (bc_correlat)
drop if menor13H!=0
drop if bc_pe3<14|bc_pe3>49

g activos1=0 if bc_pe2==1
replace activos1 = 1 if bc_pobp>=2 & bc_pobp<=5 & bc_pe2==1
replace activos1 = . if bc_pobp==1 & bc_pe2==1
g activos2=0 if bc_pe2==2
replace activos2 = 1 if bc_pobp>=2 & bc_pobp<=5 & bc_pe2==2
replace activos2 = . if bc_pobp==1 & bc_pe2==2

	foreach x of numlist 1/2  { 
	mean activos`x' [aw=bc_pesoan] if bc_filtloc==1
	matrix MATR  [`i',1]=  e(b)
	matrix MATR  [`i',2]=  0
	matrix MATR  [`i',3]=  `anio'
    matrix MATR  [`i',4]=  `x'
   	
	local i  = `i' + 1
}
	local j  = 	1
	foreach x of numlist 1/2  { 
	foreach val of numlist 1/2  {
	mean activos`x' [aw=bc_pesoan] if tramo_edad==`val' & bc_filtloc==1
	matrix MATR  [`i',1]=  e(b)
	matrix MATR  [`i',2]=  `j'
    matrix MATR  [`i',3]=  `anio'
    matrix MATR  [`i',4]=  `x'
	
	local i  = `i' + 1
	}
	local j  = `j' + 1
	}
    *
	
	local j  = 	3
	foreach x of numlist 1/2  { 
	foreach val of numlist 1/5  {
	mean activos`x' [aw=bc_pesoan] if bd_quintilesy==`val' & bc_filtloc==1
	matrix MATR  [`i',1]=  e(b)
	matrix MATR  [`i',2]=  `j'
    matrix MATR  [`i',3]=  `anio'
    matrix MATR  [`i',4]=  `x'
	
	local i  = `i' + 1
	}
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
	g tramo_edad=.
	replace tramo_edad=1 if bc_pe3!=.&bc_pe3<=29
	replace tramo_edad=2 if bc_pe3>=30 & bc_pe3<=49
	
	g bc_filtloc = region_4<3
	
	rename pobre_06 pobre06
	rename pobpcoac bc_pobp
cap drop menor13
g menor13=(bc_pe3<13)
egen menor13H= max(menor13), by (bc_correlat)
drop if menor13H!=0
drop if bc_pe3<14|bc_pe3>49

g activos1=0 if bc_pe2==1
replace activos1 = 1 if bc_pobp>=2 & bc_pobp<=5 & bc_pe2==1
replace activos1 = . if bc_pobp==1 & bc_pe2==1
g activos2=0 if bc_pe2==2
replace activos2 = 1 if bc_pobp>=2 & bc_pobp<=5 & bc_pe2==2
replace activos2 = . if bc_pobp==1 & bc_pe2==2
	
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

	local i  =  481 // (1 + `grupo') * (`canio'-1) 
	
foreach x of numlist 1/2 {
foreach mes of numlist 1/12 {
	mean activos`x' if mes==`mes'& bc_filtloc==1 [aw=pesomen] 
	matrix aux_2020 [`mes',1]=e(b)
}

	matrix MATR  [`i',1]=  (aux_2020[1,1]+aux_2020[2,1]+aux_2020[3,1]+aux_2020[4,1]+ /*
						*/ aux_2020[5,1]+aux_2020[6,1]+aux_2020[7,1]+aux_2020[8,1]+ /*
						*/ aux_2020[9,1]+aux_2020[10,1]+aux_2020[11,1]+aux_2020[12,1])/12
	matrix MATR  [`i',2]=  0
	matrix MATR  [`i',3]=  $anio
	matrix MATR  [`i',4]=  `x'
   	
	local i  = `i' + 1 
	local j  = 	1

	foreach val of numlist 1/2  {
foreach mes of numlist 1/12 {
	mean activos`x' [aw=pesomen] if mes==`mes' & tramo_edad==`val'& bc_filtloc==1
	matrix aux_2020 [`mes',1]=e(b)
}
	matrix MATR  [`i',1]=  (aux_2020[1,1]+aux_2020[2,1]+aux_2020[3,1]+aux_2020[4,1]+ /*
						*/ aux_2020[5,1]+aux_2020[6,1]+aux_2020[7,1]+aux_2020[8,1]+ /*
						*/ aux_2020[9,1]+aux_2020[10,1]+aux_2020[11,1]+aux_2020[12,1])/12
	matrix MATR  [`i',2]=  `j'
    matrix MATR  [`i',3]=  $anio
    matrix MATR  [`i',4]=  `x'
	
	local i  = `i' + 1
	local j  = `j' + 1
	}
    *
	
	local j  = 	3
	foreach val of numlist 1/5  {
foreach mes of numlist 1/12 {
	mean activos`x' [aw=pesomen] if mes==`mes' & bd_quintilesy==`val'& bc_filtloc==1
	matrix aux_2020 [`mes',1]=e(b)
}
	matrix MATR  [`i',1]=  (aux_2020[1,1]+aux_2020[2,1]+aux_2020[3,1]+aux_2020[4,1]+ /*
						*/ aux_2020[5,1]+aux_2020[6,1]+aux_2020[7,1]+aux_2020[8,1]+ /*
						*/ aux_2020[9,1]+aux_2020[10,1]+aux_2020[11,1]+aux_2020[12,1])/12
	
	matrix MATR  [`i',2]=  `j'
    matrix MATR  [`i',3]=  $anio
    matrix MATR  [`i',4]=  `x'
	
	local i  = `i' + 1
	local j  = `j' + 1
	}
    *
}
  	**

xml_tab MATR, save("$tabulados\Auxiliares\AUX`codind'_PU.xls") replace

**
/*PROBLEMA: GUARDAR COMO EXCEL 97, STATA NO ABRE CON ESTE COMANDO ARCHIVOS EXCEL 2003*/
**MODIFICACIÓN MANUAL: GUARDAR COMO LA BASE DE DATOS Y EDITAR NOMBRES DE INDICADORES E INFORMACIÓN DE METADATO**
**MODIFICACIÓN MANUAL: ACTUALIZAR LOCALS**

	local i      = 1
	local codind = 541                // Código de indicador
	local grupo  = 8                  // Cantidad de grupos
	local canio  = 31                 // Cantidad de años de la serie

import  excel "$tabulados\Auxiliares\AUX`codind'_PU.xls", sheet("sheet1") firstrow clear
rename ANIO ANIO_ANT
destring ANIO, replace
g ANIO=2000+ANIO_ANT if ANIO_ANT<=20
replace ANIO=1900+ANIO_ANT if ANIO_ANT>80
sort ANIO AUXILIAR TIPO
drop if ANIO==.
drop ANIO_ANT
destring TIPO, replace
destring AUXILIAR, replace

g CODIND                 = 541
g NOMINDICADOR           = "Tasa de participación de mujeres y varones entre 14 y 49 años sin menores en el hogar (Varón)" if TIPO==1
replace NOMINDICADOR     = "Tasa de participación de mujeres y varones entre 14 y 49 años sin menores en el hogar (Mujer)" if TIPO==2
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

replace NOMINDICADOR    = NOMINDICADOR+". Menor de 30 años"  if AUXILIAR==1
replace NOMINDICADOR    = NOMINDICADOR+". Entre 30 y 49 años"  if AUXILIAR==2

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
