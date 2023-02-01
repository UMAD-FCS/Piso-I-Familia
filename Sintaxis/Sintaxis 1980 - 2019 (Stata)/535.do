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

* INDICADOR: Prevalencia de violencia basada en género por parte de la familia en la infancia
* CÓDIGO:    535
* GRUPOS:    Nivel socioeconómico / Tramo edad / Ascendencia étnico-racial / Nivel educativo
* FUENTE:    ENVBGG
* PERÍODO:   2013, 2019

**
**MODIFICACIÓN MANUAL: CÓDIGOS Y LOCAL J PARA VARIABLES AUXILIARES**

    *PAÍS URBANO
	local i      = 1
	local codind = 535                  // Código de indicador
	local grupo  = 15                   // Cantidad de grupos
	local canio  = 2                    // Cantidad de años de la serie

	local filas = (`grupo') * `canio'*4   // Cantidad de grupos más el total por cantidad de años de la serie
	matrix def       MATR= J(`filas',4,.)
	matrix colnames  MATR= VALOR AUXILIAR ANIO TIPO


        use "$bases\EVBGG_2013.dta", clear
drop if seleccio==0
*NSE (1:bajo, 2: medio, 3: alto)
rename INSE NSE
*afro (1 afro, 2 no afro)
g afro=1 if f9_1==1
replace afro=2 if f9_1==2
*tramo (1: 15 a 18, 2: 19 a 29, 3: 30 a 49, 4: 50 a 64, 5: 65 o +)
g tramo=1 if edad>200&edad<203 
replace tramo=2 if edad==203
replace tramo=3 if edad==204
replace tramo=4 if edad==205
*nivel_edu_sig (1: primaria, 2: secundaria, 3: terciaria)
rename Nivel_edu nivel_edu_sig
*region (1: montevideo, 2: interior urbano)
recode region (1=2) (3=2) (4=2) (5=1)


*======================================================================================*
*                           PREVALENCIA      
*======================================================================================*

*Indicador:	Porcentaje de mujeres de 15 años o más que vivieron situaciones de VBG en la infancia por parte de la familia según tipo de violencia.
recode VT_infancia (0=.) (2=0)
recode VS_infancia (0=.) (2=0)
recode VF_infancia (0=.) (2=0)
recode VP_infancia (0=.) (2=0)

lab def dicotomica 0 "No" 1 "Si"
lab val VT_infancia dicotomica 
lab val VF_infancia dicotomica 
lab val VS_infancia dicotomica 
lab val VP_infancia dicotomica 

rename VT_infancia infancia1
rename VP_infancia infancia2
rename VS_infancia infancia3
rename VF_infancia infancia4


	foreach x of numlist 1/4 {
	mean infancia`x' [aw=ponderador] 
	matrix MATR  [`i',1]= e(b)
	matrix MATR  [`i',2]=  0
	matrix MATR  [`i',3]=  2013
	matrix MATR  [`i',4]=  `x'
	
	local i  = `i' + 1 
}
   	
	local j  = 	1
	foreach val of numlist 1/2  {
	foreach x of numlist 1/4 {
	mean infancia`x' [aw=ponderador] if region==`val'
	matrix MATR  [`i',1]=  e(b)
	matrix MATR  [`i',2]=  `j'
	matrix MATR  [`i',3]=  2013
	matrix MATR  [`i',4]=  `x'
	
	local i  = `i' + 1
    }
	local j  = `j' + 1
}

	
	local j  = 	3
	foreach val of numlist 1/3  {
	foreach x of numlist 1/4 {
	mean infancia`x' [aw=ponderador] if NSE==`val'
	matrix MATR  [`i',1]=  e(b)
	matrix MATR  [`i',2]=  `j'
	matrix MATR  [`i',3]=  2013
	matrix MATR  [`i',4]=  `x'
	
	local i  = `i' + 1
    }
	local j  = `j' + 1
}
    *
	local j  = 	6
	foreach val of numlist 1/2  {
	foreach x of numlist 1/4 {
	mean infancia`x' [aw=ponderador] if afro==`val'
	matrix MATR  [`i',1]=  e(b)
	matrix MATR  [`i',2]=  `j'
	matrix MATR  [`i',3]=  2013
	matrix MATR  [`i',4]=  `x'
	
	local i  = `i' + 1
    }
	local j  = `j' + 1
}
	local j  = 	8
	foreach val of numlist 1/3  {
	foreach x of numlist 1/4 {
	mean infancia`x' [aw=ponderador] if nivel_edu_sig==`val'
	matrix MATR  [`i',1]=  e(b)
	matrix MATR  [`i',2]=  `j'
	matrix MATR  [`i',3]=  2013
	matrix MATR  [`i',4]=  `x'
	
	local i  = `i' + 1
    }
	local j  = `j' + 1
}
    *
	local j  = 	11
	foreach val of numlist 1/4  {
	foreach x of numlist 1/4 {
	mean infancia`x' [aw=ponderador] if tramo==`val'
	matrix MATR  [`i',1]=  e(b)
	matrix MATR  [`i',2]=  `j'
	matrix MATR  [`i',3]=  2013
	matrix MATR  [`i',4]=  `x'
	
	local i  = `i' + 1
    }
	local j  = `j' + 1
}
    *

	
	
	
	use "$bases\EVBGG_2019.dta", clear

*NSE (1:bajo, 2: medio, 3: alto)
*afro (1 afro, 2 no afro)
*tramo (1: 15 a 18, 2: 19 a 29, 3: 30 a 49, 4: 50 a 64, 5: 65 o +)
recode tramo (2=1) (3=2) (4=3) (5=4)
*nivel_edu_sig (1: primaria, 2: secundaria, 3: terciaria)
*region (1: montevideo, 2: interior urbano)
drop if region==5
recode region (3 4=2)

*======================================================================================*
*                           PREVALENCIA      
*======================================================================================*

*Indicador:	Porcentaje de mujeres de 15 años o más que vivieron situaciones de VBG en la infancia por parte de la familia según tipo de violencia.
recode VT_infancia (0=.) (2=0)
recode VS_infancia (0=.) (2=0)
recode VF_infancia (0=.) (2=0)
recode VP_infancia (0=.) (2=0)

lab def dicotomica 0 "No" 1 "Si"
lab val VT_infancia dicotomica 
lab val VF_infancia dicotomica 
lab val VS_infancia dicotomica 
lab val VP_infancia dicotomica 

rename VT_infancia infancia1
rename VP_infancia infancia2
rename VS_infancia infancia3
rename VF_infancia infancia4

	
	foreach x of numlist 1/4 {
	mean infancia`x' [aw=ponderador] 
	matrix MATR  [`i',1]= e(b)
	matrix MATR  [`i',2]=  0
	matrix MATR  [`i',3]=  2019
	matrix MATR  [`i',4]=  `x'
	
	local i  = `i' + 1 
}
   	
	local j  = 	1
	foreach val of numlist 1/2  {
	foreach x of numlist 1/4 {
	mean infancia`x' [aw=ponderador] if region==`val'
	matrix MATR  [`i',1]=  e(b)
	matrix MATR  [`i',2]=  `j'
	matrix MATR  [`i',3]=  2019
	matrix MATR  [`i',4]=  `x'
	
	local i  = `i' + 1
    }
	local j  = `j' + 1
}
    *

	local j  = 	3
	foreach val of numlist 1/3  {
	foreach x of numlist 1/4 {
	mean infancia`x' [aw=ponderador] if NSE==`val'
	matrix MATR  [`i',1]=  e(b)
	matrix MATR  [`i',2]=  `j'
	matrix MATR  [`i',3]=  2019
	matrix MATR  [`i',4]=  `x'
	
	local i  = `i' + 1
    }
	local j  = `j' + 1
}
    *
	local j  = 	6
	foreach val of numlist 1/2  {
	foreach x of numlist 1/4 {
	mean infancia`x' [aw=ponderador] if afro==`val'
	matrix MATR  [`i',1]=  e(b)
	matrix MATR  [`i',2]=  `j'
	matrix MATR  [`i',3]=  2019
	matrix MATR  [`i',4]=  `x'
	
	local i  = `i' + 1
    }
	local j  = `j' + 1
}
	local j  = 	8
	foreach val of numlist 1/3  {
	foreach x of numlist 1/4 {
	mean infancia`x' [aw=ponderador] if nivel_edu_sig==`val'
	matrix MATR  [`i',1]=  e(b)
	matrix MATR  [`i',2]=  `j'
	matrix MATR  [`i',3]=  2019
	matrix MATR  [`i',4]=  `x'
	
	local i  = `i' + 1
    }
	local j  = `j' + 1
}
    *
	local j  = 	11
	foreach val of numlist 1/4  {
	foreach x of numlist 1/4 {
	mean infancia`x' [aw=ponderador] if tramo==`val'
	matrix MATR  [`i',1]=  e(b)
	matrix MATR  [`i',2]=  `j'
	matrix MATR  [`i',3]=  2019
	matrix MATR  [`i',4]=  `x'
	
	local i  = `i' + 1
    }
	local j  = `j' + 1
}
    *

xml_tab MATR, save("$tabulados\Auxiliares\AUX`codind'_PU.xls") replace
A
**
/*PROBLEMA: GUARDAR COMO EXCEL 97, STATA NO ABRE CON ESTE COMANDO ARCHIVOS EXCEL 2003*/
**MODIFICACIÓN MANUAL: GUARDAR COMO LA BASE DE DATOS Y EDITAR NOMBRES DE INDICADORES E INFORMACIÓN DE METADATO**
**MODIFICACIÓN MANUAL: ACTUALIZAR LOCALS**

	local i      = 1
	local codind = 535                  // Código de indicador
	local grupo  = 15                   // Cantidad de grupos
	local canio  = 2                   // Cantidad de años de la serie


import  excel "$tabulados\Auxiliares\AUX`codind'_PU.xls", firstrow clear
destring ANIO, replace
sort ANIO AUXILIAR TIPO
drop if ANIO==.
destring TIPO, replace
destring AUXILIAR, replace

g CODIND                 = 535
g NOMINDICADOR           = "Prevalencia de violencia basada en género por parte de la familia en la infancia (Total)" if TIPO==1
replace NOMINDICADOR     = "Prevalencia de violencia basada en género por parte de la familia en la infancia (Psicológica)" if TIPO==2
replace NOMINDICADOR     = "Prevalencia de violencia basada en género por parte de la familia en la infancia (Sexual)" if TIPO==3
replace NOMINDICADOR     = "Prevalencia de violencia basada en género por parte de la familia en la infancia (Física)" if TIPO==4
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
**MODIFICACIÓN MANUAL: CÓDIGOS Y TÍTULOS DE LA VARIABLES AUXILIARES CORRESPONDIENTES**

replace URBANORURALUY = "Montevideo"    if AUXILIAR==1
replace URBANORURALUY = "Interior urbano"   if AUXILIAR==2

replace NSE  = "Bajo"    if AUXILIAR==3
replace NSE  = "Medio"   if AUXILIAR==4
replace NSE  = "Alto"    if AUXILIAR==5

replace ASCENDENCIA = "Afro"   if AUXILIAR==6
replace ASCENDENCIA = "No afro"   if AUXILIAR==7

replace NIVELEDU = "Primaria"   if AUXILIAR==8
replace NIVELEDU = "Secundaria"   if AUXILIAR==9
replace NIVELEDU = "Terciaria"   if AUXILIAR==10

replace NOMINDICADOR = NOMINDICADOR + ". Entre 15 y 29 años."   if AUXILIAR==11
replace NOMINDICADOR = NOMINDICADOR + ". Entre 30 y 49 años."   if AUXILIAR==12
replace NOMINDICADOR = NOMINDICADOR + ". Entre 50 y 64 años."     if AUXILIAR==13
replace NOMINDICADOR = NOMINDICADOR + ". Mayor de 64 años."     if AUXILIAR==14


** 
save "$tabulados\Auxiliares\\`codind'_PU.dta", replace




************************************************************************************************
export excel  "$tabulados\\`codind'.xls", cell(A1) firstrow(varlabels) replace
