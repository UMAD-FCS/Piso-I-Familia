
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
*                 3.1. Composición y estructura de los hogares     
*======================================================================================*

* INDICADOR: Distribución porcentual de los hogares según cantidad de integrantes  
* CÓDIGO:    515
* GRUPOS:    Región / Ascendencia étnico-racial del jefe/a / Sexo del jefe/a / Tramo de edad del jefe/a / Quintil de ingreso / Pobreza
* FUENTE:    ECH (INE)
* PERÍODO:   1990-2020 País urbano / 2006-2020 Total país 

**
**MODIFICACIÓN MANUAL: CÓDIGOS Y LOCAL J PARA VARIABLES AUXILIARES**

    *TOTAL PAÍS
	local i      = 1
	local codind = 515                  // Código de indicador
	local grupo  = 18                   // Cantidad de grupos
	local canio  = 15                   // Cantidad de años de la serie

	local filas = (`grupo') * `canio' * 5 // Cantidad de grupos más el total por cantidad de años de la serie
	matrix def       MATR= J(`filas',4,.)
	matrix colnames  MATR= VALOR AUXILIAR ANIO NINT

	foreach anio of numlist $seieTOT  {
    use "$bases\fusionada_personasyhogares_`anio'.dta", clear
	
	g tramo_edad=.
	replace tramo_edad=1 if bc_pe3!=.&bc_pe3<=29
	replace tramo_edad=2 if bc_pe3>=30 & bc_pe3<=44
	replace tramo_edad=3 if bc_pe3>=45 & bc_pe3<=64
	replace tramo_edad=4 if bc_pe3>=65 
	
	cap drop bd_region
	g bd_region=.
	replace bd_region=1 if region_4== 1 | region_4== 2
	replace bd_region=2 if region_4== 3
	replace bd_region=3 if region_4== 4

	g identif=bc_pe4!=7
	egen nint=sum(identif), by(bc_correlat)
	replace nint=5 if nint>=5
	ta nint, gen(nint)
	
	foreach x of numlist 1/5 {
	mean nint`x' [aw=bc_pesoan] if bc_pe4==1
	matrix MATR  [`i',1]= e(b)
	matrix MATR  [`i',2]=  0
	matrix MATR  [`i',3]=  `anio'
   	matrix MATR  [`i',4]=  `x'
	
	local i  = `i' + 1 
}
   	
/*NO se procesa país urbano para no duplicar valores*/
	
	local j  = 	1
	foreach val of numlist 2/3  {
	foreach x of numlist 1/5 {
	mean nint`x' [aw=bc_pesoan] if bd_region==`val'&bc_pe4==1
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
	foreach x of numlist 1/5 {
	mean nint`x' [aw=bc_pesoan] if bc_pe2==`val'&bc_pe4==1
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
	foreach val of numlist 1/4  {
	foreach x of numlist 1/5 {
	mean nint`x' [aw=bc_pesoan] if tramo_edad==`val'&bc_pe4==1
	matrix MATR  [`i',1]=  e(b)
	matrix MATR  [`i',2]=  `j'
    matrix MATR  [`i',3]=  `anio'
    matrix MATR  [`i',4]=  `x'
	
	local i  = `i' + 1
	}
	local j  = `j' + 1
	}
    *
	
	local j  = 	9
	foreach val of numlist 1/2  {
	foreach x of numlist 1/5 {
	mean nint`x' [aw=bc_pesoan] if bd_e29_1==`val'&bc_pe4==1
	matrix MATR  [`i',1]=  e(b)
	matrix MATR  [`i',2]=  `j'
    matrix MATR  [`i',3]=  `anio'
    matrix MATR  [`i',4]=  `x'
	
	local i  = `i' + 1
}
	local j  = `j' + 1
	}
    *
	
	local j  = 	11
	foreach val of numlist 1/5  {
	foreach x of numlist 1/5 {
	mean nint`x' [aw=bc_pesoan] if bd_quintilesy==`val'&bc_pe4==1
	matrix MATR  [`i',1]=  e(b)
	matrix MATR  [`i',2]=  `j'
    matrix MATR  [`i',3]=  `anio'
    matrix MATR  [`i',4]=  `x'
	
	local i  = `i' + 1
}
	local j  = `j' + 1
	}
    *
	
	local j  = 	16
	foreach val of numlist 0/1  {
	foreach x of numlist 1/5 {
	mean nint`x' [aw=bc_pesoan] if pobre06==`val'&bc_pe4==1
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
	replace tramo_edad=2 if bc_pe3>=30 & bc_pe3<=44
	replace tramo_edad=3 if bc_pe3>=45 & bc_pe3<=64
	replace tramo_edad=4 if bc_pe3>=65 
	
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

	g bc_pe4=1 if e30==1
	replace bc_pe4=2 if e30==2
	replace bc_pe4=3 if e30==3|e30==4|e30==5
	replace bc_pe4=4 if e30==6|e30==7
	replace bc_pe4=5 if e30==8|e30==9|e30==10|e30==11|e30==12
	replace bc_pe4=6 if e30==13
	replace bc_pe4=7 if e30==14
	g identif=bc_pe4!=7
	egen nint=sum(identif), by(bc_correlat)
	replace nint=5 if nint>=5
	ta nint, gen(nint)
	
	
	local i  =  1261 // (1 + `grupo') * (`canio'-1) 
	
foreach x of numlist 1/5 {
foreach mes of numlist 1/12 {
	mean nint`x' if mes==`mes'&bc_pe4==1 [aw=pesomen] 
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
	mean nint`x' [aw=pesomen] if mes==`mes' & bd_region==`val'&bc_pe4==1
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
	mean nint`x' [aw=pesomen] if mes==`mes' & e26==`val'&bc_pe4==1
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
	foreach val of numlist 1/4  {
foreach mes of numlist 1/12 {
	mean nint`x' [aw=pesomen] if mes==`mes' & tramo_edad==`val'&bc_pe4==1
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
	
	local j  = 	9
	foreach val of numlist 1/2  {

foreach mes of numlist 1/12 {
	mean nint`x' [aw=pesomen] if mes==`mes' & e29_1==`val'&bc_pe4==1
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
	
	local j  = 	11
	foreach val of numlist 1/5  {
foreach mes of numlist 1/12 {
	mean nint`x' [aw=pesomen] if mes==`mes' & bd_quintilesy==`val'&bc_pe4==1
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
	
	local j  = 	16
	foreach val of numlist 0/1  {
foreach mes of numlist 1/12 {
	mean nint`x' [aw=pesomen] if mes==`mes' & pobre06==`val'&bc_pe4==1
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
*
xml_tab MATR, save("$tabulados\Auxiliares\AUX`codind'_TP.xls") replace

**
/*PROBLEMA: GUARDAR COMO EXCEL 97, STATA NO ABRE CON ESTE COMANDO ARCHIVOS EXCEL 2003*/
**MODIFICACIÓN MANUAL: GUARDAR COMO LA BASE DE DATOS Y EDITAR NOMBRES DE INDICADORES E INFORMACIÓN DE METADATO**
**MODIFICACIÓN MANUAL: ACTUALIZAR LOCALS**

	local i      = 1
	local codind = 515                  // Código de indicador
	local grupo  = 18                   // Cantidad de grupos
	local canio  = 15                   // Cantidad de años de la serie


import  excel "$tabulados\Auxiliares\AUX`codind'_TP.xls", firstrow clear
rename ANIO ANIO_ANT
destring ANIO, replace
g ANIO=2000+ANIO_ANT if ANIO_ANT<=20
replace ANIO=1900+ANIO_ANT if ANIO_ANT>80
sort ANIO AUXILIAR NINT
drop if ANIO==.
drop ANIO_ANT
destring NINT, replace
destring AUXILIAR, replace

g CODIND                 = 515
g NOMINDICADOR           = "Distribución porcentual de los hogares según cantidad de integrantes (1 persona)" if NINT==1
replace NOMINDICADOR     = "Distribución porcentual de los hogares según cantidad de integrantes (2 personas)" if NINT==2
replace NOMINDICADOR     = "Distribución porcentual de los hogares según cantidad de integrantes (3 personas)" if NINT==3
replace NOMINDICADOR     = "Distribución porcentual de los hogares según cantidad de integrantes (4 personas)" if NINT==4
replace NOMINDICADOR     = "Distribución porcentual de los hogares según cantidad de integrantes (5 o más personas)" if NINT==5
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

replace NOMINDICADOR    = NOMINDICADOR+". Menor de 30 años"  if AUXILIAR==5
replace NOMINDICADOR    = NOMINDICADOR+". Entre 30 y 44 años"  if AUXILIAR==6
replace NOMINDICADOR    = NOMINDICADOR+". Entre 45 y 64 años"   if AUXILIAR==7
replace NOMINDICADOR    = NOMINDICADOR+". 65 años o más"  if AUXILIAR==8

replace ASCENDENCIA    = "AFRO"        if AUXILIAR==9
replace ASCENDENCIA    = "NO AFRO"     if AUXILIAR==10

replace QUINTIL			 = "QUINTIL 1"							  if AUXILIAR==11
replace QUINTIL			 = "QUINTIL 2"							  if AUXILIAR==12
replace QUINTIL			 = "QUINTIL 3"							  if AUXILIAR==13
replace QUINTIL			 = "QUINTIL 4"							  if AUXILIAR==14
replace QUINTIL			 = "QUINTIL 5"							  if AUXILIAR==15

replace NOMINDICADOR    = NOMINDICADOR+ ". Hogares no en situación de pobreza"    if AUXILIAR==16
replace NOMINDICADOR    = NOMINDICADOR+ ". Hogares en situación de pobreza"       if AUXILIAR==17
 

** 

** 
save "$tabulados\Auxiliares\\`codind'_TP.dta", replace




************************************************************************************************
************************************************************************************************

    *PAÍS URBANO
	local i      = 1
	local codind = 515                  // Código de indicador
	local grupo  = 12                   // Cantidad de grupos (se agrega un grupo para el total)
	local canio  = 31                   // Cantidad de años de la serie

	local filas = `grupo' * `canio' * 5 // Cantidad de grupos más el total por cantidad de años de la serie
	matrix def       MATR= J(`filas',4,.)
	matrix colnames  MATR= VALOR AUXILIAR ANIO NINT

	foreach anio of numlist $seieURB  {
    use "$bases\Fusionada_personasyhogares_`anio'.dta", clear
	
	g tramo_edad=.
	replace tramo_edad=1 if bc_pe3!=.&bc_pe3<=29
	replace tramo_edad=2 if bc_pe3>=30 & bc_pe3<=44
	replace tramo_edad=3 if bc_pe3>=45 & bc_pe3<=64
	replace tramo_edad=4 if bc_pe3>=65 

	g identif=bc_pe4!=7
	egen nint=sum(identif), by(bc_correlat)
	replace nint=5 if nint>=5
	ta nint, gen(nint)

	foreach x of numlist 1/5  { 
	mean nint`x' [aw=bc_pesoan] if bc_filtloc==1&bc_pe4==1
	matrix MATR  [`i',1]=  e(b)
	matrix MATR  [`i',2]=  0
	matrix MATR  [`i',3]=  `anio'
    matrix MATR  [`i',4]=  `x'
   	
	local i  = `i' + 1
}
	local j  = 	1
	foreach x of numlist 1/5  { 
	foreach val of numlist 1/2  { 
	mean nint`x' [aw=bc_pesoan] if bc_pe2==`val' & bc_filtloc==1&bc_pe4==1
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
	foreach x of numlist 1/5  { 
	foreach val of numlist 1/4  {
	mean nint`x' [aw=bc_pesoan] if tramo_edad==`val' & bc_filtloc==1&bc_pe4==1
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
	foreach x of numlist 1/5  { 
	foreach val of numlist 1/5  {
	mean nint`x' [aw=bc_pesoan] if bd_quintilesy==`val' & bc_filtloc==1&bc_pe4==1
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
	replace tramo_edad=2 if bc_pe3>=30 & bc_pe3<=44
	replace tramo_edad=3 if bc_pe3>=45 & bc_pe3<=64
	replace tramo_edad=4 if bc_pe3>=65 
	
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

	g bc_pe4=1 if e30==1
	replace bc_pe4=2 if e30==2
	replace bc_pe4=3 if e30==3|e30==4|e30==5
	replace bc_pe4=4 if e30==6|e30==7
	replace bc_pe4=5 if e30==8|e30==9|e30==10|e30==11|e30==12
	replace bc_pe4=6 if e30==13
	replace bc_pe4=7 if e30==14
	g identif=bc_pe4!=7
	egen nint=sum(identif), by(bc_correlat)
	replace nint=5 if nint>=5
	ta nint, gen(nint)



	local i  =  1801 // (1 + `grupo') * (`canio'-1) 
	
foreach x of numlist 1/5 {
foreach mes of numlist 1/12 {
	mean nint`x' if mes==`mes'&bc_pe4==1& bc_filtloc==1 [aw=pesomen] 
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
	mean nint`x' [aw=pesomen] if mes==`mes' & e26==`val'&bc_pe4==1& bc_filtloc==1
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
	foreach val of numlist 1/4  {
foreach mes of numlist 1/12 {
	mean nint`x' [aw=pesomen] if mes==`mes' & tramo_edad==`val'&bc_pe4==1& bc_filtloc==1
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
	mean nint`x' [aw=pesomen] if mes==`mes' & bd_quintilesy==`val'&bc_pe4==1& bc_filtloc==1
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
*
xml_tab MATR, save("$tabulados\Auxiliares\AUX`codind'_PU.xls") replace

**
/*PROBLEMA: GUARDAR COMO EXCEL 97, STATA NO ABRE CON ESTE COMANDO ARCHIVOS EXCEL 2003*/
**MODIFICACIÓN MANUAL: GUARDAR COMO LA BASE DE DATOS Y EDITAR NOMBRES DE INDICADORES E INFORMACIÓN DE METADATO**
**MODIFICACIÓN MANUAL: ACTUALIZAR LOCALS**

	local i      = 1
	local codind = 515                // Código de indicador
	local grupo  = 12                  // Cantidad de grupos
	local canio  = 31                 // Cantidad de años de la serie

import  excel "$tabulados\Auxiliares\AUX`codind'_PU.xls", sheet("sheet1") firstrow clear
rename ANIO ANIO_ANT
destring ANIO, replace
g ANIO=2000+ANIO_ANT if ANIO_ANT<=20
replace ANIO=1900+ANIO_ANT if ANIO_ANT>80
sort ANIO AUXILIAR NINT
drop if ANIO==.
drop ANIO_ANT
destring NINT, replace
destring AUXILIAR, replace

g CODIND                 = 515
g NOMINDICADOR           = "Distribución porcentual de los hogares según cantidad de integrantes (1 persona)" if NINT==1
replace NOMINDICADOR     = "Distribución porcentual de los hogares según cantidad de integrantes (2 personas)" if NINT==2
replace NOMINDICADOR     = "Distribución porcentual de los hogares según cantidad de integrantes (3 personas)" if NINT==3
replace NOMINDICADOR     = "Distribución porcentual de los hogares según cantidad de integrantes (4 personas)" if NINT==4
replace NOMINDICADOR     = "Distribución porcentual de los hogares según cantidad de integrantes (5 o más personas)" if NINT==5
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

replace NOMINDICADOR    = NOMINDICADOR+". Menor de 30 años"  if AUXILIAR==3
replace NOMINDICADOR    = NOMINDICADOR+". Entre 30 y 44 años"  if AUXILIAR==4
replace NOMINDICADOR    = NOMINDICADOR+". Entre 45 y 64 años"   if AUXILIAR==5
replace NOMINDICADOR    = NOMINDICADOR+". 65 años o más"  if AUXILIAR==6

replace QUINTIL			 = "QUINTIL 1"							  if AUXILIAR==7
replace QUINTIL			 = "QUINTIL 2"							  if AUXILIAR==8
replace QUINTIL			 = "QUINTIL 3"							  if AUXILIAR==9
replace QUINTIL			 = "QUINTIL 4"							  if AUXILIAR==10
replace QUINTIL			 = "QUINTIL 5"							  if AUXILIAR==11
** 

export excel  "$tabulados\Auxiliares\\`codind'_PU.dta", cell(A1) firstrow(varlabels) replace



************************************************************************************************
************************************************************************************************
*FUSIÓN PAÍS URBANO Y TOTAL PAÍS

append using "$tabulados\Auxiliares\\`codind'_TP.dta", force
export excel  "$tabulados\\`codind'.xls", cell(A1) firstrow(varlabels) replace
