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

	cd "C:\Users\Shari\Dropbox\UMAD"
	*global bases "C:\Users\Shari\Dropbox\UMAD\Bases de datos\ENAJ"
	global tabulados "C:\Users\Shari\Dropbox\UMAD\Sociodemografico\Familia\Tabulados"

	
	global bases "C:\Users\Shari\Dropbox\UMAD\Líneas de investigación\Redes\Procesamiento\Bases ENAJ"
	
/*	cd "C:\Users\Usuario\Dropbox\1. Unidad de Métodos y Acceso a Datos\1. Observatorio UMAD"
	global bases "C:\Users\Usuario\Dropbox\1. Unidad de Métodos y Acceso a Datos\1. Observatorio UMAD\PISO I_MICRODATOS\ECH\Microdatos\Compatibilizadas Iecon\En uso"
	global tabulados C:\Users\Usuario\Dropbox\1. Unidad de Métodos y Acceso a Datos\1. Observatorio UMAD\PISO II_MOTORES\Base web\3. Empleo, salarios y transferencias\Extracciones parciales
*/
	
*======================================================================================*
*              CARACTERÍSTICAS GENERALES DE LA MATRIZ DE EXPORTACAIÓN      
*======================================================================================*
  

*======================================================================================*
*                 3.1. Composición y estructura de los hogares     
*======================================================================================*

* INDICADOR: Porcentaje de jóvenes y adolescentes que se fueron de su hogar de origen, según con quién se fueron
* CÓDIGO:    520
* GRUPOS:    Región / Sexo 
* FUENTE:    ENAJ
* PERÍODO:   2008, 2013, 2018 

**
**MODIFICACIÓN MANUAL: CÓDIGOS Y LOCAL J PARA VARIABLES AUXILIARES**

    *PAÍS URBANO
	local i      = 1
	local codind = 520                  // Código de indicador
	local grupo  = 5                   // Cantidad de grupos
	local canio  = 3                    // Cantidad de años de la serie

	local filas = (`grupo') * `canio'*5   // Cantidad de grupos más el total por cantidad de años de la serie
	matrix def       MATR= J(`filas',4,.)
	matrix colnames  MATR= VALOR AUXILIAR ANIO QUIEN

    use "$bases\enaj 2008.dta", clear
 
	g salida_q=1 if a14==1|a18==1
	replace salida_q=2 if a14==2|a18==2
	replace salida_q=3 if a14==3|a18==3
	replace salida_q=4 if a14==4|a18==4
	replace salida_q=5 if a14==5|a18==5
	replace salida_q=5 if a14==6|a18==6
	

	ta salida_q, gen(salida)	
	
	rename region20 aux_region20
	rename regi* region
	
	foreach x of numlist 1/5 {
	mean salida`x' [aw=expanj] 
	matrix MATR  [`i',1]= e(b)
	matrix MATR  [`i',2]=  0
	matrix MATR  [`i',3]=  2008
	matrix MATR  [`i',4]=  `x'
	
	local i  = `i' + 1 
}
   	
	local j  = 	1
	foreach val of numlist 1/2  {
	foreach x of numlist 1/5 {
	mean salida`x' [aw=expanj] if region==`val'
	matrix MATR  [`i',1]=  e(b)
	matrix MATR  [`i',2]=  `j'
	matrix MATR  [`i',3]=  2008
	matrix MATR  [`i',4]=  `x'
	
	local i  = `i' + 1
    }
	local j  = `j' + 1
}
    *
	local j  = 	3
	foreach val of numlist 1/2  {
	foreach x of numlist 1/5 {
	mean salida`x' [aw=expanj] if sexo==`val'
	matrix MATR  [`i',1]=  e(b)
	matrix MATR  [`i',2]=  `j'
	matrix MATR  [`i',3]=  2008
	matrix MATR  [`i',4]=  `x'
	
	local i  = `i' + 1
    }
	local j  = `j' + 1
}
    *
	
	
    use "$bases\enaj 2013.dta", clear
 /*
	g ypc=ht11/ht19
	xtile quintil=ypc [w=pesoano] if e30==1, n(5)
	replace quintil=0 if quintil==.
	egen quintily=max(quintil), by(numero)
*/
	g salida_q=1 if a_19==1|a_23==1
	replace salida_q=2 if a_19==2|a_23==2
	replace salida_q=3 if a_19==3|a_23==3
	replace salida_q=4 if a_19==4|a_23==4
	replace salida_q=5 if a_19==5|a_23==5
	replace salida_q=5 if a_19==6|a_23==6
	ta salida_q, gen(salida)	
	
	foreach x of numlist 1/5 {
	mean salida`x' [aw=peso_ena] 
	matrix MATR  [`i',1]= e(b)
	matrix MATR  [`i',2]=  0
	matrix MATR  [`i',3]=  2013
	matrix MATR  [`i',4]=  `x'
	
	local i  = `i' + 1 
}
   	
	local j  = 	1
	foreach val of numlist 1/2  {
	foreach x of numlist 1/5 {
	mean salida`x' [aw=peso_ena] if region_e==`val'
	matrix MATR  [`i',1]=  e(b)
	matrix MATR  [`i',2]=  `j'
	matrix MATR  [`i',3]=  2013
	matrix MATR  [`i',4]=  `x'
	
	local i  = `i' + 1
    }
	local j  = `j' + 1
}
    *
	local j  = 	3
	foreach val of numlist 1/2  {
	foreach x of numlist 1/5 {
	mean salida`x' [aw=peso_ena] if sexoentr==`val'
	matrix MATR  [`i',1]=  e(b)
	matrix MATR  [`i',2]=  `j'
	matrix MATR  [`i',3]=  2013
	matrix MATR  [`i',4]=  `x'
	
	local i  = `i' + 1
    }
	local j  = `j' + 1
}
    *	
	
/*	local j  = 	5
	foreach val of numlist 0/1  {
	foreach x of numlist 1/3 {
	mean salida`x' [aw=peso_ena] if pobre06==`val'
	matrix MATR  [`i',1]=  e(b)
	matrix MATR  [`i',2]=  `j'
	matrix MATR  [`i',3]=  2013
	matrix MATR  [`i',4]=  `x'
	
	local i  = `i' + 1
    }
	local j  = `j' + 1
}
    *	
	local j  = 	7
	foreach val of numlist 1/5  {
	foreach x of numlist 1/3 {
	mean salida`x' [aw=peso_ena] if quintily==`val'
	matrix MATR  [`i',1]=  e(b)
	matrix MATR  [`i',2]=  `j'
	matrix MATR  [`i',3]=  2013
	matrix MATR  [`i',4]=  `x'
	
	local i  = `i' + 1
    }
	local j  = `j' + 1
}
    *	
	
*/	
    use "$bases\enaj 2018.dta", clear
 /*
	g ypc=ht11/ht19
	xtile quintil=ypc [w=pesoano] if e30==1, n(5)
	replace quintil=0 if quintil==.
	egen quintily=max(quintil), by(numero)
*/


	g region=1 if region_4==1
	replace region=2 if region_4==2
	
	keep if (region==1|region==2)&pedad>11&pedad<30


	g salida_q=1 if a17_1==1|a21_1==1
	replace salida_q=2 if a17_2==1|a21_2==1
	replace salida_q=3 if a17_3==1|a21_3==1
	replace salida_q=4 if a17_4==1|a21_4==1
	replace salida_q=5 if a17_5==1|a21_5==1|a17_6==1|a21_6==1
	ta salida_q, gen(salida)	
	

	foreach x of numlist 1/5 {
	mean salida`x' [aw=peso] 
	matrix MATR  [`i',1]= e(b)
	matrix MATR  [`i',2]=  0
	matrix MATR  [`i',3]=  2018
	matrix MATR  [`i',4]=  `x'
	
	local i  = `i' + 1 
}
   	
	local j  = 	1
	foreach val of numlist 1/2  {
	foreach x of numlist 1/5 {
	mean salida`x' [aw=peso] if region==`val'
	matrix MATR  [`i',1]=  e(b)
	matrix MATR  [`i',2]=  `j'
	matrix MATR  [`i',3]=  2018
	matrix MATR  [`i',4]=  `x'
	
	local i  = `i' + 1
    }
	local j  = `j' + 1
}
    *
	local j  = 	3
	foreach val of numlist 1/2  {
	foreach x of numlist 1/5 {
	mean salida`x' [aw=peso] if psexo==`val'
	matrix MATR  [`i',1]=  e(b)
	matrix MATR  [`i',2]=  `j'
	matrix MATR  [`i',3]=  2018
	matrix MATR  [`i',4]=  `x'
	
	local i  = `i' + 1
    }
	local j  = `j' + 1
}
    *	
	
/*	local j  = 	5
	foreach val of numlist 0/1  {
	foreach x of numlist 1/3 {
	mean salida`x' [aw=peso_ena] if pobre06==`val'
	matrix MATR  [`i',1]=  e(b)
	matrix MATR  [`i',2]=  `j'
	matrix MATR  [`i',3]=  2013
	matrix MATR  [`i',4]=  `x'
	
	local i  = `i' + 1
    }
	local j  = `j' + 1
}
    *	
	local j  = 	7
	foreach val of numlist 1/5  {
	foreach x of numlist 1/3 {
	mean salida`x' [aw=peso_ena] if quintily==`val'
	matrix MATR  [`i',1]=  e(b)
	matrix MATR  [`i',2]=  `j'
	matrix MATR  [`i',3]=  2013
	matrix MATR  [`i',4]=  `x'
	
	local i  = `i' + 1
    }
	local j  = `j' + 1
}
  */  

xml_tab MATR, save("$tabulados\Auxiliares\AUX`codind'_PU.xls") replace

**
/*PROBLEMA: GUARDAR COMO EXCEL 97, STATA NO ABRE CON ESTE COMANDO ARCHIVOS EXCEL 2003*/
**MODIFICACIÓN MANUAL: GUARDAR COMO LA BASE DE DATOS Y EDITAR NOMBRES DE INDICADORES E INFORMACIÓN DE METADATO**
**MODIFICACIÓN MANUAL: ACTUALIZAR LOCALS**

	local i      = 1
	local codind = 520                  // Código de indicador
	local grupo  = 5                   // Cantidad de grupos
	local canio  = 3                   // Cantidad de años de la serie


import  excel "$tabulados\Auxiliares\AUX`codind'_PU.xls", firstrow clear
destring ANIO, replace
sort ANIO AUXILIAR QUIEN
drop if ANIO==.
destring QUIEN, replace
destring AUXILIAR, replace

g CODIND                 = 520
g NOMINDICADOR           = "Porcentaje de jóvenes y adolescentes que se fueron de su hogar de origen, según con quién se fueron (Solo)" if QUIEN==1
replace NOMINDICADOR     = "Porcentaje de jóvenes y adolescentes que se fueron de su hogar de origen, según con quién se fueron (Pareja)" if QUIEN==2
replace NOMINDICADOR     = "Porcentaje de jóvenes y adolescentes que se fueron de su hogar de origen, según con quién se fueron (Otros parientes)" if QUIEN==3
replace NOMINDICADOR     = "Porcentaje de jóvenes y adolescentes que se fueron de su hogar de origen, según con quién se fueron (Amigos)" if QUIEN==4
replace NOMINDICADOR     = "Porcentaje de jóvenes y adolescentes que se fueron de su hogar de origen, según con quién se fueron (Otros no parientes o institución)" if QUIEN==5
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

replace URBANORURALUY    = "MONTEVIDEO"   if AUXILIAR==1
replace URBANORURALUY    = "INTERIOR URBANO (MÁS DE 5.000 HABITANTES)"   if AUXILIAR==2

replace SEXO = "VARONES"   if AUXILIAR==3
replace SEXO = "MUJERES"   if AUXILIAR==4


** 
save "$tabulados\Auxiliares\\`codind'_PU.dta", replace




************************************************************************************************
export excel  "$tabulados\\`codind'.xls", cell(A1) firstrow(varlabels) replace
