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

	cd "C:\Users\sharo\Dropbox\UMAD"
	global bases "C:\Users\sharo\Dropbox\UMAD\Bases de datos\ENAJ"
	global tabulados "C:\Users\sharo\Dropbox\UMAD\Sociodemografico\Familia\Tabulados"

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

* INDICADOR: Porcentaje de jóvenes y adolescentes, según si se fueron de su hogar de origen 
* CÓDIGO:    518
* GRUPOS:    Región / Sexo 
* FUENTE:    ENAJ
* PERÍODO:   2008, 2013, 2018 

**
**MODIFICACIÓN MANUAL: CÓDIGOS Y LOCAL J PARA VARIABLES AUXILIARES**

    *PAÍS URBANO
	local i      = 1
	local codind = 518                  // Código de indicador
	local grupo  = 5                   // Cantidad de grupos
	local canio  = 3                    // Cantidad de años de la serie

	local filas = (`grupo') * `canio'*3   // Cantidad de grupos más el total por cantidad de años de la serie
	matrix def       MATR= J(`filas',4,.)
	matrix colnames  MATR= VALOR AUXILIAR ANIO SALIDA

    use "$bases\enaj 2008.dta", clear
 
 
	g convive=a1_2==1|a1_3==1|a1_4==1
	replace convive=. if enaj==0
	g salida=1 if a12==1
	replace salida=2 if convive==0
	replace salida=3 if convive==1&a12==2
 	ta salida, g(salida)

	rename region20 aux_region20
	rename regi* region
	
	
	foreach x of numlist 1/3 {
	mean salida`x' [aw=expanj] 
	matrix MATR  [`i',1]= e(b)
	matrix MATR  [`i',2]=  0
	matrix MATR  [`i',3]=  2008
	matrix MATR  [`i',4]=  `x'
	
	local i  = `i' + 1 
}
   	
	local j  = 	1
	foreach val of numlist 1/2  {
	foreach x of numlist 1/3 {
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
	foreach x of numlist 1/3 {
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
	g enaj=sexoentr==1|sexoentr==2
	g convive=a_1_b==1|a_1_c==1|a_1_d==1
	replace convive=. if enaj==0

	g salida=1 if a_17==1
	replace salida=2 if convive==0
	replace salida=3 if convive==1&a_17==2

	ta salida, g(salida)
	
	foreach x of numlist 1/3 {
	mean salida`x' [aw=peso_ena] 
	matrix MATR  [`i',1]= e(b)
	matrix MATR  [`i',2]=  0
	matrix MATR  [`i',3]=  2013
	matrix MATR  [`i',4]=  `x'
	
	local i  = `i' + 1 
}
   	
	local j  = 	1
	foreach val of numlist 1/2  {
	foreach x of numlist 1/3 {
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
	foreach x of numlist 1/3 {
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
	g convive=a1_b==1|a1_c==1|a1_d==1
	g salida=1 if a14==1
	replace salida=2 if convive==0
	replace salida=3 if convive==1&a14==2
	ta salida, g(salida)
	
	g region=1 if region_4==1
	replace region=2 if region_4==2
	
	keep if (region==1|region==2)&pedad>11&pedad<30
	
	foreach x of numlist 1/3 {
	mean salida`x' [aw=peso] 
	matrix MATR  [`i',1]= e(b)
	matrix MATR  [`i',2]=  0
	matrix MATR  [`i',3]=  2018
	matrix MATR  [`i',4]=  `x'
	
	local i  = `i' + 1 
}
   	
	local j  = 	1
	foreach val of numlist 1/2  {
	foreach x of numlist 1/3 {
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
	foreach x of numlist 1/3 {
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
	local codind = 518                  // Código de indicador
	local grupo  = 5                   // Cantidad de grupos
	local canio  = 3                   // Cantidad de años de la serie


import  excel "$tabulados\Auxiliares\AUX`codind'_PU.xls", firstrow clear
destring ANIO, replace
sort ANIO AUXILIAR SALIDA
drop if ANIO==.
destring SALIDA, replace
destring AUXILIAR, replace


g CODIND                 = 518
g NOMINDICADOR           = "Porcentaje de jóvenes y adolescentes que se fueron de su hogar de origen y volvieron" if SALIDA==1 
replace NOMINDICADOR     = "Porcentaje de jóvenes y adolescentes que se fueron de su hogar de origen y no volvieron" if SALIDA==2
replace NOMINDICADOR     = "Porcentaje de jóvenes y adolescentes que nunca se fueron de su hogar de origen" if SALIDA==3 
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
