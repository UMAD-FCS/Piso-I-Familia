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
	global bases "C:\Users\Usuario\Dropbox\UMAD\Bases de datos\ENAJ"
	global tabulados "C:\Users\Usuario\Dropbox\UMAD\Sociodemografico\Familia\Tabulados"

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

* INDICADOR: Porcentaje de jóvenes y adolescentes que se fueron de su hogar de origen, según edad de salida por primera vez del hogar de origen 
* CÓDIGO:    519
* GRUPOS:    Región / Sexo 
* FUENTE:    ENAJ
* PERÍODO:   2008, 2013, 2018 

**
**MODIFICACIÓN MANUAL: CÓDIGOS Y LOCAL J PARA VARIABLES AUXILIARES**

    *PAÍS URBANO
	local i      = 1
	local codind = 519                  // Código de indicador
	local grupo  = 5                   // Cantidad de grupos
	local canio  = 3                    // Cantidad de años de la serie

	local filas = (`grupo') * `canio'*3   // Cantidad de grupos más el total por cantidad de años de la serie
	matrix def       MATR= J(`filas',4,.)
	matrix colnames  MATR= VALOR AUXILIAR ANIO EDAD

    use "$bases\enaj 2008.dta", clear
 
	g edad_salida=1 if (a13>0&a13<18)|(a17>0&a17<18)
	replace edad_salida=2 if (a13>17&a13<21)|(a17>17&a17<21)
	replace edad_salida=3 if (a13>20&a13<30)|(a17>20&a17<30)

	ta edad_salida, gen(edad)	
	
	rename region20 aux_region20
	rename regi* region
	
	foreach x of numlist 1/3 {
	mean edad`x' [aw=expanj] 
	matrix MATR  [`i',1]= e(b)
	matrix MATR  [`i',2]=  0
	matrix MATR  [`i',3]=  2008
	matrix MATR  [`i',4]=  `x'
	
	local i  = `i' + 1 
}
   	
	local j  = 	1
	foreach val of numlist 1/2  {
	foreach x of numlist 1/3 {
	mean edad`x' [aw=expanj] if region==`val'
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
	mean edad`x' [aw=expanj] if sexo==`val'
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
	g edad_salida=1 if (a_18>0&a_18<18)|(a_22>0&a_22<18)
	replace edad_salida=2 if (a_18>17&a_18<21)|(a_22>17&a_22<21)
	replace edad_salida=3 if (a_18>20&a_18<30)|(a_22>20&a_22<30)
	
	ta edad_salida, gen(edad)
	
	foreach x of numlist 1/3 {
	mean edad`x' [aw=peso_ena] 
	matrix MATR  [`i',1]= e(b)
	matrix MATR  [`i',2]=  0
	matrix MATR  [`i',3]=  2013
	matrix MATR  [`i',4]=  `x'
	
	local i  = `i' + 1 
}
   	
	local j  = 	1
	foreach val of numlist 1/2  {
	foreach x of numlist 1/3 {
	mean edad`x' [aw=peso_ena] if region_e==`val'
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
	mean edad`x' [aw=peso_ena] if sexoentr==`val'
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
	mean edad`x' [aw=peso_ena] if pobre06==`val'
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
	mean edad`x' [aw=peso_ena] if quintily==`val'
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
	g edad_salida=1 if (a16>0&a16<18)|(a20>0&a20<18)
	replace edad_salida=2 if (a16>17&a16<21)|(a20>17&a20<21)
	replace edad_salida=3 if (a16>20&a16<30)|(a20>20&a20<30)
	
	ta edad_salida, gen(edad)
	
	g region=1 if region_4==1
	replace region=2 if region_4==2
	keep if (region==1|region==2)&pedad>11&pedad<30
	
	foreach x of numlist 1/3 {
	mean edad`x' [aw=peso] 
	matrix MATR  [`i',1]= e(b)
	matrix MATR  [`i',2]=  0
	matrix MATR  [`i',3]=  2018
	matrix MATR  [`i',4]=  `x'
	
	local i  = `i' + 1 
}
   	
	local j  = 	1
	foreach val of numlist 1/2  {
	foreach x of numlist 1/3 {
	mean edad`x' [aw=peso] if region==`val'
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
	mean edad`x' [aw=peso] if psexo==`val'
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
	mean edad`x' [aw=peso_ena] if pobre06==`val'
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
	mean edad`x' [aw=peso_ena] if quintily==`val'
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
	local codind = 519                  // Código de indicador
	local grupo  = 5                   // Cantidad de grupos
	local canio  = 3                   // Cantidad de años de la serie


import  excel "$tabulados\Auxiliares\AUX`codind'_PU.xls", firstrow clear
destring ANIO, replace
sort ANIO AUXILIAR EDAD
drop if ANIO==.
destring EDAD, replace
destring AUXILIAR, replace

g CODIND                 = 519
g NOMINDICADOR           = "Porcentaje de jóvenes y adolescentes que se fueron de su hogar de origen, según edad de salida por primera vez (Menor a 18)" if EDAD==1
replace NOMINDICADOR     = "Porcentaje de jóvenes y adolescentes que se fueron de su hogar de origen, según edad de salida por primera vez (Entre 18 y 20)" if EDAD==2
replace NOMINDICADOR     = "Porcentaje de jóvenes y adolescentes que se fueron de su hogar de origen, según edad de salida por primera vez (Entre 21 y 30)" if EDAD==3
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
