*======================================================================================*
*                     **   EMPLEO, SALARIOS Y TRANSFERENCIAS   **
*                            MATRIZ PARA BASE WEB
*
*
* Creación:      13/07/20201
* Institución:   UMAD, FCS
* Responsable:   Sharon Katzkowicz
* Descripción:   Construcción base motor "Familia"
*
*
*======================================================================================*


*======================================================================================*
*                           UBICACIÓN DE ARCHIVOS      
*======================================================================================*

	global tabulados "C:\Users\Usuario\Dropbox\UMAD\Sociodemografico\Familia\Tabulados"

	
	foreach ind in /*
	      */ 500 501 502 503 504 505 506 507 508 509 /*
		  */ 510 511 512 513 514 515 518 519 /*
		  */ 520 525 526 527 535 536 537 /*
		  */ 538 539 545 546 547 548 549 550 551 552 /*
		  */ 555 556 557 560 561 562 563 {
	
	import excel "$tabulados\\`ind'.xls", sheet(Sheet1) firstrow clear
	

	drop A AUXILIAR
	rename ANIO FECHA
	
	replace SEXO="TODOS" if SEXO=="NA"
	replace ASCENDENCIA="TODOS" if ASCENDENCIA=="NA"
	replace QUINTIL="TODOS" if QUINTIL=="NA"
	replace URBANORURALUY="TOTAL PAÍS" if URBANORURALUY=="NA"
	replace DEPARTAMENTOUY="" if DEPARTAMENTOUY=="NA"
	order CODIND NOMINDICADOR SEXO ASCENDENCIA QUINTIL DEPARTAMENTOUY URBANORURALUY PAÍS FECHA VALOR RESPONSABLE 
	destring VALOR, replace
	save "$tabulados\Stata\\`ind'.dta", replace 
	
	}
	
use "$tabulados\Stata\\500.dta", replace 
append using "$tabulados\Stata\\501.dta"	
append using "$tabulados\Stata\\502.dta"	
append using "$tabulados\Stata\\503.dta"	
append using "$tabulados\Stata\\504.dta"	
append using "$tabulados\Stata\\505.dta"	
append using "$tabulados\Stata\\506.dta"	
append using "$tabulados\Stata\\507.dta"	
append using "$tabulados\Stata\\508.dta"	
append using "$tabulados\Stata\\509.dta"	
append using "$tabulados\Stata\\510.dta"	
append using "$tabulados\Stata\\511.dta"	
append using "$tabulados\Stata\\512.dta"	
append using "$tabulados\Stata\\513.dta"	
append using "$tabulados\Stata\\514.dta"	
append using "$tabulados\Stata\\515.dta"	
append using "$tabulados\Stata\\518.dta"	
append using "$tabulados\Stata\\519.dta"	
append using "$tabulados\Stata\\520.dta"	
append using "$tabulados\Stata\\525.dta"	
append using "$tabulados\Stata\\526.dta"	
append using "$tabulados\Stata\\527.dta"	
append using "$tabulados\Stata\\535.dta"	
append using "$tabulados\Stata\\536.dta"	
append using "$tabulados\Stata\\537.dta"	
append using "$tabulados\Stata\\538.dta"	
append using "$tabulados\Stata\\539.dta"	
append using "$tabulados\Stata\\545.dta"
append using "$tabulados\Stata\\546.dta"	
append using "$tabulados\Stata\\547.dta"	
append using "$tabulados\Stata\\548.dta"	
append using "$tabulados\Stata\\549.dta"	
append using "$tabulados\Stata\\550.dta"	
append using "$tabulados\Stata\\551.dta"	
append using "$tabulados\Stata\\552.dta"	
append using "$tabulados\Stata\\555.dta"	
append using "$tabulados\Stata\\556.dta"	
append using "$tabulados\Stata\\557.dta"	
append using "$tabulados\Stata\\560.dta"	
append using "$tabulados\Stata\\561.dta"	
append using "$tabulados\Stata\\562.dta"	
append using "$tabulados\Stata\\563.dta"	



replace FECHA = 2000+FECHA if FECHA<100
export excel using "C:\Users\Usuario\Dropbox\UMAD\Sociodemografico\Familia\Tabulados\Familia_actualizado.xls", firstrow(variables) replace
