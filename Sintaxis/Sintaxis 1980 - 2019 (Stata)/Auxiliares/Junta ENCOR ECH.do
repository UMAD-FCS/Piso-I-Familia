	cd "C:\Users\Usuario\Dropbox\UMAD"
	global bases "C:\Users\Usuario\Dropbox\UMAD\Bases de datos\ENCOR"
	global tabulados "C:\Users\Usuario\Dropbox\UMAD\Sociodemografico\Familia\Tabulados"
    use "$bases\base_encor_terceros.dta", clear

*Ordeno según número, nper y anio de nacimiento de hijes (dejo solo el registro de primer hijo para cada persona)
rename hr12_*_cor anio
sort numero nper anio
bys numero: g id=_n
drop if id!=1 	
drop id
tostring nper, replace
cap drop identif
g identif=numero+nper
save "$bases\ENCOR.dta", replace

use "C:\Users\Usuario\Dropbox\UMAD\Bases de datos\Encuesta Continua de Hogares (ECH)\Compatibilizadas IECON\fusionada_personasyhogares_2015.dta", clear
rename bc_correlat numero
rename bc_nper nper
tostring nper, replace
g identif=numero+nper
merge 1:1 identif using "$bases\ENCOR.dta"
g encor=hr10!=.


g uno=1  						//Variabe auxiliar para construir cantidad de personas
replace uno=0 if bc_pe4==7 		//Excluyo de la suma al servicio doméstico

g ypc1=bc_ht11/bd_ht19 if bc_pe4==1
xtile tercilesy_prev= ypc1 [aw=bc_pesoan], n(3)

egen bd_tercilesy= max (tercilesy_prev), by (numero)
label var bd_quintilesy "Tercil de ingreso per cápita del hogar"

drop uno ypc1 tercilesy_prev


save "$bases\ENCOR.dta", replace
