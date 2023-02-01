*******Asistencia
	cd "C:\Users\Usuario\Dropbox\UMAD"
	global bases "C:\Users\Usuario\Dropbox\UMAD\Bases de datos\Encuesta Continua de Hogares (ECH)\Compatibilizadas IECON\Bases DESCA"
	global tabulados "C:\Users\Usuario\Dropbox\UMAD\Sociodemografico\Familia\Tabulados"

foreach i of numlist 12/19 {
    use "$bases\fusionada_personasyhogares_`i'.dta", clear
	g asistemen3=1 if e238==1&bc_pe3<3
	replace asistemen3=0 if e238!=1&bc_pe3<3
    save "$bases\fusionada_personasyhogares_`i'.dta", replace

	}
