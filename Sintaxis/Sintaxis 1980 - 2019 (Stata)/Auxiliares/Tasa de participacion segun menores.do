***Tasa de actividad de jefes y conyuges entre 14 y 49 años según presencia de hijos menores de 12 **
**Tramos de edad niñes: menor de 4, menor de 7 y menor de 14


*1. Presencia de menores en el hogar

cap drop menor12
g menor12=(bc_pe3<13)
egen menor12H= max(menor12), by (bc_correlat)
drop if menor12H==0
drop if bc_pe3<13|bc_pe3>49
g activos=0
replace activos = 1 if bc_pobp>=2 & bc_pobp<=5
replace activos = . if bc_pobp==1

	

*2. Presecia de menores en el hogar	
	
g hijomenor12=((e30==3 | e30==4 | e30==5) & (e27<13))

egen sumhijo12= sum(hijomenor12), by (numero)

