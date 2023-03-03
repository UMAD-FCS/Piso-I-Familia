***Renombra variables

rename laborabl laborable
rename trabajoa trabajoayer
rename trabajom trabajomin
rename asistedi asistedisa  



**********************************************************************************************************
************************************* CREACIÓN DE VARIABLES DE FILTRO ************************************


**************** PERSONAS CON DISCAPACIDAD 

**** Generar variable "Hogar con presencia de persona/s con discapacidad*

sort numero
cap drop hog_disc1
by numero: g hog_disc1=0
by numero: replace hog_disc1=1 if j25==1 & eut==1


* Se calcula el total de personas con discapacidad en el hogar
cap drop sumhog_disc
egen sumhog_disc= sum(hog_disc1), by (numero)

* Se identifica si hay alguna persona con discapacidad en el hogar
cap drop hog_disc
gen hog_disc=0
replace hog_disc=1 if sumhog_disc>0

* Se identifica si la persona tiene alguna discapacidad
gen n_disc1=0
replace n_disc1=1 if disc==1
replace n_disc1=. if eut==2

* Se calcula el número de personas con discapacidad en el hogar
egen n_disc= sum(n_disc1), by (numero)

**** Filtros

** 1.  Identifica si a la persona se le aplicó la encuesta, pertenece a la poblaci󮠥n edad de trabajar, hay alguna persona con discapacidad en el hogar
** y la personas encuestada no es discapacitada
gen filtro_disc1=0
replace filtro_disc1=1 if eut14==1 & hog_disc==1 & disc==2
replace filtro_disc1=. if eut==2


** 2. Identifica si a la persona se le aplic󠬡 encuesta, pertenece a la poblaci󮠥n edad de trabajar, es discapacitada y hay otra persona con
** discapacidad en el hogar
gen filtro_disc2=0
replace filtro_disc2=1 if eut14==1 & disc==1 & hog_disc==1 & n_disc>1
replace filtro_disc2=. if eut==2

** 3. Identifica si a la persona se le aplic󠬡 encuesta, pertenece a la poblaci󮠥n edad de trabajar y, hay otra persona con discapacidad en el hogar
** y la personas encuestada no es discapacitada, o la persona encuestada es discapacitada y hay alguna persona mⳠcon discapacidad en el hogar. 
gen filtroj1=0
replace filtroj1=1 if filtro_disc1==1 | filtro_disc2==1


**************** NIÑOS DE 0 a 3 

cap drop recod_edad 
cap drop menor_3 
cap drop summenor_3 
cap drop hogar_menor3


**** Variable que identifica si hay personas de 3 años o menos en el hogar

* Se recodifica la variable edad = 1 si la persona tiene entre 0 y 3 años, 2 si tiene 4 o 5, 3 si tiene entre 6 y 12 años, 0 en caso contrario.
gen recod_edad=0 if eut==1
replace recod_edad=1 if edad<4 
replace recod_edad=2 if edad>3 & edad<6 
replace recod_edad=3 if edad>5 & edad<13 


* Se identifica si la persona tiene entre 0 y 3 años 
gen menor_3=0 
replace menor_3=1 if recod_edad==1 

* Se calcula la cantidad de personas que tienen 3 años o menos por hogar
sort numero
egen summenor_3 = sum(menor_3), by (numero)

* Se identifica si hay alguna persona que tiene 3 años o menos en el hogar
gen hogar_menor3=0
replace hogar_menor3=1 if summenor_3>0
replace hogar_menor3=. if eut==0


* Se identifica si en el hogar hay personas de 3 años o menos con discapacidad (en este caso, ese menor es tratado en cuidado 
* de personas con discapacidad (j1) y no en m󤵬o j2 (cuidado de niños y niñas del hogar de 0 a 3 años de edad).

* Se identifica si la persona es menor de 3 años de edad con discapacidad
gen menor3_disc=0 
replace menor3_disc=1 if menor_3==1 & disc==1 

* Se calcula el total de ni񯳠y ni񡳠de 3 a񯳠o menos con discapacidad por hogar
sort numero
egen summenor3_disc = sum (menor3_disc), by (numero) 

* Se identifica si hay alguuna persona de 3 a񯳠de edad o menos con discapacidad en el hogar
gen hogar_menor3_disc=0
replace hogar_menor3_disc=1 if summenor3_disc>0
replace hogar_menor3_disc=. if eut==0



**** Variable que identifica si la persona encuestada tiene 14 o mⳠa񯳬 se le aplic󠬡 EUT, vive en hogares con menores de 3 a񯳬
*y que en el hogar no hay menores de 3 con discapacidad.
gen filtroj2=0
replace filtroj2=1 if edad>=14 & hogar_menor3==1 & eut==1 & hogar_menor3_disc==0


**************** NIÑAS Y NIÑOS DE 4 o 5 

**** Variable que identifica si hay niñas y niñosde 4 o 5 años

cap drop menor_5 
cap drop summenor_5 
cap drop hogar_menor5

* Se identifica si la persona tiene 4 o 5 a񯳠de edad
gen menor_5=0 
replace menor_5=1 if recod_edad==2 

* Se calcula el total de ni񯳠y ni񡳠de 4 o 5 a񯳠de edad en el hogar
sort numero
egen summenor_5 = sum(menor_5), by (numero)

* Se identifica si en el hogar hay alguna persona de 4 o 5 a񯳠de edad
gen hogar_menor5=0
replace hogar_menor5=1 if summenor_5>0
replace hogar_menor5=. if eut==0



* Se identifica si en el hogar hay personas de 4 o 5 a񯳠de edad con discapacidad (en este caso, ese menor es tratado en cuidado 
*de personas con discapacidad (j1) y no en m󤵬o j3 (cuidado de ni񯳠y ni񡳠del hogar de 4 y 5 a񯳠de edad).

* Se identifica si es un ni񯠯 ni񡠤e 4 o 5 a񯳠de edad con discapacidad 
gen menor5_disc=0 
replace menor5_disc=1 if menor_5==1 & disc==1 

* Se calcula el total de ni񯳠y ni񡳠de 4 o 5 a񯳠de edad con discapacidad en el hogar
sort numero
egen summenor5_disc = sum (menor5_disc), by (numero)

* Se identifica si en el hogar hay alg򮠮i񯠯 ni񡠤e 4 o 5 a񯳠de edad con discapacidad
gen hogar_menor5_disc=0
replace hogar_menor5_disc=1 if summenor5_disc>0
replace hogar_menor5_disc=. if eut==0


**** Variable que identifica si la persona encuestada tiene 14 o mⳠa񯳬 se le aplic󠬡 EUT, vive en hogares con ni񯳠o ni񡳠de 4 o 5 a񯳬
*y que en el hogar no hay personas de 4 o 5 a񯳠con discapacidad.
gen filtroj3=0
replace filtroj3=1 if edad>=14 & hogar_menor5==1 & eut==1 & hogar_menor5_disc==0



**************** NIÑOSS DE 6 a 12 

cap drop menor_12 
cap drop summenor_12 
cap drop hogar_menor12

* Se identifica si la persona tiene entre 6 y 12 a񯳠de edad
gen menor_12=0 
replace menor_12=1 if recod_edad==3 

* Se calcula el total de personas ente 6 y 12 a񯳠de edad en el hogar
sort numero
egen summenor_12 = sum(menor_12), by (numero)

* Se identifica si en el hogar hay alguna persona entre 6 y 12 a񯳠de edad
gen hogar_menor12=0
replace hogar_menor12=1 if summenor_12>0
replace hogar_menor12=. if eut==0


* Se identifica si en el hogar hay personas entre 6 y 12 a񯳠de edad con discapacidad (en este caso, ese menor es tratado en cuidado 
*de personas con discapacidad (j1) y no en m󤵬o j4 (cuidado de ni񯳠y ni񡳠del hogar entre 6 y 12 a񯳠de edad).


* Se identifica si es un menor entre 6 y 12 a񯳠con discapacidad 
gen menor12_disc=0 
replace menor12_disc=1 if menor_12==1 & disc==1 

* Se calcula el total de personas entre 6 y 12 a񯳠de edad con discapacidad en el hogar
sort numero
egen summenor12_disc = sum (menor12_disc), by (numero)

* Se identifica si en el hogar hay alguna persona entre 6 y 12 a񯳠de edad con discapacidad 
gen hogar_menor12_disc=0
replace hogar_menor12_disc=1 if summenor12_disc>0
replace hogar_menor12_disc=. if eut==0


**** Variable que identifica si la persona encuestada tiene 14 o mⳠa񯳬 se le aplic󠬡 EUT, vive en hogares con ni񯳠o ni񡳠entre 6 y 12 a񯳬
*y que en el hogar no hay personas entre 6 y 12 a񯳠con discapacidad.
gen filtroj4=0
replace filtroj4=1 if edad>=14 & hogar_menor12==1 & eut==1 & hogar_menor12_disc==0


**** Variable que identifica si la persona encuestada tiene 14 o mⳠa񯳬 se le aplic󠬡 EUT, vive en hogares con ni񯳠o ni񡳠de 12 a񯳠o menos
*y que en el hogar no hay personas de 12 a񯳠o menos con discapacidad.
cap drop filtro_nino
gen filtro_nino=.
replace filtro_nino=1 if filtroj2==1
replace filtro_nino=1 if filtroj3==1
replace filtro_nino=1 if filtroj4==1


**************** ADULTOS MAYORES 

recode j563 (2=0)

* Se calcula el total de personas dependientes de 65 o mⳠa񯳠en el hogar
sort numero
egen sumhog_may= sum (j563), by (numero)

* Se identifica si en el hogar hay alguna persona dependiente de 65 a񯳠o mⳍ
gen hog_may_dep=0
replace hog_may_dep=1 if sumhog_may>0

**** Variable que identifica si la persona encuestada tiene 14 a񯳠o mⳬ se le aplic󠬡 EUT, vive en hogar con alguna persona dependiente de 65 o mⳍ
*a񯳠
gen filtroj5=0
replace filtroj5=1 if edad>=14 & eut==1 & hog_may_dep==1 & j564!=8


**** Variable que identifica si la persona encuestada tiene 14 a񯳠o mⳬ se le aplic󠬡 EUT y vive en hogar con personas dependientes
* (discapacitados, ni񯳠de 12 a񯳠o menos o personas dependientes de 65 a񯳠o m⳩ 
gen filtro_cuidados=0
replace filtro_cuidados=1 if filtro_nino==1
replace filtro_cuidados=1 if filtroj1==1
replace filtro_cuidados=1 if filtroj5==1



**** HORAS DE CUIDADOS
**************** CUIDADOS DE PERSONAS CON DISCAPACIDAD

recode j131h (.=0) if eut==2
recode j131h (.=0) if j131==2 & filtroj1==1
recode j131m (.=0) if j131==2 & filtroj1==1

recode j131h (.=0) if filtroj1==0
recode j131m (.=0) if filtroj1==0

ta j132, m
ta j132h
recode j132h (.=0) if j132==2 & filtroj1==1
recode j132m (.=0) if j132==2 & filtroj1==1

recode j132h (.=0) if filtroj1==0
recode j132m (.=0) if filtroj1==0

ta j133, m
ta j133h
recode j133h (.=0) if j133==2 & filtroj1==1
recode j133m (.=0) if j133==2 & filtroj1==1

recode j133h (.=0) if filtroj1==0
recode j133m (.=0) if filtroj1==0

ta j134, m
ta j134h
recode j134h (.=0) if j134==2 & filtroj1==1
recode j134m (.=0) if j134==2 & filtroj1==1

recode j134h (.=0) if filtroj1==0
recode j134m (.=0) if filtroj1==0

ta j135, m
ta j135h
recode j135h (.=0) if j135==2 & filtroj1==1
recode j135m (.=0) if j135==2 & filtroj1==1

recode j135h (.=0) if filtroj1==0
recode j135m (.=0) if filtroj1==0

ta j136, m
ta j136h
recode j136h (.=0) if j136==2 & filtroj1==1
recode j136m (.=0) if j136==2 & filtroj1==1

recode j136h (.=0) if filtroj1==0
recode j136m (.=0) if filtroj1==0

ta j136, m
ta j136h
recode j136h (.=0) if j136==2 & filtroj1==1
recode j136m (.=0) if j136==2 & filtroj1==1

recode j136h (.=0) if filtroj1==0
recode j136m (.=0) if filtroj1==0


**************** CUIDADO DE NIҏS Y NIҁS DEL HOGAR DE 0 A 3 AҏS DE EDAD

recode j241h (.=0) if j241==2 & filtroj2==1
recode j241m (.=0) if j241==2 & filtroj2==1
recode j241h (.=0) if filtroj2==0
recode j241m (.=0) if filtroj2==0

ta j242h
recode j242h (.=0) if j242==2 & filtroj2==1
recode j242m (.=0) if j242==2 & filtroj2==1
recode j242h (.=0) if filtroj2==0
recode j242m (.=0) if filtroj2==0

recode j243h (.=0) if j243==2 & filtroj2==1
recode j243m (.=0) if j243==2 & filtroj2==1
recode j243h (.=0) if filtroj2==0
recode j243m (.=0) if filtroj2==0

recode j244h (.=0) if j244==2 & filtroj2==1
recode j244m (.=0) if j244==2 & filtroj2==1
recode j244h (.=0) if filtroj2==0
recode j244m (.=0) if filtroj2==0

recode j245h (.=0) if j245==2 & filtroj2==1
recode j245m (.=0) if j245==2 & filtroj2==1
recode j245h (.=0) if filtroj2==0
recode j245m (.=0) if filtroj2==0

recode j246h (.=0) if j246==2 & filtroj2==1
recode j246m (.=0) if j246==2 & filtroj2==1
recode j246h (.=0) if filtroj2==0
recode j246m (.=0) if filtroj2==0

recode j247h (.=0) if j247==2 & filtroj2==1
recode j247m (.=0) if j247==2 & filtroj2==1
recode j247h (.=0) if filtroj2==0
recode j247m (.=0) if filtroj2==0


**************** CUIDAD DE NIҏS Y NIҁS DEL HOGAR DE 4 O 5 AҏS DE EDAD

recode j349h (.=0) if j349==2 & filtroj3==1
recode j349m (.=0) if j349==2 & filtroj3==1
recode j349h (.=0) if filtroj3==0
recode j349m (.=0) if filtroj3==0

recode j350h (.=0) if j350==2 & filtroj3==1
recode j350m (.=0) if j350==2 & filtroj3==1
recode j350h (.=0) if filtroj3==0
recode j350m (.=0) if filtroj3==0

recode j351h (.=0) if j351==2 & filtroj3==1
recode j351m (.=0) if j351==2 & filtroj3==1
recode j351h (.=0) if filtroj3==0
recode j351m (.=0) if filtroj3==0

recode j352h (.=0) if j352==2 & filtroj3==1
recode j352m (.=0) if j352==2 & filtroj3==1
recode j352h (.=0) if filtroj3==0
recode j352m (.=0) if filtroj3==0

recode j353h (.=0) if j353==2 & filtroj3==1
recode j353m (.=0) if j353==2 & filtroj3==1
recode j353h (.=0) if filtroj3==0
recode j353m (.=0) if filtroj3==0

recode j354h (.=0) if j354==2 & filtroj3==1
recode j354m (.=0) if j354==2 & filtroj3==1
recode j354h (.=0) if filtroj3==0
recode j354m (.=0) if filtroj3==0

recode j355h (.=0) if j355==2 & filtroj3==1
recode j355m (.=0) if j355==2 & filtroj3==1
recode j355h (.=0) if filtroj3==0
recode j355m (.=0) if filtroj3==0


**************** CUIDADO DE NIҏS Y NIҁS DEL HOGAR DE 6 A 12 AҏS 

recode j457h (.=0) if j457==2 & filtroj4==1
recode j457m (.=0) if j457==2 & filtroj4==1
recode j457h (.=0) if filtroj4==0
recode j457m (.=0) if filtroj4==0

recode j458h (.=0) if j458==2 & filtroj4==1
recode j458m (.=0) if j458==2 & filtroj4==1
recode j458h (.=0) if filtroj4==0
recode j458m (.=0) if filtroj4==0

recode j459h (.=0) if j459==2 & filtroj4==1
recode j459m (.=0) if j459==2 & filtroj4==1
recode j459h (.=0) if filtroj4==0
recode j459m (.=0) if filtroj4==0

recode j460h (.=0) if j460==2 & filtroj4==1
recode j460m (.=0) if j460==2 & filtroj4==1
recode j460h (.=0) if filtroj4==0
recode j460m (.=0) if filtroj4==0

recode j461h (.=0) if j461==2 & filtroj4==1
recode j461m (.=0) if j461==2 & filtroj4==1
recode j461h (.=0) if filtroj4==0
recode j461m (.=0) if filtroj4==0

recode j462h (.=0) if j462==2 & filtroj4==1
recode j462m (.=0) if j462==2 & filtroj4==1
recode j462h (.=0) if filtroj4==0
recode j462m (.=0) if filtroj4==0


**************** CUIDADO DE PERSONAS DEPENDIENTES DE 65 O M AҏS DE EDAD

recode j564h (.=0) if j564==2 & filtroj5==1
recode j564m (.=0) if j564==2 & filtroj5==1
recode j564h (.=0) if filtroj5==0
recode j564m (.=0) if filtroj5==0

recode j565h (.=0) if j565==2 & filtroj5==1
recode j565m (.=0) if j565==2 & filtroj5==1
recode j565h (.=0) if filtroj5==0
recode j565m (.=0) if filtroj5==0

recode j566h (.=0) if j566==2 & filtroj5==1
recode j566m (.=0) if j566==2 & filtroj5==1
recode j566h (.=0) if filtroj5==0
recode j566m (.=0) if filtroj5==0

recode j567h (.=0) if j567==2 & filtroj5==1
recode j567m (.=0) if j567==2 & filtroj5==1
recode j567h (.=0) if filtroj5==0
recode j567m (.=0) if filtroj5==0

recode j568h (.=0) if j568==2 & filtroj5==1
recode j568m (.=0) if j568==2 & filtroj5==1
recode j568h (.=0) if filtroj5==0
recode j568m (.=0) if filtroj5==0

recode j569h (.=0) if j569==2 & filtroj5==1
recode j569m (.=0) if j569==2 & filtroj5==1
recode j569h (.=0) if filtroj5==0
recode j569m (.=0) if filtroj5==0


*Cuidado de personas con discapacidad

g j131_tot=((j131h*60) + j131m)/60
g j132_tot=((j132h*60) + j132m)/60
g j133_tot=((j133h*60) + j133m)/60
g j134_tot=((j134h*60) + j134m)/60
g j135_tot=((j135h*60) + j135m)/60
g j136_tot=((j136h*60) + j136m)/60


*Cuidado de ni񯳠y ni񡳠del hogar de 0 a 3 a񯳍

g j241_tot=((j241h*60) + j241m)/60
g j242_tot=((j242h*60) + j242m)/60
g j243_tot=((j243h*60) + j243m)/60
g j244_tot=((j244h*60) + j244m)/60
g j245_tot=((j245h*60) + j245m)/60
g j246_tot=((j246h*60) + j246m)/60
g j247_tot=((j247h*60) + j247m)/60


*Cuidado de ni񯳠y ni񡳠del hogar de 4 a 5 a񯳍

g j349_tot=((j349h*60) + j349m)/60
g j350_tot=((j350h*60) + j350m)/60
g j351_tot=((j351h*60) + j351m)/60
g j352_tot=((j352h*60) + j352m)/60
g j353_tot=((j353h*60) + j353m)/60
g j354_tot=((j354h*60) + j354m)/60
g j355_tot=((j355h*60) + j355m)/60


*Cuidado de ni񯳠y ni񡳠del hogar de 6 a 12 a񯳍

g j457_tot=((j457h*60) + j457m)/60
g j458_tot=((j458h*60) + j458m)/60
g j459_tot=((j459h*60) + j459m)/60
g j460_tot=((j460h*60) + j460m)/60
g j461_tot=((j461h*60) + j461m)/60
g j462_tot=((j462h*60) + j462m)/60


*Cuidado de personas dependientes de 65 o mⳠa񯳍

g j564_tot=((j564h*60) + j564m)/60
g j565_tot=((j565h*60) + j565m)/60
g j566_tot=((j566h*60) + j566m)/60
g j567_tot=((j567h*60) + j567m)/60
g j568_tot=((j568h*60) + j568m)/60
g j569_tot=((j569h*60) + j569m)/60



**** Variable que indica la cantidad de horas diarias dedicadas al cuidado de personas con discapacidad
gen dep_disc=j131_tot+j132_tot+j133_tot+j134_tot+j135_tot+j136_tot

recode dep_disc (.=0)
replace dep_disc=0 if filtroj1==0 
g horas1=dep_disc*7

**** Variable que indica la cantidad de horas diarias dedicadas al cuidado de ni񯳠y ni񡳠del hogar de 0 a 3 a񯳍
gen ninos3=j241_tot+j242_tot+j243_tot+j244_tot+j245_tot+j246_tot+j247_tot
recode ninos3 (.=0)
replace ninos3=0 if filtroj2==0
g horas2=ninos3*7

**** Variable que indica la cantidad de horas diarias dedicadas al cuidado de ni񯳠y ni񡳠del hogar de 4 o 5 a񯳍
gen ninos4a5= j349_tot+j350_tot+j351_tot+j352_tot+j353_tot+j354_tot+j355_tot 
recode ninos4a5 (.=0)
replace ninos4a5=0 if filtroj3==0
g horas3=ninos4a5*7

**** Variable que indica la cantidad de horas diarias dedicadas al cuidado de ni񯳠y ni񡳠del hogar de 6 a 12 a񯳍
gen ninos6a12=j457_tot+j458_tot+j459_tot+j460_tot+j461_tot+j462_tot
recode ninos6a12 (.=0)
replace ninos6a12=0 if filtroj4==0
g horas4=ninos6a12*7

**** Variable que indica la cantidad de horas diarias dedicadas al cuidado infantil
gen nin_tot=ninos3 + ninos4a5 + ninos6a12
recode nin_tot (.=0)
g horas5=nin_tot*7

**** Variable que indica la cantidad de horas diarias dedicadas al cuidado de personas dependientes de 65 o más años de edad
gen mayores=j564_tot+j565_tot+j566_tot+j567_tot+j568_tot+j569_tot
recode mayores (.=0)
replace mayores=0 if filtroj5==0 
g horas6=mayores*7

**** Variable que indica la cantidad de horas diarias dedicadas al total de las actividades de cuidados
gen cuidados=dep_disc+ninos3+ninos4a5+ninos6a12+mayores
recode cuidados (.=0)

**** Variable que identifica si la persona realiza actividades de cuidados en el hogar
gen hace_cuidados=0
replace hace_cuidados=1 if cuidados>0



******PARTICIPACIÓN EN LAS ACTIVIDADES DE CUIDADOS
gen part1=0 if filtroj1==1
replace part1=1 if dep_disc>0

gen part2=0 if filtroj2==1
replace part2=1 if ninos3>0 

gen part3=0 if filtroj3==1
replace part3=1 if ninos4a5>0 

gen part4=0 if filtroj4==1
replace part4=1 if ninos6a12>0 

gen part5=0 if filtro_nino==1
replace part5=1 if nin_tot>0 

gen part6=0 if filtroj5==1
replace part6=1 if mayores>0 
