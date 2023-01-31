#Modelo de pareja

sem1 <- sem1 %>% dplyr::mutate(bc_pe4 = case_when(e30==1 ~ 1, e30==2 ~ 2, e30 ==3|e30==4|e30==5 ~ 3, e30==6|e30==7 ~ 4, 
                                                  e30==8|e30==9|e30==10|e30==11|e30==12 ~ 5, e30==13 ~ 6, e30==14 ~ 7))
sem2_implant <- sem2_implant %>% dplyr::mutate(bc_pe4 = case_when (e30==1 ~ 1, e30==2 ~ 2, e30 ==3|e30==4|e30==5 ~ 3, e30==6|e30==7 ~ 4, 
                                                                   e30==8|e30==9|e30==10|e30==11|e30==12 ~ 5, e30==13 ~ 6, e30==14 ~ 7))


sem1 <- sem1 %>% dplyr::mutate(jefe = case_when(bc_pe4==1 ~ 1, bc_pe4!=1 ~ 0))
sem2_implant <- sem2_implant %>% dplyr::mutate(jefe = case_when(bc_pe4==1 ~ 1, bc_pe4!=1 ~ 0))

sem1 <- sem1 %>% dplyr::mutate(esposo_companero = case_when(bc_pe4==2 ~ 1, bc_pe4!=2 ~ 0))
sem2_implant <- sem2_implant %>% dplyr::mutate(esposo_companero = case_when(bc_pe4==2 ~ 1, bc_pe4!=2 ~ 0))

sem1 <- sem1 %>% dplyr::mutate(hijos = case_when(bc_pe4==3 ~ 1, bc_pe4!=3 ~ 0))
sem2_implant <- sem2_implant %>% dplyr::mutate(hijos = case_when(bc_pe4==3 ~ 1, bc_pe4!=3 ~ 0))

sem1 <- sem1 %>% dplyr::mutate(otro_pariente = case_when(bc_pe4==4|bc_pe4==5 ~ 1, bc_pe4!=4&bc_pe4!=5 ~ 0))
sem2_implant <- sem2_implant %>% dplyr::mutate(otro_pariente = case_when(bc_pe4==4|bc_pe4==5 ~ 1, bc_pe4!=4&bc_pe4!=5 ~ 0))

sem1 <- sem1 %>% dplyr::mutate(otro_nopariente = case_when(bc_pe4==6 ~ 1, bc_pe4!=6 ~ 0))
sem2_implant <- sem2_implant %>% dplyr::mutate(otro_nopariente = case_when(bc_pe4==6 ~ 1, bc_pe4!=6 ~ 0))

sem1 <- sem1 %>% dplyr::mutate(servicio_domestico = case_when(bc_pe4==7 ~ 1, bc_pe4!=7 ~ 0))
sem2_implant <- sem2_implant %>% dplyr::mutate(servicio_domestico = case_when(bc_pe4==7 ~ 1, bc_pe4!=7 ~ 0))

sem1 <- sem1 %>% dplyr::mutate(jefatura_femenina = case_when(bc_pe4==1&bc_pe2==2 ~ 1, bc_pe4!=1|bc_pe2!=2 ~ 0))
sem2_implant <- sem2_implant %>% dplyr::mutate(jefatura_femenina = case_when(bc_pe4==1&bc_pe2==2 ~ 1, bc_pe4!=1|bc_pe2!=2 ~ 0))

sem1 <- sem1 %>% dplyr::mutate(bc_correlat = numero)
sem2_implant <- sem2_implant %>% dplyr::mutate(bc_correlat = ID)


sem1 <- sem1 %>% group_by(bc_correlat) %>% mutate(sumjefe=sum(jefe)) %>% as.data.frame()
sem2_implant <- sem2_implant %>% group_by(bc_correlat) %>% mutate(sumjefe=sum(jefe)) %>% as.data.frame()

sem1 <- sem1 %>% group_by(bc_correlat) %>% mutate(sumesposo_companero=sum(esposo_companero)) %>% as.data.frame()
sem2_implant <- sem2_implant %>% group_by(bc_correlat) %>% mutate(sumesposo_companero=sum(esposo_companero)) %>% as.data.frame()

sem1 <- sem1 %>% group_by(bc_correlat) %>% mutate(sumhijo=sum(hijos)) %>% as.data.frame()
sem2_implant <- sem2_implant %>% group_by(bc_correlat) %>% mutate(sumhijo=sum(hijos)) %>% as.data.frame()

sem1 <- sem1 %>% group_by(bc_correlat) %>% mutate(sumotro_pariente=sum(otro_pariente)) %>% as.data.frame()
sem2_implant <- sem2_implant %>% group_by(bc_correlat) %>% mutate(sumotro_pariente=sum(otro_pariente)) %>% as.data.frame()

sem1 <- sem1 %>% group_by(bc_correlat) %>% mutate(sumno_pariente=sum(otro_nopariente)) %>% as.data.frame()
sem2_implant <- sem2_implant %>% group_by(bc_correlat) %>% mutate(sumno_pariente=sum(otro_nopariente)) %>% as.data.frame()

sem1 <- sem1 %>% group_by(bc_correlat) %>% mutate(sumservicio_domestico=sum(servicio_domestico)) %>% as.data.frame()
sem2_implant <- sem2_implant %>% group_by(bc_correlat) %>% mutate(sumservicio_domestico=sum(servicio_domestico)) %>% as.data.frame()

sem1 <- sem1 %>% group_by(bc_correlat) %>% mutate(sumjefatura_femenina=sum(jefatura_femenina)) %>% as.data.frame()
sem2_implant <- sem2_implant %>% group_by(bc_correlat) %>% mutate(sumjefatura_femenina=sum(jefatura_femenina)) %>% as.data.frame()



#bc_pobp, bc_horas y bc_horas1

sem1 <- sem1 %>% dplyr::mutate(bc_pobp = POBPCOAC)
sem2_implant <- sem2_implant %>% dplyr::mutate(bc_pobp = pobpcoac)

sem1 <- sem1 %>% dplyr::mutate(bc_horas = F85+F98)
sem2_implant <- sem2_implant %>% dplyr::mutate(bc_horas = f85+f98)

sem1 <- sem1 %>% dplyr::mutate(bc_horas_1 = F85)
sem2_implant <- sem2_implant %>% dplyr::mutate(bc_horas_1 = f85)


#generamos la variable que identifica hogares en donde hay una pareja 

sem1 <- sem1 %>% dplyr::mutate(biparentales = case_when(sumesposo_companero>0 ~ 1, sumesposo_companero==0 ~ 0))
sem2_implant <- sem2_implant %>% dplyr::mutate(biparentales = case_when(sumesposo_companero>0 ~ 1, sumesposo_companero==0 ~ 0))

#Nos quedamos solo con los hogares biparentales*
  
  
#generamos variables a nivel agregado
#Primero generamos variable a nivel personas*
  
#JEFE Ocupado tiempo completo_varón*
sem1 <- sem1 %>% dplyr::mutate(jefeocup_tc_v = case_when(bc_pobp==2 & (bc_horas>39 | bc_horas_1>39) & jefe==1 & bc_pe2==1&biparentales==1 ~ 1))
sem2_implant <- sem2_implant %>% dplyr::mutate(jefeocup_tc_v = case_when(bc_pobp==2 & (bc_horas>39 | bc_horas_1>39) & jefe==1 & bc_pe2==1&biparentales==1 ~ 1))

#JEFE Ocupado tiempo completo_mujer*
sem1 <- sem1 %>% dplyr::mutate(jefeocup_tc_m = case_when(bc_pobp==2 & (bc_horas>39 | bc_horas_1>39) & jefe==1 & bc_pe2==2&biparentales==1 ~ 1))
sem2_implant <- sem2_implant %>% dplyr::mutate(jefeocup_tc_m = case_when(bc_pobp==2 & (bc_horas>39 | bc_horas_1>39) & jefe==1 & bc_pe2==2&biparentales==1 ~ 1))

#CONYUGE Ocupado tiempo completo*
sem1 <- sem1 %>% dplyr::mutate(conyocup_tc_v = case_when(bc_pobp==2 & (bc_horas>39 | bc_horas_1>39) & esposo_companero==1 & bc_pe2==1&biparentales==1 ~ 1))
sem2_implant <- sem2_implant %>% dplyr::mutate(conyocup_tc_v = case_when(bc_pobp==2 & (bc_horas>39 | bc_horas_1>39) & esposo_companero==1 & bc_pe2==1&biparentales==1 ~ 1))

sem1 <- sem1 %>% dplyr::mutate(conyocup_tc_m = case_when(bc_pobp==2 & (bc_horas>39 | bc_horas_1>39) & esposo_companero==1 & bc_pe2==2&biparentales==1 ~ 1))
sem2_implant <- sem2_implant %>% dplyr::mutate(conyocup_tc_m = case_when(bc_pobp==2 & (bc_horas>39 | bc_horas_1>39) & esposo_companero==1 & bc_pe2==2&biparentales==1 ~ 1))

#JEFE Ocupado medio tiempo_varón*
sem1 <- sem1 %>% dplyr::mutate(jefeocup_mt_v = case_when(bc_pobp==2 & bc_horas<40 & jefe==1 & bc_pe2==1&biparentales==1 ~ 1))
sem2_implant <- sem2_implant %>% dplyr::mutate(jefeocup_mt_v = case_when(bc_pobp==2 & bc_horas<40 & jefe==1 & bc_pe2==1&biparentales==1 ~ 1))

#JEFE Ocupado medio tiempo_mujer*
sem1 <- sem1 %>% dplyr::mutate(jefeocup_mt_m = case_when(bc_pobp==2 & bc_horas<40& jefe==1 & bc_pe2==2&biparentales==1 ~ 1))
sem2_implant <- sem2_implant %>% dplyr::mutate(jefeocup_mt_m = case_when(bc_pobp==2 & bc_horas<40& jefe==1 & bc_pe2==2&biparentales==1 ~ 1))

#CONYUGE Ocupado medio tiempo_varón*
sem1 <- sem1 %>% dplyr::mutate(conyocup_mt_v = case_when(bc_pobp==2 & bc_horas<40& esposo_companero==1 & bc_pe2==1&biparentales==1 ~ 1))
sem2_implant <- sem2_implant %>% dplyr::mutate(conyocup_mt_v = case_when(bc_pobp==2 & bc_horas<40& esposo_companero==1 & bc_pe2==1&biparentales==1 ~ 1))

#CONYUGE Ocupado medio tiempo_mujer*
sem1 <- sem1 %>% dplyr::mutate(conyocup_mt_m = case_when(bc_pobp==2 & bc_horas<40& esposo_companero==1 & bc_pe2==2&biparentales==1 ~ 1))
sem2_implant <- sem2_implant %>% dplyr::mutate(conyocup_mt_m = case_when(bc_pobp==2 & bc_horas<40& esposo_companero==1 & bc_pe2==2&biparentales==1 ~ 1))

#JEFE Inactivo o desempleado_varón*
sem1 <- sem1 %>% dplyr::mutate(jefeinact_desoc_v = case_when(bc_pobp>2 & bc_pobp<12 & jefe==1 & bc_pe2==1&biparentales==1 ~ 1))
sem2_implant <- sem2_implant %>% dplyr::mutate(jefeinact_desoc_v = case_when(bc_pobp>2 & bc_pobp<12 & jefe==1 & bc_pe2==1&biparentales==1 ~ 1))

#JEFE Inactivo o desempleado_mujer*
sem1 <- sem1 %>% dplyr::mutate(jefeinact_desoc_m = case_when(bc_pobp>2 & bc_pobp<12 & jefe==1 & bc_pe2==2&biparentales==1 ~ 1))
sem2_implant <- sem2_implant %>% dplyr::mutate(jefeinact_desoc_m = case_when(bc_pobp>2 & bc_pobp<12 & jefe==1 & bc_pe2==2&biparentales==1 ~ 1))

#CONYUGE Inactivo o desempleado_varón*
sem1 <- sem1 %>% dplyr::mutate(conyinact_desoc_v = case_when(bc_pobp>2 & bc_pobp<12 & esposo_companero==1 & bc_pe2==1&biparentales==1 ~ 1))
sem2_implant <- sem2_implant %>% dplyr::mutate(conyinact_desoc_v = case_when(bc_pobp>2 & bc_pobp<12 & esposo_companero==1 & bc_pe2==1&biparentales==1 ~ 1))

#CONYUGE Inactivo o desempleado_mujer*
sem1 <- sem1 %>% dplyr::mutate(conyinact_desoc_m = case_when(bc_pobp>2 & bc_pobp<12 & esposo_companero==1 & bc_pe2==2&biparentales==1 ~ 1))
sem2_implant <- sem2_implant %>% dplyr::mutate(conyinact_desoc_m = case_when(bc_pobp>2 & bc_pobp<12 & esposo_companero==1 & bc_pe2==2&biparentales==1 ~ 1))



sem1 <- mutate_at(sem1, c("jefeocup_tc_v", "jefeocup_tc_m", "conyocup_tc_v", "conyocup_tc_m", "jefeocup_mt_v", "jefeocup_mt_m", 
                          "conyocup_mt_v", "conyocup_mt_m", "jefeinact_desoc_v", "jefeinact_desoc_m", "conyinact_desoc_v", "conyinact_desoc_m"), ~replace(., is.na(.), 0))
sem2_implant <- mutate_at(sem2_implant, c("jefeocup_tc_v", "jefeocup_tc_m", "conyocup_tc_v", "conyocup_tc_m", "jefeocup_mt_v", "jefeocup_mt_m", 
                          "conyocup_mt_v", "conyocup_mt_m", "jefeinact_desoc_v", "jefeinact_desoc_m", "conyinact_desoc_v", "conyinact_desoc_m"), ~replace(., is.na(.), 0))

        
#### Segundo, variables agregadas a nivel hogar
sem1 <- sem1 %>% group_by(bc_correlat) %>% mutate(sumjefeocup_tc_m=sum(jefeocup_tc_m)) %>% as.data.frame()
sem1 <- sem1 %>% group_by(bc_correlat) %>% mutate(sumjefeocup_tc_v=sum(jefeocup_tc_v)) %>% as.data.frame()
sem1 <- sem1 %>% group_by(bc_correlat) %>% mutate(sumconyocup_tc_m=sum(conyocup_tc_m)) %>% as.data.frame()
sem1 <- sem1 %>% group_by(bc_correlat) %>% mutate(sumconyocup_tc_v=sum(conyocup_tc_v)) %>% as.data.frame()
sem1 <- sem1 %>% group_by(bc_correlat) %>% mutate(sumjefeocup_mt_m=sum(jefeocup_mt_m)) %>% as.data.frame()
sem1 <- sem1 %>% group_by(bc_correlat) %>% mutate(sumjefeocup_mt_v=sum(jefeocup_mt_v)) %>% as.data.frame()
sem1 <- sem1 %>% group_by(bc_correlat) %>% mutate(sumconyocup_mt_v=sum(conyocup_mt_v)) %>% as.data.frame()
sem1 <- sem1 %>% group_by(bc_correlat) %>% mutate(sumconyocup_mt_m=sum(conyocup_mt_m)) %>% as.data.frame()
sem1 <- sem1 %>% group_by(bc_correlat)  %>% mutate(sumjefeinact_desoc_m=sum(jefeinact_desoc_m)) %>% as.data.frame()
sem1 <- sem1 %>% group_by(bc_correlat) %>% mutate(sumjefeinact_desoc_v=sum(jefeinact_desoc_v)) %>% as.data.frame()
sem1 <- sem1 %>% group_by(bc_correlat) %>% mutate(sumconyinact_desoc_m=sum(conyinact_desoc_m)) %>% as.data.frame()
sem1 <- sem1 %>% group_by(bc_correlat) %>% mutate(sumconyinact_desoc_v=sum(conyinact_desoc_v)) %>% as.data.frame()

sem2_implant <- sem2_implant %>% group_by(bc_correlat) %>% mutate(sumjefeocup_tc_m=sum(jefeocup_tc_m)) %>% as.data.frame()
sem2_implant <- sem2_implant %>% group_by(bc_correlat) %>% mutate(sumjefeocup_tc_v=sum(jefeocup_tc_v)) %>% as.data.frame()
sem2_implant <- sem2_implant %>% group_by(bc_correlat) %>% mutate(sumconyocup_tc_m=sum(conyocup_tc_m)) %>% as.data.frame()
sem2_implant <- sem2_implant %>% group_by(bc_correlat) %>% mutate(sumconyocup_tc_v=sum(conyocup_tc_v)) %>% as.data.frame()
sem2_implant <- sem2_implant %>% group_by(bc_correlat) %>% mutate(sumjefeocup_mt_m=sum(jefeocup_mt_m)) %>% as.data.frame()
sem2_implant <- sem2_implant %>% group_by(bc_correlat) %>% mutate(sumjefeocup_mt_v=sum(jefeocup_mt_v)) %>% as.data.frame()
sem2_implant <- sem2_implant %>% group_by(bc_correlat) %>% mutate(sumconyocup_mt_v=sum(conyocup_mt_v)) %>% as.data.frame()
sem2_implant <- sem2_implant %>% group_by(bc_correlat) %>% mutate(sumconyocup_mt_m=sum(conyocup_mt_m)) %>% as.data.frame()
sem2_implant <- sem2_implant %>% group_by(bc_correlat) %>% mutate(sumjefeinact_desoc_m=sum(jefeinact_desoc_m)) %>% as.data.frame()
sem2_implant <- sem2_implant %>% group_by(bc_correlat) %>% mutate(sumjefeinact_desoc_v=sum(jefeinact_desoc_v)) %>% as.data.frame()
sem2_implant <- sem2_implant %>% group_by(bc_correlat) %>% mutate(sumconyinact_desoc_m=sum(conyinact_desoc_m)) %>% as.data.frame()
sem2_implant <- sem2_implant %>% group_by(bc_correlat) %>% mutate(sumconyinact_desoc_v=sum(conyinact_desoc_v)) %>% as.data.frame()


#### Generamos la variable de modelos de pareja con las categorías de arriba
#1. PROVEEDOR TRADICIONAL (solo hombre trabaja, mujer inactiva o desempleada)*
#2 PROVEEDOR MODIFICADO: pareja donde ambos trabajan para el mercado pero el hombre trabaja a tiempo completo y la mujer a tiempo parcial. 
#3. DOBLE CARRERA:  pareja donde ambos trabajan para el mercado, ambos a tiempo completo o ambos a tiempo parcial. 
#4. INVERSIÓN DE ROLES: pareja donde sÓlo la mujer trabaja para el mercado y el hombre es inactivo o desocupado. 
#5. INVERSIÓN DE ROLES MODIFICADO: ambos trabajan en el mercado laboral pero hombre ocupado a tiempo parcial y mujer ocupada a tiempo completo.
#6. MODELO RESIDUAL: Ambos no trabajan (desocupados o inactivos)

sem1 <- sem1 %>% dplyr::mutate(modelo_pareja = case_when(((sumjefeocup_tc_v==1 & sumconyinact_desoc_m==1)|(sumconyocup_tc_v==1 & sumjefeinact_desoc_m==1)|(sumjefeocup_mt_v==1 & sumconyinact_desoc_m==1)|(sumconyocup_mt_v==1 & sumjefeinact_desoc_m==1))&biparentales==1 ~ 1, 
                                                          ((sumjefeocup_tc_v==1 & sumconyocup_mt_m==1) | (sumconyocup_tc_v==1 & sumjefeocup_mt_m==1))&biparentales==1 ~ 2,
                                                          ((sumjefeocup_tc_v==1 & sumconyocup_tc_m==1) | (sumjefeocup_tc_m==1 & sumconyocup_tc_v==1)| (sumjefeocup_mt_m==1 & sumconyocup_mt_v==1) | (sumjefeocup_mt_v==1 & sumconyocup_mt_m==1))&biparentales==1 ~3,
                                                          ((sumjefeocup_tc_m==1 & sumconyinact_desoc_v==1) | (sumconyocup_tc_m==1 & sumjefeinact_desoc_v==1)| (sumjefeocup_mt_m==1 & sumconyinact_desoc_v==1) | (sumconyocup_mt_m==1 & sumjefeinact_desoc_v==1))&biparentales==1 ~4,
                                                          ((sumjefeocup_tc_m==1 & sumconyocup_mt_v==1) | (sumconyocup_tc_m==1 & sumjefeocup_mt_v==1))&biparentales==1 ~ 5, 
                                                          ((sumjefeinact_desoc_m==1 & sumconyinact_desoc_v==1) | (sumjefeinact_desoc_v==1 & sumconyinact_desoc_m==1))&biparentales==1 ~6))
sem2_implant <- sem2_implant %>% dplyr::mutate(modelo_pareja = case_when(((sumjefeocup_tc_v==1 & sumconyinact_desoc_m==1)|(sumconyocup_tc_v==1 & sumjefeinact_desoc_m==1)|(sumjefeocup_mt_v==1 & sumconyinact_desoc_m==1)|(sumconyocup_mt_v==1 & sumjefeinact_desoc_m==1))&biparentales==1 ~ 1, 
                                                         ((sumjefeocup_tc_v==1 & sumconyocup_mt_m==1) | (sumconyocup_tc_v==1 & sumjefeocup_mt_m==1))&biparentales==1 ~ 2,
                                                         ((sumjefeocup_tc_v==1 & sumconyocup_tc_m==1) | (sumjefeocup_tc_m==1 & sumconyocup_tc_v==1)| (sumjefeocup_mt_m==1 & sumconyocup_mt_v==1) | (sumjefeocup_mt_v==1 & sumconyocup_mt_m==1))&biparentales==1 ~3,
                                                         ((sumjefeocup_tc_m==1 & sumconyinact_desoc_v==1) | (sumconyocup_tc_m==1 & sumjefeinact_desoc_v==1)| (sumjefeocup_mt_m==1 & sumconyinact_desoc_v==1) | (sumconyocup_mt_m==1 & sumjefeinact_desoc_v==1))&biparentales==1 ~4,
                                                         ((sumjefeocup_tc_m==1 & sumconyocup_mt_v==1) | (sumconyocup_tc_m==1 & sumjefeocup_mt_v==1))&biparentales==1 ~ 5, 
                                                         ((sumjefeinact_desoc_m==1 & sumconyinact_desoc_v==1) | (sumjefeinact_desoc_v==1 & sumconyinact_desoc_m==1))&biparentales==1 ~6))


