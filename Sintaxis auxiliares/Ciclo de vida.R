#Ciclo de vida del hogar

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



sem1 <- sem1 %>% dplyr::mutate(mujmen40 = case_when((bc_pe4==1 | bc_pe4==2) & bc_pe2==2 & bc_pe3<40 ~ 1, 
                                                    (bc_pe4!=1 & bc_pe4!=2) | bc_pe2!=2 | bc_pe3>=40 ~ 0))
sem2_implant <- sem2_implant %>% dplyr::mutate(mujmen40 = case_when((bc_pe4==1 | bc_pe4==2) & bc_pe2==2 & bc_pe3<40 ~ 1, 
                                                                    (bc_pe4!=1 & bc_pe4!=2) | bc_pe2!=2 | bc_pe3>=40 ~ 0))

sem1 <- sem1 %>% dplyr::mutate(mujmay40 = case_when((bc_pe4==1 | bc_pe4==2) & bc_pe2==2 & bc_pe3>=40 ~ 1, 
                                                    (bc_pe4!=1 & bc_pe4!=2) | bc_pe2!=2 | bc_pe3<40 ~ 0))
sem2_implant <- sem2_implant %>% dplyr::mutate(mujmay40 = case_when((bc_pe4==1 | bc_pe4==2) & bc_pe2==2 & bc_pe3>=40 ~ 1, 
                                                                    (bc_pe4!=1 & bc_pe4!=2) | bc_pe2!=2 | bc_pe3<40 ~ 0))


sem1 <- sem1 %>% dplyr::mutate(hijo_men5 = case_when(hijos==1 & bc_pe3<6 ~ 1, 
                                                     (hijos!=1 | bc_pe3>=6 ~ 0)))
sem2_implant <- sem2_implant %>% dplyr::mutate(hijo_men5 = case_when(hijos==1 & bc_pe3<6 ~ 1, 
                                                                      (hijos!=1 | bc_pe3>=6 ~ 0)))

sem1 <- sem1 %>% dplyr::mutate(hijo_6a12 = case_when(hijos==1 & bc_pe3>5&bc_pe3<13 ~ 1, 
                              (hijos!=1 | (bc_pe3<6 | bc_pe3>12) ~ 0)))
sem2_implant <- sem2_implant %>% dplyr::mutate(hijo_6a12 = case_when(hijos==1 &  bc_pe3>5&bc_pe3<13 ~ 1, 
                                              (hijos!=1 |  bc_pe3<6|bc_pe3>12) ~ 0))
                                                                                                                                                                                               

sem1 <- sem1 %>% dplyr::mutate(hijo_may12 = case_when(hijos==1 & bc_pe3>12 ~ 1, 
                                                      (hijos!=1 | (bc_pe3<13) ~ 0)))
sem2_implant <- sem2_implant %>% dplyr::mutate(hijo_may12 = case_when(hijos==1 &  bc_pe3>12 ~ 1, 
                                                                      (hijos!=1 |  bc_pe3<13) ~ 0))
                                                                                                                                                                         
sem1 <- sem1 %>% dplyr::mutate(hijo_menos12 = case_when(hijos==1 & (hijo_6a12==1 | hijo_men5==1) ~ 1, 
                                                        (hijos!=1 | (hijo_6a12!=1 & hijo_men5!=1) ~ 0)))
sem2_implant <- sem2_implant %>% dplyr::mutate(hijo_menos12 = case_when(hijos==1 &  (hijo_6a12==1 | hijo_men5==1) ~ 1, 
                                                                        (hijos!=1 |  (hijo_6a12!=1 & hijo_men5!=1)) ~ 0))
                                                                                                                                                                                                  
sem1 <- sem1 %>% dplyr::mutate(hijo_13y18 = case_when(hijos==1 & (bc_pe3>12 & bc_pe3<19) ~ 1, 
                                                      (hijos!=1 | (bc_pe3<13 | bc_pe3>18) ~ 0)))
sem2_implant <- sem2_implant %>% dplyr::mutate(hijo_13y18 = case_when(hijos==1 &  (bc_pe3>12 & bc_pe3<19) ~ 1, 
                                                                      (hijos!=1 |  (bc_pe3<13 | bc_pe3>18)) ~ 0 ))
                                                                                                                                                                                                    
sem1 <- sem1 %>% dplyr::mutate(hijo_may18 = case_when(hijos==1 & bc_pe3>18 ~ 1, 
                                                        (hijos!=1 | bc_pe3<19 ~ 0)))
sem2_implant <- sem2_implant %>% dplyr::mutate(hijo_may18 = case_when(hijos==1 &  bc_pe3>18 ~ 1, 
                                                                     (hijos!=1 | bc_pe3<19) ~ 0))
                                                                                                                                                                                                                                                                                                                                     
                                                                                                                                                                                                                                                                                                                                     
sem1 <- sem1 %>% group_by(bc_correlat) %>% mutate(sumhijo_men5 =sum(hijo_men5)) %>% as.data.frame()
sem2_implant <- sem2_implant %>% group_by(bc_correlat) %>% mutate(sumhijo_men5=sum(hijo_men5)) %>% as.data.frame()

sem1 <- sem1 %>% group_by(bc_correlat) %>% mutate(sumhijo_6a12 =sum(hijo_6a12)) %>% as.data.frame()
sem2_implant <- sem2_implant %>% group_by(bc_correlat) %>% mutate(sumhijo_6a12=sum(hijo_6a12)) %>% as.data.frame()

sem1 <- sem1 %>% group_by(bc_correlat) %>% mutate(sumhijo_may12 =sum(hijo_may12)) %>% as.data.frame()
sem2_implant <- sem2_implant %>% group_by(bc_correlat) %>% mutate(sumhijo_may12=sum(hijo_may12)) %>% as.data.frame()

sem1 <- sem1 %>% group_by(bc_correlat) %>% mutate(sumhijo_menos12 =sum(hijo_menos12)) %>% as.data.frame()
sem2_implant <- sem2_implant %>% group_by(bc_correlat) %>% mutate(sumhijo_menos12=sum(hijo_menos12)) %>% as.data.frame()

sem1 <- sem1 %>% group_by(bc_correlat) %>% mutate(sumhijo_13y18 =sum(hijo_13y18)) %>% as.data.frame()
sem2_implant <- sem2_implant %>% group_by(bc_correlat) %>% mutate(sumhijo_13y18=sum(hijo_13y18)) %>% as.data.frame()

sem1 <- sem1 %>% group_by(bc_correlat) %>% mutate(sumhijo_may18 =sum(hijo_may18)) %>% as.data.frame()
sem2_implant <- sem2_implant %>% group_by(bc_correlat) %>% mutate(sumhijo_may18=sum(hijo_may18)) %>% as.data.frame()

sem1 <- sem1 %>% group_by(bc_correlat) %>% mutate(summujmen40 =sum(mujmen40)) %>% as.data.frame()
sem2_implant <- sem2_implant %>% group_by(bc_correlat) %>% mutate(summujmen40=sum(mujmen40)) %>% as.data.frame()

sem1 <- sem1 %>% group_by(bc_correlat) %>% mutate(summujmay40 =sum(mujmay40)) %>% as.data.frame()
sem2_implant <- sem2_implant %>% group_by(bc_correlat) %>% mutate(summujmay40=sum(mujmay40)) %>% as.data.frame()



#Constuyendo ciclo de vida del hogar#

#1. Pareja joven sin hijos 
#2. Etapa inicial
#3. Etapa expansión o crecimiento
#4. Etapa consolidación 
#5. Etapa de salida 
#6. Nido vacío (pareja mayor sin hijos) 
#7. Hogares no familaires



sem1 <- sem1 %>% dplyr::mutate(ciclo_vida = case_when(sumesposo_companero>=1 & sumhijo==0 & summujmen40==1 ~ 1, 
                                                     (sumhijo>0 ) & (sumhijo_men5>0) & sumhijo_6a12==0 & sumhijo_may12==0 ~ 2, 
                                                     (sumhijo>0 ) & ((sumhijo_men5>0) & (sumhijo_6a12>0)) | (sumhijo_men5==0 & (sumhijo_6a12>0))& sumhijo_may12==0 ~ 3, 
                                                     (sumhijo>0 ) & (sumhijo_13y18>0) ~ 4, 
                                                     (sumhijo>0 ) & (sumhijo_men5==0 & sumhijo_6a12==0 & sumhijo_13y18==0 & (sumhijo_may18>0)) ~ 5, 
                                                      sumesposo_companero>=1 & sumhijo==0 & summujmay40==1 ~ 6, 
                                                    ((sumotro_pariente >=1 | sumno_pariente >=1) & (sumesposo_companero==0 & sumhijo==0))|(sumesposo_companero==0 & sumhijo==0 & sumno_pariente==0 & sumotro_pariente==0) ~ 7))



sem2_implant <- sem2_implant %>% dplyr::mutate(ciclo_vida = case_when (sumesposo_companero>=1 & sumhijo==0 & summujmen40==1 ~ 1, 
                                                                   (sumhijo>0 ) & (sumhijo_men5>0) & sumhijo_6a12==0 & sumhijo_may12==0 ~ 2, 
                                                                   (sumhijo>0 ) & ((sumhijo_men5>0) & (sumhijo_6a12>0)) | (sumhijo_men5==0 & (sumhijo_6a12>0))& sumhijo_may12==0 ~ 3, 
                                                                   (sumhijo>0 ) & (sumhijo_13y18>0) ~ 4, 
                                                                   (sumhijo>0 ) & (sumhijo_men5==0 & sumhijo_6a12==0 & sumhijo_13y18==0 & (sumhijo_may18>0)) ~ 5, 
                                                                   sumesposo_companero>=1 & sumhijo==0 & summujmay40==1 ~ 6, 
                                                                   ((sumotro_pariente >=1 | sumno_pariente >=1) & (sumesposo_companero==0 & sumhijo==0))|(sumesposo_companero==0 & sumhijo==0 & sumno_pariente==0 & sumotro_pariente==0) ~ 7))






