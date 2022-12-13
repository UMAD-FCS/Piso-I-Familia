#Tipo de hogar


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


#Constuyendo tipo de hogar#
  
#1.UNIPERSONAL 
#2. PAREJA SIN HIJOS
#3. BIPARENTAL 
#4. MONOPARENTAL FEMENINO 
#5. MONOPARENTAL MASCULINO 
#6. EXTENDIDO O COMPUESTO 
#7. EXTENDIDO O COMPUESTO CON NÚCLEO MONOPARENTAL
#8. SIN NÚCLEO CONYUGAL

sem1 <- sem1 %>% dplyr::mutate(tipo_hogar = case_when(sumesposo_companero==0 & sumhijo==0 & sumno_pariente==0 & sumotro_pariente==0 ~ 1, 
                                                      sumesposo_companero>=1 & sumhijo==0 & sumotro_pariente==0 & sumno_pariente==0 ~ 2, 
                                                      (sumesposo_companero>=1 & sumhijo>=1)& sumotro_pariente==0 & sumno_pariente==0 ~ 3, 
                                                      sumesposo_companero==0 & sumhijo>=1 & sumotro_pariente==0 & sumjefatura_femenina==1 & sumno_pariente==0 ~ 4, 
                                                      sumesposo_companero==0 & sumhijo>=1 & sumotro_pariente==0 & sumjefatura_femenina==0 & sumno_pariente==0 ~ 5, 
                                                      (sumotro_pariente>=1 & sumno_pariente==0)| sumno_pariente>=1 ~ 6, 
                                                      (sumesposo_companero==0 &sumhijo>0&sumjefatura_femenina==1&sumotro_pariente>=1) | (sumesposo_companero==0 &sumhijo>0&sumjefatura_femenina==1&sumno_pariente>=1) ~ 7,
                                                      (sumotro_pariente >=1 | sumno_pariente >=1) & (sumesposo_companero==0 & sumhijo==0) ~ 8))
sem2_implant <- sem2_implant %>% dplyr::mutate(tipo_hogar = case_when (sumesposo_companero==0 & sumhijo==0 & sumno_pariente==0 & sumotro_pariente==0 ~ 1, 
                                                                   sumesposo_companero>=1 & sumhijo==0 & sumotro_pariente==0 & sumno_pariente==0 ~ 2, 
                                                                   (sumesposo_companero>=1 & sumhijo>=1)& sumotro_pariente==0 & sumno_pariente==0 ~ 3, 
                                                                   sumesposo_companero==0 & sumhijo>=1 & sumotro_pariente==0 & sumjefatura_femenina==1 & sumno_pariente==0 ~ 4, 
                                                                   sumesposo_companero==0 & sumhijo>=1 & sumotro_pariente==0 & sumjefatura_femenina==0 & sumno_pariente==0 ~ 5, 
                                                                   (sumotro_pariente>=1 & sumno_pariente==0)| sumno_pariente>=1 ~ 6, 
                                                                   (sumesposo_companero==0 &sumhijo>0&sumjefatura_femenina==1&sumotro_pariente>=1) | (sumesposo_companero==0 &sumhijo>0&sumjefatura_femenina==1&sumno_pariente>=1) ~ 7,
                                                                   (sumotro_pariente >=1 | sumno_pariente >=1) & (sumesposo_companero==0 & sumhijo==0) ~ 8))








