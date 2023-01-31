### 563 Porcentaje de niños menores de 18 años sin cobertura de salud

## País Urbano

# Total

NOMINDICADOR <- 
  c("Porcentaje de niños menores de 18 años sin cobertura de salud")

a_mes <- sem1_svy %>%
  srvyr::filter(bd_region == 1 & bc_pe3<18) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(nocobert))
a_sem <- mean(as.numeric(a_mes$colname))

b_sem <- sem2_implant_svy %>%
  srvyr::filter(bd_region == 1 & bc_pe3<18) %>%
  srvyr::summarise(colname = srvyr::survey_mean(nocobert))
b_sem <- as.numeric(b_sem$colname)

c_ano <- mean(c(a_sem, b_sem))

c_ano <- cbind(c_ano, NOMINDICADOR)
colnames(c_ano) <- c("VALOR", "NOMINDICADOR")
c_ano <- as.data.frame(c_ano)
BASE_MOTOR <- cbind(c_ano,BASE_AUX)


# Sexo 

a_sexo <- function(x) {
  x <- sem1_svy %>%
    filter(bc_pe2 == x & bd_region == 1  & bc_pe3<18) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(nocobert))
  x <- mean(x$colname)
}       

a_e_sexo <- numeric()

for(i in 1:2){
  a_e_sexo[i] <- a_sexo(x = i)
}     

b_sexo <- function(x) {
  x <- sem2_implant_svy %>%
    filter(bc_pe2 == x  & bd_region == 1  & bc_pe3<18) %>%
    srvyr::summarise(colname = srvyr::survey_mean(nocobert))
  x <- mean(x$colname)
}       

b_e_sexo <- numeric()

for(i in 1:2){
  b_e_sexo[i] <- b_sexo(x = i)
}         

c_sexo <- as.data.frame(cbind(a_e_sexo, b_e_sexo))
c_sexo <- c_sexo %>% dplyr::mutate(m_sexo = (a_e_sexo + b_e_sexo)/2)
c_sexo <- c_sexo[,c("m_sexo")]
c_sexo_1 <- cbind(as.data.frame(c_sexo[1]), NOMINDICADOR, BASE_AUX)
c_sexo_1 <- c_sexo_1 %>% dplyr::mutate(SEXO = "VARONES")
c_sexo_2 <- cbind(as.data.frame(c_sexo[2]), NOMINDICADOR, BASE_AUX)
c_sexo_2 <- c_sexo_2 %>% dplyr::mutate(SEXO = "MUJERES")
colnames(c_sexo_1) <- colnames(BASE_MOTOR)
colnames(c_sexo_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_sexo_1,c_sexo_2)



# Quintil de ingreso del hogar

a_quintil <- function(x) {
  x <- sem1_svy %>%
    filter(quintilesy == x & bd_region == 1  & bc_pe3<18) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(nocobert))
  x <- mean(x$colname)
}       

a_e_quintil <- numeric()

for(i in 1:5){
  a_e_quintil[i] <- a_quintil(x = i)
}     

b_quintil <- function(x) {
  x <- sem2_implant_svy %>%
    filter(quintilesy == x  & bd_region == 1  & bc_pe3<18) %>%
    srvyr::summarise(colname = srvyr::survey_mean(nocobert))
  x <- mean(x$colname)
}       

b_e_quintil <- numeric()

for(i in 1:5){
  b_e_quintil[i] <- b_quintil(x = i)
}         

c_quintil <- as.data.frame(cbind(a_e_quintil, b_e_quintil))
c_quintil <- c_quintil %>% dplyr::mutate(m_quintil = (a_e_quintil + b_e_quintil)/2)
c_quintil <- c_quintil[,c("m_quintil")]
c_quintil_1 <- cbind(as.data.frame(c_quintil[1]), NOMINDICADOR, BASE_AUX)
c_quintil_1 <- c_quintil_1 %>% dplyr::mutate(QUINTIL = "Quintil 1")
c_quintil_2 <- cbind(as.data.frame(c_quintil[2]), NOMINDICADOR, BASE_AUX)
c_quintil_2 <- c_quintil_2 %>% dplyr::mutate(QUINTIL = "Quintil 2")
c_quintil_3 <- cbind(as.data.frame(c_quintil[3]), NOMINDICADOR, BASE_AUX)
c_quintil_3 <- c_quintil_3 %>% dplyr::mutate(QUINTIL = "Quintil 3")
c_quintil_4 <- cbind(as.data.frame(c_quintil[4]), NOMINDICADOR, BASE_AUX)
c_quintil_4 <- c_quintil_4 %>% dplyr::mutate(QUINTIL = "Quintil 4")
c_quintil_5 <- cbind(as.data.frame(c_quintil[5]), NOMINDICADOR, BASE_AUX)
c_quintil_5 <- c_quintil_4 %>% dplyr::mutate(QUINTIL = "Quintil 5")
colnames(c_quintil_1) <- colnames(BASE_MOTOR)
colnames(c_quintil_2) <- colnames(BASE_MOTOR)
colnames(c_quintil_3) <- colnames(BASE_MOTOR)
colnames(c_quintil_4) <- colnames(BASE_MOTOR)
colnames(c_quintil_5) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_quintil_1,c_quintil_2, c_quintil_3, c_quintil_4, c_quintil_5)




# Base motor

BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(CODIND = "563")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(CATEGORIA = "Familia y composición de los hogares")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(PESTAÑA = "País urbano")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(PAÍS = "Uruguay")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(ANIO = 2021)
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(RESPONSABLE = "Sharon Katzkowicz")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(ASCENDENCIA = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(DEPARTAMENTOUY = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(URBANORURALUY = "Urbano (más de 5.000 habitantes)")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(POBRE = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(NSE = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(NIVELEDU = "Todos")
BASE_MOTOR <- as.data.frame(BASE_MOTOR)

BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(SEXO = case_when(SEXO == "" ~ "Todos", SEXO == "MUJERES"~"Mujeres", SEXO=="VARONES"~"Varones"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(QUINTIL = case_when(QUINTIL == "" ~ "Todos",
                                                               QUINTIL == "Quintil 1"~"Quintil 1", 
                                                               QUINTIL == "Quintil 2"~"Quintil 2",
                                                               QUINTIL == "Quintil 3"~"Quintil 3", 
                                                               QUINTIL == "Quintil 4"~"Quintil 4",
                                                               QUINTIL == "Quintil 5"~"Quintil 5"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(EDAD = case_when(EDAD == "" ~ "Todos"))


m563_pu <- BASE_MOTOR %>% select(CODIND,	
                                 NOMINDICADOR,	
                                 CATEGORIA,	
                                 PESTAÑA,	
                                 SEXO,	
                                 ASCENDENCIA,	
                                 QUINTIL,	
                                 DEPARTAMENTOUY,	
                                 URBANORURALUY,	
                                 EDAD,	
                                 POBRE,
                                 NSE,
                                 NIVELEDU,
                                 PAÍS,	
                                 ANIO,	
                                 VALOR,	
                                 RESPONSABLE)	


## Total país

# Total


NOMINDICADOR <- 
  c("Porcentaje de niños menores de 18 años sin cobertura de salud")

a_mes <- sem1_svy %>%
  filter(bc_pe3<18) %>%
  srvyr::group_by(mes) %>%
  srvyr::summarise(colname = srvyr::survey_mean(nocobert))
a_sem <- mean(as.numeric(a_mes$colname))

b_sem <- sem2_implant_svy %>%
  filter(bc_pe3<18) %>%
  srvyr::summarise(colname = srvyr::survey_mean(nocobert))
b_sem <- as.numeric(b_sem$colname)

c_ano <- mean(c(a_sem, b_sem))

c_ano <- cbind(c_ano, NOMINDICADOR)
colnames(c_ano) <- c("VALOR", "NOMINDICADOR")
c_ano <- as.data.frame(c_ano)
BASE_MOTOR <- cbind(c_ano,BASE_AUX)



# Región

a_reg <- function(x) {
  x <- sem1_svy %>%
    filter(bd_region == x & bc_pe3<18) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(nocobert))
  x <- mean(x$colname)
}       

a_e_reg <- numeric()

for(i in 1:2){
  a_e_reg[i] <- a_reg(x = i)
}     

b_reg <- function(x) {
  x <- sem2_implant_svy %>%
    filter(bd_region == x  & bc_pe3<18) %>%
    srvyr::summarise(colname = srvyr::survey_mean(nocobert))
  x <- mean(x$colname)
}       

b_e_reg <- numeric()

for(i in 1:2){
  b_e_reg[i] <- b_reg(x = i)
}         

c_reg <- as.data.frame(cbind(a_e_reg[2:3], b_e_reg[2:3]))
c_reg <- c_reg %>% dplyr::mutate(m_reg = (a_e_reg[2:3] + b_e_reg[2:3])/2)
c_reg <- c_reg[,c("m_reg")]
c_reg_1 <- cbind(as.data.frame(c_reg[1]), NOMINDICADOR, BASE_AUX)
c_reg_1 <- c_reg_1 %>% dplyr::mutate(URBANORURALUY = "URBANO (MENOS DE 5.000 HABITANTES)")
c_reg_2 <- cbind(as.data.frame(c_reg[2]), NOMINDICADOR, BASE_AUX)
c_reg_2 <- c_reg_2 %>% dplyr::mutate(URBANORURALUY = "RURAL DISPERSO")
colnames(c_reg_1) <- colnames(BASE_MOTOR)
colnames(c_reg_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_reg_1,c_reg_2)


# Sexo 

a_sexo <- function(x) {
  x <- sem1_svy %>%
    filter(bc_pe2 == x  & bc_pe3<18) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(nocobert))
  x <- mean(x$colname)
}       

a_e_sexo <- numeric()

for(i in 1:2){
  a_e_sexo[i] <- a_sexo(x = i)
}     

b_sexo <- function(x) {
  x <- sem2_implant_svy %>%
    filter(bc_pe2 == x  & bc_pe3<18) %>%
    srvyr::summarise(colname = srvyr::survey_mean(nocobert))
  x <- mean(x$colname)
}       

b_e_sexo <- numeric()

for(i in 1:2){
  b_e_sexo[i] <- b_sexo(x = i)
}         

c_sexo <- as.data.frame(cbind(a_e_sexo, b_e_sexo))
c_sexo <- c_sexo %>% dplyr::mutate(m_sexo = (a_e_sexo + b_e_sexo)/2)
c_sexo <- c_sexo[,c("m_sexo")]
c_sexo_1 <- cbind(as.data.frame(c_sexo[1]), NOMINDICADOR, BASE_AUX)
c_sexo_1 <- c_sexo_1 %>% dplyr::mutate(SEXO = "VARONES")
c_sexo_2 <- cbind(as.data.frame(c_sexo[2]), NOMINDICADOR, BASE_AUX)
c_sexo_2 <- c_sexo_2 %>% dplyr::mutate(SEXO = "MUJERES")
colnames(c_sexo_1) <- colnames(BASE_MOTOR)
colnames(c_sexo_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_sexo_1,c_sexo_2)



# Ascendencia

a_ascen <- function(x) {
  x <- sem1_svy %>%
    filter(bd_e29_1 == x  & bc_pe3<18) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(nocobert))
  x <- mean(x$colname)
}       

a_e_ascen <- numeric()

for(i in 1:2){
  a_e_ascen[i] <- a_ascen(x = i)
}     

b_ascen <- function(x) {
  x <- sem2_implant_svy %>%
    filter(bd_e29_1 == x  & bc_pe3<18) %>%
    srvyr::summarise(colname = srvyr::survey_mean(nocobert))
  x <- mean(x$colname)
}       

b_e_ascen <- numeric()

for(i in 1:2){
  b_e_ascen[i] <- b_ascen(x = i)
}         

c_ascen <- as.data.frame(cbind(a_e_ascen, b_e_ascen))
c_ascen <- c_ascen %>% dplyr::mutate(m_ascen = (a_e_ascen + b_e_ascen)/2)
c_ascen <- c_ascen[,c("m_ascen")]
c_ascen_1 <- cbind(as.data.frame(c_ascen[1]), NOMINDICADOR, BASE_AUX)
c_ascen_1 <- c_ascen_1 %>% dplyr::mutate(ASCENDENCIA = "AFRO")
c_ascen_2 <- cbind(as.data.frame(c_ascen[2]), NOMINDICADOR, BASE_AUX)
c_ascen_2 <- c_ascen_2 %>% dplyr::mutate(ASCENDENCIA = "NO AFRO")
colnames(c_ascen_1) <- colnames(BASE_MOTOR)
colnames(c_ascen_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_ascen_1,c_ascen_2)


# Quintil de ingreso del hogar

a_quintil <- function(x) {
  x <- sem1_svy %>%
    filter(quintilesy == x  & bc_pe3<18) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(nocobert))
  x <- mean(x$colname)
}       

a_e_quintil <- numeric()

for(i in 1:5){
  a_e_quintil[i] <- a_quintil(x = i)
}     

b_quintil <- function(x) {
  x <- sem2_implant_svy %>%
    filter(quintilesy == x  & bc_pe3<18) %>%
    srvyr::summarise(colname = srvyr::survey_mean(nocobert))
  x <- mean(x$colname)
}       

b_e_quintil <- numeric()

for(i in 1:5){
  b_e_quintil[i] <- b_quintil(x = i)
}         

c_quintil <- as.data.frame(cbind(a_e_quintil, b_e_quintil))
c_quintil <- c_quintil %>% dplyr::mutate(m_quintil = (a_e_quintil + b_e_quintil)/2)
c_quintil <- c_quintil[,c("m_quintil")]
c_quintil_1 <- cbind(as.data.frame(c_quintil[1]), NOMINDICADOR, BASE_AUX)
c_quintil_1 <- c_quintil_1 %>% dplyr::mutate(QUINTIL = "Quintil 1")
c_quintil_2 <- cbind(as.data.frame(c_quintil[2]), NOMINDICADOR, BASE_AUX)
c_quintil_2 <- c_quintil_2 %>% dplyr::mutate(QUINTIL = "Quintil 2")
c_quintil_3 <- cbind(as.data.frame(c_quintil[3]), NOMINDICADOR, BASE_AUX)
c_quintil_3 <- c_quintil_3 %>% dplyr::mutate(QUINTIL = "Quintil 3")
c_quintil_4 <- cbind(as.data.frame(c_quintil[4]), NOMINDICADOR, BASE_AUX)
c_quintil_4 <- c_quintil_4 %>% dplyr::mutate(QUINTIL = "Quintil 4")
c_quintil_5 <- cbind(as.data.frame(c_quintil[5]), NOMINDICADOR, BASE_AUX)
c_quintil_5 <- c_quintil_4 %>% dplyr::mutate(QUINTIL = "Quintil 5")
colnames(c_quintil_1) <- colnames(BASE_MOTOR)
colnames(c_quintil_2) <- colnames(BASE_MOTOR)
colnames(c_quintil_3) <- colnames(BASE_MOTOR)
colnames(c_quintil_4) <- colnames(BASE_MOTOR)
colnames(c_quintil_5) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_quintil_1,c_quintil_2, c_quintil_3, c_quintil_4, c_quintil_5)


# Pobreza

a_pobre <- function(x) {
  x <- sem1_svy %>%
    filter(pobre_aux == x  & bc_pe3<18) %>%
    srvyr::group_by(mes) %>%
    srvyr::summarise(colname = srvyr::survey_mean(nocobert))
  x <- mean(x$colname)
}       

a_e_pobre <- numeric()

for(i in 1:2){
  a_e_pobre[i] <- a_pobre(x = i)
}     

b_pobre <- function(x) {
  x <- sem2_implant_svy %>%
    filter(pobre_aux == x & bc_pe3<18) %>%
    srvyr::summarise(colname = srvyr::survey_mean(nocobert))
  x <- mean(x$colname)
}       

b_e_pobre <- numeric()

for(i in 1:2){
  b_e_pobre[i] <- b_pobre(x = i)
}         

c_pobre <- as.data.frame(cbind(a_e_pobre, b_e_pobre))
c_pobre <- c_pobre %>% dplyr::mutate(m_pobre = (a_e_pobre + b_e_pobre)/2)
c_pobre <- c_pobre[,c("m_pobre")]
c_pobre_1 <- cbind(as.data.frame(c_pobre[1]), NOMINDICADOR, BASE_AUX)
c_pobre_1 <- c_pobre_1 %>% dplyr::mutate(POBRE = "POBRE")
c_pobre_2 <- cbind(as.data.frame(c_pobre[2]), NOMINDICADOR, BASE_AUX)
c_pobre_2 <- c_pobre_2 %>% dplyr::mutate(POBRE = "NO POBRE")
colnames(c_pobre_1) <- colnames(BASE_MOTOR)
colnames(c_pobre_2) <- colnames(BASE_MOTOR)
BASE_MOTOR <- rbind(BASE_MOTOR,c_pobre_1,c_pobre_2)


# Base motor

BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(CODIND = "563")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(CATEGORIA = "Familia y composición de los hogares")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(PESTAÑA = "Total país")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(PAÍS = "Uruguay")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(ANIO = 2021)
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(RESPONSABLE = "Sharon Katzkowicz")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(DEPARTAMENTOUY = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(NSE = "Todos")
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(NIVELEDU = "Todos")
BASE_MOTOR <- as.data.frame(BASE_MOTOR)


BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(ASCENDENCIA = case_when(ASCENDENCIA == "AFRO" ~ "Afro", 
                                                                   ASCENDENCIA == "NO AFRO" ~ "No afro",
                                                                   ASCENDENCIA == "" ~ "Todos"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(URBANORURALUY = case_when(URBANORURALUY == "URBANO (MENOS DE 5.000 HABITANTES)" ~ "Urbano (menos de 5.000 habitantes)",
                                                                     URBANORURALUY == "RURAL DISPERSO" ~ "Rural disperso", 
                                                                     URBANORURALUY == "" ~ "Todos"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(SEXO = case_when(SEXO == "MUJERES" ~ "Mujeres", 
                                                            SEXO=="VARONES" ~ "Varones",
                                                            SEXO == "" ~ "Todos"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(QUINTIL = case_when(QUINTIL == "Quintil 1"~"Quintil 1", 
                                                               QUINTIL == "Quintil 2"~"Quintil 2",
                                                               QUINTIL == "Quintil 3"~"Quintil 3", 
                                                               QUINTIL == "Quintil 4"~"Quintil 4",
                                                               QUINTIL == "Quintil 5"~"Quintil 5",
                                                               QUINTIL == "" ~ "Todos"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(EDAD = case_when(EDAD == "" ~ "Todos"))
BASE_MOTOR <- BASE_MOTOR %>% dplyr::mutate(POBRE = case_when(POBRE == "POBRE" ~ "Pobre", 
                                                             POBRE == "NO POBRE" ~ "No pobre",
                                                             POBRE == "" ~ "Todos"))


m563_tp <- BASE_MOTOR %>% select(CODIND,	
                                 NOMINDICADOR,	
                                 CATEGORIA,	
                                 PESTAÑA,	
                                 SEXO,	
                                 ASCENDENCIA,	
                                 QUINTIL,	
                                 DEPARTAMENTOUY,	
                                 URBANORURALUY,	
                                 EDAD,	
                                 POBRE,
                                 NSE,
                                 NIVELEDU,
                                 PAÍS,	
                                 ANIO,	
                                 VALOR,	
                                 RESPONSABLE)


