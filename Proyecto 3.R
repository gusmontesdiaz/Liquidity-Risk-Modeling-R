
######## Proyecto 3 Administración integral de Riesgos ###########

library(readxl)
library(ggplot2)
library(broom)

BaseDatosProyecto3 <- read_excel("C:/Users/carlo/Downloads/BaseDatosProyecto3.xlsx")
attach(BaseDatosProyecto3)
View(BaseDatosProyecto3)

#Estamos haciendo los modelos de regresión multiple para el cambio porcentual de las variables independientes.

###################################### Para TBM ###############################################


#################################### Para los Depósitos Vista


m1 <- lm(Depositos_Vista_TBM_ln~TDC_Total+Inflación+Desempleo+`PIB_LOG/100`+`PIB NORM`+TIIE+`Deuda P. NORM`+`Deuda_P_LOG/100`+Remesas+`Ind. Act. Industrial/100`+
           INPC+Pob_Econ_Act+IGAE+VaR_IGAE+Ind_Conf_Consum+Tipo_cambio+tipo_cambio_ln+INPC_log+Imp+IMFBCF+`UDIS  ln/100`+Tasa_objetivo_ln+
           Export_billonesdls_ln+Consumo_Priv_ln+Cetes+IGREMSE...27+IGREMSE...32+IGREMSE_Ln+Remun_empresas_construct+IGRESE_Ln+IGPOSE, data = BaseDatosProyecto3)

summary(m1)

#Corregiremos la regresión quitando las variables no significativas

m1 <- lm(Depositos_Vista_TBM_ln~TDC_Total+Desempleo+`PIB NORM`+
           TIIE+`Deuda P. NORM`+`Deuda_P_LOG/100`+Remesas+
           Tipo_cambio+tipo_cambio_ln+IMFBCF+Tasa_objetivo_ln+
           Consumo_Priv_ln+Cetes, data = BaseDatosProyecto3)
summary(m1)

#Corregiremos la regresión quitando las variables no significativas

m1 <- lm(Depositos_Vista_TBM_ln~TDC_Total+`PIB NORM`+
           TIIE+`Deuda P. NORM`+`Deuda_P_LOG/100`+Remesas+
           Tipo_cambio+tipo_cambio_ln+Tasa_objetivo_ln+
           Consumo_Priv_ln, data = BaseDatosProyecto3)
summary(m1)

#################################### Para los Depósitos Plazo

m2 <- lm(Depositos_Plazo_TBM_ln~TDC_Total+Inflación+Desempleo+`PIB_LOG/100`+`PIB NORM`+TIIE+`Deuda P. NORM`+`Deuda_P_LOG/100`+Remesas+`Ind. Act. Industrial/100`+
           INPC+Pob_Econ_Act+IGAE+VaR_IGAE+Ind_Conf_Consum+Tipo_cambio+tipo_cambio_ln+INPC_log+Imp+IMFBCF+`UDIS  ln/100`+Tasa_objetivo_ln+
           Export_billonesdls_ln+Consumo_Priv_ln+Cetes+IGREMSE...27+IGREMSE...32+IGREMSE_Ln+Remun_empresas_construct+IGRESE_Ln+IGPOSE, data = BaseDatosProyecto3)

summary(m2)

#Corregiremos la regresión quitando las variables no significativas


m2 <- lm(Depositos_Plazo_TBM_ln~TDC_Total+Inflación+TIIE+
           `Deuda_P_LOG/100`+IGAE+Ind_Conf_Consum+IMFBCF+
           Consumo_Priv_ln+IGREMSE...27+IGPOSE, data = BaseDatosProyecto3)
summary(m2)

#Corregiremos la regresión quitando las variables no significativas


m2 <- lm(Depositos_Plazo_TBM_ln~TIIE+`Deuda_P_LOG/100`+Ind_Conf_Consum+IMFBCF, data = BaseDatosProyecto3)
summary(m2)


#################################### Para la Captación Tradicional

m3 <- lm(CaptacioN_Tradicional_TBM_ln~TDC_Total+Inflación+Desempleo+`PIB_LOG/100`+`PIB NORM`+TIIE+`Deuda P. NORM`+`Deuda_P_LOG/100`+Remesas+`Ind. Act. Industrial/100`+
           INPC+Pob_Econ_Act+IGAE+VaR_IGAE+Ind_Conf_Consum+Tipo_cambio+tipo_cambio_ln+INPC_log+Imp+IMFBCF+`UDIS  ln/100`+Tasa_objetivo_ln+
           Export_billonesdls_ln+Consumo_Priv_ln+Cetes+IGREMSE...27+IGREMSE...32+IGREMSE_Ln+Remun_empresas_construct+IGRESE_Ln+IGPOSE, data = BaseDatosProyecto3)

summary(m3)


#Corregiremos la regresión quitando las variables no significativas

m3 <- lm(CaptacioN_Tradicional_TBM_ln~TDC_Total+
           `PIB NORM`+TIIE+`Deuda P. NORM`+Remesas+
           IGAE+Tasa_objetivo_ln+Consumo_Priv_ln, data = BaseDatosProyecto3)

summary(m3)

#Corregiremos la regresión quitando las variables no significativas

m3 <- lm(CaptacioN_Tradicional_TBM_ln~TDC_Total+
           `PIB NORM`+TIIE+`Deuda P. NORM`+Remesas+
           Tasa_objetivo_ln+Consumo_Priv_ln, data = BaseDatosProyecto3)

summary(m3)



###################################### Para Banamex ###############################################


#################################### Para los Depósitos Vista


m4 <- lm(Depositos_Vista_banamex_ln~TDC_Total+Inflación+Desempleo+`PIB_LOG/100`+`PIB NORM`+TIIE+`Deuda P. NORM`+`Deuda_P_LOG/100`+Remesas+`Ind. Act. Industrial/100`+
           INPC+Pob_Econ_Act+IGAE+VaR_IGAE+Ind_Conf_Consum+Tipo_cambio+tipo_cambio_ln+INPC_log+Imp+IMFBCF+`UDIS  ln/100`+Tasa_objetivo_ln+
           Export_billonesdls_ln+Consumo_Priv_ln+Cetes+IGREMSE...27+IGREMSE...32+IGREMSE_Ln+Remun_empresas_construct+IGRESE_Ln+IGPOSE, data = BaseDatosProyecto3)

summary(m4)

#Corregiremos la regresión quitando las variables no significativas

m4 <- lm(Depositos_Vista_TBM_ln~`Deuda P. NORM`+Remesas+tipo_cambio_ln+Tipo_cambio+Imp, data = BaseDatosProyecto3)
summary(m4)


#################################### Para los Depósitos Plazo

m5 <- lm(Depositos_Plazo_banamex_ln~TDC_Total+Inflación+Desempleo+`PIB_LOG/100`+`PIB NORM`+TIIE+`Deuda P. NORM`+`Deuda_P_LOG/100`+Remesas+`Ind. Act. Industrial/100`+
           INPC+Pob_Econ_Act+IGAE+VaR_IGAE+Ind_Conf_Consum+Tipo_cambio+tipo_cambio_ln+INPC_log+Imp+IMFBCF+`UDIS  ln/100`+Tasa_objetivo_ln+
           Export_billonesdls_ln+Consumo_Priv_ln+Cetes+IGREMSE...27+IGREMSE...32+IGREMSE_Ln+Remun_empresas_construct+IGRESE_Ln+IGPOSE, data = BaseDatosProyecto3)

summary(m5)

#Corregiremos la regresión quitando las variables no significativas


m5 <- lm(Depositos_Plazo_banamex_ln~Inflación+`Ind. Act. Industrial/100`+
           Ind_Conf_Consum+tipo_cambio_ln+Tipo_cambio+IMFBCF+IGREMSE...27+
           IGPOSE, data = BaseDatosProyecto3)
summary(m5)

#Corregiremos la regresión quitando las variables no significativas


m5 <- lm(Depositos_Plazo_banamex_ln~`Ind. Act. Industrial/100`+tipo_cambio_ln+Tipo_cambio+IGPOSE, data = BaseDatosProyecto3)
summary(m5)


#################################### Para la Captación Tradicional

m6 <- lm(Captación_tradicional_banamex_ln~TDC_Total+Inflación+Desempleo+`PIB_LOG/100`+`PIB NORM`+TIIE+`Deuda P. NORM`+`Deuda_P_LOG/100`+Remesas+`Ind. Act. Industrial/100`+
           INPC+Pob_Econ_Act+IGAE+VaR_IGAE+Ind_Conf_Consum+Tipo_cambio+tipo_cambio_ln+INPC_log+Imp+IMFBCF+`UDIS  ln/100`+Tasa_objetivo_ln+
           Export_billonesdls_ln+Consumo_Priv_ln+Cetes+IGREMSE...27+IGREMSE...32+IGREMSE_Ln+Remun_empresas_construct+IGRESE_Ln+IGPOSE, data = BaseDatosProyecto3)

summary(m6)


#Corregiremos la regresión quitando las variables no significativas

m6 <- lm(Captación_tradicional_banamex_ln~`Deuda P. NORM`+Remesas+tipo_cambio_ln+Imp, data = BaseDatosProyecto3)

summary(m6)


###################################### Para BBVA ###############################################


#################################### Para los Depósitos Vista


m7 <- lm(Depositos_Vista_BBVA_Ln~TDC_Total+Inflación+Desempleo+`PIB_LOG/100`+`PIB NORM`+TIIE+`Deuda P. NORM`+`Deuda_P_LOG/100`+Remesas+`Ind. Act. Industrial/100`+
           INPC+Pob_Econ_Act+IGAE+VaR_IGAE+Ind_Conf_Consum+Tipo_cambio+tipo_cambio_ln+INPC_log+Imp+IMFBCF+`UDIS  ln/100`+Tasa_objetivo_ln+
           Export_billonesdls_ln+Consumo_Priv_ln+Cetes+IGREMSE...27+IGREMSE...32+IGREMSE_Ln+Remun_empresas_construct+IGRESE_Ln+IGPOSE, data = BaseDatosProyecto3)

summary(m7)

#Corregiremos la regresión quitando las variables no significativas

m7 <- lm(Depositos_Vista_BBVA_Ln~Desempleo+TIIE+`Deuda_P_LOG/100`+Tasa_objetivo_ln+Consumo_Priv_ln+Cetes, data = BaseDatosProyecto3)
summary(m7)

#Corregiremos la regresión quitando las variables no significativas

m7 <- lm(Depositos_Vista_BBVA_Ln~Desempleo+TIIE+Consumo_Priv_ln+Cetes, data = BaseDatosProyecto3)
summary(m7)


#################################### Para los Depósitos Plazo

m8 <- lm(Depositos_Plazo_BBVA_Ln~TDC_Total+Inflación+Desempleo+`PIB_LOG/100`+`PIB NORM`+TIIE+`Deuda P. NORM`+`Deuda_P_LOG/100`+Remesas+`Ind. Act. Industrial/100`+
           INPC+Pob_Econ_Act+IGAE+VaR_IGAE+Ind_Conf_Consum+Tipo_cambio+tipo_cambio_ln+INPC_log+Imp+IMFBCF+`UDIS  ln/100`+Tasa_objetivo_ln+
           Export_billonesdls_ln+Consumo_Priv_ln+Cetes+IGREMSE...27+IGREMSE...32+IGREMSE_Ln+Remun_empresas_construct+IGRESE_Ln+IGPOSE, data = BaseDatosProyecto3)

summary(m8)

#Corregiremos la regresión quitando las variables no significativas


m8 <- lm(Depositos_Plazo_BBVA_Ln~ Inflación+TIIE +
           `Deuda P. NORM`+`Deuda_P_LOG/100`+IMFBCF+
           Consumo_Priv_ln+IGREMSE...32+IGREMSE_Ln+IGPOSE, data = BaseDatosProyecto3)
summary(m8)

#Corregiremos la regresión quitando las variables no significativas


m8 <- lm(Depositos_Plazo_BBVA_Ln~`Deuda P. NORM`+ Consumo_Priv_ln + IGREMSE...32 +
           IGREMSE_Ln+IGPOSE, data = BaseDatosProyecto3)
summary(m8)


#################################### Para la Captación Tradicional

m9 <- lm(Captación_tradicional_BBVA_Ln~TDC_Total+Inflación+Desempleo+`PIB_LOG/100`+`PIB NORM`+TIIE+`Deuda P. NORM`+`Deuda_P_LOG/100`+Remesas+`Ind. Act. Industrial/100`+
           INPC+Pob_Econ_Act+IGAE+VaR_IGAE+Ind_Conf_Consum+Tipo_cambio+tipo_cambio_ln+INPC_log+Imp+IMFBCF+`UDIS  ln/100`+Tasa_objetivo_ln+
           Export_billonesdls_ln+Consumo_Priv_ln+Cetes+IGREMSE...27+IGREMSE...32+IGREMSE_Ln+Remun_empresas_construct+IGRESE_Ln+IGPOSE, data = BaseDatosProyecto3)

summary(m9)


#Corregiremos la regresión quitando las variables no significativas

m9 <- lm(Captación_tradicional_BBVA_Ln~TIIE+`Deuda P. NORM`+`Deuda_P_LOG/100`+Consumo_Priv_ln, data = BaseDatosProyecto3)

summary(m9)

#Corregiremos la regresión quitando las variables no significativas

m9 <- lm(Captación_tradicional_BBVA_Ln~TIIE+`Deuda P. NORM` + Consumo_Priv_ln, data = BaseDatosProyecto3)

summary(m9)


###################################### Para Santander ###############################################


#################################### Para los Depósitos Vista


m10 <- lm(Depositos_Vista_Sant_Ln~TDC_Total+Inflación+Desempleo+`PIB_LOG/100`+`PIB NORM`+TIIE+`Deuda P. NORM`+`Deuda_P_LOG/100`+Remesas+`Ind. Act. Industrial/100`+
           INPC+Pob_Econ_Act+IGAE+VaR_IGAE+Ind_Conf_Consum+Tipo_cambio+tipo_cambio_ln+INPC_log+Imp+IMFBCF+`UDIS  ln/100`+Tasa_objetivo_ln+
           Export_billonesdls_ln+Consumo_Priv_ln+Cetes+IGREMSE...27+IGREMSE...32+IGREMSE_Ln+Remun_empresas_construct+IGRESE_Ln+IGPOSE, data = BaseDatosProyecto3)

summary(m10)

#Corregiremos la regresión quitando las variables no significativas

m10 <- lm(Depositos_Vista_Sant_Ln~TDC_Total+ Desempleo+`PIB NORM`+TIIE + 
            Remesas + Ind_Conf_Consum + Imp + Tasa_objetivo_ln + Cetes + 
            IGREMSE...27 + IGPOSE, data = BaseDatosProyecto3)
summary(m10)

#Corregiremos la regresión quitando las variables no significativas

m10 <- lm(Depositos_Vista_Sant_Ln~ TDC_Total + TIIE + Remesas + Imp + Tasa_objetivo_ln, data = BaseDatosProyecto3)
summary(m10)


#################################### Para los Depósitos Plazo

m11 <- lm(Depositos_Plazo_Sant_Ln~TDC_Total+Inflación+Desempleo+`PIB_LOG/100`+`PIB NORM`+TIIE+`Deuda P. NORM`+`Deuda_P_LOG/100`+Remesas+`Ind. Act. Industrial/100`+
           INPC+Pob_Econ_Act+IGAE+VaR_IGAE+Ind_Conf_Consum+Tipo_cambio+tipo_cambio_ln+INPC_log+Imp+IMFBCF+`UDIS  ln/100`+Tasa_objetivo_ln+
           Export_billonesdls_ln+Consumo_Priv_ln+Cetes+IGREMSE...27+IGREMSE...32+IGREMSE_Ln+Remun_empresas_construct+IGRESE_Ln+IGPOSE, data = BaseDatosProyecto3)

summary(m11)

#Corregiremos la regresión quitando las variables no significativas


m11 <- lm(Depositos_Plazo_Sant_Ln~ TDC_Total + `Deuda P. NORM` + 
            Pob_Econ_Act + IGAE + Ind_Conf_Consum + IMFBCF + IGREMSE...32 + 
            IGREMSE_Ln, data = BaseDatosProyecto3)
summary(m11)

#Corregiremos la regresión quitando las variables no significativas


m11 <- lm(Depositos_Plazo_Sant_Ln~`Deuda P. NORM`+ IGAE, data = BaseDatosProyecto3)
summary(m11)


#################################### Para la Captación Tradicional

m12 <- lm(Captación_tradicional_Sant_Ln~TDC_Total+Inflación+Desempleo+`PIB_LOG/100`+`PIB NORM`+TIIE+`Deuda P. NORM`+`Deuda_P_LOG/100`+Remesas+`Ind. Act. Industrial/100`+
           INPC+Pob_Econ_Act+IGAE+VaR_IGAE+Ind_Conf_Consum+Tipo_cambio+tipo_cambio_ln+INPC_log+Imp+IMFBCF+`UDIS  ln/100`+Tasa_objetivo_ln+
           Export_billonesdls_ln+Consumo_Priv_ln+Cetes+IGREMSE...27+IGREMSE...32+IGREMSE_Ln+Remun_empresas_construct+IGRESE_Ln+IGPOSE, data = BaseDatosProyecto3)

summary(m12)


#Corregiremos la regresión quitando las variables no significativas

m12 <- lm(Captación_tradicional_Sant_Ln~ TDC_Total + Desempleo + `PIB_LOG/100` + `PIB NORM` + TIIE + 
            `Deuda P. NORM` + Remesas + Pob_Econ_Act + IGAE + VaR_IGAE + Ind_Conf_Consum + Imp + 
            Tasa_objetivo_ln + Cetes + IGREMSE...27 + IGREMSE...32 + IGREMSE_Ln, data = BaseDatosProyecto3)

summary(m12)

#Corregiremos la regresión quitando las variables no significativas

m12 <- lm(Captación_tradicional_Sant_Ln-1~ TDC_Total + Desempleo + `PIB_LOG/100` + `PIB NORM` + TIIE + 
            `Deuda P. NORM` + Remesas + Pob_Econ_Act + IGAE + VaR_IGAE + Ind_Conf_Consum + Imp + 
            Tasa_objetivo_ln + Cetes + IGREMSE...27 + IGREMSE...32 + IGREMSE_Ln, data = BaseDatosProyecto3)

summary(m12)


###################################### Para Banorte ###############################################


#################################### Para los Depósitos Vista


m13 <- lm(Depositos_Vista_Banorte_Ln~TDC_Total+Inflación+Desempleo+`PIB_LOG/100`+`PIB NORM`+TIIE+`Deuda P. NORM`+`Deuda_P_LOG/100`+Remesas+`Ind. Act. Industrial/100`+
            INPC+Pob_Econ_Act+IGAE+VaR_IGAE+Ind_Conf_Consum+Tipo_cambio+tipo_cambio_ln+INPC_log+Imp+IMFBCF+`UDIS  ln/100`+Tasa_objetivo_ln+
            Export_billonesdls_ln+Consumo_Priv_ln+Cetes+IGREMSE...27+IGREMSE...32+IGREMSE_Ln+Remun_empresas_construct+IGRESE_Ln+IGPOSE, data = BaseDatosProyecto3)

summary(m13)

#Corregiremos la regresión quitando las variables no significativas

m13 <- lm(Depositos_Vista_Banorte_Ln~TDC_Total + `PIB_LOG/100` + TIIE + IGAE + Imp + Consumo_Priv_ln, data = BaseDatosProyecto3)
summary(m13)

#Corregiremos la regresión quitando las variables no significativas

m13 <- lm(Depositos_Vista_Banorte_Ln~ TDC_Total + Imp + Consumo_Priv_ln, data = BaseDatosProyecto3)
summary(m13)


#################################### Para los Depósitos Plazo

m14 <- lm(Depositos_Plazo_Banorte_Ln~TDC_Total+Inflación+Desempleo+`PIB_LOG/100`+`PIB NORM`+TIIE+`Deuda P. NORM`+`Deuda_P_LOG/100`+Remesas+`Ind. Act. Industrial/100`+
            INPC+Pob_Econ_Act+IGAE+VaR_IGAE+Ind_Conf_Consum+Tipo_cambio+tipo_cambio_ln+INPC_log+Imp+IMFBCF+`UDIS  ln/100`+Tasa_objetivo_ln+
            Export_billonesdls_ln+Consumo_Priv_ln+Cetes+IGREMSE...27+IGREMSE...32+IGREMSE_Ln+Remun_empresas_construct+IGRESE_Ln+IGPOSE, data = BaseDatosProyecto3)

summary(m14)

#Corregiremos la regresión quitando las variables no significativas


m14 <- lm(Depositos_Plazo_Banorte_Ln~ TDC_Total + Remesas + 
            `Ind. Act. Industrial/100` + VaR_IGAE + Ind_Conf_Consum +
            Consumo_Priv_ln + IGREMSE...32 + IGREMSE_Ln, data = BaseDatosProyecto3)
summary(m14)

#Corregiremos la regresión quitando las variables no significativas


m14 <- lm(Depositos_Plazo_Banorte_Ln~TDC_Total + Remesas, data = BaseDatosProyecto3)
summary(m14)


#################################### Para la Captación Tradicional

m15 <- lm(Captación_tradicional_Banorte_Ln~TDC_Total+Inflación+Desempleo+`PIB_LOG/100`+`PIB NORM`+TIIE+`Deuda P. NORM`+`Deuda_P_LOG/100`+Remesas+`Ind. Act. Industrial/100`+
            INPC+Pob_Econ_Act+IGAE+VaR_IGAE+Ind_Conf_Consum+Tipo_cambio+tipo_cambio_ln+INPC_log+Imp+IMFBCF+`UDIS  ln/100`+Tasa_objetivo_ln+
            Export_billonesdls_ln+Consumo_Priv_ln+Cetes+IGREMSE...27+IGREMSE...32+IGREMSE_Ln+Remun_empresas_construct+IGRESE_Ln+IGPOSE, data = BaseDatosProyecto3)

summary(m15)


#Corregiremos la regresión quitando las variables no significativas

m15 <- lm(Captación_tradicional_Banorte_Ln~ TDC_Total + `PIB_LOG/100` + TIIE + IGAE + Imp + Tasa_objetivo_ln + Consumo_Priv_ln, data = BaseDatosProyecto3)

summary(m15)


#################################################### ANOVAS ##############################################

anova(m1)
anova(m2)
anova(m3)
anova(m4)
anova(m5) #Note que tipo de cambio es una variable irrelevante para el modelo, por el F-value y el p-value
#Ajustamos para el modelo 5
m5 <- lm(Depositos_Plazo_banamex_ln~`Ind. Act. Industrial/100`+tipo_cambio_ln+IGPOSE, data = BaseDatosProyecto3)
summary(m5)
anova(m5)

anova(m6)
anova(m7)
anova(m8)
anova(m9)
anova(m10)
anova(m11)
anova(m12)
anova(m13)
anova(m14)
anova(m15) #Note que el IGAE es una variable irrelevante para el modelo, por el F-value y el p-value
#Ajustamos para el modelo 15
m15 <- lm(Captación_tradicional_Banorte_Ln~ TDC_Total + `PIB_LOG/100` + TIIE + Imp + Tasa_objetivo_ln + Consumo_Priv_ln, data = BaseDatosProyecto3)

summary(m15)

anova(m15)


################################################## Veamos las R^2 ########################################

R1 <- glance(m1)$r.squared
R2 <- glance(m2)$r.squared
R3 <- glance(m3)$r.squared
R4 <- glance(m4)$r.squared
R5 <- glance(m5)$r.squared
R6 <- glance(m6)$r.squared
R7 <- glance(m7)$r.squared
R8 <- glance(m8)$r.squared
R9 <- glance(m9)$r.squared
R10<- glance(m10)$r.squared
R11<- glance(m11)$r.squared
R12 <- glance(m12)$r.squared
R13 <- glance(m13)$r.squared
R14 <- glance(m14)$r.squared
R15 <- glance(m15)$r.squared

tab <- data.frame(rbind(R1,R2,R3,R4,R5,R6,R7,R8,R9,R10,R11,R12,R13,R14,R15))
row.names(tab) <- c("Depósitos Vista TBM", "Depósitos Plazo TMB", "Captación Tradicional TBM",
                    "Depósitos Vista Banamex", "Depósitos Plazo Banamex", "Captación Tradicional Banamex",
                    "Depósitos Vista BBVA", "Depósitos Plazo BBVA", "Captación Tradicional BBVA",
                    "Depósitos Vista Santander", "Depósitos Plazo Santander", "Captación Tradicional Santander",
                    "Depósitos Vista Banorte", "Depósitos Plazo Banorte", "Captación Tradicional Banorte")
names(tab) <- c( "R^2")

View(tab)

############################################# Prueba Kruskall Wallis ####################################################

kruskal.test(residuals(m1)~BaseDatosProyecto3$Depositos_Vista_TBM_ln) #No hay diferencias significativas entre las medianas de los grupos

kruskal.test(residuals(m2)~BaseDatosProyecto3$Depositos_Plazo_TBM_ln) #No hay diferencias significativas entre las medianas de los grupos

kruskal.test(residuals(m3)~BaseDatosProyecto3$CaptacioN_Tradicional_TBM_ln) #No hay diferencias significativas entre las medianas de los grupos

kruskal.test(residuals(m4)~BaseDatosProyecto3$Depositos_Vista_banamex_ln) #No hay diferencias significativas entre las medianas de los grupos

kruskal.test(residuals(m5)~BaseDatosProyecto3$Depositos_Plazo_banamex_ln) #No hay diferencias significativas entre las medianas de los grupos

kruskal.test(residuals(m6)~BaseDatosProyecto3$Captación_tradicional_banamex_ln)#No hay diferencias significativas entre las medianas de los grupos

kruskal.test(residuals(m7)~BaseDatosProyecto3$Depositos_Vista_BBVA) #No hay diferencias significativas entre las medianas de los grupos

kruskal.test(residuals(m8)~BaseDatosProyecto3$Depositos_Plazo_BBVA_Ln) #No hay diferencias significativas entre las medianas de los grupos

kruskal.test(residuals(m9)~BaseDatosProyecto3$Captación_tradicional_BBVA_Ln) #No hay diferencias significativas entre las medianas de los grupos

kruskal.test(residuals(m10)~BaseDatosProyecto3$Depositos_Vista_Sant_Ln)#No hay diferencias significativas entre las medianas de los grupos

kruskal.test(residuals(m11)~BaseDatosProyecto3$Depositos_Plazo_Sant_Ln) #No hay diferencias significativas entre las medianas de los grupos

kruskal.test(residuals(m12)~BaseDatosProyecto3$Captación_tradicional_Sant_Ln) #No hay diferencias significativas entre las medianas de los grupos

kruskal.test(residuals(m13)~BaseDatosProyecto3$Depositos_Vista_Banorte_Ln) #No hay diferencias significativas entre las medianas de los grupos

kruskal.test(residuals(m14)~BaseDatosProyecto3$Depositos_Plazo_Banorte_Ln)#No hay diferencias significativas entre las medianas de los grupos

kruskal.test(residuals(m15)~BaseDatosProyecto3$Captación_tradicional_Banorte_Ln)#No hay diferencias significativas entre las medianas de los grupos


############################################## Prueba Kendall ###############################################

cor.test(residuals(m1),BaseDatosProyecto3$Depositos_Vista_TBM_ln, method = "kendall") #pasa la prueba
cor.test(residuals(m2),BaseDatosProyecto3$Depositos_Plazo_TBM_ln,method = "kendall") #no pasa la prueba
cor.test(residuals(m3),BaseDatosProyecto3$CaptacioN_Tradicional_TBM_ln, method = "kendall") #pasa la prueba
cor.test(residuals(m4),BaseDatosProyecto3$Depositos_Vista_banamex_ln,method = "kendall") #pasa la prueba
cor.test(residuals(m5),BaseDatosProyecto3$Depositos_Plazo_banamex_ln,method = "kendall") #no pasa la prueba
cor.test(residuals(m6),BaseDatosProyecto3$Captación_tradicional_banamex_ln, method = "kendall") #no pasa la prueba
cor.test(residuals(m7),BaseDatosProyecto3$Depositos_Vista_BBVA_Ln,method = "kendall") #no pasa la prueba
cor.test(residuals(m8),BaseDatosProyecto3$Depositos_Plazo_BBVA_Ln,method = "kendall") #no pasa la prueba
cor.test(residuals(m9),BaseDatosProyecto3$Captación_tradicional_BBVA_Ln, method = "kendall") #pasa la prueba
cor.test(residuals(m10),BaseDatosProyecto3$Depositos_Vista_Sant_Ln,method = "kendall") #pasa la prueba
cor.test(residuals(m11),BaseDatosProyecto3$Depositos_Plazo_Sant_Ln,method = "kendall") #no pasa la prueba
cor.test(residuals(m12),BaseDatosProyecto3$Captación_tradicional_Sant_Ln, method = "kendall") #pasa la prueba
cor.test(residuals(m13),BaseDatosProyecto3$Depositos_Vista_Banorte_Ln,method = "kendall") #no pasa la prueba
cor.test(residuals(m14),BaseDatosProyecto3$Depositos_Plazo_Banorte_Ln,method = "kendall") #no pasa la prueba
cor.test(residuals(m15),BaseDatosProyecto3$Captación_tradicional_Banorte_Ln, method = "kendall") #pasa la prueba


############################################# Prueba Kolmogorov ############################################

ks.test(residuals(m1), "pnorm", mean = mean(residuals(m1)), sd = sd(residuals(m1))) #pasa la prueba
ks.test(residuals(m2), "pnorm", mean = mean(residuals(m2)), sd = sd(residuals(m2))) #pasa la prueba
ks.test(residuals(m3), "pnorm", mean = mean(residuals(m3)), sd = sd(residuals(m3))) #Pasa la prueba
ks.test(residuals(m4), "pnorm", mean = mean(residuals(m4)), sd = sd(residuals(m4))) #pasa la prueba
ks.test(residuals(m5), "pnorm", mean = mean(residuals(m5)), sd = sd(residuals(m5))) #no pasa la prueba
ks.test(residuals(m6), "pnorm", mean = mean(residuals(m6)), sd = sd(residuals(m6))) #pasa la prueba
ks.test(residuals(m7), "pnorm", mean = mean(residuals(m7)), sd = sd(residuals(m7))) #no pasa la prueba
ks.test(residuals(m8), "pnorm", mean = mean(residuals(m8)), sd = sd(residuals(m8))) #pasa la prueba
ks.test(residuals(m9), "pnorm", mean = mean(residuals(m9)), sd = sd(residuals(m9))) #pasa la prueba
ks.test(residuals(m10), "pnorm", mean = mean(residuals(m10)), sd = sd(residuals(m10))) #pasa la prueba
ks.test(residuals(m11), "pnorm", mean = mean(residuals(m11)), sd = sd(residuals(m11))) #pasa la prueba
ks.test(residuals(m12), "pnorm", mean = mean(residuals(m12)), sd = sd(residuals(m12))) #pasa la prueba
ks.test(residuals(m13), "pnorm", mean = mean(residuals(m13)), sd = sd(residuals(m13))) #pasa la prueba
ks.test(residuals(m14), "pnorm", mean = mean(residuals(m14)), sd = sd(residuals(m14))) #pasa la prueba
ks.test(residuals(m15), "pnorm", mean = mean(residuals(m15)), sd = sd(residuals(m15))) #pasa la prueba
