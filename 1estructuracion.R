
#defino el espacio de trabajo y cargo los datos---------------------------------------------------------------------------

setwd("~/Repositorios/Dex")
datosOriginales<-readRDS('2015.rds')
head(datosOriginales)
summary(datosOriginales)

#me quedo con lo que necesito----------------------------------------------------------------------------------------------

library(dplyr)
datos<-datosOriginales %>%
  select(tipoOperacion=FCTIPOOPERACION,
         idEnvio=NO_CLIENTE_REMITENTE,
         monto=FNMONTOENVIO,
         comision=FNCOMISIONMO,
         fechaEnvio=FECHA_ENVIO,
         fechaPago=FECHA_PAGO,
         estadoEnvio=FCESTADOORIGEN,
         estadoPago=FCESTADODESTINO,
         sucursalEnvio=FCNOMBRESUCURSALENVIO,
         sucursalPago=FCNOMBRESUCURSALDESTINO
         ) %>%
  filter(tipoOperacion=='ENVIO') %>%

datos$fechaEnvio<-as.character(datos$fechaEnvio)
datos$monto<-as.numeric(as.character(datos$monto))

#creo las variables de día mes y año--------------------------------------------------------------------------------------

library(stringr)

datos<-datos %>%
  mutate(anoEnvio=as.factor(str_sub(fechaEnvio,-13,-10)),
         mesEnvio=as.factor(str_sub(fechaEnvio,-16,-15)),
         diaEnvio=as.factor(str_sub(fechaEnvio,-19,-18))
         ) %>%
  filter(anoEnvio=='2015')





#creo la tabla de resumen de información-----------------------------------------------------------------------------------------

library(dplyr)

#número de envíos
enviosDia<-datos %>%
  mutate(envioMesDia=paste0(mesEnvio,'_',diaEnvio)) %>%
  group_by(idEnvio,envioMesDia) %>%
  summarize(envios=n()) %>%
  group_by(idEnvio) %>%
  summarise(enviosMaximosDia=max(envios)) %>%
  as.data.frame()

enviosMes<-datos %>%
  group_by(idEnvio,mesEnvio) %>%
  summarize(envios=n()) %>%
  group_by(idEnvio) %>%
  summarise(enviosMaximosMes=max(envios)) %>%
  as.data.frame()

enviosAno<-datos %>%
  group_by(idEnvio) %>%
  summarize(envios=n()) %>%
  as.data.frame()

#montos de las transacciones
montoDia<-datos %>%
  mutate(envioMesDia=paste0(mesEnvio,'_',diaEnvio)) %>%
  group_by(idEnvio,envioMesDia) %>%
  summarize(montoT=sum(monto)) %>%
  group_by(idEnvio) %>%
  summarise(montoMaximosDia=max(montoT)) %>%
  as.data.frame()

montoMes<-datos %>%
  group_by(idEnvio,mesEnvio) %>%
  summarize(montoT=sum(monto)) %>%
  group_by(idEnvio) %>%
  summarise(montoMaximosMes=max(montoT)) %>%
  as.data.frame()

montoAno<-datos %>%
  group_by(idEnvio) %>%
  summarize(montoMaximoAno=sum(monto)) %>%
  as.data.frame()

datosCliente<-enviosDia
datosCliente<-cbind(datosCliente,enviosMes[,2],enviosAno[,2],montoDia[,2],montoMes[,2],montoAno[,2])
names(datosCliente)<-c('id','enviosDia','enviosMes','enviosAno','montoDia','montoMes','montoAno')
head(datosCliente)

