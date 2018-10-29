
#install.packages("RODBC")
#install.packages("PKI")
library(PKI)
library(RJDBC)

#Necessary for JDBC only
driverClass <- "com.amazon.redshift.jdbc41.Driver"
classPath <- "/etc/jdbc/RedshiftJDBC41-1.1.7.1007.jar"


#Go get password key and decrypt it
require(PKI)
key <- PKI.load.key(format="PEM", file="key.rda")
load("encryptedPW.rda")
decryptedPW <- PKI.decrypt(e, key)

#Necessary for all connections
user <- "bchattin"
password <- rawToChar(decryptedPW) #This converts the raw binary to a character vector. Just one more level of security.
database <- "deci"
port <- "5439"
url <-  "jdbc:redshift://deci.cq0tiumbbtoo.us-east-1.redshift.amazonaws.com:5439/deci"


getdatajdbc <- function(conn, query) {
  
  data <-  dbGetQuery(conn, query)
  return(data)
  dbDisconnect(conn)
}


popDataQuery <- paste0("SELECT  puma00, puma10, st, serialno, agep, rac1p, hisp, pwgtp
                      from public_data.acs_2013_5yr_pus")


hhDataQuery <- paste0("SELECT serialno, hincp, adjinc from public_data.acs_2013_5yr_hus")

#"SELECT serialno, st, puma00, puma10, agep, hisp, RAC3P12, lanp05, lanp12, rac3p12, eng, pwgtp 
#                       from public_data.acs_2013_5yr_pus")
#public_data.acs_population where year=2010
#public_data.acs_2014_1yr_pus
#Only calls the driver and conn once per session. Makes it much faster.
driver <- JDBC(driverClass = driverClass,  classPath = classPath)

if(exists("conn")) {rm(conn)}
conn <- dbConnect(driver, user=user, password=password, database=database, port=port, 
                  url= url)



pop <- getdatajdbc(conn, query = popDataQuery)
hh <- getdatajdbc(conn, query = hhDataQuery)
dbDisconnect(conn, quietly = TRUE)





