library(jose)
library(httr)
ts = Sys.time();
claim = jwt_claim(iss="",scope="restlets", iat =Sys.time(), exp =Sys.time() + 3600, aud ="https://.suitetalk.api.netsuite.com/services/rest/auth/oauth2/v1/token")
header <- list(kid = '')
jwt_enc = jwt_encode_sig(claim, key = "private.pem", size = 256, header = header)
payload <- list(grant_type = "client_credentials",client_assertion_type = "urn:ietf:params:oauth:client-assertion-type:jwt-bearer",client_assertion = jwt_enc)
response <- httr::POST("https://.suitetalk.api.netsuite.com/services/rest/auth/oauth2/v1/token", body = payload, encode = "form")
data <- httr::content(response)
bearer = paste0('Bearer ', data$access_token);
query <-  jsonlite::toJSON (x = jsonlite::fromJSON ('{"query":"Select LastName from CONTACT where FirstName=\'Livio\'"}',simplifyVector = FALSE), auto_unbox = TRUE) 
result <- httr::POST("https://.restlets.api.netsuite.com/app/site/hosting/restlet.nl?script=3612&deploy=1",body = query,add_headers(.headers = c("Content-Type"="application/json","Authorization"= bearer)))
print(content(result))
