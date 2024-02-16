library(httr)

#Register an application
nouns_app <- oauth_app("noun_project",
                       key = "",
                       secret = ""
)

#Endpoint
url <- "https://586038.restlets.api.netsuite.com/app/site/hosting/restlet.nl?script=3612&deploy=1"

#Generate Signature
signature <- oauth_signature(url, method = "GET", app = nouns_app)

consumer_key <- "***"
token <- "**"

headers = c(
  'Content-Type' = 'Application/JSON',
  "Authorization" = paste0(
    "OAuth",
    " realm=\"586038\"",
    ",oauth_consumer_key=\"", consumer_key, "\"",
    ",oauth_token=\"", token, "\"",
    ",oauth_signature_method=\"HMAC-SHA256\"",
    ",oauth_timestamp=\"", signature$oauth_timestamp, "\"",
    ",oauth_nonce=\"", signature$oauth_nonce, "\"",
    ",oauth_version=\"1.0\"",
    ",oauth_signature=\"", signature$oauth_signature, "\""

  )

)

 print(headers)

#  headers = c(
#   )
# print(headers)

body = "{\r\n    \"query\":\"Select LastName from CONTACT where FirstName='Livio'\"\r\n}"

res <- VERB("POST", url = "https://586038.restlets.api.netsuite.com/app/site/hosting/restlet.nl?script=3612&deploy=1", body = body, add_headers(headers))

cat(content(res, 'text'))
