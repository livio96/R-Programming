library(httr)

#Register an application
nouns_app <- oauth_app("noun_project",
                       key = "200b3bacd548c08046abbdcb50b5374445f8081176e0fe1ca28a2d80918686d1",
                       secret = "91a127b1123f622bd951c9a52f6c3609449fbb05101fd2f5df1a13ca105c2932"
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
