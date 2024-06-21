library(httr)
library(jsonlite)
library(openssl)
library(digest)


NETSUITE_DEPLOYMENT_URL = 'https://.restlets.api.netsuite.com/app/site/hosting/restlet.nl?script=3612&deploy=1';
print(NETSUITE_DEPLOYMENT_URL);

NETSUITE_URL = 'https://.restlets.api.netsuite.com';
print(NETSUITE_URL)

NETSUITE_REST_URL='https://.restlets.api.netsuite.com/app/site/hosting/restlet.nl';
print(NETSUITE_REST_URL);

NETSUITE_SCRIPT_ID='3612';
print(NETSUITE_SCRIPT_ID);

NETSUITE_DEPLOY_ID='1';
print(NETSUITE_DEPLOY_ID);

NETSUITE_ACCOUNT='';
print(NETSUITE_ACCOUNT);


NETSUITE_CONSUMER_KEY='';
print(NETSUITE_CONSUMER_KEY);


NETSUITE_CONSUMER_SECRET='';
print(NETSUITE_CONSUMER_SECRET);


NETSUITE_TOKEN_ID = '';
print(NETSUITE_TOKEN_ID)


NETSUITE_TOKEN_SECRET = '';
query <- "Select mpn from item"

# Create a list with the query
query_list <- list(query = query)

# Convert the list to JSON
encoded <- toJSON(query_list, auto_unbox = TRUE)

oauth_nonce= 'ABCDEFGH';
oauth_timestamp= as.character(as.integer(Sys.time())); 
#oauth_timestamp= '1713360196' ;
oauth_signature_method='HMAC-SHA256';
oauth_version="1.0";

print(oauth_timestamp)

post_str = base_string = paste("POST", "&", sep = "");
print(post_str)


NETSUITE_REST_URL_STR = paste(URLencode(NETSUITE_REST_URL, reserved = TRUE), "&", sep = "");
print(NETSUITE_REST_URL_STR);


NETSUITE_DEPLOY_ID_STR     = paste("deploy=", NETSUITE_DEPLOY_ID, sep = "");
NETSUITE_CONSUMER_KEY_STR  = paste("&oauth_consumer_key=", NETSUITE_CONSUMER_KEY, sep = "");
oauth_nonce_str            = paste("&oauth_nonce=", oauth_nonce, sep = "");
oauth_signature_method_str = paste("&oauth_signature_method=", oauth_signature_method, sep = "");
oauth_timestamp_str        = paste("&oauth_timestamp=", oauth_timestamp, sep = "");
NETSUITE_TOKEN_ID_STR      = paste("&oauth_token=", NETSUITE_TOKEN_ID, sep = "");
oauth_version_str          = paste("&oauth_version=", oauth_version, sep = "");
NETSUITE_SCRIPT_ID_STR     = paste("&script=", NETSUITE_SCRIPT_ID, sep = "");


concat_str1 = paste(post_str, NETSUITE_REST_URL_STR, sep = "")
concat_str2 = paste(NETSUITE_DEPLOY_ID_STR, NETSUITE_CONSUMER_KEY_STR, sep = "")
concat_str2 = paste(concat_str2, oauth_nonce_str, sep = "")
concat_str2 = paste(concat_str2, oauth_signature_method_str, sep = "")
concat_str2 = paste(concat_str2, oauth_timestamp_str, sep = "")
concat_str2 = paste(concat_str2, NETSUITE_TOKEN_ID_STR, sep = "")
concat_str2 = paste(concat_str2, oauth_version_str, sep = "")
concat_str2 = paste(concat_str2, NETSUITE_SCRIPT_ID_STR, sep = "")


base_string = paste(concat_str1, URLencode(concat_str2, reserved = TRUE), sep = "")
print(base_string)


hashkey = paste(URLencode(NETSUITE_CONSUMER_SECRET, reserved = TRUE), '&', sep = "")
hashkey = paste(hashkey, URLencode(NETSUITE_TOKEN_SECRET, reserved = TRUE), sep = "")

print(hashkey)

oauth_signature = hmac(hashkey, object = base_string, algo = 'sha256', serialize = FALSE, raw = TRUE)
print(oauth_signature)
oauth_signature = base64_encode(oauth_signature)
print(oauth_signature)

auth_header='OAuth ';
auth_header = paste(auth_header, 'realm="', sep = "")
auth_header = paste(auth_header, URLencode(NETSUITE_ACCOUNT, reserved = TRUE), sep = "")
auth_header = paste(auth_header, '",', sep = "")

auth_header = paste(auth_header, 'oauth_consumer_key="', sep = "")
auth_header = paste(auth_header, URLencode(NETSUITE_CONSUMER_KEY, reserved = TRUE), sep = "")
auth_header = paste(auth_header, '",', sep = "")

auth_header = paste(auth_header, 'oauth_token="', sep = "")
auth_header = paste(auth_header, URLencode(NETSUITE_TOKEN_ID, reserved = TRUE), sep = "")
auth_header = paste(auth_header, '",', sep = "")

auth_header = paste(auth_header, 'oauth_signature_method="', sep = "")
auth_header = paste(auth_header, URLencode(oauth_signature_method, reserved = TRUE), sep = "")
auth_header = paste(auth_header, '",', sep = "")

auth_header = paste(auth_header, 'oauth_timestamp="', sep = "")
auth_header = paste(auth_header, URLencode(oauth_timestamp, reserved = TRUE), sep = "")
auth_header = paste(auth_header, '",', sep = "")


auth_header = paste(auth_header, 'oauth_nonce="', sep = "")
auth_header = paste(auth_header, URLencode(oauth_nonce, reserved = TRUE), sep = "")
auth_header = paste(auth_header, '",', sep = "")


auth_header = paste(auth_header, 'oauth_version="', sep = "")
auth_header = paste(auth_header, URLencode(oauth_version, reserved = TRUE), sep = "")
auth_header = paste(auth_header, '",', sep = "")


auth_header = paste(auth_header, 'oauth_signature="', sep = "")
auth_header = paste(auth_header, URLencode(oauth_signature, reserved = TRUE), sep = "")
auth_header = paste(auth_header, '"', sep = "")

print(auth_header)

#POST(NETSUITE_DEPLOYMENT_URL, body = login, encode = "form", verbose())

#Convert results into a dataframe 

Output <- content(result, "text")
Output_json <- fromJSON(Output)
Output_df <- as.data.frame(Output_json)

print(Output_df)

write.csv(Output_df, "C:/Users/LivioBeqiri/Desktop/R/NetsuiteSearchResults.csv")
