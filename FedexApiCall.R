library(xlsx)
library(tidyverse)
library(readxl)
library(dplyr)
library(httr)
library(jsonlite)
library(jqr)

headers = c(
  'Content-Type' = 'application/x-www-form-urlencoded',
  'Cookie' = '_abck=0D52AFB69133ACFD4E33208559730385~-1~YAAQURAgFzIn8d+LAQAAku/n8wpoXhHcFN6RLDjwIQKM7NEdbMm3Ouk95Qm3xhCVBLTwDGiyk87SzI7I/d4DYZFB/InrDEFGCBnJ7pJXYv9K0KFKe9TlniXLR23re/soe9Vawb86Su1tLSWDQNcYCcoX79daxShgcScOUKpjbewz18DPI+qwPecqv3Oqw+MmtGYK0/bk7TthdlIwIcOuXTGqiDg4R5PdzCw/9zZ8YUbKgojFt4RcBnd2HhbuB/D0Xt8gM7u/6h8E2AWbwSQElbmP8PBOfrTwazlZ5urd3DlAatfFuEeAArB/9RU1wo5lIHgfnmSmSAiP8o/ECLkbgJ9VCSiP7dW8D15wvDAb5kUrM0zJAYCpuntPp0EDImMjuHkaT7+T~-1~-1~-1; bm_sz=0B02446C4A0CA4CFD6EAE2DAED9FA431~YAAQURAgFzMn8d+LAQAAku/n8xXNU6qW4Q+YKqTad4p3lF/I5qCm/Fc2DYekLUPJN1sumH68lA5/SW00SQ08uoA4zMxUKINlo0sDwGo8Ny1cYt6ZjoGqVD6FL0R6jRkWT/hmgbE4LnjrLrcPmhyZAzeHdILmvHO+HWX1RXxqwCBIx6gI4eR3sgezJPe9MI/r9o/4te6sI8V/WpsSOc9oSaT9ltAWzC6XvZGagtu3dQY+V7mY98hrNqk6ZhXs/duHCKYCXMyJ8aq/rHDh+/MbeKQZCVdkYwzNPI0HhmxBbgRM+A==~3291185~3491377'
)

body = list(
  'grant_type' = 'client_credentials',
  'client_id' = '',
  'client_secret' = ''
)

res <- VERB("POST", url = "https://apis.fedex.com/oauth/token", body = body, add_headers(headers), encode = 'form')

results <- content(res, as="parsed")
as_df <- as.data.frame(results)
rownames(as_df) <- NULL
access_token <- print(as_df["access_token"[1]])
print(toString(access_token))
#parse2json <- (toJSON(results))

