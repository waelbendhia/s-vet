module SVet.Constants where

import SVet.Domain (
  GoogleClientCredentials (GoogleClientCredentials),
 )
import SVet.TH (lookupCompileEnvExp)

googleCredentials :: GoogleClientCredentials
googleCredentials =
  GoogleClientCredentials
    $(lookupCompileEnvExp "GOOGLE_CLIENT_ID" "")
    $(lookupCompileEnvExp "GOOGLE_CLIENT_SECRET" "")
