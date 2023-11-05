module SVet.Repository.Internal (
  getConsultationsByPetQuery,
) where

import Database.Selda
import Relude
import SVet.Domain
import SVet.Repository.Tables

getConsultationsByPetQuery :: EntKey Pet -> Query s (Row s ConsultationSQL)
getConsultationsByPetQuery pKey = do
  c <- select consultationsTable
  restrict $ not_ $ c ! #cancelled
  restrict $ c & #petKey `is` toSeldaID pKey
  order (c ! #time) Desc
  pure c
