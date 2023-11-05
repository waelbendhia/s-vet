module SVet.API.Statistics (StatisticsAPI, statisticsHandler) where

import Data.Time
import Data.Time.Calendar.OrdinalDate
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Relude
import SVet.API.Internal
import SVet.Authentication
import SVet.Domain
import SVet.Repository
import SVet.Tracing
import Servant

type StatisticsServer api r =
  Members '[Statistics, Input UTCTime, Embed IO, Error AuthenticationError] r =>
  ServerT api (Sem r)

type GetConsultationTotalsSub =
  "statistics"
    :> "consultation-totals"
    :> QueryParam "after" UTCTime
    :> QueryParam "before" UTCTime
    :> Get '[JSON] ConsultationStatistics

type GetConsultationTotalsAPI = SVetAuth :> GetConsultationTotalsSub

getConsultationTotalsHandler :: StatisticsServer (Traced :> GetConsultationTotalsAPI) r
getConsultationTotalsHandler = serverWithVet @GetConsultationTotalsSub \v after' before' -> do
  now <- input
  getConsultationsStatistics
    v
    (after' ?: now{utctDayTime = 0})
    (before' ?: now{utctDayTime = 86401})

type GetConsultationsByHourSub =
  "statistics"
    :> "consultations-by-hour"
    :> QueryParam "after" Day
    :> QueryParam "before" Day
    :> Get '[JSON] ConsultationsByHour

type GetConsultationsByHourAPI = SVetAuth :> GetConsultationsByHourSub

getClosest :: DayOfWeek -> Day -> Day
getClosest dow date = fromOrdinalDate y (doy + offset)
 where
  offset = fromEnum dow - fromEnum (dayOfWeek date)
  (y, doy) = toOrdinalDate date

getConsultationsByHourHandler :: StatisticsServer (Traced :> GetConsultationsByHourAPI) r
getConsultationsByHourHandler = serverWithVet @GetConsultationsByHourSub \v after' before' -> do
  today <- inputs utctDay
  getConsultationsByHourStatistics
    v
    (after' ?: getClosest Monday today)
    (before' ?: getClosest Sunday today)

type StatisticsAPI = GetConsultationTotalsAPI :<|> GetConsultationsByHourAPI

statisticsHandler :: StatisticsServer (TracedAPI StatisticsAPI) r
statisticsHandler = getConsultationTotalsHandler :<|> getConsultationsByHourHandler
