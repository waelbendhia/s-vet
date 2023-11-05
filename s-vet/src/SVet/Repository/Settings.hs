{-# LANGUAGE BlockArguments #-}

module SVet.Repository.Settings (
  SettingsRepository (..),
  initSettings,
  getSettings,
  getPassword,
  updatePassword,
  updateSettings,
  runSettingsRepository,
  runTokenInput,
  runTokenOutput,
  runTokenState,
) where

import qualified Data.List as L
import Data.Time
import Database.Selda hiding (toText)
import Katip
import Optics
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.Output
import Polysemy.State
import Relude hiding (Reader, State)
import Relude.Extra hiding ((^.))
import SVet.Domain
import SVet.Repository.Crypt
import SVet.Repository.Tables
import SVet.Selda

data SettingsRepository m a where
  InitSettings :: Email -> SettingsRepository m ()
  GetSettings :: Email -> SettingsRepository m Settings
  GetPassword :: Email -> SettingsRepository m (Maybe ByteString)
  UpdatePassword :: Email -> Password -> SettingsRepository m ()
  UpdateSettings :: Email -> Settings -> SettingsRepository m ()

makeSem ''SettingsRepository

runSettingsRepository ::
  (KatipContext (Sem r), MonadSelda (Sem r), Members '[Crypt, Input UTCTime] r) =>
  Sem (SettingsRepository : r) a ->
  Sem r a
runSettingsRepository = interpret \case
  InitSettings email' -> do
    whenM (null <$> query selectUserPassword) do
      hashedPassword <- hash $ toText defaultPassword
      now <- input
      insert_
        usersTable
        [ UserSQL
            { email = defaultEmail
            , password = hashedPassword
            , createdAt = now
            }
        ]
    existingTypes <- query (#priceType `from` select pricesTable)
    let missingTypes = universe L.\\ existingTypes
    insert_ pricesTable $
      missingTypes <&> \pt ->
        PricesSQL
          { vet = email'
          , priceType = pt
          , price = 45000
          }
    whenM
      (null <$> query (#dayOfTheWeek `from` select workHoursTable `suchThat` #vet `is` email'))
      (insert_ workHoursTable (defaultWorkHourRows email'))
   where
    selectUserPassword = #email `from` select usersTable `suchThat` #email `is` email'
  GetSettings email' -> do
    priceRows <- query $ select pricesTable `suchThat` #vet `is` email'
    let priceMap =
          fromList $
            (\p -> (p ^. #priceType, p ^. #price))
              <$> priceRows
    workHours <- query $ select workHoursTable `suchThat` #vet `is` email'
    pure $ Settings priceMap (rowsToSchedule workHours)
  GetPassword email' -> do
    $logTM DebugS "looking for password"
    pwds <- query $ #password `from` select usersTable `suchThat` #email `is` email'
    $logTM DebugS $ "found " <> show (length pwds) <> " passwords"
    pure (listToMaybe pwds)
  UpdatePassword email' pwd -> do
    hashedPassword <- hash $ toText pwd
    update_
      usersTable
      (#email `is` email')
      (`with` [#password := literal hashedPassword])
  UpdateSettings email' s -> do
    toPairs (s ^. #prices) `forM_` \(t, p) ->
      update_
        pricesTable
        ((.&&) <$> #priceType `is` t <*> #vet `is` email')
        (`with` [#price := literal p])
    let wds = keys (s ^. #schedule)
    deleteFrom_ workHoursTable \wh ->
      (#vet `is` email') wh .&& foldr (.||) false (((wh ! #dayOfTheWeek) .==) . literal <$> wds)
    insert_ workHoursTable $ scheduleToRows email' (s ^. #schedule)

scheduleToRows :: Email -> Schedule -> [WorkHoursSQL]
scheduleToRows email' =
  fmap
    ( \(d, wh) ->
        WorkHoursSQL
          { vet = email'
          , start = wh ^. #start
          , end = wh ^. #end
          , dayOfTheWeek = d
          }
    )
    . toPairs

rowsToSchedule :: [WorkHoursSQL] -> Schedule
rowsToSchedule =
  fromList
    . fmap
      ( \wh ->
          ( wh ^. #dayOfTheWeek
          , WorkHours{start = wh ^. #start, end = wh ^. #end}
          )
      )

defaultWorkHours :: Schedule
defaultWorkHours = fromList $ saturday : weekdays
 where
  weekdays =
    (,WorkHours (TimeOfDay 9 0 0) (TimeOfDay 18 0 0))
      <$> [WeekDay Monday .. WeekDay Saturday]
  saturday =
    (WeekDay Saturday, WorkHours (TimeOfDay 9 0 0) (TimeOfDay 14 0 0))

defaultWorkHourRows :: Email -> [WorkHoursSQL]
defaultWorkHourRows email' = scheduleToRows email' defaultWorkHours

data TokenError = NoRefreshToken deriving (Show, Eq)

getTokenDB :: (MonadSelda m) => m (Maybe (Token, UTCTime))
getTokenDB = do
  tkn <- query (select tokenTable)
  pure do
    t <- listToMaybe tkn
    pure
      ( Token
          (t ^. #accessToken)
          0
          ""
          ""
          (Just $ t ^. #refreshToken)
      , t ^. #expiresIn
      )

runTokenInput :: (MonadSelda (Sem r)) => Sem (Input (Maybe (Token, UTCTime)) : r) a -> Sem r a
runTokenInput = runInputSem getTokenDB

runTokenOutput ::
  (Members '[Input (Maybe (Token, UTCTime)), Error TokenError] r, MonadSelda (Sem r)) =>
  Sem (Output (Maybe (Token, UTCTime)) : r) a ->
  Sem r a
runTokenOutput = runOutputSem \tkn' ->
  tkn' `forM_` \(t, expiration) -> do
    tkn <- input @(Maybe (Token, UTCTime))
    refreshTkn <-
      maybe
        (throw NoRefreshToken)
        pure
        $ t ^. #refreshToken <|> join (tkn ^? _Just % _1 % #refreshToken)
    tkn
      & maybe
        (insert_ tokenTable [TokenSQL (toId 1) (t ^. #accessToken) refreshTkn expiration])
        \_ ->
          update_
            tokenTable
            (#key `is` toId 1)
            ( `with`
                [ #accessToken := literal (t ^. #accessToken)
                , #refreshToken := literal refreshTkn
                , #expiresIn := literal expiration
                ]
            )

runTokenState ::
  (Members '[Error TokenError, Selda, Embed IO] r) =>
  Sem (State (Maybe (Token, UTCTime)) : r) a ->
  Sem r a
runTokenState =
  runTokenInput . runTokenOutput . reinterpret2 \case
    Get -> input
    Put v -> output v
