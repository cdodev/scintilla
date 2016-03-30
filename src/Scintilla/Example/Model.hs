 {-# LANGUAGE DeriveDataTypeable         #-}
 {-# LANGUAGE DeriveFunctor              #-}
 {-# LANGUAGE DeriveGeneric              #-}
 {-# LANGUAGE FlexibleContexts           #-}
 {-# LANGUAGE FlexibleInstances          #-}
 {-# LANGUAGE GeneralizedNewtypeDeriving #-}
 {-# LANGUAGE MultiParamTypeClasses      #-}
 {-# LANGUAGE RankNTypes                 #-}
 {-# LANGUAGE ScopedTypeVariables        #-}
 {-# LANGUAGE TemplateHaskell            #-}
 {-# LANGUAGE TypeFamilies               #-}

module Scintilla.Example.Model (

  ) where
-- import           Control.Applicative
import           Control.Exception          (Exception (..))
import           Control.Lens
import           Data.Aeson                 (FromJSON (..), ToJSON (..))
import qualified Data.Aeson                 as Aeson
import Data.ByteString (ByteString)
import           Data.Data                  (Data, Typeable)
import           Data.Text                  (Text)
import Data.Thyme.Clock (UTCTime, DiffTime)
--import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import           Data.UUID                  (UUID)
import qualified Data.UUID                  as UUID
-- import qualified Data.UUID.V4               as UUID
import qualified Database.Redis             as Redis
import           GHC.Generics

data Pomodoro = Pomodoro {
    _started :: !UTCTime
  , _ended :: !UTCTime
  , _scheduledLength :: !DiffTime
   } deriving (Generic)

makeLenses ''Pomodoro

data TodoItem = TodoItem {
    _title :: Text
  , _created :: UTCTime
  , _due :: Maybe (UTCTime)
  , _pomodoros :: [Pomodoro]
   } deriving (Generic)

makeLenses ''TodoItem


