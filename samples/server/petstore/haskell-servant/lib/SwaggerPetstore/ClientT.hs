{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module SwaggerPetstore.ClientT where

import SwaggerPetstore.Client

import Control.Monad.Except (ExceptT(..), runExceptT, liftEither)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Morph (MFunctor(..))
import Control.Monad.Reader (ReaderT(..))
import Control.Monad.Reader (MonadReader(..))
import Control.Monad.Trans.Class (MonadTrans(..))
import Prelude
import Servant.Client (BaseUrl(..), ClientEnv, ServantError, Scheme(..), runClientM)


-- | Monad transformer for client calls.
newtype SwaggerPetstoreClientT m a = SwaggerPetstoreClientT {
  unClientT :: (ReaderT ClientEnv) (ExceptT ServantError m) a
} deriving (Functor, Applicative, Monad, MonadIO)

instance MonadTrans SwaggerPetstoreClientT where
  lift ma = SwaggerPetstoreClientT (ReaderT (\_ -> ExceptT (fmap Right ma)))

instance MFunctor SwaggerPetstoreClientT where
  hoist f m = SwaggerPetstoreClientT (ReaderT (\env -> ExceptT (f $ runClientT env m)))

instance MonadIO m => SwaggerPetstoreClientMonad (SwaggerPetstoreClientT m) where
  liftSwaggerPetstoreClient clientM = SwaggerPetstoreClientT $ do
    env <- ask
    res <- liftIO $ runClientM clientM env
    liftEither res


baseUrl :: BaseUrl
baseUrl = BaseUrl Http "petstore.swagger.io" 80 "/v2"

runClientT :: ClientEnv -> SwaggerPetstoreClientT m a -> m (Either ServantError a)
runClientT env =  runExceptT . flip runReaderT env . unClientT

-- | Trivial alias
type SwaggerPetstoreClient = SwaggerPetstoreClientT IO

runClient :: ClientEnv -> SwaggerPetstoreClient a -> IO (Either ServantError a)
runClient = runClientT

