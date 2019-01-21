{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-imports -freduction-depth=0 #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module SwaggerPetstore.Server where

import SwaggerPetstore.API
import SwaggerPetstore.Common
import SwaggerPetstore.Types

import Control.Monad.IO.Class (MonadIO(..))
import Data.Coerce (coerce)
import Data.Function ((&))
import qualified Data.Map as Map
import Data.Proxy (Proxy(..))
import Data.String (IsString(fromString))
import Data.Text (Text)
import qualified Network.Wai.Handler.Warp as Warp
import Prelude
import Servant.API
import Servant.Server (Handler, serve)


-- | Backend for SwaggerPetstore.
-- This is used to supply functions for handling the individual routes.
data SwaggerPetstoreBackend m = SwaggerPetstoreBackend
  { addPet :: Pet -> m ()    {- ^  -}
  , deletePet :: Int -> Maybe Text -> m ()    {- ^  -}
  , findPetsByStatus :: Maybe [Text] -> m [Pet]    {- ^ Multiple status values can be provided with comma separated strings -}
  , findPetsByTags :: Maybe [Text] -> m [Pet]    {- ^ Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing. -}
  , getPetById :: Int -> m Pet    {- ^ Returns a single pet -}
  , updatePet :: Pet -> m ()    {- ^  -}
  , updatePetWithForm :: Int -> FormUpdatePetWithForm -> m ()    {- ^  -}
  , uploadFile :: Int -> FormUploadFile -> m ApiResponse    {- ^  -}
  , deleteOrder :: Text -> m ()    {- ^ For valid response try integer IDs with value < 1000. Anything above 1000 or nonintegers will generate API errors -}
  , getInventory :: m (Map.Map String Int)    {- ^ Returns a map of status codes to quantities -}
  , getOrderById :: Int -> m Order    {- ^ For valid response try integer IDs with value <= 5 or > 10. Other values will generated exceptions -}
  , placeOrder :: Order -> m Order    {- ^  -}
  , createUser :: User -> m ()    {- ^ This can only be done by the logged in user. -}
  , createUsersWithArrayInput :: [User] -> m ()    {- ^  -}
  , createUsersWithListInput :: [User] -> m ()    {- ^  -}
  , deleteUser :: Text -> m ()    {- ^ This can only be done by the logged in user. -}
  , getUserByName :: Text -> m User    {- ^  -}
  , loginUser :: Maybe Text -> Maybe Text -> m Text    {- ^  -}
  , logoutUser :: m ()    {- ^  -}
  , updateUser :: Text -> User -> m ()    {- ^ This can only be done by the logged in user. -}
  }

-- | Run the SwaggerPetstore server at the provided host and port.
runSwaggerPetstoreServer :: MonadIO m => Warp.HostPreference -> Warp.Port -> SwaggerPetstoreBackend Handler -> m ()
runSwaggerPetstoreServer host port backend =
  liftIO $ Warp.runSettings warpSettings $ serve (Proxy :: Proxy SwaggerPetstoreAPI) (serverFromBackend backend)
  where
    warpSettings = Warp.defaultSettings
                 & Warp.setPort port
                 & Warp.setHost host

    serverFromBackend SwaggerPetstoreBackend{..} =
      (coerce addPet :<|>
       coerce deletePet :<|>
       coerce findPetsByStatus :<|>
       coerce findPetsByTags :<|>
       coerce getPetById :<|>
       coerce updatePet :<|>
       coerce updatePetWithForm :<|>
       coerce uploadFile :<|>
       coerce deleteOrder :<|>
       coerce getInventory :<|>
       coerce getOrderById :<|>
       coerce placeOrder :<|>
       coerce createUser :<|>
       coerce createUsersWithArrayInput :<|>
       coerce createUsersWithListInput :<|>
       coerce deleteUser :<|>
       coerce getUserByName :<|>
       coerce loginUser :<|>
       coerce logoutUser :<|>
       coerce updateUser)

