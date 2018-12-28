{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-imports -freduction-depth=0 #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module SwaggerPetstore.API where

import SwaggerPetstore.Common
import SwaggerPetstore.Types

import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Prelude
import Servant.API
import Servant.API.Verbs (StdMethod(..), Verb)
import Web.Internal.FormUrlEncoded (ToForm(..), FromForm(..), parseUnique)


data FormUpdatePetWithForm = FormUpdatePetWithForm
  { updatePetWithFormName :: Text
  , updatePetWithFormStatus :: Text
  } deriving (Show, Eq, Generic)

instance FromForm FormUpdatePetWithForm where
  fromForm inputs = FormUpdatePetWithForm <$> parseUnique "name" inputs <*> parseUnique "status" inputs

instance ToForm FormUpdatePetWithForm where
  toForm value =
    [ ("name", toQueryParam $ updatePetWithFormName value)
    , ("status", toQueryParam $ updatePetWithFormStatus value)
    ]
data FormUploadFile = FormUploadFile
  { uploadFileAdditionalMetadata :: Text
  , uploadFileFile :: FilePath
  } deriving (Show, Eq, Generic)

instance FromForm FormUploadFile where
  fromForm inputs = FormUploadFile <$> parseUnique "additionalMetadata" inputs <*> parseUnique "file" inputs

instance ToForm FormUploadFile where
  toForm value =
    [ ("additionalMetadata", toQueryParam $ uploadFileAdditionalMetadata value)
    , ("file", toQueryParam $ uploadFileFile value)
    ]


-- | Servant type-level API, generated from the Swagger spec for SwaggerPetstore.
type SwaggerPetstoreAPI
    =    "pet" :> ReqBody '[JSON] Pet :> Verb 'POST 200 '[JSON] () -- 'addPet' route
    :<|> "pet" :> Capture "petId" Int :> Header "api_key" Text :> Verb 'DELETE 200 '[JSON] () -- 'deletePet' route
    :<|> "pet" :> "findByStatus" :> QueryParam "status" (QueryList 'MultiParamArray (Text)) :> Verb 'GET 200 '[JSON] [Pet] -- 'findPetsByStatus' route
    :<|> "pet" :> "findByTags" :> QueryParam "tags" (QueryList 'MultiParamArray (Text)) :> Verb 'GET 200 '[JSON] [Pet] -- 'findPetsByTags' route
    :<|> "pet" :> Capture "petId" Int :> Verb 'GET 200 '[JSON] Pet -- 'getPetById' route
    :<|> "pet" :> ReqBody '[JSON] Pet :> Verb 'PUT 200 '[JSON] () -- 'updatePet' route
    :<|> "pet" :> Capture "petId" Int :> ReqBody '[FormUrlEncoded] FormUpdatePetWithForm :> Verb 'POST 200 '[JSON] () -- 'updatePetWithForm' route
    :<|> "pet" :> Capture "petId" Int :> "uploadImage" :> ReqBody '[FormUrlEncoded] FormUploadFile :> Verb 'POST 200 '[JSON] ApiResponse -- 'uploadFile' route
    :<|> "store" :> "order" :> Capture "orderId" Text :> Verb 'DELETE 200 '[JSON] () -- 'deleteOrder' route
    :<|> "store" :> "inventory" :> Verb 'GET 200 '[JSON] (Map.Map String Int) -- 'getInventory' route
    :<|> "store" :> "order" :> Capture "orderId" Int :> Verb 'GET 200 '[JSON] Order -- 'getOrderById' route
    :<|> "store" :> "order" :> ReqBody '[JSON] Order :> Verb 'POST 200 '[JSON] Order -- 'placeOrder' route
    :<|> "user" :> ReqBody '[JSON] User :> Verb 'POST 200 '[JSON] () -- 'createUser' route
    :<|> "user" :> "createWithArray" :> ReqBody '[JSON] [User] :> Verb 'POST 200 '[JSON] () -- 'createUsersWithArrayInput' route
    :<|> "user" :> "createWithList" :> ReqBody '[JSON] [User] :> Verb 'POST 200 '[JSON] () -- 'createUsersWithListInput' route
    :<|> "user" :> Capture "username" Text :> Verb 'DELETE 200 '[JSON] () -- 'deleteUser' route
    :<|> "user" :> Capture "username" Text :> Verb 'GET 200 '[JSON] User -- 'getUserByName' route
    :<|> "user" :> "login" :> QueryParam "username" Text :> QueryParam "password" Text :> Verb 'GET 200 '[JSON] Text -- 'loginUser' route
    :<|> "user" :> "logout" :> Verb 'GET 200 '[JSON] () -- 'logoutUser' route
    :<|> "user" :> Capture "username" Text :> ReqBody '[JSON] User :> Verb 'PUT 200 '[JSON] () -- 'updateUser' route


