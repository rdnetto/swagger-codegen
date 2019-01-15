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
type AddPetRoute
  =  "pet"
  :> ReqBody '[JSON] Pet
  :> Verb 'POST 200 '[JSON] ()

type DeletePetRoute
  =  "pet"
  :> Capture "petId" Int
  :> Header "api_key" Text
  :> Verb 'DELETE 200 '[JSON] ()

type FindPetsByStatusRoute
  =  "pet"
  :> "findByStatus"
  :> QueryParam "status" (QueryList 'MultiParamArray (Text))
  :> Verb 'GET 200 '[JSON] [Pet]

type FindPetsByTagsRoute
  =  "pet"
  :> "findByTags"
  :> QueryParam "tags" (QueryList 'MultiParamArray (Text))
  :> Verb 'GET 200 '[JSON] [Pet]

type GetPetByIdRoute
  =  "pet"
  :> Capture "petId" Int
  :> Verb 'GET 200 '[JSON] Pet

type UpdatePetRoute
  =  "pet"
  :> ReqBody '[JSON] Pet
  :> Verb 'PUT 200 '[JSON] ()

type UpdatePetWithFormRoute
  =  "pet"
  :> Capture "petId" Int
  :> ReqBody '[FormUrlEncoded] FormUpdatePetWithForm
  :> Verb 'POST 200 '[JSON] ()

type UploadFileRoute
  =  "pet"
  :> Capture "petId" Int
  :> "uploadImage"
  :> ReqBody '[FormUrlEncoded] FormUploadFile
  :> Verb 'POST 200 '[JSON] ApiResponse

type DeleteOrderRoute
  =  "store"
  :> "order"
  :> Capture "orderId" Text
  :> Verb 'DELETE 200 '[JSON] ()

type GetInventoryRoute
  =  "store"
  :> "inventory"
  :> Verb 'GET 200 '[JSON] (Map.Map String Int)

type GetOrderByIdRoute
  =  "store"
  :> "order"
  :> Capture "orderId" Int
  :> Verb 'GET 200 '[JSON] Order

type PlaceOrderRoute
  =  "store"
  :> "order"
  :> ReqBody '[JSON] Order
  :> Verb 'POST 200 '[JSON] Order

type CreateUserRoute
  =  "user"
  :> ReqBody '[JSON] User
  :> Verb 'POST 200 '[JSON] ()

type CreateUsersWithArrayInputRoute
  =  "user"
  :> "createWithArray"
  :> ReqBody '[JSON] [User]
  :> Verb 'POST 200 '[JSON] ()

type CreateUsersWithListInputRoute
  =  "user"
  :> "createWithList"
  :> ReqBody '[JSON] [User]
  :> Verb 'POST 200 '[JSON] ()

type DeleteUserRoute
  =  "user"
  :> Capture "username" Text
  :> Verb 'DELETE 200 '[JSON] ()

type GetUserByNameRoute
  =  "user"
  :> Capture "username" Text
  :> Verb 'GET 200 '[JSON] User

type LoginUserRoute
  =  "user"
  :> "login"
  :> QueryParam "username" Text
  :> QueryParam "password" Text
  :> Verb 'GET 200 '[JSON] Text

type LogoutUserRoute
  =  "user"
  :> "logout"
  :> Verb 'GET 200 '[JSON] ()

type UpdateUserRoute
  =  "user"
  :> Capture "username" Text
  :> ReqBody '[JSON] User
  :> Verb 'PUT 200 '[JSON] ()


type SwaggerPetstoreAPI
    =    AddPetRoute
    :<|> DeletePetRoute
    :<|> FindPetsByStatusRoute
    :<|> FindPetsByTagsRoute
    :<|> GetPetByIdRoute
    :<|> UpdatePetRoute
    :<|> UpdatePetWithFormRoute
    :<|> UploadFileRoute
    :<|> DeleteOrderRoute
    :<|> GetInventoryRoute
    :<|> GetOrderByIdRoute
    :<|> PlaceOrderRoute
    :<|> CreateUserRoute
    :<|> CreateUsersWithArrayInputRoute
    :<|> CreateUsersWithListInputRoute
    :<|> DeleteUserRoute
    :<|> GetUserByNameRoute
    :<|> LoginUserRoute
    :<|> LogoutUserRoute
    :<|> UpdateUserRoute


