{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-imports -fno-warn-missing-signatures -freduction-depth=0 #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ViewPatterns #-}

module SwaggerPetstore.Client (
    SwaggerPetstoreClientMonad(..),
    addPet,
    deletePet,
    findPetsByStatus,
    findPetsByTags,
    getPetById,
    updatePet,
    updatePetWithForm,
    uploadFile,
    deleteOrder,
    getInventory,
    getOrderById,
    placeOrder,
    createUser,
    createUsersWithArrayInput,
    createUsersWithListInput,
    deleteUser,
    getUserByName,
    loginUser,
    logoutUser,
    updateUser,
  ) where

import SwaggerPetstore.API
import SwaggerPetstore.Common
import SwaggerPetstore.Types

import Data.Coerce (coerce)
import qualified Data.Map as Map
import Data.Proxy (Proxy(..))
import Prelude
import Servant.API
import Servant.Client (ClientM, client, hoistClient)

import Data.Text (Text)
import Data.Time.LocalTime (ZonedTime)
import qualified Data.Map as Map


-- | Monads in which client calls may be embedded.
class Monad m => SwaggerPetstoreClientMonad m where
  liftSwaggerPetstoreClient :: ClientM a -> m a


-- We generate the clients directly from the routes as a workaround for https://github.com/haskell-servant/servant/issues/986
{- | POST /pet
     
 -}
addPet :: SwaggerPetstoreClientMonad m => Pet -> m ()
addPet x0  = addPet' (coerce x0)  where
  addPet' = hoistClient proxy liftSwaggerPetstoreClient (client proxy)
  proxy = Proxy :: Proxy AddPetRoute

{- | DELETE /pet/{petId}
     
 -}
deletePet :: SwaggerPetstoreClientMonad m => Int -> Maybe Text -> m ()
deletePet x0 x1  = deletePet' (coerce x0) (coerce x1)  where
  deletePet' = hoistClient proxy liftSwaggerPetstoreClient (client proxy)
  proxy = Proxy :: Proxy DeletePetRoute

{- | GET /pet/findByStatus
     Multiple status values can be provided with comma separated strings
 -}
findPetsByStatus :: SwaggerPetstoreClientMonad m => Maybe [Text] -> m [Pet]
findPetsByStatus x0  = findPetsByStatus' (coerce x0)  where
  findPetsByStatus' = hoistClient proxy liftSwaggerPetstoreClient (client proxy)
  proxy = Proxy :: Proxy FindPetsByStatusRoute

{- | GET /pet/findByTags
     Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
 -}
findPetsByTags :: SwaggerPetstoreClientMonad m => Maybe [Text] -> m [Pet]
findPetsByTags x0  = findPetsByTags' (coerce x0)  where
  findPetsByTags' = hoistClient proxy liftSwaggerPetstoreClient (client proxy)
  proxy = Proxy :: Proxy FindPetsByTagsRoute

{- | GET /pet/{petId}
     Returns a single pet
 -}
getPetById :: SwaggerPetstoreClientMonad m => Int -> m Pet
getPetById x0  = getPetById' (coerce x0)  where
  getPetById' = hoistClient proxy liftSwaggerPetstoreClient (client proxy)
  proxy = Proxy :: Proxy GetPetByIdRoute

{- | PUT /pet
     
 -}
updatePet :: SwaggerPetstoreClientMonad m => Pet -> m ()
updatePet x0  = updatePet' (coerce x0)  where
  updatePet' = hoistClient proxy liftSwaggerPetstoreClient (client proxy)
  proxy = Proxy :: Proxy UpdatePetRoute

{- | POST /pet/{petId}
     
 -}
updatePetWithForm :: SwaggerPetstoreClientMonad m => Int -> FormUpdatePetWithForm -> m ()
updatePetWithForm x0 x1  = updatePetWithForm' (coerce x0) (coerce x1)  where
  updatePetWithForm' = hoistClient proxy liftSwaggerPetstoreClient (client proxy)
  proxy = Proxy :: Proxy UpdatePetWithFormRoute

{- | POST /pet/{petId}/uploadImage
     
 -}
uploadFile :: SwaggerPetstoreClientMonad m => Int -> FormUploadFile -> m ApiResponse
uploadFile x0 x1  = uploadFile' (coerce x0) (coerce x1)  where
  uploadFile' = hoistClient proxy liftSwaggerPetstoreClient (client proxy)
  proxy = Proxy :: Proxy UploadFileRoute

{- | DELETE /store/order/{orderId}
     For valid response try integer IDs with value < 1000. Anything above 1000 or nonintegers will generate API errors
 -}
deleteOrder :: SwaggerPetstoreClientMonad m => Text -> m ()
deleteOrder x0  = deleteOrder' (coerce x0)  where
  deleteOrder' = hoistClient proxy liftSwaggerPetstoreClient (client proxy)
  proxy = Proxy :: Proxy DeleteOrderRoute

{- | GET /store/inventory
     Returns a map of status codes to quantities
 -}
getInventory :: SwaggerPetstoreClientMonad m => m (Map.Map String Int)
getInventory  = getInventory'  where
  getInventory' = hoistClient proxy liftSwaggerPetstoreClient (client proxy)
  proxy = Proxy :: Proxy GetInventoryRoute

{- | GET /store/order/{orderId}
     For valid response try integer IDs with value <= 5 or > 10. Other values will generated exceptions
 -}
getOrderById :: SwaggerPetstoreClientMonad m => Int -> m Order
getOrderById x0  = getOrderById' (coerce x0)  where
  getOrderById' = hoistClient proxy liftSwaggerPetstoreClient (client proxy)
  proxy = Proxy :: Proxy GetOrderByIdRoute

{- | POST /store/order
     
 -}
placeOrder :: SwaggerPetstoreClientMonad m => Order -> m Order
placeOrder x0  = placeOrder' (coerce x0)  where
  placeOrder' = hoistClient proxy liftSwaggerPetstoreClient (client proxy)
  proxy = Proxy :: Proxy PlaceOrderRoute

{- | POST /user
     This can only be done by the logged in user.
 -}
createUser :: SwaggerPetstoreClientMonad m => User -> m ()
createUser x0  = createUser' (coerce x0)  where
  createUser' = hoistClient proxy liftSwaggerPetstoreClient (client proxy)
  proxy = Proxy :: Proxy CreateUserRoute

{- | POST /user/createWithArray
     
 -}
createUsersWithArrayInput :: SwaggerPetstoreClientMonad m => [User] -> m ()
createUsersWithArrayInput x0  = createUsersWithArrayInput' (coerce x0)  where
  createUsersWithArrayInput' = hoistClient proxy liftSwaggerPetstoreClient (client proxy)
  proxy = Proxy :: Proxy CreateUsersWithArrayInputRoute

{- | POST /user/createWithList
     
 -}
createUsersWithListInput :: SwaggerPetstoreClientMonad m => [User] -> m ()
createUsersWithListInput x0  = createUsersWithListInput' (coerce x0)  where
  createUsersWithListInput' = hoistClient proxy liftSwaggerPetstoreClient (client proxy)
  proxy = Proxy :: Proxy CreateUsersWithListInputRoute

{- | DELETE /user/{username}
     This can only be done by the logged in user.
 -}
deleteUser :: SwaggerPetstoreClientMonad m => Text -> m ()
deleteUser x0  = deleteUser' (coerce x0)  where
  deleteUser' = hoistClient proxy liftSwaggerPetstoreClient (client proxy)
  proxy = Proxy :: Proxy DeleteUserRoute

{- | GET /user/{username}
     
 -}
getUserByName :: SwaggerPetstoreClientMonad m => Text -> m User
getUserByName x0  = getUserByName' (coerce x0)  where
  getUserByName' = hoistClient proxy liftSwaggerPetstoreClient (client proxy)
  proxy = Proxy :: Proxy GetUserByNameRoute

{- | GET /user/login
     
 -}
loginUser :: SwaggerPetstoreClientMonad m => Maybe Text -> Maybe Text -> m Text
loginUser x0 x1  = loginUser' (coerce x0) (coerce x1)  where
  loginUser' = hoistClient proxy liftSwaggerPetstoreClient (client proxy)
  proxy = Proxy :: Proxy LoginUserRoute

{- | GET /user/logout
     
 -}
logoutUser :: SwaggerPetstoreClientMonad m => m ()
logoutUser  = logoutUser'  where
  logoutUser' = hoistClient proxy liftSwaggerPetstoreClient (client proxy)
  proxy = Proxy :: Proxy LogoutUserRoute

{- | PUT /user/{username}
     This can only be done by the logged in user.
 -}
updateUser :: SwaggerPetstoreClientMonad m => Text -> User -> m ()
updateUser x0 x1  = updateUser' (coerce x0) (coerce x1)  where
  updateUser' = hoistClient proxy liftSwaggerPetstoreClient (client proxy)
  proxy = Proxy :: Proxy UpdateUserRoute



