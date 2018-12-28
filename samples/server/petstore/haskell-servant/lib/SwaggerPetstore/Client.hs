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
import Data.Text (Text)
import Servant.API
import Servant.Client (ClientM, client, hoistClient)


-- | Monads in which client calls may be embedded.
class Monad m => SwaggerPetstoreClientMonad m where
  liftSwaggerPetstoreClient :: ClientM a -> m a


{- |  -}
addPet :: SwaggerPetstoreClientMonad m => Pet -> m ()
addPet x0  = addPet' (coerce x0) 

{- |  -}
deletePet :: SwaggerPetstoreClientMonad m => Int -> Maybe Text -> m ()
deletePet x0 x1  = deletePet' (coerce x0) (coerce x1) 

{- | Multiple status values can be provided with comma separated strings -}
findPetsByStatus :: SwaggerPetstoreClientMonad m => Maybe [Text] -> m [Pet]
findPetsByStatus x0  = findPetsByStatus' (coerce x0) 

{- | Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing. -}
findPetsByTags :: SwaggerPetstoreClientMonad m => Maybe [Text] -> m [Pet]
findPetsByTags x0  = findPetsByTags' (coerce x0) 

{- | Returns a single pet -}
getPetById :: SwaggerPetstoreClientMonad m => Int -> m Pet
getPetById x0  = getPetById' (coerce x0) 

{- |  -}
updatePet :: SwaggerPetstoreClientMonad m => Pet -> m ()
updatePet x0  = updatePet' (coerce x0) 

{- |  -}
updatePetWithForm :: SwaggerPetstoreClientMonad m => Int -> FormUpdatePetWithForm -> m ()
updatePetWithForm x0 x1  = updatePetWithForm' (coerce x0) (coerce x1) 

{- |  -}
uploadFile :: SwaggerPetstoreClientMonad m => Int -> FormUploadFile -> m ApiResponse
uploadFile x0 x1  = uploadFile' (coerce x0) (coerce x1) 

{- | For valid response try integer IDs with value < 1000. Anything above 1000 or nonintegers will generate API errors -}
deleteOrder :: SwaggerPetstoreClientMonad m => Text -> m ()
deleteOrder x0  = deleteOrder' (coerce x0) 

{- | Returns a map of status codes to quantities -}
getInventory :: SwaggerPetstoreClientMonad m => m (Map.Map String Int)
getInventory  = getInventory' 

{- | For valid response try integer IDs with value <= 5 or > 10. Other values will generated exceptions -}
getOrderById :: SwaggerPetstoreClientMonad m => Int -> m Order
getOrderById x0  = getOrderById' (coerce x0) 

{- |  -}
placeOrder :: SwaggerPetstoreClientMonad m => Order -> m Order
placeOrder x0  = placeOrder' (coerce x0) 

{- | This can only be done by the logged in user. -}
createUser :: SwaggerPetstoreClientMonad m => User -> m ()
createUser x0  = createUser' (coerce x0) 

{- |  -}
createUsersWithArrayInput :: SwaggerPetstoreClientMonad m => [User] -> m ()
createUsersWithArrayInput x0  = createUsersWithArrayInput' (coerce x0) 

{- |  -}
createUsersWithListInput :: SwaggerPetstoreClientMonad m => [User] -> m ()
createUsersWithListInput x0  = createUsersWithListInput' (coerce x0) 

{- | This can only be done by the logged in user. -}
deleteUser :: SwaggerPetstoreClientMonad m => Text -> m ()
deleteUser x0  = deleteUser' (coerce x0) 

{- |  -}
getUserByName :: SwaggerPetstoreClientMonad m => Text -> m User
getUserByName x0  = getUserByName' (coerce x0) 

{- |  -}
loginUser :: SwaggerPetstoreClientMonad m => Maybe Text -> Maybe Text -> m Text
loginUser x0 x1  = loginUser' (coerce x0) (coerce x1) 

{- |  -}
logoutUser :: SwaggerPetstoreClientMonad m => m ()
logoutUser  = logoutUser' 

{- | This can only be done by the logged in user. -}
updateUser :: SwaggerPetstoreClientMonad m => Text -> User -> m ()
updateUser x0 x1  = updateUser' (coerce x0) (coerce x1) 


( addPet'
 :<|>  deletePet'
 :<|>  findPetsByStatus'
 :<|>  findPetsByTags'
 :<|>  getPetById'
 :<|>  updatePet'
 :<|>  updatePetWithForm'
 :<|>  uploadFile'
 :<|>  deleteOrder'
 :<|>  getInventory'
 :<|>  getOrderById'
 :<|>  placeOrder'
 :<|>  createUser'
 :<|>  createUsersWithArrayInput'
 :<|>  createUsersWithListInput'
 :<|>  deleteUser'
 :<|>  getUserByName'
 :<|>  loginUser'
 :<|>  logoutUser'
 :<|>  updateUser'
 ) = hoistClient proxy liftSwaggerPetstoreClient (client proxy) where
     proxy = (Proxy :: Proxy SwaggerPetstoreAPI)



