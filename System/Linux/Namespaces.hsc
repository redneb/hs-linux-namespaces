{- |
Module      : System.Linux.Namespaces

Stability   : provisional
Portability : non-portable (requires Linux)

This module provides bindings to the @unshare(2)@ and @setns(2)@ linux
system calls. These functions can be used to create new namespaces by
detaching the current process from its current namespaces, or to move
the current process to an already existing namespace. Note that linux
also provides the @clone(2)@ function which can be used to create new
namespaces, but we do not support this function in this module; the way
this function works makes it hard to use it from haskell as it interacts
badly with GHC'c RTS.

/Note/: Using this module in a program that uses the threaded RTS does
not make much sense. Namespaces are per process/thread and manipulating
them in one thread will not affect the namespaces of the other threads
of the same process. The threaded RTS makes it is hard to predict what
OS thread will be used to run the haskell threads. Therefore, using this
module in such applications will result in unpredictable behavior.
-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module System.Linux.Namespaces
    (
    -- * Main types and functions
      Namespace(..)
    , unshare
    , setNamespace
    -- * Utility functions
    , enterNamespace
    , NamespaceID
    , getNamespaceID
    -- * User/Group mappings
    , UserMapping(..)
    , GroupMapping(..)
    , writeUserMappings
    , writeGroupMappings
    ) where

#define _GNU_SOURCE
#include <sched.h>

import Foreign
import Foreign.C
import System.Posix.Types (Fd(..), ProcessID, UserID, GroupID)
import System.Posix.IO
import System.Posix.Files (readSymbolicLink)
import Control.Exception (bracket)
import Data.List (foldl')
import Data.Char (isDigit)
import Control.Arrow (first)

--------------------------------------------------------------------------------

-- | Types of namespaces.
data Namespace = IPC | Network | Mount | PID | User | UTS
  deriving (Show, Read, Eq, Bounded, Enum)

toCloneFlags :: Namespace -> CInt
toCloneFlags ns =
    case ns of
        IPC     -> (#const CLONE_NEWIPC)
        Network -> (#const CLONE_NEWNET)
        Mount   -> (#const CLONE_NEWNS)
        PID     -> (#const CLONE_NEWPID)
        User    -> (#const CLONE_NEWUSER)
        UTS     -> (#const CLONE_NEWUTS)

toProcName :: Namespace -> String
toProcName ns =
    case ns of
        IPC     -> "ipc"
        Network -> "net"
        Mount   -> "mnt"
        PID     -> "pid"
        User    -> "user"
        UTS     -> "uts"

-- | Detach the process from one or more namespaces and move it to new
-- ones. See the man page of @unshare(2)@ for more details.
unshare :: [Namespace] -> IO ()
unshare nss =
    throwErrnoIfMinus1_ "unshare" $ c_unshare flags
  where
    flags = foldl' (.|.) 0 (map toCloneFlags nss)

-- | Move process to an already existing namespace. See the man page of
-- @setns(2)@ for more details. See also 'enterNamespace' for a slightly
-- higher level version of this function.
setNamespace
    :: Fd -- ^ A file descriptor referring to a namespace file in a
          -- @\/proc\/[pid]\/ns\/@ directory.
    -> Maybe Namespace -- ^ Specify the namespace type that the file
                       -- descriptor must refer to. If the two types do not
                       -- much, the function will fail. Use 'Nothing' to
                       -- allow any type.
    -> IO ()
setNamespace fd mns =
    throwErrnoIfMinus1_ "setNamespace" $ c_setns fd nstype
  where
    nstype = maybe 0 toCloneFlags mns

--------------------------------------------------------------------------------

-- | Move process to an already existing namespace. This is a wrapper
-- around 'setNamespace'. This function requires @\/proc@ to be
-- mounted.
enterNamespace
    :: ProcessID -- ^ The @pid@ of any process in the target namespace.
    -> Namespace -- ^ The type of the namespace.
    -> IO ()
enterNamespace pid ns =
    bracket openFd' closeFd $ \fd ->
        setNamespace fd (Just ns)
  where
    openFd' = openFd path ReadOnly Nothing defaultFileFlags {nonBlock = True}
    path = toProcPath (Just pid) ns

-- | A unique namespace id.
newtype NamespaceID = NamespaceID CInt
  deriving (Eq, Ord, Enum, Integral, Num, Real)

instance Show NamespaceID where
    show (NamespaceID x) = show x

instance Read NamespaceID where
    readsPrec prec s = map (first NamespaceID) $ readsPrec prec s

-- | Retrieve the id of a Namespace. Useful for debugging. This
-- function requires @\/proc@ to be mounted.
getNamespaceID :: Maybe ProcessID -> Namespace -> IO NamespaceID
getNamespaceID mpid ns = do
    s <- readSymbolicLink path
    let s' = takeWhile isDigit $ dropWhile (not . isDigit) s
    return (read s')
  where
    path = toProcPath mpid ns

--------------------------------------------------------------------------------

-- | A single user mapping, used with user namespaces. See
-- @user_namespaces(7)@ for more details.
data UserMapping = UserMapping UserID UserID Int
  deriving (Show, Eq)

-- | A single group mapping, used with user namespaces. See
-- @user_namespaces(7)@ for more details.
data GroupMapping = GroupMapping GroupID GroupID Int
  deriving (Show, Eq)

-- | Define the user mappings for the specified user namespace. This
-- function requires @\/proc@ to be mounted. See @user_namespaces(7)@
-- for more details.
writeUserMappings
    :: Maybe ProcessID -- ^ The pid of any process in the target user
                       -- namespace. 'Nothing' means use the current
                       -- process.
    -> [UserMapping] -- The mappings.
    -> IO ()
writeUserMappings mpid ms =
    writeFile path s
  where
    path = toProcDir mpid ++ "/uid_map"
    s = concatMap toStr ms
    toStr (UserMapping o i l) = show o ++ " " ++ show i ++ " " ++ show l ++ "\n"

-- | Define the group mappings for the specified user namespace. This
-- function requires @\/proc@ to be mounted. See @user_namespaces(7)@
-- for more details.
writeGroupMappings
    :: Maybe ProcessID -- ^ The pid of any process in the target user
                       -- namespace. 'Nothing' means use the current
                       -- process.
    -> [GroupMapping] -- The mappings.
    -> IO ()
writeGroupMappings mpid ms =
    writeFile path s
  where
    path = toProcDir mpid ++ "/gid_map"
    s = concatMap toStr ms
    toStr (GroupMapping o i l) = show o ++ " " ++ show i ++ " " ++ show l ++ "\n"

--------------------------------------------------------------------------------

toProcPath :: Maybe ProcessID -> Namespace -> String
toProcPath mpid ns = toProcDir mpid ++ "/ns/" ++ toProcName ns
{-# INLINE toProcPath #-}

toProcDir :: Maybe ProcessID -> String
toProcDir mpid = "/proc/" ++ maybe "self" show mpid
{-# INLINE toProcDir #-}

--------------------------------------------------------------------------------

foreign import ccall unsafe "unshare"
    c_unshare :: CInt -> IO CInt

foreign import ccall unsafe "setns"
    c_setns :: Fd -> CInt -> IO CInt
