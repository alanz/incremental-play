
-- Based on Chapter 3 : Efficient Self Versioning Documents of
-- https://www2.eecs.berkeley.edu/Pubs/TechRpts/1997/CSD-97-946.pdf
module Language.Incremental.Versioning
  (
  ) where

import Data.Tree
import Data.Tree.Zipper

-- ---------------------------------------------------------------------

data NodeVal =
  NV
   { nested :: NestedChanges
   , unversioned :: UnversionedData
   }

data NestedChanges = NestedChanges
data UnversionedData = UnversionedData

-- ---------------------------------------------------------------------

-- Low level interfaces

data Versioned t = Versioned t
data VersionId = VI Int

get :: Versioned t -> t
get = undefined

set :: Versioned t -> t -> Versioned t
set = undefined

changed :: Versioned t -> Bool
changed = undefined

alter_version :: Versioned t -> VersionId -> Versioned t
alter_version = undefined

had_value :: Versioned t -> t -> VersionId -> VersionId -> Bool
had_value v t from_version to_version = undefined

exists :: Versioned t -> Bool
exists = undefined

existsVersion :: Versioned t -> VerionId -> Bool
existsVersion = undefined

discard :: Versioned t -> Versioned t
discard = undefined

mark_deleted :: Versioned t -> Versioned t
mark_deleted = undefined

undelete :: Versioned t -> Versioned t
undelete = undefined
{-

<T> get ()
void set (<T> value)
bool changed ()
bool changed (from_version, to_version)
void alter_version (version_id)
bool had_value (<T> value, from_version, to_version)
bool exists ([version_id])
void discard ()
void mark_deleted ()
void undelete ()

Table 3.1: Interface to low-level versioned objects. Several of these
functions are expressed as templates; an instantiation is provided for
each versioned datatype. Arguments in square brackets are
optional. Versions are identified by clients using an opaque type,
which is implemented as an integer. (Its value is simply the index of
the version in the creation sequence.)
-}

-- ---------------------------------------------------------------------
-- Node level interface
-- A Node is a rosetree Node, with a versioned object inside.
-- ie Tree NodeVal

data ChangeScope = Local | Nested
                 deriving (Eq,Show)
type N = Tree NodeVal

AZ carry on here

{-
bool has_changes([local|nested])
bool has_changes(version_id, [local|nested])
    These routines permit clients to discover changes to a single node
    or to traverse an entire subtree, visiting only the changed
    areas. When no version is provided, the query refers to the
    current version. The optional argument restricts the query to only
    local or only nested changes.
-}

{-
node child(i)
node child(i, version_id)
    These methods return the 'i' th child.  With a single argument,
    the current (cached) version is used.  Similar pairs of methods
    exist for each versioned attribute of the node: parent link,
    versioned semantic data, etc.
-}

{-
void set_child(node, i)
    Sets the 'i' th child to node . Because the children are
    versioned, this method automatically records the change with the
    history log. Similar methods exist to update each versioned field.
-}
{-
void discard(and_nested?)
    Discards any uncommitted modifications to either this node alone
    or in the entire subtree rooted by it when and_nested?  is true.
-}
{-
bool exists([version_id])
    Determines whether the node exists in the current or a specified
    version.
-}
{-
bool is_new()
    Determines if a node was created in the current version.
-}
{-
void mark_deleted()
void undelete()
    Used to indicate that a node has been removed from the tree (or to
    reverse the decision).  undelete can only be used prior to
    committing the deletion.
-}
{-
Table 3.2: Summary of node-level interface used by incremental
analyses.  Each node maintains its own version history, and is capable
of reporting both local changes and nested changes—modifications
within the subtree rooted at the node.  The version_id arguments refer
to the document as a whole; they are efficiently translated into names
for values in the local history of each versioned object.
-}

-- ---------------------------------------------------------------------

-- Global Version Tree (GVT) tracks the parent relationship for
-- versions. Like the history view in emacs undotree.

-- ---------------------------------------------------------------------
