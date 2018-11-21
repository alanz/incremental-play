-- Based on Chapter 3 : Efficient Self Versioning Documents of
-- https://www2.eecs.berkeley.edu/Pubs/TechRpts/1997/CSD-97-946.pdf
module Language.Incremental.Versioning
  (
  ) where

import Data.Tree
import Data.Tree.Zipper
import Language.Incremental.Versioned

-- ---------------------------------------------------------------------
-- Node level interface
-- A Node is a rosetree Node, with a versioned object inside.
-- ie Tree NodeVal

data ChangeScope = Local | Nested
                 deriving (Eq,Show)
type N t = Tree (NodeVal t)
type ChildIdx = Int

{-
bool has_changes([local|nested])
bool has_changes(version_id, [local|nested])
    These routines permit clients to discover changes to a single node
    or to traverse an entire subtree, visiting only the changed
    areas. When no version is provided, the query refers to the
    current version. The optional argument restricts the query to only
    local or only nested changes.
-}

has_changes :: N t -> Maybe VersionId -> Maybe ChangeScope -> Bool
has_changes = undefined


{-
node child(i)
node child(i, version_id)
    These methods return the 'i' th child.  With a single argument,
    the current (cached) version is used.  Similar pairs of methods
    exist for each versioned attribute of the node: parent link,
    versioned semantic data, etc.
-}
node_child :: N t -> ChildIdx -> Maybe VersionId -> N t
node_child = undefined -- Total?

{-
void set_child(node, i)
    Sets the 'i' th child to node . Because the children are
    versioned, this method automatically records the change with the
    history log. Similar methods exist to update each versioned field.
-}
set_child :: N t -> N t -> ChildIdx -> N t
set_child parent child idx = undefined

{-
void discard(and_nested?)
    Discards any uncommitted modifications to either this node alone
    or in the entire subtree rooted by it when and_nested?  is true.
-}
discard :: N t -> Maybe Bool -> N t
discard = undefined

{-
bool exists([version_id])
    Determines whether the node exists in the current or a specified
    version.
-}
exists :: N t -> Maybe VersionId -> Bool
exists = undefined

{-
bool is_new()
    Determines if a node was created in the current version.
-}
is_new :: N t -> Bool
is_new = undefined

{-
void mark_deleted()
void undelete()
    Used to indicate that a node has been removed from the tree (or to
    reverse the decision).  undelete can only be used prior to
    committing the deletion.
-}

mark_deleted :: N t -> N t
mark_deleted = undefined

undelete :: N t -> N t
undelete = undefined

{-
Table 3.2: Summary of node-level interface used by incremental
analyses.  Each node maintains its own version history, and is capable
of reporting both local changes and nested changes—modifications
within the subtree rooted at the node.  The version_id arguments refer
to the document as a whole; they are efficiently translated into names
for values in the local history of each versioned object.
-}

-- ---------------------------------------------------------------------
