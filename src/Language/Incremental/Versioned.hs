{-# LANGUAGE StandaloneDeriving #-}
-- Based on Chapter 3 : Efficient Self Versioning Documents of
-- https://www2.eecs.berkeley.edu/Pubs/TechRpts/1997/CSD-97-946.pdf
module Language.Incremental.Versioned
  (
    NodeVal(..)
  , NestedChanges(..)
  , Versioned(..)
  , UnversionedData(..)

  , VersionId
  , firstId
  , nextId

  , new
  , get
  , set
  , changed
  , changed_versions
  , alter_version
  , had_value
  , exists
  , discard
  , mark_deleted
  , undelete
  ) where

-- ---------------------------------------------------------------------

import qualified Data.Map.Ordered as OMap
import           Data.Map.Ordered ( (|<), (<|), (|>), (>|) )
import           Data.Tree
import           Data.Tree.Zipper

-- ---------------------------------------------------------------------

data NodeVal t =
  NV
   { nested      :: NestedChanges
   , unversioned :: UnversionedData
   }

data NestedChanges = NestedChanges
data UnversionedData = UnversionedData

-- ---------------------------------------------------------------------

-- Low level interfaces

data Versioned t =
  Versioned
    { vCached :: t -- most recent version value, cached.
    , vData :: OMap.OMap VersionId t
    , vChanged :: Bool
    }
deriving instance (Show t) => Show (Versioned t)

-- ---------------------------------------------------------------------

data VersionId = VI Int
               deriving (Eq,Ord,Show)

nextId :: VersionId -> VersionId
nextId (VI n) = VI (n+1)

firstId :: VersionId
firstId = VI 0

-- ---------------------------------------------------------------------

new :: VersionId -> t -> Versioned t
new v t = Versioned t (OMap.empty >| (v, t)) False

get :: Versioned t -> t
get (Versioned t _ _) = t

set :: VersionId -> Versioned t -> t -> Versioned t
set v (Versioned _ m _) t = Versioned t (m >| (v, t) ) True

changed :: Versioned t -> Bool
changed Versioned {vChanged = c} = c

changed_versions :: (Show t) => Versioned t -> VersionId -> VersionId -> Bool
changed_versions v from_version to_version = r
  where
    l = OMap.size (vData v)
    kvs = OMap.assocs (vData v)
    kvs1 = dropWhile (\(k,_) -> k < from_version) kvs
    kvs2 = dropWhile (\(k,_) -> k > to_version) (reverse kvs1)
    r = not (null kvs2)

alter_version :: Versioned t -> VersionId -> Versioned t
alter_version = undefined

had_value :: Versioned t -> t -> VersionId -> VersionId -> Bool
had_value v t from_version to_version = undefined

exists :: Versioned t -> Maybe VersionId -> Bool
exists = undefined

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

-- Global Version Tree (GVT) tracks the parent relationship for
-- versions. Like the history view in emacs undotree.

data VersionTree = VT Int
                 deriving Show

-- ---------------------------------------------------------------------
{-

From Appendix A.1 : Version Alteration Algorithm for Objects with
Differential Storage
https://www2.eecs.berkeley.edu/Pubs/TechRpts/1997/CSD-97-946.pdf

The following algorithm is used to alter the current (cached) version
of a differential versioned object. Conceptually, it forms the path in
the global version tree between the current version and the target
version, then projects this path onto its local equivalent. The
elements of the local path are exactly the differential log entries
that must be applied (in the proper order) to transform the current
version of the object to its value in the target version. Neither path
is constructed explicitly; the actual computation is
traversal-based. Note that some unnecessary projections have been
optimized out by directly updating the global version identifier.

The right (‘redo’) side of the path is computed recursively, starting
from the target version.  The actual update takes place as the
recursion unwinds

void diff_vobject::alter_version (const VG *vg, VData *vd, int target_gvid) {
  Vlstring *vls = (Vlstring*)vd;
  int from = vg->currentI;
  int   to = target_gvid;

  if (from == to) return;
  // Updated current pointer in the data object before we begin.
  vls->current = project(vg, target_gvid);

  GD *from_gd = vg->log->maps[from];
  GD *to_gd   = vg->log->maps[to  ];
  // This is both the returned value from 'project()'
  // and the limit setting for the subsequent
  // 'project()' call.
  int lvid = -1;

  if (from_gd == to_gd) {
    // Common (and easy!) case: source and target in same linear run.
    if (from > to) {
      for (lvid = project(vg, from); gvids[lvid] > to; --lvid)
        vls->apply(values[lvid], false);
    } else if (to > from) {
      for (lvid = project(vg, from + 1);
           lvid < num && gvids[lvid] > from && gvids[lvid] <= to;
           lvid++)
        vls->apply(values[lvid], true);
    }
    return;
  } else {
    // Uncommon (and hard) case: perform a general search.
    const GD *gd = from_gd;
    if (vg->log->ancestor(gd, to_gd) && from + 1 < vg->log->num_maps) {
      // Handle suffix in starting 'gd'.
      for (lvid = project(vg, from + 1);
           lvid < num && gvids[lvid] > from &&
             gvids[lvid] < gd->start + gd->num;
           lvid++)
        vls->apply(values[lvid], true);
    } else {
      // Handle prefix in starting 'gd'.
      for (lvid = project(vg, from);
           gvids[lvid] >= from_gd->start &&
             gvids[lvid] < from_gd->start + from_gd->num;
           --lvid)
        vls->apply(values[lvid], false);
      bool need_proj = true;
      gd = from_gd->parent;
      while (!vg->log->ancestor(gd, to_gd)) {
        if (need_proj) lvid = project(vg, gd->start + gd->num - 1, lvid);
        for (;
             gvids[lvid] >= gd->start && gvids[lvid] < gd->start + gd->num;
             --lvid)
          vls->apply(values[lvid], false);
        need_proj = gd->parent->kids[0] != gd;
        gd = gd->parent;
      }
    }
    const GD *lca = gd;
    // Now we process the redo side.
    // This is not handled the same way, since the path must be traversed
    // top-to-bottom, and we cannot discover it that way.  Use recursion to
    // wind up the path and then traverse the path during the unwind.
    // The LCA has already been computed.
    gd = to_gd;
    if (vg->log->ancestor(gd, from_gd)) {
      // Handle suffix in ending gd.
      for (lvid = project(vg, gd->start + gd->num - 1);
           gvids[lvid] > to;
           --lvid)
        vls->apply(values[lvid], false);
    } else {
      int lvid = project(vg, to_gd->start);
      int lvid_copy = lvid;
      find_path_and_unwind_doing_alter_version(vg, vls, gd, lca, lvid);
      // Handle prefix in ending gd.
      for (lvid = lvid_copy;
           lvid < num && gvids[lvid] >= to_gd->start && gvids[lvid] <= to;
           lvid++)
        vls->apply(values[lvid], true);
    }
  }
}

void find_path_and_unwind_doing_alter_version (Vlstring *vls,
                                               const GD *gd,
                                               const GD *lca,
                                               int &lvid) {
  const GD *parent = gd->parent;
  if (parent == lca) return;
  lvid = project(vg, parent->start, lvid);
  int local_lvid = lvid;
  find_path_and_unwind_doing_alter_version(vg, vls, parent, lca, lvid);
  for (;
       lvid < num && gvids[lvid] >= parent->start &&
         gvids[lvid] < parent->start + parent->num;
       lvid++)
    vls->apply(values[lvid], true);
}

-}
