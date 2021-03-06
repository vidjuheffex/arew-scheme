(library (scheme hash-table)
  (export
   make-hash-table
   hash-table
   hash-table-unfold
   alist->hash-table

   hash-table?
   hash-table-contains?
   hash-table-empty?
   hash-table=?
   hash-table-mutable?

   hash-table-ref
   hash-table-ref/default

   hash-table-set!
   hash-table-delete!
   hash-table-intern!
   hash-table-update!
   hash-table-update!/default
   hash-table-pop!
   hash-table-clear!

   hash-table-size
   hash-table-keys
   hash-table-values
   hash-table-entries
   hash-table-find
   hash-table-count

   hash-table-map
   hash-table-for-each
   hash-table-map!
   hash-table-map->list
   hash-table-fold
   hash-table-prune!

   hash-table-copy
   hash-table-empty-copy
   hash-table->alist

   hash-table-union!
   hash-table-intersection!
   hash-table-difference!
   hash-table-xor!)
  (import (srfi srfi-125)))
