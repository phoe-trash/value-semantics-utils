# VALUE-SEMANTICS-UTILS Sets

A functional set data structure with automatic deduplication. Linear time complexity of most operations; if you need something more performant and/or heavy-duty, use FSet.

TODO: describe `DICT`.

All symbols are exported from package `VALUE-SEMANTICS-UTILS`.

## Sets

### **Class `SET`**

An immutable functional set data structure, [`EQV`](EQV.md)-comparable. Accepts two initargs:

* `:TEST`, a function designator for the equality predicate. Defaults to [`EQV`](EQV.md).
* `:CONTENTS`, a list that is the contents of the set. Automatically deduplicated on set creation. Defaults to an empty list.

Readers:
* **Function `SET-TEST`** - returns the test function of the set.
* **Function `SET-CONTENTS`** - returns the list which is the contents of the set.
* **Function `SET-COUNT`** - returns the element count of the set.

Sets have `CLASS-WITH-VALUE-SEMANTICS` as their metaclass; see [the class manual](CLASSES.md) for more information.

### **Function `SET`**

Lambda list: `(set &rest contents)`

Creates an [`EQV`](EQV.md) set from the provided contents.

### **Function `SET-INSERT`**

Lambda list: `(set-insert set thing)`

If `SET` includes `THING`, `SET` is returned; otherwise, a copy of `SET` containing `THING` is returned.

### **Function `SET-REMOVE`**

Lambda list: `(set-remove set thing)`

If `SET` does not include `THING`, `SET` is returned; otherwise, a copy of `SET` not containing `THING` is returned.

### **Function `SET-FIND`**

Lambda list: `(set-insert set thing)`

If `SET` does not include `THING`, `(VALUES THING T)` is returned; otherwise, `(VALUES NIL NIL)` is returned.

### **Function `SET-DIFFERENCE`**

Lambda list: `(set-difference x y)`

Returns a new set whose contents are a set difference of the two sets.

### **Function `SET-UNION`**

Lambda list: `(set-union x y)`

Returns a new set whose contents are a set union of the two sets.

### **Function `SET-INTERSECTION`**

Lambda list: `(set-intersection x y)`

Returns a new set whose contents are a set intersection of the two sets.

### **Function `SET-EXCLUSIVE-OR`**

Lambda list: `(set-exclusive-or x y)`

Returns a new set whose contents are a set exclusive-or of the two sets.
