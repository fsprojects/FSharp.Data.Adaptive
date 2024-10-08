### 1.2.17-prerelease0001
- added ASet.ofListTree and ASet.ofSetTree
- added AList.mapToASet
- added AMap.ofASetMapped/ofASetMappedIgnoreDuplicates (optimized ASet.groupBy)
- added MapNode/SetNode addInPlace helpers
- added SetNode.head
- added special implementation for ASet.filterA
- added special implementation for ASet.union with one set constant
- improved AList.append
- changed internal tuples/options to value types
- changed Index garbage collection to run in finalizer
- preferred using struct enumerators
- avoided using active patterns to match set operations
- updated to net 8.0
- updated Aardvark.Build 2.0.2
- updated aardpack to 1.0.25
- updated Fable to 4.22.0
- fixed race condition in Index

### 1.2.16 
* avoid WPF contention-inline problems with transaction

### 1.2.15 
* updated MultiCallbackObject with proper locking https://github.com/fsprojects/FSharp.Data.Adaptive/pull/113

### 1.2.14
* added some combinators for HashSet/HashMap

### 1.2.13
* made FSharp.Data.Adaptive trimmable via `<IsTrimmable>true</IsTrimmable>`

### 1.2.12
* fixed KeyNotFoundException in async evaluation of `ASet.mapA` and `ASet.flattenA` (issue #103)

### 1.2.11
* fixed `AList.sub` problem

### 1.2.10
* added `HashSet.ChooseToMap(V)`
* added `ASet.sort(By)Descending`
* added `AMap.sortBy(Descending)`

### 1.2.10
* added `clist.UpdateTo(seq, list, array)`
* fixed AssemblyVersions

### 1.2.9
* new `AList.sub` implementation
* switched to `aardpack` based build with CI publishing
* switched to newest fsdocs

### 1.2.8
* Index-Deletion now handled via Async/Task instead of its own thread (threads not supported on blazor-wasm)

### 1.2.7
* several AList bugfixes
* added AList slicing utilities

### 1.2.6
* added `AMap.choose2(V)` and derived combinators (`intersectWith`, `intersect(V)`)

### 1.2.5
* fixed GC problem with `AVal.mapNonAdaptive`
* added adaptive FileSystem tools
* implemented efficient `IndexList.computeDeltaTo(Array|List|Seq)` (https://neil.fraser.name/writing/diff/myers.pdf)

### 1.2.4
* raised minimal FSharp.Core version to 4.7.0 (necessary due to Fable.Core update)

### 1.2.3
* fixed fable build

### 1.2.2
* added [HashMap|HashSet|IndexList] computeDeltaCustom

### 1.2.1
* ShallowEquality now descends into non-recursive DUs (e.g. option)

### 1.2.0
* changed Transaction.Current/Running to ValueOption
* small performance improvements
* more c# extensions
* fixed bind of cset.Content
* fixed ASet.force

### 1.1.0
* major upgrade with improved performance
* several new combinators `ASet.xor`, `AVal.cast`, etc.
* public enumerators for all datastructures.
* AdaptiveSynchronizationContext
* added several interfaces to clist, cset, etc.
* UpdateTo returning boolean

### 1.0.1
* exposed ThreadStatic fields

### 1.0.0
* stable release

### 0.0.26
* attempted fix for Xamarin.IOS problems

### 0.0.25
* allowing overrides for ShallowEquality via `ShallowEquality<'T>.Set(...)`

### 0.0.24
* weak callbacks are now correctly GC'ed individually
* some new IndexList combinators
* Fable compatibility

### 0.0.23
* added missing `AddWeakMarkingCallback` for C#

### 0.0.22
* re-added weak callbacks

### 0.0.21
* All callbacks are now GC roots avoiding strange behaviour when ignoring their Subscriptions.

### 0.0.20
* added `AList.toASetIndexed`

### 0.0.19
* added ChangeableLazyVal
* more C# interop (MarkOutdated, AList creators)

### 0.0.18
* loads of new combinators (custom/mapUse/etc.)
* better C# interop thanks to @luithefirst
* reintroduced `groupBy`
* several conveniene combinators (bind2, bind3, etc.)

### 0.0.17
* fixed shallowEquals for enums
* added several shallowEquals tests

### 0.0.16
* fixed transaction bug
* proper locking in WeakOutputSet

### 0.0.15
* consistent equality everywhere
* DefaultEqualityComparer.SetProvider allows to override default equality before first use

### 0.0.14
* fixed fable build
* HashSet/HashMap.ofSeq now type-tests the given seq

### 0.0.13
* building with lowest matching FSharp.Core version
* avoiding C#-project problems

### 0.0.12
* added CSharp.Data.Adaptive package template

### 0.0.11
* Transaction performance improvements

### 0.0.10
* several performance improvements in Transaction
* implemented ChangeableModelList/ChangeableModelMap here 

### 0.0.9
* several improvements for HashMap/HashSet/MapExt
* clist.AddRange
* removed ADAPTIVE_NO_TYPE_TESTS
* added ShallowEqualityComparer

### 0.0.8
* fixed ref-counting bug in history

### 0.0.7
* added CList/CMap.UpdateTo
* optimized versions for (AList|ASet|AMap).(map|choose|filter)
* added Transaction.using
* improved Fable representation of WeakOutputSet
* all collections now have mapA/chooseA/filterA
* improved AList.mapA/chooseA
* added standard reductions (forall, exists, countBy, isEmpty, sumBy, exists, etc.)
* added reduce/reduceBy/reduceByA

### 0.0.6
* fixed fable build

### 0.0.5
* AList.countBy/countByA

### 0.0.5-prerelease09
* AList.rev/tryMin/tryMax
* AList.indexed
* AList.sorts/exists/forall/etc.

### 0.0.5-prerelease08
* added AdaptiveReductions
* AList.reduce/reduceBy/reduceByA
* AList.count/isEmpty

### 0.0.5-prerelease07
* AList.mapA/chooseA/filterA

### 0.0.5-prerelease06
* Fable compat
    * ConditionalWeakTable polyfill
    * fixed Interlocked.Increment in Callbacks for Fable

### 0.0.5-prerelease05
* callback optimizations (single CallbackObject for many callbacks)

### 0.0.5-prerelease04
* AVal.bind3
* added non-generic AdaptiveValue interface
* implemented Bind<N>Return/Bind<N> for adaptive builder

### 0.0.5-prerelease03
* IndexList.tryGetPosition

### 0.0.5-prerelease02
* IndexListDelta.ofIndexList
* IndexList.tryRemove IndexList.neighbours

### 0.0.5-prerelease01
* added ConservativeEquals / UpdateTo to HashMap/IndexList/etc.

### 0.0.4
* IndexList.choose2 / toSeqIndexed / etc.

### 0.0.3
* relaxed FSharp.Core version

### 0.0.2
fixed fable package

### 0.0.1
added several missing operators like
* `AList.ofAVal`
* `AMap.fold(Group|HalfGroup)`
* `AMap.tryFind`
* `AList.try(Get|At)`
* `AMap.ofASet`

### 0.0.1-prerelease01
initial version