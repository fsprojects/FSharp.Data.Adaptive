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