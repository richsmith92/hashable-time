0.3
---

Package is deprecated. Instances are moved to
[`time-compat`](https://hackage.haskell.org/package/time-compat)
package.

`Hashable ZonedTime` instance is removed, as there is no `Eq Hashable`.
This is in a preparation for making `Eq` a superclass of `Hashable`.

`Data.Hashable.Time` is empty module, i.e .it doesn't re-export `Hashable`
class, but only infects you with the orphan instances.

0.2.1
-----

Add instances for `Month`, `Quarter`, `QuarterOfYear` and `DayOfWeek`.


