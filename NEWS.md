# arphit v0.5.0 Release Notes

## New features

* Add new layer type - `agg_step` - to create step line graphs ([#341](https://github.com/angusmoore/arphit/pull/341))

## Breaking changes

## Bugfixes

* `yearqtr` and `yearmon` x variables in a `data.frame` or `tibble` are correctly
treated as dates ([#356](https://github.com/angusmoore/arphit/pull/356))
* Y axis tick labels are now formatted to the same number of decimal places
([#339](https://github.com/angusmoore/arphit/pull/339))
* Exporting to XLSX respects x limits ([#309](https://github.com/angusmoore/arphit/pull/309))
* Better measurement of required margins on left and right sides ([#312](https://github.com/angusmoore/arphit/pull/312))
* Improved performance for autolabeller on column graphs ([#318](https://github.com/angusmoore/arphit/pull/318))
* Fixed autolabeller failure when have to identical column series ([#324](https://github.com/angusmoore/arphit/pull/324))
* Underscores in titles no longer throw errors for unknown width ([#337](https://github.com/angusmoore/arphit/pull/337))
* Better vertical spacing of footnotes, sources, and axis labels ([#342](https://github.com/angusmoore/arphit/pull/342))

## Deprecated or removed

* `x` and `y` arguments to `agg_abline` for drawing vertical and horizontal lines have been deprecated.
These have been replaced with `agg_vline` and `agg_hline` respectively ([#338](https://github.com/angusmoore/arphit/pull/338))
