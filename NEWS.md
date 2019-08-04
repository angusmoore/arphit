# arphit v0.5.0 Release Notes

## New features

* New argument to `agg_col` layers (`reorder_bars`) that allows you to manually
specify the order for bars within that layer ([#369](https://github.com/angusmoore/arphit/pull/369))
* Add new layer type - `agg_step` - to create step line graphs ([#341](https://github.com/angusmoore/arphit/pull/341))
* Add new layer types - `agg_waterfall` - to create waterfall column/bar graphs
([#364](https://github.com/angusmoore/arphit/pull/364))
* New layout type - `1h` - to create horizontal graphs (where the x axis is along
the left axis and the y axis along on the bottom). This is particularly useful for
bar graphs; but other layer types work too.
([#350](https://github.com/angusmoore/arphit/pull/350))

## Breaking changes

## Bugfixes

* Thousands now have comma separators in y labels
([#362](https://github.com/angusmoore/arphit/pull/362))
* Multipanel scatter graphs respect `dropxlabel`
([#361](https://github.com/angusmoore/arphit/pull/361))
* NAs in text (like titles, subtitles) no longer throw errors, just don't show
the relevant text ([#362](https://github.com/angusmoore/arphit/pull/362))
* Fixed error for bar graphs with weekly data due to the fact that years don't
have a whole number of weeks ([#355](https://github.com/angusmoore/arphit/pull/355))
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
