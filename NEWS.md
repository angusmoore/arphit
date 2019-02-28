# arphit v0.3.0 Release Notes

## New features
 
 * New option to control size of text for `agg_label` ([#189](https://github.com/angusmoore/arphit/pull/189))
 * Greatly improved performance of autolabeller ([#184](https://github.com/angusmoore/arphit/pull/184))
 * Now able to reference multiple panels in constructors ([#195](https://github.com/angusmoore/arphit/pull/195))
 * Can supply only upper or lower bound in `agg_xlim` by setting the other to NA ([#207](https://github.com/angusmoore/arphit/pull/207))
 * Can now group by the `x` variable. This means it's possible to draw candlestick
 graphs ([#223](https://github.com/angusmoore/arphit/pull/223)).
 * Ordering can be done by a non-plotting variable for grouped aesthetics. Previously
 only ungrouped aesthetics could use a non-plotting variable; grouped aesthetics
 could only use the `x` variable or the `y` values of one of the groups
 ([#223](https://github.com/angusmoore/arphit/pull/223)).
 * Aesthetics now use tidy evaluation ([#251](https://github.com/angusmoore/arphit/pull/251)).
 This means aesthetics can be _expressions_, which are evaluated within the environment
 of the provided data, rather than  just variables in the data. This means you
 can do things like `order = desc(variable)`, or `y = my_variable^2`.
 * x-axis now supports decades, quarter and month spaced x-ticks for suitable length
 samples ([#259](https://github.com/angusmoore/arphit/pull/259)). These are 
 automatically selected if the data is short/long enough to warrant. Automatic
 x-limits will choose partial years if the axis is in quarter or month terms. 
 Frequencies can be manually set using `agg_xaxisfreq` ([#268](https://github.com/angusmoore/arphit/pull/268)).
 * Can now export graph as XLSX, which exports the graph data into a nicely formatted
 Excel spreadsheet ([#250](https://github.com/angusmoore/arphit/pull/250)).

## Breaking changes

 * `agg_qplot` now only supports single panel, single sided layout. As a result, 
 the arguments `series` and `x` are now just a vector and string respectively,
 instead of lists. And `paneltitle` and `panelsubtitle` are no longer accepted
 arguments ([#242](https://github.com/angusmoore/arphit/pull/242)).
 * Duplicate categorical `x` values in the same group are now plotted as a single
 location, rather than at separate locations with the same x-tick ([#223](https://github.com/angusmoore/arphit/pull/223)).
 * Numerical categorical graphs now show all x axis labels by default (i.e. `showallxlabels = TRUE`)
 ([#233](https://github.com/angusmoore/arphit/pull/223)).
 * All graphs are now automatically ordered by the value of the `x` variable. 
 Previous (pre-0.3) behaviour was inconsistent. Previously, graphs with groups
 were ordered by `x` variable,  but graphs without groups were ordered in the
 order of the underlying data ([#233](https://github.com/angusmoore/arphit/pull/223)).
 
## Bugfixes

 * Automatically guessed y-limits are now based only on visible data (e.g. they now respect x-limits) ([#213](https://github.com/angusmoore/arphit/pull/213))
 * Corrected handling of higher frequency data (e.g. weekly, hourly) and semi-annual data ([#221](https://github.com/angusmoore/arphit/pull/221))

## Deprecated or removed
