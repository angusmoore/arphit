# arphit v0.2.0 Release Notes

## New features

 * Aesthetics now have an optional `order` argument. This specifies a variable that will be used to order the x-labels in a categorical plot ([#164](https://github.com/angusmoore/arphit/pull/164)). E.g. `agg_aes(x=x,y=y,order=y)` would ensure that the graph would be ordered by value of `y` rather than the order in the data (as previously). 
 * SVG file format now supported for exporting graphs ([#149](https://github.com/angusmoore/arphit/pull/149))
 * Graphs can now be exported as an EMF+ by using `.emf+` as the file extension ([#183](https://github.com/angusmoore/arphit/pull/183))
 * Support for `zoo` and `xts` objects ([#169](https://github.com/angusmoore/arphit/pull/169))
 * Add support for log scales ([#173](https://github.com/angusmoore/arphit/pull/173))
 * Can control pointsize in scatter plots ([#175](https://github.com/angusmoore/arphit/pull/175))

## Breaking changes

 * Setting user-defined default colours now uses R's `options` rather than `ARPHIT_USERCOLORS` ([#181](https://github.com/angusmoore/arphit/pull/181))

## Bugfixes

 * Multiline x-axis labels in categorical graphs are now correctly aligned of being too high and crossing the x axis ([#142](https://github.com/angusmoore/arphit/pull/142))
 * Singleton bar graphs are now correctly placed rather than filling the whole x axis ([#177](https://github.com/angusmoore/arphit/pull/177))
 * Time series bar graphs with missing observations no longer ignore the missing  observation, but show it as zero ([#158](https://github.com/angusmoore/arphit/pull/158))
 * Automatic y-axes no longer chop off the top and bottom of stacked bar graphs ([#178](https://github.com/angusmoore/arphit/pull/178))
 * Saving as EMF now correctly saves as EMF, rather than EMF+ ([#180](https://github.com/angusmoore/arphit/pull/180))
 * Unordered dates in data no longer cause problems ([#144](https://github.com/angusmoore/arphit/pull/181))

## Deprecated or removed
