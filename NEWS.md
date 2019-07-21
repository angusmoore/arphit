# arphit v0.4.2 Release Notes

## Bugfixes

* NAs in text (like titles, subtitles) no longer throw errors, just don't show
the relevant text ([#362](https://github.com/angusmoore/arphit/pull/362))
* Fixed error for bar graphs with weekly data due to the fact that years don't
have a whole number of weeks ([#355](https://github.com/angusmoore/arphit/pull/355))
* `yearqtr` and `yearmon` x variables in a `data.frame` or `tibble` are correctly
treated as dates ([#356](https://github.com/angusmoore/arphit/pull/356))
* Y axis tick labels are now formatted to the same number of decimal places
([#339](https://github.com/angusmoore/arphit/pull/339))
* Exporting to XLSX respects x limits ([#309](https://github.com/angusmoore/arphit/pull/309))
