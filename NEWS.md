# arphit v0.4.0 Release Notes

## New features

 * Time series graphs now automatically add a small margin of spacing to the right hand side
 if the data is close to the right hand axis. You can stop this by setting a manual x-limit
 for the right hand side. Vertically divided time series graphs always add a margin. 
 ([#292](https://github.com/angusmoore/arphit/pull/292))
 * Export a list of graphs as a GIF animation ([#293](https://github.com/angusmoore/arphit/pull/293))

## Breaking changes

## Bugfixes

## Deprecated or removed

* `color` has been replaced with `colour` everywhere. The package was inconsistent
 in its spelling, so this has all been harmonised on the Australian spelling.
 Code using `color` will continue to work for now, with a warning. But it  will be
 an error in future versions, so you should update code now ([#296](https://github.com/angusmoore/arphit/pull/293)).
