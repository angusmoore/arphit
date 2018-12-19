# arphit v0.3.0 Release Notes

## New features
 
 * New option to control size of text for `agg_label` ([#189](https://github.com/angusmoore/arphit/pull/189))
 * Greatly improved performance of autolabeller ([#184](https://github.com/angusmoore/arphit/pull/184))
 * Now able to reference multiple panels in constructors ([#195](https://github.com/angusmoore/arphit/pull/195))
 * Can supply only upper or lower bound in `agg_xlim` by setting the other to NA ([#207](https://github.com/angusmoore/arphit/pull/207))

## Breaking changes
 
## Bugfixes

 * Automatically guessed y-limits are now based only on visible data (e.g. they now respect x-limits) ([#213](https://github.com/angusmoore/arphit/pull/213))

## Deprecated or removed
