# How to contribute

I’m really glad you’re reading this, because we need volunteer
developers to help this project come to fruition.

## How to get involved

Please feel free to either create an issue for a proposed edit to the
package, or to help build functionality already identified in an issue.
Since we do not want to waste your time, please do not begin to develop
new functionality until we have discussed your plans. There may be a
reason why we have not built in the functionality already.

## Testing

Before submitting a pull request, please ensure that all tests pass
using `testthat::test()` and that you have run the package checks with
`devtools::check()`. Please address all warnings and where possible, all
notes, before submitting a pull request.

## Submitting changes

Please send a GitHub Pull Request to the main branch with a clear list
of what you’ve done (read more about [pull
requests](http://help.github.com/pull-requests/)). When you send a pull
request, we will love you forever if you include RSpec examples. We can
always use more test coverage.

Always write a clear log message for your commits. One-line messages are
fine for small changes, but bigger changes should look like this:

``` R
$ git commit -m "A brief summary of the commit
> 
> A paragraph describing what changed and its impact."
```

## More information

More information about using R for health economic evaluation can be
found at the following open source academic papers:

> Smith R, Mohammed W and Schneider P. Packaging cost-effectiveness
> models in R: A tutorial. version 1; peer review: awaiting peer review.
> Wellcome Open Res 2023, 8:419
> <https://doi.org/10.12688/wellcomeopenres.19656.1>
>
> Smith RA, Schneider PP and Mohammed W. Living HTA: Automating Health
> Technology Assessment with R. Wellcome Open Res 2022, 7:194
> (<https://doi.org/10.12688/wellcomeopenres.17933.2>)
>
> Smith R and Schneider P. Making health economic models Shiny: A
> tutorial. Wellcome Open Res 2020, 5:69
> (<https://doi.org/10.12688/wellcomeopenres.15807.2>)

Please reach out to
[rsmith@darkpeakanalytics.com](https://dark-peak-analytics.github.io/assertHE/rsmith@darkpeakanalytics.com)
if you have any questions.
