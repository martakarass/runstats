## Test environments
* local OS X install, R 3.5.2
* ubuntu 14.04 (on travis-ci), R 3.5.2
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.


## Responses to remarks I received from 1st submission

This is 2nd submission. As for the remarks I received from 1st submission: 

### 1

    Thanks, if there are references describing the methods in your package, 
    please add these in the Description field of your DESCRIPTION file in 
    the form
    authors (year) <doi:...>
    authors (year) <arXiv:...>
    authors (year, ISBN:...)
    with no space after 'doi:', 'arXiv:' and angle brackets for auto-linking.

There are NO references describing the methods in this package. 

### 2

    Please replace \dontrun{} by \donttest{} or unwap the examples if they 
    can be executed in less than 5 sec per Rd-file.
    
The only examples for which I use `\dontrun{}` are all in runstats.demo.Rd file: 

```
\examples{
\dontrun{
runstats.demo(func.name = "RunningMean")
runstats.demo(func.name = "RunningSd")
runstats.demo(func.name = "RunningVar")
runstats.demo(func.name = "RunningCov")
runstats.demo(func.name = "RunningCor")
runstats.demo(func.name = "RunningL2Norm")
}

}
```

they generate animantions and they CANNOT be executed in less then 5 sec, hence I am keeping them as they are.

### 3 

    You have examples for unexported functions which cannot run in this way.
    Please either add runstats::: to the function calls in the examples, 
    omit these examples or export these functions.

I have removed examples from unexported function in question, that is, `convJU()` function in core_func.R. I also added tag `@noRd` to this function, and removed corresponding .Rd file. 
