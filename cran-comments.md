# Version 1.0.1

## R CMD check results
There were no ERRORs, WARNINGs or NOTEs.

## Downstream dependencies
There are currently no downstream dependencies for this package.

## Submission 1 (27/10/2021)

**Problem**

```
The Description field is intended to be a (one paragraph) description of what the package does and why it may be useful.
Please add more details about the package functionality and implemented methods in your Description text.

Please do not start the description with "This package", package name, title or similar.
```

**Correction**

The description was changed from "Draws an awesome table" to "An easy-to-use tool for drawing paper-quality tables with
double-information encoded in grobs shapes and colors."

**Problem**

```
Please write TRUE and FALSE instead of T and F. (Please don't use 'T' or 'F' as vector names.), e.g.:
  man/ttheme_awesome.Rd
```

**Correction**

All the 'T' and 'F' were replaced by TRUE and FALSE.

**Problem**

```
Please add \value to .Rd files regarding exported methods and explain the functions results in the documentation. Please
write about the structure of the output (class) and also what the output means. (If a function does not return a value,
please document that too, e.g. \value{No return value, called for side effects} or similar) 
Missing Rd-tags:
     bind.Rd: \value
     ttheme_awesome.Rd: \value
```

**Correction**

Added `@return` tags with descriptions for the exported functions missing it.

**Problem**

```
\dontrun{} should only be used if the example really cannot be executed (e.g. because of missing additional software,
missing API keys, ...) by the user. That's why wrapping examples in \dontrun{} adds the comment ('# Not run:') as
a warning for the user. Does not seem necessary.

Please unwrap the examples if they are executable in < 5 sec, or create additionally small toy examples to allow
automatic testing. (You could also replace \dontrun{} with \donttest, if it takes longer than 5 sec to be executed, but
it would be preferable to have automatic checks for functions. Otherwise, you can also write some tests.)

Please ensure that your functions do not write by default or in your examples/vignettes/tests in the user's home
filespace (including the package directory and getwd()). This is not allowed by CRAN policies. In your
examples/vignettes/tests you can write to tempdir(). 
```

**Correction**

Use `dev.off()` instead of `graphics.off()` at the end of `draw_table_extra` function and write to `tempdir()`. Change
`dontrun `to `donttest` as the examples runs in about `8s` which is more than the `5s` limit of CRAN.
