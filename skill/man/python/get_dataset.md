# `get_dataset()` {.unnumbered}


Download and read a dataset as a Polars DataFrame from the `marginaleffects` or from the list at https://vincentarelbundock.github.io/Rdatasets/.
Returns documentation link if `docs` is True.

## Parameters {.unnumbered}

`dataset`: (str) String. Name of the dataset to download.

 - marginaleffects archive: affairs, airbnb, ces_demographics, ces_survey, immigration, lottery, military, thornton, factorial_01, interaction_01, interaction_02, interaction_03, interaction_04, polynomial_01, polynomial_02
 - Rdatasets archive: The name of a dataset listed on the Rdatasets index. See the website or the search argument.

`package`: (str, optional) The package to download the dataset from.

`docs`: (bool, optional) If True, return the documentation URL instead of the dataset. Default is False.

`search`: (str, optional) The string is a regular expression. Download the dataset index from Rdatasets; search the "Package", "Item", and "Title" columns; and return the matching rows.

## Returns {.unnumbered}
(Union[str, pl.DataFrame])
* A string representing the documentation URL if `docs` is True, or
    a Polars DataFrame containing the dataset if `docs` is False.

## Raises {.unnumbered}
ValueError
* If the dataset is not among the specified choices.

## Examples {.unnumbered}
```py
get_dataset()
get_dataset("Titanic", package="Stat2Data")
get_dataset(search = "(?i)titanic)
```
