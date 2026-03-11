import polars as pl


def get_dataset_search(search: str):
    """Internal function to search available datasets"""
    try:
        index = pl.read_csv(
            "https://vincentarelbundock.github.io/Rdatasets/datasets.csv"
        )
        index = index.filter(
            index["Package"].str.contains(search)
            | index["Item"].str.contains(search)
            | index["Title"].str.contains(search)
        )
        return index.select(["Package", "Item", "Title", "Rows", "Cols", "CSV"])
    except BaseException as e:
        raise ValueError(f"Error searching dataset: {e}")


def get_dataset(
    dataset: str = "thornton",
    package: str = None,
    docs: bool = False,
    search: str = None,
):
    """
    # `get_dataset()`


    Download and read a dataset as a Polars DataFrame from the `marginaleffects` or from the list at https://vincentarelbundock.github.io/Rdatasets/.
    Returns documentation link if `docs` is True.

    ## Parameters

    `dataset`: (str) String. Name of the dataset to download.

     - marginaleffects archive: affairs, airbnb, ces_demographics, ces_survey, immigration, lottery, military, thornton, factorial_01, interaction_01, interaction_02, interaction_03, interaction_04, polynomial_01, polynomial_02
     - Rdatasets archive: The name of a dataset listed on the Rdatasets index. See the website or the search argument.

    `package`: (str, optional) The package to download the dataset from.

    `docs`: (bool, optional) If True, return the documentation URL instead of the dataset. Default is False.

    `search`: (str, optional) The string is a regular expression. Download the dataset index from Rdatasets; search the "Package", "Item", and "Title" columns; and return the matching rows.

    ## Returns
    (Union[str, pl.DataFrame])
    * A string representing the documentation URL if `docs` is True, or
        a Polars DataFrame containing the dataset if `docs` is False.

    ## Raises
    ValueError
    * If the dataset is not among the specified choices.

    ## Examples
    ```py
    get_dataset()
    get_dataset("Titanic", package="Stat2Data")
    get_dataset(search = "(?i)titanic)
    ```
    """
    if search:
        return get_dataset_search(search)

    datasets = {
        "affairs": "https://marginaleffects.com/data/affairs",
        "airbnb": "https://marginaleffects.com/data/airbnb",
        "ces_demographics": "https://marginaleffects.com/data/ces_demographics",
        "ces_survey": "https://marginaleffects.com/data/ces_survey",
        "immigration": "https://marginaleffects.com/data/immigration",
        "lottery": "https://marginaleffects.com/data/lottery",
        "military": "https://marginaleffects.com/data/military",
        "thornton": "https://marginaleffects.com/data/thornton",
        "factorial_01": "https://marginaleffects.com/data/factorial_01",
        "interaction_01": "https://marginaleffects.com/data/interaction_01",
        "interaction_02": "https://marginaleffects.com/data/interaction_02",
        "interaction_03": "https://marginaleffects.com/data/interaction_03",
        "interaction_04": "https://marginaleffects.com/data/interaction_04",
        "polynomial_01": "https://marginaleffects.com/data/polynomial_01",
        "polynomial_02": "https://marginaleffects.com/data/polynomial_02",
    }

    # If package is None, try to guess the correct source
    if package is None:
        # First check if it's a marginaleffects dataset
        if dataset in datasets:
            package = "marginaleffects"
        else:
            # Try to find exact match in Rdatasets
            matches = get_dataset_search(f"^{dataset}$")
            if len(matches) == 1:
                package = matches["Package"][0]
                dataset = matches["Item"][0]
            elif len(matches) > 1:
                options = "\n".join(
                    [
                        f"  - {p}::{i}"
                        for p, i in zip(matches["Package"], matches["Item"])
                    ]
                )
                msg = f"Multiple matches found for dataset '{dataset}'. Please specify the package name.\nAvailable options:\n{options}"
                raise ValueError(msg)
            else:
                msg = f"Dataset '{dataset}' not found. Please:\n1. Specify the package name, or\n2. Use get_dataset(search='...') to search available datasets"
                raise ValueError(msg)

    try:
        if package == "marginaleffects":
            if dataset not in datasets:
                raise ValueError(
                    f"Dataset '{dataset}' is not available in the 'marginaleffects' package."
                )

            base_url = datasets[dataset]
            df = pl.read_parquet(f"{base_url}.parquet")
            if (
                "factorial" in dataset
                or "interaction" in dataset
                or "polynomial" in dataset
            ):
                doc_url = "https://marginaleffects.com/data/model_to_meaning_simulated_data.html"
            elif dataset.startswith("ces"):
                doc_url = "https://marginaleffects.com/data/ces.html"
            else:
                doc_url = f"{base_url}.html"
        else:
            parquet_url = f"https://vincentarelbundock.github.io/Rdatasets/parquet/{package}/{dataset}.parquet"
            doc_url = f"https://vincentarelbundock.github.io/Rdatasets/doc/{package}/{dataset}.html"
            df = pl.read_parquet(parquet_url)

        if docs:
            return doc_url

        return df

    except BaseException as e:
        raise ValueError(f"Error reading dataset: {e}")
