import re

import formulaic
import narwhals as nw
import numpy as np
from narwhals.typing import IntoFrame


__all__ = ["listwise_deletion", "model_matrices"]


def parse_variables_categorical(fml: str) -> list[str]:
    return re.findall(r"C\(\s*([^,)\s]+)", fml)


# @validate_types
def parse_variables(formula: str) -> list[str]:
    """
    Extract all variables (column names) from a formula string.

    Parameters
    ----------
    formula : str
        A string representing a statistical formula (e.g., "y ~ x1 + x2").

    Returns
    -------
    list[str]
        A list of variable names extracted from the formula. Only names/identifiers
        are included, operators and special tokens are excluded. The response variable
        is returned first, followed by predictor variables in the order they appear
        in the formula.

    Examples
    --------
    >>> get_variables("y ~ x1 + x2")
    ['y', 'x1', 'x2']
    """

    if not isinstance(formula, str):
        return []

    if "~" not in formula:
        # No response variable, just return all variables
        fml = formulaic.Formula(formula)
        return list(fml.required_variables)

    # Split into LHS (response) and RHS (predictors)
    lhs, rhs = formula.split("~", 1)
    lhs = lhs.strip()

    # Get all variables from the formula
    fml = formulaic.Formula(formula)
    all_vars = fml.required_variables

    # Response is the LHS variable
    response = lhs

    # Predictors are all other variables, sorted by their position in RHS
    predictors = []
    for var in all_vars:
        if var != response:
            predictors.append(var)

    # Sort predictors by their position in the RHS string to preserve order
    predictors.sort(key=lambda v: rhs.find(v))

    return [response] + predictors


# @validate_types
def listwise_deletion(formula: str, data: "IntoFrame"):
    """
    Remove all rows with missing values in any of the variables specified in the formula.

    Parameters
    ----------
    formula : str
        A string representing a statistical formula (e.g., "y ~ x1 + x2") from which
        variable names will be extracted.
    data : IntoFrame
        The input data frame containing the variables. Can be any type that can be
        converted to a native data frame (pandas DataFrame, polars DataFrame, etc.).

    Returns
    -------
    IntoFrame
        A new data frame of the same type as the input, with rows removed where
        any of the variables in the formula contain missing values.

    Examples
    --------
    >>> import pandas as pd
    >>> df = pd.DataFrame({
    ...     'y': [1, 2, None, 4],
    ...     'x1': [1, None, 3, 4],
    ...     'x2': [1, 2, 3, 4]
    ... })
    >>> listwise_deletion("y ~ x1 + x2", df)
       y  x1  x2
    0  1   1   1
    3  4   4   4
    """
    data = nw.from_native(data)
    if callable(formula) or not formula:
        out = data.drop_nulls().to_native()
    else:
        variables = parse_variables(formula)
        variables = [x for x in variables if x in data.columns]
        out = data.drop_nulls(subset=variables).to_native()
    return out


def model_matrices(formula: str, data: "IntoFrame", formula_engine: str = "formulaic"):
    """
    Construct model matrices (design matrices) from a formula and data using different formula engines.

    Parameters
    ----------
    formula : str
        A string specifying the model formula. The format depends on the formula_engine:
        - formulaic: "y ~ x1 + x2"
        - patsy: "x1 + x2" (right-hand side only)
        - linearmodels: "y ~ x1 + x2 + EntityEffects"
    data : IntoFrame
        The input data frame containing the variables. Can be any type that can be
        converted to a pandas DataFrame.
    formula_engine : str, default="formulaic"
        The formula processing engine to use. Options are:
        - "formulaic": Uses the formulaic package
        - "patsy": Uses the patsy package
        - "linearmodels": Parses formulas with formulaic as linearmodels would

    Returns
    -------
    A tuple containing:
        - First element: Endogenous variable matrix (dependent variable)
            - numpy array for formulaic
            - None for patsy
            - pandas DataFrame for linearmodels
        - Second element: Exogenous variable matrix (independent variables)
            - numpy array for formulaic
            - numpy array for patsy
            - pandas DataFrame for linearmodels
    """
    data = nw.from_native(data)

    if formula_engine in ["formulaic", "linearmodels"]:
        context = {"np": np}
        endog, exog = formulaic.model_matrix(formula, data.to_pandas(), context=context)
        if formula_engine == "linearmodels":
            try:
                import pandas as pd
            except ImportError:
                raise ImportError("The pandas package is required to use this feature.")
            return pd.DataFrame(endog), pd.DataFrame(exog)
        else:
            return endog, exog

    elif formula_engine == "patsy":
        try:
            import patsy
        except ImportError:
            raise ImportError("The patsy package is required to use this feature.")

        if isinstance(formula, str):
            formula = formula.split("~")[1].strip()
            exog = patsy.dmatrix(formula, data.to_pandas())

        elif callable(formula):
            endog, exog = formula(data)

        return None, exog


def extract_patsy_variable_names(formula, data):
    order = {}
    for var in data.columns:
        match = re.search(rf"\b{re.escape(var)}\b", formula.split("~")[1])
        if match:
            order[var] = match.start()
    return sorted(order, key=lambda i: order[i])
