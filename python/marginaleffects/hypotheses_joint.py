import re
import numpy as np
import scipy.stats as stats
import polars as pl

from .sanity import sanitize_hypothesis_null
from .result import MarginaleffectsResult


def joint_hypotheses(obj, joint_index=None, joint_test="f", hypothesis=0):
    assert joint_test in ["f", "chisq"], "`joint_test` must be `f` or `chisq`"

    # theta_hat: P x 1 vector of estimated parameters
    theta_hat = obj.get_coef()

    var_names = obj.find_predictors()

    if len(theta_hat) == len(var_names) + 1:
        var_names = ["Intercept"] + var_names

    if isinstance(joint_index, bool):
        joint_index = range(len(theta_hat))
    # if joint_index is a string, use grep to find the indices
    elif isinstance(joint_index, str):
        joint_index = [
            i for i in range(len(var_names)) if re.search(joint_index, var_names[i])
        ]
    else:
        if not isinstance(joint_index, list):
            joint_index = [joint_index]
        if all(isinstance(i, str) for i in joint_index):
            joint_index = [
                i for i in range(len(var_names)) if var_names[i] in joint_index
            ]
        assert min(joint_index) >= 0 and max(joint_index) <= len(var_names), (
            "`joint_index` contain invalid indices"
        )

    V_hat = obj.get_vcov()

    # R: Q x P matrix for testing Q hypotheses on P parameters
    # build R matrix based on joint_index
    R = np.zeros((len(joint_index), len(theta_hat)))
    for i in range(len(joint_index)):
        R[i, joint_index[i]] = 1

    if not isinstance(hypothesis, list):
        if hypothesis is None:
            hypothesis = 0
        hypothesis = np.ones(R.shape[0]) * hypothesis
    hypothesis = [sanitize_hypothesis_null(h) for h in hypothesis]

    # Calculate the difference between R*theta_hat and r
    diff = R @ theta_hat - hypothesis

    # Calculate the inverse of R*(V_hat/n)*R'
    inv = np.linalg.inv(R @ V_hat @ R.T)

    # Calculate the Wald test statistic
    if joint_test == "f":
        wald_statistic = (
            diff.T @ inv @ diff / R.shape[0]
        )  # Q is the number of rows in R
    elif joint_test == "chisq":
        wald_statistic = (
            diff.T @ inv @ diff
        )  # Not normalized for chi-squared joint_test

    # Degrees of freedom
    df1 = R.shape[0]  # Q
    df2 = obj.get_df()  # n - P

    # Calculate the p-value
    if joint_test == "f":
        p_value = 1 - stats.f.cdf(wald_statistic, df1, df2)
    elif joint_test == "chisq":
        p_value = 1 - stats.chi2.cdf(wald_statistic, df1)
        df2 = None

    # Return the Wald joint_test statistic and p-value
    mapping = {}
    if joint_test == "f":
        out = pl.DataFrame({"statistic": wald_statistic})
        mapping["statistic"] = "F"
        mapping["p_value"] = "P(>|F|)"
    elif joint_test == "chisq":
        out = pl.DataFrame({"statistic": wald_statistic})
        mapping["statistic"] = "ChiSq"
        mapping["p_value"] = "P(>|ChiSq|)"
    out = out.with_columns(p_value=p_value)

    # degrees of freedom print
    if joint_test == "f":
        out = out.with_columns(df1=df1, df2=df2)
        out = out.cast({"df1": pl.Int64, "df2": pl.Int64})
        mapping["df1"] = "Df 1"
        mapping["df2"] = "Df 2"
    elif joint_test == "chisq":
        out = out.with_columns(df=df1)
        out = out.cast({"df": pl.Int64})
        mapping["df"] = "Df"

    # Create the print_head string
    print_head = "Joint hypothesis test:\n"
    for i, j in enumerate(joint_index):
        print_head += var_names[j] + f" = {hypothesis[i]}\n"
    print_head += "\n"

    out = MarginaleffectsResult(out, mapping=mapping, print_head=print_head)

    return out
