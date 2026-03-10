import pytest

# Try to import pyfixest - skip all tests in this module if it fails
try:
    from pyfixest.estimation import feols, fepois
    from pyfixest.utils import ssc

    PYFIXEST_AVAILABLE = True
except ImportError:
    PYFIXEST_AVAILABLE = False

import polars as pl
from polars.testing import assert_series_equal
import numpy as np
from marginaleffects import *

pytestmark = pytest.mark.skipif(
    not PYFIXEST_AVAILABLE,
    reason="pyfixest not available (may require compatible numba/numpy versions)",
)

rtol = 1e-4


def create_test_data():
    np.random.seed(1024)
    data = pl.DataFrame(
        {
            "X1": np.random.normal(size=1000),
            "X2": np.random.normal(size=1000),
            "Z1": np.random.normal(size=1000),
            "e": np.random.normal(size=1000),
            "f1": np.random.choice([0, 1, 2, 3, 4, 5], size=1000, replace=True),
            "f2": np.random.choice([0, 1, 2, 3, 4, 5], size=1000, replace=True),
        }
    ).with_columns(
        (pl.col("X1") * pl.col("X2") * pl.col("Z1") + pl.col("e")).alias("Y"),
    )
    return data


# @pytest.mark.skipif(sys.version_info > (3, 11), reason="Requires Python 3.11 or lower")
def test_bare_minimum():
    data = create_test_data()

    # test 1: no fixed effects
    fit = feols("Y ~ X1 * X2 * Z1", data=data, ssc=ssc(fixef_k="none"))

    with pytest.warns(
        UserWarning, match="Standard errors are not available for predictions"
    ):
        p = predictions(fit)
    # With automatic vcov=False for pyfixest, no uncertainty columns should be present
    assert p.shape[0] == 1000  # Same number of rows
    assert "std_error" not in p.columns
    assert "p_value" not in p.columns
    assert "conf_low" not in p.columns
    assert "conf_high" not in p.columns

    comp = comparisons(fit)
    assert "std_error" in comp.columns
    assert comp["std_error"].null_count() == 0
    assert any(abs(val) > 0 for val in comp["std_error"].to_list())

    p = avg_predictions(fit, vcov=False)
    assert_series_equal(
        p["estimate"], pl.Series([0.010447]), check_names=False, rel_tol=rtol
    )
    s = avg_slopes(fit, vcov=False)
    known = pl.Series([0.010960664156094414, -0.02592049598947146, 0.08384415120847774])
    assert_series_equal(s["estimate"], known, check_names=False, rel_tol=rtol)

    c = comparisons(fit, newdata=datagrid(X1=[2, 4], model=fit), vcov=False)
    known = pl.Series(
        [
            0.025874865365717037,
            0.02587486536571701,
            0.06166037023566736,
            0.14618691715896967,
            -0.12440034801695467,
            -0.2807823993887409,
        ]
    )
    assert_series_equal(c["estimate"], known, check_names=False, rel_tol=rtol)

    # test 2: fixed effects
    fit2 = feols("Y ~ X1 * X2 * Z1 | f1", data=data)
    with pytest.warns(
        UserWarning, match="cannot take into account the uncertainty in fixed-effects"
    ):
        p2 = predictions(fit2)
    # With automatic vcov=False for pyfixest, no uncertainty columns should be present
    assert p2.shape[0] == 1000  # Same number of rows
    assert "std_error" not in p2.columns
    assert "p_value" not in p2.columns
    assert "conf_low" not in p2.columns
    assert "conf_high" not in p2.columns

    p2 = avg_predictions(fit2, vcov=False)
    assert_series_equal(
        p["estimate"], pl.Series([0.0104466614683]), check_names=False, rel_tol=rtol
    )

    s2 = avg_slopes(fit2, vcov=False)
    known = pl.Series([0.0109451775035, -0.0218987575428, 0.0811949147670])
    assert_series_equal(s2["estimate"], known, check_names=False, rel_tol=rtol)

    comp2 = comparisons(fit2)
    assert "std_error" in comp2.columns
    assert comp2["std_error"].null_count() == 0
    assert any(abs(val) > 0 for val in comp2["std_error"].to_list())

    c2 = comparisons(fit2, newdata=datagrid(X1=[2, 4], model=fit2), vcov=False)
    known = pl.Series(
        [
            0.0258895660319,
            0.0258895660319,
            0.0711836991586,
            0.1612363096034,
            -0.1305279178471,
            -0.2903009296021,
        ]
    )
    assert_series_equal(c2["estimate"], known, check_names=False, rel_tol=rtol)

    # dontrun as bug in pyfixest with ^ interaction for fixed effects and predict()
    # test 3: special syntax - interacted fixed effects
    # fit3 = feols("Y ~ X1 * X2 * Z1 | f1^f2", data=data)
    # p3 = predictions(fit3)
    # assert p3.shape == (1000, 16)
    #
    # p3 = avg_predictions(fit3)
    # assert_series_equal(
    #     p3["estimate"], pl.Series([0.01044666147]), check_names=False, rel_tol=rtol
    # )
    #
    # s3 = avg_slopes(fit3)
    # known = pl.Series([-0.002662644695, -0.012290790756, 0.090667738344])
    # assert_series_equal(s3["estimate"], known, check_names=False, rel_tol=rtol)
    #
    # c3 = comparisons(fit3, newdata=datagrid(X1=[2, 4], model=fit3))
    # known = pl.Series(
    #     [
    #         0.01222839154,
    #         0.01222839154,
    #         0.06484658716,
    #         0.13887504383,
    #         -0.11390247943,
    #         -0.26667151028,
    #     ]
    # )
    # assert_series_equal(c3["estimate"], known, check_names=False, rel_tol=rtol)
    #


def test_pyfixest_standard_errors_across_models():
    data = create_test_data()
    poisson_data = data.with_columns(pl.col("Y").abs().round())

    # Non-linear without fixed effects: SEs should be available
    fit_pois = fepois("Y ~ X1 * X2 * Z1", data=poisson_data)
    try:
        comp_pois = comparisons(fit_pois)
    except NotImplementedError:
        pytest.skip(
            "PyFixest does not yet support prediction with newdata for Poisson regression."
        )
    assert "std_error" in comp_pois.columns
    assert comp_pois["std_error"].null_count() == 0
    assert any(abs(val) > 0 for val in comp_pois["std_error"].to_list())

    # Non-linear with fixed effects: SEs disabled automatically
    fit_pois_fe = fepois("Y ~ X1 * X2 * Z1 | f1", data=poisson_data)
    with pytest.warns(
        UserWarning,
        match="uncertainty in fixed-effects parameters when computing contrasts",
    ):
        try:
            comp_pois_fe = comparisons(fit_pois_fe)
        except NotImplementedError:
            pytest.skip(
                "PyFixest does not yet support prediction with newdata for Poisson regression."
            )
    assert "std_error" not in comp_pois_fe.columns
