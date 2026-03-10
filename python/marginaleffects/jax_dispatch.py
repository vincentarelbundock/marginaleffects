"""
JAX dispatch layer for marginaleffects.

Provides functions to attempt JAX-based computation of predictions/comparisons,
returning None if JAX cannot be used (triggering fallback to finite differences).
"""

from typing import Optional, Dict, Any
import numpy as np
import polars as pl

# Mapping from comparison string to ComparisonType enum value
# Only includes non-averaging comparisons supported by the autodiff module
# Averaging comparisons (differenceavg, etc.) have complex aggregation logic
COMPARISON_TYPE_MAP = {
    "difference": 0,  # ComparisonType.DIFFERENCE
    "ratio": 1,  # ComparisonType.RATIO
    "lnratio": 2,  # ComparisonType.LNRATIO
    "lnor": 3,  # ComparisonType.LNOR
    "lift": 4,  # ComparisonType.LIFT
}


def try_jax_predictions(
    model,
    exog: np.ndarray,
    vcov: Optional[np.ndarray],
    by,
    wts,
    hypothesis,
) -> Optional[Dict[str, Any]]:
    """
    Attempt JAX-based prediction with jacobian and standard errors.

    Returns None if JAX cannot be used. Otherwise returns dict with:
    - estimate: np.ndarray of predictions
    - jacobian: np.ndarray (n_obs x n_coefs)
    - std_error: np.ndarray of standard errors

    Conditions that prevent JAX usage (returns None):
    - Global autodiff setting is disabled
    - Model doesn't have get_autodiff_config() method
    - Model's autodiff config is None (unsupported model type)
    - wts argument is not None (weights not supported)
    - hypothesis is not None (hypothesis testing not supported in fast path)
    - by is a list/complex aggregation (only False and True supported initially)
    - vcov is None (no SEs needed anyway)
    """
    from .settings import is_autodiff_enabled

    # Check global setting first
    if not is_autodiff_enabled():
        return None

    # Check basic requirements
    if vcov is None:
        return None  # No point using JAX if we don't need SEs

    if wts is not None:
        return None  # Weights not supported

    if hypothesis is not None:
        return None  # Hypothesis testing uses different path

    # Only support False or sanitized list (any complexity handled upstream)
    is_by_false = by is False
    is_by_list = isinstance(by, list)
    if not is_by_false and not is_by_list:
        return None

    # Check model compatibility
    if not hasattr(model, "get_autodiff_config"):
        return None

    config = model.get_autodiff_config()
    if config is None:
        return None

    try:
        beta = model.get_coef()
        X = np.asarray(exog)
        V = np.asarray(vcov)

        # Select appropriate autodiff module based on model type
        if config["model_type"] == "linear":
            from .autodiff import linear as ad_module

            result = ad_module.predictions.predictions(
                beta=beta,
                X=X,
                vcov=V,
            )

        elif config["model_type"] == "glm":
            from .autodiff import glm as ad_module

            result = ad_module.predictions.predictions(
                beta=beta,
                X=X,
                vcov=V,
                family_type=config["family_type"],
                link_type=config["link_type"],
            )
        else:
            return None

        return {
            "estimate": result["estimate"],
            "jacobian": result["jacobian"],
            "std_error": result["std_error"],
        }

    except Exception:
        # Any error -> fall back to finite differences
        return None


def try_jax_comparisons(
    model,
    hi_X: np.ndarray,
    lo_X: np.ndarray,
    vcov: Optional[np.ndarray],
    by,
    wts,
    hypothesis,
    comparison: str,
    cross: bool,
    nd=None,  # DataFrame with term/contrast info for by=True
) -> Optional[Dict[str, Any]]:
    """
    Attempt JAX-based comparisons with jacobian and standard errors.

    Returns None if JAX cannot be used. Otherwise returns dict with:
    - estimate: np.ndarray of comparison estimates
    - jacobian: np.ndarray
    - std_error: np.ndarray of standard errors
    - For by=True: also includes 'terms' and 'contrasts' arrays

    Conditions that prevent JAX usage (returns None):
    - Global autodiff setting is disabled
    - Model doesn't have get_autodiff_config() method
    - Model's autodiff config is None (unsupported model type)
    - wts argument is not None (weights not supported)
    - hypothesis is not None (hypothesis testing not supported in fast path)
    - by is a complex aggregation (only False and ['group'] supported)
    - vcov is None (no SEs needed anyway)
    - comparison is not a supported string type
    - cross is True (cross comparisons not supported)
    """
    from .settings import is_autodiff_enabled

    # Check global setting first
    if not is_autodiff_enabled():
        return None

    # Check basic requirements
    if vcov is None:
        return None

    if wts is not None:
        return None

    if hypothesis is not None:
        return None

    if cross:
        return None

    # Only support simple comparison strings
    if not isinstance(comparison, str):
        return None

    comparison_lower = comparison.lower()
    if comparison_lower not in COMPARISON_TYPE_MAP:
        return None

    comparison_type = COMPARISON_TYPE_MAP[comparison_lower]

    # Check by parameter
    is_by_false = by is False
    is_by_list = isinstance(by, list)
    is_by_true = is_by_list and by == ["group"]
    is_by_complex = is_by_list and by != ["group"]
    if not is_by_false and not is_by_list:
        return None

    if is_by_complex and nd is None:
        return None

    # For by=True, we need the nd DataFrame to get term groupings
    if is_by_true and nd is None:
        return None

    # Check model compatibility
    if not hasattr(model, "get_autodiff_config"):
        return None

    config = model.get_autodiff_config()
    if config is None:
        return None

    try:
        beta = model.get_coef()
        X_hi = np.asarray(hi_X)
        X_lo = np.asarray(lo_X)
        V = np.asarray(vcov)

        # Select appropriate autodiff module based on model type
        if config["model_type"] == "linear":
            from .autodiff import linear as ad_module
        elif config["model_type"] == "glm":
            from .autodiff import glm as ad_module
        else:
            return None

        if is_by_false:
            # Row-level comparisons
            if config["model_type"] == "linear":
                result = ad_module.comparisons.comparisons(
                    beta=beta,
                    X_hi=X_hi,
                    X_lo=X_lo,
                    vcov=V,
                    comparison_type=comparison_type,
                )
            else:
                result = ad_module.comparisons.comparisons(
                    beta=beta,
                    X_hi=X_hi,
                    X_lo=X_lo,
                    vcov=V,
                    comparison_type=comparison_type,
                    family_type=config["family_type"],
                    link_type=config["link_type"],
                )

            return {
                "estimate": result["estimate"],
                "jacobian": result["jacobian"],
                "std_error": result["std_error"],
            }

        elif is_by_true or is_by_complex:
            if nd is None:
                return None

            idx_series = pl.Series(range(nd.height), dtype=pl.Int64).alias("__idx__")
            nd_with_idx = nd.with_columns(idx_series)

            group_cols = ["term", "contrast"]
            if is_by_complex:
                group_cols.extend([col for col in by if col in nd.columns])
            group_cols.extend(
                [col for col in nd.columns if col.startswith("contrast_")]
            )
            seen_cols = set()
            group_cols = [
                col
                for col in group_cols
                if not (col in seen_cols or seen_cols.add(col))
            ]

            grouped = nd_with_idx.group_by(group_cols, maintain_order=True).agg(
                pl.col("__idx__").alias("__idx__")
            )

            estimates = []
            std_errors = []
            jacobians = []

            for idx_list in grouped["__idx__"].to_list():
                idx = np.asarray(idx_list, dtype=int)
                X_hi_group = X_hi[idx]
                X_lo_group = X_lo[idx]

                if config["model_type"] == "linear":
                    result = ad_module.comparisons.comparisons_byT(
                        beta=beta,
                        X_hi=X_hi_group,
                        X_lo=X_lo_group,
                        vcov=V,
                        comparison_type=comparison_type,
                    )
                else:
                    result = ad_module.comparisons.comparisons_byT(
                        beta=beta,
                        X_hi=X_hi_group,
                        X_lo=X_lo_group,
                        vcov=V,
                        comparison_type=comparison_type,
                        family_type=config["family_type"],
                        link_type=config["link_type"],
                    )

                estimates.append(float(result["estimate"]))
                std_errors.append(float(result["std_error"]))
                jacobians.append(result["jacobian"])

            metadata = grouped.drop("__idx__")

            return {
                "estimate": np.array(estimates),
                "jacobian": np.vstack(jacobians) if jacobians else np.array([]),
                "std_error": np.array(std_errors),
                "metadata": metadata,
            }

    except Exception:
        # Any error -> fall back to finite differences
        return None
