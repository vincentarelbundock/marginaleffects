"""Standard-error dispatch shared by ``predictions()`` and ``comparisons()``."""

import polars as pl

from .analytic import analytic_try
from .delta import get_jacobian, get_se
from .unconditional import is_unconditional, unconditional_result


def attach_standard_errors(out, *, plan, model, V, kind, newdata, eps_vcov, replay):
    """Add a ``std_error`` column to ``out`` and return ``(out, J, method)``.

    Unconditional requests are resolved against the plan. Otherwise the exact
    Jacobian is tried first, falling back to forward finite differences of
    ``replay(beta)``. An explicit ``eps_vcov`` requests finite differences with
    that step size, so the exact-derivative path is skipped when it is given.
    ``V is None`` means no uncertainty: ``out`` is returned unchanged.
    """
    if is_unconditional(V):
        out, J, method, _V = unconditional_result(
            plan=plan,
            model=model,
            kind=kind,
            request=V,
            newdata=newdata,
            out=out,
            eps_vcov=eps_vcov,
        )
        return out, J, method
    if V is None:
        return out, None, None

    ad = None
    if eps_vcov is None:
        ad = analytic_try(
            plan=plan,
            model=model,
            V=V,
            estimate=out["estimate"].to_numpy(),
            kind=kind,
        )
    if ad is not None:
        J, method, se = ad.jacobian, ad.method, ad.std_error
    else:
        J = get_jacobian(replay, model.get_coef(), eps_vcov)
        method, se = "finite_difference", get_se(J, V)
    return out.with_columns(pl.Series(se).alias("std_error")), J, method
