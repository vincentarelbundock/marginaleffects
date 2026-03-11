from .variables import HiLo, _clean_global


def get_categorical_combinations(
    variable, uniqs, newdata, comparison, lab, combo="reference"
):
    def clean(k):
        return _clean_global(k, newdata.shape[0])

    if not isinstance(combo, str):
        raise ValueError("The 'variables' value must be a string.")

    if len(uniqs) > 25:
        raise ValueError("There are too many unique categories to compute comparisons.")

    out = []

    if combo == "reference":
        for u in uniqs:
            if u != uniqs[0]:
                hl = HiLo(
                    variable=variable,
                    hi=clean([u]),
                    lo=clean([uniqs[0]]),
                    lab=lab.format(hi=u, lo=uniqs[0]),
                    pad=uniqs,
                    comparison=comparison,
                )
                out.append(hl)
    elif combo == "revreference":
        last_element = uniqs[-1]
        for u in uniqs:
            if u != last_element:
                hl = HiLo(
                    variable=variable,
                    hi=clean([u]),
                    lo=clean([last_element]),
                    lab=lab.format(hi=u, lo=last_element),
                    comparison=comparison,
                    pad=uniqs,
                )
                out.append(hl)
    elif combo == "sequential":
        for i in range(len(uniqs) - 1):
            hl = HiLo(
                variable=variable,
                hi=clean([uniqs[i + 1]]),
                lo=clean([uniqs[i]]),
                lab=lab.format(hi=uniqs[i + 1], lo=uniqs[i]),
                comparison=comparison,
                pad=uniqs,
            )
            out.append(hl)
    elif combo == "revsequential":
        for i in range(len(uniqs) - 1, 0, -1):
            hl = HiLo(
                variable=variable,
                hi=clean([uniqs[i - 1]]),
                lo=clean([uniqs[i]]),
                lab=lab.format(hi=uniqs[i - 1], lo=uniqs[i]),
                comparison=comparison,
                pad=uniqs,
            )
            out.append(hl)
    elif combo == "pairwise":
        for i in range(len(uniqs)):
            for j in range(i + 1, len(uniqs)):
                hl = HiLo(
                    variable=variable,
                    hi=clean([uniqs[j]]),
                    lo=clean([uniqs[i]]),
                    lab=lab.format(hi=uniqs[j], lo=uniqs[i]),
                    comparison=comparison,
                    pad=uniqs,
                )
                out.append(hl)
    elif combo == "all":
        for i in range(len(uniqs)):
            for j in range(len(uniqs)):
                if i == j:
                    continue
                hl = HiLo(
                    variable=variable,
                    hi=clean([uniqs[j]]),
                    lo=clean([uniqs[i]]),
                    lab=lab.format(hi=uniqs[j], lo=uniqs[i]),
                    comparison=comparison,
                    pad=uniqs,
                )
                out.append(hl)
    elif combo == "revpairwise":
        for i in range(len(uniqs)):
            for j in range(i + 1, len(uniqs)):
                hl = HiLo(
                    variable=variable,
                    hi=clean([uniqs[i]]),
                    lo=clean([uniqs[j]]),
                    lab=lab.format(hi=uniqs[i], lo=uniqs[j]),
                    comparison=comparison,
                    pad=uniqs,
                )
                out.append(hl)
    else:
        raise ValueError(
            "The supported comparisons are: 'reference', 'revreference', 'sequential', "
            "'revsequential', 'pairwise', 'revpairwise', and 'all'."
        )

    return out


def _get_cross_factorial_combinations(
    variables, model, newdata, comparison, eps, by, wts, modeldata
):
    """Create HiLo objects for cross comparisons with factorial grid combinations."""
    from itertools import product

    from .comparison import sanitize_comparison

    def clean(k):
        return _clean_global(k, newdata.shape[0])

    # Get unique levels for each variable
    var_levels = {}
    var_names = []
    var_combos = {}  # Store the comparison type for each variable

    for var_name, var_value in variables.items():
        if var_name not in newdata.columns:
            continue

        var_names.append(var_name)
        var_combos[var_name] = var_value

        # Get unique levels from modeldata
        uniqs = modeldata[var_name].unique().sort()
        var_levels[var_name] = uniqs.to_list()

    # Create factorial grid
    level_lists = [var_levels[vn] for vn in var_names]
    grid_combinations = list(product(*level_lists))

    comparison_obj, lab = sanitize_comparison(comparison, by, wts)

    out = []

    # For factorial grids, use pairwise logic (C(n,2) unique unordered pairs)
    # This applies to both "pairwise" and "all" to keep comparisons manageable
    for i in range(len(grid_combinations)):
        for j in range(i + 1, len(grid_combinations)):
            hi_vals = grid_combinations[j]
            lo_vals = grid_combinations[i]

            # Create label showing which variables differ
            lab_parts = [
                f"{var_names[k]}{hi_vals[k]} - {var_names[k]}{lo_vals[k]}"
                for k in range(len(var_names))
            ]
            combined_lab = " ".join(lab_parts)

            # Create HiLo with all variables set
            # Store var_names as a tuple in the variable field to signal this is a grid comparison
            hl = HiLo(
                variable=tuple(
                    var_names
                ),  # Tuple of variable names signals factorial grid
                hi=clean(list(hi_vals)),
                lo=clean(list(lo_vals)),
                lab=combined_lab,
                comparison=comparison_obj,
                pad=None,
            )
            out.append(hl)

    return out
