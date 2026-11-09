from .variables import HiLo, _clean_global


def get_categorical_combinations(
    variable, uniqs, newdata, comparison, lab, combo="reference"
):
    def clean(k):
        return _clean_global(k, newdata.shape[0])

    if not isinstance(combo, str):
        raise TypeError("The 'variables' value must be a string.")

    if len(uniqs) > 25:
        raise ValueError("There are too many unique categories to compute comparisons.")

    n = len(uniqs)
    if combo == "reference":
        pairs = [(u, uniqs[0]) for u in uniqs if u != uniqs[0]]
    elif combo == "revreference":
        pairs = [(u, uniqs[-1]) for u in uniqs if u != uniqs[-1]]
    elif combo == "sequential":
        pairs = [(uniqs[i + 1], uniqs[i]) for i in range(n - 1)]
    elif combo == "revsequential":
        pairs = [(uniqs[i - 1], uniqs[i]) for i in range(n - 1, 0, -1)]
    elif combo == "pairwise":
        pairs = [(uniqs[j], uniqs[i]) for i in range(n) for j in range(i + 1, n)]
    elif combo == "all":
        pairs = [(uniqs[j], uniqs[i]) for i in range(n) for j in range(n) if i != j]
    elif combo == "revpairwise":
        pairs = [(uniqs[i], uniqs[j]) for i in range(n) for j in range(i + 1, n)]
    else:
        raise ValueError(
            "The supported comparisons are: 'reference', 'revreference', 'sequential', "
            "'revsequential', 'pairwise', 'revpairwise', and 'all'."
        )

    return [
        HiLo(
            variable=variable,
            hi=clean([hi]),
            lo=clean([lo]),
            lab=lab.format(hi=hi, lo=lo),
            comparison=comparison,
            pad=uniqs,
        )
        for hi, lo in pairs
    ]


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

    comparison_obj, _lab = sanitize_comparison(comparison, by, wts)

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
