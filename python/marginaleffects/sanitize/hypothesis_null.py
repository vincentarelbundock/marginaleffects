def _is_ratio_formula(hypothesis):
    lhs = None
    if isinstance(hypothesis, str) and "~" in hypothesis:
        lhs = hypothesis.split("~", 1)[0]
    elif hasattr(hypothesis, "lhs"):
        lhs = hypothesis.lhs
    if lhs is None:
        return False
    if not isinstance(lhs, str):
        lhs = str(lhs)
    return lhs.strip().lower() == "ratio"


def sanitize_hypothesis_null(hypothesis):
    if isinstance(hypothesis, (int, float)):
        hypothesis_null = hypothesis
    elif _is_ratio_formula(hypothesis):
        hypothesis_null = 1
    elif isinstance(hypothesis, (list, tuple)):
        # Align with the R behavior: only switch the null to 1 when *all* formula-style
        # hypotheses in the sequence are ratio contrasts. Mixed contrast types retain
        # the default null of 0 to avoid shifting difference-based tests.
        formula_like = [
            hyp
            for hyp in hypothesis
            if hasattr(hyp, "lhs") or (isinstance(hyp, str) and "~" in hyp)
        ]
        ratio_formulas = [hyp for hyp in hypothesis if _is_ratio_formula(hyp)]
        if ratio_formulas and len(ratio_formulas) == len(formula_like):
            hypothesis_null = 1
        else:
            hypothesis_null = 0
    else:
        hypothesis_null = 0
    return hypothesis_null
