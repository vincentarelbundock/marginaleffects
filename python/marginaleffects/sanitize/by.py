def sanitize_by(by):
    if by is True:
        by = ["group"]
    elif isinstance(by, str):
        by = ["group", by] if by != "group" else ["group"]
    elif isinstance(by, list):
        if "group" not in by:
            by = ["group"] + by
    elif by is False:
        by = False
    else:
        raise ValueError(
            "The `by` argument must be True, False, a string, or a list of strings."
        )
    return by
