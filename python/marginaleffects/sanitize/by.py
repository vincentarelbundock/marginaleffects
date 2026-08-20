import polars as pl


def by_is_frame(by) -> bool:
    """True when `by` is a data frame of group labels rather than column names."""
    return isinstance(by, pl.DataFrame)


def by_frame_keys(by) -> list[str]:
    """Columns a data frame `by` joins on, i.e. everything but the label."""
    return [x for x in by.columns if x != "by"]


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
        # A data frame maps combinations of predictor values to a label in its
        # `by` column, so several groups can collapse into one aggregate row.
        from ..utils import ingest

        try:
            frame = ingest(by)
        except Exception as exc:
            raise ValueError(
                "The `by` argument must be True, False, a string, a list of "
                "strings, or a data frame with a `by` column of labels."
            ) from exc
        if "by" not in frame.columns:
            raise ValueError(
                "A data frame passed to `by` must have a column named `by`, "
                "holding the label of the group each row belongs to."
            )
        if len(by_frame_keys(frame)) == 0:
            raise ValueError(
                "A data frame passed to `by` must have at least one column "
                "besides `by`, to match against the estimates."
            )
        by = frame
    return by
