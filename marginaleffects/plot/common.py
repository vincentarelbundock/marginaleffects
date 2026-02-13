import numpy as np
from ..datagrid import datagrid  # noqa
from ..sanitize_model import sanitize_model
import polars as pl


def dt_on_condition(model, condition):
    model = sanitize_model(model)

    condition_new = condition  # two pointers to the same object? this looks like a bug

    # not sure why newdata gets added
    modeldata = model.get_modeldata()

    if isinstance(condition_new, str):
        condition_new = [condition_new]

    to_datagrid = {}
    first_key = ""  # special case when the first element is numeric

    if isinstance(condition_new, list):
        assert all(ele in modeldata.columns for ele in condition_new), (
            "All elements of condition must be columns of the model."
        )
        first_key = condition_new[0]
        to_datagrid = {key: None for key in condition_new}

    elif isinstance(condition_new, dict):
        assert all(key in modeldata.columns for key in condition_new.keys()), (
            "All keys of condition must be columns of the model."
        )
        first_key = next(iter(condition_new))
        to_datagrid = (
            condition_new  # third pointer to the same object? looks like a BUG
        )

    # not sure why `newdata` sometimes gets added
    if isinstance(condition_new, dict) and "newdata" in to_datagrid.keys():
        condition_new.pop("newdata", None)

    assert 1 <= len(condition_new) <= 4, (
        f"Lenght of condition must be inclusively between 1 and 4. Got : {len(condition_new)}."
    )

    for key, value in to_datagrid.items():
        variable_type = model.get_variable_type(key)

        if variable_type in ["numeric", "integer"]:
            to_datagrid[key] = condition_numeric(
                modeldata, key, value, key == first_key
            )

        elif variable_type in ["character"]:
            # get specified names of the condition
            # here is the BUG, we take the values of "species" back from the model
            to_datagrid[key] = (
                to_datagrid[key]
                if to_datagrid[key]
                else modeldata[key].unique().sort().to_list()
            )
            assert len(to_datagrid[key]) <= 10, (
                f"Character type variables of more than 10 unique values are not supported. {key} variable has {len(to_datagrid[key])} unique values."
            )

        elif variable_type in ["boolean", "binary"]:
            # get specified names of the condition
            # here is the BUG, we take the values of "species" back from the model
            if to_datagrid[key] is None:
                to_datagrid[key] = modeldata[key].unique().sort().to_list()

    to_datagrid["newdata"] = modeldata
    dt = datagrid(**to_datagrid)
    return dt  # noqa: F821


def condition_numeric(modeldata, key, value, first):
    # upgrade this to use match-case when python 3.9 reaches end-of-life
    if value is None:
        if first:
            out = np.linspace(modeldata[key].min(), modeldata[key].max(), 100).tolist()
        else:
            out = np.percentile(
                modeldata[key], [0, 25, 50, 75, 100], method="midpoint"
            ).tolist()
    elif isinstance(value, str) and value == "threenum":
        m = modeldata[key].mean()
        s = modeldata[key].std()
        out = [m - s, m, m + s]
    elif isinstance(value, str) and value == "fivenum":
        out = [0, 0.25, 0.5, 0.75, 1]
        out = [modeldata[key].quantile(x) for x in out]
    elif isinstance(value, str) and value == "minmax":
        out = [0, 1]
        out = [modeldata[key].quantile(x) for x in out]
    else:
        out = value

    return out


def plot_labels(model, dt, condition):
    if not isinstance(condition, dict):
        return dt

    for k, v in condition.items():
        if model.get_variable_type(k) in ["numeric", "integer"]:
            # upgrade this to use match-case when python 3.9 reaches end-of-life
            if condition[k] == "threenum":
                lab = ["-SD", "Mean", "+SD"]
                dt = ordered_cat(dt, k, lab)
            elif condition[k] == "fivenum":
                lab = ["Min", "Q1", "Q2", "Q3", "Max"]
                dt = ordered_cat(dt, k, lab)
            elif condition[k] == "minmax":
                lab = ["Min", "Max"]
                dt = ordered_cat(dt, k, lab)
            elif condition[k] is None:
                dt = dt.with_columns(pl.col(k).round_sig_figs(3).alias(k))
    return dt


# polars does not seem to have a custom ordered categorical. only physical and lexical.
def ordered_cat(dt, k, lab):
    uniq = dict(zip(dt[k].unique().sort(), list(range(len(lab)))))
    dt = dt.with_columns(dt[k].replace_strict(uniq).alias(k))
    dt = dt.sort(by=k)
    uniq = dict(zip(list(range(len(lab))), lab))
    dt = dt.with_columns(dt[k].replace_strict(uniq).cast(pl.Categorical).alias(k))
    dt = dt.sort(by="rowid")
    return dt


def plot_common(model, dt, y_label, var_list, gray=False, points=0):
    from plotnine import (
        aes,
        facet_wrap,
        facet_grid,
        geom_pointrange,
        geom_ribbon,
        geom_line,
        geom_point,
        ggplot,
        labs,
        position_dodge,
        scale_fill_grey,
        scale_linetype_manual,
    )

    discrete = model.get_variable_type()[var_list[0]] not in ["numeric", "integer"]
    interval = "conf_low" in dt.columns
    # Keep a plain Polars view around for dtype checks when dt is a MarginaleffectsResult.
    dt_data = dt.data if hasattr(dt, "data") else dt

    # treat all variables except x-axis as categorical
    if len(var_list) > 1:
        for i in range(len(var_list) - 1, 0, -1):  # because .pop()
            # treat all variables except x-axis as categorical
            if dt[var_list[i]].dtype.is_numeric() and i != 0 and i != 1:
                dt = dt.with_columns(pl.col(var_list[i]))
            elif dt[var_list[i]].dtype != pl.Categorical:
                dt = dt.with_columns(pl.col(var_list[i]).cast(pl.Utf8))

            # unique values do not get a distinct aesthetic/geom/facet
            if dt[var_list[i]].unique().len() == 1:
                var_list.pop(i)

    # aes
    # mapping = {"x": var_list[0], "y": y_label}  # proposed change to make y axis label correspond to R  but needs some debugging
    mapping = {"x": var_list[0], "y": "estimate"}
    if interval:
        mapping["ymin"] = "conf_low"
        mapping["ymax"] = "conf_high"
    mapping = aes(**mapping)

    p = ggplot(data=dt, mapping=mapping)

    if points and points > 0:
        # We need to keep the raw-point layer in sync with every aesthetic used
        # elsewhere (color, shape, facets). That means pulling the same columns,
        # trimming categories to the grid, and dropping nulls before the plotnine layer.
        raw_data = getattr(model, "get_modeldata", lambda: None)()
        if isinstance(raw_data, pl.DataFrame):
            required_cols = [col for col in var_list if col in raw_data.columns]
            if (
                y_label in raw_data.columns
                and var_list
                and var_list[0] in required_cols
            ):
                cols = list(dict.fromkeys(required_cols + [y_label]))
                raw_subset = raw_data.select(cols)
                categorical_types = (
                    pl.Categorical,
                    pl.Enum,
                    pl.Utf8,
                    pl.String,
                    pl.Boolean,
                )
                for col in cols:
                    # Matching the categorical levels avoids extra legend entries or plotnine errors.
                    if col not in raw_subset.columns or col not in dt_data.columns:
                        continue
                    dtype = dt_data[col].dtype
                    is_numeric = getattr(dtype, "is_numeric", lambda: False)()
                    if dtype in categorical_types or not is_numeric:
                        values = (
                            dt_data[col].unique().drop_nulls().to_list()
                            if col in dt_data.columns
                            else None
                        )
                        if values:
                            raw_subset = raw_subset.filter(pl.col(col).is_in(values))
                raw_subset = raw_subset.drop_nulls(subset=[var_list[0], y_label])
                if raw_subset.height > 0:
                    point_mapping = {"x": var_list[0], "y": y_label}
                    if len(var_list) > 1 and var_list[1] in raw_subset.columns:
                        if gray:
                            point_mapping["shape"] = var_list[1]
                        else:
                            point_mapping["color"] = var_list[1]
                    point_kwargs = {"alpha": points, "inherit_aes": False}
                    if gray:
                        point_kwargs["color"] = "gray30"
                    p = p + geom_point(
                        aes(**point_mapping), data=raw_subset, **point_kwargs
                    )

    if discrete:
        if interval:
            if len(var_list) > 1:  #
                p = p + geom_pointrange(
                    aes(shape=var_list[1]) if gray else aes(color=var_list[1]),
                    position=position_dodge(width=0.1),
                )
            else:
                p = p + geom_pointrange()
        else:
            p = p + geom_point()
    else:
        if interval:
            if len(var_list) > 1:
                p = p + geom_ribbon(
                    aes(fill=var_list[1]),
                    alpha=0.2,
                )
                if gray:
                    p = p + scale_fill_grey(
                        start=0.2, end=0.8
                    )  # this could be improved by putting texture on the background
            else:
                p = p + geom_ribbon(alpha=0.2)
        if len(var_list) > 1:
            if gray:
                # get the number of unique values in the column "var_list[1]"
                unique_values = dt[var_list[1]].unique().len()
                if unique_values > 5:
                    raise ValueError(
                        f"The number of elements in the second position of the `condition` or `by` argument (variable {var_list[1]}) cannot exceed 5. It has currently {len(unique_values)} elements, with values {unique_values}."
                    )
                custom_line_types = [
                    "solid",
                    "dashed",
                    "dotted",
                    "dashdot",
                    (2, (5, 3, 1, 3, 1, 3)),
                ]  # maximum number of lines is 5, this is the default, can add more linetypes by following the documentation at https://plotnine.org/reference/scale_linetype_manual.html
                p = p + geom_line(aes(linetype=var_list[1]))
                p = p + scale_linetype_manual(values=custom_line_types)
            else:
                p = p + geom_line(aes(color=var_list[1]))
        else:
            p = p + geom_line()

    if len(var_list) == 3:
        p = p + facet_wrap(f"~ {var_list[2]}")

    elif len(var_list) == 4:
        p = p + facet_grid(f"{var_list[3]} ~ {var_list[2]}", scales="free")

    p = p + labs(y=y_label)

    return p
