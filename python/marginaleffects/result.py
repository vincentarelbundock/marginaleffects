from __future__ import annotations

from dataclasses import dataclass
from typing import Any, Dict, Iterable, Iterator, Optional

import polars as pl


def _default_mapping(
    data: pl.DataFrame,
    datagrid_explicit: Iterable[str],
) -> Dict[str, str]:
    datagrid_explicit = list(datagrid_explicit)
    datagrid_map = dict(zip(datagrid_explicit, datagrid_explicit))
    contrast_columns = {
        col: f"C: {col.split('_', 1)[1]}"
        for col in data.columns
        if col.startswith("contrast_")
    }
    mapping: Dict[str, str] = {
        "term": "Term",
        "group": "Group",
        **datagrid_map,
        **contrast_columns,
        "contrast": "Contrast",
        "estimate": "Estimate",
        "std_error": "Std.Error",
        "statistic": "z",
        "p_value": "P(>|z|)",
        "s_value": "S",
        "p_value_noninf": "p (NonInf)",
        "p_value_nonsup": "p (NonSup)",
        "p_value_equiv": "p (Equiv)",
        "pred_low": "Pred low",
        "pred_high": "Pred high",
    }
    return mapping


@dataclass
class _Metadata:
    by: Optional[Any]
    conf_level: float
    jacobian: Optional[Any]
    datagrid_explicit: Iterable[str]
    print_head: str
    mapping: Dict[str, str]


class MarginaleffectsResult:
    """
    Wrapper around a Polars DataFrame that preserves metadata for
    marginaleffects outputs while leaving the underlying data untouched.
    """

    def __init__(
        self,
        data: pl.DataFrame,
        *,
        by: Optional[Any] = None,
        conf_level: float = 0.95,
        jacobian: Optional[Any] = None,
        newdata: Optional[Any] = None,
        mapping: Optional[Dict[str, str]] = None,
        print_head: str = "",
    ) -> None:
        if not isinstance(data, pl.DataFrame):
            raise TypeError("MarginaleffectsResult requires a Polars DataFrame.")

        datagrid_explicit = getattr(newdata, "datagrid_explicit", []) or []
        base_mapping = _default_mapping(data, datagrid_explicit)
        if mapping is None:
            final_mapping = base_mapping
        else:
            final_mapping = mapping.copy()
            for key, value in base_mapping.items():
                final_mapping.setdefault(key, value)

        self._data = data
        self._meta = _Metadata(
            by=by,
            conf_level=conf_level,
            jacobian=jacobian,
            datagrid_explicit=list(datagrid_explicit),
            print_head=print_head,
            mapping=final_mapping,
        )

    # --------------------------------------------------------------------- #
    # Public attributes
    # --------------------------------------------------------------------- #
    @property
    def data(self) -> pl.DataFrame:
        """Return the underlying Polars DataFrame without modification."""
        return self._data

    @property
    def by(self) -> Optional[Any]:
        return self._meta.by

    @property
    def conf_level(self) -> float:
        return self._meta.conf_level

    @property
    def jacobian(self) -> Optional[Any]:
        return self._meta.jacobian

    @property
    def datagrid_explicit(self) -> Iterable[str]:
        return self._meta.datagrid_explicit

    # --------------------------------------------------------------------- #
    # Formatting / summary
    # --------------------------------------------------------------------- #
    def summary(self) -> str:
        mapping = self._meta.mapping.copy()
        mapping.setdefault("conf_low", "conf_low")
        mapping.setdefault("conf_high", "conf_high")

        if self.conf_level is not None:
            conf_low = f"{(1 - self.conf_level) / 2 * 100:.1f}%"
            conf_high = f"{(1 - (1 - self.conf_level) / 2) * 100:.1f}%"
        else:
            conf_low, conf_high = "[", "]"
        mapping["conf_low"] = conf_low
        mapping["conf_high"] = conf_high

        columns = list(mapping.keys())
        if isinstance(self.by, list):
            columns = list(self.by) + columns
        elif isinstance(self.by, str):
            columns = [self.by] + columns
        elif self.by is False:
            columns = list(mapping.keys())

        columns = list(self.datagrid_explicit) + columns
        columns = [col for col in columns if col in self._data.columns]
        columns = list(dict.fromkeys(columns))  # preserve order, drop dupes

        tmp = self._data.select(columns).rename(
            {k: mapping[k] for k in columns if k in mapping}
        )

        numeric_cols = [col for col in tmp.columns if tmp.schema[col].is_numeric()]
        if numeric_cols:
            tmp = tmp.with_columns(
                [
                    pl.col(col).map_elements(
                        lambda v: f"{v:.3g}" if v is not None else v,
                        return_dtype=pl.Utf8,
                    )
                    for col in numeric_cols
                ]
            )

        term_str = None
        contrast_str = None
        if "Term" in tmp.columns and tmp["Term"].n_unique() == 1:
            term_str = tmp["Term"].unique()[0]
            tmp = tmp.drop("Term")
        if "Contrast" in tmp.columns and tmp["Contrast"].n_unique() == 1:
            contrast_str = tmp["Contrast"].unique()[0]
            tmp = tmp.drop("Contrast")

        out_lines = self._meta.print_head + tmp.__str__()
        if term_str is not None:
            out_lines += f"\nTerm: {term_str}"
        if contrast_str is not None:
            out_lines += f"\nContrast: {contrast_str}"
        return out_lines

    # --------------------------------------------------------------------- #
    # Dunder forwarding
    # --------------------------------------------------------------------- #
    def __str__(self) -> str:  # pragma: no cover - simple forwarder
        return self.summary()

    def __repr__(self) -> str:  # pragma: no cover
        return self.summary()

    def __len__(self) -> int:
        return len(self._data)

    def __iter__(self) -> Iterator[Any]:
        return iter(self._data)

    def __getitem__(self, key: Any) -> Any:
        return self._data[key]

    def head(self, *args: Any, **kwargs: Any) -> pl.DataFrame:
        return self._data.head(*args, **kwargs)

    def __getattr__(self, item: str) -> Any:
        attr = getattr(self._data, item)

        if callable(attr):

            def wrapper(*args: Any, **kwargs: Any) -> Any:
                result = attr(*args, **kwargs)
                if isinstance(result, pl.DataFrame):
                    clone = MarginaleffectsResult(
                        result,
                        by=self.by,
                        conf_level=self.conf_level,
                        jacobian=self.jacobian,
                        mapping=self._meta.mapping.copy(),
                        print_head=self._meta.print_head,
                    )
                    clone._meta.datagrid_explicit = list(self.datagrid_explicit)
                    return clone
                return result

            return wrapper

        return attr

    # Helpful explicit method for conversions
    def to_polars(self) -> pl.DataFrame:
        return self._data
