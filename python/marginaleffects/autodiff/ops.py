"""Shared comparison operation registry for autodiff lowering and execution."""

from __future__ import annotations

from collections.abc import Callable
from dataclasses import dataclass
from typing import Any


ArrayFn = Callable[[list[Any]], Any]
WMeanFn = Callable[[Any, Any | None], Any]
EstimateFn = Callable[[Any, Any, Any | None, ArrayFn, WMeanFn], Any]


@dataclass(frozen=True)
class PipelineOp:
    estimate: EstimateFn
    scalar: bool

    def output_size(self, n: int) -> int:
        return 1 if self.scalar else n


@dataclass(frozen=True)
class ComparisonOp:
    pipeline_op: str
    weighted: bool


def _difference(hi, lo, _w, _array, _wmean):
    return hi - lo


def _ratio(hi, lo, _w, _array, _wmean):
    return hi / lo


def _differenceavg(hi, lo, w, array, wmean):
    return array([wmean(hi, w) - wmean(lo, w)])


def _ratioavg(hi, lo, w, array, wmean):
    return array([wmean(hi, w) / wmean(lo, w)])


PIPELINE_OPS = {
    "difference": PipelineOp(estimate=_difference, scalar=False),
    "ratio": PipelineOp(estimate=_ratio, scalar=False),
    "differenceavg": PipelineOp(estimate=_differenceavg, scalar=True),
    "ratioavg": PipelineOp(estimate=_ratioavg, scalar=True),
}

COMPARISON_OPS = {
    "difference": ComparisonOp(pipeline_op="difference", weighted=False),
    "ratio": ComparisonOp(pipeline_op="ratio", weighted=False),
    "differenceavg": ComparisonOp(pipeline_op="differenceavg", weighted=False),
    "ratioavg": ComparisonOp(pipeline_op="ratioavg", weighted=False),
    "differenceavgwts": ComparisonOp(pipeline_op="differenceavg", weighted=True),
    "ratioavgwts": ComparisonOp(pipeline_op="ratioavg", weighted=True),
}
