from .result import MarginaleffectsDataFrame, MarginaleffectsResult

__all__ = [
    "MarginaleffectsDataFrame",
    "MarginaleffectsResult",
    "ModelAbstract",
    "ModelAdapter",
    "ModelVault",
]


def __getattr__(name):
    if name in ("ModelAbstract", "ModelAdapter", "ModelVault"):
        from .model import ModelAbstract, ModelAdapter, ModelVault

        globals()["ModelAbstract"] = ModelAbstract
        globals()["ModelAdapter"] = ModelAdapter
        globals()["ModelVault"] = ModelVault
        return globals()[name]
    raise AttributeError(f"module 'marginaleffects.classes' has no attribute {name!r}")
