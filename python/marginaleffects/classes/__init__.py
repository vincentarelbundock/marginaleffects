from .result import MarginaleffectsResult, MarginaleffectsDataFrame

__all__ = [
    "MarginaleffectsResult",
    "MarginaleffectsDataFrame",
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
