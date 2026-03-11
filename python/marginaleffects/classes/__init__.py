from .result import MarginaleffectsResult, MarginaleffectsDataFrame

__all__ = [
    "MarginaleffectsResult",
    "MarginaleffectsDataFrame",
    "ModelAbstract",
    "ModelVault",
]


def __getattr__(name):
    if name in ("ModelAbstract", "ModelVault"):
        from .model import ModelAbstract, ModelVault

        globals()["ModelAbstract"] = ModelAbstract
        globals()["ModelVault"] = ModelVault
        return ModelAbstract if name == "ModelAbstract" else ModelVault
    raise AttributeError(f"module 'marginaleffects.classes' has no attribute {name!r}")
