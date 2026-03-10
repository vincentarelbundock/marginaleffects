"""
Minimal example that mirrors the introductory Scikit-learn walkthrough in
`bonus/sklearn.qmd`. Run directly with `python bonus/sklearn_basic.py`.
"""

from marginaleffects import avg_predictions, fit_sklearn, get_dataset
from sklearn.linear_model import LinearRegression


military = get_dataset("military")
model = fit_sklearn(
    "rank ~ officer + hisp + C(branch)",
    data=military,
    engine=LinearRegression(),
)
p = avg_predictions(model, by="branch")
print(p)
