# Predict Method Reference

This note captures the current implementations of `predict` for the core linear
model (`statsmodels/regression/linear_model.py`) and the generalized linear
model (`statsmodels/genmod/generalized_linear_model.py`) so their logic is easy
to inspect without opening multiple files. Code is copied verbatim with
contextual annotations below each block.

## `LinearModel.predict`

```python
    def predict(self, params, exog=None):
        """
        Return linear predicted values from a design matrix.

        Parameters
        ----------
        params : array_like
            Parameters of a linear model.
        exog : array_like, optional
            Design / exogenous data. Model exog is used if None.

        Returns
        -------
        array_like
            An array of fitted values.

        Notes
        -----
        If the model has not yet been fit, params is not optional.
        """
        # JP: this does not look correct for GLMAR
        # SS: it needs its own predict method

        if exog is None:
            exog = self.exog

        return np.dot(exog, params)
```

- Pulls the design matrix from the instance if `exog` is not passed; this uses
  the same column order the model was fit with (`self.exog`).
- Computes predictions purely via matrix multiplication (`X @ β`). No offsets,
  transformations, or variance adjustments are applied in linear models.
- Callers must supply `params` explicitly when the model has not been fitted,
  since the method does not look up stored parameter estimates.

## `GLM.predict`

```python
    def predict(
        self, params, exog=None, exposure=None, offset=None, which="mean", linear=None
    ):
        """
        Return predicted values for a design matrix

        Parameters
        ----------
        params : array_like
            Parameters / coefficients of a GLM.
        exog : array_like, optional
            Design / exogenous data. Is exog is None, model exog is used.
        exposure : array_like, optional
            Exposure time values, only can be used with the log link
            function.  See notes for details.
        offset : array_like, optional
            Offset values.  See notes for details.
        which : 'mean', 'linear', 'var'(optional)
            Statistic to predict. Default is 'mean'.

            - 'mean' returns the conditional expectation of endog E(y | x),
              i.e. inverse of the model's link function of linear predictor.
            - 'linear' returns the linear predictor of the mean function.
            - 'var_unscaled' variance of endog implied by the likelihood model.
              This does not include scale or var_weights.

        linear : bool
            The ``linear` keyword is deprecated and will be removed,
            use ``which`` keyword instead.
            If True, returns the linear predicted values.  If False or None,
            then the statistic specified by ``which`` will be returned.


        Returns
        -------
        An array of fitted values

        Notes
        -----
        Any `exposure` and `offset` provided here take precedence over
        the `exposure` and `offset` used in the model fit.  If `exog`
        is passed as an argument here, then any `exposure` and
        `offset` values in the fit will be ignored.

        Exposure values must be strictly positive.
        """
        if linear is not None:
            msg = 'linear keyword is deprecated, use which="linear"'
            warnings.warn(msg, FutureWarning, stacklevel=2)
            if linear is True:
                which = "linear"

        # Use fit offset if appropriate
        if offset is None and exog is None and hasattr(self, "offset"):
            offset = self.offset
        elif offset is None:
            offset = 0.0

        if exposure is not None and not isinstance(
            self.family.link, families.links.Log
        ):
            raise ValueError("exposure can only be used with the log link function")

        # Use fit exposure if appropriate
        if exposure is None and exog is None and hasattr(self, "exposure"):
            # Already logged
            exposure = self.exposure
        elif exposure is None:
            exposure = 0.0
        else:
            exposure = np.log(np.asarray(exposure))

        if exog is None:
            exog = self.exog

        linpred = np.dot(exog, params) + offset + exposure

        if which == "mean":
            return self.family.fitted(linpred)
        elif which == "linear":
            return linpred
        elif which == "var_unscaled":
            mean = self.family.fitted(linpred)
            var_ = self.family.variance(mean)
            return var_
        else:
            raise ValueError(f'The which value "{which}" is not recognized')
```

- Offsets default to those stored on the model (`self.offset`) unless the user
  provides both new `exog` and an explicit `offset`; otherwise the offset term
  is zero.
- Exposure values are only permitted with a log link; they are log-transformed
  and added to the linear predictor just like an offset.
- Linear predictor `η = Xβ + offset + log(exposure)` is mapped to the mean via
  the inverse link (`self.family.fitted`). Variance requests route through the
  family’s variance function evaluated at the mean.
- The `which` argument selects the returned statistic, and the deprecated
  `linear` flag simply forwards to `which="linear"`.
