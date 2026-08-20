# Specification: Linear Predictions and Exact Jacobian Analytics

**Status**: Comprehensive analysis of all 52+ model classes - **REFINED after examining actual predict methods**

**Objective**: Identify which model types can benefit from exact Jacobian computation, accounting for special features in underlying packages' predict methods.

**Methodology**: Analyzed predict method signatures from source packages to identify special parameters, multiple linear predictors, and complex structures.

---

## Summary: Models Removed After Code Review

**DISQUALIFIED** (special handling too complex):
- ❌ **glmmTMB** - Multi-component model (conditional, zi, dispersion), uses `newparams`
- ❌ **crch** - Censoring mechanism, non-linear
- ❌ **survreg** - Parametric survival, distribution-dependent
- ❌ **flexsurvreg** - Flexible parametric, distribution-specific
- ❌ **lme4::merMod** - `re.form` parameter makes it complex
- ❌ **nlme::lme** - Multiple grouping levels, complex
- ❌ **betareg** - Multiple non-linear types; only link scale viable
- ❌ **crch, hxlr** - Censoring mechanism

**CONDITIONAL/VIABLE** (with restrictions):
- ✓ **fixest** - Has `fixef` parameter but manageable
- ✓ **coxph** - Only for type="lp"; others non-linear
- ✓ **biglm** - Simple, similar to lm
- ✓ **survey::svyglm** - Already analytical
- ✓ **rms::ols, lrm** - Already analytical
- ✓ **quantreg::rq** - Already analytical

---

## Updated Prioritized Candidate List

### TIER 1: HIGH PRIORITY (Straightforward + High Impact)

#### 1. **fixest** (Fixed Effects Linear Regression)
- **Predict Method**: `fixest::predict.fixest`
- **Key Parameters**: `type` ("response", "link"), `fixef` (include/exclude FE), `sample`
- **Special Features**: Fixed effects handling via `fixef` parameter
- **Design Matrix**: Standard design matrix, handles rank deficiency
- **Complications**: `fixef` parameter can include/exclude fixed effects
- **Analytical Jacobian Feasibility**: ✓ YES
- **Implementation Difficulty**: LOW-MEDIUM
- **Expected Impact**: VERY HIGH (widely used, currently slow)
- **Recommendation**: PROCEED - handle `fixef` parameter in design matrix construction

#### 2. **coxph** (Cox Proportional Hazards)
- **Predict Method**: `survival::predict.coxph`
- **Key Parameters**: `type` ("lp", "risk", "expected", "terms", "survival"), `reference`
- **Special Features**: Multiple types; only "lp" is linear
- **Design Matrix**: Standard design matrix
- **Complications**: Other types are non-linear; `reference` parameter for strata
- **Analytical Jacobian Feasibility**: ✓ YES (for type="lp")
- **Implementation Difficulty**: LOW
- **Expected Impact**: MEDIUM-HIGH (survival analysis is common)
- **Recommendation**: PROCEED - implement only for type="lp"

---

### TIER 2: MEDIUM PRIORITY (Feasible with Conditions)

#### 3. **biglm** (Big Data Linear Regression)
- **Predict Method**: `biglm::predict.biglm`
- **Key Parameters**: `type` ("response", "terms"), `se.fit`
- **Special Features**: None; simple
- **Analytical Jacobian Feasibility**: ✓ YES
- **Implementation Difficulty**: LOW (identical to lm)
- **Expected Impact**: MEDIUM (less common than lm)
- **Recommendation**: PROCEED if simple

#### 4. **nlme::gls** (Generalized Least Squares)
- **Predict Method**: `nlme::predict.gls`
- **Key Parameters**: Minimal; `newdata`, `na.action`
- **Special Features**: Handles weighted/correlated errors
- **Design Matrix**: Standard
- **Analytical Jacobian Feasibility**: ✓ YES
- **Implementation Difficulty**: LOW-MEDIUM
- **Expected Impact**: LOW-MEDIUM (less common than lme4)
- **Recommendation**: PROCEED if time allows

#### 5. **survey::svyglm** (Survey-Weighted GLM)
- **Predict Method**: `survey::predict.svyglm`
- **Key Parameters**: `type` ("link", "response", "terms"), `total`, `se.fit`, `vcov`
- **Status**: ALREADY ANALYTICAL ✓
- **No Action Needed**: Already has `get_jacobian_analytic.svyglm`

#### 6. **rms::ols, lrm** (RMS Models)
- **Status**: ALREADY ANALYTICAL ✓
- **ols**: Already has `get_jacobian_analytic.ols`
- **lrm**: Already has `get_jacobian_analytic.lrm`
- **No Action Needed**

#### 7. **quantreg::rq** (Quantile Regression)
- **Status**: ALREADY ANALYTICAL ✓
- **No Action Needed**

---

### TIER 3: LOW PRIORITY (Complications Warrant Lower Priority)

#### 8. **lme4::merMod, lmerMod** (Linear Mixed Effects)
- **Predict Method**: `lme4::predict.merMod`
- **Key Parameters**: `re.form`, `random.only`, `newparams`, `allow.new.levels`
- **Special Features**: 
  - `re.form` controls random effect inclusion (NULL=all, NA=none)
  - `random.only` means predictions from random effects ONLY
  - `newparams` for coefficient modification
- **Design Matrix**: Standard (ignoring Z)
- **Complications**: `re.form` parameter is critical and complex
- **Analytical Jacobian Feasibility**: PARTIAL (only for re.form=NA)
- **Implementation Difficulty**: MEDIUM
- **Expected Impact**: MEDIUM (very widely used, but marginal effects often use re.form=NA)
- **Recommendation**: LOWER PRIORITY - too many parameter combinations
- **Note**: marginaleffects often uses re.form=NA, so could implement just that case

#### 9. **ivreg::ivreg** (Instrumental Variables)
- **Status**: ALREADY ANALYTICAL ✓
- **No Action Needed**

#### 10. **stats::predict.lm** (Rank Deficiency)
- **Predict Method**: `stats::predict.lm`
- **Key Parameters**: `rankdeficient` (controls handling of rank-deficient fits)
- **Special Features**: Complex rank handling
- **Analytical Jacobian Feasibility**: ✓ YES (but requires careful rank handling)
- **Implementation Difficulty**: LOW-MEDIUM
- **Expected Impact**: VERY HIGH (lm is already analytical)
- **Status**: ALREADY ANALYTICAL (no action needed)

#### 11. **nlme::predict.lme** (Mixed Effects)
- **Predict Method**: `nlme::predict.lme`
- **Key Parameters**: `level`, `asList`
- **Special Features**: Multiple grouping levels
- **Analytical Jacobian Feasibility**: PARTIAL
- **Implementation Difficulty**: MEDIUM
- **Expected Impact**: MEDIUM (less used than lme4)
- **Recommendation**: LOWER PRIORITY

---

### TIER 4: NOT VIABLE (Fundamental Issues)

#### ❌ **glmmTMB** (Generalized Linear Mixed Model)
- **Predict Method**: `glmmTMB::predict.glmmTMB`
- **Key Parameters**: `re.form`, `type` = "link", "response", "conditional", "zprob", "zlink", "disp", "latent"
- **Critical Issues**:
  1. **Multiple linear predictors**: conditional, zero-inflation, dispersion components
  2. **`newparams` mechanism**: Uses parameter substitution, not `set_coef`
  3. **Complex type handling**: 7 different prediction types with different meanings
  4. **TMB environment management**: Complex optimization state handling
- **Status**: ❌ TOO COMPLEX - DO NOT IMPLEMENT
- **Recommendation**: REMOVE from candidates

#### ❌ **crch** (Censored Regression)
- **Predict Method**: `crch::predict.crch`
- **Key Parameters**: `type` (location, scale, response, density, probability, quantile, crps), `left`/`right`
- **Critical Issues**:
  1. **Censoring mechanism**: Non-linear relationship between X and observed outcome
  2. **Multiple non-linear types**: location/scale both affect predictions
  3. **Censoring bounds**: `left`/`right` parameters affect predictions non-linearly
- **Status**: ❌ FUNDAMENTALLY NON-LINEAR - DO NOT IMPLEMENT

#### ❌ **survreg** (Parametric Survival)
- **Predict Method**: `survival::predict.survreg`
- **Key Parameters**: `type` (response, link, lp, linear, terms, quantile, uquantile), `p`
- **Critical Issues**:
  1. **Distribution-dependent**: Predictions depend on distribution family (Weibull, etc.)
  2. **Multiple non-linear types**: Quantiles, survival probability, etc.
  3. **Quantile computation**: Requires distribution-specific quantile functions
- **Status**: ❌ DISTRIBUTION-DEPENDENT - DO NOT IMPLEMENT

#### ❌ **flexsurvreg** (Flexible Parametric Survival)
- **Predict Method**: `flexsurv::predict.flexsurvreg`
- **Key Parameters**: `type`, `times`, `p` (quantiles)
- **Critical Issues**:
  1. **Flexible parametric**: Custom distributions and transformations
  2. **Time-dependent predictions**: `times` parameter makes it time-dependent
  3. **Distribution-specific**: Different for each flexible distribution
- **Status**: ❌ TOO FLEXIBLE/DISTRIBUTION-SPECIFIC - DO NOT IMPLEMENT

#### ❌ **betareg** (Beta Regression)
- **Predict Method**: `betareg::predict.betareg`
- **Key Parameters**: `type` (response, link, precision, variance, parameters, distribution, density, probability, quantile), `at`
- **Critical Issues**:
  1. **Multiple non-linear types**: precision, density, probability, quantile are all non-linear
  2. **Conditional quantiles**: `at` parameter adds complexity
  3. **Only link scale is linear**: Response scale requires beta transformation
- **Status**: ❌ LINK SCALE ONLY VIABLE - LOW PRIORITY (marginal benefit)

#### ❌ **lme4::merMod** with complex re.form
- **Predict Method**: `lme4::predict.merMod`
- **Key Parameters**: `re.form` (NULL, NA, or formula), `random.only`
- **Critical Issues**:
  1. **Multiple modes**: `re.form=NULL` includes all RE, `re.form=NA` excludes all, custom formulas
  2. **random.only parameter**: Can request ONLY random effect predictions (no fixed)
  3. **newparams mechanism**: Uses parameter substitution for coefficient changes
- **Status**: ⚠️ CONDITIONAL - only viable for fixed re.form=NA case
- **Recommendation**: LOWER PRIORITY due to complexity

---

## Final Recommendation: Focus on These

### **GO AHEAD** (2-3 models)
1. ✓ **fixest** - HIGH impact, LOW complexity
2. ✓ **coxph** (type="lp" only) - MEDIUM-HIGH impact, LOW complexity  
3. ✓ **biglm** - MEDIUM impact, LOW complexity (if time)

### **CONDITIONAL** (if time allows)
4. ⚠️ **nlme::gls** - LOW-MEDIUM impact, LOW complexity
5. ⚠️ **lme4::merMod** (re.form=NA only) - MEDIUM impact, MEDIUM complexity

### **DO NOT PURSUE**
- ❌ glmmTMB - Too complex (multi-component, newparams, TMB state)
- ❌ crch - Fundamentally non-linear (censoring)
- ❌ survreg - Distribution-dependent
- ❌ flexsurvreg - Flexible/distribution-dependent
- ❌ betareg - Only link scale viable; low benefit
- ❌ merMod with general re.form - Too many parameter combinations

---

## Implementation Roadmap (REVISED)

### **Immediate** (High ROI)
1. **fixest** - 4-6 hours
   - Handle standard linear FE regression
   - Account for `fixef` parameter in design matrix
   
2. **coxph** (type="lp") - 2-4 hours
   - Linear predictor only
   - Simple design matrix approach

### **Short-term** (If time)
3. **biglm** - 1-2 hours
   - Identical to lm implementation

4. **nlme::gls** - 4-6 hours
   - Weighted/correlated error structure

### **DO NOT IMPLEMENT**
- glmmTMB, crch, survreg, flexsurvreg, betareg, complex merMod

---

## Technical Lessons Learned

**Key Finding**: Just because predictions follow X*β on some scale doesn't mean analytical Jacobians are feasible:

1. **Multi-component models** (glmmTMB): Multiple linear predictors that interact non-linearly
2. **Distribution-dependent** (survreg, flexsurvreg): Family/distribution-specific math required
3. **Mechanism non-linearity** (crch, censoring): Even if X*β is linear, the censoring/mechanism is not
4. **Parameter complexity** (merMod, betareg): Multiple modes or types that aren't easily composable
5. **Framework-specific** (glmmTMB's TMB environment): Internal optimization state management

**Conclusion**: Linear X*β ≠ Analytically feasible. Must examine:
- All prediction types supported
- All parameters and their effects
- Multiple model components
- Internal framework requirements
- Interactions between components
