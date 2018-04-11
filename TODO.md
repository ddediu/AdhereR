# TODO for AdhereR

  - *delete columns not used* (?) <-- not actually a problem: **FIXED**
  
  - *more portable interactive plotting* -- try *shiny* (seems better than using python + R)  **DONE** (using Shiny)
  
  - *given vector of windows (i.e., not only uniformly sliding)*  <-- there does not seem to be a good usecase for it and can be already done by setting the OW; maybe discuss it somewhere? **DONT FIX**
  
  - *check running slave threads on different machine over the network*
  
  - *dates with no day specificed (i.e., only month & year)* -- probably assume 1st of the month?
  
  - *plotting bugs*: check carefully all sorts of plotting bugs (size, etc)  <-- **SEEMS FIXED**
    + title too verbose for sliding window & per episodes  **FIXED**
    + cma limits get too crowded  **FIXED**
    + cma per episode too crowded  **FIXED**
    
  - *carry.over* could be in terms in a *percentage* and not just TRUE/FALSE
  
  - *sliding window* as a time series visulaisation/analysis
  
  - *plotting per episode shiny*: add percent
  
  - *foreign* - capture messages spitted by (non-interactive) plotting as well
  
  - check if plot legend_x and legend_y work!
  
  - add parameter *dosage.change.means.new.treatment.episode* to compute_treatment_episodes()
  
  - *new plotting function* inspired from plot.CMA0() that can plot prescription, dispensation, hospitalisation and adverse events
  
**Shiny interactive plots**:  *DONE*

  - plot size   **DONE**
  - force plot redrawing on resizing   **DONE**
  - save plot   **DONE**
  - when exiting, also attempt to close tab (?)   **DONT FIX** (not worth it)
  - plot multiple patients  **DONE**
  
**Pythn 3 warpper**

  - automatically attempt to locate `RScript` depending on the platform and complain if this fails or if `RScript` is not located where the user said it should be
  