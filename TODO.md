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
  
**Shiny interactive plots**:  *DONE*

  - plot size   **DONE**
  - force plot redrawing on resizing   **DONE**
  - save plot   **DONE**
  - when exiting, also attempt to close tab (?)   **DONT FIX** (not worth it)
  - plot multiple patients  **DONE**
  
  
  