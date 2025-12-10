# NDA Validator Refactoring Plan

## Current Architecture Problems

### 1. Global State Pollution
- **Issue**: Data stored in 3+ environments (globalenv, origin_env, .wizaRdry_env)
- **Impact**: Unclear data flow, race conditions, hard to debug
- **Location**: Throughout ndaValidator.R and ndaRequest.R

### 2. Hidden State via Attributes
- **Issue**: State passed via `attr(df, "missing_required_fields")`
- **Impact**: Easy to lose state, unclear dependencies
- **Location**: transform_value_ranges() → ndaValidator()

### 3. Side Effects Everywhere
- **Issue**: Functions mutate dataframes and environments
- **Impact**: Can't reason about function behavior, hard to test
- **Location**: All transformation functions

### 4. Fragile Error Handling
- **Issue**: Functions return NULL on error, causing cascading failures
- **Impact**: One error breaks entire pipeline
- **Location**: ndaValidator() tryCatch returns NULL

### 5. Unclear Phase Dependencies
- **Issue**: Transformations depend on implicit order
- **Impact**: Changing order breaks things unexpectedly
- **Location**: PHASE 1-4 in ndaValidator()

### 6. Tight Coupling
- **Issue**: ndaRequest and ndaValidator share state through environments
- **Impact**: Changes in one break the other
- **Location**: Environment syncing code

## Proposed Architecture

### Core Principles
1. **Explicit State**: Pass state object, no global mutations
2. **Pure Functions**: Transformations return new data, don't mutate
3. **Clear Pipeline**: Each phase has explicit inputs/outputs
4. **Robust Errors**: Errors are handled gracefully, don't break pipeline
5. **Testable**: Functions can be tested in isolation

### State Object Structure
```r
validation_state <- list(
  df = data.frame(),
  elements = data.frame(),
  metadata = list(
    measure_name = character(),
    api = character(),
    structure_name = character(),
    redcap_metadata = NULL
  ),
  tracking = list(
    missing_required_fields = character(),
    renamed_fields = character(),
    columns_to_drop = character(),
    transformations = list()
  ),
  config = list(
    verbose = logical(),
    limited_dataset = logical(),
    auto_drop_unknown = logical(),
    interactive_mode = logical()
  )
)
```

### Pipeline Structure
```r
ndaValidator <- function(measure_name, api, ...) {
  # 1. Initialize state (no global envs)
  state <- initialize_validation_state(measure_name, api, ...)
  
  # 2. Load and prepare data
  state <- load_data_phase(state)
  
  # 3. Load and enhance structure
  state <- load_structure_phase(state)
  
  # 4. Phase 1: NA Value Mapping
  state <- phase_1_na_mapping(state)
  
  # 5. Phase 2: Column Standardization
  state <- phase_2_column_standardization(state)
  
  # 6. Phase 3: Value Transformation
  state <- phase_3_value_transformation(state)
  
  # 7. Phase 4: Validation and De-Identification
  state <- phase_4_validation_deidentification(state)
  
  # 8. Build results
  return(build_validation_results(state))
}
```

## Refactoring Steps

### Phase 1: Fix Immediate Issues (DONE)
- [x] Fix sex field string-to-string transformation
- [ ] Add value_range_violations detection for data definition creation
- [ ] Fix premature missing value warnings

### Phase 2: Extract State Management
- [ ] Create validation_state object structure
- [ ] Replace attribute passing with state object
- [ ] Remove global environment mutations

### Phase 3: Refactor Pipeline
- [ ] Extract each phase into separate function
- [ ] Make functions pure (return new state, don't mutate)
- [ ] Add explicit dependency documentation

### Phase 4: Improve Error Handling
- [ ] Replace NULL returns with proper error objects
- [ ] Add graceful degradation
- [ ] Improve error messages

### Phase 5: Clean Integration
- [ ] Refactor ndaRequest to use new architecture
- [ ] Remove environment syncing code
- [ ] Simplify file creation logic

## Implementation Notes

- Maintain backward compatibility during transition
- Test each phase independently
- Document all function dependencies
- Add validation at each pipeline stage
