# issueirt 0.0.0.9000

* Initialized the package.

# issueirt 0.0.1

* Added a dynamic model.
* Added vignettes.

# issueirt 0.0.2

* Changed the default model to a bingham distribution based one.

# issueirt 0.0.3

* Updated the codes to be compatible with the new default stan model.

# issueirt 0.0.4

* Added user-friendly one-shot wrapper functions:
  - `issueirt()`: Fits a static IssueIRT model with a single function call
  - `issueirt_dynamic()`: Fits a dynamic IssueIRT model with a single function call
* Added S3 methods for model objects:
  - `print()`, `summary()`, and `plot()` methods for `issueirt_fit` objects
  - `print()`, `summary()`, and `plot()` methods for `issueirt_dynamic_fit` objects
* Updated README with Quick Start section demonstrating the new simplified interface
* All existing functions and parameters remain unchanged for backward compatibility
