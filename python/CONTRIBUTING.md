# Contributing to `marginaleffects` for Python

Thank you for your interest in contributing to the `marginaleffects` Python project!

This document outlines guidelines and instructions for setting up the development environment, running tests, and ensuring your code follows the project standards.

## Development Environment

We manage the development environment using [`uv`](https://docs.astral.sh/uv/guides/projects/#creating-a-new-project) for dependency management, task execution, and testing. All necessary information is in the `.toml` and `.lock` files, and `uv` will handle dependency management automatically.

### Setting Up the Environment

Clone the repository

```bash
git clone git@github.com:vincentarelbundock/pymarginaleffects.git
cd pymarginaleffects
```

Create and activate the virtual environment using `uv`:

```bash
uv venv .venv
source .venv/bin/activate.sh
```

Install the project and its dependencies:

```bash
uv pip install .
```

### Running Tests

We use `pytest` for testing. The testing environment is fully managed by `uv`. To run the tests, simply execute:

```bash
uv run --all-extras pytest
```

`uv` will automatically install any missing dependencies before running the tests.

### Code Formatting and Linting

This project follows strict code formatting and linting rules enforced by `ruff`. Before submitting any code, make sure to run `ruff` to format your files and check for linting errors.

You can run `ruff` by executing:

```bash
uv run --all-extras ruff check marginaleffects
uv run --all-extras ruff format marginaleffects
```

Make sure there are no linting errors before submitting your changes.

## Contribution Guidelines

0. Be nice!
1. Fork the repository and create a new branch for your feature or bugfix.
2. Ensure all your changes are well-documented in the code, documentation, and changelog (if necessary).
3. Write tests for any new functionality or changes you make.
4. Run the tests and ensure everything passes before submitting your pull request.
5. Ensure your code is formatted and linted using `ruff`.
6. Push your changes to your forked repository.
7. Create a pull request (PR) on the main repository.
8. Make sure to include a clear description of your changes and reference any related issues (if applicable).

We appreciate your contributions and look forward to reviewing your pull requests!

## Makefile

The `marginaleffects` repository includes a `Makefile` to facilitate some common tasks.

### Windows

1. Install make for Windows https://gnuwin32.sourceforge.net/packages/make.htm.
2. Add it to your path variable.
3. Then you can use scripts specified in the Makefile. e.g. `make test`

