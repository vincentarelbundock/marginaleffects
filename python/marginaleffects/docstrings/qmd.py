import os
import inspect
import marginaleffects


def docstrings_to_qmd(output_dir: str):
    """
    Loops over every name in marginaleffects.__all__ and writes the
    function's docstring (if it is indeed a function) to a .qmd file
    in the specified directory.

    Parameters
    ----------
    output_dir : str
        The directory to which the .qmd files will be saved.
    """
    os.makedirs(output_dir, exist_ok=True)

    for name in getattr(marginaleffects, "__all__", []):
        # Retrieve the object by name
        obj = getattr(marginaleffects, name, None)

        # Check if the object is a function
        if obj is not None and inspect.isfunction(obj):
            docstring = inspect.getdoc(obj) or ""

            # Construct the filepath as "output_dir/name.qmd"
            filepath = os.path.join(output_dir, f"{name}.qmd")

            # Write the docstring to the file
            with open(filepath, "w", encoding="utf-8") as f:
                f.write(docstring)
                print(f"File written: {f.name}")


if __name__ == "__main__":
    import sys

    output_dir = sys.argv[1] if len(sys.argv) > 1 else "qmd_files"
    docstrings_to_qmd(output_dir)
