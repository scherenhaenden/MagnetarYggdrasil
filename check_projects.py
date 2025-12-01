import os
import yaml
import subprocess
import re
from pathlib import Path

# Configuration
ROOT_DIR = Path(".")
MATRIX_FILE = ROOT_DIR / "MATRIX.md"
README_FILE = ROOT_DIR / "README.md"
CHECK_MARK = "✅"
CROSS_MARK = "❌"
QUESTION_MARK = "❓"

# Column indices in the Markdown table (0-based)
# Ecosystem | Implemented | Canonical | Tests Written | Docker Written | Test Status | Business Equal | Notes
ECOSYSTEM_COL_IDX = 0
IMPLEMENTED_COL_IDX = 1
CANONICAL_COL_IDX = 2
TESTS_WRITTEN_COL_IDX = 3
DOCKER_WRITTEN_COL_IDX = 4
TEST_STATUS_COL_IDX = 5
BUSINESS_EQUAL_COL_IDX = 6
NOTES_COL_IDX = 7

# Repository configuration (Update this if the repo changes)
REPO_OWNER = "scherenhaenden"
REPO_NAME = "MagnetarYggdrasil"

# Explicit mapping from Directory Name to Matrix Ecosystem Name (normalized) or substring
# Used when automatic matching fails or is ambiguous
DIR_TO_MATRIX_MAP = {
    "CKore": "ckoreio",
    "CSharp": "c#.net10nativeaot",
    "Cpp": "c++drogon",
    "FSharpAot": "f#.netaot",
    "NodeNestJs": "node.jsfastify",
    "PHPSymphony": "phpsymphony",
    "RubyInRails": "rubyrails",
    "Go": "gogin",
    "Vvweb": "vvweb",
    "ClojureRing": "clojurering",
    "CrystalKemal": "crystalkemal",
    "DVibe": "dvibed",
    "Elixir": "elixirphoenix",
    "Erlang": "erlangcowboy",
    "Haskell": "haskellservant",
    "JavaGraalVM": "javagraalvm",
    "JavaSpring": "javaspringboot",
    "JuliaGenie": "juliagenie",
    "Kotlin": "kotlinktor",
    "LuaOpenResty": "luaopenresty",
    "NimJester": "nimjester",
    "OCamlDream": "ocamldream",
    "Odin": "odinmanual",
    "PythonFastApi": "pythonfastapi",
    "Rust": "rustactixaxum",
    "ScalaAkka": "scalaakka",
    "Swift": "swiftvapor",
    "Zig": "zigstd.http",
    "AdaSPARK": "adaspark",
    "Bun": "bun",
    "Carbon": "carbon",
    "Fortran": "fortran",
    "Racket": "racket",
    "Unison": "unison",
    "WebAssembly": "webassembly",
}

def load_matrix(file_path):
    """Load a matrix from a file."""
    if not file_path.exists():
        return None
    with open(file_path, "r", encoding="utf-8") as f:
        return f.readlines()

def save_matrix(file_path, lines):
    """Saves the given lines to a file at the specified path."""
    with open(file_path, "w", encoding="utf-8") as f:
        f.writelines(lines)

def run_test(project_dir, cmd):
    """Run a test command in the specified project directory."""
    print(f"Running test for {project_dir}: {cmd}")
    try:
        # Run the command inside the project directory
        result = subprocess.run(
            cmd,
            shell=True,
            cwd=project_dir,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            timeout=300  # 5 minutes timeout
        )
        if result.returncode == 0:
            return True, result.stdout.decode()
        else:
            return False, result.stderr.decode() + result.stdout.decode()
    except subprocess.TimeoutExpired:
        return False, "Timeout expired"
    except Exception as e:
        return False, str(e)

def find_project_yaml(directory):
    # Look for projects/*.project.yml
    """Find the first .project.yml file in the projects directory."""
    projects_dir = directory / "projects"
    if projects_dir.exists():
        for file in projects_dir.glob("*.project.yml"):
            return file
    return None

def normalize_name(name):
    # Normalize "C (kore.io)" to something we can match with directory or yaml name
    """Normalize a name for matching with directory or YAML names."""
    return name.lower().replace("*", "").replace(" ", "").replace("(", "").replace(")", "").replace("/", "").replace("-", "").strip()

def get_row_index_for_project(lines, project_name, project_dir_name):
    
    # Try explicit map first
    """Find the row index in the markdown table for a given project.
    
    This function iterates through the provided lines of a markdown table to locate
    the row index corresponding to a specified project name and directory name. It
    first attempts to use a mapping from directory names to normalized values, then
    checks each line for valid entries while skipping headers and separators. The
    function employs normalization to ensure accurate matching and includes a
    fallback mechanism for substring matches.
    
    Args:
        lines (list): A list of strings representing the lines of the markdown table.
        project_name (str): The name of the project to find.
        project_dir_name (str): The directory name associated with the project.
    
    Returns:
        int: The index of the row containing the project, or -1 if not found.
    """
    target_normalized = DIR_TO_MATRIX_MAP.get(project_dir_name)
    
    for i, line in enumerate(lines):
        if not line.strip().startswith("|"):
            continue

        # Skip header and separator
        if "Ecosystem" in line or ":---" in line:
            continue

        columns = [c.strip() for c in line.split("|")]
        if len(columns) < 8:
            continue

        ecosystem_cell = columns[1] # Element 0 is empty string because line starts with |
        normalized_cell = normalize_name(ecosystem_cell)

        if target_normalized:
            if target_normalized == normalized_cell:
                return i
            # Fallback if map value is substring (though for DIR_TO_MATRIX_MAP we aim for exact normalized match if possible)
            if target_normalized in normalized_cell and len(normalized_cell) < len(target_normalized) + 5: # loose heuristic
                 return i

        normalized_dir = normalize_name(project_dir_name)
        normalized_proj = normalize_name(project_name)

        # Strict Matching to avoid "Go" inside "Drogon"
        if normalized_dir == normalized_cell:
            return i

        if normalized_proj and normalized_proj == normalized_cell:
            return i

    return -1

def update_table_row(lines, row_index,
                     implemented=False,
                     canonical=False,
                     tests_written=False,
                     docker_written=False,
                     passed=None,
                     badge_url=None,
                     validation_cmd_exists=False):
    """Update the status columns in a specified row of a table."""
    line = lines[row_index]
    columns = line.split("|")
    # columns[0] is empty string

    # Helper to set column content
    def set_col(idx, val):
        if len(columns) > idx + 1:
            columns[idx + 1] = f" {val} "

    set_col(IMPLEMENTED_COL_IDX, CHECK_MARK if implemented else CROSS_MARK)
    set_col(CANONICAL_COL_IDX, CHECK_MARK if canonical else CROSS_MARK)
    set_col(TESTS_WRITTEN_COL_IDX, CHECK_MARK if tests_written else CROSS_MARK)
    set_col(DOCKER_WRITTEN_COL_IDX, CHECK_MARK if docker_written else CROSS_MARK)

    # Test Status
    if badge_url:
        set_col(TEST_STATUS_COL_IDX, badge_url)
    elif passed is not None:
        set_col(TEST_STATUS_COL_IDX, CHECK_MARK if passed else CROSS_MARK)
    else:
        # Keep existing if not running tests, or set to ?
        pass

    # Business Equal - Default to X for now as per instructions unless manual override logic added
    set_col(BUSINESS_EQUAL_COL_IDX, CROSS_MARK)

    lines[row_index] = "|".join(columns)
    return lines

def check_implementation(directory):
    """Check if the directory exists and is not empty."""
    return directory.exists() and any(directory.iterdir())

def check_canonical(directory):
    """Check if the directory contains required files and a project YAML.
    
    This function verifies the presence of essential markdown files in the
    specified directory. It also checks for the existence of a 'projects'
    subdirectory containing at least one file with a '.project.yml' extension. If
    any required file is missing or the 'projects' directory is absent or empty,
    the function returns False; otherwise, it returns True.
    """
    required_files = [
        "PLAN.md", "BITACORA.md", "RULES.md", "STATUS.md",
        "TESTING.md", "BLOCKERS.md", "BRANCHING_MODEL.md",
        "WIP_GUIDELINES.md", "CONTRIBUTING.md"
    ]
    # Check for files
    for f in required_files:
        if not (directory / f).exists():
            return False
    # Check for project yaml inside projects/
    projects_dir = directory / "projects"
    if not projects_dir.exists() or not list(projects_dir.glob("*.project.yml")):
         return False
    return True

def check_tests(directory, config):
    # Check for test directories
    """def check_tests(directory, config):
    Check for the presence of test directories or validation commands.  This
    function checks if any of the predefined test directories exist  within the
    specified directory. It also verifies if a validation command  or workflow is
    present in the provided config. If either condition is  met, the function
    returns True; otherwise, it returns False.
    
    Args:
        directory: The directory to check for test directories.
        config: The configuration dictionary that may contain validation commands."""
    test_dirs = ["tests", "test", "spec", "specs", "src/test", "test/unit"]
    for d in test_dirs:
        if (directory / d).exists():
            return True
    
    # Check if validation command is present in yaml
    if config:
        validation = config.get("validation", {})
        if validation.get("cmd") or validation.get("workflow"):
            return True

    return False

def check_docker(directory):
    """Check if a Dockerfile exists in the specified directory."""
    return (directory / "Dockerfile").exists()

def main():
    """Main function to update project matrices and README files.
    
    This function loads the matrix and README lines from specified files, iterates
    over directories in the root, and checks the implementation status, canonical
    status, and Docker presence for each project. It also attempts to read project-
    specific YAML configurations to extract metadata and validation commands. The
    function updates the matrix and README with the gathered information and saves
    the changes back to the respective files.
    """
    matrix_lines = load_matrix(MATRIX_FILE)
    readme_lines = load_matrix(README_FILE)

    if not matrix_lines:
        print("MATRIX.md not found!")
        return

    # Iterate over directories in root
    for item in ROOT_DIR.iterdir():
        if item.is_dir() and not item.name.startswith("."):

            # Check Implementation Status
            is_implemented = check_implementation(item)
            is_canonical = check_canonical(item)
            is_docker = check_docker(item)

            project_name = item.name
            config = {}
            yaml_file = find_project_yaml(item)
            if yaml_file:
                try:
                    with open(yaml_file, "r") as f:
                        config = yaml.safe_load(f)
                    project_name = config.get("metadata", {}).get("name", item.name)
                except Exception as e:
                    print(f"Error reading yaml for {item.name}: {e}")

            is_tests = check_tests(item, config)

            validation = config.get("validation", {})
            cmd = validation.get("cmd")
            workflow = validation.get("workflow")

            badge_md = None
            passed = None

            if workflow:
                badge_image = f"https://github.com/{REPO_OWNER}/{REPO_NAME}/actions/workflows/{workflow}/badge.svg"
                badge_link = f"https://github.com/{REPO_OWNER}/{REPO_NAME}/actions/workflows/{workflow}"
                badge_md = f"[![Status]({badge_image})]({badge_link})"
            elif cmd:
                pass

            # Update Matrix
            row_idx = get_row_index_for_project(matrix_lines, project_name, item.name)
            if row_idx != -1:
                update_table_row(matrix_lines, row_idx,
                                 implemented=is_implemented,
                                 canonical=is_canonical,
                                 tests_written=is_tests,
                                 docker_written=is_docker,
                                 badge_url=badge_md,
                                 passed=passed)
                print(f"Updated Matrix for {item.name}")
            else:
                print(f"Could not find row for {item.name}")

            # Update Readme
            if readme_lines:
                row_idx_readme = get_row_index_for_project(readme_lines, project_name, item.name)
                if row_idx_readme != -1:
                    update_table_row(readme_lines, row_idx_readme,
                                     implemented=is_implemented,
                                     canonical=is_canonical,
                                     tests_written=is_tests,
                                     docker_written=is_docker,
                                     badge_url=badge_md,
                                     passed=passed)
                    print(f"Updated README for {item.name}")

    save_matrix(MATRIX_FILE, matrix_lines)
    if readme_lines:
        save_matrix(README_FILE, readme_lines)
    print("Matrix updated.")

if __name__ == "__main__":
    main()
