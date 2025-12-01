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
TEST_STATUS_COL_IDX = 5

# Repository configuration (Update this if the repo changes)
REPO_OWNER = "scherenhaenden"
REPO_NAME = "MagnetarYggdrasil"

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
    # Look for projects/<Project>.project.yml
    """Find the first .project.yml file in the projects directory."""
    projects_dir = directory / "projects"
    if projects_dir.exists():
        for file in projects_dir.glob("*.project.yml"):
            return file
    return None

def normalize_name(name):
    # Normalize "C (kore.io)" to something we can match with directory or yaml name
    """Normalize a name for matching with directory or YAML names."""
    return name.lower().replace("*", "").strip()

def get_row_index_for_project(lines, project_name, project_dir_name):
    """Find the row index in the markdown table for a given project.
    
    This function iterates through the provided lines of a markdown table to locate
    the row index corresponding to a specified project. It skips lines that do not
    start with a pipe character or are part of the header or separator. The
    function normalizes the project name and directory name, then checks for
    matches in the ecosystem cell of each row, returning the index of the first
    match found.
    
    Args:
        lines (list): A list of strings representing the lines of the markdown table.
        project_name (str): The name of the project to find.
        project_dir_name (str): The directory name associated with the project.
    
    Returns:
        int: The index of the row containing the project, or -1 if not found.
    """
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
        normalized_dir = normalize_name(project_dir_name)
        normalized_proj = normalize_name(project_name)

        if "kore" in normalized_dir and "kore" in normalized_cell:
            return i

        if normalized_dir in normalized_cell:
            return i

        if normalized_proj in normalized_cell:
            return i

    return -1

def update_table_status(lines, row_index, passed=None, badge_url=None):
    """Update the test status in a specified row of a table."""
    line = lines[row_index]
    columns = line.split("|")

    # Update Test Status column (index 5 + 1 because of split empty start)
    target_col = TEST_STATUS_COL_IDX + 1

    if len(columns) > target_col:
        if badge_url:
            columns[target_col] = f" {badge_url} "
        else:
            columns[target_col] = f" {CHECK_MARK if passed else CROSS_MARK} "
        lines[row_index] = "|".join(columns)

    return lines

def main():
    """Process project directories and update validation status in matrices.
    
    The function loads matrix data from specified files and iterates through
    directories in the root. For each directory, it attempts to find a YAML
    configuration file, from which it extracts project metadata and validation
    commands. Depending on the presence of a workflow or command, it either
    constructs a badge for the project or runs tests locally. The results are then
    updated in the corresponding matrices, which are saved at the end of the
    process.
    """
    matrix_lines = load_matrix(MATRIX_FILE)
    readme_lines = load_matrix(README_FILE)

    if not matrix_lines:
        print("MATRIX.md not found!")
        return

    # Iterate over directories in root
    for item in ROOT_DIR.iterdir():
        if item.is_dir() and not item.name.startswith("."):
            yaml_file = find_project_yaml(item)
            if yaml_file:
                try:
                    with open(yaml_file, "r") as f:
                        config = yaml.safe_load(f)

                    project_name = config.get("metadata", {}).get("name", item.name)
                    validation = config.get("validation", {})
                    cmd = validation.get("cmd")
                    workflow = validation.get("workflow")

                    if workflow:
                        # Construct Badge Markdown
                        # [![Status](https://github.com/OWNER/REPO/actions/workflows/WORKFLOW.yml/badge.svg)](https://github.com/OWNER/REPO/actions/workflows/WORKFLOW.yml)
                        badge_image = f"https://github.com/{REPO_OWNER}/{REPO_NAME}/actions/workflows/{workflow}/badge.svg"
                        badge_link = f"https://github.com/{REPO_OWNER}/{REPO_NAME}/actions/workflows/{workflow}"
                        badge_md = f"[![Status]({badge_image})]({badge_link})"

                        print(f"Project: {project_name} - Setting Badge: {workflow}")

                        row_idx = get_row_index_for_project(matrix_lines, project_name, item.name)
                        if row_idx != -1:
                            update_table_status(matrix_lines, row_idx, badge_url=badge_md)

                        if readme_lines:
                             row_idx_readme = get_row_index_for_project(readme_lines, project_name, item.name)
                             if row_idx_readme != -1:
                                 update_table_status(readme_lines, row_idx_readme, badge_url=badge_md)

                    elif cmd:
                        # Fallback to local execution if no workflow specified
                        passed, output = run_test(item, cmd)
                        print(f"Project: {project_name} - Passed: {passed}")

                        row_idx = get_row_index_for_project(matrix_lines, project_name, item.name)
                        if row_idx != -1:
                            update_table_status(matrix_lines, row_idx, passed=passed)

                        if readme_lines:
                             row_idx_readme = get_row_index_for_project(readme_lines, project_name, item.name)
                             if row_idx_readme != -1:
                                 update_table_status(readme_lines, row_idx_readme, passed=passed)

                    else:
                        print(f"No validation command or workflow for {item.name}")

                except Exception as e:
                    print(f"Error processing {item.name}: {e}")

    save_matrix(MATRIX_FILE, matrix_lines)
    if readme_lines:
        save_matrix(README_FILE, readme_lines)
    print("Matrix updated.")

if __name__ == "__main__":
    main()
