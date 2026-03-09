# Repository Guidelines

## Project Structure & Module Organization
`src/` contains the Xojo desktop project and library code. The reusable SVG parser and renderer live in `src/SVG.xojo_code` and `src/SVG/`, while `App`, `WindowMain`, and `Build Automation` support local validation builds. `tests/` stores numbered SVG inputs and expected PNG outputs such as `test_00014.svg` and `test_00014.png`.

## Build, Test, and Development Commands
This repository is primarily developed through the Xojo IDE.

- `Open src/SVG.xojo_project` in Xojo to edit the module, run the sample app, or build targets.
- Use Xojo `Run` to validate rendering changes interactively in `WindowMain`.
- Use Xojo `Build` to execute the configured Windows, macOS, or Linux build steps in `src/Build Automation.xojo_code`.

There is no checked-in CLI build script, package manager, or automated test runner.

## Coding Style & Naming Conventions
Follow existing Xojo formatting: tabs for indentation, one logical statement per line, and concise inline comments only where behavior is not obvious. Keep classes and modules in `PascalCase` (`SVGPicture`, `SVGException`). Preserve the current casing of existing methods and helpers when extending code so diffs stay small and consistent with the file you are editing. Name new test fixtures with zero-padded numeric IDs: `test_00071.svg` and `test_00071.png`.

## Testing Guidelines
Regression coverage is fixture-based. For each renderer change, add or update a paired SVG source and expected PNG under `tests/`. Verify the new case manually in the Xojo app and confirm existing fixtures still render correctly. Prefer focused fixtures that isolate one SVG feature, for example `<tspan>` text layout or transform handling.

## Commit & Pull Request Guidelines
Recent history uses short, imperative commit subjects such as `Implemented SVGPicture.ToPicture()` and `Added new test svg for <tspan> node.` Keep commits narrowly scoped and describe the behavior change, not the process. Pull requests should include a brief summary, note which fixtures were added or updated, and attach screenshots when a rendering change is hard to judge from the PNG diff alone.

## Security & Configuration Tips
Do not commit private signing credentials or machine-specific Xojo IDE settings. Keep sample assets in `tests/` small enough to review and prefer sanitized SVG fixtures over third-party files when reproducing bugs.
