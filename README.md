# pynb-mode

pynb-mode is an Emacs minor mode for editing Python notebook files stored as plain `.nb.py` scripts.  Each cell is a regular Python snippet wrapped by comment markers, allowing notebook-style execution while keeping everything in a single text file.

## Overview

The mode turns Emacs into a lightweight notebook environment:
- Cells are delimited by lines like `## cell N input ##` and `## cell N end ##`.
- The current cell is highlighted and technical headers are hidden.
- `C-c C-c` sends the cell to a persistent Python executor and displays output inline in a gray block.
- Output is transient and not written back to the file.

## Implementation details

### Minor mode

- On activation, the mode assigns stable IDs to cell headers and uses an overlay to highlight the one around point.
- Execution keeps a map of cell IDs to running commands so that previous runs can be aborted before new ones start.
- Output is collected through an Emacs process filter and inserted below the cell, appending to the existing overlay when more text arrives.

### Executor protocol

The accompanying `executor.py` process speaks a small line‑based protocol.

**Execute cell**
```
cmd <id>: execute <n>\n
<bytes>
```
Emacs sends a `cmd` line with a unique `<id>` and the byte length `<n>` of the cell's UTF‑8 encoded code, followed immediately by those `<n>` bytes.

**Abort**
```
cmd <id>: abort\n
```
Stops a running job associated with `<id>`.

**Output**
```
cmd-output <id>: <n>\n
<payload>
```
The executor responds asynchronously with `cmd-output` messages whose payload length `<n>` is counted in bytes.  The mode routes the payload to the corresponding cell overlay, inserting a trailing newline if the chunk does not end with one.

This protocol allows multiple cells to run concurrently while preserving ordering of output chunks for each cell.
