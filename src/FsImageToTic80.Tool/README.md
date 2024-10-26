# FsImageToTic80

A tool for converting images into formats compatible with [TIC-80](https://tic80.com/), the fantasy computer. This tool converts images into LUA code or text that can be pasted directly into the TIC-80 sprite editor. It also supports exporting PNG images with the TIC-80 palette.

## Dependencies

1. **SixLabors.ImageSharp** – A powerful image processing library by SixLabors. ImageSharp is split-licensed under the Apache License 2.0 and a commercial license. For this open-source project, it qualifies for usage under the Apache 2.0 license, as it meets the criteria for open-source software use.
2. **System.CommandLine** – A command-line parser library from Microsoft, licensed under the MIT License.

## Examples

```bash
# Convert an image to LUA code with color 5 as transparency.
# Output is saved as tic80-theimage.lua.
fsimg2tic80 -i theimage.png -tk 5 -ot lua

# Open the generated LUA code in Visual Studio Code.
code tic80-theimage.lua
```
