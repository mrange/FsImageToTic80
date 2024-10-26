# FsImageToTic80

A tool for converting images into formats compatible with [TIC-80](https://tic80.com/), the fantasy computer. This tool converts images into LUA code or text that can be pasted directly into the TIC-80 sprite editor. It also supports exporting PNG images with the TIC-80 palette.

## Dependencies

1. **SixLabors.ImageSharp** – A powerful image processing library by SixLabors. ImageSharp is split-licensed under the Apache License 2.0 and a commercial license. For this open-source project, it qualifies for usage under the Apache 2.0 license, as it meets the criteria for open-source software use.
2. **System.CommandLine** – A command-line parser library from Microsoft, licensed under the MIT License.

## Build Instructions

```bash
cd src/FsImageToTic80.Tool
dotnet build -c Release
```

## How to run

```bash
cd src/FsImageToTic80.Tool
# -i: Specifies the input image file
# -tk: Specifies the transparency key that transparent pixel will be mapped to
# -ot: Write image as LUA code
# -h: Use -h or --help to print the help
# File is written to
dotnet run -- -i ../../assets/twister.png -tk 5 -oo -ot lua
# Opens the output in Visual Studio code
code ../../assets/tic80-twister.lua
```
