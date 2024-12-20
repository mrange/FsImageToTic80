﻿(*
MIT License

Copyright (c) 2024 Mårten Rånge

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*)

open System
open System.Collections.Generic
open System.Globalization
open System.IO
open System.Text
open System.Threading

open FSharp.Core.Printf

(*
# Dependencies
1. **SixLabors.ImageSharp** – A powerful image processing library by SixLabors.
   ImageSharp is split-licensed under the Apache License 2.0 and a commercial
   license. For this open-source project, it qualifies for usage under the
   Apache 2.0 license, as it meets the criteria for open-source software use.
2. **System.CommandLine** – A command-line parser library from Microsoft,
   licensed under the MIT License.
*)
open System.CommandLine
open System.CommandLine.Invocation

// I use the excellent SixLabors ImageSharp for image processing
//  Note this library has a split license, basically for open source it's free,
//  in a commercial setting it costs money.
//  There are more nuances to this, check the license file.
open SixLabors.ImageSharp
open SixLabors.ImageSharp.PixelFormats
open SixLabors.ImageSharp.Processing
open SixLabors.ImageSharp.Processing.Processors.Quantization

exception AbortException of (int)

let invariant = CultureInfo.InvariantCulture

let writeToConsole
  (cc       : ConsoleColor)
  (prelude  : string      )
  (msg      : string      )
  : unit =
  let occ = Console.ForegroundColor
  Console.ForegroundColor <- cc
  try
    Console.Write prelude
    Console.Write " - "
    Console.WriteLine msg
  finally
    Console.ForegroundColor <- occ

let good    msg = writeToConsole ConsoleColor.Green    "GOOD" msg
let hili    msg = writeToConsole ConsoleColor.Cyan     "HILI" msg
let info    msg = writeToConsole ConsoleColor.Gray     "INFO" msg
let warn    msg = writeToConsole ConsoleColor.Yellow   "WARN" msg
let fail    msg = writeToConsole ConsoleColor.Red      "FAIL" msg

let goodf  fmt  = kprintf good fmt
let hilif  fmt  = kprintf hili fmt
let infof  fmt  = kprintf info fmt
let warnf  fmt  = kprintf warn fmt
let failf  fmt  = kprintf fail fmt

let abort exitCode msg =
  fail msg
  raise (AbortException exitCode)

let abortf exitCode fmt  = kprintf (abort exitCode) fmt

let isBetween x a b =
  if   x < a then false
  elif b < x then false
  else true

let inline isNotNull o = not (isNull o)

let tic80Palette  =
  [|
    let inline RGB (r,g,b) = Color.FromRgb (byte r,byte g,byte b)
    0x0, RGB(0x1C, 0x1C, 0x2C)
    0x1, RGB(0x5D, 0x27, 0x5D)
    0x2, RGB(0xB1, 0x3E, 0x53)
    0x3, RGB(0xEF, 0x7D, 0x57)
    0x4, RGB(0xFF, 0xCD, 0x75)
    0x5, RGB(0xA7, 0xF0, 0x70)
    0x6, RGB(0x38, 0xB7, 0x64)
    0x7, RGB(0x25, 0x71, 0x79)
    0x8, RGB(0x29, 0x36, 0x6F)
    0x9, RGB(0x3B, 0x5F, 0xC9)
    0xA, RGB(0x41, 0xA6, 0xF6)
    0xB, RGB(0x73, 0xEF, 0xF7)
    0xC, RGB(0xF4, 0xF4, 0xF4)
    0xD, RGB(0x94, 0xB0, 0xC2)
    0xE, RGB(0x56, 0x6C, 0x86)
    0xF, RGB(0x33, 0x3C, 0x57)
  |]
let tic80PaletteLookup =
  tic80Palette
  |> Array.map (fun kv -> snd kv, fst kv)
  |> dict

type OutputType =
  | Lua     = 0
  | Png     = 1
  | Tic80   = 2

let inputOption =
  Option<string>(
      aliases         = [|"-i"; "--input"|]
    , description     = "Input image path"
    , IsRequired      = true
    )

let outputTypeOption =
  Option<OutputType>(
      aliases         = [|"-ot"; "--output-type"|]
    , description     = "Output image type"
    , getDefaultValue = fun () -> OutputType.Tic80
    )

let transparencyKeyOption =
  Option<int Nullable>(
      aliases         = [|"-tk"; "--transparency-key"|]
    , description     = "Which color key (0-15) is considered transparent"
    )

let allowedKeysOption =
  Option<string>(
      aliases         = [|"-ak"; "--allowed-keys"|]
    , description     = "Which color keys (0-15) are allowed"
    )

let forbiddenKeysOption =
  Option<string>(
      aliases         = [|"-fk"; "--forbidden-keys"|]
    , description     = "Which color keys (0-15) are forbidden"
    )

let ditheringOption =
  Option<bool>(
      aliases         = [|"-d"; "--dithering"|]
    , description     = "Allow color dithering"
    )

let overwriteOutputOption =
  Option<bool>(
      aliases         = [|"-oo"; "--overwrite-output"|]
    , description     = "If the output file exists should we overwrite it"
    , getDefaultValue = fun () -> false
    )

type ImagePath   =
  | ImagePath of string*string

  member x.Pretty : string =
    let (ImagePath (_, fullPath)) = x
    fullPath

  member x.FullPath : string =
    let (ImagePath (_, fullPath)) = x
    fullPath

let toImagePath
  (path : string)
  : ImagePath =
  ImagePath (path, Path.GetFullPath path)

type TransparencyKey =
  | NoKey
  | TransparencyKey of int

  member x.Pretty : string =
    match x with
    | NoKey             -> "No key"
    | TransparencyKey k -> string k


let toTransparencyKey
  (k : int Nullable)
  : TransparencyKey =
  if k.HasValue then
    let k = k.Value
    if not (isBetween k 0 15) then
      abortf 80 "Transparency key is expected to be in range [0,15] but is: %d" k
    else
      TransparencyKey k
  else
    NoKey


type Keys =
  | Keys of int array

  member x.Pretty : string =
    let (Keys keys) = x
    String.Join(',', keys)

  member x.AsArray : int array =
    let (Keys keys) = x
    keys


let toKeys
  (nm : string      )
  (s  : string|null )
  (dv : string      )
  : Keys =
  let s =
    if isNull s
    then dv
    else s
  if String.IsNullOrWhiteSpace s then
    Keys [||]
  else
    let mapper i (s : string) : int =
      let numberStyle =
        NumberStyles.Integer
        ||| NumberStyles.AllowLeadingWhite
        ||| NumberStyles.AllowTrailingWhite

      match Int32.TryParse (s, numberStyle, CultureInfo.InvariantCulture) with
      | true  , v ->
        if not (isBetween v 0 15) then
          abortf 81 "Value in %s '%s' is outside range [0,15]. It broke at index %d" nm s i
        else
          v
      | false , _ -> abortf 82 "Unable to parse a value from %s '%s' into an integer. It broke at index %d" nm s i
    s.Split (',')
    |> Array.mapi mapper
    |> Array.distinct
    |> Array.sort
    |> Keys

let printFile name =
  let baseDir  = AppDomain.CurrentDomain.BaseDirectory
  let fileName = Path.GetFullPath (Path.Combine (baseDir, name))
  let text     = File.ReadAllText fileName
  Console.WriteLine text

let noticeCommandHandler
  (ctx            : InvocationContext )
  : unit =

  printFile "NOTICE"

  ctx.ExitCode <- 0

let readmeCommandHandler
  (ctx            : InvocationContext )
  : unit =

  printFile "README.md"

  ctx.ExitCode <- 0

let licenseCommandHandler
  (ctx            : InvocationContext )
  : unit =

  printFile "LICENSE"

  ctx.ExitCode <- 0

// Example: dotnet run -- -tk 5 -i ..\..\assets\twister.png -oo -ot Png
let rootCommandHandler
  (ctx            : InvocationContext )
  : unit =
  let inline getValue option = ctx.ParseResult.GetValueForOption option

  let input           = getValue inputOption
  let outputType      = getValue outputTypeOption
  let transparencyKey = getValue transparencyKeyOption
  let allowedKeys     = getValue allowedKeysOption
  let forbiddenKeys   = getValue forbiddenKeysOption
  let dithering       = getValue ditheringOption
  let overwriteOutput = getValue overwriteOutputOption

  let exitCode =
    try
      assert (isNotNull input)

      let input             = toImagePath       input
      let transparencyKey   = toTransparencyKey transparencyKey
      let allowedKeys       = toKeys            "allowed keys"   allowedKeys     "0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15"
      let forbiddenKeys     = toKeys            "forbidden keys" forbiddenKeys   ""

      hili  "fsimg2tic80 - Converts image to images usable in TIC-80"
      infof "  Input image path         : %s"   input.Pretty
      infof "  Output type              : %A"   outputType
      infof "  Transparency key         : %s"   transparencyKey.Pretty
      infof "  Allowed keys             : %s"   allowedKeys.Pretty
      infof "  Forbidden keys           : %s"   forbiddenKeys.Pretty
      infof "  Dithering                : %A"   dithering
      infof "  Overwrite output         : %A"   overwriteOutput

      if not (Path.Exists input.FullPath) then
        abort 91 "Input path does not exists"

      let output =
        let output = input.FullPath
        let outputExtension =
          match outputType with
          | OutputType.Lua    -> "lua"
          | OutputType.Png    -> "png"
          | OutputType.Tic80  -> "tic80"
          | _                 -> abortf 70 "Illegal case %A" outputType
        let outputFileName  = Path.GetFileNameWithoutExtension output
        let outputFileName  = sprintf "tic80-%s.%s" outputFileName outputExtension
        let outputDirName   = Path.GetDirectoryName output

        Path.Combine (outputDirName, outputFileName)
      infof "Effective output file name: %s" output

      if not overwriteOutput && Path.Exists output then
        abort 92 "Output path exists, specify -oo to overwrite it"

      let tic80AllowedPalette  =
        tic80Palette
        |> Array.filter (fun kv -> Array.BinarySearch (allowedKeys.AsArray, fst kv) > -1)
        |> Array.filter (fun kv -> Array.BinarySearch (forbiddenKeys.AsArray, fst kv) < 0)

      infof "Effective palette: %s" <| String.Join (',', tic80AllowedPalette |> Array.map fst)

      let tic80AllowedPalette  =
        tic80AllowedPalette
        |> Array.map snd

      hili "Loading image"
      use image     = Image.Load<Rgba32> input.FullPath

      // Capture the alpha channel
      let imageBits : Rgba32 array = Array.zeroCreate (image.Width*image.Height)
      image.CopyPixelDataTo (imageBits.AsSpan ())

      hili "Quantifying image colors to TIC-80 palette"
      // TODO: Seems this causes transparency to be lost?
      do
        let options = new QuantizerOptions()
        if not dithering then
          options.Dither <- null
        let quantizer = PaletteQuantizer (tic80AllowedPalette, options)
        let mutator (ctx : IImageProcessingContext) =
          ctx.Quantize quantizer |> ignore
        image.Mutate mutator

      // Write back the alpha channel becuase dropped for some reason?
      do
        let pa =
          PixelAccessorAction<Rgba32> (
            fun a ->
              let h = a.Height
              let w = a.Width
              for y = 0 to h - 1 do
                let yoff = w*y
                let row = a.GetRowSpan y
                for x = 0 to w - 1 do
                  let bit = imageBits.[yoff+x]
                  let mutable pix  = row.[x]
                  pix.A   <- bit.A
                  row.[x] <- pix
            )
        image.ProcessPixelRows pa

      let tk =
        match transparencyKey with
        | NoKey               -> 0
        | TransparencyKey tk  -> tk

      match outputType with
      | OutputType.Lua     ->
        hilif "Saving as LUA code: %s" output
        let sb = StringBuilder ""
        let inline app      str = sb.Append (str : string) |> ignore
        let inline appline  str = sb.AppendLine (str : string) |> ignore
        let inline num      i   =
          sb.Append (i : int) |> ignore
          sb.Append ',' |> ignore

        let pa =
          PixelAccessorAction<Rgba32> (
            fun a ->
              let h = a.Height
              let w = a.Width

              let prelude =
                sprintf """
-- Example on how to draw imageData to screen
function drawImage()
  local x,y,yoff
  for y=0,%d do
  yoff = y*%d
  for x=0,%d do
    c = imageData[1+yoff+x]
    pix(x,y,c)
  end
  end
end
-- Image data: %dx%d
imageData = {
"""               (h - 1) w (w - 1) w h

              appline prelude
              for y = 0 to h - 1 do
                app "  "
                let row = a.GetRowSpan y
                for x = 0 to w - 1 do
                  let pix = row.[x]
                  if pix.A < 127uy then
                    num tk
                  else
                    let idx = tic80PaletteLookup.[pix.Rgb]
                    num idx
                appline ""
              appline "}"
            )
        image.ProcessPixelRows pa

        File.WriteAllText (output, sb.ToString ())

      | OutputType.Png     ->
        hilif "Saving as PNG image: %s" output
        image.Save output
      | OutputType.Tic80   ->
        hilif "Saving as text that can be pasted into the sprite editor: %s" output

        let sb = StringBuilder """
-- To paste image data into TIC-80, copy one of the four data lines below.
-- In TIC-80, switch the sprite editor to 64x64 mode, choose the target sprite position,
-- and press Ctrl+V to paste the data.

"""

        let inline num      i   =
          let i = i &&& 0xF
          if i < 10 then
            sb.Append i |> ignore
          else
            let i = int 'a' + i - 10
            sb.Append (char i) |> ignore

        let pa =
          PixelAccessorAction<Rgba32> (
            fun a ->
              // Writes the image as 4 64x64 TIC-80 paste buffer
              for yy = 0 to 1 do
                for xx = 0 to 1 do
                  for y = 64*yy to 64*yy+63 do
                    if y < a.Height then
                      let row = a.GetRowSpan y
                      for x = 64*xx to 64*xx+63 do
                        if x < a.Width then
                          let pix = row.[x]
                          if pix.A < 127uy then
                            num tk
                          else
                            let idx = tic80PaletteLookup.[pix.Rgb]
                            num idx
                        else
                          num tk
                    else
                      for x = 0 to 63 do
                        num tk
                  for x = 0 to 127 do
                    num 0
                  sb.AppendLine () |> ignore
            )
        image.ProcessPixelRows pa

        File.WriteAllText (output, sb.ToString ())
      | _ ->
        abortf 70 "Illegal case %A" outputType

      0
    with
    | :? AbortException as e ->
      e.Data0

  ctx.ExitCode <- exitCode
[<EntryPoint>]
let main
  (args : string array)
  : int =
  CultureInfo.CurrentCulture                <- invariant
  CultureInfo.CurrentUICulture              <- invariant
  CultureInfo.DefaultThreadCurrentCulture   <- invariant
  CultureInfo.DefaultThreadCurrentUICulture <- invariant

  let rootCommand = RootCommand "fsimg2tic80 - Converts image to images usable in tic80"

  ([|
      inputOption
      outputTypeOption
      transparencyKeyOption
      allowedKeysOption
      forbiddenKeysOption
      ditheringOption
      overwriteOutputOption
    |] : Option array)
    |> Array.iter rootCommand.AddOption

  rootCommand.SetHandler rootCommandHandler

  let readmeCommand = Command ("readme", "Displays fsimg2tic80's README file")
  readmeCommand.SetHandler readmeCommandHandler
  rootCommand.AddCommand readmeCommand

  let licenseCommand = Command ("license", "Displays fsimg2tic80's LICENSE file")
  licenseCommand.SetHandler licenseCommandHandler
  rootCommand.AddCommand licenseCommand

  let noticeCommand = Command ("notice", "Displays fsimg2tic80's NOTICE file")
  noticeCommand.SetHandler noticeCommandHandler
  rootCommand.AddCommand noticeCommand


  rootCommand.Invoke args
