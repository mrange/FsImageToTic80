(*
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
  | Lua
  | Png
  | Tic80

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
    , getDefaultValue = fun () -> Tic80
    )

let transparencyKeyOption =
  Option<int Nullable>(
      aliases         = [|"-tk"; "--transparency-key"|]
    , description     = "Which color key (0-15) is considered transparent"
    )

let allowedKeysOption =
  Option<string>(
      aliases         = [|"-ac"; "--allowed-keys"|]
    , description     = "Which color keys (0-15) are allowed"
    )

let forbiddenKeysOption =
  Option<string>(
      aliases         = [|"-fc"; "--forbidden-keys"|]
    , description     = "Which color keys (0-15) are forbidden"
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
    if isBetween k 0 15 then
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
  (s  : string|null ) 
  (dv : string      )
  : Keys =
  let s =
    if isNull s 
    then dv
    else s
  let mapper i (s : string) : int =
    let numberStyle = 
      NumberStyles.Integer
      ||| NumberStyles.AllowHexSpecifier
      ||| NumberStyles.AllowLeadingWhite
      ||| NumberStyles.AllowTrailingWhite

    match Int32.TryParse (s, numberStyle, CultureInfo.InvariantCulture) with
    | true  , v -> 
      if isBetween v 0 15 then
        abortf 81 "Key list '%s' contains value(s) that are outside range [0,15]. It broke at index %d" s i
      else
        v
    | false , _ -> abortf 82 "Unable to parse key list '%s' into a sequence of integers. It broke at index %d" s i
  s.Split (',')
  |> Array.mapi mapper
  |> Array.distinct
  |> Array.sort
  |> Keys

let rootCommandHandler
  (ctx            : InvocationContext )
  : unit =
  let inline getValue option = ctx.ParseResult.GetValueForOption option

  let input           = getValue inputOption
  let outputType      = getValue outputTypeOption
  let transparencyKey = getValue transparencyKeyOption
  let allowedKeys     = getValue allowedKeysOption
  let forbiddenKeys   = getValue forbiddenKeysOption
  let overwriteOutput = getValue overwriteOutputOption

  let exitCode = 
    try
      assert (isNotNull input)

      let input             = toImagePath       input
      let transparencyKey   = toTransparencyKey transparencyKey
      let allowedKeys       = toKeys            allowedKeys     "0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15"
      let forbiddenKeys     = toKeys            forbiddenKeys   ""

      hili  "fsimg2tic80 - Converts image to images usable in TIC-80"
      infof "  Input image path         : %s"   input.Pretty
      infof "  Output type              : %A"   outputType
      infof "  Transparency key         : %s"   transparencyKey.Pretty
      infof "  Allowed keys             : %s"   allowedKeys.Pretty
      infof "  Forbidden keys           : %s"   forbiddenKeys.Pretty

      if not (Path.Exists input.FullPath) then
        abort 91 "Input path does not exists"

      let output = 
        let output = input.FullPath
        let outputExtension = 
          match outputType with
          | Lua   -> ".lua"
          | Png   -> ".png"
          | Tic80 -> ".tic80"
        let outputFileName  = Path.GetFileNameWithoutExtension output
        let outputFileName  = sprintf "tic80-%s.%s" outputFileName outputExtension
        let outputDirName   = Path.GetDirectoryName output

        Path.Combine (outputDirName, outputFileName)

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

      hilif "Loading image: %s" input.FullPath

      use image     = Image.Load<Rgba32> input.FullPath

      hilif "Changing image to TIC-80 palette: %s" input.FullPath
      let quantizer = PaletteQuantizer tic80AllowedPalette
      quantizer.Options.Dither <- null
      do
        let mutator (ctx : IImageProcessingContext) = 
          ctx.Quantize quantizer |> ignore
      
          ()
        image.Mutate mutator

      match outputType with
      | Lua     ->
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
              appline (sprintf "-- Image data: %dx%d\nimageData = {" w h)
              for y = 0 to h - 1 do
                app "  "
                let row = a.GetRowSpan y
                for x = 0 to w - 1 do
                  let pix = row.[x]
                  let idx = tic80PaletteLookup.[pix.Rgb]
                  num idx
                appline ""
              appline "}"
            )
        image.ProcessPixelRows pa

        File.WriteAllText (output, sb.ToString ())

      | Png     ->
        hilif "Saving as PNG image: %s" output
        image.Save output
      | Tic80   ->
        hilif "Saving as text that can be pasted into the sprite editor: %s" output
        
        let sb = StringBuilder """
-- In order to paste image data into TIC-80 copy one of the four data lines
-- below. Then in TIC-80 set the sprite editor in 64x64 modus. Select
-- the place for the sprite and paste by pressing ctrl-v

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
              let tk =
                match transparencyKey with
                | NoKey               -> 0
                | TransparencyKey tk  -> tk
              // Writes the image as 4 64x64 tic-80 paste buffer
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
      overwriteOutputOption
    |] : Option array)
    |> Array.iter rootCommand.AddOption

  rootCommand.SetHandler rootCommandHandler

  rootCommand.Invoke args
