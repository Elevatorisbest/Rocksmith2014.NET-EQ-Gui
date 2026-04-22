namespace DLCBuilder.Controls

open Avalonia
open Avalonia.Controls
open Avalonia.Controls.Shapes
open Avalonia.FuncUI.Builder
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open Avalonia.Input
open Avalonia.Media
open System
open DLCBuilder.ToneGear
open Avalonia.Layout

type EQBand =
    { BaseName: string
      FixedFreq: float32 option
      GainKnob: GearKnob option
      FreqKnob: GearKnob option
      QKnob: GearKnob option }

module VisualEQParser =
    let isEQGear (gear: GearData) =
        let name = gear.Name.ToUpperInvariant()
        let key = gear.Key.ToUpperInvariant()
        name.Contains("EQ") || key.Contains("EQ") || 
        (gear.Category = "Filter" && (name.Contains("EQ") || name.Contains("GRAPHIC")))

    let getBaseName (name: string) =
        name.Replace(" Frequency", "")
            .Replace(" Freq. Shift", "")
            .Replace(" Freq.", "")
            .Replace(" Freq", "")
            .Replace(" \"Q\"", "")
            .Replace(" Q", "")
            .Replace("Midrange", "Mid")

    let parseFixedFreq (name: string) =
        let lower = name.ToLowerInvariant().Replace(" ", "")
        if lower.EndsWith("hz") then
            let vStr = lower.Substring(0, lower.Length - 2)
            if vStr.EndsWith("k") then
                let numStr = vStr.Substring(0, vStr.Length - 1)
                match Single.TryParse(numStr, System.Globalization.NumberStyles.Any, System.Globalization.CultureInfo.InvariantCulture) with
                | true, v -> Some (v * 1000.f)
                | _ -> None
            else
                match Single.TryParse(vStr, System.Globalization.NumberStyles.Any, System.Globalization.CultureInfo.InvariantCulture) with
                | true, v -> Some v
                | _ -> None
        else None

    let parseBands (knobs: GearKnob array) : EQBand array =
        knobs
        |> Array.groupBy (fun k -> getBaseName k.Name)
        |> Array.map (fun (baseName, group) ->
            let gain = group |> Array.tryFind (fun k -> k.Name.Replace("Midrange", "Mid") = baseName)
            let freq = group |> Array.tryFind (fun k -> k.Name.Contains("Freq"))
            let q = group |> Array.tryFind (fun k -> k.Name.Contains("Q"))
            let fixedFreq = if freq.IsNone then parseFixedFreq baseName else None
            { BaseName = baseName
              FixedFreq = fixedFreq
              GainKnob = gain
              FreqKnob = freq
              QKnob = q }
        )
        |> Array.filter (fun b -> b.GainKnob.IsSome || b.FreqKnob.IsSome)
        |> Array.sortBy (fun b -> 
            match b.FixedFreq, b.FreqKnob with
            | Some f, _ -> f
            | None, Some fKnob -> fKnob.MinValue
            | _ -> 0.0f)

type BandUI = {
    Band: EQBand
    Handle: Ellipse
    FillPath: Path
    Color: IBrush
}

type VisualEQGraph() as this =
    inherit UserControl()

    let mutable m_gearData: GearData option = None
    let mutable m_knobValues = Map.empty<string, float32>
    let mutable changeCallback: string * float32 -> unit = ignore

    let canvas = Canvas(Background = Brushes.Transparent, ClipToBounds = true)
    let container = Border(Background = SolidColorBrush.Parse("#1e1e1e"), BorderBrush = Brushes.Gray, BorderThickness = Thickness(1.0), Child = canvas, Height = 250.0, Margin = Thickness(16., 8., 0., 8.))
    
    let gridPath = Path(Stroke = Brushes.DarkGray, StrokeThickness = 1.0, Opacity = 0.3, IsHitTestVisible = false)
    let globalCurvePath = Path(Stroke = Brushes.White, StrokeThickness = 2.0, IsHitTestVisible = false)

    let mutable bandUIs = [] : BandUI list
    let mutable minG = -15.0
    let mutable maxG = 15.0

    let minFreqLog = Math.Log10(20.0)
    let maxFreqLog = Math.Log10(20000.0)
    let freqRangeLog = maxFreqLog - minFreqLog

    let freqToX (freq: float) (width: float) =
        let logF = Math.Log10(Math.Max(20.0, freq))
        let normalized = (logF - minFreqLog) / freqRangeLog
        normalized * width

    let xToFreq (x: float) (width: float) =
        let normalized = Math.Clamp(x / width, 0.0, 1.0)
        let logF = minFreqLog + normalized * freqRangeLog
        Math.Pow(10.0, logF)

    let gainToY (gain: float) (minGain: float) (maxGain: float) (height: float) =
        let range = maxGain - minGain
        if range = 0.0 then height / 2.0
        else
            let normalized = (gain - minGain) / range
            height - (normalized * height)

    let yToGain (y: float) (minGain: float) (maxGain: float) (height: float) =
        let normalized = Math.Clamp(1.0 - (y / height), 0.0, 1.0)
        let range = maxGain - minGain
        minGain + normalized * range

    let getFreqForBand (band: EQBand) =
        match band.FixedFreq, band.FreqKnob with
        | Some f, _ -> float f
        | None, Some fk -> 
            let v = m_knobValues.TryFind fk.Key |> Option.defaultValue fk.DefaultValue
            let multiplier = if fk.UnitType = "khz" then 1000.0 else 1.0
            float v * multiplier
        | _ -> 1000.0

    let getGainForBand (band: EQBand) =
        match band.GainKnob with
        | Some gk -> m_knobValues.TryFind gk.Key |> Option.defaultValue gk.DefaultValue |> float
        | None -> 0.0

    let getQForBand (band: EQBand) =
        match band.QKnob with
        | Some qk ->
            let v = m_knobValues.TryFind qk.Key |> Option.defaultValue qk.DefaultValue |> float
            if qk.EnumValues.IsSome then
                // If it's an enum (like Narrow/Wide), 0 is usually narrow (higher Q), 1 is wide (lower Q)
                if v > 0.0 then 0.5 else 2.0
            else v
        | None -> 1.414

    let evaluateBandResponse (band: EQBand) (f: float) =
        let fc = getFreqForBand band
        let g = getGainForBand band
        let q = getQForBand band
        if g = 0.0 || f = 0.0 || fc = 0.0 then 0.0
        else
            let w = f / fc
            let invQ = 1.0 / q
            let A = Math.Pow(10.0, g / 40.0)
            let numerator = Math.Pow(1.0 - w*w, 2.0) + Math.Pow(A * invQ * w, 2.0)
            let denominator = Math.Pow(1.0 - w*w, 2.0) + Math.Pow((1.0/A) * invQ * w, 2.0)
            let magLinear = Math.Sqrt(numerator / denominator)
            20.0 * Math.Log10(Math.Max(magLinear, 0.0001))

    let updateCurve () =
        let width = Math.Max(1.0, canvas.Bounds.Width)
        let height = Math.Max(1.0, canvas.Bounds.Height)

        let gridGeo = StreamGeometry()
        using (gridGeo.Open()) (fun ctx ->
            let freqs = [ 20.0; 50.0; 100.0; 200.0; 500.0; 1000.0; 2000.0; 5000.0; 10000.0; 20000.0 ]
            for f in freqs do
                let x = freqToX f width
                ctx.BeginFigure(Point(x, 0.0), false)
                ctx.LineTo(Point(x, height))
                ctx.EndFigure(false)
            
            let gains = [ -15.0 .. 3.0 .. 15.0 ]
            for g in gains do
                if g >= minG && g <= maxG then
                    let y = gainToY g minG maxG height
                    ctx.BeginFigure(Point(0.0, y), false)
                    ctx.LineTo(Point(width, y))
                    ctx.EndFigure(false)
        )
        gridPath.Data <- gridGeo

        for child in canvas.Children do
            match child with
            | :? TextBlock as tb ->
                match tb.Tag with
                | :? (string * float) as tag ->
                    match tag with
                    | ("freq", f) ->
                        let x = freqToX f width
                        Canvas.SetLeft(tb, x - 10.0)
                        Canvas.SetTop(tb, height - 20.0)
                    | ("gain", g) ->
                        let y = gainToY g minG maxG height
                        Canvas.SetLeft(tb, width - 24.0)
                        Canvas.SetTop(tb, y - 8.0)
                    | _ -> ()
                | _ -> ()
            | _ -> ()

        let globalGeo = StreamGeometry()
        using (globalGeo.Open()) (fun ctx ->
            let startY = gainToY 0.0 minG maxG height
            ctx.BeginFigure(Point(0.0, startY), false)
            for x in 0.0 .. 2.0 .. width do
                let f = xToFreq x width
                let mutable totalGain = 0.0
                for b in bandUIs do
                    totalGain <- totalGain + evaluateBandResponse b.Band f
                let y = gainToY totalGain minG maxG height
                ctx.LineTo(Point(x, y))
            ctx.EndFigure(false)
        )
        globalCurvePath.Data <- globalGeo

        for b in bandUIs do
            let bandGeo = StreamGeometry()
            using (bandGeo.Open()) (fun ctx ->
                let startY = gainToY 0.0 minG maxG height
                ctx.BeginFigure(Point(0.0, startY), true)
                for x in 0.0 .. 2.0 .. width do
                    let f = xToFreq x width
                    let g = evaluateBandResponse b.Band f
                    let y = gainToY g minG maxG height
                    ctx.LineTo(Point(x, y))
                ctx.LineTo(Point(width, startY))
                ctx.EndFigure(true)
            )
            b.FillPath.Data <- bandGeo

            let fc = getFreqForBand b.Band
            let g = getGainForBand b.Band
            let x = freqToX fc width
            let y = gainToY g minG maxG height
            Canvas.SetLeft(b.Handle, x - b.Handle.Width / 2.0)
            Canvas.SetTop(b.Handle, y - b.Handle.Height / 2.0)

    let updateLayout () =
        canvas.Children.Clear()
        bandUIs <- []
        canvas.Children.Add(gridPath)

        match m_gearData with
        | Some gd when gd.Knobs.IsSome ->
            let bands = VisualEQParser.parseBands gd.Knobs.Value
            
            let mns = bands |> Array.choose (fun b -> b.GainKnob |> Option.map (fun k -> float k.MinValue)) |> Array.toList
            let mxs = bands |> Array.choose (fun b -> b.GainKnob |> Option.map (fun k -> float k.MaxValue)) |> Array.toList
            if not mns.IsEmpty then minG <- List.min mns else minG <- -15.0
            if not mxs.IsEmpty then maxG <- List.max mxs else maxG <- 15.0

            let colors = [| Brushes.Crimson; Brushes.DarkOrange; Brushes.Gold; Brushes.LimeGreen; Brushes.DodgerBlue; Brushes.MediumOrchid; Brushes.DeepPink |]
            
            bands |> Array.iteri (fun i band ->
                let color = colors.[i % colors.Length]
                let fillPath = Path(Fill = color, Opacity = 0.3, IsHitTestVisible = false)
                canvas.Children.Add(fillPath)
                let handle = Ellipse(Width = 14.0, Height = 14.0, Fill = color, Stroke = Brushes.White, StrokeThickness = 2.0, Cursor = new Cursor(StandardCursorType.Hand))
                bandUIs <- { Band = band; Handle = handle; FillPath = fillPath; Color = color } :: bandUIs
            )
        | _ ->
            minG <- -15.0
            maxG <- 15.0

        canvas.Children.Add(globalCurvePath)

        for b in bandUIs |> List.rev do
            canvas.Children.Add(b.Handle)

        let freqs = [| (20.0, "20"); (50.0, "50"); (100.0, "100"); (200.0, "200"); (500.0, "500"); (1000.0, "1k"); (2000.0, "2k"); (5000.0, "5k"); (10000.0, "10k"); (20000.0, "20k") |]
        for (f, label) in freqs do
            let tb = TextBlock(Text = label, Foreground = Brushes.Gray, FontSize = 10.0, Tag = ("freq", f), IsHitTestVisible = false)
            canvas.Children.Add(tb)

        let gains = [ -15.0 .. 3.0 .. 15.0 ]
        for g in gains do
            if g >= minG && g <= maxG then
                let text = if g > 0.0 then sprintf "+%g" g else sprintf "%g" g
                let tb = TextBlock(Text = text, Foreground = Brushes.Gray, FontSize = 10.0, Tag = ("gain", g), IsHitTestVisible = false)
                canvas.Children.Add(tb)

        updateCurve()

    let mutable draggingBand: EQBand option = None

    do
        this.Content <- container
        canvas.PropertyChanged.Add(fun e ->
            if e.Property.Name = "Bounds" then updateCurve()
        )

        canvas.PointerPressed.Add(fun e ->
            let pos = e.GetPosition(canvas)
            let width = Math.Max(1.0, canvas.Bounds.Width)
            let height = Math.Max(1.0, canvas.Bounds.Height)
            let closest = 
                if bandUIs.IsEmpty then None
                else
                    Some (bandUIs |> List.minBy (fun b ->
                        let f = getFreqForBand b.Band
                        let g = getGainForBand b.Band
                        let x = freqToX f width
                        let y = gainToY g minG maxG height
                        Math.Pow(pos.X - x, 2.0) + Math.Pow(pos.Y - y, 2.0)
                    ))
            
            match closest with
            | Some b ->
                let f = getFreqForBand b.Band
                let g = getGainForBand b.Band
                let x = freqToX f width
                let y = gainToY g minG maxG height
                if Math.Sqrt(Math.Pow(pos.X - x, 2.0) + Math.Pow(pos.Y - y, 2.0)) < 20.0 then
                    draggingBand <- Some b.Band
                    e.Pointer.Capture(canvas) |> ignore
            | None -> ()
        )

        canvas.PointerMoved.Add(fun e ->
            match draggingBand with
            | Some b ->
                let pos = e.GetPosition(canvas)
                let width = Math.Max(1.0, canvas.Bounds.Width)
                let height = Math.Max(1.0, canvas.Bounds.Height)

                match b.GainKnob with
                | Some gk ->
                    let mg = float gk.MinValue
                    let xg = float gk.MaxValue
                    let newGain = yToGain pos.Y minG maxG height
                    let clampedGain = Math.Max(mg, Math.Min(xg, newGain))
                    let step = float gk.ValueStep
                    let snappedGain = Math.Round(clampedGain / step) * step
                    changeCallback(gk.Key, float32 snappedGain)
                | None -> ()

                match b.FreqKnob with
                | Some fk when fk.EnumValues.IsNone ->
                    let newFreq = xToFreq pos.X width
                    let multiplier = if fk.UnitType = "khz" then 1000.0 else 1.0
                    let actualFreq = newFreq / multiplier
                    let mf = float fk.MinValue
                    let xf = float fk.MaxValue
                    let clampedFreq = Math.Max(mf, Math.Min(xf, actualFreq))
                    let step = float fk.ValueStep
                    let snappedFreq = Math.Round(clampedFreq / step) * step
                    changeCallback(fk.Key, float32 snappedFreq)
                | _ -> ()

            | None -> ()
        )

        canvas.PointerReleased.Add(fun e ->
            if draggingBand.IsSome then
                draggingBand <- None
                e.Pointer.Capture(null) |> ignore
        )

        canvas.PointerWheelChanged.Add(fun e ->
            let pos = e.GetPosition(canvas)
            let width = Math.Max(1.0, canvas.Bounds.Width)
            let height = Math.Max(1.0, canvas.Bounds.Height)

            let closest = 
                if bandUIs.IsEmpty then None
                else
                    Some (bandUIs |> List.minBy (fun b ->
                        let f = getFreqForBand b.Band
                        let g = getGainForBand b.Band
                        let x = freqToX f width
                        let y = gainToY g minG maxG height
                        Math.Pow(pos.X - x, 2.0) + Math.Pow(pos.Y - y, 2.0)
                    ))
            
            match closest with
            | Some b ->
                let f = getFreqForBand b.Band
                let g = getGainForBand b.Band
                let x = freqToX f width
                let y = gainToY g minG maxG height
                if Math.Sqrt(Math.Pow(pos.X - x, 2.0) + Math.Pow(pos.Y - y, 2.0)) < 30.0 then
                    match b.Band.QKnob with
                    | Some qk ->
                        let mq = float qk.MinValue
                        let xq = float qk.MaxValue
                        let step = float qk.ValueStep
                        let currentQ = m_knobValues.TryFind qk.Key |> Option.defaultValue qk.DefaultValue |> float
                        if qk.EnumValues.IsSome then
                            let delta = if e.Delta.Y > 0.0 then 1.0 else -1.0
                            let newQ = Math.Clamp(currentQ + delta, mq, xq)
                            changeCallback(qk.Key, float32 newQ)
                        else
                            let delta = if e.Delta.Y > 0.0 then 1.0 else -1.0
                            let newQ = Math.Clamp(currentQ + delta * (xq - mq) * 0.05, mq, xq)
                            let snappedQ = Math.Round(newQ / step) * step
                            changeCallback(qk.Key, float32 snappedQ)
                        e.Handled <- true
                    | None -> ()
            | None -> ()
        )

    member this.GearData
        with get() = m_gearData
        and set(v) = 
            m_gearData <- v
            updateLayout()

    member this.KnobValues
        with get() = m_knobValues
        and set(v) =
            m_knobValues <- v
            updateCurve()

    member this.OnKnobValueChangedCallback
        with get() = changeCallback
        and set(v) = changeCallback <- v

    static member gearData(gear: GearData option) =
        let getter (c: VisualEQGraph) = c.GearData
        let setter: VisualEQGraph * GearData option -> unit = fun (c, v) -> c.GearData <- v
        AttrBuilder<VisualEQGraph>.CreateProperty<GearData option>
            ("GearData", gear, ValueSome getter, ValueSome setter, ValueNone)

    static member knobValues(values: Map<string, float32>) =
        let getter (c: VisualEQGraph) = c.KnobValues
        let setter: VisualEQGraph * Map<string, float32> -> unit = fun (c, v) -> c.KnobValues <- v
        AttrBuilder<VisualEQGraph>.CreateProperty<Map<string, float32>>
            ("KnobValues", values, ValueSome getter, ValueSome setter, ValueNone)

    static member onKnobValueChanged fn =
        let getter (c: VisualEQGraph) = c.OnKnobValueChangedCallback
        let setter: VisualEQGraph * (string * float32 -> unit) -> unit = fun (c, f) -> c.OnKnobValueChangedCallback <- f
        AttrBuilder<VisualEQGraph>.CreateProperty<string * float32 -> unit>
            ("OnKnobValueChanged", fn, ValueSome getter, ValueSome setter, ValueSome (fun _ -> true))

[<RequireQualifiedAccess>]
module VisualEQGraph =
    let create (attrs: IAttr<VisualEQGraph> list) : IView<VisualEQGraph> =
        ViewBuilder.Create<VisualEQGraph>(attrs)
