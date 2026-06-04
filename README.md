# Kinetochore Morphology Analysis Pipeline

This pipeline quantifies kinetochore morphology from live-cell fluorescence movies. It combines TrackMate-based tracking, manually curated sister-pair information, K-K-axis rotation, per-kinetochore image analysis, movement summaries, and merged-window filtering.

## Files

| File | Purpose |
|---|---|
| `KKCalculationAndRotation.wl` | Reads TrackMate `export.csv` and `pairs.xlsx`, computes K-K vectors, K-K distance, K-K angle, and optionally exports rotated central-z KT projections. |
| `mainprogram.wl` | Batch analysis of cropped/rotated KT images. Exports one CSV per KT and inspection plots. |
| `singleImageAnalysis.wl` | Interactive analysis of one image. Useful for parameter tuning and visual inspection. |
| `result_extract.wl` | Combines per-KT CSVs, K-K/movement outputs, Ch1/Ch2 information, and merged-window filtering into experiment-level result tables. |
| `runSingleImageAnalysis.nb` | Notebook wrapper for one-image testing. |
| `runMainprogram.nb` | Notebook wrapper for batch KT image analysis. |
| `runKKCalculationRotation.nb` | Notebook wrapper for K-K calculation and rotation. |

## Required Inputs

At minimum, each cell folder should contain:

| Input | Location | Notes |
|---|---|---|
| `export.csv` | Cell folder | TrackMate export table containing spot IDs, labels, frame numbers, and XYZ coordinates. |
| `pairs.xlsx` or `Pairs.xlsx` | Cell folder | Manually curated sister KT pair labels. |
| 4D image files, `.tif` or `.tiff` | Channel folders under each cell folder | Used when running rotation. ImageJ metadata should contain channel, slice, and frame counts. |
| Per-KT image folders | Under channel and pair folders | Batch analysis expects KT folders with a `(+-1)` image folder after rotation/export. |

Recommended cell-set structure:

```text
experiment_root/
  cell_001/
    export.csv
    pairs.xlsx
    ch1/
      movie_or_KT_files.tif
      pair_folder/
        KT_label/
          (+-1)/
    ch2/
      ...
  cell_002/
    ...
```

Folder names may vary, but the scripts rely on `export.csv`, `pairs.xlsx`, channel folders such as `ch1` and `ch2`, sister-pair folders, KT-label folders, and `(+-1)` analysis folders.

## Recommended Workflow

### 1. Calculate K-K Geometry and Rotate KT Movies

Use this step after TrackMate tracking and sister-pair curation.

```wl
script = SystemDialogInput["FileOpen", WindowTitle -> "Select KKCalculationAndRotation.wl"];
root = SystemDialogInput["Directory", WindowTitle -> "Select the experiment root folder"];

If[script =!= $Canceled && root =!= $Canceled,
  Get[script];
  runKKCalculationAndRotation[
    root,
    "ChannelCount" -> 2,
    "RunRotation" -> True,
    "ImageChannel" -> 1,
    "CropFraction" -> 0.7,
    "Parallel" -> True,
    "Kernels" -> 4
  ]
]
```

Important outputs include:

| Output | Meaning |
|---|---|
| `kkVECTOR*.csv` | K-K vectors for paired sisters. |
| `(number)kkDistance*.csv` | K-K distance. |
| `(number)kkDistanceWithFramesNumberOnly*.csv` | K-K distance aligned to frame numbers. |
| `kkAngle*.csv` and `kkAngleWithFrame*.csv` | Rotation angles for each KT. |
| `KTlocation_*self.csv` / `KTlocation_*sister.csv` | Self and sister KT location tables. |
| `(+-1).tif` | Central-z stack selected for projection. |
| `(+-1)/` | Rotated projected frames used by batch image analysis. |

### 2. Tune Parameters on One Image

Before running a large batch, inspect representative images.

```wl
script = SystemDialogInput["FileOpen", WindowTitle -> "Select singleImageAnalysis.wl"];
If[script =!= $Canceled,
  Get[script];
  runAndDisplaySingleImageAnalysis[]
]
```

Select one KT image when prompted. Use the inspection plots to confirm:

| QC Item | What to Check |
|---|---|
| Segmentation mask | The main KT signal is retained and nearby objects are excluded. |
| Peak detection | True 1D/2D peaks are marked, weak noise peaks are rejected. |
| Tail detection | Derivative extrema match visually obvious tail-like signal. |
| Scale bar | The displayed scale bar should match the configured `pixelsize`. |

### 3. Run Batch KT Image Analysis

```wl
script = SystemDialogInput["FileOpen", WindowTitle -> "Select mainprogram.wl"];
root = SystemDialogInput["Directory", WindowTitle -> "Select the experiment root folder"];

If[script =!= $Canceled && root =!= $Canceled,
  Get[script];
  batch = runMainBatchAnalysis[
    root,
    "Kernels" -> 4,
    "ChunkSize" -> Automatic,
    "Resume" -> True,
    "ExportPlots" -> True
  ];
]
```

The batch runner searches for `(+-1)` folders, analyzes each KT image sequence, and writes per-KT CSVs plus inspection images. With `"Resume" -> True`, completed KT folders are stored in `finished.csv` and skipped in later runs.

### 4. Extract Experiment-Level Results

```wl
script = SystemDialogInput["FileOpen", WindowTitle -> "Select result_extract.wl"];
If[script =!= $Canceled, Get[script]];
```

Set `configuredChannelNumber` in `result_extract.wl` before running. The script asks for the root folder, then combines per-KT measurements with K-K distance, movement metrics, Ch1/Ch2 ZNCC fitting vectors, and merged-window filtering.

For double-channel data, `exportZNCCFittingResults = True` adds `znccFittingVectorX(um)`, `znccFittingVectorY(um)`, and `znccFittingScore` to `Compiled results` and to `YYYY-MM-DDallKT.csv`. The fitted vector is subpixel-refined, converted to micrometers with `znccPixelSizeUm`, and is independent from the merged-window `PreSelect` flag. For a quick test run, set `exportZNCCFittingResults = False`; the vector columns will be exported as `NA`.

`displayResultExtractionReport[]` shows the compiled table with the ZNCC vector columns, plus a Ch1/Ch2 ZNCC fitting check panel for visual inspection of a selected compiled row.

Main outputs:

| Output | Meaning |
|---|---|
| `YYYY-MM-DDallKT.csv` | Combined per-frame KT morphology table. Double-channel runs include subpixel ZNCC fitting vector columns in micrometers. |
| `YYYY-MM-DDMovement_condition1.csv` | Per-KT movement and morphology summary. |
| `YYYY-MM-DDNormalized_of_YYYY-MM-DDMovement.csv` | Movement table normalized within KT groups. |
| `YYYY-MM-DDallKT_PreSelectMark.csv` | Combined table with merged-window preselection features and `PreSelect` flag. |
| `YYYY-MM-DDallKT_filtered.csv` | `allKT` table with likely merged-window frames removed. |
| `YYYY-MM-DDextract_PreSelectMark.csv` | Extract table with merged-window `PreSelect` flag and optional Ch1/Ch2 registration columns. |
| `YYYY-MM-DDextract_filtered.csv` | Final filtered extract table. |

## Output Metrics

Per-frame KT analysis exports these columns:

| Column | Meaning |
|---|---|
| `ktArea(pixels)` | Segmented KT object area in pixels. |
| `width_X(AUC/max)(um)` | Kinetochore length along the K-K/force axis using AUC/fmax. |
| `width_Y(AUC/max)(um)` | Kinetochore width perpendicular to the K-K/force axis using AUC/fmax. |
| `orientationAngle(deg)` | Orientation angle of the segmented object. |
| `elongationRatioResult` | Shape elongation metric from object morphology. |
| `semiaxesRatioResult` | Ratio of fitted semiaxes. |
| `tail or not` | Whether derivative-based tail detection is positive. |
| `tail direction` | `left`, `right`, `both`, or `none`. |
| `tail length(um)` | Side-specific AUC/fmax-derived tail length. |
| `asymmetryResult` | Signed left/right intensity imbalance along the X axis. |
| `Xproj_peak number`, `Yproj_peak number` | Number of accepted 1D peaks in X and Y projections. |
| `number of 2D peaks` | Number of accepted 2D local maxima. |
| `2D peak dist X(um)`, `2D peak dist Y(um)` | X/Y distance between the two strongest 2D peaks. |
| `ratio 2nd/1st peak` | Background-corrected intensity ratio of the second 2D peak to the primary 2D peak. |
| `totalIntensity` | Background-corrected integrated intensity in the analyzed KT mask. |

## Quality Control and Parameter Tuning

Use `singleImageAnalysis.wl` first on representative images from each imaging condition. Tune parameters only when visual QC indicates a consistent failure mode.

| Symptom | Likely Parameter to Adjust |
|---|---|
| KT mask misses dim boundary signal | Decrease `backgroundSeprationFactor` or increase `boundaryDilation`. |
| Mask includes neighboring KT or chromatin signal | Increase `backgroundSeprationFactor`, decrease `boundaryDilation`, or raise `smallNoiseComponentSize`. |
| Too many weak 2D peaks are accepted | Increase `peakIntensityThreasholdFactor` or `peakRatioThreashold`. |
| True secondary peaks are missed | Decrease `peakIntensityThreasholdFactor` or `peakRatioThreashold`. |
| Tail calls are too frequent | Increase `d1PeakBackgroundThreasholdRatio` or adjust smoothing. |
| Weak tails are missed | Decrease `d1PeakBackgroundThreasholdRatio` or adjust smoothing. |
| Merged-window filter removes too many frames | Increase `overlapSeparationIntensityThreshold` or `overlapSeparationPixelThreshold`. |
| Merged-window filter misses obvious merged KT windows | Decrease `overlapSeparationIntensityThreshold` or `overlapSeparationPixelThreshold`. |

## Notes

- `pixelsize` must be set correctly before analysis. It controls all micrometer-scale length outputs.
- The code keeps several historical variable names such as `Threashold`, `Sepration`, and `asymetry`. Treat these as code identifiers, not spelling guidance for manuscripts or figures.
- Merged-window filtering is a single-channel intensity/area QC step for frames where two nearby KT signals may have been detected as one window.
- Ch1/Ch2 channel overlay and ZNCC registration are two-channel checks for the relative position of the two protein labels. They are separate from merged-window filtering.
- Parallel execution is optional. Result extraction uses serial tracking-index mapping by default for stability; set `useParallelTrackingIndexMap = True` only after a small serial test run completes normally.
- Keep the original TrackMate exports and pair files unchanged. The scripts write derived outputs into channel, pair, KT, and experiment folders.
