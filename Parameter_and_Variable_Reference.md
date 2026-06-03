# Parameter and Variable Reference

This document maps paper-level concepts and symbols to the corresponding code variables, output columns, defaults, adjustment effects, and recommended user ranges.

## Core Image-Analysis Parameters

These parameters are defined near the top of `singleImageAnalysis.wl` and `mainprogram.wl`. Set them before running single-image or batch analysis.

| Paper concept or symbol | Code variable | Default | Meaning | Adjustment effect | Recommended user range |
|---|---:|---:|---|---|---|
| Pixel size, `dx` | `pixelsize` | `0.092` um/pixel in `singleImageAnalysis.wl`; `0.046*2` um/pixel in `mainprogram.wl` | Converts pixel distances to micrometers. | Larger value increases all exported length measurements in um; smaller value decreases them. Does not change segmentation in pixel units. | Must match microscope calibration. Use a positive numeric value. |
| Refined segmentation multiplier, `n` in `cutoff = mu_bg + n*sigma_bg` | `backgroundSeprationFactor` | `7` | Background SD multiplier for separating KT signal from background. | Lower values keep dimmer signal but may include background or neighboring KTs. Higher values are stricter but may remove real dim KT boundary signal. | Positive numeric. Start near `5` to `8`; inspect masks before batch use. |
| Minimum connected-component area | `smallNoiseComponentSize` | `9` pixels | Removes small connected components before selecting the main KT mask. | Lower values keep smaller objects and more noise. Higher values remove more small objects but may delete very small real signal. | Integer `>= 1`; common tuning range `3` to `30` pixels. |
| Boundary dilation, `m` | `boundaryDilation` | `3` pixels | Expands the selected main KT mask to include dim boundary signal. | Lower values reduce contamination from nearby objects. Higher values include more KT boundary but can capture neighbor signal. | Integer `>= 0`; typical range `1` to `5` pixels. |
| 2D peak background multiplier, beta | `peakIntensityThreasholdFactor` | `3` | Multiplier used in background- and boundary-referenced 2D peak thresholds. | Lower values accept weaker 2D peaks. Higher values reject more weak peaks. | Positive numeric; typical range `2` to `5`. |
| 2D peak relative-ratio coefficient, alpha | `peakRatioThreashold` | `0.1` | Requires a 2D secondary peak to reach a fraction of the primary peak above background. | Lower values accept weaker secondary peaks. Higher values keep only stronger secondary peaks. | Numeric `0` to `1`; typical range `0.05` to `0.3`. |
| Inspection contour count | `contours` | `15` | Number of contour levels in inspection plots. | Affects display only, not measurement values. | Positive integer; common range `8` to `30`. |
| 1D projection smoothing | `lowpassThreashold` | `2` | Smoothing parameter for the intensity projection before AUC/fmax, 1D peak, and tail analysis. | More smoothing reduces noise but can merge nearby features; less smoothing preserves sharp features but may increase noisy peaks. | Positive numeric. Tune visually with `singleImageAnalysis.wl`. |
| First-derivative smoothing | `d1LowpassThreashold` | `2` | Smoothing parameter for the first derivative used in tail detection. | More smoothing reduces false derivative extrema but can miss weak tails; less smoothing increases sensitivity and noise. | Positive numeric. Tune visually with derivative inspection plots. |
| Tail derivative threshold, `r` | `d1PeakBackgroundThreasholdRatio` | `0.5` | Threshold for significant extrema in the first derivative relative to background derivative noise. | Lower values call more tails. Higher values reduce false-positive tails but can miss weak tails. | Positive numeric; common range `0.3` to `1.5`. |

## Result-Extraction and Overlap-Filtering Parameters

These parameters are defined in the Workflow Configuration section of `result_extract.wl`.

| Paper concept or symbol | Code variable | Default | Meaning | Adjustment effect | Recommended user range |
|---|---:|---:|---|---|---|
| Movement summary window | `windowSize` | `6` frames | Window length for short-term KT and sister movement SD metrics. | Smaller windows capture short events but are noisier. Larger windows are smoother but less time-local. | Integer `>= 2`; choose based on movie frame rate. |
| Overlap intensity separation, `p` | `overlapSeparationIntensityThreshold` | `1.7` | Required ratio between high- and low-intensity clusters before overlap filtering is applied. | Lower values flag more possible overlaps and increase sensitivity. Higher values are stricter and reduce false positives. | Paper-defined range `1 < p < 2`; start with `1.7`. |
| Overlap pixel-area threshold, `q` | `overlapSeparationPixelThreshold` | `60` pixels | Minimum segmented area for a high-intensity timepoint to be called a likely merged-window overlap. | Lower values remove more frames. Higher values keep more frames but may miss real overlaps. | Positive integer. Tune for pixel size and expected KT area. |
| Parallel tracking-index mapping | `useParallelTrackingIndexMap` | `True` | Enables block-level parallel mapping between KT tracking indices and movie frames. | `True` improves speed on large datasets. `False` is easier to debug and avoids parallel overhead. | `True` or `False`. |
| Maximum subkernels for result extraction | `maxParallelKernels` | `8` | Upper limit on kernels used by tracking-index mapping. | Higher can improve speed but uses more memory and CPU. | Integer `1` to available CPU cores. Start with `4` to `8`. |
| Per-block tracking-index timeout | `trackingIndexTimeoutSeconds` | `300` seconds | Time limit for tracking-index mapping blocks. | Higher allows slow blocks to complete. Lower fails faster if input structure is problematic. | Positive integer. |
| Close newly launched kernels | `closeLaunchedParallelKernels` | `True` | Closes only subkernels started by this workflow after parallel mapping. | Keep `True` for routine use. Set `False` only when intentionally reusing kernels in the same session. | `True` or `False`. |

## K-K Geometry and Rotation Options

These options belong to `runKKCalculationAndRotation` in `KKCalculationAndRotation.wl`.

| Option | Default | Meaning | Adjustment effect | Recommended user range |
|---|---:|---|---|---|
| `"ChannelCount"` | `1` | Number of channel output folders to prepare, usually `1` or `2`. | Set to `2` for two-channel KT experiments. | Positive integer, usually `1` or `2`. |
| `"RunRotation"` | `True` | Whether to rotate KT image movies after K-K angle calculation. | `False` computes tables only. `True` also exports rotated central-z projections. | `True` or `False`. |
| `"ImageChannel"` | `1` | Image channel selected from multi-channel TIFFs for rotation/export. | Use `1` for Ch1, `2` for Ch2, etc. | Integer between `1` and the number of image channels. |
| `"CropFraction"` | `0.7` | Fraction of rotated image retained after rotation. | Lower values crop more black edge after rotation. Higher values retain more field of view but may include black borders. | Numeric `0 < value <= 1`; typical range `0.6` to `1.0`. |
| `"ExportCentralStack"` | `True` | Whether to export the selected central-z stack as `(+-1).tif`. | Keep `True` if downstream inspection or archiving needs the selected central stack. | `True` or `False`. |
| `"Parallel"` | `False` | Whether to process cells and rotation jobs through `ParallelMap`. | `True` can speed up large datasets. `False` is safer for first runs and debugging. | `True` or `False`. |
| `"Kernels"` | `Automatic` | Number of parallel kernels when `"Parallel" -> True`. | Higher values increase throughput but consume more memory. | `Automatic` or positive integer. Start with `4`. |

`centralZRange[slices]` selects the central z-slices used for `(+-1)` projection. For a 5-slice stack it selects 3 central slices. This follows the paper default `Nz = 3` when available. Change this helper only if the acquisition z-stack or biological signal requires a different z-range.

## Ch1/Ch2 ZNCC Registration Parameters

The Ch1/Ch2 vector calculation uses `shiftVector[c1Image, c2Image, maxShift_:3, blurSigma_:1, cropMargin_:Automatic]` in `result_extract.wl`.

| Paper concept | Code variable or output | Default | Meaning | Adjustment effect | Recommended user range |
|---|---:|---:|---|---|---|
| Coarse search window | `maxShift` | `3` pixels | Maximum integer-pixel shift searched in X and Y before subpixel refinement. | Larger values handle bigger channel offsets but increase search area and risk incorrect matches. | Positive integer. Use a value slightly larger than expected channel registration error. |
| Gaussian smoothing before ZNCC | `blurSigma` | `1` pixel | Smooths both channels before ZNCC scoring. | Larger values reduce pixel noise but can blur small offsets. `0` disables smoothing. | Numeric `>= 0`; typical range `0` to `2`. |
| Edge crop for scoring | `cropMargin` | `Automatic` | Removes edge regions affected by shifting before computing ZNCC. | Larger margins reduce padding artifacts but use fewer pixels. | `Automatic` or non-negative integer. |
| ZNCC score | `bestScore`, exported as `fitting score` | No fixed default | Similarity score at the selected Ch1/Ch2 shift. | Low scores indicate poor registration or unsuitable images. | Inspect visually; no universal cutoff. |
| Protein1-to-protein2 vector | `{xShift, yShift}` columns | No fixed default | Estimated displacement between Ch1 and Ch2 in pixels. Convert with `pixelsize` if reporting physical units. | Sign convention follows the code output from the shift fit. Verify with overlay images before final interpretation. | Determined from data. |

This Ch1/Ch2 ZNCC fitting is not the overlap filter. It estimates channel displacement for two-channel vector analysis. Overlap filtering is performed by intensity clustering and pixel-area thresholding.

## Paper Metrics and Code Outputs

| Paper metric | Code output column or variable | Meaning and notes |
|---|---|---|
| Kinetochore length, `AUC/fmax` along the K-K axis | `width_X(AUC/max)(um)`, `widthXResult`, `generalizedFW*pixelsize` | Non-parametric length estimate using the smoothed X-axis intensity projection. |
| Kinetochore width, `AUC/fmax` perpendicular to K-K axis | `width_Y(AUC/max)(um)`, `widthYResult`, `generalizedFWR90*pixelsize` | Same AUC/fmax method applied after rotating the projection axis by 90 degrees. |
| `AUC` | `intensityIntegration`, `intensityIntegrationR90` | Area under the smoothed, background-subtracted 1D intensity profile. |
| `fmax` | `mainPeakInProjection[[2]]`, `mainPeakInProjectionR90[[2]]` | Primary peak intensity in the smoothed 1D profile. |
| Segmented object area | `ktArea(pixels)` | Pixel area of the selected main KT mask before boundary dilation. |
| Total integrated intensity | `totalIntensity` | Background-corrected intensity inside the analyzed KT signal crop. |
| Asymmetry index | `asymmetryResult`, internal `asymetryRatio` | Signed left/right imbalance along X. In code, the sign follows the implemented right-minus-left convention; verify sign orientation before manuscript-level interpretation. |
| Tail call | `tail or not`, internal `ifTail` | `True` when more than two significant first-derivative extrema are detected. |
| Tail direction | `tail direction`, internal `tailDirection` | `left`, `right`, `both`, or `none`, based on positive and negative derivative extrema. |
| Tail length | `tail length(um)`, internal `tailLength*pixelsize` | Side-specific AUC/fmax value converted to micrometers. |
| 1D multimodality | `Xproj_peak number`, `Yproj_peak number` | Number of accepted peaks in X and Y intensity projections. |
| 2D multimodality | `number of 2D peaks` | Number of accepted local maxima in the 2D image after intensity, ratio, and boundary thresholds. |
| Secondary-to-primary peak ratio | `ratio 2nd/1st peak` | Background-corrected intensity ratio of the second 2D peak to the primary 2D peak. |
| 2D peak separation vector | `2D peak dist X(um)`, `2D peak dist Y(um)` | X/Y distance between the two strongest accepted 2D peaks. |
| K-K distance | `(number)kkDistance*.csv`, `(number)kkDistanceWithFramesNumberOnly*.csv`, `meanKKDistance[]`, `sdKKDistance[]` | Sister KT distance from TrackMate coordinates. |
| K-K angle | `kkAngle*.csv`, `kkAngleWithFrame*.csv` | Angle used to rotate KT images to the K-K/force axis. |
| KT movement | `movementDisplacement-*`, `ktMeanSpeed[]`, `ktSDSpeed[]`, `sisterMeanSpeed[]`, `sisterSDSpeed[]` | Frame-to-frame displacement and movement summaries. |
| Short-window positional SD | `windowSD_Minimum`, `sisterMovingWindowSD2DMinimum`, `windowSize(frames)` | Minimum local XY positional SD over `windowSize` frames. |
| Overlap intensity separation | `ch1IntensitySeparationRate`, `ch2IntensitySeparationRate` | Ratio between high- and low-intensity clusters within one KT track. |
| Overlap cluster label | `ch1Classifier`, `ch2Classifier` | Cluster identity after sorting low-intensity group as group 1 and high-intensity group as group 2. |
| Overlap area separation | `ch1AreaSeparationRate`, `ch2AreaSeparationRate` | Ratio between area statistics for the clustered groups. |
| Overlap preselection flag | `PreSelect` | `1` marks a likely merged-window overlap frame; `0` keeps the frame. |

## Function Reference

| Function | File | Use |
|---|---|---|
| `runSingleImageAnalysis[file_:Automatic, cropFraction_:0.7]` | `singleImageAnalysis.wl` | Runs analysis on one selected or specified image and returns `{metrics, plots}`. |
| `runAndDisplaySingleImageAnalysis[file_:Automatic, cropFraction_:1]` | `singleImageAnalysis.wl` | Runs one-image analysis and displays a labeled metrics table plus inspection plots. |
| `runMainBatchAnalysis[rootDir_:Automatic, OptionsPattern[]]` | `mainprogram.wl` | Finds KT `(+-1)` folders and runs batch image analysis. |
| `runKKCalculationAndRotation[rootDir_:Automatic, OptionsPattern[]]` | `KKCalculationAndRotation.wl` | Computes K-K geometry and optionally exports rotated central-z KT projections. |
| `shiftVector[c1Image, c2Image, maxShift_:3, blurSigma_:1, cropMargin_:Automatic]` | `result_extract.wl` | Estimates Ch1/Ch2 displacement by ZNCC registration. |
| `clusterKTIntensity[singleKTDataset]` | `result_extract.wl` | Computes intensity-cluster and area features used for overlap preselection. |

## Batch Options in `mainprogram.wl`

| Option | Default | Meaning | Recommended use |
|---|---:|---|---|
| `"Kernels"` | `Automatic`, capped near available CPU count | Number of subkernels for batch analysis. | Use `1` for debugging; `4` to `6` for routine runs if memory allows. |
| `"ChunkSize"` | `Automatic`, `4*"Kernels"` | Number of KT folders per processing chunk. | Keep `Automatic` unless memory pressure or long chunks are a problem. |
| `"Resume"` | `True` | Skips folders already listed in `finished.csv`. | Keep `True` for large datasets. |
| `"ExportPlots"` | `True` | Exports inspection plots for each KT image. | Keep `True` for QC; set `False` only for fast reprocessing after validation. |
| `"PerImageTimeout"` | `120` seconds | Time limit for one image. | Increase for very large images or slow machines. |
| `"PerImageMemoryLimit"` | `2*10^9` bytes | Memory limit for one image calculation. | Increase only if valid images fail due to memory limit. |

## Practical Tuning Order

1. Set `pixelsize` from microscope calibration.
2. Use `singleImageAnalysis.wl` on representative images.
3. Tune segmentation first: `backgroundSeprationFactor`, `smallNoiseComponentSize`, `boundaryDilation`.
4. Tune peak detection next: `peakIntensityThreasholdFactor`, `peakRatioThreashold`.
5. Tune tail detection with derivative plots: `lowpassThreashold`, `d1LowpassThreashold`, `d1PeakBackgroundThreasholdRatio`.
6. Run a small batch and inspect CSV plus plots.
7. Tune overlap filtering after batch extraction: `overlapSeparationIntensityThreshold`, `overlapSeparationPixelThreshold`.
8. Only after QC is stable, process the full dataset with parallel execution.

## Important Conventions

- The K-K axis is treated as the X/force axis after rotation.
- `width_X(AUC/max)` corresponds to kinetochore length in the paper.
- `width_Y(AUC/max)` corresponds to kinetochore width in the paper.
- `Ch1/Ch2 Shift Fitting` refers to ZNCC channel registration for two-color vector analysis.
- `Kinetochore Merged-Window Detection` refers to the overlap filter based on intensity clustering and segmented area.
- Several code identifiers preserve historical spellings such as `Threashold`, `Sepration`, and `asymetry`. Do not rename them unless all dependent code is updated together.
