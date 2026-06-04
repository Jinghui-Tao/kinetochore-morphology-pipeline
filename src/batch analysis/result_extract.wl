(* ::Package:: *)



(* ::Title:: *)
(* Kinetochore Result Extraction Pipeline *)


(*
Purpose
  Combine per-kinetochore analysis CSV files, TrackMate exports, sister-pair
  metadata, movement summaries, and target/background KT signal overlap
  preselection output.

*)


(* ::Section:: *)
(* Robust I/O and Header Utilities *)


ClearAll[
  standardWidthXName, standardWidthYName, widthXAliases, widthYAliases,
  number2DPeaksAliases,
  stripDuplicateHeaderSuffix, normalizeHeaderName, normalizeHeaderKey,
  normalizeHeaderRow, normalizeImportedTable,
  workflowWarnings, workflowErrors, logMessage, recordWarning, recordError,
  safeImportRaw, safeImportTable, safeExportCSV, columnPositionsByName,
  columnValuesByName, numberStringPattern, toNumericOrMissing, numericValueQ,
  numericColumnValues, safeMean, safeStandardDeviation, widthXValues,
  titleColumnValues, numericTitleColumnValues, numericTitleColumnPairs,
  safeFractionPercent,
  safeSeparationRatio, safeMaxValue,
  widthYValues, widthXSlope, configuredChannelNumber, channelNumber,
  doubleChannelEnabled, useParallelTrackingIndexMap,
  useParallelZNCCFitting, maxParallelKernels, maxParallelZNCCKernels,
  trackingIndexTimeoutSeconds, znccBatchProgressInterval,
  exportZNCCFittingResults, reuseExistingZNCCFittingResults,
  znccPixelSizeUm, znccMaxShift, znccBlurSigma,
  znccFittingTimeoutSeconds, znccRefinementTimeoutSeconds,
  znccPreviewTimeoutSeconds,
  closeLaunchedParallelKernels, withManagedParallelKernels,
  makeUniqueHeaders, tableRowsToAssociations, displayTablePreview,
  rowCount, allKTResultTable, compiledResultTable, ktOverlapInspectionTable, matchValueQ,
  displayCellValue,
  rowValueByHeader, channelSplitPosition, channelBlockRange, channelBlockValue, channelImageInfo,
  metadataRowMatchQ, selectCompiledRowsByMetadata,
  channelRegistrationTable, displayZNCCVectorTable,
  ktOverlapCandidateRows, ktOverlapInspectionScore, sortedKTOverlapInspectionRows,
  ktAllInOnePlotPath, ktOriginalImagePath, plotImageCell,
  channelOverlayImageCell,
  finiteNumericQ, parabolicSubpixelOffset, existingAllKTResultTable,
  znccRowKeyString,
  storedZNCCFitFromRow, storedZNCCFitLookup,
  shiftVectorFitOnly, fitZNCCForRow, fitZNCCRows,
  znccFittingVectorUm, appendZNCCFittingColumns,
  compiledResultsBase, compiledResultsWithZNCC,
  measurementAssociation, displayIntermediateAnalysisRows,
  displayIntermediateAnalysisPreview, displayIntermediateAnalysisAt,
  displayIntermediateAnalysisAtRow,
  displayIntermediateAnalysisSelector, displayIntermediateAnalysisControl,
  displayKTOverlapInspectionPreview,
  displayKTOverlapCandidatePreview,
  displayResultExtractionReport,
  displayZNCCChannelRegistrationPreview,
  displayZNCCChannelRegistrationControl
];

workflowWarnings = {};
workflowErrors = {};

logMessage[message_] := Print[
  DateString[{"ISODate", " ", "Time"}] <> "  " <> ToString[message]
];

recordWarning[tag_String, detail_: <||>] := (
  AppendTo[workflowWarnings, <|"Tag" -> tag, "Detail" -> detail|>];
  Missing["Warning", tag]
);

recordError[tag_String, detail_: <||>] := (
  AppendTo[workflowErrors, <|"Tag" -> tag, "Detail" -> detail|>];
  Failure[tag, <|"Detail" -> detail|>]
);

standardWidthXName = "width_X(AUC/max)(um)";
standardWidthYName = "width_Y(AUC/max)(um)";

widthXAliases = DeleteDuplicates@{
  standardWidthXName,
  "width_X(AUC/max)(\[Micro]m)",
  "width_X(AUC/max)(\[Mu]m)",
  "generalizedFWX(um)",
  "generalizedFWX(\[Micro]m)",
  "generalizedFWX(\[Mu]m)",
  "meanareawidthX",
  "X_AUC/Peak (um)",
  "X_AUC/Peak (\[Micro]m)",
  "X_AUC/Peak (\[Mu]m)"
};

widthYAliases = DeleteDuplicates@{
  standardWidthYName,
  "width_Y(AUC/max)(\[Micro]m)",
  "width_Y(AUC/max)(\[Mu]m)",
  "generalizedFWY(um)",
  "generalizedFWY(\[Micro]m)",
  "generalizedFWY(\[Mu]m)",
  "meanareawidthY",
  "Y_AUC/Peak (um)",
  "Y_AUC/Peak (\[Micro]m)",
  "Y_AUC/Peak (\[Mu]m)"
};

number2DPeaksAliases = DeleteDuplicates@{
  "number of 2D peaks",
  "number of 2d peaks",
  "number 2D peaks",
  "number 2d peaks"
};

stripDuplicateHeaderSuffix[name_String] := StringReplace[
  name,
  RegularExpression["(?:\\s*\\(\\d+\\)|\\.\\d+|_\\d+|-\\d+)$"] -> ""
];
stripDuplicateHeaderSuffix[name_] := name;

normalizeHeaderName[name_String] := Module[{s = name, base},
  base = stripDuplicateHeaderSuffix[s];
  Which[
    MemberQ[ToString /@ widthXAliases, s] || MemberQ[ToString /@ widthXAliases, base],
      standardWidthXName,
    MemberQ[ToString /@ widthYAliases, s] || MemberQ[ToString /@ widthYAliases, base],
      standardWidthYName,
    True,
      base
  ]
];
normalizeHeaderName[name_] := name;

normalizeHeaderKey[name_] := ToString@normalizeHeaderName[name];

normalizeHeaderRow[row_List] := normalizeHeaderName /@ row;
normalizeHeaderRow[row_] := row;

normalizeImportedTable[data_List] /; Length[data] > 0 := ReplacePart[data, 1 -> normalizeHeaderRow[First[data]]];
normalizeImportedTable[data_] := data;

safeImportRaw[path_String] := Module[{data},
  If[!FileExistsQ[path], Return[recordError["MissingFile", <|"Path" -> path|>]]];
  data = Quiet@Check[Import[path], recordError["ImportFailed", <|"Path" -> path|>]];
  data
];
safeImportRaw[failure_?FailureQ] := failure;

safeImportTable[path_String] := Module[{data},
  data = safeImportRaw[path];
  If[FailureQ[data], data, normalizeImportedTable[data]]
];
safeImportTable[failure_?FailureQ] := failure;

safeExportCSV[path_String, data_] := Module[{dir = DirectoryName[path]},
  If[StringQ[dir] && dir =!= "" && !DirectoryQ[dir], CreateDirectory[dir, CreateIntermediateDirectories -> True]];
  Quiet@Check[Export[path, data, "CSV"], Failure["ExportFailed", <|"Path" -> path|>]]
];

columnPositionsByName[headers_List, aliases_List] := Module[
  {normalizedHeaders, normalizedAliases},
  normalizedHeaders = normalizeHeaderKey /@ headers;
  normalizedAliases = DeleteDuplicates@(normalizeHeaderKey /@ aliases);
  Flatten@Position[normalizedHeaders, Alternatives @@ normalizedAliases]
];

columnValuesByName[data_List, aliases_List, occurrence_: 1] := Module[
  {normalizedData, headers, rows, positions, column},
  If[Length[data] < 2, Return[{}]];
  normalizedData = normalizeImportedTable[data];
  headers = First[normalizedData];
  rows = Rest[normalizedData];
  positions = columnPositionsByName[headers, aliases];
  If[Length[positions] < occurrence, Return[{}]];
  column = positions[[occurrence]];
  Cases[rows, row_List /; Length[row] >= column :> row[[column]]]
];

numberStringPattern = RegularExpression[
  "^[+-]?(?:(?:\\d+\\.?\\d*)|(?:\\.\\d+))(?:[eE][+-]?\\d+)?$"
];

toNumericOrMissing[value_] := Module[{s},
  If[NumberQ[value], Return[value]];
  s = StringTrim@ToString[value];
  If[s == "" || s == "NA" || s == "NaN", Return[Missing["NotNumeric"]]];
  If[
    StringMatchQ[s, numberStringPattern],
    Quiet@Check[ToExpression[s], Missing["NotNumeric"]],
    Missing["NotNumeric"]
  ]
];

numericValueQ[value_] := NumberQ[toNumericOrMissing[value]];

rowValueByHeader[headers_List, row_List, aliases_List, occurrence_: 1] := Module[
  {positions, column},
  positions = columnPositionsByName[headers, aliases];
  If[Length[positions] < occurrence, Return[Missing["ColumnNotFound"]]];
  column = positions[[occurrence]];
  If[Length[row] >= column, row[[column]], Missing["NotAvailable"]]
];

channelSplitPosition[headers_List] := Module[
  {textHeaders, directSplit, conditionColumns},
  textHeaders = ToString /@ headers;
  directSplit = Flatten@Position[textHeaders, "//"];
  If[Length[directSplit] >= 1, Return[First[directSplit]]];
  conditionColumns = columnPositionsByName[headers, {"condition"}];
  If[
    Length[conditionColumns] >= 2,
    conditionColumns[[2]] - 1,
    Missing["ChannelSplitNotFound"]
  ]
];

channelBlockRange[headers_List, channel_: 1] := Module[
  {pathColumns, split, start, stop},
  pathColumns = columnPositionsByName[headers, {"KT trackingPureData path"}];
  Which[
    channel === 1 && Length[pathColumns] >= 1,
      Return[{1, pathColumns[[1]]}],
    channel === 2 && Length[pathColumns] >= 2,
      Return[{pathColumns[[1]] + 1, pathColumns[[2]]}]
  ];
  split = channelSplitPosition[headers];
  Which[
    channel === 1 && IntegerQ[split],
      start = 1;
      stop = Max[1, split - 1],
    channel === 1,
      start = 1;
      stop = Length[headers],
    channel === 2 && IntegerQ[split],
      start = split + 1;
      stop = Length[headers],
    True,
      Return[Missing["ChannelBlockNotFound"]]
  ];
  {start, stop}
];

channelBlockValue[headers_List, row_List, aliases_List, channel_: 1] := Module[
  {range, start, stop, localHeaders, localPositions, column},
  range = channelBlockRange[headers, channel];
  If[!ListQ[range], Return[Missing["ChannelBlockNotFound"]]];
  {start, stop} = range;
  localHeaders = headers[[start ;; stop]];
  localPositions = columnPositionsByName[localHeaders, aliases];
  If[Length[localPositions] < 1, Return[Missing["ColumnNotFound"]]];
  column = start + First[localPositions] - 1;
  If[Length[row] >= column, row[[column]], Missing["NotAvailable"]]
];

channelImageInfo[headers_List, row_List, channel_: 1] := Module[
  {path, index},
  path = channelBlockValue[headers, row, {"KT trackingPureData path"}, channel];
  index = toNumericOrMissing[channelBlockValue[headers, row, {"index"}, channel]];
  If[StringQ[path] && NumericQ[index], {path, index}, Missing["ImageInfoNotFound"]]
];

numericColumnValues[data_List, aliases_List, occurrence_: 1] := Cases[toNumericOrMissing /@ columnValuesByName[data, aliases, occurrence], _?NumericQ];
safeMean[values_List] := If[Length[values] > 0, Mean[values], Missing["NoNumericData"]];
safeStandardDeviation[values_List] := If[Length[values] > 1, StandardDeviation[values], Missing["InsufficientData"]];

titleColumnValues[data_List, aliases_List, occurrence_: 1] := Module[{positions, column},
  positions = columnPositionsByName[title, aliases];
  If[Length[positions] < occurrence, Return[{}]];
  column = positions[[occurrence]];
  Cases[data, row_List /; Length[row] >= column :> row[[column]]]
];

numericTitleColumnValues[data_List, aliases_List, occurrence_: 1] :=
  Cases[toNumericOrMissing /@ titleColumnValues[data, aliases, occurrence], _?NumericQ];

numericTitleColumnPairs[data_List, aliases1_List, aliases2_List, occurrence_: 1] := Module[
  {positions1, positions2, column1, column2, rawPairs},
  positions1 = columnPositionsByName[title, aliases1];
  positions2 = columnPositionsByName[title, aliases2];
  If[Length[positions1] < occurrence || Length[positions2] < occurrence, Return[{}]];
  column1 = positions1[[occurrence]];
  column2 = positions2[[occurrence]];
  rawPairs = Cases[
    data,
    row_List /; Length[row] >= Max[column1, column2] :>
      {toNumericOrMissing[row[[column1]]], toNumericOrMissing[row[[column2]]]}
  ];
  Cases[rawPairs, {_?NumericQ, _?NumericQ}]
];

widthXValues[data_, occurrence_] := numericTitleColumnValues[data, widthXAliases, occurrence];
widthYValues[data_, occurrence_] := numericTitleColumnValues[data, widthYAliases, occurrence];

safeFractionPercent[values_List, predicate_] := Module[{nums, denom},
  nums = Cases[toNumericOrMissing /@ values, _?NumericQ];
  denom = Length[Select[nums, # >= 0&]];
  If[denom == 0, Return[Missing["NoNumericData"]]];
  PercentForm[N[Length[Select[nums, predicate]]/denom]]
];

safeSeparationRatio[values_List] := Module[{nums, mn, mx},
  nums = Cases[toNumericOrMissing /@ Flatten[{values}], _?NumericQ];
  If[Length[nums] == 0, Return[0]];
  {mn, mx} = MinMax[nums];
  Which[
    mn > 0, mx/mn,
    mx == 0, 0,
    True, Infinity
  ]
];

safeMaxValue[values_List] := Module[{nums},
  nums = Cases[toNumericOrMissing /@ Flatten[{values}], _?NumericQ];
  If[Length[nums] == 0, 0, Max[nums]]
];

widthXSlope[data_, occurrence_] := Module[{pairs},
  pairs = numericTitleColumnPairs[data, widthXAliases, widthYAliases, occurrence];
  If[Length[pairs] > 1,
    Quiet@Check[LinearModelFit[pairs, {1, x}, x]["BestFitParameters"][[2]], Missing["FitFailed"]],
    Missing["InsufficientData"]
  ]
];


(* ::Section:: *)
(* Workflow Configuration *)


(*
The workflow extracts individually analyzed kinetochore data, movement metrics,
and target/background KT-overlap inspection tables.
*)
(*
Tunable parameters:
*)
configuredChannelNumber = 2;
(* User setting. Supported values:
   1 -> single-channel dataset; runs Ch1 extraction, movement summaries, and
        Ch1 target/background KT signal overlap filtering.
   2 -> double-channel dataset; additionally enables Ch1/Ch2 vector fitting. *)

windowSize = 6;
(* Frames. Window length for short-term SD analysis. Use an integer. *)

overlapSeparationIntensityThreshold = 1.7;
(* Total-intensity threshold for target/background KT signal overlap
   preselection. Lower values detect more possible overlap frames but may
   increase false positives. Recommended default: 1.7. *)

overlapSeparationPixelThreshold = 60;
(* Pixel-area threshold for target/background KT signal overlap preselection.
   Lower values detect more possible overlap frames but may increase false
   positives. Tune for movie pixel size. *)

exportZNCCFittingResults = True;
(* When True, compute and export Ch1/Ch2 ZNCC fitting vector columns in the
   compiled results table. This calculation does not read, write, filter by,
   or depend on the KT-overlap PreSelect flag. *)

reuseExistingZNCCFittingResults = True;
(* When True, reuse stored Ch1/Ch2 ZNCC vector columns from an existing allKT.csv
   in the selected result folder before computing missing rows. *)

znccPixelSizeUm = 0.046*2;
(* Micrometers per camera pixel after acquisition binning. This matches the
   default pixel size used by the image-analysis step. *)

znccMaxShift = 3;
(* Pixels. Search radius for Ch1/Ch2 ZNCC shift fitting. *)

znccBlurSigma = 1;
(* Gaussian smoothing sigma applied before ZNCC fitting. Use 0 to disable. *)

znccFittingTimeoutSeconds = 30;
(* Maximum time allowed for each row-level ZNCC fit. Timed-out rows export NA. *)

znccRefinementTimeoutSeconds = 20;
(* Maximum time allowed for NMaximize subpixel refinement in each fit. *)

znccPreviewTimeoutSeconds = 30;
(* Maximum time allowed for one interactive ZNCC preview. *)

useParallelZNCCFitting = True;
maxParallelZNCCKernels = 4;
znccBatchProgressInterval = 1;
(* Batch ZNCC export policy. Batch export computes only the subpixel vector and
   score; overlay images are generated only by the interactive preview. *)

useParallelTrackingIndexMap = False;
maxParallelKernels = 8;
trackingIndexTimeoutSeconds = 300;
closeLaunchedParallelKernels = False;
(* Parallel policy. Leave launched subkernels open after ParallelMap. Closing
   subkernels immediately after a large image-fitting job can leave the main
   kernel waiting on stale links after the progress dialog has disappeared. *)

channelNumber = Module[{n = configuredChannelNumber},
  Which[
    IntegerQ[n] && MemberQ[{1, 2}, n], n,
    NumericQ[n] && IntegerQ[Round[n]] && MemberQ[{1, 2}, Round[n]], Round[n],
    True,
      recordWarning[
        "UnsupportedChannelNumber",
        <|"Configured" -> configuredChannelNumber, "Used" -> 1|>
      ];
      1
  ]
];

doubleChannelEnabled = channelNumber == 2; (* Internal flag derived from configuredChannelNumber. *)


(* ::Section:: *)
(* KT Data Collection *)


(*
Part 1: combine one- or two-channel analysis data with KK-distance data.
*)

(*
Select the folder that contains all cell folders.
*)
cellSetDir = SystemDialogInput[
  "Directory",
  WindowTitle -> "Please select the folder of all cells"
];


cellsDirTable={Select[FileNames["*",cellSetDir],DirectoryQ[#]&&!StringEndsQ[FileNameTake[#],"Overlaps"]&]};


allCellsTable=Flatten[cellsDirTable];


allChannelsTable=Table[Table[Select[FileNames["*",cellsDirTable[[k]][[i]]],DirectoryQ],{i,Length[cellsDirTable[[k]]]}],{k,Length[cellsDirTable]}];


sisterKTPairsTable=Table[Table[Table[Select[FileNames["*",allChannelsTable[[l]][[j]][[i]]],DirectoryQ],{i,Length[allChannelsTable[[l]][[j]]]}],{j,Length[allChannelsTable[[l]]]}],{l,Length[allChannelsTable]}];


toImageDirFunction=Function[dir,Select[FileNames["*",dir],DirectoryQ[#]&&!StringEndsQ[#,"tifcroppedimages"]&]];


allKTDirs=Map[toImageDirFunction,sisterKTPairsTable,{4}];


stringEndSelect[list_,end_,notend_:"notend"]:=Select[list,StringEndsQ[#,end]&&!StringEndsQ[#,notend]&]; (*Select strings that end with "end" but not with "notend".*)


allKTDataPath=Map[If[Length[FileNames[{"ch*0.csv","ch*1.csv","ch*2.csv","ch*3.csv","ch*4.csv","ch*5.csv","ch*6.csv","ch*7.csv","ch*8.csv","ch*9.csv","ch*0..csv","ch*1..csv","ch*2..csv","ch*3..csv","ch*4..csv","ch*5..csv","ch*6..csv","ch*7..csv","ch*8..csv","ch*9..csv"},#]]!=0,FileNames[{"ch*0.csv","ch*1.csv","ch*2.csv","ch*3.csv","ch*4.csv","ch*5.csv","ch*6.csv","ch*7.csv","ch*8.csv","ch*9.csv","ch*0..csv","ch*1..csv","ch*2..csv","ch*3..csv","ch*4..csv","ch*5..csv","ch*6..csv","ch*7..csv","ch*8..csv","ch*9..csv"},#][[1]],Null]&,Map[stringEndSelect[#,"(+-1)"]&,Map[Flatten[#]&,Map[toImageDirFunction,allKTDirs,{5}],{4}],{4}],{5}]/. {Null,rest___}:>{rest};
(* Collect per-KT CSV paths from the (+-1) analysis folders. *)


allKTDataPathClean=DeleteCases[allKTDataPath,Null,Infinity];


toKKDistanceFunction=Function[dir,Select[FileNames["*",dir],StringContainsQ[#,"(number)kkDistanceWithFramesNumberOnly"]&]];


kkDistanceFilePaths=Map[If[Length[toKKDistanceFunction[#]]!=0,{toKKDistanceFunction[#][[1]],toKKDistanceFunction[#][[1]]},Null]&,sisterKTPairsTable,{4}];


kkDistanceData=Map[ToExpression,Map[Transpose[Import[#]][[1]]&,kkDistanceFilePaths,{5}],{6}];


allKTDataDims=Commonest[ToExpression/@(Flatten[Map[Length,Map[If[!#===Null,Import[#],{}]&,Flatten[allKTDataPathClean]],{2}]])];


ktDataDim=If[Length[allKTDataDims]==1,allKTDataDims[[1]],Throw[$Failed]];


fullDimCheck[list_]:=Length[list]==ktDataDim ;


fullDataCheck[list_] := fullDimCheck[list] && And @@ (numericValueQ /@ list[[{3, 2, 4}]]);


doubleChannelConjugate[x_]:=If[x==1,2,1];(*Switch between Ch1 and Ch2.*)


pathToTrackID[path_]:=FileNameSplit[path][[-3]];


pathToLocationFile[path_]:=FileNames["export.csv",FileNameDrop[path,-5]][[1]];


pathToKKFile[path_]:=FileNames["*kkDistanceWithFramesNumberOnly*",FileNameDrop[path,-3]][[1]];


trackableKKFrames[path_]:=ToExpression/@(Transpose[Import[pathToKKFile[path]]][[2]]);


ktIndexToFrames[path_,index_]:=ToExpression[If[index<=Length[trackableKKFrames[path]],Sort[ToExpression/@trackableKKFrames[path]][[index]]]];


(* Map tracking indices to movie frames without shared mutable state. *)
Clear[m, n, l, i, j, k];
ktIndexToFramesList = allKTDataPathClean;
kernelsUse = Max[1, Min[$ProcessorCount - 1, maxParallelKernels]];

mapTrackingIndexBlock[{n_, m_}] := Module[{localBlock},
  localBlock = Quiet@Check[
    TimeConstrained[
      Table[
        Table[
          Table[
            If[
              allKTDataPathClean[[n, m, l, j]] =!= Null && allKTDataPathClean[[n, m, l, j]] =!= {} &&
              allKTDataPathClean[[n, m, l, j, i]] =!= {} && allKTDataPathClean[[n, m, l, j, i]] =!= Null,
              Quiet@Check[
                Sort[trackableKKFrames[allKTDataPathClean[[n, m, l, j, i]]]],
                Null
              ],
              Null
            ],
            {i, Length[allKTDataPathClean[[n, m, l, j]]]}
          ],
          {j, Length[allKTDataPathClean[[n, m, l]]]}
        ],
        {l, channelNumber}
      ],
      trackingIndexTimeoutSeconds,
      $Aborted
    ],
    $Failed
  ];
  <|
    "Index" -> {n, m},
    "Status" -> Which[
      localBlock === $Failed, "Failed",
      localBlock === $Aborted, "TimedOut",
      True, "OK"
    ],
    "Frames" -> localBlock
  |>
];

SetAttributes[withManagedParallelKernels, HoldRest];
withManagedParallelKernels[desired_Integer?Positive, expr_] := Module[
  {before, needed, result},
  before = Kernels[];
  needed = Max[0, desired - Length[before]];
  If[needed > 0, Quiet@Check[LaunchKernels[needed], {}]];
  result = Quiet@Check[expr, $Failed];
  If[
    TrueQ[closeLaunchedParallelKernels],
    Scan[Quiet@Check[CloseKernels[#], Null] &, Complement[Kernels[], before]]
  ];
  result
];

mapTrackingIndicesToFrames[parallel_: True] := Module[{blocks, mapped},
  blocks = Flatten[Table[{n, m}, {n, Length[allKTDataPathClean]}, {m, Length[allKTDataPathClean[[n]]]}], 1];
  If[blocks === {}, Return[{}]];
  If[TrueQ[parallel] && kernelsUse > 1 && Length[blocks] > 1,
    mapped = withManagedParallelKernels[
      kernelsUse,
      If[KeyExistsQ[Association@SystemOptions[], "EvaluateInFrontEnd"], SetSystemOptions["EvaluateInFrontEnd" -> False]];
      DistributeDefinitions[
        allKTDataPathClean, channelNumber, ktIndexToFrames, trackableKKFrames,
        pathToKKFile, trackingIndexTimeoutSeconds, mapTrackingIndexBlock
      ];
      ParallelMap[mapTrackingIndexBlock, blocks, Method -> "CoarsestGrained"]
    ];
    If[mapped === $Failed,
      recordWarning["ParallelMapFailed", <|"Fallback" -> "Serial mapping"|>];
      mapped = mapTrackingIndexBlock /@ blocks
    ],
    mapped = mapTrackingIndexBlock /@ blocks
  ];
  Do[
    If[AssociationQ[item] && item["Status"] === "OK",
      ktIndexToFramesList[[Sequence @@ item["Index"]]] = item["Frames"]
      ,
      recordWarning["TrackingIndexBlockFailed", item]
    ],
    {item, mapped}
  ];
  mapped
];

logMessage["Tracking-index map: start."];
trackingFrameMapResults = mapTrackingIndicesToFrames[useParallelTrackingIndexMap];
logMessage["Tracking-index map: done."];


pathToKTNames[ktdatapath_]:=StringSplit[ktdatapath,{".",$PathnameSeparator}][[-2]];


pathToKTPeaks[ktdatapath_]:=FileNames["*peaks.csv",DirectoryName[ktdatapath]][[1]];


ktPathToTrackableFrames[path_,list_]:=Table[ToExpression[ktIndexToFrames[path,i]],{i,list}];


ktIDToFramesFunction[n_,m_,l_,j_,i_]:=ktIndexToFramesList[[n,m,l,j,i]]


ifFrameIsInTrackingInKT[n_,m_,l_,j_,i_,frame_]:=MemberQ[ToExpression/@ktIDToFramesFunction[n,m,l,j,i],ToExpression[frame]]


frameToIndexAtKTID[n_, m_, l_, j_, i_, frame_] := Module[
  {frames, frameNumber},
  frames = ToExpression /@ ktIDToFramesFunction[n, m, l, j, i];
  frameNumber = ToExpression[frame];
  If[
    MemberQ[frames, frameNumber],
    FirstPosition[frames, frameNumber][[1]],
    Missing["FrameNotTracked"]
  ]
]; (* Map a movie frame to the KT tracking index. *)


mapToKTpath[n_,m_,l_,j_,i_]:=allKTDataPathClean[[n]][[m]][[l]][[j]][[i]];


mapToKTpathCh2[n_,m_,l_,j_,i_]:=allKTDataPathClean[[n]][[m]][[doubleChannelConjugate[l]]][[j]][[i]];


Clear[n,m,l,j,i]


(* Collect all KT data and KK-distance data. *)
logMessage["Compiled KT row collection: start."];
ktDataCollection=
If[doubleChannelEnabled,
Table[Table[Table[Table[Table[
If[StringQ[allKTDataPathClean[[n]][[m]][[l]][[j]][[i]]]&&StringQ[allKTDataPathClean[[n]][[m]][[doubleChannelConjugate[l]]][[j]][[i]]],

Table[Module[{timeIndex,ch2TimeIndex,timeIndexSkipTitle,ch2TimeIndexSkipTitle,ktDataCh1,ktDataCh2},
timeIndex=frameToIndexAtKTID[n,m,l,j,i,frame];
timeIndexSkipTitle=timeIndex+1;
ch2TimeIndex=frameToIndexAtKTID[n,m,doubleChannelConjugate[l],j,i,frame];
ch2TimeIndexSkipTitle=ch2TimeIndex+1;
ktDataCh1=Map[ToExpression,Import[allKTDataPathClean[[n]][[m]][[l]][[j]][[i]]]/.{Missing["NotAvailable"]->"NA"},{2}];
ktDataCh2=Map[ToExpression,Import[allKTDataPathClean[[n]][[m]][[doubleChannelConjugate[l]]][[j]][[i]]]/.{Missing["NotAvailable"]->"NA"},{2}];

If[ifFrameIsInTrackingInKT[n,m,l,j,i,frame]&&Length[ktDataCh1]>=timeIndex&&fullDataCheck[ktDataCh1[[timeIndexSkipTitle]]]&&ifFrameIsInTrackingInKT[n,m,doubleChannelConjugate[l],j,i,frame]&&Length[ktDataCh2]>=ch2TimeIndex&&fullDataCheck[ktDataCh2[[ch2TimeIndexSkipTitle]]],

Join[{n,m,l,j,i,frame,"/"},Join[ktDataCh1[[timeIndexSkipTitle]],{kkDistanceData[[n]][[m]][[l]][[j]][[i]][[ktDataCh1[[timeIndexSkipTitle]][[1]]]]},{allKTDataPathClean[[n]][[m]][[l]][[j]][[i]]}],{"//",n,m,doubleChannelConjugate[l],j,i,frame,"/"},Join[ktDataCh2[[ch2TimeIndexSkipTitle]],
{kkDistanceData[[n]][[m]][[doubleChannelConjugate[l]]][[j]][[i]][[ktDataCh2[[ch2TimeIndexSkipTitle]][[1]]]]},{allKTDataPathClean[[n]][[m]][[doubleChannelConjugate[l]]][[j]][[i]]}],{"/","/"}
]/.{Missing["NotAvailable"]->"NA"}]
]
,{frame,0,Max[Join[ToExpression/@ktIDToFramesFunction[n,m,l,j,i],{0}]]}]]
,{i,Length[allKTDataPathClean[[n]][[m]][[l]][[j]]]}],{j,Length[allKTDataPathClean[[n]][[m]][[l]]]}],{l,1}],{m,Length[allKTDataPathClean[[n]]]}],{n,Length[allKTDataPathClean]}],

Table[Table[Table[Table[Table[
If[StringQ[allKTDataPathClean[[n]][[m]][[l]][[j]][[i]]],Table[
Module[{timeIndex,timeIndexSkipTitle,ktDataCh1},
timeIndex=frameToIndexAtKTID[n,m,l,j,i,frame];
timeIndexSkipTitle=timeIndex+1;
ktDataCh1=Map[ToExpression,Import[allKTDataPathClean[[n]][[m]][[l]][[j]][[i]]]/.{Missing["NotAvailable"]->"NA"},{2}];
If[ifFrameIsInTrackingInKT[n,m,l,j,i,frame]&&Length[ktDataCh1]>=timeIndex&&fullDataCheck[ktDataCh1[[timeIndexSkipTitle]]],Join[{n,m,l,j,i,frame,"/"},Join[ktDataCh1[[timeIndexSkipTitle]],{kkDistanceData[[n]][[m]][[l]][[j]][[i]][[ktDataCh1[[timeIndexSkipTitle]][[1]]]]},{allKTDataPathClean[[n]][[m]][[l]][[j]][[i]]}],{"//",n,m,doubleChannelConjugate[l],j,i,frame,"/"},Table["No Ch2 trackingPureData",{i,25}],{"/","/"}
]/.{Missing["NotAvailable"]->"NA"}]
]
,{frame,0,Max[Join[ToExpression/@ktIDToFramesFunction[n,m,l,j,i],{0}]]}]]
,{i,Length[allKTDataPathClean[[n]][[m]][[l]][[j]]]}],{j,Length[allKTDataPathClean[[n]][[m]][[l]]]}],{l,1}],{m,Length[allKTDataPathClean[[n]]]}],{n,Length[allKTDataPathClean]}]];


titles=
If[doubleChannelEnabled,

Commonest[Select[Flatten[Table[Table[Table[Table[Table[If[StringQ[allKTDataPathClean[[n]][[m]][[l]][[j]][[i]]]&&StringQ[allKTDataPathClean[[n]][[m]][[doubleChannelConjugate[l]]][[j]][[i]]],

Join[{"condition","Cell index","Channel","KT pairs","KT","Time","/"},Join[Import[allKTDataPathClean[[n]][[m]][[l]][[j]][[i]]][[1]],{"kk dist"},{"KT trackingPureData path"}],{"//","condition","Cell index","Channel","KT pairs","KT","Time","/"},Join[Import[allKTDataPathClean[[n]][[m]][[doubleChannelConjugate[l]]][[j]][[i]]][[1]],{"kk  dist"},{"KT trackingPureData path"},{"/"},{"/"}]]

],{i,Length[allKTDataPathClean[[n]][[m]][[l]][[j]]]}],{j,Length[allKTDataPathClean[[n]][[m]][[l]]]}],{l,1}],{m,Length[allKTDataPathClean[[n]]]}],{n,Length[allKTDataPathClean]}],4],Length[#]>2&]],

Commonest[Select[Flatten[Table[Table[Table[Table[Table[If[StringQ[allKTDataPathClean[[n]][[m]][[l]][[j]][[i]]],

Join[{"condition","Cell index","Channel","KT pairs","KT","Time","/"},Join[Import[allKTDataPathClean[[n]][[m]][[l]][[j]][[i]]][[1]],{"kk dist"},{"KT trackingPureData path"}],{"//","condition","Cell index","Channel","KT pairs","KT","Time","/"},Join[Table["No Ch2 trackingPureData",{i,25}],{"kk  dist"},{"KT trackingPureData path"}]]

],{i,Length[allKTDataPathClean[[n]][[m]][[l]][[j]]]}],{j,Length[allKTDataPathClean[[n]][[m]][[l]]]}],{l,1}],{m,Length[allKTDataPathClean[[n]]]}],{n,Length[allKTDataPathClean]}],4],Length[#]>2&]]


];

title = normalizeHeaderRow[Map[If[NumberQ[#], Null, #]&, titles[[1]]]];


ktDataCollectionExport=Table[Join[{title},Flatten[i,4]],{i,ktDataCollection}];

compiledResultsBase = DeleteCases[ktDataCollectionExport[[1]], Null];
logMessage["Compiled KT row collection: done. Rows = " <> ToString[Max[0, Length[compiledResultsBase] - 1]] <> "."];

outputs = {};


(* ::Section:: *)
(* Movement Summary *)


(*
Summarize each KT's behavior over trackable frames, including width statistics,
location SD, speed metrics, KK-distance statistics, and short-window SD.
*)


(*Read and gather KT data paths and their location data paths.*)


ktDataPaths=outputs;(*Exported data path from the previous section.*)


collectionOfAllCellAllKTData=Map[Select[#,#1!={""}&]&,ktDataCollectionExport];(*Select non-empty rows.*)


columnOfKTDataPath=Flatten[Position[title,"KT trackingPureData path"]];(*Find the column index of the KT tracking/data path.*)


channelChoseOfDataPathColumn[channel_]:=columnOfKTDataPath[[channel]];
(*Column selector by channel.*)


trackingDataPath[datainput_,channel_,index_]:=Module[{trackingFileDir,trackingFilePath},
trackingFileDir=DirectoryName[DirectoryName[DirectoryName[DirectoryName[DirectoryName[AbsoluteFileName[datainput[[index]][[channelChoseOfDataPathColumn[channel]]]]]]]]];
trackingFilePath=trackingFileDir<>"export.csv"
];(*Compute the TrackMate tracking CSV path for a KT row.*)


trackingDataDir[datainput_,channel_,index_]:=Module[{trackingFileDir},
trackingFileDir=DirectoryName[DirectoryName[DirectoryName[DirectoryName[DirectoryName[AbsoluteFileName[datainput[[index]][[channelChoseOfDataPathColumn[channel]]]]]]]]];
trackingFileDir
];


allTrackingDataFilePath=Table[DeleteDuplicates[Flatten[Table[Table[trackingDataPath[conditioni,channel,i],{i,2,Length[conditioni]}],{channel,1,channelNumber}]]],{conditioni,collectionOfAllCellAllKTData}];


allTrackingDataDir=Table[DeleteDuplicates[Flatten[Table[Table[trackingDataDir[conditioni,channel,i],{i,2,Length[conditioni]}],{channel,1,channelNumber}]]],{conditioni,collectionOfAllCellAllKTData}];


(* ::Subsection:: *)
(* TrackMate Location Export *)


LocationDataCollect[trackingFileDir_] := Module[
  {
    trackingPath, pairingFilePath, exportpath1, exportpath2, trackingDataTable,
    pairingData, ktPairLabels, trackingPureData, ktTrackLabeltoID, maxID,
    movieLength, categorizedData, trackableFramesOverlapMatrix, dataOfPairs,
    trackableFramesOfPairs, pairedXMeasurements, pairedXMeasurementsWithFrames,
    pairedYMeasurements, pairedYMeasurementsWithFrames, pairedZMeasurements,
    pairedZMeasurementsWithFrames, XYZwithFrame, FunctionXYZwithFrames,
    FunctionXYZwithFramesNumberOnly, locationExportPath, exportLocation
  },


trackingPath=FileNameJoin[{trackingFileDir,"export.csv"}];
pairingFilePath=FileNameJoin[{trackingFileDir,"pairs.xlsx"}];

exportpath1=FileNameJoin[{trackingFileDir,"ch1"}];
exportpath2=FileNameJoin[{trackingFileDir,"ch2"}];
trackingDataTable=safeImportRaw[trackingPath];
If[FailureQ[trackingDataTable], Return[trackingDataTable]];
pairingData=safeImportRaw[pairingFilePath];
If[FailureQ[pairingData], Return[pairingData]];
ktPairLabels=Select[Map[IntegerPart,Flatten[pairingData,1],{2}],IntegerQ[#[[1]]]&];
trackingPureData=Drop[trackingDataTable,4];
ktTrackLabeltoID[x_]:=ToExpression[Flatten[Select[trackingPureData,#[[1]]=="Track_"<>ToString[IntegerPart[x]]&,1]][[3]]];
maxID=Max[Select[ToExpression/@Transpose[trackingPureData][[3]],NumberQ]];
movieLength=Max[ToExpression/@Transpose[trackingPureData][[9]]];
categorizedData=Table[Sort[Select[trackingPureData,ToExpression[#[[3]]]==id&],ToExpression[#1[[9]]]<ToExpression[#2[[9]]]&],{id,0,maxID}];
trackableFramesOverlapMatrix=Table[{{ii,jj,tt},{Select[categorizedData[[ii]],ToExpression[#[[9]]]==tt-1&],Select[categorizedData[[jj]],ToExpression[#[[9]]]==tt-1&]}},{ii,1,maxID+1},{jj,1,maxID+1},{tt,1,movieLength+1}];
dataOfPairs[reali_,realj_,realt_]:=trackableFramesOverlapMatrix[[reali+1,realj+1,realt+1]];
trackableFramesOfPairs[reali_,realj_,realt_]:=ContainsNone[dataOfPairs[reali,realj,realt][[2]],{{}}];
trackableFramesOfPairs[reali_,realj_]:=Select[Range[0,movieLength],trackableFramesOfPairs[reali,realj,#]==True&];

pairedXMeasurements=Table[
{Select[
categorizedData[[ii]],ToExpression[#[[9]]]==realt&][[1]][[5]],Select[
categorizedData[[jj]],ToExpression[#[[9]]]==realt&][[1]][[5]]}
,{ii,maxID+1},{jj,maxID+1},{realt,trackableFramesOfPairs[ii-1,jj-1]}];
pairedXMeasurementsWithFrames=Table[
{Select[
categorizedData[[ii]],ToExpression[#[[9]]]==realt&][[1]][[5]],Select[
categorizedData[[jj]],ToExpression[#[[9]]]==realt&][[1]][[5]],{"frame=",Select[
categorizedData[[jj]],ToExpression[#[[9]]]==realt&][[1]][[9]]}}
,{ii,maxID+1},{jj,maxID+1},{realt,trackableFramesOfPairs[ii-1,jj-1]}];

pairedYMeasurements=Table[
{Select[
categorizedData[[ii]],ToExpression[#[[9]]]==realt&][[1]][[6]],Select[
categorizedData[[jj]],ToExpression[#[[9]]]==realt&][[1]][[6]]}
,{ii,maxID+1},{jj,maxID+1},{realt,trackableFramesOfPairs[ii-1,jj-1]}];
pairedYMeasurementsWithFrames=Table[
{Select[
categorizedData[[ii]],ToExpression[#[[9]]]==realt&][[1]][[6]],Select[
categorizedData[[jj]],ToExpression[#[[9]]]==realt&][[1]][[6]],{"frame=",Select[
categorizedData[[jj]],ToExpression[#[[9]]]==realt&][[1]][[9]]}}
,{ii,maxID+1},{jj,maxID+1},{realt,trackableFramesOfPairs[ii-1,jj-1]}];

pairedZMeasurements=Table[
{Select[
categorizedData[[ii]],ToExpression[#[[9]]]==realt&][[1]][[7]],Select[
categorizedData[[jj]],ToExpression[#[[9]]]==realt&][[1]][[7]]}
,{ii,maxID+1},{jj,maxID+1},{realt,trackableFramesOfPairs[ii-1,jj-1]}];
pairedZMeasurementsWithFrames=Table[
{Select[
categorizedData[[ii]],ToExpression[#[[9]]]==realt&][[1]][[7]],Select[
categorizedData[[jj]],ToExpression[#[[9]]]==realt&][[1]][[7]],{"frame=",Select[
categorizedData[[jj]],ToExpression[#[[9]]]==realt&][[1]][[9]]}}
,{ii,maxID+1},{jj,maxID+1},{realt,trackableFramesOfPairs[ii-1,jj-1]}];

XYZwithFrame=Table[If[Transpose[pairedXMeasurements[[ii,ii]]]=={},{},Transpose[{(Transpose[pairedXMeasurements[[ii,ii]]][[1]]),(Transpose[pairedYMeasurements[[ii,ii]]][[1]]),(Transpose[pairedZMeasurements[[ii,ii]]][[1]]),Transpose[pairedXMeasurementsWithFrames[[ii,ii]]][[3]]}]],{ii,maxID+1}];
FunctionXYZwithFrames[reali_]:=XYZwithFrame[[reali+1]];
FunctionXYZwithFramesNumberOnly[reali_]:=Transpose[{Transpose[FunctionXYZwithFrames[ToExpression[reali]]][[1]],Transpose[FunctionXYZwithFrames[ToExpression[reali]]][[2]],Transpose[FunctionXYZwithFrames[ToExpression[reali]]][[3]],
ToExpression[Transpose[Transpose[FunctionXYZwithFrames[ToExpression[reali]]][[4]]][[2]]]}];


locationExportPath[channelDir_, pair_, ktLabel_, role_] := FileNameJoin[{
  channelDir,
  ToString[pair[[1]]] <> "-" <> ToString[pair[[2]]],
  ToString[IntegerPart[ktLabel]],
  "(+-1)",
  "KTlocation_" <> ToString[IntegerPart[ktLabel]] <> role <> ".csv"
}];

exportLocation[channelDir_, pair_, ktLabel_, role_, sourceLabel_] := Module[
  {trackId, data, path},
  path = locationExportPath[channelDir, pair, ktLabel, role];
  trackId = Quiet@Check[ktTrackLabeltoID[sourceLabel], Missing["TrackLabelNotFound"]];
  If[MatchQ[trackId, _Missing],
    Return[recordWarning["TrackLabelNotFound", <|"Label" -> sourceLabel, "Path" -> path|>]]
  ];
  data = Quiet@Check[FunctionXYZwithFramesNumberOnly[trackId], $Failed];
  If[data === $Failed,
    Return[recordWarning["LocationDataBuildFailed", <|"Label" -> sourceLabel, "Path" -> path|>]]
  ];
  safeExportCSV[path, data]
];

{
  Table[
    {
      exportLocation[exportpath1, ktPairLabels[[ii]], ktPairLabels[[ii, 1]], "self", ktPairLabels[[ii, 1]]],
      exportLocation[exportpath1, ktPairLabels[[ii]], ktPairLabels[[ii, 2]], "self", ktPairLabels[[ii, 2]]]
    },
    {ii, Length[ktPairLabels]}
  ],
  If[
    doubleChannelEnabled,
    Table[
      {
        exportLocation[exportpath2, ktPairLabels[[ii]], ktPairLabels[[ii, 1]], "self", ktPairLabels[[ii, 1]]],
        exportLocation[exportpath2, ktPairLabels[[ii]], ktPairLabels[[ii, 2]], "self", ktPairLabels[[ii, 2]]]
      },
      {ii, Length[ktPairLabels]}
    ],
    {}
  ],
  Table[
    {
      exportLocation[exportpath1, ktPairLabels[[ii]], ktPairLabels[[ii, 1]], "sister", ktPairLabels[[ii, 2]]],
      exportLocation[exportpath1, ktPairLabels[[ii]], ktPairLabels[[ii, 2]], "sister", ktPairLabels[[ii, 1]]]
    },
    {ii, Length[ktPairLabels]}
  ],
  If[
    doubleChannelEnabled,
    Table[
      {
        exportLocation[exportpath2, ktPairLabels[[ii]], ktPairLabels[[ii, 1]], "sister", ktPairLabels[[ii, 2]]],
        exportLocation[exportpath2, ktPairLabels[[ii]], ktPairLabels[[ii, 2]], "sister", ktPairLabels[[ii, 1]]]
      },
      {ii, Length[ktPairLabels]}
    ],
    {}
  ]
}
]


logMessage["TrackMate location export: start."];
Table[Table[LocationDataCollect[movefolderi[[i]]],{i,Length[movefolderi]}],{movefolderi,allTrackingDataDir}];
logMessage["TrackMate location export: done."];

ktDataTableTitlesImport = Table[normalizeHeaderRow[ImportString[i]], {i, Commonest[Table[conditionsdatai[[1]], {conditionsdatai, collectionOfAllCellAllKTData}]][[1]]}];


(* ::Subsection:: *)
(* Per-KT Width, Tail, and Movement Metrics *)


allKTDataNoTitle=Table[GatherBy[Drop[conditionsdatai,1](*drop titles*),#[[{1,2,3,4,5}]]&],{conditionsdatai,collectionOfAllCellAllKTData}];

(* Compute mean X/Y width (per channel) over trackable frames. *)
ch1meanx[inputdata_] := safeMean[widthXValues[inputdata, 1]];

ch2meanx[inputdata_] := safeMean[widthXValues[inputdata, 2]];

ch1meany[inputdata_] := safeMean[widthYValues[inputdata, 1]];

ch2meany[inputdata_] := safeMean[widthYValues[inputdata, 2]];


(*Compute SD of X/Y width (per channel) over trackable frames.*)

ch1SDx[inputdata_] := safeStandardDeviation[widthXValues[inputdata, 1]];

ch2SDx[inputdata_] := safeStandardDeviation[widthXValues[inputdata, 2]];

ch1SDy[inputdata_] := safeStandardDeviation[widthYValues[inputdata, 1]];

ch2SDy[inputdata_] := safeStandardDeviation[widthYValues[inputdata, 2]];

poissonsRatioCh1[inputdata_] := widthXSlope[inputdata, 1];

poissonsRatioCh2[inputdata_] := widthXSlope[inputdata, 2];


(*Check whether a tail appears during KT tracking (per channel).*)


ch1tail[inputdata_]:=AnyTrue[titleColumnValues[inputdata, {"tail or not"}, 1],ToString[#]=="TRUE"||ToString[#]=="True"&];


ch2tail[inputdata_]:=AnyTrue[titleColumnValues[inputdata, {"tail or not"}, 2],ToString[#]=="TRUE"||ToString[#]=="True"&];


(*Tail direction function:-1 if the tail faces left (Pole),+1 if right (DNA).*)


directfunction[direction_]:=If[direction=="left",-1,1];


(*Count the frequency of multi-peak detections.*)


ch1MultiPeakFrequency[inputdata_]:=safeFractionPercent[titleColumnValues[inputdata, number2DPeaksAliases, 1], #>=2&]


ch2MultiPeakFrequency[inputdata_]:=safeFractionPercent[titleColumnValues[inputdata, number2DPeaksAliases, 2], #>=2&]


(*Tail frequency counts.*)


ch1LeftTailCount[inputdata_]:=Count[titleColumnValues[inputdata, {"tail direction"}, 1],"left"]+Count[titleColumnValues[inputdata, {"tail direction"}, 1],"both"]


ch1RightTailCount[inputdata_]:=Count[titleColumnValues[inputdata, {"tail direction"}, 1],"right"]+Count[titleColumnValues[inputdata, {"tail direction"}, 1],"both"]


ch2LeftTailCount[inputdata_]:=Count[titleColumnValues[inputdata, {"tail direction"}, 2],"left"]+Count[titleColumnValues[inputdata, {"tail direction"}, 2],"both"]


ch2RightTailCount[inputdata_]:=Count[titleColumnValues[inputdata, {"tail direction"}, 2],"right"]+Count[titleColumnValues[inputdata, {"tail direction"}, 2],"both"]


(*SD of each KT's position and of its sister across tracking.*)


ktLocationDataFinder[inputdata_,channel_]:=Module[{trackingFileDir,trackingFilePath,ktDataFolder},
ktDataFolder=DirectoryName[AbsoluteFileName[Commonest[Table[If[Length[inputdata[[i]]]>channelChoseOfDataPathColumn[channel],inputdata[[i,channelChoseOfDataPathColumn[channel]]],{}],{i,Length[inputdata]}]][[1]]]];
Select[FileNames[All,ktDataFolder],StringEndsQ[#,"0self.csv"]||StringEndsQ[#,"1self.csv"]||StringEndsQ[#,"2self.csv"]||StringEndsQ[#,"3self.csv"]||StringEndsQ[#,"4self.csv"]||StringEndsQ[#,"5self.csv"]||StringEndsQ[#,"6self.csv"]||StringEndsQ[#,"7self.csv"]||StringEndsQ[#,"8self.csv"]||StringEndsQ[#,"9self.csv"]||StringEndsQ[#,"0.self.csv"]||StringEndsQ[#,"1.self.csv"]||StringEndsQ[#,"2.self.csv"]||StringEndsQ[#,"3.self.csv"]||StringEndsQ[#,"4.self.csv"]||StringEndsQ[#,"5.self.csv"]||StringEndsQ[#,"6.self.csv"]||StringEndsQ[#,"7.self.csv"]||StringEndsQ[#,"8.self.csv"]||StringEndsQ[#,"9.self.csv"]&]];


sisterLocationDataFinder[inputdata_,channel_]:=Module[{trackingFileDir,trackingFilePath,ktDataFolder},
ktDataFolder=DirectoryName[AbsoluteFileName[Commonest[Table[If[Length[inputdata[[i]]]>channelChoseOfDataPathColumn[channel],inputdata[[i,channelChoseOfDataPathColumn[channel]]],{}],{i,Length[inputdata]}]][[1]]]];
Select[FileNames[All,ktDataFolder],StringEndsQ[#,"0sister.csv"]||StringEndsQ[#,"1sister.csv"]||StringEndsQ[#,"2sister.csv"]||StringEndsQ[#,"3sister.csv"]||StringEndsQ[#,"4sister.csv"]||StringEndsQ[#,"5sister.csv"]||StringEndsQ[#,"6sister.csv"]||StringEndsQ[#,"7sister.csv"]||StringEndsQ[#,"8sister.csv"]||StringEndsQ[#,"9sister.csv"]||StringEndsQ[#,"0.sister.csv"]||StringEndsQ[#,"1.sister.csv"]||StringEndsQ[#,"2.sister.csv"]||StringEndsQ[#,"3.sister.csv"]||StringEndsQ[#,"4.sister.csv"]||StringEndsQ[#,"5.sister.csv"]||StringEndsQ[#,"6.sister.csv"]||StringEndsQ[#,"7.sister.csv"]||StringEndsQ[#,"8.sister.csv"]||StringEndsQ[#,"9.sister.csv"]&]];


ktLocationData[inputdata_,channel_]:=Import[ktLocationDataFinder[inputdata[[1]],channel][[1]]];


sisterLocationData[inputdata_,channel_]:=Import[sisterLocationDataFinder[inputdata[[1]],channel][[1]]];


ktXYZLocationDataNumberForm[inputdata_,channel_]:=Transpose[Import[ktLocationDataFinder[inputdata,channel][[1]]]][[{1,2,3}]]


sisterXYZLocationDataNumberForm[inputdata_,channel_]:=Transpose[Import[sisterLocationDataFinder[inputdata,channel][[1]]]][[{1,2,3}]]


ktLocationSD[inputdata_,channel_]:=Sqrt[StandardDeviation[ktXYZLocationDataNumberForm[inputdata,channel][[1]]]^2+StandardDeviation[ktXYZLocationDataNumberForm[inputdata,channel][[2]]]^2];
sisterLocationSD[inputdata_,channel_]:=Sqrt[StandardDeviation[sisterXYZLocationDataNumberForm[inputdata,channel][[1]]]^2+StandardDeviation[sisterXYZLocationDataNumberForm[inputdata,channel][[2]]]^2];


ktSpeed[inputdata_,channel_]:=Sqrt[Differences[ktXYZLocationDataNumberForm[inputdata,channel][[1]]]^2+Differences[ktXYZLocationDataNumberForm[inputdata,channel][[2]]]^2];
sisterSpeed[inputdata_,channel_]:=Sqrt[Differences[sisterXYZLocationDataNumberForm[inputdata,channel][[1]]]^2+Differences[sisterXYZLocationDataNumberForm[inputdata,channel][[2]]]^2];


ktMeanSpeed[inputdata_,channel_]:=Mean[Sqrt[Differences[ktXYZLocationDataNumberForm[inputdata,channel][[1]]]^2+Differences[ktXYZLocationDataNumberForm[inputdata,channel][[2]]]^2]];
sisterMeanSpeed[inputdata_,channel_]:=Mean[Sqrt[Differences[sisterXYZLocationDataNumberForm[inputdata,channel][[1]]]^2+Differences[sisterXYZLocationDataNumberForm[inputdata,channel][[2]]]^2]];


ktSDSpeed[inputdata_,channel_]:=StandardDeviation[Sqrt[Differences[ktXYZLocationDataNumberForm[inputdata,channel][[1]]]^2+Differences[ktXYZLocationDataNumberForm[inputdata,channel][[2]]]^2]];
sisterSDSpeed[inputdata_,channel_]:=StandardDeviation[Sqrt[Differences[sisterXYZLocationDataNumberForm[inputdata,channel][[1]]]^2+Differences[sisterXYZLocationDataNumberForm[inputdata,channel][[2]]]^2]];


sdKKDistance[inputdata_,channel_]:=StandardDeviation[Transpose[inputdata][[Flatten[Position[title,"kk dist"]][[1]]]]];


meanKKDistance[inputdata_,channel_]:=Mean[Transpose[inputdata][[Flatten[Position[title,"kk dist"]][[1]]]]];


xMeanLocation[inputdata_,channel_]:=Mean[ktXYZLocationDataNumberForm[inputdata,channel][[1]]];
yMeanLocation[inputdata_,channel_]:=Mean[ktXYZLocationDataNumberForm[inputdata,channel][[2]]];
zMeanLocation[inputdata_,channel_]:=Mean[ktXYZLocationDataNumberForm[inputdata,channel][[3]]];


(*Define the short-term standard deviation to reduce the influence of cell-level motion.It is computed by the SD in a sliding window (window<movie duration) and then taking the minimum.*)


xyWindowSD[locationData2D_, window_] :=
  Table[
    Norm[StandardDeviation[locationData2D[[Range[window] + i]]]],
    {i, 0, Length[locationData2D] - window}
  ]; (* SD of XY locations. *)

minXYWindowSD[locationData2D_, window_] :=
  Min[xyWindowSD[locationData2D, window]];
(* Minimum windowed SD indicates whether the KT is constrained at any point. *)

ktMinXYWindowSD[inputdata_, window_] :=
  minXYWindowSD[
    Transpose[ktXYZLocationDataNumberForm[inputdata, 1][[{1, 2}]]],
    window
  ];

sisterMinXYWindowSD[inputdata_, window_] :=
  minXYWindowSD[
    Transpose[sisterXYZLocationDataNumberForm[inputdata, 1][[{1, 2}]]],
    window
  ];


(* ::Subsection:: *)
(* Movement Export *)


logMessage["Movement summary: start."];
resultTitle=Join[ktDataTableTitlesImport[[{1,2,4,5,7}]],{"ch1meanx[]","ch2meanx[]","ch1meany[]","ch2meany[]","Poisson'ratio ch1","Poisson'ratio ch2","ch1SDx[]","ch2SDx[]","ch1SDy[]","ch2SDy[]","ch1tail[]","ch2tail[]","ch1MultiPeakFrequency[]","ch2MultiPeakFrequency[]","ch1LeftTailCount[]","ch1RightTailCount[]","ch2LeftTailCount[]","ch2RightTailCount[]","ktLocationSD[]","sisterLocationSD[]","ktMeanSpeed[]","sisterMeanSpeed[]","ktSDSpeed[]","sisterSDSpeed[]","sdKKDistance[]","meanKKDistance[]","meanxlocation[]","meanylocation[]","meanzlocation[]","windowSD_Minimum","sisterMovingWindowSD2DMinimum","windowSize(frames)"}];


movementResults=If[channelNumber===1,Table[Map[#[[1]]&,GatherBy[Table[Join[i[[1]][[{1,2,4,5,7}]],{ch1meanx[i],0,ch1meany[i],0,poissonsRatioCh1[i],0,ch1SDx[i],0,ch1SDy[i],0,ch1tail[i],0,ToString[ch1MultiPeakFrequency[i]],0,ch1LeftTailCount[i],ch1RightTailCount[i],0,0,ktLocationSD[i,1],sisterLocationSD[i,1],ktMeanSpeed[i,1],sisterMeanSpeed[i,1],ktSDSpeed[i,1],sisterSDSpeed[i,1],sdKKDistance[i,1],meanKKDistance[i,1],xMeanLocation[i,1],yMeanLocation[i,1],zMeanLocation[i,1],ktMinXYWindowSD[i,windowSize],sisterMinXYWindowSD[i,windowSize],windowSize}],{i,j}],#[[{1,2,3,4}]]&]],{j,allKTDataNoTitle}],Table[(*double-channel summary*)Map[#[[1]]&,GatherBy[Table[Join[i[[1]][[{1,2,4,5,7}]],{ch1meanx[i],ch2meanx[i],ch1meany[i],ch2meany[i],poissonsRatioCh1[i],poissonsRatioCh2[i],ch1SDx[i],ch2SDx[i],ch1SDy[i],ch2SDy[i],ch1tail[i],ch2tail[i],ToString[ch1MultiPeakFrequency[i]],ToString[ch2MultiPeakFrequency[i]],ch1LeftTailCount[i],ch1RightTailCount[i],ch2LeftTailCount[i],ch2RightTailCount[i],ktLocationSD[i,1],sisterLocationSD[i,1],ktMeanSpeed[i,1],sisterMeanSpeed[i,1],ktSDSpeed[i,1],sisterSDSpeed[i,1],sdKKDistance[i,1],meanKKDistance[i,1],xMeanLocation[i,1],yMeanLocation[i,1],zMeanLocation[i,1],ktMinXYWindowSD[i,windowSize],sisterMinXYWindowSD[i,windowSize],windowSize}],{i,j}],#[[{1,2,3,4}]]&]],{j,allKTDataNoTitle}]];


movementExportFileName=DateString["ISODate"]<>"Movement"<>".csv";

movementExport = safeExportCSV[FileNameJoin[{cellSetDir, DateString["ISODate"] <> "Movement_condition1.csv"}], Join[{resultTitle}, movementResults[[1]]]];

movementData = safeImportTable[movementExport];
logMessage["Movement summary: done."];


(* ::Subsection:: *)
(* Normalized Movement Metrics *)


(* Calculate normalized movement metrics against each cell-level average. *)


ktDataTitle=ktDataCollectionExport[[1]][[1]];


movementDataTitle=movementData[[1]];


cleanUpEmptyList[list_]:=Select[list,Length[#]!=1&&Length[#]!=0&];


movementDataGroupByKT=GatherBy[cleanUpEmptyList[movementData],#[[{1,2}]]&];


ktLocationSDColumn=Flatten[Position[movementDataTitle,"ktLocationSD[]"]][[1]];


sisterLocationSDColumn=Flatten[Position[movementDataTitle,"sisterLocationSD[]"]][[1]];


ktMeanSpeedColumn=Flatten[Position[movementDataTitle,"ktMeanSpeed[]"]][[1]];


sisterMeanSpeedColumn=Flatten[Position[movementDataTitle,"sisterMeanSpeed[]"]][[1]];


ktSDSpeedColumn=Flatten[Position[movementDataTitle,"ktSDSpeed[]"]][[1]];(*Standard deviation of KT speed.*)


sisterSDSpeedColumn=Flatten[Position[movementDataTitle,"sisterSDSpeed[]"]][[1]];


sdKKDistanceColumn=Flatten[Position[movementDataTitle,"sdKKDistance[]"]][[1]];


meanKKDistanceColumn=Flatten[Position[movementDataTitle,"meanKKDistance[]"]][[1]];


meanOfAllMovementData=Map[Mean,movementDataGroupByKT,{1}];


meanOfKT[datalisti_]:=Select[meanOfAllMovementData,#[[{1,2}]]==datalisti[[{1,2}]]&][[1]]


normalize[datalisti_,columnslist_]:=ReplacePart[datalisti,Table[i->(datalisti[[i]]/meanOfKT[datalisti][[i]]),{i,columnslist}]]


normalizedTitle[titles_,columnslist_]:=ReplacePart[titles,Table[i->(titles[[i]]<>"_Normalized"),{i,columnslist}]]


logMessage["Normalized movement summary: start."];
normalizedExtractData=
Join[{normalizedTitle[movementDataTitle,{ktLocationSDColumn,
sisterLocationSDColumn,
ktMeanSpeedColumn,
sisterMeanSpeedColumn,
ktSDSpeedColumn,
sisterSDSpeedColumn,
sdKKDistanceColumn,
meanKKDistanceColumn}]},Table[If[Length[movementData[[i]]]>0,normalize[movementData[[i]],{ktLocationSDColumn,
sisterLocationSDColumn,
ktMeanSpeedColumn,
sisterMeanSpeedColumn,
ktSDSpeedColumn,
sisterSDSpeedColumn,
sdKKDistanceColumn,
meanKKDistanceColumn}]],{i,2,Length[movementData]}]];

normalizedMovementExport = safeExportCSV[FileNameJoin[{DirectoryName[movementExport], DateString["ISODate"] <> "Normalized_of_" <> movementExportFileName}], normalizedExtractData];
logMessage["Normalized movement summary: done."];


(* ::Section:: *)
(* Ch1/Ch2 Channel Registration *)


(*
Defines the Ch1/Ch2 ZNCC fitting utilities used for protein-label displacement
measurements. This two-channel vector fitting is separate from KT-overlap
inspection and does not use PreSelect.
*)


(* ::Subsection:: *)
(* ZNCC Shift Fitting *)


(*
Estimate the Ch1/Ch2 displacement by ZNCC-based channel-shift fitting. The
returned shift is {xTranslation, yTranslation, score, shiftedOverlay,
originalOverlay}; exported protein1-to-protein2 vectors use the negative fitted
translation converted to micrometers.
*)


updateKTimagePath[ktdatapath_,index_]:=Import[StringDrop[ktdatapath,-4]<>$PathnameSeparator<>"allinone"<>$PathnameSeparator<>ToString[index]<>"allinone.png"];


updateKTChImg[ktdatapath_,index_]:=Import[StringDrop[ktdatapath,-4]<>$PathnameSeparator<>"original"<>$PathnameSeparator<>ToString[index]<>"original.tif"]


overlayCh12[pic1_,pic2_]:=Image[MapThread[List,{ImageData[ImageAdjust[pic1],"Real"],ImageData[ImageAdjust[pic2],"Real"],ConstantArray[0.,Dimensions[ImageData[pic1,"Real"]]]},2],"Real",ColorSpace->"RGB"];

shiftImageSmooth[im_,tx_?NumericQ,ty_?NumericQ]:=ImageTransformation[im,({#[[1]]-tx,#[[2]]-ty}&),DataRange->Full,Padding->0,Resampling->"Lanczos"];

centerCrop[im_Image,margin_Integer?NonNegative]:=Module[{w,h,m},{w,h}=ImageDimensions[im];
m=Min[margin,Floor[(Min[w,h]-2)/2]-1];
m=Max[0,m];
ImageTake[im,{m+1,h-m},{m+1,w-m}]];

znccImage[a_Image,b_Image]:=Module[{va,vb,ma,mb,sa,sb},va=Flatten@N@ImageData[a,"Real"];
vb=Flatten@N@ImageData[b,"Real"];
If[Length[va]<4||Length[vb]<4,Return[-Infinity]];
ma=Mean[va];
mb=Mean[vb];
sa=StandardDeviation[va];
sb=StandardDeviation[vb];
If[sa==0.||sb==0.,-Infinity,Total[(va-ma) (vb-mb)]/(Length[va] sa sb)]];

finiteNumericQ[x_]:=NumericQ[x]&&TrueQ[-Infinity<N[x]<Infinity];

parabolicSubpixelOffset[left_,center_,right_]:=Module[{vals,den,offset},
vals=N@{left,center,right};
If[!VectorQ[vals,finiteNumericQ],Return[0.]];
den=vals[[1]]-2 vals[[2]]+vals[[3]];
If[!finiteNumericQ[den]||Abs[den]<10^-12,Return[0.]];
offset=0.5 (vals[[1]]-vals[[3]])/den;
If[finiteNumericQ[offset],Clip[offset,{-1.,1.}],0.]];

shiftVectorFitOnly[c1Image_,c2Image_,maxShift_:3,blurSigma_:1,cropMargin_:Automatic]:=Module[{img1,img2,margin,fixed1,objective,coarseGrid,coarseBest,tx0,ty0,integerScore,dx,dy,quadShift,quadScore,sol,bestShift,bestScore},
img1=ColorConvert[ImageAdjust[c1Image],"Grayscale"];
img2=ColorConvert[ImageAdjust[c2Image],"Grayscale"];
If[blurSigma>0,img1=GaussianFilter[img1,blurSigma];
img2=GaussianFilter[img2,blurSigma];];
margin=Replace[cropMargin,Automatic->Ceiling[maxShift+1 blurSigma]];
fixed1=centerCrop[img1,margin];
objective[tx_?NumericQ,ty_?NumericQ]:=Module[{bShift,crop2},bShift=shiftImageSmooth[img2,tx,ty];
crop2=centerCrop[bShift,margin];
znccImage[fixed1,crop2]];
coarseGrid=Flatten[Table[{tx,ty,objective[tx,ty]},{tx,-maxShift,maxShift,1},{ty,-maxShift,maxShift,1}],1];
coarseBest=First@MaximalBy[coarseGrid,Last];
If[!TrueQ[-Infinity<coarseBest[[3]]<Infinity],Return[$Failed]];
{tx0,ty0}=coarseBest[[1;;2]];
integerScore[x_Integer,y_Integer]:=Module[{hit=Cases[coarseGrid,{x,y,s_}:>s,1]},
If[hit==={},-Infinity,First[hit]]];
dx=If[tx0<=-maxShift||tx0>=maxShift,0.,
parabolicSubpixelOffset[integerScore[tx0-1,ty0],integerScore[tx0,ty0],integerScore[tx0+1,ty0]]];
dy=If[ty0<=-maxShift||ty0>=maxShift,0.,
parabolicSubpixelOffset[integerScore[tx0,ty0-1],integerScore[tx0,ty0],integerScore[tx0,ty0+1]]];
quadShift=N@{Clip[tx0+dx,{tx0-1.,tx0+1.}],Clip[ty0+dy,{ty0-1.,ty0+1.}]};
quadScore=Quiet@Check[objective[quadShift[[1]],quadShift[[2]]],-Infinity];
sol=Quiet@Check[
TimeConstrained[
NMaximize[{objective[tx,ty],tx0-1<=tx<=tx0+1&&ty0-1<=ty<=ty0+1},{tx,ty},Method->"NelderMead"],
znccRefinementTimeoutSeconds,
$Aborted],
$Failed];
If[sol===$Failed||sol===$Aborted||!ListQ[sol],
bestShift=quadShift;
bestScore=N@quadScore,
bestShift=N@({tx,ty}/. Last[sol]);
bestScore=N@First[sol];
If[!VectorQ[bestShift,NumericQ]||!NumericQ[bestScore]||!TrueQ[-Infinity<bestScore<Infinity],
bestShift=quadShift;
bestScore=N@quadScore]];
If[finiteNumericQ[quadScore]&&finiteNumericQ[bestScore]&&quadScore>bestScore,
bestShift=quadShift;
bestScore=N@quadScore];
If[!VectorQ[bestShift,finiteNumericQ]||!finiteNumericQ[bestScore],
bestShift=N@{tx0,ty0};
bestScore=N@coarseBest[[3]]];
{bestShift[[1]],bestShift[[2]],bestScore}];

shiftVector[c1Image_,c2Image_,maxShift_:3,blurSigma_:1,cropMargin_:Automatic]:=Module[{fit,img1,img2,margin,bestShift,bestScore,aligned,fixed1,fixed2,originalFixed2,shiftOverlay,originalOverlay},
fit=shiftVectorFitOnly[c1Image,c2Image,maxShift,blurSigma,cropMargin];
If[fit===$Failed||!ListQ[fit]||Length[fit]<3,Return[$Failed]];
img1=ColorConvert[ImageAdjust[c1Image],"Grayscale"];
img2=ColorConvert[ImageAdjust[c2Image],"Grayscale"];
If[blurSigma>0,img1=GaussianFilter[img1,blurSigma];
img2=GaussianFilter[img2,blurSigma];];
margin=Replace[cropMargin,Automatic->Ceiling[maxShift+1 blurSigma]];
bestShift=N@fit[[1;;2]];
bestScore=N@fit[[3]];
aligned=shiftImageSmooth[img2,bestShift[[1]],bestShift[[2]]];
fixed1=centerCrop[img1,margin];
fixed2=centerCrop[aligned,margin];
originalFixed2=centerCrop[img2,margin];
shiftOverlay=overlayCh12[fixed1,fixed2];
originalOverlay=overlayCh12[fixed1,originalFixed2];
{bestShift[[1]],bestShift[[2]],bestScore,shiftOverlay,originalOverlay}];


ktOriginalImagePath[ktdatapath_String, index_?NumericQ] := FileNameJoin[
  {
    StringReplace[ktdatapath, RegularExpression["\\.csv$"] -> ""],
    "original",
    ToString[Round[index]] <> "original.tif"
  }
];


znccFittingVectorUm[fit_List] := Module[{scale, tx, ty, score},
  If[Length[fit] < 3, Return[{"NA", "NA", "NA"}]];
  {tx, ty, score} = N[fit[[1 ;; 3]]];
  If[!VectorQ[{tx, ty, score}, NumericQ], Return[{"NA", "NA", "NA"}]];
  scale = If[NumericQ[znccPixelSizeUm] && znccPixelSizeUm > 0, N[znccPixelSizeUm], 1.];
  {-tx*scale, -ty*scale, score}
];
znccFittingVectorUm[_] := {"NA", "NA", "NA"};


existingAllKTResultTable[] := Module[{path, data},
  If[!ValueQ[cellSetDir] || !StringQ[cellSetDir], Return[{}]];
  path = FileNameJoin[{cellSetDir, DateString["ISODate"] <> "allKT.csv"}];
  If[!FileExistsQ[path], Return[{}]];
  data = Quiet@Check[safeImportTable[path], $Failed];
  If[ListQ[data] && Length[data] > 0, data, {}]
];


znccRowKeyString[headers_List, row_List] := Module[
  {ch1Info, ch2Info, path1, path2, index1, index2},
  ch1Info = channelImageInfo[headers, row, 1];
  ch2Info = channelImageInfo[headers, row, 2];
  If[!ListQ[ch1Info] || !ListQ[ch2Info], Return[Missing["NoRowKey"]]];
  {path1, index1} = ch1Info;
  {path2, index2} = ch2Info;
  If[
    !StringQ[path1] || !StringQ[path2] || !NumericQ[index1] || !NumericQ[index2],
    Return[Missing["NoRowKey"]]
  ];
  StringRiffle[{path1, path2, ToString[Round[index1]], ToString[Round[index2]]}, "||"]
];


storedZNCCFitFromRow[headers_List, row_List] := Module[
  {xCols, yCols, scoreCols, x, y, score},
  xCols = columnPositionsByName[
    headers,
    {"Ch1/Ch2 ZNCC fitting vector X (um)", "znccFittingVectorX(um)"}
  ];
  yCols = columnPositionsByName[
    headers,
    {"Ch1/Ch2 ZNCC fitting vector Y (um)", "znccFittingVectorY(um)"}
  ];
  scoreCols = columnPositionsByName[
    headers,
    {"Ch1/Ch2 ZNCC fitting score", "znccFittingScore"}
  ];
  If[Length[xCols] < 1 || Length[yCols] < 1, Return[Missing["NoStoredZNCC"]]];
  x = If[Length[row] >= First[xCols], toNumericOrMissing[row[[First[xCols]]]], Missing["NoStoredZNCC"]];
  y = If[Length[row] >= First[yCols], toNumericOrMissing[row[[First[yCols]]]], Missing["NoStoredZNCC"]];
  score = If[
    Length[scoreCols] >= 1 && Length[row] >= First[scoreCols],
    toNumericOrMissing[row[[First[scoreCols]]]],
    Missing["NoStoredZNCC"]
  ];
  If[NumericQ[x] && NumericQ[y], {x, y, If[NumericQ[score], score, "NA"]}, Missing["NoStoredZNCC"]]
];


storedZNCCFitLookup[data_List] := Module[{headers, rows, pairs},
  If[Length[data] < 2, Return[<||>]];
  headers = First[data];
  rows = Rest[data];
  pairs = DeleteCases[
    Map[
      Module[{key = znccRowKeyString[headers, #], fit = storedZNCCFitFromRow[headers, #]},
        If[StringQ[key] && ListQ[fit], key -> fit, Missing["NoStoredZNCC"]]
      ]&,
      rows
    ],
    _Missing
  ];
  Association[pairs]
];


fitZNCCForRow[headers_List, row_List, maxShift_: 3, blurSigma_: 1] := Module[
  {
    ch1Info, ch2Info, path1, path2, index1, index2,
    imagePath1, imagePath2, image1, image2, fit
  },
  ch1Info = channelImageInfo[headers, row, 1];
  ch2Info = channelImageInfo[headers, row, 2];
  If[!ListQ[ch1Info] || !ListQ[ch2Info], Return[{"NA", "NA", "NA"}]];
  {path1, index1} = ch1Info;
  {path2, index2} = ch2Info;
  If[
    !StringQ[path1] || !StringQ[path2] || !NumericQ[index1] || !NumericQ[index2],
    Return[{"NA", "NA", "NA"}]
  ];
  imagePath1 = ktOriginalImagePath[path1, index1];
  imagePath2 = ktOriginalImagePath[path2, index2];
  If[!FileExistsQ[imagePath1] || !FileExistsQ[imagePath2], Return[{"NA", "NA", "NA"}]];
  image1 = Quiet@Check[Import[imagePath1], $Failed];
  image2 = Quiet@Check[Import[imagePath2], $Failed];
  If[!ImageQ[image1] || !ImageQ[image2], Return[{"NA", "NA", "NA"}]];
  If[ImageDimensions[image1] =!= ImageDimensions[image2],
    image2 = ImageResize[image2, ImageDimensions[image1]]
  ];
  fit = Quiet@Check[
    TimeConstrained[
      shiftVectorFitOnly[image1, image2, maxShift, blurSigma],
      znccFittingTimeoutSeconds,
      $Aborted
    ],
    $Failed
  ];
  If[
    fit === $Failed || fit === $Aborted || !ListQ[fit] || Length[fit] < 3,
    {"NA", "NA", "NA"},
    znccFittingVectorUm[fit]
  ]
];


fitZNCCRows[headers_List, rows_List] := Module[
  {total, kernels, fits, parallelHeaders, storedLookup, storedFits},
  total = Length[rows];
  If[total == 0, Return[{}]];
  If[
    !TrueQ[exportZNCCFittingResults],
    Return[ConstantArray[{"NA", "NA", "NA"}, total]]
  ];
  If[
    TrueQ[reuseExistingZNCCFittingResults],
    storedLookup = storedZNCCFitLookup[existingAllKTResultTable[]];
    If[AssociationQ[storedLookup] && Length[storedLookup] > 0,
      storedFits = Map[
        With[{key = znccRowKeyString[headers, #]},
          If[StringQ[key], Lookup[storedLookup, key, Missing["NoStoredZNCC"]], Missing["NoStoredZNCC"]]
        ]&,
        rows
      ];
      If[
        Length[storedFits] == total && AllTrue[storedFits, ListQ],
        logMessage["Reused stored Ch1/Ch2 ZNCC fitting vectors from existing allKT.csv for " <> ToString[total] <> " rows."];
        Return[storedFits]
      ]
    ]
  ];
  logMessage["Computing Ch1/Ch2 ZNCC fitting vectors for " <> ToString[total] <> " rows."];
  kernels = Max[1, Min[$ProcessorCount - 1, maxParallelKernels, maxParallelZNCCKernels]];
  If[
    TrueQ[useParallelZNCCFitting] && kernels > 1 && total > 1,
    parallelHeaders = headers;
    fits = withManagedParallelKernels[
      kernels,
      If[KeyExistsQ[Association@SystemOptions[], "EvaluateInFrontEnd"], SetSystemOptions["EvaluateInFrontEnd" -> False]];
      DistributeDefinitions[
        parallelHeaders, fitZNCCForRow, shiftVectorFitOnly,
        ktOriginalImagePath, toNumericOrMissing, columnPositionsByName,
        rowValueByHeader, channelSplitPosition, channelBlockRange,
        channelBlockValue, channelImageInfo,
        normalizeHeaderName, normalizeHeaderKey, normalizeHeaderRow, standardWidthXName,
        standardWidthYName, widthXAliases, widthYAliases, numberStringPattern,
        znccFittingVectorUm, znccImage, shiftImageSmooth, centerCrop,
        finiteNumericQ, parabolicSubpixelOffset,
        znccMaxShift, znccBlurSigma, znccPixelSizeUm,
        znccFittingTimeoutSeconds, znccRefinementTimeoutSeconds
      ];
      ParallelMap[
        fitZNCCForRow[parallelHeaders, #, znccMaxShift, znccBlurSigma]&,
        rows,
        Method -> "CoarsestGrained"
      ]
    ];
    If[fits === $Failed || !ListQ[fits] || Length[fits] =!= total,
      recordWarning["ParallelZNCCFailed", <|"Fallback" -> "Serial ZNCC fitting"|>];
      fits = MapIndexed[
        (
          If[
            IntegerQ[znccBatchProgressInterval] && znccBatchProgressInterval > 0 &&
            (First[#2] == 1 || Mod[First[#2], znccBatchProgressInterval] == 0 || First[#2] == total),
            logMessage["ZNCC fitting row " <> ToString[First[#2]] <> "/" <> ToString[total]]
          ];
          fitZNCCForRow[headers, #1, znccMaxShift, znccBlurSigma]
        )&,
        rows
      ]
    ],
    fits = MapIndexed[
      (
        If[
          IntegerQ[znccBatchProgressInterval] && znccBatchProgressInterval > 0 &&
          (First[#2] == 1 || Mod[First[#2], znccBatchProgressInterval] == 0 || First[#2] == total),
          logMessage["ZNCC fitting row " <> ToString[First[#2]] <> "/" <> ToString[total]]
        ];
        fitZNCCForRow[headers, #1, znccMaxShift, znccBlurSigma]
      )&,
      rows
    ]
  ];
  logMessage["Finished Ch1/Ch2 ZNCC fitting vectors."];
  fits
];


appendZNCCFittingColumns[data_List] := Module[{headers, rows, fits},
  If[Length[data] < 1, Return[data]];
  headers = First[data];
  rows = Rest[data];
  fits = fitZNCCRows[headers, rows];
  Join[
    {Join[headers, {
      "Ch1/Ch2 ZNCC fitting vector X (um)",
      "Ch1/Ch2 ZNCC fitting vector Y (um)",
      "Ch1/Ch2 ZNCC fitting score"
    }]},
    MapThread[Join, {rows, fits}]
  ]
];


logMessage["Compiled Ch1/Ch2 ZNCC export columns: start."];
compiledResultsWithZNCC = If[
  TrueQ[doubleChannelEnabled],
  appendZNCCFittingColumns[compiledResultsBase],
  compiledResultsBase
];
logMessage["Compiled Ch1/Ch2 ZNCC export columns: done."];

outputs = {
  safeExportCSV[
    FileNameJoin[{cellSetDir, DateString["ISODate"] <> "allKT.csv"}],
    compiledResultsWithZNCC
  ]
};
logMessage["Compiled allKT export: done."];


(* ::Section:: *)
(* Target/Background KT Signal Overlap Filtering *)


(*
Single-channel intensity/area QC for frames where background or neighboring
kinetochore signal overlaps the target KT signal. This filter does not use
Ch1/Ch2 ZNCC fitting.
*)


(* ::Subsection:: *)
(* KT-Signal Overlap Candidate Table *)


inspectionDataPath=outputs[[1]];


logMessage["KT-overlap inspection table: start."];
inspectionData=Select[
  compiledResultsWithZNCC,
  # =!= Null && Length[#] == Commonest[Length /@ compiledResultsWithZNCC][[1]]&
];


markData=Transpose[Join[Transpose[inspectionData],{Join[{"select"},ConstantArray[0,Length[inspectionData]-1]]}]]; (* Initialize KT-overlap inspection mark. *)


markData[[1]][[-1]]="Inspection mark";


markDataLength=Length[markData];


displayTitles=markData[[1]][[Length[markData[[1]]]]];


n=Length[markData];


markDataSet=Join[{markData[[1]]},ReverseSortBy[markData[[Range[2,markDataLength]]],#[[{31,66,17,52,10,45}]]&]]; (*Create the dataset for display.*)


(* ::Subsection:: *)
(* Target/Background KT Signal Overlap Detection *)


(* Detect frames where background or neighboring kinetochore signal overlaps the
   target KT signal by clustering total intensity and comparing object area. *)
clusterKTIntensity[singleKTDataset_]:=Module[{ch1data,ch2data,clustersCh1,clustersCh2,ch1ClustersMean,ch2ClustersMean,ch1SeparationRate,ch2SeparationRate,dataClustersCh1Ch2,ch1Classifier,ch2Classifier,ch1SortedDataWithLabels,ch1FirstLabel,ch1LabelMap,ch1TransformedLabels,ch2SortedDataWithLabels,ch2FirstLabel,ch2LabelMap,ch2TransformedLabels,ch1Labels,ch2Labels,ch1ktArea,ch2ktArea,ch1ktAreaGroup1,ch1ktAreaGroup2,ch2ktAreaGroup1,ch2ktAreaGroup2,ch1ktAreaGroup1Mean,ch1ktAreaGroup2Mean,ch2ktAreaGroup1Mean,ch2ktAreaGroup2Mean,ch1IndexGroup1,ch1IndexGroup2,ch2IndexGroup1,ch2IndexGroup2,data,emptyClusterResult},

data=Cases[toNumericOrMissing /@ Transpose[singleKTDataset][[1]], _?NumericQ];
emptyClusterResult[] := ConstantArray[{0, 1, 0, 0, 0, 1, 0, 0}, Length[data]];

If[doubleChannelEnabled,
ch1data=Select[Cases[toNumericOrMissing /@ Transpose[singleKTDataset][[33]], _?NumericQ], # > 0&];
ch2data=Select[Cases[toNumericOrMissing /@ Transpose[singleKTDataset][[69]], _?NumericQ], # > 0&];
If[Length[ch1data] < 2 || Length[ch2data] < 2, Return[emptyClusterResult[]]];
clustersCh1=Quiet@Check[FindClusters[ch1data,2], Return[emptyClusterResult[]]];
clustersCh2=Quiet@Check[FindClusters[ch2data,2], Return[emptyClusterResult[]]];
ch1Classifier=Quiet@Check[ClusterClassify[ch1data,2], Return[emptyClusterResult[]]];
ch2Classifier=Quiet@Check[ClusterClassify[ch2data,2], Return[emptyClusterResult[]]];
ch1ClustersMean=Mean/@clustersCh1;
ch2ClustersMean=Mean/@clustersCh2;
ch1SeparationRate=safeSeparationRatio[ch1ClustersMean];
ch2SeparationRate=safeSeparationRatio[ch2ClustersMean];
ch1Labels=ch1Classifier/@ch1data;
ch2Labels=ch2Classifier/@ch2data;

(*Ensure label 1=low,2=high by ordering on intensity.*)
ch1SortedDataWithLabels=SortBy[Transpose[{ch1data,ch1Labels}],First];
ch1FirstLabel=ch1SortedDataWithLabels[[1,2]];
ch1LabelMap=If[ch1FirstLabel==1,{1->1,2->2},{1->2,2->1}]; 
ch1TransformedLabels=ch1Labels/. ch1LabelMap;
ch2SortedDataWithLabels=SortBy[Transpose[{ch2data,ch2Labels}],First];
ch2FirstLabel=ch2SortedDataWithLabels[[1,2]];
ch2LabelMap=If[ch2FirstLabel==1,{1->1,2->2},{1->2,2->1}];
ch2TransformedLabels=ch2Labels/. ch2LabelMap;

ch1IndexGroup1=Flatten[Position[ch1TransformedLabels,1]];
ch1IndexGroup2=Flatten[Position[ch1TransformedLabels,2]];
ch2IndexGroup1=Flatten[Position[ch2TransformedLabels,1]];
ch2IndexGroup2=Flatten[Position[ch2TransformedLabels,2]];
ch1ktArea=Transpose[singleKTDataset][[9]];
ch2ktArea=Transpose[singleKTDataset][[45]];
ch1ktAreaGroup1=ch1ktArea[[ch1IndexGroup1]];
ch1ktAreaGroup2=ch1ktArea[[ch1IndexGroup2]];
ch2ktAreaGroup1=ch2ktArea[[ch2IndexGroup1]];
ch2ktAreaGroup2=ch2ktArea[[ch2IndexGroup2]];
ch1ktAreaGroup1Mean=safeMean[Cases[toNumericOrMissing /@ ch1ktAreaGroup1, _?NumericQ]];
ch1ktAreaGroup2Mean=safeMean[Cases[toNumericOrMissing /@ ch1ktAreaGroup2, _?NumericQ]];
ch2ktAreaGroup1Mean=safeMean[Cases[toNumericOrMissing /@ ch2ktAreaGroup1, _?NumericQ]];
ch2ktAreaGroup2Mean=safeMean[Cases[toNumericOrMissing /@ ch2ktAreaGroup2, _?NumericQ]],


ch1data=Select[Cases[toNumericOrMissing /@ Transpose[singleKTDataset][[33]], _?NumericQ], # > 0&];
If[Length[ch1data] < 2, Return[emptyClusterResult[]]];
clustersCh1=Quiet@Check[FindClusters[ch1data,2], Return[emptyClusterResult[]]];

ch1Classifier=Quiet@Check[ClusterClassify[ch1data,2], Return[emptyClusterResult[]]];

ch1ClustersMean=Mean/@clustersCh1;

ch1SeparationRate=safeSeparationRatio[ch1ClustersMean];

ch1Labels=ch1Classifier/@ch1data;


(*Ensure label 1=low,2=high by ordering on intensity.*)
ch1SortedDataWithLabels=SortBy[Transpose[{ch1data,ch1Labels}],First];
ch1FirstLabel=ch1SortedDataWithLabels[[1,2]];
ch1LabelMap=If[ch1FirstLabel==1,{1->1,2->2},{1->2,2->1}]; 
ch1TransformedLabels=ch1Labels/. ch1LabelMap;


ch1IndexGroup1=Flatten[Position[ch1TransformedLabels,1]];
ch1IndexGroup2=Flatten[Position[ch1TransformedLabels,2]];

ch1ktArea=Transpose[singleKTDataset][[9]];

ch1ktAreaGroup1=ch1ktArea[[ch1IndexGroup1]];
ch1ktAreaGroup2=ch1ktArea[[ch1IndexGroup2]];

ch1ktAreaGroup1Mean=safeMean[Cases[toNumericOrMissing /@ ch1ktAreaGroup1, _?NumericQ]];
ch1ktAreaGroup2Mean=safeMean[Cases[toNumericOrMissing /@ ch1ktAreaGroup2, _?NumericQ]];
];


If[doubleChannelEnabled,
dataClustersCh1Ch2=Transpose[{ConstantArray[N[ch1SeparationRate],Length[data]],ch1TransformedLabels,ConstantArray[N[safeSeparationRatio[{ch1ktAreaGroup1Mean,ch1ktAreaGroup2Mean}]],Length[data]],ConstantArray[N[safeMaxValue[{ch1ktAreaGroup1Mean,ch1ktAreaGroup2Mean}]],Length[data]],ConstantArray[N[ch2SeparationRate],Length[data]],
ch2TransformedLabels,ConstantArray[N[safeSeparationRatio[{ch2ktAreaGroup1Mean,ch2ktAreaGroup2Mean}]],Length[data]],ConstantArray[N[safeMaxValue[{ch2ktAreaGroup1Mean,ch2ktAreaGroup2Mean}]],Length[data]]}],

dataClustersCh1Ch2=Transpose[{ConstantArray[N[ch1SeparationRate],Length[data]],ch1TransformedLabels,ConstantArray[N[safeSeparationRatio[{ch1ktAreaGroup1Mean,ch1ktAreaGroup2Mean}]],Length[data]],ConstantArray[N[safeMaxValue[{ch1ktAreaGroup1Mean,ch1ktAreaGroup2Mean}]],Length[data]],ConstantArray[Infinity,Length[data]],
ConstantArray[1,Length[data]],ConstantArray[Infinity,Length[data]],ConstantArray[Infinity,Length[data]]}]
]]
(* dataClustersCh1Ch2 is returned as the module result. *)


clusterKTIntensityJoinKTDataset[singleKTDataset_]:=Map[Catenate,Transpose[{singleKTDataset,clusterKTIntensity[singleKTDataset]}]];(*Append cluster features to the KT dataset.*)


logMessage["KT-overlap intensity clustering: start."];
markDataSetNormalizedTotIntensitySorted=Join[{Join[markData[[1]],{"ch1IntensitySeparationRate","ch1Classifier","ch1AreaSeparationRate","ch1Group2AreaMean","ch2IntensitySeparationRate","ch2Classifier","ch2AreaSeparationRate","ch2Group2AreaMean"}]},SortBy[Flatten[Map[clusterKTIntensityJoinKTDataset,GatherBy[Drop[markDataSet,1],#[[Range[5]]]&]],1],#[[{1,2,3,4,5}]]&]];
logMessage["KT-overlap intensity clustering: done."];


(* Mark likely target/background KT signal overlap frames when:
   - the frame belongs to the high-intensity cluster,
   - the high/low intensity ratio exceeds the configured threshold, and
   - the detected pixel area exceeds the configured threshold.
   Both channels are evaluated when doubleChannelEnabled is True. *)


markDataSetNormalizedTotIntensitySortedIntensityPreSelect=Join[{Join[markDataSetNormalizedTotIntensitySorted[[1]],{"PreSelect"}]},SortBy[Table[Join[i,{If[i[[-8]]>overlapSeparationIntensityThreshold
&&ToExpression[i[[-7]]]!=1&&i[[-5]]>overlapSeparationPixelThreshold||i[[-4]]>overlapSeparationIntensityThreshold&&ToExpression[i[[-3]]]!=1&&i[[-1]]>overlapSeparationPixelThreshold,1,0]}],{i,Drop[markDataSetNormalizedTotIntensitySorted,1]}],#[[{1,2,3,4,5}]]&]];

outputPreSelect = {safeExportCSV[FileNameJoin[{cellSetDir, DateString["ISODate"] <> "allKT_PreSelectMark.csv"}], markDataSetNormalizedTotIntensitySortedIntensityPreSelect]};

outputFiltered = {safeExportCSV[FileNameJoin[{cellSetDir, DateString["ISODate"] <> "allKT_filtered.csv"}], Select[markDataSetNormalizedTotIntensitySortedIntensityPreSelect, #[[-1]] != 1&]]};
logMessage["KT-overlap inspection table: done."];

If[doubleChannelEnabled,
displayDataWithKTOverlapFlags = markDataSetNormalizedTotIntensitySortedIntensityPreSelect;

outputPreSelectKTOverlapInspection = {safeExportCSV[FileNameJoin[{cellSetDir, DateString["ISODate"] <> "extract_PreSelectMark.csv"}], displayDataWithKTOverlapFlags]};

outputFilteredKTOverlapInspection = {safeExportCSV[FileNameJoin[{cellSetDir, DateString["ISODate"] <> "extract_filtered.csv"}], Select[displayDataWithKTOverlapFlags, #[[-1]] != 1&]]};
,
displayDataWithKTOverlapFlags = {};
outputPreSelectKTOverlapInspection = {};
outputFilteredKTOverlapInspection = {};
];


workflowSummary = <|
  "Warnings" -> workflowWarnings,
  "Errors" -> workflowErrors,
  "Outputs" -> Flatten@{
    outputs,
    movementExport,
    normalizedMovementExport,
    outputPreSelect,
    outputFiltered,
    outputPreSelectKTOverlapInspection,
    outputFilteredKTOverlapInspection
  }
|>;


(* ::Section:: *)
(* Notebook Display *)


makeUniqueHeaders[headers_List] := Module[
  {counts = <||>, baseHeaders, totalCounts, key, n},
  baseHeaders = ToString /@ (normalizeHeaderName /@ headers);
  totalCounts = Counts[baseHeaders];
  Table[
    key = baseHeaders[[ii]];
    n = Lookup[counts, key, 0] + 1;
    counts[key] = n;
    Which[
      key === "/" || key === "//",
        key <> " " <> ToString[n],
      n == 1 && Lookup[totalCounts, key, 0] == 1,
        key,
      n == 1,
        "Ch1 " <> key,
      n == 2,
        "Ch2 " <> key,
      True,
        key <> " (" <> ToString[n] <> ")"
    ],
    {ii, Length[headers]}
  ]
];


tableRowsToAssociations[data_List, maxRows_: 50] := Module[{headers, rows},
  If[Length[data] < 2, Return[{}]];
  headers = makeUniqueHeaders[First[data]];
  rows = Take[Rest[data], UpTo[maxRows]];
  AssociationThread[
      headers,
      Take[PadRight[#, Length[headers], ""], Length[headers]]
    ]& /@ rows
];


displayTablePreview[data_List, maxRows_: 50] := Module[{rows},
  rows = tableRowsToAssociations[data, maxRows];
  If[Length[rows] == 0, Style["No rows available.", Italic, Gray], Dataset[rows]]
];
displayTablePreview[_, ___] := Style["No rows available.", Italic, Gray];


rowCount[data_] := If[ListQ[data] && Length[data] > 0, Max[Length[data] - 1, 0], 0];


allKTResultTable[] := Module[{paths, path, data},
  paths = DeleteDuplicates@Cases[
    Flatten@{
      If[ValueQ[outputs] && ListQ[outputs], outputs, {}],
      If[ValueQ[cellSetDir] && StringQ[cellSetDir],
        FileNameJoin[{cellSetDir, DateString["ISODate"] <> "allKT.csv"}],
        Nothing
      ]
    },
    s_String /; FileExistsQ[s] && StringEndsQ[FileNameTake[s], "allKT.csv"]
  ];
  If[Length[paths] == 0, Return[{}]];
  path = First[paths];
  data = Quiet@Check[safeImportTable[path], $Failed];
  If[ListQ[data] && Length[data] > 0, data, {}]
];


compiledResultTable[] := Module[{fromAllKT},
  fromAllKT = allKTResultTable[];
  Which[
    ListQ[fromAllKT] && Length[fromAllKT] > 0,
      fromAllKT,
    ValueQ[compiledResultsWithZNCC] && ListQ[compiledResultsWithZNCC],
      compiledResultsWithZNCC,
    ValueQ[compiledResultsBase] && ListQ[compiledResultsBase],
      compiledResultsBase,
    ValueQ[ktDataCollectionExport] && ListQ[ktDataCollectionExport] &&
      Length[ktDataCollectionExport] >= 1,
      ktDataCollectionExport[[1]],
    True,
      {}
  ]
];


ktOverlapInspectionTable[] := If[
  ValueQ[markDataSetNormalizedTotIntensitySortedIntensityPreSelect] &&
    ListQ[markDataSetNormalizedTotIntensitySortedIntensityPreSelect],
  markDataSetNormalizedTotIntensitySortedIntensityPreSelect,
  {}
];


channelRegistrationTable[] := compiledResultTable[];


matchValueQ[value_, criterion_] := Module[{left, right},
  If[criterion === All || criterion === Automatic, Return[True]];
  left = toNumericOrMissing[value];
  right = toNumericOrMissing[criterion];
  If[NumericQ[left] && NumericQ[right], Return[left == right]];
  ToString[value] == ToString[criterion]
];

displayCellValue[value_] := If[MatchQ[value, _Missing], "NA", value];


displayZNCCVectorTable[maxRows_: 50] := Module[{data, headers, rows},
  data = compiledResultTable[];
  If[Length[data] < 2, Return[Style["No compiled result rows are available.", Italic, Gray]]];
  headers = First[data];
  rows = Take[Rest[data], UpTo[maxRows]];
  Dataset[
    MapIndexed[
      <|
        "Compiled row index" -> First[#2],
        "Cell index" -> rowValueByHeader[headers, #1, {"Cell index"}, 1],
        "KT pair" -> rowValueByHeader[headers, #1, {"KT pairs"}, 1],
        "KT" -> rowValueByHeader[headers, #1, {"KT"}, 1],
        "Time" -> rowValueByHeader[headers, #1, {"Time"}, 1],
        "Ch1/Ch2 ZNCC fitting vector X (um)" ->
          displayCellValue@rowValueByHeader[
            headers,
            #1,
            {"Ch1/Ch2 ZNCC fitting vector X (um)", "znccFittingVectorX(um)"}
          ],
        "Ch1/Ch2 ZNCC fitting vector Y (um)" ->
          displayCellValue@rowValueByHeader[
            headers,
            #1,
            {"Ch1/Ch2 ZNCC fitting vector Y (um)", "znccFittingVectorY(um)"}
          ],
        "Ch1/Ch2 ZNCC fitting score" ->
          displayCellValue@rowValueByHeader[
            headers,
            #1,
            {"Ch1/Ch2 ZNCC fitting score", "znccFittingScore"}
          ]
      |>&,
      rows
    ]
  ]
];


metadataRowMatchQ[headers_List, row_List, cellIndex_, ktPair_, kt_, time_] :=
  matchValueQ[rowValueByHeader[headers, row, {"Cell index"}, 1], cellIndex] &&
  matchValueQ[rowValueByHeader[headers, row, {"KT pairs"}, 1], ktPair] &&
  matchValueQ[rowValueByHeader[headers, row, {"KT"}, 1], kt] &&
  matchValueQ[rowValueByHeader[headers, row, {"Time"}, 1], time];


selectCompiledRowsByMetadata[cellIndex_, ktPair_, kt_: All, time_: All] := Module[
  {data, headers, rows, selected},
  data = compiledResultTable[];
  If[Length[data] < 2, Return[{}]];
  headers = First[data];
  rows = Rest[data];
  selected = Select[rows, metadataRowMatchQ[headers, #, cellIndex, ktPair, kt, time]&];
  If[Length[selected] == 0, {}, Join[{headers}, selected]]
];


ktOverlapCandidateRows[] := Module[{data, headers, col},
  data = ktOverlapInspectionTable[];
  If[Length[data] < 2, Return[{}]];
  headers = First[data];
  col = Flatten@Position[headers, "PreSelect"];
  If[Length[col] == 0, Return[{}]];
  col = First[col];
  Select[
    Rest[data],
    Length[#] >= col && TrueQ[toNumericOrMissing[#[[col]]] == 1]&
  ]
];


ktOverlapInspectionScore[headers_List, row_List] := Max[
  Cases[
    {
      toNumericOrMissing[rowValueByHeader[headers, row, {"ch1IntensitySeparationRate"}]]/
        overlapSeparationIntensityThreshold,
      toNumericOrMissing[rowValueByHeader[headers, row, {"ch1Group2AreaMean"}]]/
        overlapSeparationPixelThreshold,
      toNumericOrMissing[rowValueByHeader[headers, row, {"ch2IntensitySeparationRate"}]]/
        overlapSeparationIntensityThreshold,
      toNumericOrMissing[rowValueByHeader[headers, row, {"ch2Group2AreaMean"}]]/
        overlapSeparationPixelThreshold
    },
    _?NumericQ
  ] /. {} -> {0}
];


sortedKTOverlapInspectionRows[] := Module[{data, headers, rows},
  data = ktOverlapInspectionTable[];
  If[Length[data] < 2, Return[{}]]; 
  headers = First[data];
  rows = Rest[data];
  ReverseSortBy[rows, ktOverlapInspectionScore[headers, #]&]
];

ktAllInOnePlotPath[ktdatapath_String, index_?NumericQ] := FileNameJoin[
  {
    StringReplace[ktdatapath, RegularExpression["\\.csv$"] -> ""],
    "allinone",
    ToString[Round[index]] <> "allinone.png"
  }
];


plotImageCell[label_String, ktdatapath_, index_] := Module[{numericIndex, plotPath, image},
  numericIndex = toNumericOrMissing[index];
  If[
    !StringQ[ktdatapath] || !NumericQ[numericIndex],
    Return[Column[{Style[label, Bold], Style["No plot available.", Italic, Gray]}]]
  ];
  plotPath = ktAllInOnePlotPath[ktdatapath, numericIndex];
  If[
    !FileExistsQ[plotPath],
    Return[
      Column[
        {
          Style[label, Bold],
          Style["Plot file was not found.", Red],
          Style[plotPath, Small]
        }
      ]
    ]
  ];
  image = Quiet@Check[Import[plotPath], $Failed];
  If[
    ImageQ[image],
    Labeled[Show[image, ImageSize -> 420], Style[label, Bold], Top],
    Column[{Style[label, Bold], Style["Plot file could not be imported.", Red]}]
  ]
];


channelOverlayImageCell[headers_List, row_List] := Module[
  {
    ch1Info, ch2Info, ch1Path, ch2Path, ch1Index, ch2Index, imagePath1, imagePath2,
    image1, image2, overlay, titleLabel
  },
  If[!TrueQ[doubleChannelEnabled], Return[Nothing]];
  titleLabel = Column[{Style["Ch1/Ch2", Bold], Style["channel overlay", Bold]},
    Alignment -> Center, Spacings -> 0];
  ch1Info = channelImageInfo[headers, row, 1];
  ch2Info = channelImageInfo[headers, row, 2];
  If[
    !ListQ[ch1Info] || !ListQ[ch2Info],
    Return[Column[{titleLabel, Style["No channel-overlay image available.", Italic, Gray]}]]
  ];
  {ch1Path, ch1Index} = ch1Info;
  {ch2Path, ch2Index} = ch2Info;
  imagePath1 = ktOriginalImagePath[ch1Path, ch1Index];
  imagePath2 = ktOriginalImagePath[ch2Path, ch2Index];
  If[
    !FileExistsQ[imagePath1] || !FileExistsQ[imagePath2],
    Return[
      Column[
        {
          titleLabel,
          Style["Original image files were not found.", Red],
          Style[imagePath1, Small],
          Style[imagePath2, Small]
        }
      ]
    ]
  ];
  image1 = Quiet@Check[Import[imagePath1], $Failed];
  image2 = Quiet@Check[Import[imagePath2], $Failed];
  If[
    !ImageQ[image1] || !ImageQ[image2],
    Return[Column[{titleLabel, Style["Original images could not be imported.", Red]}]]
  ];
  If[ImageDimensions[image1] =!= ImageDimensions[image2],
    image2 = ImageResize[image2, ImageDimensions[image1]]
  ];
  overlay = Quiet@Check[overlayCh12[image1, image2], $Failed];
  If[
    ImageQ[overlay],
    Labeled[Show[overlay, ImageSize -> 420], titleLabel, Top],
    Column[{titleLabel, Style["Channel-overlay image could not be generated.", Red]}]
  ]
];

measurementAssociation[headers_List, row_List, occurrence_: 1, label_: "Ch1"] := <|
  "Channel" -> label,
  "Condition" -> channelBlockValue[headers, row, {"condition"}, occurrence],
  "Cell index" -> channelBlockValue[headers, row, {"Cell index"}, occurrence],
  "KT pair" -> channelBlockValue[headers, row, {"KT pairs"}, occurrence],
  "KT" -> channelBlockValue[headers, row, {"KT"}, occurrence],
  "Time" -> channelBlockValue[headers, row, {"Time"}, occurrence],
  "Frame index" -> channelBlockValue[headers, row, {"index"}, occurrence],
  "KT area (pixels)" -> channelBlockValue[headers, row, {"ktArea(pixels)"}, occurrence],
  "Width X (um)" -> channelBlockValue[headers, row, widthXAliases, occurrence],
  "Width Y (um)" -> channelBlockValue[headers, row, widthYAliases, occurrence],
  "Orientation (deg)" -> channelBlockValue[headers, row, {"orientationAngle(deg)"}, occurrence],
  "Tail" -> channelBlockValue[headers, row, {"tail or not"}, occurrence],
  "Tail direction" -> channelBlockValue[headers, row, {"tail direction"}, occurrence],
  "2D peak count" -> channelBlockValue[headers, row, number2DPeaksAliases, occurrence],
  "2nd/1st peak ratio" -> channelBlockValue[headers, row, {"ratio 2nd/1st peak"}, occurrence],
  "Total intensity" -> channelBlockValue[headers, row, {"totalIntensity"}, occurrence]
|>;


displayIntermediateAnalysisRows[data_List, maxRows_: 4, sectionTitle_: "Main-program intermediate plots"] := Module[
  {headers, rows, panels, ch1Info, ch2Info, ch1Path, ch2Path, ch1Index, ch2Index, panelTitle},
  If[Length[data] < 2, Return[Style["No rows available.", Italic, Gray]]];
  headers = First[data];
  rows = Take[Rest[data], UpTo[maxRows]];
  panels = MapIndexed[
    (
      ch1Info = channelImageInfo[headers, #1, 1];
      ch2Info = channelImageInfo[headers, #1, 2];
      {ch1Path, ch1Index} = If[ListQ[ch1Info], ch1Info, {Missing["NotAvailable"], Missing["NotAvailable"]}];
      {ch2Path, ch2Index} = If[ListQ[ch2Info], ch2Info, {Missing["NotAvailable"], Missing["NotAvailable"]}];
      panelTitle = If[
        StringMatchQ[sectionTitle, ___ ~~ "row " ~~ DigitCharacter ..],
        sectionTitle,
        sectionTitle <> " " <> ToString[First[#2]]
      ];
      Panel[
        Column[
          {
            Style[panelTitle, Bold, 13],
            Grid[
              {
                DeleteCases[
                  {
                    plotImageCell["Ch1", ch1Path, ch1Index],
                    If[TrueQ[doubleChannelEnabled], plotImageCell["Ch2", ch2Path, ch2Index], Nothing],
                    If[TrueQ[doubleChannelEnabled], channelOverlayImageCell[headers, #1], Nothing]
                  },
                  Nothing
                ]
              },
              Spacings -> {1.2, 1}
            ],
            Dataset[
              DeleteCases[
                {
                  measurementAssociation[headers, #1, 1, "Ch1"],
                  If[TrueQ[doubleChannelEnabled], measurementAssociation[headers, #1, 2, "Ch2"], Nothing]
                },
                Nothing
              ]
            ]
          },
          Spacings -> 0.8
        ]
      ]
    )&,
    rows
  ];
  Column[panels, Spacings -> 1]
];


displayIntermediateAnalysisPreview[maxRows_: 4] := Module[{compiledTable},
  compiledTable = compiledResultTable[];
  displayIntermediateAnalysisRows[compiledTable, maxRows, "Compiled row"]
];


displayIntermediateAnalysisAt[cellIndex_, ktPair_, kt_: All, time_: All, maxRows_: 20] := Module[
  {selected},
  selected = selectCompiledRowsByMetadata[cellIndex, ktPair, kt, time];
  If[
    Length[selected] < 2,
    Style["No matching compiled result rows were found.", Italic, Gray],
    displayIntermediateAnalysisRows[selected, maxRows, "Selected row"]
  ]
];


displayIntermediateAnalysisAtRow[rowIndex_: 1] := Module[
  {data, rows},
  data = compiledResultTable[];
  If[Length[data] < 2, Return[Style["No compiled result rows are available.", Italic, Gray]]];
  rows = Rest[data];
  If[
    !IntegerQ[rowIndex] || rowIndex < 1 || rowIndex > Length[rows],
    Return[
      Style[
        "Index is out of range. Available range: 1-" <> ToString[Length[rows]],
        Red
      ]
    ]
  ];
  displayIntermediateAnalysisRows[
    Join[{First[data]}, {rows[[rowIndex]]}],
    1,
    "Compiled row " <> ToString[rowIndex]
  ]
];


displayIntermediateAnalysisSelector[] := DynamicModule[
  {rowIndex = 1,
   result = Style["Enter a compiled row index and click Check.", Italic, Gray]},
  Column[
    {
      Style[
        Row[{"Available compiled result rows: ", Dynamic[rowCount[compiledResultTable[]]]}],
        Small,
        Gray
      ],
      Grid[
        {
          {"Compiled row index", InputField[Dynamic[rowIndex], Number, FieldSize -> 8]}
        },
        Alignment -> Left,
        Spacings -> {1.2, 0.7}
      ],
      Button[
        "Check",
        With[
          {
            idx = If[NumericQ[rowIndex], Round[rowIndex], rowIndex]
          },
          result = displayIntermediateAnalysisAtRow[idx]
        ],
        Method -> "Queued"
      ],
      Dynamic[result]
    },
    Spacings -> 1
  ]
];


displayIntermediateAnalysisControl[] := displayIntermediateAnalysisSelector[];


displayKTOverlapInspectionPreview[maxRows_: 1] := Module[{data, rows},
  data = ktOverlapInspectionTable[];
  rows = sortedKTOverlapInspectionRows[];
  If[
    Length[data] < 1 || Length[rows] == 0,
    Style["No KT-overlap inspection rows are available.", Italic, Gray],
    displayIntermediateAnalysisRows[
      Join[{First[data]}, Take[rows, UpTo[maxRows]]],
      maxRows,
      "KT-overlap inspection"
    ]
  ]
];


displayKTOverlapCandidatePreview[maxRows_: 1] := Module[{data, candidates},
  data = ktOverlapInspectionTable[];
  candidates = ktOverlapCandidateRows[];
  If[
    Length[data] < 1,
    Style["No KT-overlap inspection rows are available.", Italic, Gray],
    If[
      Length[candidates] == 0,
      displayKTOverlapInspectionPreview[maxRows],
      displayIntermediateAnalysisRows[
        Join[{First[data]}, Take[candidates, UpTo[maxRows]]],
        maxRows,
        "KT-overlap example"
      ]
    ]
  ]
];

displayResultExtractionReport[maxRows_: 50, previewRows_: 4, includeZNCCPreview_: True] := Module[
  {
    compiledTable, movementTable, normalizedTable, ktOverlapTable, registrationTable,
    candidates, outputRows, overview
  },
  compiledTable = compiledResultTable[];
  movementTable = If[ValueQ[movementData] && ListQ[movementData], movementData, {}];
  normalizedTable = If[
    ValueQ[normalizedExtractData] && ListQ[normalizedExtractData],
    normalizedExtractData,
    {}
  ];
  ktOverlapTable = ktOverlapInspectionTable[];
  registrationTable = channelRegistrationTable[];
  candidates = ktOverlapCandidateRows[];
  outputRows = If[
    ValueQ[workflowSummary] && AssociationQ[workflowSummary],
    Cases[Flatten[Lookup[workflowSummary, "Outputs", {}]], _String],
    {}
  ];
  overview = Dataset[{
    <|
      "Channel number" -> channelNumber,
      "Compiled rows" -> rowCount[compiledTable],
      "Movement rows" -> rowCount[movementTable],
      "Normalized rows" -> rowCount[normalizedTable],
      "KT-overlap inspection rows" -> rowCount[ktOverlapTable],
      "Preselected KT-overlap candidates" -> Length[candidates],
      "Ch1/Ch2 registration rows" -> rowCount[registrationTable],
      "Warnings" -> If[ValueQ[workflowWarnings], Length[workflowWarnings], 0],
      "Errors" -> If[ValueQ[workflowErrors], Length[workflowErrors], 0]
    |>
  }];
  Column[
    {
      Style["Result extraction report", Bold, 16],
      overview,
      Style["Exported files", Bold, 14],
      If[
        Length[outputRows] > 0,
        Dataset[Map[<|"Path" -> #|>&, outputRows]],
        Style["No exported files recorded.", Italic, Gray]
      ],
      Style["Compiled results", Bold, 14],
      displayTablePreview[compiledTable, maxRows],
      Style["Compiled Ch1/Ch2 ZNCC fitting vectors", Bold, 14],
      If[
        TrueQ[doubleChannelEnabled],
        displayZNCCVectorTable[maxRows],
        Style["Single-channel mode: no Ch1/Ch2 ZNCC fitting vectors.", Italic, Gray]
      ],
      Style["Main-program intermediate plot check", Bold, 14],
      displayIntermediateAnalysisControl[],
      Style["Compiled-row Ch1/Ch2 ZNCC fitting result", Bold, 14],
      If[
        TrueQ[doubleChannelEnabled] && TrueQ[includeZNCCPreview] &&
          rowCount[registrationTable] > 0,
        displayZNCCChannelRegistrationControl[],
        Style["No double-channel rows are available for Ch1/Ch2 ZNCC fitting.", Italic, Gray]
      ],
      Style["Movement summary", Bold, 14],
      displayTablePreview[movementTable, maxRows],
      Style["Normalized movement summary", Bold, 14],
      displayTablePreview[normalizedTable, maxRows],
      Style["KT-overlap inspection table", Bold, 14],
      displayTablePreview[ktOverlapTable, maxRows],
      Style["Warnings", Bold, 14],
      If[ValueQ[workflowWarnings] && workflowWarnings =!= {}, Dataset[workflowWarnings], "None"],
      Style["Errors", Bold, 14],
      If[ValueQ[workflowErrors] && workflowErrors =!= {}, Dataset[workflowErrors], "None"]
    },
    Spacings -> 1.2
  ]
];


displayZNCCChannelRegistrationControl[] := DynamicModule[
  {
    rowIndex = 1,
    result = Style["Enter a compiled result row and click Check.", Italic, Gray]
  },
  Column[
    {
      Style[
        Row[{"Available compiled result rows: ", Dynamic[rowCount[channelRegistrationTable[]]]}],
        Small,
        Gray
      ],
      Grid[
        {
          {"Compiled row index", InputField[Dynamic[rowIndex], Number, FieldSize -> 8]}
        },
        Alignment -> Left,
        Spacings -> {1.2, 0.7}
      ],
      Button[
        "Check",
        With[
          {
            idx = If[NumericQ[rowIndex], Round[rowIndex], rowIndex]
          },
          result = displayZNCCChannelRegistrationPreview[idx]
        ],
        Method -> "Queued"
      ],
      Dynamic[result]
    },
    Spacings -> 1
  ]
];


displayZNCCChannelRegistrationPreview[rowIndex_: 1, ___] := Module[
  {
    data, rows, row, headers, ch1Info, ch2Info, path1, path2, index1, index2,
    imagePath1, imagePath2, image1, image2,
    rawVectorXUm, rawVectorYUm, rawScore, vectorXUm, vectorYUm, score,
    scale, tx, ty, img1, img2, margin, fixed1, fixed2, originalFixed2,
    shiftOverlay, originalOverlay
  },
  If[
    !TrueQ[doubleChannelEnabled],
    Return[Style["ZNCC fitting requires double-channel data.", Italic, Gray]]
  ];
  data = channelRegistrationTable[];
  If[Length[data] < 2, Return[Style["No compiled result rows are available for Ch1/Ch2 ZNCC fitting.", Italic, Gray]]];
  headers = First[data];
  rows = Rest[data];
  If[
    Length[rows] == 0,
    Return[Style["No compiled result rows are available for Ch1/Ch2 ZNCC fitting.", Italic, Gray]]
  ];
  If[
    !IntegerQ[rowIndex] || rowIndex < 1 || rowIndex > Length[rows],
    Return[
      Style[
        "Index is out of range. Available range: 1-" <> ToString[Length[rows]],
        Red
      ]
    ]
  ];
  row = rows[[rowIndex]];
  rawVectorXUm = rowValueByHeader[
    headers,
    row,
    {"Ch1/Ch2 ZNCC fitting vector X (um)", "znccFittingVectorX(um)"}
  ];
  rawVectorYUm = rowValueByHeader[
    headers,
    row,
    {"Ch1/Ch2 ZNCC fitting vector Y (um)", "znccFittingVectorY(um)"}
  ];
  rawScore = rowValueByHeader[
    headers,
    row,
    {"Ch1/Ch2 ZNCC fitting score", "znccFittingScore"}
  ];
  If[
    MatchQ[rawVectorXUm, _Missing] || MatchQ[rawVectorYUm, _Missing],
    Return[
      Style[
        "Stored Ch1/Ch2 ZNCC fitting vector columns were not found in the compiled result table.",
        Red
      ]
    ]
  ];
  vectorXUm = toNumericOrMissing[rawVectorXUm];
  vectorYUm = toNumericOrMissing[rawVectorYUm];
  score = toNumericOrMissing[rawScore];
  If[
    !NumericQ[vectorXUm] || !NumericQ[vectorYUm],
    Return[
      Column[
        {
          Style[
            "Stored Ch1/Ch2 ZNCC fitting vector is NA for the selected compiled row. Rerun result extraction after loading the updated script.",
            Red
          ],
          Dataset[
            {
              <|
                "Compiled row index" -> rowIndex,
                "X (um)" -> displayCellValue[rawVectorXUm],
                "Y (um)" -> displayCellValue[rawVectorYUm],
                "Score" -> displayCellValue[rawScore]
              |>
            }
          ]
        }
      ]
    ]
  ];
  ch1Info = channelImageInfo[headers, row, 1];
  ch2Info = channelImageInfo[headers, row, 2];
  If[
    !ListQ[ch1Info] || !ListQ[ch2Info],
    Return[Style["Required image path or frame-index columns were not found in the Ch1/Ch2 blocks.", Red]]
  ];
  {path1, index1} = ch1Info;
  {path2, index2} = ch2Info;
  If[
    !StringQ[path1] || !StringQ[path2] || !NumericQ[index1] || !NumericQ[index2],
    Return[Style["Selected row does not contain valid Ch1/Ch2 image paths and indices.", Red]]
  ];
  imagePath1 = ktOriginalImagePath[path1, index1];
  imagePath2 = ktOriginalImagePath[path2, index2];
  If[
    !FileExistsQ[imagePath1] || !FileExistsQ[imagePath2],
    Return[
      Column[
        {
          Style["Original image files were not found.", Red],
          Dataset[{<|"Ch1" -> imagePath1, "Ch2" -> imagePath2|>}]
        }
      ]
    ]
  ];
  image1 = Quiet@Check[Import[imagePath1], $Failed];
  image2 = Quiet@Check[Import[imagePath2], $Failed];
  If[
    !ImageQ[image1] || !ImageQ[image2],
    Return[Style["Original image files could not be imported as images.", Red]]
  ];
  If[ImageDimensions[image1] =!= ImageDimensions[image2],
    image2 = ImageResize[image2, ImageDimensions[image1]]
  ];
  scale = If[NumericQ[znccPixelSizeUm] && znccPixelSizeUm > 0, N[znccPixelSizeUm], 1.];
  {tx, ty} = N@{-vectorXUm/scale, -vectorYUm/scale};
  img1 = ColorConvert[ImageAdjust[image1], "Grayscale"];
  img2 = ColorConvert[ImageAdjust[image2], "Grayscale"];
  margin = Max[0, Ceiling[Max[Abs /@ {tx, ty}] + 2]];
  fixed1 = centerCrop[img1, margin];
  fixed2 = centerCrop[shiftImageSmooth[img2, tx, ty], margin];
  originalFixed2 = centerCrop[img2, margin];
  shiftOverlay = overlayCh12[fixed1, fixed2];
  originalOverlay = overlayCh12[fixed1, originalFixed2];
  Column[
    {
      Style["Compiled-row Ch1/Ch2 ZNCC fitting result", Bold, 16],
      Dataset[{
        <|
          "Compiled row index" -> rowIndex,
          "Source" -> "Stored allKT/compiled results",
          "protein1-to-protein2 vector x (um)" -> vectorXUm,
          "protein1-to-protein2 vector y (um)" -> vectorYUm,
          "applied image shift x (pixel)" -> tx,
          "applied image shift y (pixel)" -> ty,
          "ZNCC score" -> If[NumericQ[score], score, "NA"],
          "Pixel size (um/pixel)" -> znccPixelSizeUm,
          "Ch1 image" -> imagePath1,
          "Ch2 image" -> imagePath2
        |>
      }],
      GraphicsRow[
        {
          Labeled[ImageAdjust[image1], "Ch1"],
          Labeled[ImageAdjust[image2], "Ch2"],
          Labeled[originalOverlay, "Original overlay"],
          Labeled[shiftOverlay, "Stored-vector overlay"]
        },
        ImageSize -> 850
      ]
    },
    Spacings -> 1
  ]
];

(* ::Section:: *)
(* Usage *)


(*
Run the full extraction workflow from a selected cell-set directory

Before running, tune these values in Workflow Configuration when needed:

  windowSize
  configuredChannelNumber
  overlapSeparationIntensityThreshold
  overlapSeparationPixelThreshold
  exportZNCCFittingResults
  znccPixelSizeUm
  useParallelTrackingIndexMap
  maxParallelKernels
  trackingIndexTimeoutSeconds
  closeLaunchedParallelKernels

Set configuredChannelNumber = 1 for single-channel datasets. Single-channel
mode runs Ch1 extraction, movement summaries, and target/background KT signal
overlap filtering, then
skips Ch1/Ch2 ZNCC vector fitting. Set configuredChannelNumber = 2 for
double-channel datasets.

After running, inspect workflowSummary, workflowWarnings, and workflowErrors for
output paths, recoverable warnings, and failed imports/exports.

For notebook review, run:

  displayResultExtractionReport[]

The report shows compiled result tables, a compiled-row Ch1/Ch2 ZNCC fitting
panel, movement tables, normalized movement tables, and the KT-overlap
inspection table. The ZNCC panel and the KT-overlap table are separate outputs.

The report includes Ch1/Ch2 ZNCC and intermediate-plot check panels. Enter a
compiled row index, then click Check to inspect the selected compiled row.

To display intermediate plots for a specific compiled row, run:

  displayIntermediateAnalysisAtRow[rowIndex]

For example:

  displayIntermediateAnalysisAtRow[10]

For an input panel, run:

  displayIntermediateAnalysisSelector[]

Use the Ch1/Ch2 ZNCC registration check panel in displayResultExtractionReport[]
for selected double-channel rows. The panel reads the stored allKT ZNCC vector
and score, then applies that stored vector to the overlay display. It does not
rerun fitting and does not use the target/background KT signal overlap PreSelect
flag.

Width headers from old CSV files are normalized during column lookup, and
exported files use:

  width_X(AUC/max)(um)
  width_Y(AUC/max)(um)

For large datasets, tracking-index mapping can use block-level ParallelMap by
setting useParallelTrackingIndexMap = True after a small serial test run.
I/O-heavy collection steps intentionally remain serial to avoid nested subkernel
contention and filesystem thrashing.
*)

