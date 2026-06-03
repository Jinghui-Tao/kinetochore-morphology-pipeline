(* ::Package:: *)

(* KKCalculationAndRotation_professional.wl

   This script computes kinetochore-kinetochore geometry from TrackMate exports,
   writes the angle and distance tables used by downstream analysis, and optionally rotates central-z projection movies by the computed
   KK angle.  The public entry point is runKKCalculationAndRotation[].
*)


ClearAll[
  failureQ, ensureDirectory, safeImport, safeExport, parseNumber,
  parseTrackLabel, numericSpotQ, trackKey, frameKey, coordinateRow,
  readTrackMateSpots, readPairLabels, trackLabelToIDMap,
  groupedSpotsByTrackID, trackFramesAssociation, angleDegrees,
  vectorRowsForPair, trackRowsWithFrames, movementDistances,
  angleRowsForTrackFrames, kkCorrelationForPair, buildPairMetrics,
  resolvePairTrackIDs, pairMetricStatus, safeBuildPairMetrics,
  diagnoseKKCell,
  pairDirectory, exportPairMetricsForChannel, processKKCell,
  getIJDims, imagesCZT, centralZRange, zProjectionMovie,
  ktIndexFromFile, readAngles, cleanDirFiles, updateProjectionMetadata,
  exportRotatedFrames, discoverRotationJobs, rotateKTMovie,
  prepareParallelKernels, mapJobs, runKKCalculationAndRotation,
  $KKCalculationAndRotationVersion
];

$KKCalculationAndRotationVersion = "2026-06-02-pair-rows-missing-fix";


(* Returns True for structured failures propagated by the pipeline. *)
failureQ[expr_] := MatchQ[expr, _Failure];


(* Ensures that an output directory exists before a file is written. *)
ensureDirectory[dir_String] := If[! DirectoryQ[dir],
  CreateDirectory[dir, CreateIntermediateDirectories -> True],
  dir
];


(* Imports a file and converts common I/O messages into a returned Failure. *)
safeImport[path_String, rest___] := Module[{result},
  If[! FileExistsQ[path],
    Return[Failure["FileMissing", <|"Path" -> path|>]]
  ];

  result = Quiet @ Check[Import[path, rest], $Failed];

  If[result === $Failed,
    Failure["ImportFailed", <|"Path" -> path|>],
    result
  ]
];


(* Exports a file after creating its parent directory. *)
safeExport[path_String, expr_, rest___] := Module[{dir, result},
  dir = DirectoryName[path];
  If[StringQ[dir] && dir =!= "", ensureDirectory[dir]];

  result = Quiet @ Check[Export[path, expr, rest], $Failed];

  If[result === $Failed,
    Failure["ExportFailed", <|"Path" -> path|>],
    result
  ]
];


(* Parses numeric table entries without modifying nonnumeric identifiers. *)
parseNumber[value_] := Which[
  NumberQ[value], value,
  MatchQ[value, _Missing], Missing["NotAvailable"],
  StringQ[value] && StringMatchQ[StringTrim[value], NumberString],
    Quiet @ Check[ToExpression[StringTrim[value]], Missing["NotNumeric"]],
  True, Missing["NotNumeric"]
];


(* Extracts the integer track label from TrackMate labels such as Track_12. *)
parseTrackLabel[label_] := Module[{matches},
  matches = StringCases[ToString[label], "Track_" ~~ (digits : DigitCharacter ..) :> digits];
  If[matches === {},
    Missing["TrackLabelUnavailable"],
    ToExpression[First[matches]]
  ]
];


(* Checks whether a TrackMate spot row contains the fields required for KK metrics. *)
numericSpotQ[spot_Association] := And @@ (NumericQ[Lookup[spot, #]] & /@
  {"TrackID", "X", "Y", "Z", "Frame"});


(* Normalizes TrackMate's numeric track IDs for association keys. *)
trackKey[value_] := IntegerPart[value];


(* Normalizes frame values for association keys. *)
frameKey[value_] := IntegerPart[value];


(* Returns a {x,y,z,frame} row for location exports. *)
coordinateRow[spot_Association] := Lookup[spot, {"X", "Y", "Z", "Frame"}];


(* Reads the TrackMate export.csv table into typed spot associations. *)
readTrackMateSpots[trackingPath_String] := Module[
  {raw, rows, spots},

  raw = safeImport[trackingPath];
  If[failureQ[raw], Return[raw]];
  If[! MatrixQ[raw],
    Return[Failure["InvalidTrackingTable", <|"Path" -> trackingPath|>]]
  ];

  rows = Drop[raw, 4];
  If[rows === {},
    Return[Failure["EmptyTrackingTable", <|"Path" -> trackingPath|>]]
  ];

  spots = Map[
    Function[row,
      Module[{numeric = parseNumber /@ Rest[row]},
        If[Length[row] < 9,
          Failure["ShortTrackingRow", <|"Row" -> row|>],
          <|
            "TrackLabel" -> ToString[First[row]],
            "TrackLabelNumber" -> parseTrackLabel[First[row]],
            "TrackID" -> numeric[[2]],
            "X" -> numeric[[4]],
            "Y" -> numeric[[5]],
            "Z" -> numeric[[6]],
            "Frame" -> numeric[[8]],
            "RawRow" -> row
          |>
        ]
      ]
    ],
    rows
  ];

  spots = Select[spots, AssociationQ[#] && numericSpotQ[#] &];
  If[spots === {},
    Failure["NoUsableTrackingRows", <|"Path" -> trackingPath|>],
    spots
  ]
];


(* Reads the manually curated sister-pair table from pairs.xlsx. *)
readPairLabels[pairsPath_String] := Module[{raw, rows, pairs},
  raw = safeImport[pairsPath];
  If[failureQ[raw], Return[raw]];

  rows = Which[
    MatrixQ[raw], raw,
    ListQ[raw] && AnyTrue[raw, MatrixQ], Flatten[Select[raw, MatrixQ], 1],
    True, Select[Flatten[raw, 1], ListQ[#] && Length[#] >= 2 &]
  ];
  rows = Select[rows, ListQ[#] && Length[#] >= 2 &];

  pairs = Cases[
    rows,
    row_ :> With[{values = parseNumber /@ row[[1 ;; 2]]},
      If[VectorQ[values, NumericQ], IntegerPart /@ values, Nothing]
    ]
  ];

  If[pairs === {},
    Failure["NoPairLabels", <|"Path" -> pairsPath|>],
    pairs
  ]
];


(* Maps TrackMate labels to numeric track IDs, with track IDs accepted directly. *)
trackLabelToIDMap[spots_List] := Association @ DeleteDuplicatesBy[
  Join[
    Cases[
      spots,
      spot_Association /; NumericQ[spot["TrackLabelNumber"]] && NumericQ[spot["TrackID"]] :>
        (trackKey[spot["TrackLabelNumber"]] -> trackKey[spot["TrackID"]])
    ],
    Cases[
      spots,
      spot_Association /; NumericQ[spot["TrackID"]] :>
        (trackKey[spot["TrackID"]] -> trackKey[spot["TrackID"]])
    ]
  ],
  First
];


(* Groups spots by numeric TrackMate track ID and sorts each track by frame. *)
groupedSpotsByTrackID[spots_List] := GroupBy[
  spots,
  trackKey[#["TrackID"]] &,
  SortBy[#, frameKey[#["Frame"]] &] &
];


(* Creates a frame-indexed association for one track. *)
trackFramesAssociation[trackRows_List] := Association @ Map[
  frameKey[#["Frame"]] -> # &,
  trackRows
];


(* Computes the image rotation angle from an XY vector. *)
angleDegrees[vector_List] := -ArcTan[vector[[1]], vector[[2]]] / Degree;


(* Returns frame-wise vector and position rows for a sister pair. *)
vectorRowsForPair[trackA_List, trackB_List] := Module[
  {framesA, framesB, overlap, rowsA, rowsB},

  framesA = trackFramesAssociation[trackA];
  framesB = trackFramesAssociation[trackB];
  overlap = Sort @ Intersection[Keys[framesA], Keys[framesB]];

  Table[
    rowsA = framesA[frame];
    rowsB = framesB[frame];
    With[
      {
        a = Lookup[rowsA, {"X", "Y", "Z"}],
        b = Lookup[rowsB, {"X", "Y", "Z"}]
      },
      <|
        "Frame" -> frame,
        "A" -> a,
        "B" -> b,
        "VectorAB" -> (b - a),
        "VectorBA" -> (a - b),
        "DistanceXY" -> Norm[(b - a)[[1 ;; 2]]],
        "DistanceXYZ" -> Norm[b - a],
        "AngleAB" -> angleDegrees[(b - a)[[1 ;; 2]]],
        "AngleBA" -> angleDegrees[(a - b)[[1 ;; 2]]]
      |>
    ],
    {frame, overlap}
  ]
];


(* Returns {x,y,z,frame} rows for a sorted track. *)
trackRowsWithFrames[trackRows_List] := coordinateRow /@ SortBy[trackRows, frameKey[#["Frame"]] &];


(* Computes frame-to-frame XY movement lengths for one track. *)
movementDistances[trackRows_List] := Module[{xy},
  xy = ({#["X"], #["Y"]} & /@ SortBy[trackRows, frameKey[#["Frame"]] &]);
  If[Length[xy] < 2, {}, Norm /@ Differences[xy]]
];


(* Aligns angle values to the frames in which a single track is visible. *)
angleRowsForTrackFrames[trackRows_List, pairRows_List, angleKey_String] := Module[
  {angleByFrame, frames},

  angleByFrame = Association @ Cases[
    pairRows,
    row_Association :> (row["Frame"] -> row[angleKey])
  ];

  frames = frameKey /@ (#["Frame"] & /@ SortBy[trackRows, frameKey[#["Frame"]] &]);
  Lookup[angleByFrame, frames, "NA"]
];


(* Computes the 3D co-movement correlation between sister KTs. *)
kkCorrelationForPair[pairRows_List] := Module[
  {a, b, num, den},

  If[Length[pairRows] < 2, Return["NA"]];

  a = #["A"] & /@ pairRows;
  b = #["B"] & /@ pairRows;

  num =
    Covariance[a[[All, 1]], b[[All, 1]]] +
    Covariance[a[[All, 2]], b[[All, 2]]] +
    Covariance[a[[All, 3]], b[[All, 3]]];

  den =
    Sqrt[Variance[a[[All, 1]]] + Variance[a[[All, 2]]] + Variance[a[[All, 3]]]] *
    Sqrt[Variance[b[[All, 1]]] + Variance[b[[All, 2]]] + Variance[b[[All, 3]]]];

  If[! NumericQ[den] || den == 0, "NA", num / den]
];


(* Builds all output tables for one curated sister pair. *)
buildPairMetrics[pairLabels_, labelToID_, groupedTracks_] := Module[
  {labelA, labelB, idA, idB, trackA, trackB, rows, resolveTrackID},

  {labelA, labelB} = trackKey /@ pairLabels;
  resolveTrackID[label_] := Lookup[
    labelToID,
    label,
    If[KeyExistsQ[groupedTracks, label],
      label,
      Missing["TrackIDUnavailable", label]
    ]
  ];

  idA = resolveTrackID[labelA];
  idB = resolveTrackID[labelB];

  If[! FreeQ[{idA, idB}, _Missing],
    Return[Failure["PairTrackMissing", <|"Pair" -> pairLabels, "TrackIDs" -> {idA, idB}|>]]
  ];

  trackA = Lookup[groupedTracks, idA, Missing["TrackRowsUnavailable", idA]];
  trackB = Lookup[groupedTracks, idB, Missing["TrackRowsUnavailable", idB]];

  (* Only treat the whole Lookup result as missing.
     Do not use FreeQ here: individual spot rows may contain unrelated Missing
     values such as TrackLabelNumber -> Missing[...], while the track rows
     themselves are still valid. *)
  If[MatchQ[trackA, _Missing] || MatchQ[trackB, _Missing],
    Return[Failure["PairRowsMissing", <|"Pair" -> pairLabels, "TrackIDs" -> {idA, idB}|>]]
  ];

  rows = vectorRowsForPair[trackA, trackB];

  <|
    "Labels" -> {labelA, labelB},
    "TrackIDs" -> {idA, idB},
    "PairRows" -> rows,
    "VectorXYZ" -> (#["VectorAB"] & /@ rows),
    "VectorXYZWithFrames" -> (Lookup[#, "VectorAB"] ~Join~ {#["Frame"]} & /@ rows),
    "DistanceXY" -> (#["DistanceXY"] & /@ rows),
    "DistanceXYWithFrames" -> ({#["DistanceXY"], #["Frame"]} & /@ rows),
    "AngleA" -> angleRowsForTrackFrames[trackA, rows, "AngleAB"],
    "AngleB" -> angleRowsForTrackFrames[trackB, rows, "AngleBA"],
    "AngleAWithFrame" -> ({#["AngleAB"], #["Frame"]} & /@ rows),
    "AngleBWithFrame" -> ({#["AngleBA"], #["Frame"]} & /@ rows),
    "TrackableFramesA" -> (frameKey /@ (#["Frame"] & /@ trackA)),
    "TrackableFramesB" -> (frameKey /@ (#["Frame"] & /@ trackB)),
    "MovementA" -> movementDistances[trackA],
    "MovementB" -> movementDistances[trackB],
    "LocationA" -> trackRowsWithFrames[trackA],
    "LocationB" -> trackRowsWithFrames[trackB],
    "Correlation3D" -> kkCorrelationForPair[rows]
  |>
];


(* Resolves curated pair labels to TrackMate track IDs, accepting direct track IDs as fallback. *)
resolvePairTrackIDs[pairLabels_, labelToID_, groupedTracks_] := Module[
  {labels},

  labels = trackKey /@ pairLabels;
  Lookup[
    labelToID,
    #,
    If[KeyExistsQ[groupedTracks, #], #, Missing["TrackIDUnavailable", #]]
  ] & /@ labels
];


(* Compact per-pair diagnostic: this is safe to paste into Codex/ChatGPT. *)
pairMetricStatus[result_, pairLabels_, labelToID_, groupedTracks_] := Module[
  {labels, ids, trackA, trackB, framesA, framesB, overlap, resultType, failureTag, failureInfo, resultHead},

  labels = trackKey /@ pairLabels;
  ids = resolvePairTrackIDs[labels, labelToID, groupedTracks];
  trackA = If[! MatchQ[ids[[1]], _Missing], Lookup[groupedTracks, ids[[1]], Missing["TrackRowsUnavailable", ids[[1]]]], Missing["TrackRowsUnavailable"]];
  trackB = If[! MatchQ[ids[[2]], _Missing], Lookup[groupedTracks, ids[[2]], Missing["TrackRowsUnavailable", ids[[2]]]], Missing["TrackRowsUnavailable"]];

  framesA = If[ListQ[trackA], Sort @ DeleteDuplicates[frameKey /@ (Lookup[#, "Frame"] & /@ trackA)], {}];
  framesB = If[ListQ[trackB], Sort @ DeleteDuplicates[frameKey /@ (Lookup[#, "Frame"] & /@ trackB)], {}];
  overlap = Sort @ Intersection[framesA, framesB];

  resultType = Which[
    AssociationQ[result], "Association",
    failureQ[result], "Failure",
    True, "Other"
  ];
  failureTag = If[failureQ[result], result[[1]], Missing["NotFailure"]];
  failureInfo = If[failureQ[result], result[[2]], <||>];
  resultHead = ToString[Head[result], InputForm];

  <|
    "Pair" -> labels,
    "ResolvedTrackIDs" -> ids,
    "TracksAvailable" -> (If[MatchQ[#, _Missing], False, KeyExistsQ[groupedTracks, #]] & /@ ids),
    "TrackLengths" -> {If[ListQ[trackA], Length[trackA], 0], If[ListQ[trackB], Length[trackB], 0]},
    "FrameOverlapCount" -> Length[overlap],
    "FrameOverlapSample" -> Take[overlap, UpTo[10]],
    "MetricResultType" -> resultType,
    "MetricResultHead" -> resultHead,
    "FailureTag" -> failureTag,
    "FailureInfo" -> failureInfo
  |>
];


(* Builds one pair while converting unexpected messages/non-evaluation into compact Failures. *)
safeBuildPairMetrics[pairLabels_, labelToID_, groupedTracks_] := Module[
  {result},

  result = Quiet @ Check[
    buildPairMetrics[pairLabels, labelToID, groupedTracks],
    Failure["BuildPairMetricsError", <|"Pair" -> pairLabels|>]
  ];

  If[AssociationQ[result] || failureQ[result],
    result,
    Failure["BuildPairMetricsDidNotEvaluate", <|
      "Pair" -> pairLabels,
      "ResultHead" -> ToString[Head[result], InputForm]
    |>]
  ]
];


(* Reports how one cell folder was parsed before metrics are exported. *)
diagnoseKKCell[cellDir_String] := Module[
  {trackingPath, pairPath, spots, pairs, labelToID, groupedTracks, pairStatus},

  trackingPath = FileNameJoin[{cellDir, "export.csv"}];
  pairPath = FirstCase[
    FileNames[{"pairs.xlsx", "Pairs.xlsx"}, cellDir, 1],
    _String,
    Missing["PairsFileUnavailable"]
  ];

  If[MatchQ[pairPath, _Missing],
    Return[Failure["PairsFileMissing", <|"CellDirectory" -> cellDir|>]]
  ];

  spots = readTrackMateSpots[trackingPath];
  If[failureQ[spots], Return[spots]];

  pairs = readPairLabels[pairPath];
  If[failureQ[pairs], Return[pairs]];

  labelToID = trackLabelToIDMap[spots];
  groupedTracks = groupedSpotsByTrackID[spots];
  pairStatus = pairMetricStatus[Missing["NotBuilt"], #, labelToID, groupedTracks] & /@ pairs;

  <|
    "CellDirectory" -> cellDir,
    "SpotCount" -> Length[spots],
    "Pairs" -> pairs,
    "LabelToIDSample" -> Take[Normal[labelToID], UpTo[20]],
    "GroupedTrackIDs" -> Sort[Keys[groupedTracks]],
    "PairStatus" -> pairStatus
  |>
];


(* Returns the output folder for one sister pair in one channel. *)
pairDirectory[channelDir_String, labels_List] := FileNameJoin[
  {channelDir, ToString[labels[[1]]] <> "-" <> ToString[labels[[2]]]}
];


(* Writes all KK output tables for one sister pair into one channel folder. *)
exportPairMetricsForChannel[channelDir_String, metrics_Association] := Module[
  {labels, pairDir, labelA, labelB, ktADir, ktBDir, pairToken},

  labels = metrics["Labels"];
  {labelA, labelB} = labels;
  pairToken = ToString[labelA] <> "-" <> ToString[labelB];
  pairDir = pairDirectory[channelDir, labels];
  ktADir = FileNameJoin[{pairDir, ToString[labelA], "(+-1)"}];
  ktBDir = FileNameJoin[{pairDir, ToString[labelB], "(+-1)"}];

  ensureDirectory[ktADir];
  ensureDirectory[ktBDir];

  {
    safeExport[FileNameJoin[{pairDir, "kkVECTOR" <> pairToken <> ".csv"}], metrics["VectorXYZ"]],
    safeExport[FileNameJoin[{pairDir, "(number)kkDistance" <> pairToken <> ".csv"}], metrics["DistanceXY"]],
    safeExport[FileNameJoin[{pairDir, "(test)kkVECTORwithFramesNumberOnly" <> pairToken <> ".csv"}], metrics["VectorXYZWithFrames"]],
    safeExport[FileNameJoin[{pairDir, "(number)kkDistanceWithFramesNumberOnly" <> pairToken <> ".csv"}], metrics["DistanceXYWithFrames"]],
    safeExport[FileNameJoin[{pairDir, "kkAngle" <> ToString[labelA] <> ".csv"}], metrics["AngleA"]],
    safeExport[FileNameJoin[{pairDir, "kkAngle" <> ToString[labelB] <> ".csv"}], metrics["AngleB"]],
    safeExport[FileNameJoin[{pairDir, "kkAngleWithFrame" <> ToString[labelA] <> ".csv"}], metrics["AngleAWithFrame"]],
    safeExport[FileNameJoin[{pairDir, "kkAngleWithFrame" <> ToString[labelB] <> ".csv"}], metrics["AngleBWithFrame"]],
    safeExport[FileNameJoin[{pairDir, ToString[labelA], "TrackableFrames-" <> ToString[labelA] <> ".csv"}], metrics["TrackableFramesA"]],
    safeExport[FileNameJoin[{pairDir, ToString[labelB], "TrackableFrames-" <> ToString[labelB] <> ".csv"}], metrics["TrackableFramesB"]],
    safeExport[FileNameJoin[{pairDir, "movementDisplacement-" <> ToString[labelA] <> ".csv"}], metrics["MovementA"]],
    safeExport[FileNameJoin[{pairDir, "movementDisplacement-" <> ToString[labelB] <> ".csv"}], metrics["MovementB"]],
    safeExport[FileNameJoin[{ktADir, "KTlocation_" <> ToString[labelA] <> "self.csv"}], metrics["LocationA"]],
    safeExport[FileNameJoin[{ktADir, "KTlocation_" <> ToString[labelA] <> "sister.csv"}], metrics["LocationB"]],
    safeExport[FileNameJoin[{ktBDir, "KTlocation_" <> ToString[labelB] <> "self.csv"}], metrics["LocationB"]],
    safeExport[FileNameJoin[{ktBDir, "KTlocation_" <> ToString[labelB] <> "sister.csv"}], metrics["LocationA"]]
  }
];


(* Processes one cell folder: TrackMate export, curated pairs, and channel outputs. *)
Options[processKKCell] = {"ChannelCount" -> 1};

processKKCell[cellDir_String, OptionsPattern[]] := Module[
  {
    channelCount, trackingPath, pairPath, spots, pairs, labelToID, groupedTracks,
    metrics, validMetrics, metricStatus, channelDirs, correlationRows
  },

  channelCount = OptionValue["ChannelCount"];
  trackingPath = FileNameJoin[{cellDir, "export.csv"}];
  pairPath = FirstCase[
    FileNames[{"pairs.xlsx", "Pairs.xlsx"}, cellDir, 1],
    _String,
    Missing["PairsFileUnavailable"]
  ];

  If[MatchQ[pairPath, _Missing],
    Return[Failure["PairsFileMissing", <|"CellDirectory" -> cellDir|>]]
  ];

  spots = readTrackMateSpots[trackingPath];
  If[failureQ[spots], Return[spots]];

  pairs = readPairLabels[pairPath];
  If[failureQ[pairs], Return[pairs]];

  labelToID = trackLabelToIDMap[spots];
  groupedTracks = groupedSpotsByTrackID[spots];
  metrics = safeBuildPairMetrics[#, labelToID, groupedTracks] & /@ pairs;
  validMetrics = Cases[metrics, _Association];
  metricStatus = MapThread[pairMetricStatus[#1, #2, labelToID, groupedTracks] &, {metrics, pairs}];

  If[validMetrics === {},
    Return[Failure["NoValidPairs", <|
      "CellDirectory" -> cellDir,
      "PairCount" -> Length[pairs],
      "PairMetricStatus" -> metricStatus,
      "Diagnostics" -> diagnoseKKCell[cellDir]
    |>]]
  ];

  channelDirs = FileNameJoin[{cellDir, "ch" <> ToString[#]}] & /@ Range[channelCount];
  Scan[ensureDirectory, channelDirs];

  Scan[
    Function[channelDir,
      Scan[exportPairMetricsForChannel[channelDir, #] &, validMetrics];
      correlationRows = Prepend[
        ({#["Labels"][[1]], #["Labels"][[2]], #["Correlation3D"]} & /@ validMetrics),
        {"KT1", "KT2", "kk3DCorrelation"}
      ];
      safeExport[FileNameJoin[{channelDir, "kkCorrelation.csv"}], correlationRows];
    ],
    channelDirs
  ];

  <|
    "CellDirectory" -> cellDir,
    "Status" -> "OK",
    "PairCount" -> Length[validMetrics],
    "SkippedPairCount" -> Count[metrics, _Failure],
    "PairMetricStatus" -> metricStatus,
    "ChannelDirectories" -> channelDirs
  |>
];


(* Extracts channel, z-slice, and frame counts from ImageJ TIFF metadata. *)
getIJDims[path_String] := Module[{meta, desc, getInt},
  meta = safeImport[path, "MetaInformation"];
  If[failureQ[meta],
    Return[Failure["ImageMetadataUnavailable", <|"Path" -> path|>]]
  ];

  desc = Lookup[Lookup[meta, "Exif", <||>], "ImageDescription", ""];
  getInt[key_, default_] := Module[{matches},
    matches = StringCases[ToString[desc], (key <> "=") ~~ digits : DigitCharacter .. :> digits];
    If[matches === {}, default, ToExpression[First[matches]]]
  ];

  <|
    "Channels" -> getInt["channels", 1],
    "Slices" -> getInt["slices", 1],
    "Frames" -> getInt["frames", 1]
  |>
];


(* Imports an ImageJ TIFF as frames x z-slices for one channel. *)
imagesCZT[path_String, cPick_: 1] := Module[{imgs, dims, c, z, t, byC, byZ},
  imgs = safeImport[path, "ImageList"];
  If[failureQ[imgs], Return[imgs]];

  dims = getIJDims[path];
  If[failureQ[dims], Return[dims]];

  {c, z, t} = Lookup[dims, {"Channels", "Slices", "Frames"}];
  If[Length[imgs] != c z t,
    Return[Failure["ImagePageCountMismatch", <|
      "Path" -> path,
      "Expected" -> c z t,
      "Observed" -> Length[imgs]
    |>]]
  ];

  If[cPick < 1 || cPick > c,
    Return[Failure["ImageChannelUnavailable", <|"Path" -> path, "Channel" -> cPick|>]]
  ];

  byC = Partition[imgs, c];
  byZ = Partition[byC, z];
  Map[Map[#[[cPick]] &, #] &, byZ]
];


(* Selects the central z-slices used for the (+-1) projection. *)
centralZRange[slices_Integer] := Module[{first, last},
  first = Floor[slices / 2];
  last = Ceiling[(slices + 2) / 2];
  Range[Max[1, first], Min[slices, last]]
];


(* Builds one projection image for each frame from selected z-slices. *)
zProjectionMovie[framesByZ_List, zIdx_List] := Module[{slices, nonEmpty},
  Table[
    slices = framesByZ[[frame, zIdx]];
    nonEmpty = Count[
      slices,
      img_ /; Quiet @ Check[Total[Flatten[ImageData[img]]], 0] > 0
    ];
    If[nonEmpty == 0,
      First[slices],
      (ImageAdd @@ slices) / nonEmpty
    ],
    {frame, Length[framesByZ]}
  ]
];


(* Extracts the KT index token from movie filenames ending in _<KT>.tif. *)
ktIndexFromFile[file_String] := Module[{tokens},
  tokens = StringSplit[
    StringReplace[FileNameTake[file], "_" -> "."],
    RegularExpression["[\\./-]+"]
  ];
  If[Length[tokens] >= 2, tokens[[-2]], ""]
];


(* Reads a one-column kkAngle CSV and keeps nonnumeric entries as NA. *)
readAngles[path_String] := Module[{tbl, values},
  tbl = safeImport[path];
  If[failureQ[tbl], Return[tbl]];
  If[! ListQ[tbl] || tbl === {},
    Return[Failure["EmptyAngleTable", <|"Path" -> path|>]]
  ];

  values = If[MatrixQ[tbl], tbl[[All, 1]], Flatten[tbl]];
  parseNumber /@ values
];


(* Removes generated files from a directory while preserving subdirectories. *)
cleanDirFiles[dir_String] := Module[{},
  ensureDirectory[dir];
  Scan[
    If[FileType[#] === File, Quiet @ DeleteFile[#]] &,
    FileNames["*", dir]
  ];
  dir
];


(* Updates ImageJ metadata after selecting central z-slices. *)
updateProjectionMetadata[meta_, dims_Association, zIdx_List] := Module[
  {updated = meta, desc, newDesc},

  If[! AssociationQ[updated], Return[meta]];
  If[! KeyExistsQ[updated, "Exif"] || ! AssociationQ[updated["Exif"]],
    Return[meta]
  ];

  desc = Lookup[updated["Exif"], "ImageDescription", ""];
  newDesc = StringReplace[
    ToString[desc],
    {
      RegularExpression["slices=\\d+"] -> "slices=" <> ToString[Length[zIdx]],
      RegularExpression["images=\\d+"] -> "images=" <>
        ToString[Length[zIdx] * dims["Channels"] * dims["Frames"]]
    }
  ];

  ReplacePart[updated, {Key["Exif"], Key["ImageDescription"]} -> newDesc]
];


(* Rotates and exports projected frames with the frame-aligned KK angle table. *)
Options[exportRotatedFrames] = {"CropFraction" -> 0.7};

exportRotatedFrames[imgs_List, angles_List, outDir_String, base_String, OptionsPattern[]] := Module[
  {n, cropFraction, angle, out},

  ensureDirectory[outDir];
  n = Min[Length[imgs], Length[angles]];
  cropFraction = OptionValue["CropFraction"];

  Table[
    angle = angles[[frame]];
    If[NumericQ[angle],
      out = FileNameJoin[{outDir, base <> "_" <> ToString[frame] <> ".tif"}];
      safeExport[
        out,
        ImageCrop[
          ImageRotate[imgs[[frame]], -angle Degree],
          IntegerPart[Min[ImageDimensions[imgs[[frame]]]] * cropFraction]
        ],
        "BitDepth" -> 32
      ],
      Nothing
    ],
    {frame, n}
  ]
];


(* Discovers channel/pair/KT/movie jobs available for rotation. *)
discoverRotationJobs[cellDir_String] := Module[
  {channelFolders, moviesList, pairFolders, ktSubfolders},

  channelFolders = Select[FileNames["ch*", cellDir, 1], DirectoryQ];
  If[channelFolders === {},
    channelFolders = Select[FileNames["*", cellDir, 1], DirectoryQ]
  ];

  Flatten[
    Table[
      moviesList = Select[FileNames[{"*.tif", "*.tiff"}, channelFolder, 1], FileType[#] === File &];
      pairFolders = Select[FileNames["*", channelFolder, 1], DirectoryQ];
      Table[
        ktSubfolders = Select[FileNames["*", pairFolder, 1], DirectoryQ];
        Table[
          With[{ktIndex = FileNameTake[ktFolder]},
            Table[
              <|
                "CellDirectory" -> cellDir,
                "ChannelDirectory" -> channelFolder,
                "PairDirectory" -> pairFolder,
                "KTDirectory" -> ktFolder,
                "KTIndex" -> ktIndex,
                "MoviePath" -> movie
              |>,
              {movie, Select[
                moviesList,
                StringMatchQ[
                  FileNameTake[#],
                  ___ ~~ "_" ~~ ToString[ktIndex] ~~ (".tif" | ".tiff"),
                  IgnoreCase -> True
                ] &
              ]}
            ]
          ],
          {ktFolder, ktSubfolders}
        ],
        {pairFolder, pairFolders}
      ],
      {channelFolder, channelFolders}
    ],
    Infinity
  ]
];


(* Rotates one KT movie using the corresponding kkAngle<KT>.csv table. *)
Options[rotateKTMovie] = {
  "ImageChannel" -> 1,
  "CropFraction" -> 0.7,
  "ExportCentralStack" -> True
};

rotateKTMovie[job_Association, OptionsPattern[]] := Module[
  {
    movie, pairPath, ktIndex, outputDir, plusMinusDir, framesByZ, dims,
    zIdx, projection, angleCSV, angles, baseName, meta, updatedMeta
  },

  movie = job["MoviePath"];
  pairPath = job["PairDirectory"];
  ktIndex = job["KTIndex"];
  outputDir = FileNameJoin[{pairPath, ktIndex}];
  plusMinusDir = FileNameJoin[{outputDir, "(+-1)"}];
  cleanDirFiles[plusMinusDir];

  framesByZ = imagesCZT[movie, OptionValue["ImageChannel"]];
  If[failureQ[framesByZ], Return[framesByZ]];

  dims = getIJDims[movie];
  If[failureQ[dims], Return[dims]];

  zIdx = centralZRange[dims["Slices"]];

  If[TrueQ[OptionValue["ExportCentralStack"]],
    meta = safeImport[movie, "MetaInformation"];
    updatedMeta = If[failureQ[meta], meta, updateProjectionMetadata[meta, dims, zIdx]];
    If[failureQ[updatedMeta],
      safeExport[FileNameJoin[{outputDir, "(+-1).tif"}], Flatten[framesByZ[[All, zIdx]]], "BitDepth" -> 32],
      safeExport[
        FileNameJoin[{outputDir, "(+-1).tif"}],
        Flatten[framesByZ[[All, zIdx]]],
        "BitDepth" -> 32,
        "MetaInformation" -> updatedMeta
      ]
    ];
  ];

  projection = zProjectionMovie[framesByZ, zIdx];
  angleCSV = FileNameJoin[{pairPath, "kkAngle" <> ToString[ktIndex] <> ".csv"}];
  angles = readAngles[angleCSV];
  If[failureQ[angles], Return[angles]];

  baseName = FileBaseName[movie];
  exportRotatedFrames[
    projection,
    angles,
    plusMinusDir,
    baseName,
    "CropFraction" -> OptionValue["CropFraction"]
  ];

  <|
    "MoviePath" -> movie,
    "KTIndex" -> ktIndex,
    "Status" -> "OK",
    "OutputDirectory" -> plusMinusDir,
    "FrameCount" -> Min[Length[projection], Length[angles]]
  |>
];


(* Starts additional kernels only when explicit parallel execution is requested. *)
prepareParallelKernels[kernels_] := Module[{target},
  target = Replace[kernels, Automatic :> $ProcessorCount];
  If[IntegerQ[target] && target > 1,
    While[Length[Kernels[]] < target, LaunchKernels[]]
  ];
];


(* Maps independent jobs serially or through ParallelMap with shared definitions. *)
Options[mapJobs] = {"Parallel" -> False, "Kernels" -> Automatic};

mapJobs[items_List, func_, OptionsPattern[]] := Module[{useParallel, kernels},
  useParallel = TrueQ[OptionValue["Parallel"]];
  kernels = OptionValue["Kernels"];

  If[useParallel && Length[items] > 1,
    prepareParallelKernels[kernels];
    DistributeDefinitions[
      failureQ, ensureDirectory, safeImport, safeExport, parseNumber,
      parseTrackLabel, numericSpotQ, trackKey, frameKey, coordinateRow,
      readTrackMateSpots, readPairLabels, trackLabelToIDMap,
      groupedSpotsByTrackID, trackFramesAssociation, angleDegrees,
      vectorRowsForPair, trackRowsWithFrames, movementDistances,
      angleRowsForTrackFrames, kkCorrelationForPair, buildPairMetrics,
      resolvePairTrackIDs, pairMetricStatus, safeBuildPairMetrics, diagnoseKKCell, pairDirectory, exportPairMetricsForChannel, processKKCell,
      getIJDims, imagesCZT, centralZRange, zProjectionMovie,
      ktIndexFromFile, readAngles, cleanDirFiles, updateProjectionMetadata,
      exportRotatedFrames, discoverRotationJobs, rotateKTMovie
    ];
    ParallelMap[func, items],
    Map[func, items]
  ]
];


(* Runs the complete KK table generation and optional image-rotation pipeline. *)
Options[runKKCalculationAndRotation] = {
  "ChannelCount" -> 1,
  "RunRotation" -> True,
  "ImageChannel" -> 1,
  "CropFraction" -> 0.7,
  "ExportCentralStack" -> True,
  "Parallel" -> False,
  "Kernels" -> Automatic
};

runKKCalculationAndRotation[rootDir_: Automatic, OptionsPattern[]] := Module[
  {
    root, cellDirs, kkResults, rotationJobs, rotationResults,
    channelCount, runRotation, imageChannel, cropFraction, exportCentralStack,
    useParallel, kernels
  },

  root = Replace[
    rootDir,
    Automatic :> SystemDialogInput["Directory", WindowTitle -> "Choose the folder containing cell folders"]
  ];

  If[root === $Canceled || ! StringQ[root] || ! DirectoryQ[root],
    Return[Failure["RootDirectoryUnavailable", <|"Input" -> root|>]]
  ];

  channelCount = OptionValue["ChannelCount"];
  runRotation = TrueQ[OptionValue["RunRotation"]];
  imageChannel = OptionValue["ImageChannel"];
  cropFraction = OptionValue["CropFraction"];
  exportCentralStack = TrueQ[OptionValue["ExportCentralStack"]];
  useParallel = TrueQ[OptionValue["Parallel"]];
  kernels = OptionValue["Kernels"];

  cellDirs = If[
    FileExistsQ[FileNameJoin[{root, "export.csv"}]],
    {root},
    Select[FileNames["*", root, 1], DirectoryQ]
  ];
  cellDirs = Select[cellDirs, FileExistsQ[FileNameJoin[{#, "export.csv"}]] &];

  If[cellDirs === {},
    Return[Failure["NoCellDirectories", <|"RootDirectory" -> root|>]]
  ];

  kkResults = mapJobs[
    cellDirs,
    processKKCell[#, "ChannelCount" -> channelCount] &,
    "Parallel" -> useParallel,
    "Kernels" -> kernels
  ];

  rotationJobs = If[runRotation,
    Flatten[discoverRotationJobs /@ cellDirs],
    {}
  ];

  rotationResults = If[runRotation && rotationJobs =!= {},
    mapJobs[
      rotationJobs,
      rotateKTMovie[
        #,
        "ImageChannel" -> imageChannel,
        "CropFraction" -> cropFraction,
        "ExportCentralStack" -> exportCentralStack
      ] &,
      "Parallel" -> useParallel,
      "Kernels" -> kernels
    ],
    {}
  ];

  <|
    "RootDirectory" -> root,
    "CellCount" -> Length[cellDirs],
    "KKResults" -> kkResults,
    "RotationJobCount" -> Length[rotationJobs],
    "RotationResults" -> rotationResults
  |>
];


(* Usage examples:

   Get["path/to/KKCalculationAndRotation_professional.wl"];

   runKKCalculationAndRotation[]

   runKKCalculationAndRotation[
     "path/to/experiment",
     "ChannelCount" -> 2,
     "RunRotation" -> True,
     "Parallel" -> True,
     "Kernels" -> 4
   ]
*)
