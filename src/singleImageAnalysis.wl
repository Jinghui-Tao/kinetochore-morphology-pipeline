(* ::Package:: *)

(* ::Title:: *)
(* Single-Image Kinetochore Analysis *)


(*
This script measures kinetochore dimensions, intensity peaks, asymmetry, and
tail-like extensions from one centered fluorescence image. It returns a metric
list together with plots for visual inspection.
*)



(* ::Section:: *)
(* Setup *)


ClearAll[
  allAnalysis, runSingleImageAnalysis, listPixelsToum, pointsPixelsToum,
  displaySingleImageAnalysisResult, runAndDisplaySingleImageAnalysis,
  analysisOutputColumns, xTicks, yTicks, commonStyle,
  pixelsize, backgroundSeprationFactor, smallNoiseComponentSize,
  boundaryDilation, peakIntensityThreasholdFactor, peakRatioThreashold,
  contours, lowpassThreashold, d1LowpassThreashold,
  d1PeakBackgroundThreasholdRatio
];



(* ::Section:: *)
(* Analysis Parameters *)


(* Pixel size in micrometers per pixel after acquisition binning. *)
pixelsize = 0.092;

(* Threshold multiplier for separating background from real kinetochore signal. *)
backgroundSeprationFactor = 7;

(* Connected components smaller than this area are treated as noise. *)
smallNoiseComponentSize = 9;

(* Dilation radius used to include the full kinetochore boundary. *)
boundaryDilation = 3;

(* Threshold multiplier for rejecting low-intensity 2D peaks. *)
peakIntensityThreasholdFactor = 3;

(* Minimum intensity ratio required to keep a secondary peak. *)
peakRatioThreashold = 0.1;

(* Number of contour levels shown in the inspection plot. *)
contours = 15;

(* Low-pass smoothing cutoff for intensity projections. *)
lowpassThreashold = 2;

(* Low-pass smoothing cutoff for the first derivative of the projection. *)
d1LowpassThreashold = 2;

(* Sensitivity threshold for derivative peaks used in tail detection. *)
d1PeakBackgroundThreasholdRatio = 0.5;

analysisOutputColumns = {
  "ktArea(pixels)", "width_X(AUC/max)(\[Micro]m)", "width_Y(AUC/max)(\[Micro]m)",
  "orientationAngle(deg)", "elongationRatioResult", "semiaxesRatioResult",
  "tail or not", "tail direction", "tail length(\[Micro]m)",
  "asymmetryResult", "x_1st peak intensity", "x_2nd peak intensity",
  "x_valley intensity", "Xproj_peak number", "y_1st peak intensity",
  "y_2nd peak intensity", "y_valley intensity", "Yproj_peak number",
  "2D 1st peak intensity", "2D 2nd peak intensity",
  "2D peak dist X(\[Micro]m)", "2D peak dist Y(\[Micro]m)",
  "number of 2D peaks", "ratio 2nd/1st peak", "totalIntensity"
};



(* ::Section:: *)
(* Plot Style *)


xTicks = Table[{x, "", {0.02, 0}}, {x, 0, 3, 0.5}];
yTicks = Table[{y, "", {0.02, 0}}, {y, 0.5, 1.0, 0.5}];
commonStyle = {
  Frame -> False,
  Axes -> True,
  Background -> White,
  ImageSize -> 500,
  AxesStyle -> Directive[Black, AbsoluteThickness[2]],
  TicksStyle -> Directive[Black, 20],
  Ticks -> {xTicks, yTicks},
  PlotLabel -> None,
  AxesOrigin -> {0, 0},
  ImagePadding -> {{60, 20}, {50, 15}},
  PlotRangePadding -> {{Scaled[0.03], Scaled[0.03]}, {Scaled[0.03], Scaled[0.05]}},
  PlotRangeClipping -> False
};



(* ::Section:: *)
(* Unit Conversion Helpers *)


(* Converts a one-dimensional pixel-indexed profile to micrometer coordinates. *)
listPixelsToum[list_] := Table[{i*pixelsize, list[[i]]}, {i, Length[list]}];

(* Converts point coordinates from pixels to micrometers along the x-axis. *)
pointsPixelsToum[listofpoints_] := Transpose[{Transpose[listofpoints][[1]]*pixelsize, Transpose[listofpoints][[2]]}];



(* ::Section:: *)
(* Image Input *)


runSingleImageAnalysis::file = "The selected file could not be imported as an image.";

(* Selects one image file, centers the analysis crop, and runs the analysis. *)
runSingleImageAnalysis[file_: Automatic, cropFraction_: 0.7] := Module[
  {path, importedImage, rawInputImg, cropSize},
  path = If[file === Automatic, SystemDialogInput["FileOpen"], file];
  If[path === $Canceled, Return[$Canceled]];
  importedImage = Quiet@Check[Import[path], $Failed];
  If[importedImage === $Failed || !ImageQ[importedImage],
    Message[runSingleImageAnalysis::file];
    Return[$Failed]
  ];
  cropSize = Floor[Min[ImageDimensions[importedImage]]*cropFraction];
  rawInputImg = ImageAdjust[ImageCrop[importedImage, cropSize]];
  allAnalysis[rawInputImg]
];



(* ::Section:: *)
(* Core Analysis *)


(*
Input: a centered kinetochore image.
Output: {metrics, plots}, where metrics follows analysisOutputColumns.
*)
allAnalysis[rawInputImg_]:=Module[{scaletextsize,signalthreshhold,image,imageY,imageDimensions,imageData,boundaryCheck,backgroundSignalPreliminary,backgroundSignal,centerBoxIntensity,centerSignalCheck,meanY,imageDataBackground,backgroundIntensity,meanBackground,sdBackgroud,preliminaryComponentsMask,realComponentsMask,fullBackgroudsMask,mainComponentMask,backgroundPlusMainComponentMask,newothercomponents,sideComponentMask,backgroundProjectionCheck,interestComponentDilationMask,componentUninterestedDilationMask,preProjectionCheck,yProjectionBackgroundSignalPre,yProjectionBackgroundSignalInterpolation,yProjectionBackgroundSignal,interestedComponentSignalCrop,showPeaks,peakCount,peakPointsCrop,prePeakLocations,prePeakLocationsAndValue,maxBackground,boundaryMax,peakBoundaryMaxThreashold,peakBackgroundThreashold,maxPeak,finalizedPeaks,peakIntensities,peakLocations,firstTwoPeaksDistanceX,firstTwoPeaksDistanceY,showAllPeaks,showAllPeaksContours,irrelaventComponentsRemove,projectionYIrrelaventComponentsRemove,cleanYProjection,oversizecheck,filteredCleanYProjection,interpolationFilteredCleanYProjectionFunction,sdYProjectionBackground,sdD1YProjectionBackground,sdD1YProjectionBackgroundFiltered,d1peakBackgroundThreashold,peaksInProjectionFiltered,peaksInProjection,mainPeakInProjection,secondPeakInProjection,peaksDistance,valleyIntenistyBetweenPeaks,d1PositivePeaks,d1NegtiveValleys,d1PositiveMainPeak,d1NegativeMainValley,d1PositivePeaksFiltered,d1NegativeValleysFiltered,allD1PositivePeaks,allD1NegativeValleys,allD1PeaksAndValleys,ifTail,leftEdge,rightEdge,widthByEdge,leftGFW,rightGFW,ifSecondPeakInProjection,secondPeakInProjectionIntensity,ifFilteredSecondPeakInProjection,secondPeakInProjectionDirection,distanceOfPeaksInProjection,centerLeftIntensityInProjection,centerLeftIntensityInProjectionTotal,centerRightIntensityInProjection,centerRightIntensityInProjectionTotal,intenistyRatioLvsR,skewDirection,highestIntensity,highestIntensityLocation,centerRightEdgeLeftIntensityInProjectionTotal,centerRightEdgeRightIntensityInProjectionTotal,centerLeftEdgeLeftIntensityInProjectionTotal,centerLeftEdgeRightIntensityInProjectionTotal,centerLeftEdgeCompare,centerRightEdgeCompare,ifAsymetry,intensityIntegration,generalizedFW,tailDirection,tailLength,rw,lw,D1negtivepeaks,D1positivepeaks,D1peaks,ifTailDirectionLength,fullMeasurement,highlightedImg,showIntensityPeaks,showAllD1Peaks,linePlot,intensitycentroidX,intensityCurveToPlot,irrelaventComponentsRemoveR90,componentUninterestedDilationMaskR90,backgroundSignalR90,fullBackgroudsMaskR90,yProjectionBackgroundSignalPreR90,yProjectionBackgroundSignalInterpolationR90,yProjectionBackgroundSignalR90,projectionYIrrelaventComponentsRemoveR90,cleanYProjectionR90,intensityIntegrationR90,sdYProjectionBackgroundR90,peaksInProjectionFilteredR90,peaksInProjectionR90,mainPeakInProjectionR90,generalizedFWR90,elongation,orientation,semiaxes,semiaxesRatio,ifSecondPeakInProjectionR90,secondPeakInProjectionR90,valleyIntenistyBetweenPeaksR90,distanceOfPeaksInProjectionR90,measuredLength,scaleBarPixels,totalIntensity,componentsCount,SignificantPeaks,ktArea,peakNumbersInProjectionFiltered,peakNumbersInProjectionFilteredR90,asymetryRatio,normFactor,peakPointsNorm,xMax,peakPtsNorm,d1PtsNorm,d1ExtremaPtsPlot,d1NegPeaksTmp,d1CurveNorm,d1CurveNormPlot,d1CurveNormFunPixel,d1ExtremaX,linePlotWithGridLines,linePlotWithPurplePts,toPairs,d1PositiveCurve,d1NegativeCurve,posMainCandidates,negMainCandidates,d1PositiveMainPeakList,d1NegativeMainValleyList},(* Local variables used by the image-processing pipeline. *)

$DisplayFunction=Identity;
image=rawInputImg;

(*Import image data.*)
imageDimensions=ImageDimensions[image];
     imageData=ImageData[image];


(*Part 1:pre-processing.*)
(*Extract background using Max Entropy.*)
backgroundSignalPreliminary=RemoveAlphaChannel[SetAlphaChannel[image,ColorNegate[Dilation[Binarize[image,Method -> "Entropy"], DiskMatrix[1]]]], Black];
backgroundIntensity=Select[Flatten[ImageData[backgroundSignalPreliminary]],#>0&];

(*Mean intensity of the central 4\[Times]4 box to gauge signal level.*)
centerBoxIntensity=Transpose[imageData[[Range[Floor[(imageDimensions[[1]]+1)/2]-2,Ceiling[(imageDimensions[[1]]+1)/2]+2]]]][[Range[Floor[(imageDimensions[[2]]+1)/2]-2,Ceiling[(imageDimensions[[2]]+1)/2]+2]]];


(*Basic image measurements.*)
meanY=Total[imageData]/imageDimensions[[2]];
imageDataBackground=ImageData[backgroundSignalPreliminary];
meanBackground=Mean[backgroundIntensity];
sdBackgroud=StandardDeviation[backgroundIntensity];
maxBackground=Max[Select[Flatten[ImageData[backgroundSignalPreliminary]],#1!=0&]];


(*Part 2:component detection and further measurements.Separate three components:
1) Kinetochore (main component) 
2) Pure background (noise source 1) 
3) Other high-intensity components not connected to the central KT (likely other kinetochores) (noise source 2;side components)

 Naming conventions:
*mask \[LongDash] binarized mask of signals
*signal \[LongDash] keep original intensity in the component of interest and remove other noise*)

preliminaryComponentsMask=Binarize[image,Max[maxBackground,meanBackground+backgroundSeprationFactor sdBackgroud]];
realComponentsMask=If[Total[DeleteSmallComponents[preliminaryComponentsMask,smallNoiseComponentSize]]<1,DeleteSmallComponents[preliminaryComponentsMask,Floor[smallNoiseComponentSize/2]-1],DeleteSmallComponents[preliminaryComponentsMask,smallNoiseComponentSize]];

(*Select the full background=image-realComponentsMask.*)
fullBackgroudsMask=Binarize[DeleteSmallComponents[ColorNegate[
Dilation[realComponentsMask, DiskMatrix[1]]
],smallNoiseComponentSize],0.5];(*0.5 is arbitrary since the mask is binarized.*)

(*Select the component closest to the image center.*)
mainComponentMask=
SelectComponents[realComponentsMask,"Centroid",1,Norm[Flatten[#1]-(imageDimensions/2)]<Norm[Flatten[#2]-(imageDimensions/2)]&]; 
ktArea=Total[Flatten[ImageData[mainComponentMask]]];
backgroundPlusMainComponentMask=Binarize[fullBackgroudsMask+Dilation[mainComponentMask, DiskMatrix[boundaryDilation]],0.5];
sideComponentMask=ColorNegate[backgroundPlusMainComponentMask];(*Possibly due to other KTs in the background.*)
backgroundSignal=RemoveAlphaChannel[SetAlphaChannel[image,fullBackgroudsMask], Black];
backgroundProjectionCheck=!Select[Total[ImageData[fullBackgroudsMask]],#==0&]=={};(*Ensure there are background pixels in every column so the projection is well-defined.*)
interestComponentDilationMask=Dilation[mainComponentMask, DiskMatrix[boundaryDilation]]*(1-sideComponentMask);
componentUninterestedDilationMask=
Image[(Abs[ImageData[sideComponentMask-interestComponentDilationMask]]+ImageData[sideComponentMask-interestComponentDilationMask])/2];(*Restrict dilation to background regions to keep interestComponentDilationMask untouched,so we can fully crop out uninterested components later.*)
preProjectionCheck=Select[Total[ImageData[ColorNegate[componentUninterestedDilationMask]]],#1==0&]=={};
(*(*Ensure there are pixels along the projection direction in each column after cropping out uninterested components.*)*)
yProjectionBackgroundSignalPre=Total[ImageData[backgroundSignal]]/Total[ImageData[fullBackgroudsMask]];
yProjectionBackgroundSignalInterpolation=Interpolation[Select[Transpose[{Range[Length[yProjectionBackgroundSignalPre]],yProjectionBackgroundSignalPre}],NumericQ[Last[#]]&],InterpolationOrder->1]; (*Interpolate to ensure the mean Y-projection is defined if some columns lack background pixels.*)
yProjectionBackgroundSignal=Table[yProjectionBackgroundSignalInterpolation[i],{i,1,Length[yProjectionBackgroundSignalPre]}];
interestedComponentSignalCrop=RemoveAlphaChannel[SetAlphaChannel[image,Dilation[mainComponentMask, DiskMatrix[boundaryDilation]]], Black]; (*Crop the component of interest.*)
totalIntensity=Total[Flatten[ImageData[interestedComponentSignalCrop]]]-meanBackground*Total[Flatten[ImageData[Binarize[interestedComponentSignalCrop,0]]]];


(*Part 2.1:2D intensity peaks.*)
showPeaks=MaxDetect[image]*mainComponentMask;
peakCount=IntegerPart[Total[showPeaks]];(*IntegerPart ensures correct data type.*)
peakPointsCrop=RemoveAlphaChannel[SetAlphaChannel[image,showPeaks], Black];(*Select peak points.*)
prePeakLocations=Position[ImageData[peakPointsCrop],x_?Positive,Infinity];(*Find peak locations.*)
prePeakLocationsAndValue=Table[{location,imageData[[location[[1]],location[[2]]]]},{location,prePeakLocations}];
boundaryMax=Max[Flatten[ImageData[EdgeDetect[interestComponentDilationMask]*interestedComponentSignalCrop]]];(*Measure max intensity at KT boundary.*)
peakBoundaryMaxThreashold=boundaryMax+peakIntensityThreasholdFactor sdBackgroud;(*Threshold for excluding low-intensity peaks likely due to noise,based on KT boundary intensity.*)
peakBackgroundThreashold=peakIntensityThreasholdFactor sdBackgroud+meanBackground;(*Threshold for excluding low-intensity peaks likely due to noise,based on background intensity.*)
maxPeak=SortBy[prePeakLocationsAndValue,-#[[2]]&][[1,2]];
SignificantPeaks=Select[SortBy[prePeakLocationsAndValue,-#[[2]]&],(#[[2]])>Max[peakRatioThreashold*(maxPeak-meanBackground)+meanBackground,peakBackgroundThreashold,peakBoundaryMaxThreashold]&];
finalizedPeaks=If[Length[SignificantPeaks]==0,SortBy[prePeakLocationsAndValue,-#[[2]]&],SignificantPeaks];(*If no peak exceeds threshold,keep only the maximum peak.*)
peakIntensities=Transpose[finalizedPeaks][[2]];
peakLocations= Transpose[finalizedPeaks][[1]];
firstTwoPeaksDistanceX=If[Length[peakLocations]<2,{0,0},(peakLocations[[2]]-peakLocations[[1]])*pixelsize][[2]];
firstTwoPeaksDistanceY=If[Length[peakLocations]<2,{0,0},-(peakLocations[[2]]-peakLocations[[1]])*pixelsize][[1]];

(*Plots.*)
showAllPeaks=HighlightImage[Colorize[ImageAdjust[image]],ImageMarker[Map[{#[[2]],imageDimensions[[2]]-#[[1]]}+{-1/2,1/2}&,peakLocations]]];
crossMarker[{x_,y_},size_:0.5]:={Line[{{x-size,y},{x+size,y}}],Line[{{x,y-size},{x,y+size}}] };
showAllPeaksContours=ListContourPlot[Reverse[Clip[ImageData[interestedComponentSignalCrop]-meanBackground,{0,Infinity}]],Contours->contours,ColorFunction->"Rainbow",ColorFunctionScaling->True,PlotRange->All,PlotLegends->BarLegend[Automatic],Epilog->{Black,PointSize[0.1],crossMarker[#,3]&/@Map[{#[[2]],imageDimensions[[2]]-#[[1]]}+{1/2-1/2,1/2+1/2}&,peakLocations] }];



(*Part 3:asymmetry detection and generalized full-width measurement.*)

irrelaventComponentsRemove=RemoveAlphaChannel[SetAlphaChannel[image,ColorNegate[componentUninterestedDilationMask]], Black]; (*Remove kinetochores that may intrude into the field of view.*)
projectionYIrrelaventComponentsRemove=Total[ImageData[irrelaventComponentsRemove]]/Total[ImageData[ColorNegate[componentUninterestedDilationMask]]]; (*Compute the projection.*)

listToD1Function[list_]:=Interpolation[list]';(*Compute the first derivative using Interpolation to obtain a continuous curve.*)
d1List[list_]:=Table[listToD1Function[list][i],{i,1,Length[list]}];
cleanYProjection=projectionYIrrelaventComponentsRemove-yProjectionBackgroundSignal;
filteredCleanYProjection=LowpassFilter[cleanYProjection,lowpassThreashold];(*Smooth the curve.*)
interpolationFilteredCleanYProjectionFunction=Interpolation[filteredCleanYProjection];
sdYProjectionBackground=StandardDeviation[yProjectionBackgroundSignal];
sdD1YProjectionBackground=StandardDeviation[d1List[yProjectionBackgroundSignal]];(*Std dev of the background's first derivative.*)
sdD1YProjectionBackgroundFiltered=StandardDeviation[LowpassFilter[d1List[LowpassFilter[yProjectionBackgroundSignal,lowpassThreashold]],d1LowpassThreashold]];(*Smooth the first-derivative curve.*)
d1peakBackgroundThreashold=d1PeakBackgroundThreasholdRatio*sdD1YProjectionBackgroundFiltered;
peakIntensitySignificance[x_]:=(interpolationFilteredCleanYProjectionFunction[x])/sdYProjectionBackground;(*Peak intensity divided by background SD;
indicates likelihood of a real peak.*)

(*Detect peaks in the intensity projection curve.*)
peaksInProjection=FindPeaks[LowpassFilter[cleanYProjection,lowpassThreashold],0,0,0,InterpolationOrder->3];
peaksInProjectionFiltered=FindPeaks[LowpassFilter[cleanYProjection,lowpassThreashold],0,0,sdYProjectionBackground,InterpolationOrder->3];(*Filter spurious peaks using background SD.*)
mainPeakInProjection=If[Length[peaksInProjectionFiltered]>0,Sort[peaksInProjectionFiltered,#1[[2]]>#2[[2]]&][[1]],Sort[peaksInProjection,#1[[2]]>#2[[2]]&][[1]]];
ifSecondPeakInProjection=Length[peaksInProjectionFiltered]>1;
secondPeakInProjection=If[Length[peaksInProjectionFiltered]>1,Sort[peaksInProjectionFiltered,#1[[2]]>#2[[2]]&][[2]],{"NA","NA"}];
secondPeakInProjectionIntensity=If[ifSecondPeakInProjection,secondPeakInProjection[[2]],"NA"];
valleyIntenistyBetweenPeaks=If[secondPeakInProjection=!={"NA","NA"},Min[cleanYProjection[[Range[MinMax[{Round[secondPeakInProjection[[1]]],Round[mainPeakInProjection[[1]]]}][[1]],MinMax[{Round[secondPeakInProjection[[1]]],Round[mainPeakInProjection[[1]]]}][[-1]]]]]],"NA"];
secondPeakInProjectionDirection=If[ifSecondPeakInProjection,If[mainPeakInProjection[[1]]>secondPeakInProjection[[1]],"left","right"],"NA"];
distanceOfPeaksInProjection=If[ifSecondPeakInProjection,Abs[mainPeakInProjection[[1]]-secondPeakInProjection[[1]]],"NA"];
peakNumbersInProjectionFiltered=Length[peaksInProjectionFiltered];

(*Detect tails via peaks/valleys of the first derivative.*)toPairs[expr_]:=Cases[expr,{x_?NumericQ,y_?NumericQ}:>{x,y},Infinity];

d1PositiveCurve=LowpassFilter[d1List[LowpassFilter[cleanYProjection,lowpassThreashold]],d1LowpassThreashold];

d1NegativeCurve=LowpassFilter[d1List[LowpassFilter[-cleanYProjection,lowpassThreashold]],d1LowpassThreashold];

d1PositivePeaks=toPairs[FindPeaks[d1PositiveCurve,0,0,d1peakBackgroundThreashold,InterpolationOrder->3]];

d1NegPeaksTmp=toPairs[FindPeaks[d1NegativeCurve,0,0,d1peakBackgroundThreashold,InterpolationOrder->3]];

d1NegtiveValleys=({#[[1]],-#[[2]]}&/@d1NegPeaksTmp);

posMainCandidates=Select[d1PositivePeaks,#[[1]]<mainPeakInProjection[[1]]&];

negMainCandidates=Select[d1NegtiveValleys,#[[1]]>mainPeakInProjection[[1]]&];

d1PositiveMainPeakList=If[posMainCandidates==={},{},{First[SortBy[posMainCandidates,-#[[1]]&]]}];

d1NegativeMainValleyList=If[negMainCandidates==={},{},{First[SortBy[negMainCandidates,#[[1]]&]]}];

d1PositivePeaksFiltered=Select[d1PositivePeaks,peakIntensitySignificance[#[[1]]]>d1PeakBackgroundThreasholdRatio&];

d1NegativeValleysFiltered=Select[d1NegtiveValleys,peakIntensitySignificance[Round[#[[1]]]]>d1PeakBackgroundThreasholdRatio&];

allD1PositivePeaks=DeleteDuplicates[toPairs@Join[d1PositiveMainPeakList,d1PositivePeaksFiltered]];

allD1NegativeValleys=DeleteDuplicates[toPairs@Join[d1NegativeMainValleyList,d1NegativeValleysFiltered]];

allD1PeaksAndValleys=DeleteDuplicates[toPairs@Join[allD1PositivePeaks,allD1NegativeValleys]];

ifTail=Length[allD1PeaksAndValleys]>2;

leftEdge=If[Length[allD1PositivePeaks]>0,Min[allD1PositivePeaks[[All,1]]],1];

rightEdge=If[Length[allD1NegativeValleys]>0,Max[allD1NegativeValleys[[All,1]]],Length[cleanYProjection]];

widthByEdge=rightEdge-leftEdge;

leftEdge=Min[Transpose[allD1PositivePeaks][[1]]];
rightEdge=Max[Transpose[allD1NegativeValleys][[1]]];
widthByEdge=rightEdge-leftEdge;


(*Compute asymmetry via left/right intensity distribution around the main peak.Use both pixel edges of the peak;accept the result only if both agree.*)
highestIntensity=Max[cleanYProjection];
highestIntensityLocation=Position[cleanYProjection,highestIntensity][[1,1]];
(*Use the peak's right pixel edge as the border dividing left and right.*)
centerRightEdgeLeftIntensityInProjectionTotal=Total[cleanYProjection[[Range[highestIntensityLocation]]]];
centerLeftEdgeRightIntensityInProjectionTotal=Total[cleanYProjection[[Range[highestIntensityLocation,Length[cleanYProjection]]]]];
(*Use the peak's left pixel edge as the border dividing left and right.*)
centerLeftEdgeLeftIntensityInProjectionTotal=Total[cleanYProjection[[Range[highestIntensityLocation-1]]]];
centerRightEdgeRightIntensityInProjectionTotal=Total[cleanYProjection[[Range[highestIntensityLocation+1,Length[cleanYProjection]]]]];
(*Compare both;only if consistent do we accept the asymmetry call.*)
centerLeftEdgeCompare=If[centerLeftEdgeLeftIntensityInProjectionTotal>centerLeftEdgeRightIntensityInProjectionTotal,"L","R"];
centerRightEdgeCompare=If[centerRightEdgeLeftIntensityInProjectionTotal>centerRightEdgeRightIntensityInProjectionTotal,"L","R"];
ifAsymetry=Module[{bl=centerLeftEdgeCompare,br=centerRightEdgeCompare},If[bl==br,br, "unsure"]];
asymetryRatio= If[ifAsymetry!="unsure",(centerRightEdgeRightIntensityInProjectionTotal-centerLeftEdgeLeftIntensityInProjectionTotal)/Total[cleanYProjection],0];

(*Length measurement:AUC divided by peak intensity.*)
intensityIntegration=Integrate[Interpolation[LowpassFilter[cleanYProjection,lowpassThreashold]][x],{x,1,imageDimensions[[1]]}];
generalizedFW=intensityIntegration/mainPeakInProjection[[2]];
leftGFW=centerLeftEdgeLeftIntensityInProjectionTotal/mainPeakInProjection[[2]];
rightGFW=centerRightEdgeRightIntensityInProjectionTotal/mainPeakInProjection[[2]];

(*Tail direction and length. 
Direction is defined by the counts of peaks/valleys in the derivative;
tail length is the side-specific AUC divided by peak intensity.*)
tailDirection=Which[Length[allD1NegativeValleys]>=2&&Length[allD1PositivePeaks]<2,"right",Length[allD1NegativeValleys]<2&&Length[allD1PositivePeaks]>=2,"left",Length[allD1NegativeValleys]>=2&&Length[allD1PositivePeaks]>=2,"both",Length[allD1NegativeValleys]<2&&Length[allD1PositivePeaks]<2,"none"];
tailLength=Which[Length[allD1NegativeValleys]>=2&&Length[allD1PositivePeaks]<2,rightGFW,Length[allD1NegativeValleys]<2&&Length[allD1PositivePeaks]>=2,leftGFW,Length[allD1NegativeValleys]>=2&&Length[allD1PositivePeaks]>=2,If[rightGFW>leftGFW,rightGFW,-leftGFW],Length[allD1NegativeValleys]<2&&Length[allD1PositivePeaks]<2,0];
rw=rightGFW;lw=leftGFW;
{D1negtivepeaks,D1positivepeaks}={allD1NegativeValleys,allD1PositivePeaks};
D1peaks=Join[D1positivepeaks,D1negtivepeaks];
(*Collect tail data.*)
ifTailDirectionLength=
{ifTail,tailDirection,tailLength};


(*Part 4:Y-axis measurements (rotate the image by 90\[Degree]).*)
irrelaventComponentsRemoveR90=ImageRotate[irrelaventComponentsRemove];
componentUninterestedDilationMaskR90= ImageRotate[componentUninterestedDilationMask];
backgroundSignalR90=ImageRotate[backgroundSignal];
fullBackgroudsMaskR90=ImageRotate[fullBackgroudsMask];
yProjectionBackgroundSignalPreR90=Total[ImageData[backgroundSignalR90]]/Total[ImageData[fullBackgroudsMaskR90]];
yProjectionBackgroundSignalInterpolationR90=Interpolation[Select[Transpose[{Range[Length[yProjectionBackgroundSignalPreR90]],yProjectionBackgroundSignalPreR90}],NumericQ[Last[#]]&],InterpolationOrder->1];
yProjectionBackgroundSignalR90=Table[yProjectionBackgroundSignalInterpolationR90[i],{i,1,Length[yProjectionBackgroundSignalPreR90]}];
projectionYIrrelaventComponentsRemoveR90=Total[ImageData[irrelaventComponentsRemoveR90]]/Total[ImageData[ColorNegate[componentUninterestedDilationMaskR90]]];
cleanYProjectionR90=projectionYIrrelaventComponentsRemoveR90-yProjectionBackgroundSignalR90;
sdYProjectionBackgroundR90=StandardDeviation[yProjectionBackgroundSignalR90];
peaksInProjectionFilteredR90=FindPeaks[LowpassFilter[cleanYProjectionR90,lowpassThreashold],0,0,sdYProjectionBackgroundR90,InterpolationOrder->3];
peaksInProjectionR90=FindPeaks[LowpassFilter[cleanYProjectionR90,lowpassThreashold],0,0,0,InterpolationOrder->3];
mainPeakInProjectionR90=If[Length[peaksInProjectionFilteredR90]>0,Sort[peaksInProjectionFilteredR90,#1[[2]]>#2[[2]]&][[1]],Sort[peaksInProjectionR90,#1[[2]]>#2[[2]]&][[1]]];
intensityIntegrationR90=Integrate[Interpolation[LowpassFilter[cleanYProjectionR90,lowpassThreashold]][x],{x,1,imageDimensions[[2]]}];
generalizedFWR90=intensityIntegrationR90/mainPeakInProjectionR90[[2]];
ifSecondPeakInProjectionR90=Length[peaksInProjectionFilteredR90]>1;
secondPeakInProjectionR90=If[ifSecondPeakInProjectionR90,Sort[peaksInProjectionFilteredR90,#1[[2]]>#2[[2]]&][[2]],{"NA","NA"}];
valleyIntenistyBetweenPeaksR90=If[secondPeakInProjectionR90=!={"NA","NA"},Min[cleanYProjectionR90[[Range[MinMax[{Round[secondPeakInProjectionR90[[1]]],Round[mainPeakInProjectionR90[[1]]]}][[1]],MinMax[{Round[secondPeakInProjectionR90[[1]]],Round[mainPeakInProjectionR90[[1]]]}][[-1]]]]]],"NA"];
distanceOfPeaksInProjectionR90=If[ifSecondPeakInProjectionR90,Abs[mainPeakInProjectionR90[[1]]-secondPeakInProjectionR90[[1]]],"NA"];
peakNumbersInProjectionFilteredR90=Length[peaksInProjectionFilteredR90];

(*Other 2D morphology metrics.*)
elongation=ComponentMeasurements[mainComponentMask,{"Elongation"}][[All,2]][[1,1]];
orientation=ComponentMeasurements[mainComponentMask,{"Orientation"}][[All,2]][[1,1]];
semiaxes=ComponentMeasurements[mainComponentMask,{"SemiAxes"}][[All,2]][[1,1]];
semiaxesRatio=Min[semiaxes]/Max[semiaxes];

(*Check the four corner intensities to confirm the cropped KT window does not include the black boundary.*)
boundaryCheck=Min[Flatten[{imageData[[1,1]],imageData[[imageDimensions[[2]],1]],imageData[[1,imageDimensions[[1]]]],imageData[[imageDimensions[[2]],imageDimensions[[1]]]]}]]>0;
centerSignalCheck=Total[Flatten[Transpose[ImageData[interestComponentDilationMask][[Range[Floor[(imageDimensions[[1]]+1)/2]-2,Ceiling[(imageDimensions[[1]]+1)/2]+2]]]][[Range[Floor[(imageDimensions[[2]]+1)/2]-2,Ceiling[(imageDimensions[[2]]+1)/2]+2]]]]]>0;

(*Collect and output results.*)
fullMeasurement=If[boundaryCheck&&centerSignalCheck,If[backgroundProjectionCheck||!preProjectionCheck,{"NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA"},Module[{centerSignalCheckresult,signalstrength,averagebackground,meanYmainPeakInProjectionintensityoverbackground,halfAreaWidth,widthXResult,widthYResult,secondPeakInProjectionornot,intensitysecondPeakInProjection,secondPeakInProjectiondirection,peaksdistance,tailOrNotResult,directionTailResult,tailLengthResult,discR, intensitycentroidtodisccenterx,peaktodiscX,brightesttodisccenterX,brightesttodisccenterY,diskcoverage,polygoncoverage,areaofKT,tiltAngleResult,elongationRatioResult,semiaxesRatioResult,boundboxX,leftrightratio,skewdirec,asymetryResult,tailInfoResult},
centerSignalCheckresult="T";

widthXResult=generalizedFW*pixelsize;
widthYResult=generalizedFWR90*pixelsize;
tiltAngleResult=orientation;
elongationRatioResult=elongation;
semiaxesRatioResult=semiaxesRatio;
tailInfoResult=ifTailDirectionLength;
tailOrNotResult=tailInfoResult[[1]];
directionTailResult=tailInfoResult[[2]];
tailLengthResult=tailInfoResult[[3]]*pixelsize;
asymetryResult=asymetryRatio;
highlightedImg=HighlightImage[ImageAdjust[image],interestComponentDilationMask,{Red,Opacity[0.5]}];
intensityCurveToPlot=filteredCleanYProjection;

normFactor=mainPeakInProjection[[2]];
xMax=imageDimensions[[1]]*pixelsize;

peaksForPlot=If[Length[peaksInProjectionFiltered]==0,{mainPeakInProjection},TakeLargestBy[peaksInProjectionFiltered,#[[2]]&,UpTo[100]]];

curveForPlot=listPixelsToum[intensityCurveToPlot/normFactor];

curveFun=Interpolation[curveForPlot,InterpolationOrder->3];

peakPtsNorm=({#[[1]]*pixelsize,curveFun[#[[1]]*pixelsize]}&/@peaksForPlot);

(*red derivative curve:exactly the normalized curve used for plotting*)d1CurveNorm=LowpassFilter[d1List[intensityCurveToPlot],d1LowpassThreashold]/normFactor;

d1CurveNorm=LowpassFilter[d1List[intensityCurveToPlot],d1LowpassThreashold]/normFactor;

d1CurveNormPlot=listPixelsToum[d1CurveNorm];

d1CurveNormFunPixel=Interpolation[Transpose[{Range[Length[d1CurveNorm]],d1CurveNorm}],InterpolationOrder->3];

d1ExtremaX=DeleteDuplicates[Cases[allD1PeaksAndValleys,{x_?NumericQ,y_?NumericQ}:>x,Infinity]];

d1ExtremaX=Select[d1ExtremaX,1<=#<=Length[d1CurveNorm]&];

d1PtsNorm=({#*pixelsize,d1CurveNormFunPixel[#]}&/@d1ExtremaX);


showIntensityPeaks=Show[ListPlot[listPixelsToum[cleanYProjection/normFactor],PlotStyle->Directive[GrayLevel[0.5],AbsolutePointSize[5]],PlotRange->{{0,xMax},{0,1.05}}],ListLinePlot[curveForPlot,InterpolationOrder->3,PlotStyle->Directive[Blue,AbsoluteThickness[2.5]],PlotRange->{{0,xMax},{0,1.05}}],ListPlot[peakPtsNorm,PlotStyle->Directive[Orange,AbsolutePointSize[10]],PlotRange->{{0,xMax},{0,1.05}}],FrameLabel->{Style["Position (\[Mu]m)",18],Style["Normalized intensity",18]},PlotLabel->None,Evaluate@commonStyle];

showAllD1Peaks=Show[ListPlot[listPixelsToum[d1List[intensityCurveToPlot]/normFactor],PlotStyle->Brown,PlotRange->All,Evaluate@commonStyle],ListLinePlot[d1CurveNormPlot,InterpolationOrder->3,PlotStyle->Directive[Red,AbsoluteThickness[2.5]],PlotRange->All,FrameLabel->{Style["Position (\[Mu]m)",18],Style["Normalized derivative",18]},PlotLabel->None,Evaluate@commonStyle]];
measuredLength=widthXResult;
scaleBarPixels=1/pixelsize;


d1ExtremaPtsPlot=ListPlot[d1PtsNorm,PlotStyle->Directive[Purple,AbsolutePointSize[12]],PlotRange->All];

(*Show derivative extrema as gray vertical guide lines for visual reference.*)
linePlotWithGridLines=Show[showIntensityPeaks,showAllD1Peaks,GridLines->{Transpose[allD1PeaksAndValleys][[1]]*pixelsize,{}},GridLinesStyle->Directive[GrayLevel[0.65],AbsoluteThickness[2]],PlotRange->All,Evaluate@commonStyle];

(*Show derivative extrema as purple points on the derivative curve.*)
linePlotWithPurplePts=Show[showIntensityPeaks,showAllD1Peaks,d1ExtremaPtsPlot,PlotRange->All,Evaluate@commonStyle];

(*Result list:"ktArea(pixels)","width_Y(AUC/max)(\[Micro]m)","width_Y(AUC/max)(\[Micro]m)","orientationAngle(deg)","elongationRatioResult","semiaxesRatioResult","tail or not","tail direction","tail length(\[Micro]m)","asymmetryResult","x_1st peak intensity","x_2nd peak intensity","x_valley intensity","Xproj_peak number","y_1st peak intensity","y_2nd peak intensity","y_valley intensity","Yproj_peak number","2D 1st peak intensity","2D 2nd peak intensity","2D peak dist X(\[Micro]m)","2D peak dist Y(\[Micro]m)","number of 2D peaks","ratio 2nd/1st peak","totalIntensity"*)
	d1ExtremaPtsPlot=ListPlot[d1PtsNorm,PlotStyle->Directive[Purple,AbsolutePointSize[12]],PlotRange->All];

linePlotWithGridLines=Show[showIntensityPeaks,showAllD1Peaks,GridLines->{d1ExtremaX*pixelsize,{}},GridLinesStyle->Directive[GrayLevel[0.65],AbsoluteThickness[2]],PlotRange->All,Evaluate@commonStyle];

linePlotWithPurplePts=Show[showIntensityPeaks,showAllD1Peaks,d1ExtremaPtsPlot,PlotRange->All,Evaluate@commonStyle];

{{ktArea,widthXResult,widthYResult,tiltAngleResult,elongationRatioResult,semiaxesRatioResult,tailOrNotResult,directionTailResult,tailLengthResult,asymetryResult,mainPeakInProjection[[2]],secondPeakInProjection[[2]],
valleyIntenistyBetweenPeaks,peakNumbersInProjectionFiltered,
mainPeakInProjectionR90[[2]],secondPeakInProjectionR90[[2]],
valleyIntenistyBetweenPeaksR90,peakNumbersInProjectionFilteredR90,
If[Length[peakIntensities]>0,peakIntensities[[1]],"NA"],
If[Length[peakIntensities]>1,peakIntensities[[2]],"NA"],
firstTwoPeaksDistanceX,firstTwoPeaksDistanceY,
Length[peakLocations],
If[Length[peakIntensities]>1,(peakIntensities[[2]]-meanBackground)/(peakIntensities[[1]]-meanBackground),"NA"],
totalIntensity
},

(*Plots*)
{Show[showAllPeaksContours[[1]],ImageSize->350,ImageResolution->500],Show[showAllPeaks,ImageSize->350,ImageResolution->500,Epilog->{White,Thickness[0.03],Line[{{1,1},{1+scaleBarPixels,1}}]}],Show[highlightedImg,ImageSize->350,ImageResolution->500,Epilog->{White,Thickness[0.03],Line[{{1,1},{1+scaleBarPixels,1}}]}],Show[linePlotWithGridLines,ImageSize->350,ImageResolution->500],Show[linePlotWithPurplePts,ImageSize->350,ImageResolution->500]}}]
],{{"NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA"}}]]



(* ::Section:: *)
(* Interactive Output *)


(* Displays the metric table and inspection plots returned by allAnalysis. *)
displaySingleImageAnalysisResult[result_] := Module[
  {metrics, plotList, metricValues, metricTable, plotPanel},
  If[MemberQ[{$Canceled, $Failed}, result], Return[result]];
  If[!ListQ[result] || Length[result] < 1, Return[$Failed]];

  metrics = First[result];
  If[!ListQ[metrics], Return[$Failed]];

  plotList = If[Length[result] >= 2 && ListQ[result[[2]]], result[[2]], {}];
  metricValues = Take[
    PadRight[metrics, Length[analysisOutputColumns], "NA"],
    Length[analysisOutputColumns]
  ];

  metricTable = Grid[
    Prepend[Transpose[{analysisOutputColumns, metricValues}], {"Metric", "Value"}],
    Frame -> All,
    Alignment -> Left
  ];

  plotPanel = If[
    Length[plotList] > 0,
    Column[plotList, Spacings -> 1],
    Style["No inspection plots returned.", Italic, Gray]
  ];

  Column[
    {
      Style["Measurements", Bold, 14],
      metricTable,
      Style["Inspection plots", Bold, 14],
      plotPanel
    },
    Spacings -> 1.2
  ]
];


(* Selects one image and displays the analysis output. *)
runAndDisplaySingleImageAnalysis[file_: Automatic, cropFraction_: 1(*This is for the black edge from rotation, if there is no black edge, please keep it as 1, otherwise chose any positive number less than 1*)] :=
  displaySingleImageAnalysisResult[runSingleImageAnalysis[file, cropFraction]];



(* ::Section:: *)
(* Usage *)


(*
Choose an image interactively:

  runAndDisplaySingleImageAnalysis[];

Use an explicit file path for a reproducible run:

  runAndDisplaySingleImageAnalysis["C:\\path\\to\\image.tif"];

Use a different centered crop fraction if needed:

  runAndDisplaySingleImageAnalysis[Automatic, 0.8];

Store the raw result object if needed:

  result = runSingleImageAnalysis[];
*)
