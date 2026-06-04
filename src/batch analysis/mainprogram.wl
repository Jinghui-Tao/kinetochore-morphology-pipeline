(* ::Package:: *)

(* ::Title:: *)
(* Batch Kinetochore Image Analysis *)


(*
This script measures kinetochore dimensions, intensity peaks, asymmetry, and
projection-based width from every cropped KT image folder in a selected cell set.
Each KT folder is analyzed independently and exports a per-KT CSV plus inspection
images. The batch runner processes KT folders in controlled chunks so kernels are
started and closed only by the batch runner when parallel execution is requested.
*)



(* ::Section:: *)
(* Setup *)


ClearAll[
  allAnalysis, runMainBatchAnalysis, analyzeKTImageDirectory,
  discoverKTImageDirectories, sortImageNames, safeAnalyzeImage,
  safeImportImage, ensureDirectory, safeExport, ktOutputStem,
  withManagedParallelKernels,
  listPixelsToum, pointsPixelsToum, analysisHeaders,
  pixelsize, backgroundSeprationFactor, smallNoiseComponentSize,
  boundaryDilation, peakIntensityThreasholdFactor, peakRatioThreashold,
  contours, lowpassThreashold, d1LowpassThreashold,
  d1PeakBackgroundThreasholdRatio
];



(* ::Section:: *)
(* Analysis Parameters *)


(* Pixel size in micrometers per pixel after acquisition binning. *)
pixelsize = 0.046*2;

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

analysisHeaders = {
  "index", "ktArea(pixels)", "width_X(AUC/max)(um)",
  "width_Y(AUC/max)(um)", "orientationAngle(deg)",
  "elongationRatioResult", "semiaxesRatioResult", "tail or not",
  "tail direction", "tail length(um)", "asymmetryResult",
  "x_1st peak intensity", "x_2nd peak intensity", "x_valley intensity",
  "Xproj_peak number", "y_1st peak intensity", "y_2nd peak intensity",
  "y_valley intensity", "Yproj_peak number", "2D 1st peak intensity",
  "2D 2nd peak intensity", "2D peak dist X(um)",
  "2D peak dist Y(um)", "number of 2D peaks",
  "ratio 2nd/1st peak", "totalIntensity"
};



(* ::Section:: *)
(* Unit Conversion Helpers *)


(* Converts a one-dimensional pixel-indexed profile to micrometer coordinates. *)
listPixelsToum[list_] := Table[{i*pixelsize, list[[i]]}, {i, Length[list]}];

(* Converts point coordinates from pixels to micrometers along the x-axis. *)
pointsPixelsToum[listofpoints_] := Transpose[{Transpose[listofpoints][[1]]*pixelsize, Transpose[listofpoints][[2]]}];



(* ::Section:: *)
(* Core Single-Image Analysis *)


(*
Input: a centered KT image.
Output: {metrics, plots}, where metrics follows Rest[analysisHeaders].
*)
allAnalysis[rawInputImg_]:=Module[{scaletextsize,signalthreshhold,image,imageY,imageDimensions,imageData,boundaryCheck,backgroundSignalPreliminary,backgroundSignal,centerBoxIntensity,centerSignalCheck,meanY,imageDataBackground,backgroundIntensity,meanBackground,sdBackgroud,preliminaryComponentsMask,realComponentsMask,fullBackgroudsMask,mainComponentMask,backgroundPlusMainComponentMask,newothercomponents,sideComponentMask,backgroundProjectionCheck,interestComponentDilationMask,componentUninterestedDilationMask,preProjectionCheck,yProjectionBackgroundSignalPre,yProjectionBackgroundSignalInterpolation,yProjectionBackgroundSignal,interestedComponentSignalCrop,showPeaks,peakCount,peakPointsCrop,prePeakLocations,prePeakLocationsAndValue,maxBackground,boundaryMax,peakBoundaryMaxThreashold,peakBackgroundThreashold,maxPeak,finalizedPeaks,peakIntensities,peakLocations,firstTwoPeaksDistanceX,firstTwoPeaksDistanceY,showAllPeaks,showAllPeaksContours,irrelaventComponentsRemove,projectionYIrrelaventComponentsRemove,cleanYProjection,oversizecheck,filteredCleanYProjection,interpolationFilteredCleanYProjectionFunction,sdYProjectionBackground,sdD1YProjectionBackground,sdD1YProjectionBackgroundFiltered,d1peakBackgroundThreashold,peaksInProjectionFiltered,peaksInProjection,mainPeakInProjection,secondPeakInProjection,peaksDistance,valleyIntenistyBetweenPeaks,d1PositivePeaks,d1NegtiveValleys,d1PositiveMainPeak,d1NegativeMainValley,d1PositivePeaksFiltered,d1NegativeValleysFiltered,allD1PositivePeaks,allD1NegativeValleys,allD1PeaksAndValleys,ifTail,leftEdge,rightEdge,widthByEdge,leftGFW,rightGFW,ifSecondPeakInProjection,secondPeakInProjectionIntensity,ifFilteredSecondPeakInProjection,secondPeakInProjectionDirection,distanceOfPeaksInProjection,centerLeftIntensityInProjection,centerLeftIntensityInProjectionTotal,centerRightIntensityInProjection,centerRightIntensityInProjectionTotal,intenistyRatioLvsR,skewDirection,highestIntensity,highestIntensityLocation,centerRightEdgeLeftIntensityInProjectionTotal,centerRightEdgeRightIntensityInProjectionTotal,centerLeftEdgeLeftIntensityInProjectionTotal,centerLeftEdgeRightIntensityInProjectionTotal,centerLeftEdgeCompare,centerRightEdgeCompare,ifAsymetry,intensityIntegration,generalizedFW,tailDirection,tailLength,rw,lw,D1negtivepeaks,D1positivepeaks,D1peaks,ifTailDirectionLength,fullMeasurement,highlightedImg,showIntensityPeaks,showAllD1Peaks,linePlot,intensitycentroidX,intensityCurveToPlot,irrelaventComponentsRemoveR90,componentUninterestedDilationMaskR90,backgroundSignalR90,fullBackgroudsMaskR90,yProjectionBackgroundSignalPreR90,yProjectionBackgroundSignalInterpolationR90,yProjectionBackgroundSignalR90,projectionYIrrelaventComponentsRemoveR90,cleanYProjectionR90,intensityIntegrationR90,sdYProjectionBackgroundR90,peaksInProjectionFilteredR90,peaksInProjectionR90,mainPeakInProjectionR90,generalizedFWR90,elongation,orientation,semiaxes,semiaxesRatio,ifSecondPeakInProjectionR90,secondPeakInProjectionR90,valleyIntenistyBetweenPeaksR90,distanceOfPeaksInProjectionR90,measuredLength,scaleBarPixels,totalIntensity,componentsCount,SignificantPeaks,ktArea,peakNumbersInProjectionFiltered,peakNumbersInProjectionFilteredR90,asymetryRatio},(* Local variables used by the image-processing pipeline. *)

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

(*Detect tails via peaks/valleys of the first derivative.*)
d1PositivePeaks=FindPeaks[LowpassFilter[d1List[LowpassFilter[cleanYProjection,lowpassThreashold]],d1LowpassThreashold],0,0,d1peakBackgroundThreashold,InterpolationOrder->3];
d1NegtiveValleys=Transpose[{Transpose[FindPeaks[LowpassFilter[d1List[LowpassFilter[-cleanYProjection,lowpassThreashold]],d1LowpassThreashold],0,0,d1peakBackgroundThreashold,InterpolationOrder->3]][[1]],-Transpose[FindPeaks[LowpassFilter[d1List[LowpassFilter[-cleanYProjection,lowpassThreashold]],d1LowpassThreashold],0,0,d1peakBackgroundThreashold,InterpolationOrder->3]][[2]]}];
d1PositiveMainPeak=Sort[Select[d1PositivePeaks,#[[1]]<mainPeakInProjection[[1]]&],#1[[1]]>#2[[1]]&][[1]];
d1NegativeMainValley=Sort[Select[d1NegtiveValleys,#[[1]]>mainPeakInProjection[[1]]&],#1[[1]]<#2[[1]]&][[1]];

(*Filter derivative peaks by comparing to background fluctuations.*)
d1PositivePeaksFiltered=Select[d1PositivePeaks,peakIntensitySignificance[#[[1]]]>d1PeakBackgroundThreasholdRatio&];
d1NegativeValleysFiltered=Select[d1NegtiveValleys,peakIntensitySignificance[Round[#[[1]]]]>d1PeakBackgroundThreasholdRatio&];
allD1PositivePeaks=DeleteDuplicates[Join[{d1PositiveMainPeak},d1PositivePeaksFiltered]];
allD1NegativeValleys=DeleteDuplicates[Join[{d1NegativeMainValley},d1NegativeValleysFiltered]];
allD1PeaksAndValleys=Join[allD1PositivePeaks,allD1NegativeValleys];

(*If there are more than two first-order derivative extrema,that indicates a second edge of the kinetochore (defined as the tail).*)
ifTail=Length[allD1PeaksAndValleys]>2; 
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

showIntensityPeaks=Show[ListPlot[listPixelsToum[cleanYProjection],PlotStyle->Gray,PlotRange->All],ListLinePlot[{listPixelsToum[intensityCurveToPlot],pointsPixelsToum[If[Length[peaksInProjectionFiltered]==0,{mainPeakInProjection},peaksInProjectionFiltered]]},InterpolationOrder->3,Joined->{True,False},PlotStyle->{Automatic,PointSize[.03]},PlotRange->All]];
showAllD1Peaks=Show[ListPlot[listPixelsToum[d1List[intensityCurveToPlot]],PlotStyle->Brown,PlotRange->All],ListLinePlot[{listPixelsToum[LowpassFilter[d1List[intensityCurveToPlot],d1LowpassThreashold]],pointsPixelsToum[allD1PeaksAndValleys]},InterpolationOrder->3,Joined->{True,False},PlotStyle->{Red,{PointSize[.03],Orange}},PlotRange->All]];
measuredLength=widthXResult;scaleBarPixels=measuredLength/pixelsize;

linePlot=Show[showIntensityPeaks,showAllD1Peaks,PlotRange-> All,GridLines->{Transpose[allD1PeaksAndValleys][[1]]*pixelsize,{}}];

(*Result list:"ktArea(pixels)","width_X(AUC/max)(um)","width_Y(AUC/max)(um)","orientationAngle(deg)","elongationRatioResult","semiaxesRatioResult","tail or not","tail direction","tail length(um)","asymmetryResult","x_1st peak intensity","x_2nd peak intensity","x_valley intensity","Xproj_peak number","y_1st peak intensity","y_2nd peak intensity","y_valley intensity","Yproj_peak number","2D 1st peak intensity","2D 2nd peak intensity","2D peak dist X(um)","2D peak dist Y(um)","number of 2D peaks","ratio 2nd/1st peak","totalIntensity"*)

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
{Show[showAllPeaksContours[[1]],ImageSize->350,ImageResolution->500],Show[showAllPeaks,ImageSize->350,ImageResolution->500,Epilog->{White,Thickness[0.005],Line[{{1,1},{1+scaleBarPixels,1}}],Text[Style[ToString[measuredLength]<>" um",5,White],{3+scaleBarPixels/2,4}  ]}],Show[highlightedImg,ImageSize->350,ImageResolution->500,Epilog->{White,Thickness[0.005],Line[{{1,1},{1+scaleBarPixels,1}}],Text[Style[ToString[measuredLength]<>" um",5,White],{3+scaleBarPixels/2,4}  ]}],Show[linePlot,ImageSize->350,ImageResolution->500]}}]
],{{"NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA"}}]]



(* ::Section:: *)
(* File and Batch Utilities *)


sortImageNames[imageNames_] := SortBy[
  imageNames,
  Quiet@Check[ToExpression[StringSplit[#, {"_", "."}][[-2]]], Infinity]&
];

ensureDirectory[dir_String] := If[DirectoryQ[dir], dir, CreateDirectory[dir, CreateIntermediateDirectories -> True]];

safeExport[path_String, expr_, format_: Automatic] := Quiet@Check[
  If[format === Automatic, Export[path, expr], Export[path, expr, format]],
  Failure["ExportFailed", <|"Path" -> path|>]
];

safeImportImage[path_String] := Module[{img},
  img = Quiet@Check[Import[path], $Failed];
  If[ImageQ[img], img, Failure["ImportFailed", <|"Path" -> path|>]]
];

safeAnalyzeImage[img_Image, timeout_: 120, memoryLimit_: 2*10^9] :=
  MemoryConstrained[
    TimeConstrained[Quiet@Check[allAnalysis[img], $Failed], timeout, $Aborted],
    memoryLimit,
    $Aborted
  ];

ktOutputStem[imageDir_String] := Module[{parts = FileNameSplit[imageDir]},
  If[Length[parts] >= 4,
    parts[[-4]] <> "KT" <> parts[[-2]],
    FileBaseName[DirectoryName[imageDir]] <> "KT"
  ]
];

Options[analyzeKTImageDirectory] = {
  "PerImageTimeout" -> 120,
  "PerImageMemoryLimit" -> 2*10^9,
  "ExportPlots" -> True
};

analyzeKTImageDirectory[imageDir_String, OptionsPattern[]] := Module[
  {imageNames, sortedNames, stem, outputBase, peaksDir, allInOneDir, originalDir,
   frameResults, csvRows, csvPath, failures},
  If[!DirectoryQ[imageDir],
    Return[Failure["MissingDirectory", <|"ImageDirectory" -> imageDir|>]]
  ];

  imageNames = FileNames["*.tif", imageDir];
  If[Length[imageNames] == 0,
    Return[Failure["NoImages", <|"ImageDirectory" -> imageDir|>]]
  ];

  sortedNames = sortImageNames[imageNames];
  stem = ktOutputStem[imageDir];
  outputBase = FileNameJoin[{imageDir, stem}];
  peaksDir = ensureDirectory[FileNameJoin[{outputBase, "peaks"}]];
  allInOneDir = ensureDirectory[FileNameJoin[{outputBase, "allinone"}]];
  originalDir = ensureDirectory[FileNameJoin[{outputBase, "original"}]];

  frameResults = MapIndexed[
    Module[{index = First[#2], imagePath = #1, rawInputImg, analyzed, metrics, plots},
      rawInputImg = safeImportImage[imagePath];
      If[FailureQ[rawInputImg],
        Return[<|"Index" -> index, "Status" -> "Failed", "Reason" -> "ImportFailed", "Path" -> imagePath|>]
      ];

      analyzed = safeAnalyzeImage[rawInputImg, OptionValue["PerImageTimeout"], OptionValue["PerImageMemoryLimit"]];
      If[MemberQ[{$Aborted, $Failed}, analyzed] || FailureQ[analyzed] || !ListQ[analyzed],
        Return[<|"Index" -> index, "Status" -> "Failed", "Reason" -> "AnalysisFailed", "Path" -> imagePath|>]
      ];

      metrics = If[ListQ[First[analyzed]], First[analyzed], ConstantArray["NA", Length[analysisHeaders] - 1]];
      plots = If[Length[analyzed] >= 2 && ListQ[analyzed[[2]]], analyzed[[2]], {}];

      If[TrueQ[OptionValue["ExportPlots"]] && Length[plots] >= 1,
        safeExport[FileNameJoin[{peaksDir, ToString[index] <> "_2d_plot1.png"}], Show[plots[[1]], ImageSize -> 500], "PNG"]
      ];
      If[TrueQ[OptionValue["ExportPlots"]] && Length[plots] >= 2,
        safeExport[FileNameJoin[{peaksDir, ToString[index] <> "_2d_plot2.png"}], Show[plots[[2]], ImageSize -> 500], "PNG"]
      ];
      If[TrueQ[OptionValue["ExportPlots"]] && Length[plots] >= 3,
        safeExport[FileNameJoin[{peaksDir, ToString[index] <> "_boundary_plot.png"}], Show[plots[[3]], ImageSize -> 500], "PNG"]
      ];
      If[TrueQ[OptionValue["ExportPlots"]] && Length[plots] > 0,
        safeExport[FileNameJoin[{allInOneDir, ToString[index] <> "allinone.png"}], Show[GraphicsRow[plots], ImageSize -> 1200], "PNG"]
      ];
      safeExport[FileNameJoin[{originalDir, ToString[index] <> "original.tif"}], ImageAdjust[rawInputImg], "TIFF"];
      ClearSystemCache[];

      <|"Index" -> index, "Status" -> "Success", "Row" -> Join[{index}, metrics], "Path" -> imagePath|>
    ]&,
    sortedNames
  ];

  csvRows = Lookup[Select[frameResults, AssociationQ[#] && Lookup[#, "Status", None] === "Success"&], "Row", {}];
  csvPath = outputBase <> ".csv";
  safeExport[csvPath, Join[{analysisHeaders}, csvRows], "CSV"];
  failures = Select[frameResults, AssociationQ[#] && Lookup[#, "Status", None] =!= "Success"&];

  <|
    "Status" -> If[Length[csvRows] > 0, "Success", "Failed"],
    "ImageDirectory" -> imageDir,
    "CSV" -> csvPath,
    "FramesAnalyzed" -> Length[csvRows],
    "Failures" -> failures
  |>
];

(* Discovers (+-1) KT image folders under the selected cell-set directory. *)
discoverKTImageDirectories[rootDir_String] := Module[
  {cells, channels, ktPairs, ktDirs, imageDirs},
  cells = Select[FileNames["*", rootDir], DirectoryQ];
  channels = Flatten[Select[FileNames["*", #], DirectoryQ]& /@ cells];
  ktPairs = Flatten[Select[FileNames["*", #], DirectoryQ]& /@ channels];
  ktDirs = Flatten[Select[FileNames["*", #], DirectoryQ]& /@ ktPairs];
  imageDirs = FileNameJoin[{#, "(+-1)"}]& /@ ktDirs;
  Select[imageDirs, DirectoryQ]
];

SetAttributes[withManagedParallelKernels, HoldRest];

withManagedParallelKernels[desired_, expr_] := Module[
  {target, before, needed, result},
  target = Replace[desired, Automatic :> $ProcessorCount];
  If[! IntegerQ[target] || target <= 1, Return[expr]];

  before = Kernels[];
  needed = Max[0, target - Length[before]];
  If[needed > 0, Quiet @ Check[LaunchKernels[needed], {}]];

  result = Quiet @ Check[expr, $Failed];
  Scan[Quiet @ Check[CloseKernels[#], Null] &, Complement[Kernels[], before]];
  result
];

Options[runMainBatchAnalysis] = {
  "Kernels" -> Automatic,
  "ChunkSize" -> Automatic,
  "Resume" -> True,
  "ExportPlots" -> True,
  "PerImageTimeout" -> 120,
  "PerImageMemoryLimit" -> 2*10^9
};

runMainBatchAnalysis[rootDir_: Automatic, OptionsPattern[]] := Module[
  {cellSetDir, ktImgDirs, finishedPath, finished, todo, kernels, chunkSize,
   chunks, allResults = {}, chunkResults, successes, failures},
  cellSetDir = If[rootDir === Automatic,
    SystemDialogInput["Directory", WindowTitle -> "Select the folder containing all cells"],
    rootDir
  ];
  If[cellSetDir === $Canceled, Return[$Canceled]];
  If[!DirectoryQ[cellSetDir], Return[Failure["MissingDirectory", <|"Directory" -> cellSetDir|>]]];

  ktImgDirs = discoverKTImageDirectories[cellSetDir];
  finishedPath = FileNameJoin[{cellSetDir, "finished.csv"}];
  finished = If[TrueQ[OptionValue["Resume"]] && FileExistsQ[finishedPath],
    Flatten@Import[finishedPath],
    {}
  ];
  todo = Complement[ktImgDirs, finished];

  kernels = Replace[OptionValue["Kernels"], Automatic :> Max[1, Min[$ProcessorCount - 1, 6]]];
  If[! IntegerQ[kernels] || kernels < 1, kernels = 1];
  chunkSize = Replace[OptionValue["ChunkSize"], Automatic :> Max[1, 4*kernels]];
  chunks = Partition[todo, UpTo[chunkSize]];

  Do[
    If[kernels > 1,
      chunkResults = withManagedParallelKernels[
        kernels,
        If[KeyExistsQ[Association@SystemOptions[], "EvaluateInFrontEnd"], SetSystemOptions["EvaluateInFrontEnd" -> False]];
        DistributeDefinitions[
          allAnalysis, analyzeKTImageDirectory, sortImageNames, safeAnalyzeImage,
          safeImportImage, safeExport, ensureDirectory, ktOutputStem, listPixelsToum,
          pointsPixelsToum, analysisHeaders, pixelsize, backgroundSeprationFactor,
          smallNoiseComponentSize, boundaryDilation, peakIntensityThreasholdFactor,
          peakRatioThreashold, contours, lowpassThreashold, d1LowpassThreashold,
          d1PeakBackgroundThreasholdRatio
        ];
        ParallelMap[
          analyzeKTImageDirectory[#, "ExportPlots" -> OptionValue["ExportPlots"],
            "PerImageTimeout" -> OptionValue["PerImageTimeout"],
            "PerImageMemoryLimit" -> OptionValue["PerImageMemoryLimit"]]&,
          chunk,
          Method -> "CoarsestGrained"
        ]
      ];
      If[chunkResults === $Failed,
        chunkResults = Map[
          analyzeKTImageDirectory[#, "ExportPlots" -> OptionValue["ExportPlots"],
            "PerImageTimeout" -> OptionValue["PerImageTimeout"],
            "PerImageMemoryLimit" -> OptionValue["PerImageMemoryLimit"]]&,
          chunk
        ]
      ],

      chunkResults = Map[
        analyzeKTImageDirectory[#, "ExportPlots" -> OptionValue["ExportPlots"],
          "PerImageTimeout" -> OptionValue["PerImageTimeout"],
          "PerImageMemoryLimit" -> OptionValue["PerImageMemoryLimit"]]&,
        chunk
      ]
    ];

    allResults = Join[allResults, chunkResults];
    successes = Lookup[Select[chunkResults, AssociationQ[#] && Lookup[#, "Status", None] === "Success"&], "ImageDirectory", {}];
    finished = DeleteDuplicates@Join[finished, successes];
    safeExport[finishedPath, finished, "CSV"];
    ClearSystemCache[];
    ,
    {chunk, chunks}
  ];

  failures = Select[allResults, FailureQ[#] || (AssociationQ[#] && Lookup[#, "Status", None] =!= "Success")&];
  If[Length[failures] > 0,
    safeExport[FileNameJoin[{cellSetDir, DateString["ISODate"] <> "_batch_error_log.mx"}], failures]
  ];

  <|
    "RootDirectory" -> cellSetDir,
    "TotalKTDirectories" -> Length[ktImgDirs],
    "Analyzed" -> Length[Select[allResults, AssociationQ[#] && Lookup[#, "Status", None] === "Success"&]],
    "Failed" -> Length[failures],
    "FinishedFile" -> finishedPath,
    "Results" -> allResults
  |>
];



(* ::Section:: *)
(* Usage *)


(*
Choose a cell-set directory and run the batch analysis:

  batch = runMainBatchAnalysis[];

Run without parallel subkernels:

  batch = runMainBatchAnalysis[Automatic, "Kernels" -> 1];

Resume from finished.csv when present:

  batch = runMainBatchAnalysis["C:\\path\\to\\cell-set", "Resume" -> True];
*)
