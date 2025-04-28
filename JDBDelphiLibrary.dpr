program JDBDelphiLibrary;

{$IFNDEF TESTINSIGHT}
{$APPTYPE CONSOLE}
{$ENDIF}
{$STRONGLINKTYPES ON}
uses
  System.SysUtils,
  {$IFDEF TESTINSIGHT}
  TestInsight.DUnitX,
  {$ELSE}
  DUnitX.Loggers.Console,
  DUnitX.Loggers.Xml.NUnit,
  {$ENDIF }
  DUnitX.TestFramework,
  LimitStateAngleClass in 'Source\Engineering\LimitStateAngleClass.pas',
  LimitStateMaterialClass in 'Source\Engineering\LimitStateMaterialClass.pas',
  GeometryBaseClass in 'Source\Geometry\GeometryBaseClass.pas',
  GeometryMathMethods in 'Source\Geometry\GeometryMathMethods.pas',
  GeometryTypes in 'Source\Geometry\GeometryTypes.pas',
  GeomLineClass in 'Source\Geometry\GeomLineClass.pas',
  GeomPolygonClass in 'Source\Geometry\GeomPolygonClass.pas',
  GeomPolyLineClass in 'Source\Geometry\GeomPolyLineClass.pas',
  GeomSpaceVectorClass in 'Source\Geometry\GeomSpaceVectorClass.pas',
  TEST_GeomLineClass in 'Source\Geometry\TEST_GeomLineClass.pas',
  DrawingAxisConversionBaseClass in 'Source\Graphics\Drawing\AxisConversion\DrawingAxisConversionBaseClass.pas',
  DrawingAxisConversionCalculationsClass in 'Source\Graphics\Drawing\AxisConversion\DrawingAxisConversionCalculationsClass.pas',
  DrawingAxisConversionClass in 'Source\Graphics\Drawing\AxisConversion\DrawingAxisConversionClass.pas',
  DrawingAxisConversionPanningClass in 'Source\Graphics\Drawing\AxisConversion\DrawingAxisConversionPanningClass.pas',
  DrawingAxisConversionZoomingClass in 'Source\Graphics\Drawing\AxisConversion\DrawingAxisConversionZoomingClass.pas',
  LinearRescalingMethods in 'Source\Mathematics\LinearRescalingMethods.pas',
  InterpolatorClass in 'Source\Mathematics\InterpolatorClass.pas',
  LinearAlgebraTypes in 'Source\Mathematics\LinearAlgebra\LinearAlgebraTypes.pas',
  LineIntersectionMethods in 'Source\Mathematics\LinearAlgebra\LineIntersectionMethods.pas',
  TEST_MatrixMethods in 'Source\Mathematics\LinearAlgebra\Matrices\TEST_MatrixMethods.pas',
  VectorMethods in 'Source\Mathematics\LinearAlgebra\VectorMethods.pas',
  MatrixDeterminantMethods in 'Source\Mathematics\LinearAlgebra\Matrices\MatrixDeterminantMethods.pas',
  MatrixHelperMethods in 'Source\Mathematics\LinearAlgebra\Matrices\MatrixHelperMethods.pas',
  MatrixMethods in 'Source\Mathematics\LinearAlgebra\Matrices\MatrixMethods.pas',
  GeneralComponentHelperMethods in 'Source\WindowsVCL\GeneralComponentHelperMethods.pas',
  CustomComponentPanelClass in 'Source\WindowsVCL\CustomComponents\CustomComponentPanelClass.pas',
  CustomComponentRegistrationUnit in 'Source\WindowsVCL\CustomComponents\CustomComponentRegistrationUnit.pas',
  Graphic2DComponent in 'Source\WindowsVCL\CustomComponents\Graphic2DUnits\Graphic2DComponent.pas',
  Graphic2DFrame in 'Source\WindowsVCL\CustomComponents\Graphic2DUnits\Graphic2DFrame.pas' {CustomGraphic2D: TFrame},
  Drawer2DTypes in 'Source\WindowsVCL\CustomComponents\Drawer2DUnits\Drawer2DTypes.pas',
  PageControlHelperClass in 'Source\WindowsVCL\HelperClasses\PageControlHelperClass.pas',
  StringGridInterposerClass in 'Source\WindowsVCL\HelperClasses\StringGridInterposerClass.pas',
  ColourMethods in 'Source\Graphics\ColourMethods.pas',
  DrawingAxisConversionMouseControlClass in 'Source\Graphics\Drawing\AxisConversion\DrawingAxisConversionMouseControlClass.pas',
  GeomBox in 'Source\Geometry\GeomBox.pas',
  LinearInterpolationMethods in 'Source\Mathematics\LinearInterpolationMethods.pas',
  DrawingAxisConversionAspectRatioClass in 'Source\Graphics\Drawing\AxisConversion\DrawingAxisConversionAspectRatioClass.pas',
  GraphicDrawingTypes in 'Source\Graphics\Drawing\GraphicDrawingTypes.pas',
  GraphicObjectBaseClass in 'Source\Graphics\Drawing\GraphicDrawingClasses\GraphicObjectBaseClass.pas',
  GraphicDrawerAxisConversionInterfaceClass in 'Source\Graphics\Drawing\GraphicDrawer\GraphicDrawerAxisConversionInterfaceClass.pas',
  GraphicDrawerBaseClass in 'Source\Graphics\Drawing\GraphicDrawer\GraphicDrawerBaseClass.pas',
  GraphicDrawerLayersClass in 'Source\Graphics\Drawing\GraphicDrawer\GraphicDrawerLayersClass.pas',
  Direct2DGraphicDrawingClass in 'Source\Graphics\Drawing\GraphicDrawer\Direct2D\Direct2DGraphicDrawingClass.pas',
  GraphicDrawerObjectAdderClass in 'Source\Graphics\Drawing\GraphicDrawer\GraphicDrawerObjectAdderClass.pas',
  GraphicTextClass in 'Source\Graphics\Drawing\GraphicDrawingClasses\GraphicTextClass.pas',
  InputManagerClass in 'Source\WindowsVCL\InputManagerClass.pas',
  FileReaderWriterClass in 'Source\FileManagement\FileReaderWriterClass.pas',
  TEST_FileReaderWriterClass in 'Source\FileManagement\TEST_FileReaderWriterClass.pas',
  ArrayConversionMethods in 'Source\FileManagement\ArrayConversionMethods.pas',
  XMLDocumentMethods in 'Source\FileManagement\XMLDocumentMethods.pas',
  GraphicObjectGroupClass in 'Source\Graphics\Drawing\GraphicDrawingClasses\GraphicObjectGroupClasses\GraphicObjectGroupClass.pas',
  GraphicArrowClass in 'Source\Graphics\Drawing\GraphicDrawingClasses\GraphicObjectGroupClasses\GraphicArrowClass.pas',
  GraphicArrowGroupClass in 'Source\Graphics\Drawing\GraphicDrawingClasses\GraphicObjectGroupClasses\GraphicArrowGroupClass.pas',
  Drawer2DPaintBoxClass in 'Source\WindowsVCL\CustomComponents\Drawer2DUnits\Drawer2DPaintBoxClass.pas',
  GraphXYComponent in 'Source\WindowsVCL\CustomComponents\GraphXYUnits\GraphXYComponent.pas',
  GraphXYFrame in 'Source\WindowsVCL\CustomComponents\GraphXYUnits\GraphXYFrame.pas' {CustomGraphXY: TFrame},
  GraphicArcClass in 'Source\Graphics\Drawing\GraphicDrawingClasses\GraphicObjectShapeClasses\GraphicArcClass.pas',
  GraphicEllipseClass in 'Source\Graphics\Drawing\GraphicDrawingClasses\GraphicObjectShapeClasses\GraphicEllipseClass.pas',
  GraphicGeometryClass in 'Source\Graphics\Drawing\GraphicDrawingClasses\GraphicObjectShapeClasses\GraphicGeometryClass.pas',
  GraphicLineClass in 'Source\Graphics\Drawing\GraphicDrawingClasses\GraphicObjectShapeClasses\GraphicLineClass.pas',
  GraphicPolygonClass in 'Source\Graphics\Drawing\GraphicDrawingClasses\GraphicObjectShapeClasses\GraphicPolygonClass.pas',
  GraphicPolylineClass in 'Source\Graphics\Drawing\GraphicDrawingClasses\GraphicObjectShapeClasses\GraphicPolylineClass.pas',
  GraphicRectangleClass in 'Source\Graphics\Drawing\GraphicDrawingClasses\GraphicObjectShapeClasses\GraphicRectangleClass.pas';

{ keep comment here to protect the following conditional from being removed by the IDE when adding a unit }
{$IFNDEF TESTINSIGHT}
var
  runner: ITestRunner;
  results: IRunResults;
  logger: ITestLogger;
  nunitLogger : ITestLogger;
{$ENDIF}
begin
{$IFDEF TESTINSIGHT}
  TestInsight.DUnitX.RunRegisteredTests;
{$ELSE}
  try
    //Check command line options, will exit if invalid
    TDUnitX.CheckCommandLine;
    //Create the test runner
    runner := TDUnitX.CreateRunner;
    //Tell the runner to use RTTI to find Fixtures
    runner.UseRTTI := True;
    //When true, Assertions must be made during tests;
    runner.FailsOnNoAsserts := False;

    //tell the runner how we will log things
    //Log to the console window if desired
    if TDUnitX.Options.ConsoleMode <> TDunitXConsoleMode.Off then
    begin
      logger := TDUnitXConsoleLogger.Create(TDUnitX.Options.ConsoleMode = TDunitXConsoleMode.Quiet);
      runner.AddLogger(logger);
    end;
    //Generate an NUnit compatible XML File
    nunitLogger := TDUnitXXMLNUnitFileLogger.Create(TDUnitX.Options.XMLOutputFile);
    runner.AddLogger(nunitLogger);

    //Run tests
    results := runner.Execute;
    if not results.AllPassed then
      System.ExitCode := EXIT_ERRORS;

    {$IFNDEF CI}
    //We don't want this happening when running under CI.
    begin
      System.Write('Done.. press <Enter> key to quit.');
      System.Readln;
    end;
    {$ENDIF}
  except
    on E: Exception do
      System.Writeln(E.ClassName, ': ', E.Message);
  end;
{$ENDIF}
end.
