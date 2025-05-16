unit GraphicMousePointTrackerClass;

interface

    uses
        Winapi.D2D1,
        system.SysUtils, System.Math, system.Classes, system.UITypes, system.Types,
        Vcl.Direct2D, vcl.Graphics, vcl.StdCtrls, vcl.Themes,
        InterpolatorClass,
        LinearAlgebraTypes, VectorMethods,
        GeometryTypes, GeomBox,
        GraphicDrawingTypes,
        GraphXYTypes,
        DrawingAxisConversionClass,
        GraphicObjectBaseClass,
        GraphicTextClass,
        GraphicEllipseClass;

    type
        TGraphicMousePointTracker = class(TGraphicObject)
            private
                var
                    graphPlotType       : EGraphPlotType;
                    lineInterpolator    : TInterpolator;
                    graphicPointText    : TGraphicText;
                    graphicPointEllipse : TGraphicEllipse;
                    mousePointLT        : TPointF;
                    arrPointsLT         : TArray<TPointF>;
                    arrPlotPointsXY     : TArray<TGeomPoint>;
                //determine the closest point to the mouse
                    procedure determinePointAndIndexClosestToMouse( const mousePointIn          : TGeomPoint;
                                                                    const axisConverterIn       : TDrawingAxisConverter;
                                                                    out closestPointIndexOut    : integer;
                                                                    out closestPointOut         : TGeomPoint            );
                //find point on
                    //continuous line
                        //use vector projection to find the closest interpolated point
                            function findClosestInterpolatedPoint(const mousePointLTIn, lowerClosestPointIn, closestPointLTIn, upperClosestPointLTIn : TPointF) : TPointF;
                        function determineLinePlotPointClosestToMouse(  const mousePointIn      : TGeomPoint;
                                                                        const axisConverterIn   : TDrawingAxisConverter ) : TGeomPoint;
                    //scatter plot
                        function determineScatterPlotPointClosestToMouse(   const mousePointIn      : TGeomPoint;
                                                                            const axisConverterIn   : TDrawingAxisConverter ) : TGeomPoint;
            public
                //constructor
                    constructor create(const graphPlotIn : TGraphXYPlot);
                //destructor
                    destructor destroy(); override;
                //draw to canvas
                    procedure drawToCanvas( const axisConverterIn   : TDrawingAxisConverter;
                                            var canvasInOut         : TDirect2DCanvas       ); override;
                //bounding box
                    function determineBoundingBox() : TGeomBox; override;
        end;

implementation

    //private
        //determine the closest point to the mouse
            procedure TGraphicMousePointTracker.determinePointAndIndexClosestToMouse(   const mousePointIn          : TGeomPoint;
                                                                                        const axisConverterIn       : TDrawingAxisConverter;
                                                                                        out closestPointIndexOut    : integer;
                                                                                        out closestPointOut         : TGeomPoint            );
                var
                    i, arrLen           : integer;
                    pointDistance,
                    minPointDistance    : double;
                begin
                    mousePointLT    := axisConverterIn.XY_to_LT( mousePointIn );
                    arrPointsLT     := axisConverterIn.arrXY_to_arrLT( arrPlotPointsXY );

                    arrLen := length( arrPlotPointsXY );

                    minPointDistance := 1e15;

                    for i := 0 to ( arrLen - 1 ) do
                        begin
                            pointDistance := Norm( [mousePointLT.x - arrPointsLT[i].x, mousePointLT.y - arrPointsLT[i].y] );

                            if ( minPointDistance < pointDistance ) then
                                Continue;

                            closestPointIndexOut := i;

                            minPointDistance := pointDistance;

                            closestPointOut.copyPoint( arrPlotPointsXY[i] );
                        end;
                end;

        //find point on
            //continuous line
                //use vector projection to find the closest interpolated point
                    function TGraphicMousePointTracker.findClosestInterpolatedPoint(const mousePointLTIn, lowerClosestPointIn, closestPointLTIn, upperClosestPointLTIn : TPointF) : TPointF;
                        var
                            minLength       : double;
                            closestPointOut : TPointF;
                        procedure _interpolateAlongLine();
                            var
                                i, interpCount      : integer;
                                lineLength, t       : double;
                                lineInterpPoint     : TPointF;
                                mouseToLineVector   : TLAVector;
                            begin
                                interpCount := min(100, Ceil( lineInterpolator.calculateLineLength() ) );

                                for i := 0 to interpCount do
                                    begin
                                        t := i / interpCount;

                                        lineInterpPoint := lineInterpolator.interpolate( t );

                                        mouseToLineVector := [ lineInterpPoint.X - mousePointLTIn.X, lineInterpPoint.Y - mousePointLTIn.Y ];

                                        lineLength := Norm( mouseToLineVector );

                                        if NOT( lineLength < minLength ) then
                                            Continue;

                                        minLength := lineLength;
                                        closestPointOut := lineInterpPoint;
                                    end;
                            end;
                        begin
                            minLength := 1e9;

                            //interpolate along first line
                                lineInterpolator.setPoints( lowerClosestPointIn, closestPointLTIn );

                                _interpolateAlongLine();

                            //interpolate along second line
                                lineInterpolator.setPoints( closestPointLTIn, upperClosestPointLTIn );

                                _interpolateAlongLine();

                            result := closestPointOut;
                        end;

                function TGraphicMousePointTracker.determineLinePlotPointClosestToMouse(const mousePointIn      : TGeomPoint;
                                                                                        const axisConverterIn   : TDrawingAxisConverter) : TGeomPoint;
                    var
                        arrLen,
                        closestPointIndex           : integer;
                        closestPointXY              : TGeomPoint;
                        closestInterpolatedPointLT  : TPointF;
                    begin
                        //find the two closest points
                            determinePointAndIndexClosestToMouse( mousePointIn, axisConverterIn, closestPointIndex, closestPointXY );

                            arrLen := length( arrPointsLT);

                            if (closestPointIndex = 0) then
                                closestPointIndex := 1;

                            if ( closestPointIndex = arrLen - 1 ) then
                                closestPointIndex := arrLen - 2;

                        //find the point along the interconnecting line which is the closest to the mouse
                            closestInterpolatedPointLT := findClosestInterpolatedPoint( mousePointLT,
                                                                                        arrPointsLT[closestPointIndex - 1],
                                                                                        arrPointsLT[closestPointIndex],
                                                                                        arrPointsLT[closestPointIndex + 1] );

                        result := axisConverterIn.LT_to_XY( closestInterpolatedPointLT );
                    end;

            //scatter plot
                function TGraphicMousePointTracker.determineScatterPlotPointClosestToMouse( const mousePointIn      : TGeomPoint;
                                                                                            const axisConverterIn   : TDrawingAxisConverter ) : TGeomPoint;
                    var
                        closestPointIndex   : integer;
                        closestPointXY      : TGeomPoint;
                    begin
                        determinePointAndIndexClosestToMouse( mousePointIn, axisConverterIn, closestPointIndex, closestPointXY );

                        result := closestPointXY;
                    end;

    //public
        //constructor
            constructor TGraphicMousePointTracker.create(const graphPlotIn : TGraphXYPlot);
                begin
                    inherited create();

                    lineInterpolator := TInterpolator.create();

                    graphPlotType := graphPlotIn.graphPlotType;

                    TGeomPoint.copyPoints( graphPlotIn.arrDataPoints, arrPlotPointsXY );

                    graphicPointText    := TGraphicText.create( True, 9, 0, '', TAlignment.taLeftJustify, TTextLayout.tlCenter, clWindowText, [], TGeomPoint.create( 0, 0 ) );
                    graphicPointEllipse := TGraphicEllipse.create( True, 1, 8, 8, clWindowText, clWindowText, TPenStyle.psSolid, TGeomPoint.create( 0, 0 ) );
                    graphicPointEllipse.setObjectScaleType( EScaleType.scCanvas );
                end;

        //destructor
            destructor TGraphicMousePointTracker.destroy();
                begin
                    FreeAndNil( lineInterpolator );
                    FreeAndNil( graphicPointText );

                    inherited destroy();
                end;

        //draw to canvas
            procedure TGraphicMousePointTracker.drawToCanvas(   const axisConverterIn   : TDrawingAxisConverter;
                                                                var canvasInOut         : TDirect2DCanvas       );
                var
                    closestPointIndex   : integer;
                    coordText           : string;
                    mousePointXY,
                    closestPointXY      : TGeomPoint;
                begin
                    mousePointXY := axisConverterIn.getMouseCoordinatesXY();

                    case ( graphPlotType ) of
                        EGraphPlotType.gpScatter:
                            closestPointXY := determineScatterPlotPointClosestToMouse( mousePointXY, axisConverterIn );

                        EGraphPlotType.gpLine, EGraphPlotType.gpFuntion:
                            closestPointXY := determineLinePlotPointClosestToMouse( mousePointXY, axisConverterIn );
                    end;

                    graphicPointEllipse.setCentrePoint( closestPointXY.x, closestPointXY.y );

                    graphicPointEllipse.drawToCanvas( axisConverterIn, canvasInOut );

                    graphicPointText.setHandlePoint( closestPointXY.x + axisConverterIn.dL_To_dX(4), closestPointXY.y );

                    coordText := '(' + FloatToStrF(closestPointXY.x, ffFixed, 5, 3) + ', ' + FloatToStrF(closestPointXY.y, ffFixed, 5, 3) + ')';

                    graphicPointText.setTextString( coordText );

                    graphicPointText.drawToCanvas( axisConverterIn, canvasInOut );
                end;

        //bounding box
            function TGraphicMousePointTracker.determineBoundingBox() : TGeomBox;
                begin
                    result := TGeomBox.determineBoundingBox( arrPlotPointsXY );
                end;

end.
