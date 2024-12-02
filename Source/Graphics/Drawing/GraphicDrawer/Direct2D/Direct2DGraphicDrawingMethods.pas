unit Direct2DGraphicDrawingMethods;

interface

    uses
        //Delphi
            system.SysUtils, system.types, system.UITypes, system.UIConsts,
            vcl.Graphics,
            vcl.Direct2D, Winapi.D2D1,
        //custom
            GraphicDrawingTypes,
            DrawingAxisConversionClass,
            GraphicGeometryClass,
            GeometryTypes,
            GeometryBaseClass, GeomLineClass, GeomPolyLineClass, GeomPolygonClass;

    //draw geometry
        procedure drawDirect2DGeometry( const drawingGeometryIn : TGraphicGeometry;
                                        const axisConverterIn   : TDrawingAxisConverter;
                                        var canvasInOut         : TDirect2DCanvas       );

implementation

    //draw geometry
        procedure determineRegionState( const drawingGeometryIn : TGraphicGeometry;
                                        out figureBeginInOut    : D2D1_FIGURE_BEGIN;
                                        out figureEndInOut      : D2D1_FIGURE_END   );
            begin
                case ( drawingGeometryIn.getGraphicDrawingType() ) of
                    EGraphicDrawing.gdLine, EGraphicDrawing.gdPolyline:
                        begin
                            figureBeginInOut    := D2D1_FIGURE_BEGIN.D2D1_FIGURE_BEGIN_HOLLOW;
                            figureEndInOut      := D2D1_FIGURE_END.D2D1_FIGURE_END_OPEN;
                        end;

                    EGraphicDrawing.gdPolygon:
                        begin
                            figureBeginInOut    := D2D1_FIGURE_BEGIN.D2D1_FIGURE_BEGIN_FILLED;
                            figureEndInOut      := D2D1_FIGURE_END.D2D1_FIGURE_END_CLOSED;
                        end;
                end;
            end;

        function createPathGeometry(const figureBeginIn     : D2D1_FIGURE_BEGIN;
                                    const figureEndIn       : D2D1_FIGURE_END;
                                    const axisConverterIn   : TDrawingAxisConverter;
                                    const geometryPointsIn  : TArray<TGeomPoint>    ) : ID2D1PathGeometry;
            var
                i               : integer;
                geometrySink    : ID2D1GeometrySink;
                pathGeometryOut : ID2D1PathGeometry;
                drawingPoints   : TArray<TPointF>;
            begin
                //convert geometry into canvas drawing points
                    drawingPoints := axisConverterIn.arrXY_to_arrLT(
                                                                        geometryPointsIn
                                                                   );

                //factory
                    D2DFactory( D2D1_FACTORY_TYPE.D2D1_FACTORY_TYPE_MULTI_THREADED ).CreatePathGeometry( pathGeometryOut );

                //path geometry
                    pathGeometryOut.Open( geometrySink );

                //geometry sink
                    geometrySink.BeginFigure( D2D1PointF( drawingPoints[0].x, drawingPoints[0].y ), figureBeginIn );

                    //add lines
                        //single line
                            if ( length(drawingPoints) < 3 ) then
                                geometrySink.AddLine( D2D1PointF( drawingPoints[1].x, drawingPoints[1].y ) )
                        //polyline
                            else
                                begin
                                    for i := 1 to ( length(drawingPoints) - 1 ) do
                                        geometrySink.AddLine( D2D1PointF( drawingPoints[i].x, drawingPoints[i].y ) );
                                end;

                    geometrySink.EndFigure( figureEndIn );

                    geometrySink.Close();

                result := pathGeometryOut;
            end;

        procedure assignCanvasProperties(   const pathGeometryIn    : ID2D1PathGeometry;
                                            const drawingGeometryIn : TGraphicGeometry;
                                            var canvasInOut         : TDirect2DCanvas       );
            begin
                //fill
                    if ( (drawingGeometryIn.getGraphicDrawingType() = EGraphicDrawing.gdPolygon) AND (drawingGeometryIn.getFillColour() <> TColors.Null) ) then
                        begin
                            canvasInOut.Brush.Color := drawingGeometryIn.getFillColour();
                            canvasInOut.Brush.Style := TBrushStyle.bsSolid;

                            canvasInOut.FillGeometry( pathGeometryIn );
                        end;

                //line
                    canvasInOut.Pen.Color := drawingGeometryIn.getLineColour();
                    canvasInOut.Pen.Style := drawingGeometryIn.getLineStyle();
                    canvasInOut.Pen.Width := drawingGeometryIn.getLineThickness();

                    canvasInOut.DrawGeometry( pathGeometryIn );
            end;

        procedure drawDirect2DGeometry( const drawingGeometryIn : TGraphicGeometry;
                                        const axisConverterIn   : TDrawingAxisConverter;
                                        var canvasInOut         : TDirect2DCanvas       );
            var

                figureBegin     : D2D1_FIGURE_BEGIN;
                figureEnd       : D2D1_FIGURE_END;
                pathGeometry    : ID2D1PathGeometry;
                geometryPoints  : TArray<TGeomPoint>;
            begin
                geometryPoints  := drawingGeometryIn.getDrawingPoints();

                //only draw if there is more than two points
                    if ( length(geometryPoints) < 2 ) then
                        exit();

                //determine if the region is open or closed
                    determineRegionState( drawingGeometryIn, figureBegin, figureEnd );

                //generate path geometry
                    pathGeometry := createPathGeometry(figureBegin, figureEnd, axisConverterIn, geometryPoints);

                //assign the canvas properties
                    assignCanvasProperties( pathGeometry, drawingGeometryIn, canvasInOut );
            end;


end.
