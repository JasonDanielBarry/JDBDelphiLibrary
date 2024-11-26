unit Direct2DDrawingMethods;

interface

    uses
        //Delphi
            system.SysUtils, system.types, system.UITypes, system.UIConsts,
            vcl.Graphics,
            vcl.Direct2D, Winapi.D2D1,
        //custom
            DrawingAxisConversionClass,
            DrawingGeometryClass,
            GeometryTypes,
            GeometryBaseClass, GeomLineClass, GeomPolyLineClass, GeomPolygonClass;

    //draw geometry
        procedure drawDirect2DGeometry( const drawingGeometryIn : TDrawingGeometry;
                                        const axisConverterIn   : TDrawingAxisConverter;
                                        var canvasInOut         : TDirect2DCanvas       );

implementation

    //draw line
        procedure drawDirect2DLine( const lineThicknessIn   : integer;
                                    const linePointsIn      : TArray<TGeomPoint>;
                                    const colourIn          : TColor;
                                    const axisConverterIn   : TDrawingAxisConverter;
                                    var canvasInOut         : TDirect2DCanvas       );
            var
                D2DPen          : TDirect2DPen;
                drawingPoints   : TArray<TPointF>;
            begin
                //convert geometry to canvas drawing points
                    drawingPoints := axisConverterIn.arrXY_to_arrLT(
                                                                        linePointsIn
                                                                   );

                //assign the colour and line thickness using pen
                    D2DPen := TDirect2DPen.Create( canvasInOut );
                    D2DPen.Color := colourIn;
                    D2DPen.Style := TPenStyle.psSolid;

                //draw the line
                    canvasInOut.RenderTarget.DrawLine(
                                                            D2D1PointF(drawingPoints[0].X, drawingPoints[0].Y), //start point
                                                            D2D1PointF(drawingPoints[1].X, drawingPoints[1].Y), //end points
                                                            D2DPen.Brush.Handle, //brush
                                                            lineThicknessIn, //line thickness
                                                            D2DPen.StrokeStyle //stroke style
                                                     );
            end;

    //draw polyline
        procedure drawDirect2DPolyline( const lineThicknessIn   : integer;
                                        const polylinePointsIn  : TArray<TGeomPoint>;
                                        const colourIn          : TColor;
                                        const axisConverterIn   : TDrawingAxisConverter;
                                        var canvasInOut         : TDirect2DCanvas       );
            var
                i               : integer;
                pathGeometry    : ID2D1PathGeometry;
                geometrySink    : ID2D1GeometrySink;
                drawingPoints   : TArray<TPointF>;
            begin
                //only draw if there is more than one vertix
                    if ( length(polylinePointsIn) < 2) then
                        exit();

                //convert geometry into canvas drawing points
                    drawingPoints := axisConverterIn.arrXY_to_arrLT(
                                                                        polylinePointsIn
                                                                   );

                //create the geometry
                    //factory
                        D2DFactory( D2D1_FACTORY_TYPE.D2D1_FACTORY_TYPE_MULTI_THREADED ).CreatePathGeometry( pathGeometry );

                    //path geometry
                        pathGeometry.Open( geometrySink );

                    //geometry sink
                        geometrySink.BeginFigure( D2D1PointF( drawingPoints[0].x, drawingPoints[0].y ), D2D1_FIGURE_BEGIN.D2D1_FIGURE_BEGIN_HOLLOW );

                        for i := 1 to ( length(drawingPoints) - 1 ) do
                            geometrySink.AddLine( D2D1PointF( drawingPoints[i].x, drawingPoints[i].y ) );

                        geometrySink.EndFigure( D2D1_FIGURE_END.D2D1_FIGURE_END_OPEN );

                        geometrySink.Close();

                //assign the canvas the colour and line thickness
                    canvasInOut.Pen.Color := AlphaColorToColor(colourIn);
                    canvasInOut.Pen.Style := TPenStyle.psSolid;
                    canvasInOut.Pen.Width := lineThicknessIn;

                //draw the polyline
                    canvasInOut.DrawGeometry( pathGeometry );
            end;

    //draw polygon
        procedure drawDirect2DPolygon(  const   lineThicknessIn : integer;
                                        const   polygonPointsIn : TArray<TGeomPoint>;
                                        const   fillColourIn,
                                                lineColourIn    : TColor;
                                        const   axisConverterIn : TDrawingAxisConverter;
                                        var     canvasInOut     : TDirect2DCanvas      );
            var
                i               : integer;
                pathGeometry    : ID2D1PathGeometry;
                geometrySink    : ID2D1GeometrySink;
                drawingPoints   : TArray<TPointF>;
            begin
                //only draw if there is more than one vertix
                    if ( length(polygonPointsIn) < 2 ) then
                        exit();

                //convert geometry into canvas drawing points
                    drawingPoints := axisConverterIn.arrXY_to_arrLT(
                                                                        polygonPointsIn
                                                                   );

                //create the geometry
                    //factory
                        D2DFactory( D2D1_FACTORY_TYPE.D2D1_FACTORY_TYPE_MULTI_THREADED ).CreatePathGeometry( pathGeometry );

                    //path geometry
                        pathGeometry.Open( geometrySink );

                    //geometry sink
                        geometrySink.BeginFigure( D2D1PointF( drawingPoints[0].x, drawingPoints[0].y ), D2D1_FIGURE_BEGIN.D2D1_FIGURE_BEGIN_FILLED );

                        for i := 1 to ( length(drawingPoints) - 1 ) do
                            geometrySink.AddLine( D2D1PointF( drawingPoints[i].x, drawingPoints[i].y ) );

                        geometrySink.EndFigure( D2D1_FIGURE_END.D2D1_FIGURE_END_CLOSED );

                        geometrySink.Close();

                //assign the canvas the colour and line thickness
                    //fill
                        canvasInOut.Brush.Color := AlphaColorToColor(fillColourIn);
                        canvasInOut.Brush.Style := TBrushStyle.bsSolid;

                    //line
                        canvasInOut.Pen.Color := AlphaColorToColor(lineColourIn);
                        canvasInOut.Pen.Style := TPenStyle.psSolid;
                        canvasInOut.Pen.Width := lineThicknessIn;

                //draw the polyline
                    canvasInOut.FillGeometry( pathGeometry );
                    canvasInOut.DrawGeometry( pathGeometry );
            end;

    //draw geometry
        procedure drawDirect2DGeometry( const drawingGeometryIn : TDrawingGeometry;
                                        const axisConverterIn   : TDrawingAxisConverter;
                                        var canvasInOut         : TDirect2DCanvas       );
            var
                lineThickness           : integer;
                geometryType            : EGeomType;
                fillColour, lineColour  : TColor;
                geometry                : TGeomBase;
            begin
                lineThickness   := drawingGeometryIn.getLineThickness();
                fillColour      := drawingGeometryIn.getFillColour();
                lineColour      := drawingGeometryIn.getLineColour();
                geometryType    := drawingGeometryIn.getGeometry().getGeomType();

                case (geometryType) of
                    EGeomType.gtLine:
                        drawDirect2DLine(
                                            lineThickness,
                                            drawingGeometryIn.getDrawingPoints(),
                                            lineColour,
                                            axisConverterIn,
                                            canvasInOut
                                        );

                    EGeomType.gtPolyline:
                        drawDirect2DPolyline(
                                                lineThickness,
                                                drawingGeometryIn.getDrawingPoints(),
                                                lineColour,
                                                axisConverterIn,
                                                canvasInOut
                                            );

                    EGeomType.gtPolygon:
                        drawDirect2DPolygon(
                                                lineThickness,
                                                drawingGeometryIn.getDrawingPoints(),
                                                fillColour,
                                                lineColour,
                                                axisConverterIn,
                                                canvasInOut
                                           );
                end;
            end;


end.
