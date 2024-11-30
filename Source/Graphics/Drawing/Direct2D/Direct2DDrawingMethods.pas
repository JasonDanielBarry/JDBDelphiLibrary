unit Direct2DDrawingMethods;

interface

    uses
        //Delphi
            system.SysUtils, system.types, system.UITypes, system.UIConsts,
            vcl.Graphics,
            vcl.Direct2D, Winapi.D2D1,
        //custom
            DrawingTypes,
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
        procedure drawDirect2DLine( const drawingGeometryIn : TDrawingGeometry;
                                    const axisConverterIn   : TDrawingAxisConverter;
                                    var canvasInOut         : TDirect2DCanvas       );
            var
                pathGeometry    : ID2D1PathGeometry;
                geometrySink    : ID2D1GeometrySink;
                drawingPoints   : TArray<TPointF>;
            begin
                //convert geometry to canvas drawing points
                    drawingPoints := axisConverterIn.arrXY_to_arrLT(
                                                                        drawingGeometryIn.getDrawingPoints()
                                                                   );

                //create the geometry
                    //factory
                        D2DFactory( D2D1_FACTORY_TYPE.D2D1_FACTORY_TYPE_MULTI_THREADED ).CreatePathGeometry( pathGeometry );

                    //path geometry
                        pathGeometry.Open( geometrySink );

                    //geometry sink
                        geometrySink.BeginFigure( D2D1PointF( drawingPoints[0].x, drawingPoints[0].y ), D2D1_FIGURE_BEGIN.D2D1_FIGURE_BEGIN_HOLLOW );

                        geometrySink.AddLine( D2D1PointF( drawingPoints[1].x, drawingPoints[1].y ) );

                        geometrySink.EndFigure( D2D1_FIGURE_END.D2D1_FIGURE_END_OPEN );

                        geometrySink.Close();

                //assign the colour and line thickness using pen
                    canvasInOut.pen.Color := drawingGeometryIn.getLineColour();
                    canvasInOut.pen.Style := drawingGeometryIn.getLineStyle();
                    canvasInOut.pen.Width := drawingGeometryIn.getLineThickness();

                //draw the line
                    canvasInOut.DrawGeometry( pathGeometry );
            end;

    //draw polyline
        procedure drawDirect2DPolyline( const drawingGeometryIn : TDrawingGeometry;
                                        const axisConverterIn   : TDrawingAxisConverter;
                                        var canvasInOut         : TDirect2DCanvas       );
            var
                i               : integer;
                pathGeometry    : ID2D1PathGeometry;
                geometrySink    : ID2D1GeometrySink;
                drawingPoints   : TArray<TPointF>;
                polylinePoints  : TArray<TGeomPoint>;
            begin
                polylinePoints := drawingGeometryIn.getDrawingPoints();

                //only draw if there is more than one vertix
                    if ( length(polylinePoints) < 2) then
                        exit();

                //convert geometry into canvas drawing points
                    drawingPoints := axisConverterIn.arrXY_to_arrLT(
                                                                        polylinePoints
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
                    canvasInOut.Pen.Color := drawingGeometryIn.getLineColour;
                    canvasInOut.Pen.Style := drawingGeometryIn.getLineStyle();
                    canvasInOut.Pen.Width := drawingGeometryIn.getLineThickness();

                //draw the polyline
                    canvasInOut.DrawGeometry( pathGeometry );
            end;

    //draw polygon
        procedure drawDirect2DPolygon(  const drawingGeometryIn : TDrawingGeometry;
                                        const axisConverterIn   : TDrawingAxisConverter;
                                        var canvasInOut         : TDirect2DCanvas      );
            var
                i               : integer;
                pathGeometry    : ID2D1PathGeometry;
                geometrySink    : ID2D1GeometrySink;
                drawingPoints   : TArray<TPointF>;
                polygonPoints   : TArray<TGeomPoint>;
            begin
                polygonPoints := drawingGeometryIn.getDrawingPoints();

                //only draw if there is more than one vertix
                    if ( length(polygonPoints) < 2 ) then
                        exit();

                //convert geometry into canvas drawing points
                    drawingPoints := axisConverterIn.arrXY_to_arrLT(
                                                                        polygonPoints
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
                        if ( NOT(drawingGeometryIn.getFillColour() = TColors.Null) ) then
                            begin
                                canvasInOut.Brush.Color := drawingGeometryIn.getFillColour();
                                canvasInOut.Brush.Style := TBrushStyle.bsSolid;

                                canvasInOut.FillGeometry( pathGeometry );
                            end;

                    //polyline
                        canvasInOut.Pen.Color := drawingGeometryIn.getLineColour();
                        canvasInOut.Pen.Style := drawingGeometryIn.getLineStyle();
                        canvasInOut.Pen.Width := drawingGeometryIn.getLineThickness();

                        canvasInOut.DrawGeometry( pathGeometry );
            end;

    //draw geometry
        procedure drawDirect2DGeometry( const drawingGeometryIn : TDrawingGeometry;
                                        const axisConverterIn   : TDrawingAxisConverter;
                                        var canvasInOut         : TDirect2DCanvas       );
            begin
                case ( drawingGeometryIn.getDrawingType() ) of
                    EDrawingType.dtLine:
                        drawDirect2DLine(
                                            drawingGeometryIn,
                                            axisConverterIn,
                                            canvasInOut
                                        );

                    EDrawingType.dtPolygon:
                        drawDirect2DPolygon(
                                                drawingGeometryIn,
                                                axisConverterIn,
                                                canvasInOut
                                           );

                    EDrawingType.dtPolyline:
                        drawDirect2DPolyline(
                                                drawingGeometryIn,
                                                axisConverterIn,
                                                canvasInOut
                                            );
                end;
            end;


end.
