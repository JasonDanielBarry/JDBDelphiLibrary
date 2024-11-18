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
                                        var canvasInOut         : TDirect2DCanvas;
                                        const freeGeometryIn    : boolean = True        );

implementation

    //draw line
        procedure drawDirect2DLine( const lineIn            : TGeomLine;
                                    const colourIn          : TAlphaColor;
                                    const axisConverterIn   : TDrawingAxisConverter;
                                    var canvasInOut         : TDirect2DCanvas;
                                    const freeLineIn        : boolean = True;
                                    const lineThicknessIn   : integer = 2           );
            var
                drawingPoints : TArray<TPoint>;
            begin
                //convert geometry to canvas drawing points
                    drawingPoints := axisConverterIn.arrXY_to_arrLT(
                                                                        lineIn.getDrawingPoints()
                                                                   );

                //assign the canvas the colour and line thickness
                    canvasInOut.Pen.Color := AlphaColorToColor(colourIn);
                    canvasInOut.Pen.Style := TPenStyle.psSolid;
                    canvasInOut.Pen.Width := lineThicknessIn;

                //draw the line
                    canvasInOut.MoveTo( drawingPoints[0].X, drawingPoints[0].Y );
                    canvasInOut.LineTo( drawingPoints[1].X, drawingPoints[1].Y );

                if (freeLineIn) then
                    begin
                        FreeAndNil( lineIn );
                    end;
            end;

    //draw polyline
        procedure drawDirect2DPolyline( const polylineIn        : TGeomPolyLine;
                                        const colourIn          : TAlphaColor;
                                        const axisConverterIn   : TDrawingAxisConverter;
                                        var canvasInOut         : TDirect2DCanvas;
                                        const freePolyLineIn    : boolean = True;
                                        const lineThicknessIn   : integer = 2           );
            var
                drawingPoints : TArray<TPoint>;
            begin
                if (polylineIn.vertexCount() > 1) then //only draw if there is more than one vertix
                    begin
                        //convert geometry into canvas drawing points
                            drawingPoints := axisConverterIn.arrXY_to_arrLT(
                                                                                polylineIn.getDrawingPoints()
                                                                           );

                        //assign the canvas the colour and line thickness
                            canvasInOut.Pen.Color := AlphaColorToColor(colourIn);
                            canvasInOut.Pen.Style := TPenStyle.psSolid;
                            canvasInOut.Pen.Width := lineThicknessIn;

                        //draw the polyline
                            canvasInOut.MoveTo( drawingPoints[0].X, drawingPoints[0].Y );
                            canvasInOut.Polyline( drawingPoints );
                    end;

                if (freePolyLineIn) then
                    FreeAndNil(polylineIn);
            end;

    //draw polygon
        procedure drawDirect2DPolygon(  const   polygonIn       : TGeomPolygon;
                                        const   fillColourIn,
                                                lineColourIn    : TAlphaColor;
                                        const   axisConverterIn : TDrawingAxisConverter;
                                        var     canvasInOut     : TDirect2DCanvas;
                                        const   freePolyLineIn  : boolean = True;
                                        const   lineThicknessIn : integer = 2           );
            var
                drawingPoints : TArray<TPoint>;
            begin
                if (polygonIn.vertexCount() > 1) then //only draw if there is more than one vertix
                    begin
                        //convert geometry into canvas drawing points
                            drawingPoints := axisConverterIn.arrXY_to_arrLT(
                                                                                polygonIn.getDrawingPoints()
                                                                           );

                        //assign the canvas the colour and line thickness
                            //fill
                                canvasInOut.Brush.Color := AlphaColorToColor(fillColourIn);
                                canvasInOut.Brush.Style := TBrushStyle.bsSolid;

                            //line
                                canvasInOut.Pen.Color := AlphaColorToColor(lineColourIn);
                                canvasInOut.Pen.Style := TPenStyle.psSolid;
                                canvasInOut.Pen.Width := lineThicknessIn;

                        //draw the polygon
                            canvasInOut.MoveTo( drawingPoints[0].X, drawingPoints[0].Y );
                            canvasInOut.Polygon( drawingPoints );
                    end;

                if (freePolyLineIn) then
                    FreeAndNil(polygonIn);
            end;

    //draw geometry
        procedure drawDirect2DGeometry( const drawingGeometryIn : TDrawingGeometry;
                                        const axisConverterIn   : TDrawingAxisConverter;
                                        var canvasInOut         : TDirect2DCanvas;
                                        const freeGeometryIn    : boolean = True        );
            var
                lineThickness           : integer;
                geometryType            : EGeomType;
                fillColour, lineColour  : TAlphaColor;
                geometry                : TGeomBase;
            begin
                lineThickness   := drawingGeometryIn.getLineThickness();
                fillColour      := drawingGeometryIn.getFillColour();
                lineColour      := drawingGeometryIn.getLineColour();
                geometry        := drawingGeometryIn.getGeometry();

                geometryType    := geometry.getGeomType();

                case (geometryType) of
                    EGeomType.gtLine:
                        drawDirect2DLine(   TGeomLine(geometry),
                                            lineColour,
                                            axisConverterIn,
                                            canvasInOut,
                                            False,
                                            lineThickness       );

                    EGeomType.gtPolyline:
                        drawDirect2DPolyline(   TGeomPolyLine(geometry),
                                                lineColour,
                                                axisConverterIn,
                                                canvasInOut,
                                                False,
                                                lineThickness           );

                    EGeomType.gtPolygon:
                        drawDirect2DPolygon(    TGeomPolygon(geometry),
                                                fillColour,
                                                lineColour,
                                                axisConverterIn,
                                                canvasInOut,
                                                False,
                                                lineThickness           );
                end;

                if (freeGeometryIn) then
                    FreeAndNil(drawingGeometryIn);
            end;


end.
