unit SkiaDrawingMethods;

interface

    uses
        //Delphi
            system.SysUtils, system.types, system.UITypes, system.UIConsts,
            System.Skia, Vcl.Skia,
        //custom
            DrawingAxisConversionClass,
            DrawingGeometryClass,
            GeometryTypes,
            GeometryBaseClass, GeomLineClass, GeomPolyLineClass, GeomPolygonClass;

    //draw line
        procedure drawSkiaLine( const lineIn            : TGeomLine;
                                const colourIn          : TAlphaColor;
                                const axisConverterIn   : TDrawingAxisConverter;
                                var canvasInOut         : ISkCanvas;
                                const freeLineIn        : boolean = True;
                                const lineThicknessIn   : integer = 2           );

    //draw polyline
        procedure drawSkiaPolyline( const polylineIn        : TGeomPolyLine;
                                    const colourIn          : TAlphaColor;
                                    const axisConverterIn   : TDrawingAxisConverter;
                                    var canvasInOut         : ISkCanvas;
                                    const freePolyLineIn    : boolean = True;
                                    const lineThicknessIn   : integer = 2           );

    //draw polygon
        procedure drawSkiaPolygon(  const   polygonIn       : TGeomPolygon;
                                    const   fillColourIn,
                                            lineColourIn    : TAlphaColor;
                                    const   axisConverterIn : TDrawingAxisConverter;
                                    var     canvasInOut     : ISkCanvas;
                                    const   freePolyLineIn  : boolean = True;
                                    const   lineThicknessIn : integer = 2           );

    //draw geometry
        procedure drawSkiaGeometry( const drawingGeometryIn : TDrawingGeometry;
                                    const axisConverterIn   : TDrawingAxisConverter;
                                    var canvasInOut         : ISkCanvas;
                                    const freeGeometryIn    : boolean = True        );

implementation

    //draw line
        procedure drawSkiaLine( const lineIn            : TGeomLine;
                                const colourIn          : TAlphaColor;
                                const axisConverterIn   : TDrawingAxisConverter;
                                var canvasInOut         : ISkCanvas;
                                const freeLineIn        : boolean = True;
                                const lineThicknessIn   : integer = 2           );
            var
                drawingPoints   : TArray<TPointF>;
                pathbuilder     : ISkPathBuilder;
                path            : ISkPath;
                paint           : ISkPaint;
            begin
                //convert geometry into canvas drawing points
                    drawingPoints := axisConverterIn.arrXY_to_arrLT(
                                                                        lineIn.getDrawingPoints()
                                                                   );

                //define the drawing path
                    pathbuilder := TSkPathBuilder.Create();

                    pathbuilder.MoveTo(drawingPoints[0]);
                    pathbuilder.LineTo(drawingPoints[1]);

                    path := pathbuilder.Detach();

                //draw the shape
                    paint := TSkPaint.create();
                    paint.AntiAlias := True;

                    paint.Style         := TSkPaintStyle.Stroke;
                    paint.Color         := colourIn;
                    paint.StrokeWidth   := lineThicknessIn;

                    canvasInOut.DrawPath(path, paint);

                if (freeLineIn) then
                    FreeAndNil(lineIn);
            end;

    //draw polyline
        procedure drawSkiaPolyline( const polylineIn        : TGeomPolyLine;
                                    const colourIn          : TAlphaColor;
                                    const axisConverterIn   : TDrawingAxisConverter;
                                    var canvasInOut         : ISkCanvas;
                                    const freePolyLineIn    : boolean = True;
                                    const lineThicknessIn   : integer = 2           );
            var
                drawingPoints   : TArray<TPointF>;
                pathbuilder     : ISkPathBuilder;
                path            : ISkPath;
                paint           : ISkPaint;
            begin
                if (polylineIn.vertexCount() > 1) then //only draw if there is more than one vertix
                    begin
                        //convert geometry into canvas drawing points
                            drawingPoints := axisConverterIn.arrXY_to_arrLT(
                                                                                polylineIn.getDrawingPoints()
                                                                           );

                        //define the drawing path
                            pathbuilder := TSkPathBuilder.Create();

                            pathbuilder.MoveTo(drawingPoints[0]);

                            pathbuilder.PolylineTo(drawingPoints);

                            path := pathbuilder.Detach();

                        //draw the shape
                            paint := TSkPaint.create();
                            paint.AntiAlias := True;

                            //line
                                paint.Style         := TSkPaintStyle.Stroke;
                                paint.Color         := colourIn;
                                paint.StrokeWidth   := lineThicknessIn;
                                canvasInOut.DrawPath(path, paint);
                    end;

                if (freePolyLineIn) then
                    FreeAndNil(polylineIn);
            end;

    //draw polyline
        procedure drawSkiaPolygon(  const   polygonIn       : TGeomPolygon;
                                    const   fillColourIn,
                                            lineColourIn    : TAlphaColor;
                                    const   axisConverterIn : TDrawingAxisConverter;
                                    var     canvasInOut     : ISkCanvas;
                                    const   freePolyLineIn  : boolean = True;
                                    const   lineThicknessIn : integer = 2           );
            var
                drawingPoints   : TArray<TPointF>;
                pathbuilder     : ISkPathBuilder;
                path            : ISkPath;
                paint           : ISkPaint;
            begin
                if (polygonIn.vertexCount() > 1) then //only draw if there is more than one vertix
                    begin
                        //convert geometry into canvas drawing points
                            drawingPoints := axisConverterIn.arrXY_to_arrLT(
                                                                                polygonIn.getDrawingPoints()
                                                                           );

                        //define the drawing path
                            pathbuilder := TSkPathBuilder.Create();

                            pathbuilder.MoveTo(drawingPoints[0]);

                            pathbuilder.PolylineTo(drawingPoints);

                            pathbuilder.LineTo(drawingPoints[1]); //with skia this is necessary to ensure final corner is closed smoothly

                            path := pathbuilder.Detach();

                        //draw the shape
                            paint := TSkPaint.create();
                            paint.AntiAlias := True;

                            //line
                                paint.Style         := TSkPaintStyle.Stroke;
                                paint.Color         := lineColourIn;
                                paint.StrokeWidth   := lineThicknessIn;
                                canvasInOut.DrawPath(path, paint);

                            //fill
                                paint.Style         := TSkPaintStyle.Fill;
                                paint.Color         := fillColourIn;
                                canvasInOut.DrawPath(path, paint);
                    end;

                if (freePolyLineIn) then
                    FreeAndNil(polygonIn);
            end;

    //draw geometry
        procedure drawSkiaGeometry( const drawingGeometryIn : TDrawingGeometry;
                                    const axisConverterIn   : TDrawingAxisConverter;
                                    var canvasInOut         : ISkCanvas;
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
                        drawSkiaLine(   TGeomLine(geometry),
                                        lineColour,
                                        axisConverterIn,
                                        canvasInOut,
                                        False,
                                        lineThickness       );

                    EGeomType.gtPolyline:
                        drawSkiaPolyline(   TGeomPolyLine(geometry),
                                            lineColour,
                                            axisConverterIn,
                                            canvasInOut,
                                            False,
                                            lineThickness           );

                    EGeomType.gtPolygon:
                        drawSkiaPolygon(    TGeomPolygon(geometry),
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
