unit SkiaDrawingClass;

interface

    uses
        //Delphi
            system.SysUtils, system.types, system.UITypes,
            System.Skia, Vcl.Skia,
        //custom
            DrawingAxisConversionClass,
            GeometryTypes, GeometryDrawingTypes,
            GeometryBaseClass,
            GeomLineClass, GeomPolyLineClass, GeomPolygonClass,
            SkiaDrawingMethods;

    type
        TSkiaGeomDrawer = class
            private
                var
                    arrDrawingGeom      : TArray<TDrawingGeometry>;
                    skiaDrawingCanvas   : ISkCanvas;
                    axisConverter       : TDrawingAxisConverter;
                //helper methods
                    function drawingGeomCount() : integer;
                //accessors
                    //return geom array for bounding box
                        function getArrGeom() : TArray<TGeomBase>;
                //modifiers
                    //drawing canvas
                        procedure setDrawingCanvas(const canvasIn : ISkCanvas);
                    //axis converter
                        procedure setAxisConverter(const axisConverterIn : TDrawingAxisConverter);
                //drawing procedures
                    //line
                        procedure drawLine(const drawingLineIn : TDrawingGeometry);
                    //polyline
                        procedure drawPolyline(const drawingPolylineIn : TDrawingGeometry);
                    //polygon
                        procedure drawPolygon(const drawingPolygonIn : TDrawingGeometry);
                    //auto detect geom type
                        procedure drawGeometry(const drawingGeometryIn : TDrawingGeometry);
                //add geometry
                    procedure addGeometry(const drawingGeometryIn : TDrawingGeometry);
            public
                //constructor
                    constructor create();
                //destructor
                    destructor destroy(); override;
                //add drawing geometry
                    procedure addLine(  const lineIn            : TGeomLine;
                                        const lineThicknessIn   : integer = 2;
                                        const colourIn          : TAlphaColor = TAlphaColors.Black  );
                    procedure addPolyline(  const polylineIn        : TGeomPolyLine;
                                            const lineThicknessIn   : integer = 2;
                                            const colourIn          : TAlphaColor = TAlphaColors.Black  );
                    procedure addPolygon(   const polygonIn         : TGeomLine;
                                            const lineThicknessIn   : integer = 2;
                                            const fillColourIn      : TAlphaColor = TAlphaColors.Null;
                                            const lineColourIn      : TAlphaColor = TAlphaColors.Black  );
                //geometry net bounding box
                    function determineGeomBoundingBox() : TGeomBox;
                //draw all geometry
                    procedure drawAllGeometry(  const canvasIn          : ISkCanvas;
                                                const axisConverterIn   : TDrawingAxisConverter );
                //reset
                    procedure resetDrawingGeometry();
        end;

implementation

    //private
        //helper methods
            function TSkiaGeomDrawer.drawingGeomCount() : integer;
                begin
                    result := length( arrDrawingGeom );
                end;

        //accessors
            //return geom array for bounding box
                function TSkiaGeomDrawer.getArrGeom() : TArray<TGeomBase>;
                    var
                        i, arrLen   : integer;
                        arrGeomOut  : TArray<TGeomBase>;
                    begin
                        arrLen := drawingGeomCount();

                        SetLength( arrGeomOut, arrLen );

                        for i := 0 to (arrLen - 1) do
                            arrGeomOut[i] := arrDrawingGeom[i].geometry;

                        result := arrGeomOut;
                    end;

        //modifiers
            //set the drawing canvas
                procedure TSkiaGeomDrawer.setDrawingCanvas(const canvasIn : ISkCanvas);
                    begin
                        skiaDrawingCanvas := canvasIn;
                    end;

            //set the drawing  axis converter
                procedure TSkiaGeomDrawer.setAxisConverter(const axisConverterIn : TDrawingAxisConverter);
                    begin
                        axisConverter := axisConverterIn;
                    end;

        //drawing procedures
            //line
                procedure TSkiaGeomDrawer.drawLine(const drawingLineIn : TDrawingGeometry);
                    begin
                        drawSkiaLine(
                                        TGeomLine(drawingLineIn.geometry),
                                        drawingLineIn.lineColour,
                                        axisConverter,
                                        skiaDrawingCanvas,
                                        false,
                                        drawingLineIn.lineThickness
                                    );
                    end;

            //polyline
                procedure TSkiaGeomDrawer.drawPolyline(const drawingPolylineIn : TDrawingGeometry);
                    begin
                        drawSkiaPolyline(
                                            TGeomPolyLine(drawingPolylineIn.geometry),
                                            drawingPolylineIn.lineColour,
                                            axisConverter,
                                            skiaDrawingCanvas,
                                            false,
                                            drawingPolylineIn.lineThickness
                                        );

                    end;

            //polygon
                procedure TSkiaGeomDrawer.drawPolygon(const drawingPolygonIn : TDrawingGeometry);
                    begin
                        drawSkiaPolygon(
                                            TGeomPolygon(drawingPolygonIn.geometry),
                                            drawingPolygonIn.fillColour,
                                            drawingPolygonIn.lineColour,
                                            axisConverter,
                                            skiaDrawingCanvas,
                                            false,
                                            drawingPolygonIn.lineThickness
                                       );
                    end;

            //auto detect geom type
                procedure TSkiaGeomDrawer.drawGeometry(const drawingGeometryIn : TDrawingGeometry);
                    begin
                        case ( drawingGeometryIn.geometry.getGeomType() ) of
                            EGeomType.gtLine:
                                drawLine( drawingGeometryIn );

                            EGeomType.gtPolyline:
                                drawPolyline( drawingGeometryIn );

                            EGeomType.gtPolygon:
                                drawPolygon( drawingGeometryIn );
                        end;
                    end;

    //public
        //constructor
            constructor TSkiaGeomDrawer.create();
                begin
                    SetLength(arrDrawingGeom, 0);
                end;

        //destructor
            destructor TSkiaGeomDrawer.destroy();
                begin
                    resetDrawingGeometry();
                end;

        //add drawing geometry
            procedure TSkiaGeomDrawer.addLine(  const lineIn            : TGeomLine;
                                                const lineThicknessIn   : integer = 2;
                                                const colourIn          : TAlphaColor = TAlphaColors.Black  );
                var
                    newDrawingGeometry : TDrawingGeometry;
                begin
                    newDrawingGeometry := TDrawingGeometry.create(  lineThicknessIn,
                                                                    TAlphaColors.Null,
                                                                    colourIn,
                                                                    lineIn              );

                    addGeometry( newDrawingGeometry );
                end;

            procedure TSkiaGeomDrawer.addPolyline(  const polylineIn        : TGeomPolyLine;
                                                    const lineThicknessIn   : integer = 2;
                                                    const colourIn          : TAlphaColor = TAlphaColors.Black  );
                var
                    newDrawingGeometry : TDrawingGeometry;
                begin
                    newDrawingGeometry := TDrawingGeometry.create(  lineThicknessIn,
                                                                    TAlphaColors.Null,
                                                                    colourIn,
                                                                    polylineIn          );

                    addGeometry( newDrawingGeometry );
                end;

            procedure TSkiaGeomDrawer.addPolygon(   const polygonIn         : TGeomLine;
                                                    const lineThicknessIn   : integer = 2;
                                                    const fillColourIn      : TAlphaColor = TAlphaColors.Null;
                                                    const lineColourIn      : TAlphaColor = TAlphaColors.Black  );
                var
                    newDrawingGeometry : TDrawingGeometry;
                begin
                    newDrawingGeometry := TDrawingGeometry.create(  lineThicknessIn,
                                                                    fillColourIn,
                                                                    lineColourIn,
                                                                    polygonIn       );

                    addGeometry( newDrawingGeometry );
                end;

        //add geometry
            procedure TSkiaGeomDrawer.addGeometry(const drawingGeometryIn : TDrawingGeometry);
                var
                    geomCount : integer;
                begin
                    geomCount := drawingGeomCount();

                    SetLength(arrDrawingGeom, geomCount + 1);

                    arrDrawingGeom[geomCount] := drawingGeometryIn;
                end;

        //geometry net bounding box
            function TSkiaGeomDrawer.determineGeomBoundingBox() : TGeomBox;
                var
                    boundingBoxOut  : TGeomBox;
                    arrGeom         : TArray<TGeomBase>;
                begin
                    arrGeom := getArrGeom();

                    boundingBoxOut := determineBoundingBox( arrGeom );

                    result := boundingBoxOut;
                end;

        //draw all geometry
            procedure TSkiaGeomDrawer.drawAllGeometry(  const canvasIn          : ISkCanvas;
                                                        const axisConverterIn   : TDrawingAxisConverter );
                var
                    i : integer;
                begin
                    //set canvas and converter
                        setDrawingCanvas( canvasIn );
                        setAxisConverter( axisConverterIn );

                    //loop through and draw geometry objects
                        for i := 0 to (drawingGeomCount() - 1) do
                            drawGeometry( arrDrawingGeom[i] );
                end;

        //reset
            procedure TSkiaGeomDrawer.resetDrawingGeometry();
                var
                    i : integer;
                begin
                    for i := 0 to (drawingGeomCount() - 1) do
                        arrDrawingGeom[i].freeGeometry();

                    SetLength(arrDrawingGeom, 0);
                end;

end.
