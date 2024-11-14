unit GeomDrawerBaseClass;

interface

    uses
        //Delphi
            system.SysUtils, system.types, system.UITypes,
        //custom
            DrawingAxisConversionClass,
            DrawingGeometryClass,
            GeometryTypes, GeomBox,
            GeometryBaseClass,
            GeomLineClass, GeomPolyLineClass, GeomPolygonClass
            ;

    type
        TGeomDrawerBase = class
            strict private
                var
                    arrDrawingGeom : TArray<TDrawingGeometry>;
                //helper methods
                    //return geometry array for bounding box
                        function getArrGeom() : TArray<TGeomBase>;
                    //count geometry objects
                        function getDrawingGeomCount() : integer;
                    //add geometry to the geometry array
                        procedure addGeometry(const drawingGeometryIn : TDrawingGeometry);
            strict protected
                var
                    drawingBackgroundColour : TAlphaColor;
                    axisConverter           : TDrawingAxisConverter;
                //drawing procedures
                    //draw a drawing geometry object
                        procedure drawGeometry(const drawingGeometryIn : TDrawingGeometry); virtual; abstract;
                    //draw all geometry
                        procedure drawAllGeometry(const canvasHeightIn, canvasWidthIn : integer);
            public
                //constructor
                    constructor create(); virtual;
                //destructor
                    destructor destroy(); override;
                //add drawing geometry
                    procedure addLine(  const lineIn            : TGeomLine;
                                        const lineThicknessIn   : integer = 2;
                                        const colourIn          : TAlphaColor = TAlphaColors.Black  );
                    procedure addPolyline(  const polylineIn        : TGeomPolyLine;
                                            const lineThicknessIn   : integer = 2;
                                            const colourIn          : TAlphaColor = TAlphaColors.Black  );
                    procedure addPolygon(   const polygonIn         : TGeomPolygon;
                                            const lineThicknessIn   : integer = 2;
                                            const fillColourIn      : TAlphaColor = TAlphaColors.Null;
                                            const lineColourIn      : TAlphaColor = TAlphaColors.Black  );
                //modifiers
                    procedure setDrawingBackgroundColour(const colourIn : TAlphaColor);
                //geometry net bounding box
                    function determineGeomBoundingBox() : TGeomBox;
                //reset
                    procedure resetDrawingGeometry();
        end;

implementation

    //private
        //helper methods
            //return geometry array for bounding box
                function TGeomDrawerBase.getArrGeom() : TArray<TGeomBase>;
                    var
                        i, arrLen   : integer;
                        arrGeomOut  : TArray<TGeomBase>;
                    begin
                        arrLen := getDrawingGeomCount();

                        SetLength( arrGeomOut, arrLen );

                        for i := 0 to (arrLen - 1) do
                            arrGeomOut[i] := arrDrawingGeom[i].getGeometry();

                        result := arrGeomOut;
                    end;

            //count geometry objects
                function TGeomDrawerBase.getDrawingGeomCount() : integer;
                    begin
                        result := length( arrDrawingGeom );
                    end;

            //add geometry to the geometry array
                procedure TGeomDrawerBase.addGeometry(const drawingGeometryIn : TDrawingGeometry);
                    var
                        geomCount : integer;
                    begin
                        geomCount := getDrawingGeomCount();

                        SetLength(arrDrawingGeom, geomCount + 1);

                        arrDrawingGeom[geomCount] := drawingGeometryIn;
                    end;

    //protected
        //accessors


        //drawing procedures
            //draw all geometry
                procedure TGeomDrawerBase.drawAllGeometry(const canvasHeightIn, canvasWidthIn : integer);
                    var
                        i : integer;
                    begin
                        //set axis converter canvas dimensions
                            axisConverter.setCanvasDimensions( canvasHeightIn, canvasWidthIn );

                            axisConverter.setGeometryBoundary( self.determineGeomBoundingBox() );

                            axisConverter.setDrawingSpaceRatio( 1 );

                        //loop through and draw geometry objects
                            for i := 0 to (getDrawingGeomCount() - 1) do
                                drawGeometry( arrDrawingGeom[i] );
                    end;

    //public
        //constructor
            constructor TGeomDrawerBase.create();
                begin
                    inherited create();

                    SetLength(arrDrawingGeom, 0);

                    axisConverter := TDrawingAxisConverter.create();
                end;

        //destructor
            destructor TGeomDrawerBase.destroy();
                begin
                    resetDrawingGeometry();

                    FreeAndNil( axisConverter );

                    inherited destroy();
                end;

        //add drawing geometry
            procedure TGeomDrawerBase.addLine(  const lineIn            : TGeomLine;
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

            procedure TGeomDrawerBase.addPolyline(  const polylineIn        : TGeomPolyLine;
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

            procedure TGeomDrawerBase.addPolygon(   const polygonIn         : TGeomPolygon;
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

        //modifiers
            procedure TGeomDrawerBase.setDrawingBackgroundColour(const colourIn : TAlphaColor);
                begin
                    drawingBackgroundColour := colourIn;
                end;

        //geometry net bounding box
            function TGeomDrawerBase.determineGeomBoundingBox() : TGeomBox;
                var
                    boundingBoxOut  : TGeomBox;
                    arrGeom         : TArray<TGeomBase>;
                begin
                    arrGeom := getArrGeom();

                    boundingBoxOut := TGeomBase.determineBoundingBox( arrGeom );

                    result := boundingBoxOut;
                end;

        //reset drawing geometry by freeing all drawing geometry objects
            procedure TGeomDrawerBase.resetDrawingGeometry();
                var
                    i : integer;
                begin
                    for i := 0 to (getDrawingGeomCount() - 1) do
                        FreeAndNil( arrDrawingGeom[i] );

                    SetLength(arrDrawingGeom, 0);
                end;



end.
