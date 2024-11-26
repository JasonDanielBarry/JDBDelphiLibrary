unit GeomDrawerLayersClass;

interface

    uses
        //Delphi
            system.SysUtils, system.types, system.UIConsts, system.UITypes, system.Generics.Collections,
        //custom
            ColourMethods,
            DrawingAxisConversionClass,
            DrawingGeometryClass,
            GeometryTypes, GeomBox,
            GeometryBaseClass,
            GeomLineClass, GeomPolyLineClass, GeomPolygonClass
            ;

    type
        TGeomDrawer = class
            strict private
                type
                    TLayerGeometryPair  = TPair<string, TArray<TDrawingGeometry>>;
                    TLayerGeometryMap   = TDictionary<string, TArray<TDrawingGeometry>>;
                var
                    currentDrawingLayer : string;
                    orderedLayerKeys    : TList<string>;
                    arrActiveLayers     : TArray<string>;
                    layerGeometryMap    : TLayerGeometryMap;
                //helper methods
                    //return a specified layer's drawing geometry array
                        function getArrDrawingGeom(const layerKeyIn : string) : TArray<TDrawingGeometry>; inline;
                    //return a layer's geometry array for bounding box
                        function getArrGeom(const layerKeyIn : string) : TArray<TGeomBase>;
                    //add geometry to the geometry array
                        procedure addGeometry(const drawingGeometryIn : TDrawingGeometry);
                //calculate the geometry net bounding box for active geometry
                    procedure calculateGeomBoundingBox();
            strict protected
                var
                    drawingBackgroundColour : TColor;
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
                                        const colourIn          : TColor = TColors.Black );
                    procedure addPolyline(  const polylineIn        : TGeomPolyLine;
                                            const lineThicknessIn   : integer = 2;
                                            const colourIn          : TColor = TColors.Black );
                    procedure addPolygon(   const polygonIn         : TGeomPolygon;
                                            const lineThicknessIn   : integer = 2;
                                            const fillColourIn      : TColor = TColors.Null;
                                            const lineColourIn      : TColor = TColors.Black );
                //accessors
                    function getAllDrawingLayers() : TArray<string>;
                //modifiers
                    procedure setDrawingBackgroundColour(const colourIn : TColor);
                    procedure setCurrentDrawingLayer(const layerKeyIn : string);
                    procedure setActiveDrawingLayers(const arrActiveLayersIn : TArray<string>);

                //reset
                    procedure resetDrawingGeometry();
        end;

implementation

    //private
        //helper methods
            //return a drawing geometry array for a specified layer
                function TGeomDrawer.getArrDrawingGeom(const layerKeyIn : string) : TArray<TDrawingGeometry>;
                    var
                        arrDrawingGeomOut : TArray<TDrawingGeometry>;
                    begin
                        layerGeometryMap.TryGetValue( layerKeyIn, arrDrawingGeomOut );

                        result := arrDrawingGeomOut;
                    end;

            //return geometry array for bounding box
                function TGeomDrawer.getArrGeom(const layerKeyIn : string) : TArray<TGeomBase>;
                    var
                        i, arrLen       : integer;
                        arrGeomOut      : TArray<TGeomBase>;
                        arrDrawingGeom  : TArray<TDrawingGeometry>;
                    begin
                        arrDrawingGeom := getArrDrawingGeom( layerKeyIn );

                        arrLen := length( arrDrawingGeom );

                        SetLength( arrGeomOut, arrLen );

                        for i := 0 to (arrLen - 1) do
                            arrGeomOut[i] := arrDrawingGeom[i].getGeometry;

                        result := arrGeomOut;
                    end;

            //add geometry to the layer-geometry map
                procedure TGeomDrawer.addGeometry(const drawingGeometryIn : TDrawingGeometry);
                    var
                        geomCount       : integer;
                        arrDrawingGeom  : TArray<TDrawingGeometry>;
                    begin
                        //if the current layer is not set then set to default
                            if (currentDrawingLayer = '') then
                                setCurrentDrawingLayer( 'Default Layer' );

                        //get the drawing geometry array and add the new drawing geometry to it
                            layerGeometryMap.TryGetValue( currentDrawingLayer, arrDrawingGeom );

                            geomCount := length(arrDrawingGeom);

                            SetLength(arrDrawingGeom, geomCount + 1);

                            arrDrawingGeom[geomCount] := drawingGeometryIn;

                        //add the array back to the map
                            layerGeometryMap[ currentDrawingLayer ] := arrDrawingGeom;
                    end;

        //calculate the geometry net bounding box for active geometry
            procedure TGeomDrawer.calculateGeomBoundingBox();
                var
                    i, layerCount       : integer;
                    layer               : string;
                    geomBoundingBox     : TGeomBox;
                    arrBoundingBoxes    : TArray<TGeomBox>;
                    arrGeom             : TArray<TGeomBase>;
                begin
                    layerCount := length(arrActiveLayers);

                    SetLength( arrBoundingBoxes, layerCount );

                    i := 0;

                    for layer in arrActiveLayers do
                        begin
                            arrGeom := getArrGeom( layer );

                            arrBoundingBoxes[i] := TGeomBase.determineBoundingBox( arrGeom );

                            inc( i );
                        end;

                    geomBoundingBox := TGeomBox.determineBoundingBox( arrBoundingBoxes );

                    axisConverter.setGeometryBoundary( geomBoundingBox );
                end;

    //protected
        //drawing procedures
            //draw all geometry
                procedure TGeomDrawer.drawAllGeometry(const canvasHeightIn, canvasWidthIn : integer);
                    var
                        i                   : integer;
                        layer               : string;
                        arrDrawingGeometry  : TArray<TDrawingGeometry>;
                    begin
                        //set axis converter canvas dimensions
                            axisConverter.setCanvasDimensions( canvasHeightIn, canvasWidthIn );

                            axisConverter.setDrawingSpaceRatio( 1 );

                        //loop through active layers of the layer-geometry map
                            for layer in arrActiveLayers do
                                begin
                                    arrDrawingGeometry := getArrDrawingGeom( layer );

                                    for i := 0 to ( length(arrDrawingGeometry) - 1 ) do
                                        drawGeometry( arrDrawingGeometry[i] );
                                end;
                    end;

    //public
        //constructor
            constructor TGeomDrawer.create();
                begin
                    inherited create();

                    axisConverter := TDrawingAxisConverter.create();

                    orderedLayerKeys := TList<string>.Create();

                    layerGeometryMap := TLayerGeometryMap.Create();

                    currentDrawingLayer := '';
                end;

        //destructor
            destructor TGeomDrawer.destroy();
                begin
                    resetDrawingGeometry();

                    FreeAndNil( axisConverter );

                    FreeAndNil( orderedLayerKeys );

                    FreeAndNil( layerGeometryMap );

                    inherited destroy();
                end;

        //add drawing geometry
            procedure TGeomDrawer.addLine(  const lineIn            : TGeomLine;
                                            const lineThicknessIn   : integer = 2;
                                            const colourIn          : TColor = TColors.Black  );
                var
                    newDrawingGeometry : TDrawingGeometry;
                begin
                    newDrawingGeometry := TDrawingGeometry.create(  lineThicknessIn,
                                                                    TColors.Null,
                                                                    colourIn,
                                                                    lineIn          );

                    addGeometry( newDrawingGeometry );
                end;

            procedure TGeomDrawer.addPolyline(  const polylineIn        : TGeomPolyLine;
                                                const lineThicknessIn   : integer = 2;
                                                const colourIn          : TColor = TColors.Black  );
                var
                    newDrawingGeometry : TDrawingGeometry;
                begin
                    newDrawingGeometry := TDrawingGeometry.create(  lineThicknessIn,
                                                                    TColors.Null,
                                                                    colourIn,
                                                                    polylineIn      );

                    addGeometry( newDrawingGeometry );
                end;

            procedure TGeomDrawer.addPolygon(   const polygonIn         : TGeomPolygon;
                                                const lineThicknessIn   : integer = 2;
                                                const fillColourIn      : TColor = TColors.Null;
                                                const lineColourIn      : TColor = TColors.Black    );
                var
                    newDrawingGeometry : TDrawingGeometry;
                begin
                    newDrawingGeometry := TDrawingGeometry.create(  lineThicknessIn,
                                                                    fillColourIn,
                                                                    lineColourIn,
                                                                    polygonIn       );

                    addGeometry( newDrawingGeometry );
                end;

        //accessors
            function TGeomDrawer.getAllDrawingLayers() : TArray<string>;
                begin
                    result := orderedLayerKeys.ToArray();
                end;

        //modifiers
            procedure TGeomDrawer.setDrawingBackgroundColour(const colourIn : TColor);
                begin
                    drawingBackgroundColour := colourIn;
                end;

            procedure TGeomDrawer.setCurrentDrawingLayer(const layerKeyIn : string);
                var
                    layerExists         : boolean;
                    drawingGeomArray    : TArray<TDrawingGeometry>;
                begin
                    currentDrawingLayer := layerKeyIn;

                    //check to see if the key exists
                        layerExists := ( layerGeometryMap.TryGetValue( layerKeyIn, drawingGeomArray ) AND orderedLayerKeys.Contains(currentDrawingLayer) );

                        if (layerExists) then
                            exit();

                    //add the key to the keys list
                        orderedLayerKeys.Add( currentDrawingLayer );

                    //add a new array for the new key
                        SetLength(drawingGeomArray, 0);

                        layerGeometryMap.AddOrSetValue( layerKeyIn, drawingGeomArray );
                end;

            procedure TGeomDrawer.setActiveDrawingLayers(const arrActiveLayersIn : TArray<string>);
                begin
                    arrActiveLayers := arrActiveLayersIn;

                    calculateGeomBoundingBox();
                end;

        //reset drawing geometry by freeing all drawing geometry objects
            procedure TGeomDrawer.resetDrawingGeometry();
                var
                    layer               : string;
                    arrDrawingLayers    : TArray<string>;
                    arrDrawingGeom      : TArray<TDrawingGeometry>;
                procedure
                    _freeDrawingGeometry( var arrDrawingGeometryInOut : TArray<TDrawingGeometry> );
                        var
                            i : integer;
                        begin
                            for i := 0 to ( length(arrDrawingGeometryInOut) - 1 ) do
                                FreeAndNil( arrDrawingGeometryInOut[i] );
                        end;
                begin
                    arrDrawingLayers := getAllDrawingLayers();

                    for layer in arrDrawingLayers do
                        begin
                            arrDrawingGeom := getArrDrawingGeom( layer );

                            _freeDrawingGeometry( arrDrawingGeom );

                            layerGeometryMap.Remove( layer );
                        end;

                    orderedLayerKeys.Clear();

                    SetLength(arrActiveLayers, 0);

                    currentDrawingLayer := '';
                end;

end.
