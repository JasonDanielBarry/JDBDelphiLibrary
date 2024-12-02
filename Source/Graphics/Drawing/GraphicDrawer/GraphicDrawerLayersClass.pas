unit GraphicDrawerLayersClass;

interface

    uses
        //Delphi
            system.SysUtils, system.types, system.UIConsts, system.UITypes, system.Generics.Collections,
        //custom
            ColourMethods,
            DrawingAxisConversionClass,
            GraphicObjectBaseClass,
            GraphicGeometryClass,
            GeometryTypes, GeomBox,
            GraphicDrawerBaseClass
            ;

    type
        TGraphicDrawerLayers = class(TGraphicDrawer)
            strict private
                type
                    TLayerGeometryPair  = TPair<string, TArray<TGraphicObject>>;
                    TLayerGeometryMap   = TDictionary<string, TArray<TGraphicObject>>;
                var
                    currentDrawingLayer : string;
                    orderedLayerKeys    : TList<string>;
                    arrActiveLayers     : TArray<string>;
                    layerGeometryMap    : TLayerGeometryMap;
                //add graphic drawing object to the drawing object container
                    procedure addGraphicObject(const drawingGeometryIn : TGraphicObject); override;
                //helper methods
                    //return a specified layer's drawing geometry array
                        function getArrGraphicGeom(const layerKeyIn : string) : TArray<TGraphicObject>;
                    //calculate the bounding box for a specific layer
                        function calculateLayerBoundingBox(const layerKeyIn : string) : TGeomBox;
                    //calculate the net bounding box for active layers
                        procedure calculateNetBoundingBox();
            strict protected
                //draw all geometry
                    procedure drawAllGeometry(  const canvasWidthIn, canvasHeightIn : integer;
                                                const drawingBackgroundColourIn     : TColor    ); override;
            public
                //constructor
                    constructor create(); virtual;
                //destructor
                    destructor destroy(); override;
                //accessors
                    function getAllDrawingLayers() : TArray<string>;
                //modifiers
                    procedure setCurrentDrawingLayer(const layerKeyIn : string); override;
                    procedure setActiveDrawingLayers(const arrActiveLayersIn : TArray<string>);
                //reset
                    procedure resetDrawingGeometry();
        end;

implementation

    //private
        //helper methods
            //return a drawing geometry array for a specified layer
                function TGraphicDrawerLayers.getArrGraphicGeom(const layerKeyIn : string) : TArray<TGraphicObject>;
                    var
                        arrDrawingGeomOut : TArray<TGraphicObject>;
                    begin
                        layerGeometryMap.TryGetValue( layerKeyIn, arrDrawingGeomOut );

                        result := arrDrawingGeomOut;
                    end;

            //add geometry to the layer-geometry map
                procedure TGraphicDrawerLayers.addGraphicObject(const drawingGeometryIn : TGraphicObject);
                    var
                        graphicObjectCount  : integer;
                        arrGraphicObjects   : TArray<TGraphicObject>;
                    begin
                        //if the current layer is not set then set to default
                            if (currentDrawingLayer = '') then
                                setCurrentDrawingLayer( 'Default Layer' );

                        //get the drawing geometry array and add the new drawing geometry to it
                            layerGeometryMap.TryGetValue( currentDrawingLayer, arrGraphicObjects );

                            graphicObjectCount := length( arrGraphicObjects );

                            SetLength(arrGraphicObjects, graphicObjectCount + 1);

                            arrGraphicObjects[ graphicObjectCount ] := drawingGeometryIn;

                        //add the array back to the map
                            layerGeometryMap.AddOrSetValue( currentDrawingLayer, arrGraphicObjects );
                    end;

            //calculate the bounding box for a specific layer
                function TGraphicDrawerLayers.calculateLayerBoundingBox(const layerKeyIn : string) : TGeomBox;
                    var
                        i, graphicObjectCount   : integer;
                        arrBoundingBoxes        : TArray<TGeomBox>;
                        arrGraphicObjects       : TArray<TGraphicObject>;
                    begin
                        arrGraphicObjects := getArrGraphicGeom( layerKeyIn );

                        graphicObjectCount := length( arrGraphicObjects );

                        SetLength( arrBoundingBoxes, graphicObjectCount );

                        for i := 0 to (graphicObjectCount - 1) do
                            arrBoundingBoxes[i] := arrGraphicObjects[i].determineBoundingBox();

                        result := TGeomBox.determineBoundingBox( arrBoundingBoxes );
                    end;

            //calculate the geometry net bounding box for active geometry
                procedure TGraphicDrawerLayers.calculateNetBoundingBox();
                    var
                        i, activeLayerCount : integer;
                        layerKey            : string;
                        geomBoundingBox     : TGeomBox;
                        arrBoundingBoxes    : TArray<TGeomBox>;
                        arrGraphicGeom      : TArray<TGraphicObject>;
                    begin
                        activeLayerCount := length(arrActiveLayers);

                        SetLength( arrBoundingBoxes, activeLayerCount );

                        i := 0;

                        for layerKey in arrActiveLayers do
                            begin
                                arrBoundingBoxes[i] := calculateLayerBoundingBox( layerKey );

                                inc( i );
                            end;

                        geomBoundingBox := TGeomBox.determineBoundingBox( arrBoundingBoxes );

                        axisConverter.setGeometryBoundary( geomBoundingBox );
                    end;

    //protected
        //drawing procedures
            //draw all geometry
                procedure TGraphicDrawerLayers.drawAllGeometry( const canvasWidthIn, canvasHeightIn : integer;
                                                                const drawingBackgroundColourIn     : TColor    );
                    var
                        i                   : integer;
                        layer               : string;
                        arrDrawingGeometry  : TArray<TGraphicObject>;
                    begin
                        inherited drawAllGeometry(  canvasWidthIn, canvasHeightIn,
                                                    drawingBackgroundColourIn       );

                        //loop through active layers of the layer-geometry map
                            for layer in arrActiveLayers do
                                begin
                                    arrDrawingGeometry := getArrGraphicGeom( layer );

                                    for i := 0 to ( length(arrDrawingGeometry) - 1 ) do
                                        arrDrawingGeometry[i].drawToCanvas( axisConverter, Direct2DDrawingCanvas );
                                end;
                    end;

    //public
        //constructor
            constructor TGraphicDrawerLayers.create();
                begin
                    inherited create();

                    orderedLayerKeys := TList<string>.Create();

                    layerGeometryMap := TLayerGeometryMap.Create();

                    currentDrawingLayer := '';
                end;

        //destructor
            destructor TGraphicDrawerLayers.destroy();
                begin
                    resetDrawingGeometry();

                    FreeAndNil( orderedLayerKeys );

                    FreeAndNil( layerGeometryMap );

                    inherited destroy();
                end;

        //accessors
            function TGraphicDrawerLayers.getAllDrawingLayers() : TArray<string>;
                begin
                    result := orderedLayerKeys.ToArray();
                end;

        //modifiers
            procedure TGraphicDrawerLayers.setCurrentDrawingLayer(const layerKeyIn : string);
                var
                    layerExists         : boolean;
                    drawingGeomArray    : TArray<TGraphicObject>;
                begin
                    currentDrawingLayer := layerKeyIn;

                    //check to see if the key exists
                        layerExists := ( layerGeometryMap.TryGetValue( layerKeyIn, drawingGeomArray ) AND orderedLayerKeys.Contains( currentDrawingLayer ) );

                        if (layerExists) then
                            exit();

                    //add the key to the keys list
                        orderedLayerKeys.Add( currentDrawingLayer );

                    //add a new array for the new key
                        SetLength(drawingGeomArray, 0);

                        layerGeometryMap.AddOrSetValue( currentDrawingLayer, drawingGeomArray );
                end;

            procedure TGraphicDrawerLayers.setActiveDrawingLayers(const arrActiveLayersIn : TArray<string>);
                begin
                    arrActiveLayers := arrActiveLayersIn;

                    calculateNetBoundingBox();
                end;

        //reset drawing geometry by freeing all drawing geometry objects
            procedure TGraphicDrawerLayers.resetDrawingGeometry();
                var
                    layer               : string;
                    arrDrawingLayers    : TArray<string>;
                    arrDrawingGeom      : TArray<TGraphicObject>;
                procedure
                    _freeDrawingGeometry( var arrDrawingGeometryInOut : TArray<TGraphicObject> );
                        var
                            i : integer;
                        begin
                            for i := 0 to ( length(arrDrawingGeometryInOut) - 1 ) do
                                try
                                    FreeAndNil( arrDrawingGeometryInOut[i] );
                                except

                                end;

                            SetLength( arrDrawingGeometryInOut, 0 );
                        end;
                begin
                    arrDrawingLayers := getAllDrawingLayers();

                    for layer in arrDrawingLayers do
                        begin
                            arrDrawingGeom := getArrGraphicGeom( layer );

                            _freeDrawingGeometry( arrDrawingGeom );

                            layerGeometryMap.Remove( layer );
                        end;

                    orderedLayerKeys.Clear();

                    SetLength(arrActiveLayers, 0);

                    currentDrawingLayer := '';
                end;

end.
