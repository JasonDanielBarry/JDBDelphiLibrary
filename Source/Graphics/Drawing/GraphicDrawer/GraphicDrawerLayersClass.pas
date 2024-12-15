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
            GraphicDrawerObjectAdderClass
            ;

    type
        TGraphicDrawerLayers = class(TGraphicDrawerObjectAdder)
            strict private
                type
                    TLayerGeometryPair  = TPair<string, TArray<TGraphicObject>>;
                    TLayerGeometryMap   = TDictionary<string, TArray<TGraphicObject>>;
                var
                    currentDrawingLayer     : string;
                    orderedLayerKeys        : TList<string>;
                    arrActiveDrawingLayers  : TArray<string>;
                    layerGeometryMap        : TLayerGeometryMap;
                //add graphic drawing object to the drawing object container
                    procedure addGraphicObject(const drawingGeometryIn : TGraphicObject); override;
                //helper methods
                    //return a specified layer's graphic objects array
                        function getArrGraphicObjects(const layerKeyIn : string) : TArray<TGraphicObject>;
                //boudning box
                    //calculate the bounding box for a specific layer
                        function calculateLayerBoundingBox(const layerKeyIn : string) : TGeomBox;
                    //calculate the net bounding box for active layers
                        procedure calculateNetBoundingBox();
            strict protected
                //draw all geometry
                    procedure drawAll(  const canvasWidthIn, canvasHeightIn : integer;
                                        const drawingBackgroundColourIn     : TColor    ); override;
            public
                //constructor
                    constructor create(); override;
                //destructor
                    destructor destroy(); override;
                //accessors
                    function getAllDrawingLayers() : TArray<string>;
                //modifiers
                    procedure setCurrentDrawingLayer(const layerKeyIn : string); override;
                    procedure setActiveDrawingLayers(const arrActiveDrawingLayersIn : TArray<string>);
                //reset
                    procedure resetDrawingGeometry();
        end;

implementation

    //private
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
                        arrGraphicObjects := getArrGraphicObjects( currentDrawingLayer );

                        graphicObjectCount := length( arrGraphicObjects );

                        SetLength(arrGraphicObjects, graphicObjectCount + 1);

                        arrGraphicObjects[ graphicObjectCount ] := drawingGeometryIn;

                    //add the array back to the map
                        layerGeometryMap.AddOrSetValue( currentDrawingLayer, arrGraphicObjects );
                end;

        //helper methods
            //return a drawing geometry array for a specified layer
                function TGraphicDrawerLayers.getArrGraphicObjects(const layerKeyIn : string) : TArray<TGraphicObject>;
                    var
                        arrGraphicObjectsOut : TArray<TGraphicObject>;
                    begin
                        layerGeometryMap.TryGetValue( layerKeyIn, arrGraphicObjectsOut );

                        result := arrGraphicObjectsOut;
                    end;

        //boudning box
            //calculate the bounding box for a specific layer
                function TGraphicDrawerLayers.calculateLayerBoundingBox(const layerKeyIn : string) : TGeomBox;
                    var
                        i, graphicObjectCount   : integer;
                        arrBoundingBoxes        : TArray<TGeomBox>;
                        arrGraphicObjects       : TArray<TGraphicObject>;
                    begin
                        //get graphic objects array for specified layer
                            arrGraphicObjects := getArrGraphicObjects( layerKeyIn );

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
                        netBoundingBox      : TGeomBox;
                        arrBoundingBoxes    : TArray<TGeomBox>;
                        arrGraphicGeom      : TArray<TGraphicObject>;
                    begin
                        activeLayerCount := length( arrActiveDrawingLayers );

                        SetLength( arrBoundingBoxes, activeLayerCount );

                        i := 0;

                        for layerKey in arrActiveDrawingLayers do
                            begin
                                arrBoundingBoxes[i] := calculateLayerBoundingBox( layerKey );

                                inc( i );
                            end;

                        netBoundingBox := TGeomBox.determineBoundingBox( arrBoundingBoxes );

                        axisConverter.setGeometryBoundary( netBoundingBox );
                    end;

    //protected
        //drawing procedures
            //draw all geometry
                procedure TGraphicDrawerLayers.drawAll( const canvasWidthIn, canvasHeightIn : integer;
                                                        const drawingBackgroundColourIn     : TColor    );
                    var
                        i                   : integer;
                        layer               : string;
                        arrDrawingGeometry  : TArray<TGraphicObject>;
                    begin
                        inherited drawAll(  canvasWidthIn, canvasHeightIn,
                                            drawingBackgroundColourIn       );

                        //only draw if axis converter has valid dimensions
                            if ( NOT(axisConverter.isValid()) ) then
                                exit();

                        //loop through active layers of the layer-geometry map
                            for layer in arrActiveDrawingLayers do
                                begin
                                    arrDrawingGeometry := getArrGraphicObjects( layer );

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
                    existsInList,
                    existsInMap,
                    layerKeyExists      : boolean;
                    drawingGeomArray    : TArray<TGraphicObject>;
                begin
                    currentDrawingLayer := layerKeyIn;

                    //check to see if the layer key exists
                        existsInMap     := layerGeometryMap.TryGetValue( layerKeyIn, drawingGeomArray );
                        existsInList    := orderedLayerKeys.Contains( currentDrawingLayer );

                        layerKeyExists := existsInMap AND existsInList ;

                        if (layerKeyExists) then
                            exit();

                    //add the key to the keys list
                        orderedLayerKeys.Add( currentDrawingLayer );

                    //add a new array for the new key
                        SetLength( drawingGeomArray, 0 );

                        layerGeometryMap.AddOrSetValue( currentDrawingLayer, drawingGeomArray );
                end;

            procedure TGraphicDrawerLayers.setActiveDrawingLayers(const arrActiveDrawingLayersIn : TArray<string>);
                begin
                    arrActiveDrawingLayers := arrActiveDrawingLayersIn;

                    calculateNetBoundingBox();
                end;

        //reset drawing geometry by freeing all drawing geometry objects
            procedure TGraphicDrawerLayers.resetDrawingGeometry();
                var
                    layer               : string;
                    arrDrawingLayers    : TArray<string>;
                procedure
                    _freeArrGraphicObjects(const layerKeyIn : string);
                        var
                            i                   : integer;
                            arrGraphicObjects   : TArray<TGraphicObject>;
                        begin
                            arrGraphicObjects := getArrGraphicObjects( layer );

                            for i := 0 to ( length(arrGraphicObjects) - 1 ) do
                                FreeAndNil( arrGraphicObjects[i] );

                            SetLength( arrGraphicObjects, 0 );
                        end;
                begin
                    arrDrawingLayers := getAllDrawingLayers();

                    for layer in arrDrawingLayers do
                        begin
                            _freeArrGraphicObjects( layer );

                            layerGeometryMap.Remove( layer );
                        end;

                    orderedLayerKeys.Clear();

                    SetLength( arrActiveDrawingLayers, 0 );

                    currentDrawingLayer := '';
                end;

end.
