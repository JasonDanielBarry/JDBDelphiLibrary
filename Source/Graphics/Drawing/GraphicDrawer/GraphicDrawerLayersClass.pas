unit GraphicDrawerLayersClass;

interface

    uses
        //Delphi
            system.SysUtils, system.types, system.UIConsts, system.UITypes, system.Generics.Collections,
        //custom
            ColourMethods,
            DrawingAxisConversionClass,
            GraphicObjectBaseClass, GraphicObjectGroupClass,
            GeometryTypes, GeomBox,
            GraphicDrawerObjectAdderClass
            ;

    type
        TGraphicDrawerLayers = class(TGraphicDrawerObjectAdder)
            private
                type
                    TLayerGeometryMap = TDictionary<string, TArray<TGraphicObject>>;
                var
                    currentDrawingLayer     : string;
                    orderedLayerKeys        : TList<string>;
                    arrActiveDrawingLayers  : TArray<string>;
                    activeGraphicObjects    : TGraphicObjectGroup;
                    layerGeometryMap        : TLayerGeometryMap;
                //add graphic drawing object to the drawing object container
                    procedure addGraphicObject(const drawingGeometryIn : TGraphicObject); override;
                //helper methods
                    //return a specified layer's graphic objects array
                        function getArrGraphicObjects(const layerKeyIn : string) : TArray<TGraphicObject>;
                    //collect all active graphic objects into one array in order
                        procedure collectAllActiveGraphicObjects();
                //bounding box
                    //calculate the bounding box for a specific layer
                        function calculateLayerBoundingBox(const layerKeyIn : string) : TGeomBox;
                    //calculate the net bounding box for active layers
                        procedure calculateNetBoundingBox();
            protected
                //draw all geometry
                    procedure drawAll(  const canvasWidthIn, canvasHeightIn : integer;
                                        const drawingBackgroundColourIn     : TColor    );
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
                    procedure activateAllDrawingLayers();
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

                        SetLength( arrGraphicObjects, graphicObjectCount + 1 );

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

            //collect all active graphic objects into one array in order
                procedure TGraphicDrawerLayers.collectAllActiveGraphicObjects();
                    var
                        layer                           : string;
                        arrCurrentLayerGraphicObjects   : TArray<TGraphicObject>;
                    begin
                        //clear the active group - DO NOT FREE THE OBJECTS
                            activeGraphicObjects.clearGraphicObjectsGroup( False );

                        //loop through the active layers and place their graphic objects in the group
                            for layer in arrActiveDrawingLayers do
                                begin
                                    //get the graphic objects for the specified layer
                                        arrCurrentLayerGraphicObjects := getArrGraphicObjects( layer );

                                    //add the graphic objects to the active graphic objects group
                                        activeGraphicObjects.addGraphicObjectsToGroup( arrCurrentLayerGraphicObjects );
                                end;
                    end;

        //bounding box
            //calculate the bounding box for a specific layer
                function TGraphicDrawerLayers.calculateLayerBoundingBox(const layerKeyIn : string) : TGeomBox;
                    var
                        arrGraphicObjects : TArray<TGraphicObject>;
                    begin
                        //get graphic objects array for specified layer
                            arrGraphicObjects := getArrGraphicObjects( layerKeyIn );

                        result := TGraphicObject.determineBoundingBox( arrGraphicObjects );
                    end;

            //calculate the geometry net bounding box for active geometry
                procedure TGraphicDrawerLayers.calculateNetBoundingBox();
                    var
                        netBoundingBox : TGeomBox;
                    begin
                        netBoundingBox := activeGraphicObjects.determineBoundingBox();

                        axisConverter.setGeometryBoundary( netBoundingBox );
                    end;

    //protected
        //drawing procedures
            //draw all geometry
                procedure TGraphicDrawerLayers.drawAll( const canvasWidthIn, canvasHeightIn : integer;
                                                        const drawingBackgroundColourIn     : TColor    );
                    begin
                        inherited drawAll(  canvasWidthIn, canvasHeightIn,
                                            drawingBackgroundColourIn       );

                        activeGraphicObjects.drawToCanvas( axisConverter, Direct2DDrawingCanvas );
                    end;

    //public
        //constructor
            constructor TGraphicDrawerLayers.create();
                begin
                    inherited create();

                    orderedLayerKeys        := TList<string>.Create();
                    layerGeometryMap        := TLayerGeometryMap.Create();
                    activeGraphicObjects    := TGraphicObjectGroup.create();
                    currentDrawingLayer     := '';
                end;

        //destructor
            destructor TGraphicDrawerLayers.destroy();
                begin
                    resetDrawingGeometry();

                    FreeAndNil( orderedLayerKeys );
                    FreeAndNil( layerGeometryMap );

                    //the graphic object group is cleared (without freeing objects) before freeing
                    //as all graphic objects are freed in resetDrawingGeometry()
                        activeGraphicObjects.clearGraphicObjectsGroup( False );
                        FreeAndNil( activeGraphicObjects );

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
                    //set active layers array
                        arrActiveDrawingLayers := arrActiveDrawingLayersIn;

                    //place all the active graphic object into a single array for drawing
                        collectAllActiveGraphicObjects();

                    //calculate the bounding box for the reset zoom function
                        calculateNetBoundingBox();
                end;

            procedure TGraphicDrawerLayers.activateAllDrawingLayers();
                var
                    arrDrawingLayers : TArray<string>;
                begin
                    arrDrawingLayers := getAllDrawingLayers();

                    setActiveDrawingLayers( arrDrawingLayers );
                end;

        //reset drawing geometry by freeing all drawing geometry objects
            procedure TGraphicDrawerLayers.resetDrawingGeometry();
                begin
                    //by activating all drawing layers all graphic objects are collected into activeGraphicObjects
                        activateAllDrawingLayers();

                    //all graphic objects can now be freed by activeGraphicObjects clear procedure
                        activeGraphicObjects.clearGraphicObjectsGroup( True );

                    layerGeometryMap.clear();

                    orderedLayerKeys.Clear();

                    SetLength( arrActiveDrawingLayers, 0 );

                    currentDrawingLayer := '';
                end;

end.
