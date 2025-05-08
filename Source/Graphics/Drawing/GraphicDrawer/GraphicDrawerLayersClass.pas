unit GraphicDrawerLayersClass;

interface

    uses
        //Delphi
            system.SysUtils, system.types, system.UIConsts, system.UITypes, system.Generics.Collections,
        //custom
            ColourMethods,
            DrawingAxisConversionClass,
            GraphicObjectBaseClass, GraphicObjectGroupClass, GraphicGridClass,
            GeometryTypes, GeomBox,
            GraphicDrawerObjectAdderClass
            ;

    type
        TGraphicDrawerLayers = class(TGraphicDrawerObjectAdder)
            private
                type
                    TLayerGeometryMap = TOrderedDictionary<string, TArray<TGraphicObject>>;
                var
                    gridEnabled             : boolean;
                    currentDrawingLayer     : string;
                    graphicGrid             : TGraphicGrid;
                    activeGraphicObjects    : TGraphicObjectGroup;
                    layerGeometryMap        : TLayerGeometryMap;
                //add graphic drawing object to the drawing object container
                    procedure addGraphicObject(const drawingGeometryIn : TGraphicObject); override;
                //helper methods
                    //return a specified layer's graphic objects array
                        function getArrGraphicObjects(const layerKeyIn : string) : TArray<TGraphicObject>;
                    //collect all active graphic objects into one array in order
                        procedure collectAllActiveGraphicObjects(const arrActiveDrawingLayersIn : TArray<string>);
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
                    procedure setGridEnabled(const enabledIn : boolean);
                    procedure setGridElementsVisiblity(const gridVisibilitySettingsIn : TGridVisibilitySettings);
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
                procedure TGraphicDrawerLayers.collectAllActiveGraphicObjects(const arrActiveDrawingLayersIn : TArray<string>);
                    var
                        layer                           : string;
                        arrCurrentLayerGraphicObjects   : TArray<TGraphicObject>;
                    begin
                        //clear the active group - DO NOT FREE THE OBJECTS
                            activeGraphicObjects.clearGraphicObjectsGroup( False );

                        //loop through the active layers and place their graphic objects in the group
                            for layer in arrActiveDrawingLayersIn do
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

                        //draw the grid
                            if ( gridEnabled ) then
                                graphicGrid.drawToCanvas( axisConverter, Direct2DDrawingCanvas );

                        //draw graphic objects
                            activeGraphicObjects.drawToCanvas( axisConverter, Direct2DDrawingCanvas );

                        //draw the grid axis labels
                            if ( gridEnabled ) then
                                graphicGrid.drawAxisLabels( axisConverter, Direct2DDrawingCanvas );
                    end;

    //public
        //constructor
            constructor TGraphicDrawerLayers.create();
                begin
                    inherited create();

                    graphicGrid             := TGraphicGrid.create();
                    layerGeometryMap        := TLayerGeometryMap.Create();
                    activeGraphicObjects    := TGraphicObjectGroup.create();
                    currentDrawingLayer     := '';
                end;

        //destructor
            destructor TGraphicDrawerLayers.destroy();
                begin
                    resetDrawingGeometry();

                    FreeAndNil( graphicGrid );
                    FreeAndNil( layerGeometryMap );
                    FreeAndNil( activeGraphicObjects );

                    inherited destroy();
                end;

        //accessors
            function TGraphicDrawerLayers.getAllDrawingLayers() : TArray<string>;
                begin
                    result := layerGeometryMap.Keys.ToArray();
                end;

        //modifiers
            procedure TGraphicDrawerLayers.setGridEnabled(const enabledIn : boolean);
                begin
                    gridEnabled := enabledIn;
                end;

            procedure TGraphicDrawerLayers.setGridElementsVisiblity(const gridVisibilitySettingsIn : TGridVisibilitySettings);
                begin
                    graphicGrid.setGridElementsVisiblity( gridVisibilitySettingsIn );
                end;

            procedure TGraphicDrawerLayers.setCurrentDrawingLayer(const layerKeyIn : string);
                begin
                    currentDrawingLayer := layerKeyIn;
                end;

            procedure TGraphicDrawerLayers.setActiveDrawingLayers(const arrActiveDrawingLayersIn : TArray<string>);
                begin
                    //place all the active graphic object into a single array for drawing
                        collectAllActiveGraphicObjects( arrActiveDrawingLayersIn );

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

                    currentDrawingLayer := '';
                end;

end.
