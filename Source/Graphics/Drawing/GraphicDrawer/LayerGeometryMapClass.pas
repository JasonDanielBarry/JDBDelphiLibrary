unit LayerGeometryMapClass;

interface

    uses
        //Delphi
            Winapi.D2D1, Vcl.Direct2D,
            system.SysUtils, system.Generics.Collections,
        //custom
            DrawingAxisConversionClass,
            GraphicObjectBaseClass, GraphicObjectGroupClass, GraphicGridClass,
            GeomBox
            ;

    type
        TLayerGeometryMap = class(TOrderedDictionary< string, TArray< TGraphicObject > >)
            private
                var
                    currentDrawingLayer     : string;
                    activeGraphicObjects    : TGraphicObjectGroup;
            public
                //constructor
                    constructor create();
                //destructor
                    destructor destroy(); override;
                //clear graphic objects
                    procedure clear();
                //set the drawing layer for adding objects
                    procedure setCurrentDrawingLayer(const layerKeyIn : string);
                //add graphic drawing object
                    procedure addGraphicObject(const drawingGeometryIn : TGraphicObject);
                //set active drawing layers
                    procedure setActiveDrawingLayers(const arrDrawingLayersToActiveIn : TArray<string>);
                    procedure activateAllDrawingLayers();
                //active drawing layers bounding box
                    function determineActiveBoundingBox() : TGeomBox;
                //draw active graphic objects
                    procedure drawActiveGraphicObjectsToCanvas( const axisConverter : TDrawingAxisConverter;
                                                                var D2DCanvasInOut : TDirect2DCanvas        ); inline;
        end;

implementation

    //public
        //constructor
            constructor TLayerGeometryMap.create();
                begin
                    inherited Create();

                    currentDrawingLayer     := '';
                    activeGraphicObjects    := TGraphicObjectGroup.create();
                end;

        //destructor
            destructor TLayerGeometryMap.destroy();
                begin
                    clear();
                    FreeAndNil( activeGraphicObjects );

                    inherited destroy();
                end;

        //clear graphic objects
            procedure TLayerGeometryMap.clear();
                begin
                    activateAllDrawingLayers();

                    activeGraphicObjects.clearGraphicObjectsGroup( True );

                    currentDrawingLayer := '';

                    inherited clear();
                end;

        //set the drawing layer for adding objects
            procedure TLayerGeometryMap.setCurrentDrawingLayer(const layerKeyIn : string);
                begin
                    currentDrawingLayer := layerKeyIn;
                end;

        //add graphic drawing object
            procedure TLayerGeometryMap.addGraphicObject(const drawingGeometryIn : TGraphicObject);
                var
                    graphicObjectCount  : integer;
                    arrGraphicObjects   : TArray<TGraphicObject>;
                begin
                    //if the current layer is not set then set to default
                        if (currentDrawingLayer = '') then
                            setCurrentDrawingLayer( 'Default Layer' );

                    //get the drawing geometry array and add the new drawing geometry to it
                        TryGetValue( currentDrawingLayer, arrGraphicObjects );

                        graphicObjectCount := length( arrGraphicObjects );

                        SetLength( arrGraphicObjects, graphicObjectCount + 1 );

                        arrGraphicObjects[ graphicObjectCount ] := drawingGeometryIn;

                    //add the array back to the map
                        AddOrSetValue( currentDrawingLayer, arrGraphicObjects );
                end;

        //set active drawing layers
            procedure TLayerGeometryMap.setActiveDrawingLayers(const arrDrawingLayersToActiveIn : TArray<string>);
                var
                    i, arrLen           : integer;
                    layer               : string;
                    arrGraphicObjects   : TArray<TGraphicObject>;
                begin
                    //clear active graphic objects
                        activeGraphicObjects.clearGraphicObjectsGroup( False );

                    arrLen := length( arrDrawingLayersToActiveIn );

                    for i := 0 to (arrLen - 1) do
                        begin
                            layer := arrDrawingLayersToActiveIn[i];

                            //check if layer is valid
                                if NOT( ContainsKey( layer ) ) then
                                    Continue;

                            //try get the layer's graphic objects
                                if NOT( TryGetValue( layer, arrGraphicObjects ) ) then
                                    Continue;

                            activeGraphicObjects.addGraphicObjectsToGroup( arrGraphicObjects );
                        end;
                end;

            procedure TLayerGeometryMap.activateAllDrawingLayers();
                var
                    allDrawingLayers : TArray<string>;
                begin
                    allDrawingLayers := Keys.ToArray();

                    setActiveDrawingLayers( allDrawingLayers );
                end;

        //active drawing layers bounding box
            function TLayerGeometryMap.determineActiveBoundingBox() : TGeomBox;
                begin
                    result := activeGraphicObjects.determineBoundingBox();
                end;

        //draw active graphic objects
            procedure TLayerGeometryMap.drawActiveGraphicObjectsToCanvas(   const axisConverter : TDrawingAxisConverter;
                                                                            var D2DCanvasInOut : TDirect2DCanvas        );
                begin
                    activeGraphicObjects.drawToCanvas( axisConverter, D2DCanvasInOut );
                end;

end.
