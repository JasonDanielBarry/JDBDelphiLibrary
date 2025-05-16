unit LayerGraphicObjectMapClass;

interface

    uses
        //Delphi
            Winapi.D2D1, Vcl.Direct2D,
            system.SysUtils, system.Generics.Collections,
        //custom
            DrawingAxisConversionClass,
            GraphicObjectBaseClass, GraphicObjectGroupClass,
            GeomBox,
            GraphicObjectListBaseClass
            ;

    type
        TLayerGraphicObjectMap = class(TOrderedDictionary< string, TArray< TGraphicObject > >)
            private
                var
                    activeGraphicObjects : TGraphicObjectGroup;
                //add graphic drawing object
                    procedure addGraphicObject( const layerIn           : string;
                                                const graphicObjectIn   : TGraphicObject );
            public
                //constructor
                    constructor create();
                //destructor
                    destructor destroy(); override;
                //clear graphic objects
                    procedure clear();
                //read in a graphic object list
                    procedure readGraphicObjectList(const graphicObjectListIn : TGraphicObjectListBase);
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
            constructor TLayerGraphicObjectMap.create();
                begin
                    inherited Create();

                    activeGraphicObjects := TGraphicObjectGroup.create();
                end;

        //destructor
            destructor TLayerGraphicObjectMap.destroy();
                begin
                    clear();
                    FreeAndNil( activeGraphicObjects );

                    inherited destroy();
                end;

        //clear graphic objects
            procedure TLayerGraphicObjectMap.clear();
                begin
                    activateAllDrawingLayers();

                    activeGraphicObjects.clearGraphicObjectsGroup( True );

                    inherited clear();
                end;

        //add graphic drawing object
            procedure TLayerGraphicObjectMap.addGraphicObject(  const layerIn           : string;
                                                                const graphicObjectIn   : TGraphicObject    );
                var
                    graphicObjectCount  : integer;
                    arrGraphicObjects   : TArray<TGraphicObject>;
                begin
                    //get the drawing geometry array and add the new drawing geometry to it
                        TryGetValue( layerIn, arrGraphicObjects );

                        graphicObjectCount := length( arrGraphicObjects );

                        SetLength( arrGraphicObjects, graphicObjectCount + 1 );

                        arrGraphicObjects[ graphicObjectCount ] := graphicObjectIn;

                    //add the array back to the map
                        AddOrSetValue( layerIn, arrGraphicObjects );
                end;

        //read in a graphic object list
            procedure TLayerGraphicObjectMap.readGraphicObjectList(const graphicObjectListIn : TGraphicObjectListBase);
                var
                    layerGraphicObjectItem : TPair<string, TGraphicObject>;
                begin
                    for layerGraphicObjectItem in graphicObjectListIn do
                        addGraphicObject(
                                            layerGraphicObjectItem.Key,
                                            layerGraphicObjectItem.Value
                                        );
                end;

        //set active drawing layers
            procedure TLayerGraphicObjectMap.setActiveDrawingLayers(const arrDrawingLayersToActiveIn : TArray<string>);
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

            procedure TLayerGraphicObjectMap.activateAllDrawingLayers();
                var
                    allDrawingLayers : TArray<string>;
                begin
                    allDrawingLayers := Keys.ToArray();

                    setActiveDrawingLayers( allDrawingLayers );
                end;

        //active drawing layers bounding box
            function TLayerGraphicObjectMap.determineActiveBoundingBox() : TGeomBox;
                begin
                    result := activeGraphicObjects.determineBoundingBox();
                end;

        //draw active graphic objects
            procedure TLayerGraphicObjectMap.drawActiveGraphicObjectsToCanvas(  const axisConverter : TDrawingAxisConverter;
                                                                                var D2DCanvasInOut : TDirect2DCanvas        );
                begin
                    activeGraphicObjects.drawToCanvas( axisConverter, D2DCanvasInOut );
                end;

end.
