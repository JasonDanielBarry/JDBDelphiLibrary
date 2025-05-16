unit GraphicDrawerLayersClass;

interface

    uses
        //Delphi
            Winapi.D2D1, Vcl.Direct2D,
            system.SysUtils, system.UITypes, system.Generics.Collections,
        //custom
            GraphicObjectListBaseClass,
            LayerGraphicObjectMapClass,
            DrawingAxisConversionClass,
            GraphicObjectBaseClass, GraphicGridClass,
            GeomBox,
            GraphicDrawerBaseClass
            ;

    type
        TGraphicDrawerLayers = class(TGraphicDrawerBase)
            private
                var
                    gridEnabled             : boolean;
                    graphicGrid             : TGraphicGrid;
                    layerGraphicObjectMap   : TLayerGraphicObjectMap;
                //bounding box
                    procedure determineActiveBoundingBox();
            protected
                //draw all geometry
                    procedure drawAll(  const canvasWidthIn, canvasHeightIn : integer;
                                        const drawingBackgroundColourIn     : TColor;
                                        var D2DCanvasInOut                  : TDirect2DCanvas);
            public
                //constructor
                    constructor create(); override;
                //destructor
                    destructor destroy(); override;
                //add graphic drawing objects to the drawing object container
                    procedure readGraphicObjectList(const graphicObjectListIn : TGraphicObjectListBase);
                //accessors
                    function getAllDrawingLayers() : TArray<string>;
                //modifiers
                    procedure setGridEnabled(const enabledIn : boolean);
                    procedure setGridElementsVisiblity(const gridVisibilitySettingsIn : TGridVisibilitySettings);
                    procedure setActiveDrawingLayers(const arrActiveDrawingLayersIn : TArray<string>);
                    procedure activateAllDrawingLayers();
                //reset
                    procedure resetDrawingGeometry();
        end;

implementation

    //private
        //bounding box
            procedure TGraphicDrawerLayers.determineActiveBoundingBox();
                var
                    activeBoundingBox : TGeomBox;
                begin
                    activeBoundingBox := layerGraphicObjectMap.determineActiveBoundingBox();

                    axisConverter.setGeometryBoundary( activeBoundingBox );
                end;

    //protected
        //drawing procedures
            //draw all geometry
                procedure TGraphicDrawerLayers.drawAll( const canvasWidthIn, canvasHeightIn : integer;
                                                        const drawingBackgroundColourIn     : TColor;
                                                        var D2DCanvasInOut                  : TDirect2DCanvas);
                    begin
                        inherited drawAll(  canvasWidthIn, canvasHeightIn,
                                            drawingBackgroundColourIn,
                                            D2DCanvasInOut                  );

                        //draw the grid
                            if ( gridEnabled ) then
                                graphicGrid.drawToCanvas( axisConverter, D2DCanvasInOut );

                        //draw graphic objects
                            layerGraphicObjectMap.drawActiveGraphicObjectsToCanvas( axisConverter, D2DCanvasInOut );

                        //draw the grid axis labels
                            if ( gridEnabled ) then
                                graphicGrid.drawAxisLabels( axisConverter, D2DCanvasInOut );
                    end;

    //public
        //constructor
            constructor TGraphicDrawerLayers.create();
                begin
                    inherited create();

                    graphicGrid             := TGraphicGrid.create();
                    layerGraphicObjectMap        := TLayerGraphicObjectMap.Create();
                end;

        //destructor
            destructor TGraphicDrawerLayers.destroy();
                begin
                    resetDrawingGeometry();

                    FreeAndNil( graphicGrid );
                    FreeAndNil( layerGraphicObjectMap );

                    inherited destroy();
                end;

        //add graphic objects to the map
            procedure TGraphicDrawerLayers.readGraphicObjectList(const graphicObjectListIn : TGraphicObjectListBase);
                begin
                    layerGraphicObjectMap.readGraphicObjectList( graphicObjectListIn );
                end;

        //accessors
            function TGraphicDrawerLayers.getAllDrawingLayers() : TArray<string>;
                begin
                    result := layerGraphicObjectMap.Keys.ToArray();
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

            procedure TGraphicDrawerLayers.setActiveDrawingLayers(const arrActiveDrawingLayersIn : TArray<string>);
                begin
                    layerGraphicObjectMap.setActiveDrawingLayers( arrActiveDrawingLayersIn );

                    determineActiveBoundingBox();
                end;

            procedure TGraphicDrawerLayers.activateAllDrawingLayers();
                begin
                    layerGraphicObjectMap.activateAllDrawingLayers();

                    determineActiveBoundingBox();
                end;

        //reset drawing geometry by freeing all drawing geometry objects
            procedure TGraphicDrawerLayers.resetDrawingGeometry();
                begin
                    layerGraphicObjectMap.clear();
                end;

end.
