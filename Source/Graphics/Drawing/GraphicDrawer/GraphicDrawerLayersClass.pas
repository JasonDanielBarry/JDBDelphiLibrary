unit GraphicDrawerLayersClass;

interface

    uses
        //Delphi
            Winapi.D2D1, Vcl.Direct2D,
            system.SysUtils, system.UITypes, system.Generics.Collections,
        //custom
            LayerGeometryMapClass,
            DrawingAxisConversionClass,
            GraphicObjectBaseClass, GraphicGridClass,
            GeomBox,
            GraphicDrawerObjectAdderClass
            ;

    type
        TGraphicDrawerLayers = class(TGraphicDrawerObjectAdder)
            private
                var
                    gridEnabled             : boolean;
                    graphicGrid             : TGraphicGrid;
                    layerGeometryMap        : TLayerGeometryMap;
                //add graphic drawing object to the drawing object container
                    procedure addGraphicObject(const drawingObjectIn : TGraphicObject); override;
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
            procedure TGraphicDrawerLayers.addGraphicObject(const drawingObjectIn : TGraphicObject);
                begin
                    layerGeometryMap.addGraphicObject( drawingObjectIn );
                end;

        //bounding box
            procedure TGraphicDrawerLayers.determineActiveBoundingBox();
                var
                    activeBoundingBox : TGeomBox;
                begin
                    activeBoundingBox := layerGeometryMap.determineActiveBoundingBox();

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
                            layerGeometryMap.drawActiveGraphicObjectsToCanvas( axisConverter, D2DCanvasInOut );

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
                    layerGeometryMap        := TLayerGeometryMap.Create();
                end;

        //destructor
            destructor TGraphicDrawerLayers.destroy();
                begin
                    resetDrawingGeometry();

                    FreeAndNil( graphicGrid );
                    FreeAndNil( layerGeometryMap );

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
                    layerGeometryMap.setCurrentDrawingLayer( layerKeyIn );
                end;

            procedure TGraphicDrawerLayers.setActiveDrawingLayers(const arrActiveDrawingLayersIn : TArray<string>);
                var
                    activeBoundingBox : TGeomBox;
                begin
                    layerGeometryMap.setActiveDrawingLayers( arrActiveDrawingLayersIn );

                    determineActiveBoundingBox();
                end;

            procedure TGraphicDrawerLayers.activateAllDrawingLayers();
                begin
                    layerGeometryMap.activateAllDrawingLayers();

                    determineActiveBoundingBox();
                end;

        //reset drawing geometry by freeing all drawing geometry objects
            procedure TGraphicDrawerLayers.resetDrawingGeometry();
                begin
                    layerGeometryMap.clear();
                end;

end.
