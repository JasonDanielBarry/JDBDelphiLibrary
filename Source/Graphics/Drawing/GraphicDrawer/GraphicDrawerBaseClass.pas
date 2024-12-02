unit GraphicDrawerBaseClass;

interface

    uses
        //Delphi
            system.SysUtils, system.types, system.UIConsts, system.UITypes, system.Generics.Collections,
            vcl.Graphics,
            vcl.Direct2D, Winapi.D2D1,
        //custom
            ColourMethods,
            DrawingAxisConversionClass,
            GraphicObjectBaseClass,
            GraphicGeometryClass, GraphicLineClass, GraphicPolylineClass, GraphicPolygonClass,
            GeometryTypes, GeomBox,
            GeometryBaseClass,
            GeomLineClass, GeomPolyLineClass, GeomPolygonClass
            ;

    type
        TGraphicDrawer = class
            strict protected
                var
                    axisConverter           : TDrawingAxisConverter;
                    Direct2DDrawingCanvas   : TDirect2DCanvas;
                //add graphic drawing object to the drawing object container
                    procedure addGraphicObject(const drawingGeometryIn : TGraphicObject); virtual; abstract;
                //drawing procedures
                    //draw all geometry
                        procedure drawAllGeometry(  const canvasWidthIn, canvasHeightIn : integer;
                                                    const drawingBackgroundColourIn     : TColor    ); virtual;
            public
                //constructor
                    constructor create(); virtual;
                //destructor
                    destructor destroy(); override;
                //add different drawing geometry
                    procedure addLine(  const lineIn            : TGeomLine;
                                        const lineThicknessIn   : integer = 2;
                                        const colourIn          : TColor = TColors.Black;
                                        const styleIn           : TPenStyle = TPenStyle.psSolid );
                    procedure addPolyline(  const polylineIn        : TGeomPolyLine;
                                            const lineThicknessIn   : integer = 2;
                                            const colourIn          : TColor = TColors.Black;
                                            const styleIn           : TPenStyle = TPenStyle.psSolid );
                    procedure addPolygon(   const polygonIn         : TGeomPolygon;
                                            const lineThicknessIn   : integer = 2;
                                            const fillColourIn      : TColor = TColors.Null;
                                            const lineColourIn      : TColor = TColors.Black;
                                            const lineStyleIn       : TPenStyle = TPenStyle.psSolid     );
                //modifiers
                    procedure setCurrentDrawingLayer(const layerKeyIn : string); virtual; abstract;
        end;

implementation

    //protected
        //drawing procedures
            //draw all geometry
                procedure TGraphicDrawer.drawAllGeometry(   const canvasWidthIn, canvasHeightIn : integer;
                                                            const drawingBackgroundColourIn     : TColor    );
                    begin
                        //set axis converter canvas dimensions
                            axisConverter.setCanvasDimensions( canvasHeightIn, canvasWidthIn );

                        //set the drawing space ratio
                            axisConverter.setDrawingSpaceRatio( 1 );

                        //clear the canvas
                            Direct2DDrawingCanvas.Brush.Color := drawingBackgroundColourIn;

                            Direct2DDrawingCanvas.FillRect( Rect(0, 0, canvasWidthIn, canvasHeightIn) );
                    end;

    //public
        //constructor
            constructor TGraphicDrawer.create();
                begin
                    inherited create();

                    axisConverter := TDrawingAxisConverter.create();
                end;

        //destructor
            destructor TGraphicDrawer.destroy();
                begin
                    FreeAndNil( axisConverter );

                    inherited destroy();
                end;

        //add drawing geometry
            procedure TGraphicDrawer.addLine(   const lineIn            : TGeomLine;
                                                const lineThicknessIn   : integer = 2;
                                                const colourIn          : TColor = TColors.Black;
                                                const styleIn           : TPenStyle = TPenStyle.psSolid );
                var
                    newDrawingGeometry : TGraphicGeometry;
                begin
                    newDrawingGeometry := TGraphicLine.create(  lineThicknessIn,
                                                                colourIn,
                                                                styleIn,
                                                                lineIn          );

                    addGraphicObject( newDrawingGeometry );
                end;

            procedure TGraphicDrawer.addPolyline(   const polylineIn        : TGeomPolyLine;
                                                    const lineThicknessIn   : integer = 2;
                                                    const colourIn          : TColor = TColors.Black;
                                                    const styleIn           : TPenStyle = TPenStyle.psSolid );
                var
                    newDrawingGeometry : TGraphicGeometry;
                begin
                    newDrawingGeometry := TGraphicPolyline.create(  lineThicknessIn,
                                                                    colourIn,
                                                                    styleIn,
                                                                    polylineIn      );

                    addGraphicObject( newDrawingGeometry );
                end;

            procedure TGraphicDrawer.addPolygon(const polygonIn         : TGeomPolygon;
                                                const lineThicknessIn   : integer = 2;
                                                const fillColourIn      : TColor = TColors.Null;
                                                const lineColourIn      : TColor = TColors.Black;
                                                const lineStyleIn       : TPenStyle = TPenStyle.psSolid     );
                var
                    newDrawingGeometry : TGraphicGeometry;
                begin
                    newDrawingGeometry := TGraphicPolygon.create(   lineThicknessIn,
                                                                    fillColourIn,
                                                                    lineColourIn,
                                                                    lineStyleIn,
                                                                    polygonIn       );

                    addGraphicObject( newDrawingGeometry );
                end;


end.
