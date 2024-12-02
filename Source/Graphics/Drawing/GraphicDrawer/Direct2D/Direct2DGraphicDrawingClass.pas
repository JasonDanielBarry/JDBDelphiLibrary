unit Direct2DGraphicDrawingClass;

interface

    uses
        //Delphi
            system.SysUtils, system.types, system.UITypes, system.UIConsts,
            vcl.Graphics,
            vcl.Direct2D, Winapi.D2D1,
        //custom
            DrawingAxisConversionClass,
            GraphicGeometryClass,
            GraphicDrawerAxisConversionInterfaceClass,
            GeometryBaseClass,
            Direct2DGraphicDrawingMethods;

    type
        TDirect2DGraphicDrawer = class(TGraphicDrawerAxisConversionInterface)
            private
                var
                    Direct2DDrawingCanvas : TDirect2DCanvas;
                //modifiers
                    //drawing canvas
                        procedure setDrawingCanvas(const canvasIn : TDirect2DCanvas);
                //drawing procedures
                    //auto detect geom type
                        procedure drawGeometry(const drawingGeometryIn : TGraphicGeometry); override;

            public
                //constructor
                    constructor create();
                //destructor
                    destructor destroy(); override;
                //draw all geometry
                    procedure drawAllGeometry(  const canvasWidthIn, canvasHeightIn : integer;
                                                const canvasIn                      : TDirect2DCanvas );
        end;

implementation

    //private
        //modifiers
            //set the drawing canvas
                procedure TDirect2DGraphicDrawer.setDrawingCanvas(const canvasIn : TDirect2DCanvas);
                    begin
                        Direct2DDrawingCanvas := canvasIn;
                    end;

        //drawing procedures
            //auto detect geom type
                procedure TDirect2DGraphicDrawer.drawGeometry(const drawingGeometryIn : TGraphicGeometry);
                    begin
                        drawDirect2DGeometry(   drawingGeometryIn,
                                                axisConverter,
                                                Direct2DDrawingCanvas   );
                    end;

    //public
        //constructor
            constructor TDirect2DGraphicDrawer.create();
                begin
                    inherited create();
                end;

        //destructor
            destructor TDirect2DGraphicDrawer.destroy();
                begin
                    inherited destroy();
                end;

        //draw all geometry
            procedure TDirect2DGraphicDrawer.drawAllGeometry(  const canvasWidthIn, canvasHeightIn : integer;
                                                            const canvasIn                      : TDirect2DCanvas );
                begin
                    //set canvas
                        setDrawingCanvas( canvasIn );

                    //clear the canvas
                        Direct2DDrawingCanvas.Brush.Color := drawingBackgroundColour;

                        Direct2DDrawingCanvas.FillRect( Rect(0, 0, canvasWidthIn, canvasHeightIn) );

                    //draw all geometry
                        inherited drawAllGeometry( canvasWidthIn, canvasHeightIn );
                end;

end.
