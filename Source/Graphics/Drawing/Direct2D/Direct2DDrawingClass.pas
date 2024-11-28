unit Direct2DDrawingClass;

interface

    uses
        //Delphi
            system.SysUtils, system.types, system.UITypes, system.UIConsts,
            vcl.Graphics,
            vcl.Direct2D, Winapi.D2D1,
        //custom
            DrawingAxisConversionClass,
            DrawingGeometryClass,
            GeomDrawerAxisConversionInterfaceClass,
            GeometryBaseClass,
            Direct2DDrawingMethods;

    type
        TDirect2DGeomDrawer = class(TGeomDrawerAxisConversionInterface)
            private
                var
                    Direct2DDrawingCanvas : TDirect2DCanvas;
                //modifiers
                    //drawing canvas
                        procedure setDrawingCanvas(const canvasIn : TDirect2DCanvas);
                //drawing procedures
                    //auto detect geom type
                        procedure drawGeometry(const drawingGeometryIn : TDrawingGeometry); override;

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
                procedure TDirect2DGeomDrawer.setDrawingCanvas(const canvasIn : TDirect2DCanvas);
                    begin
                        Direct2DDrawingCanvas := canvasIn;
                    end;

        //drawing procedures
            //auto detect geom type
                procedure TDirect2DGeomDrawer.drawGeometry(const drawingGeometryIn : TDrawingGeometry);
                    begin
                        drawDirect2DGeometry(   drawingGeometryIn,
                                                axisConverter,
                                                Direct2DDrawingCanvas   );
                    end;

    //public
        //constructor
            constructor TDirect2DGeomDrawer.create();
                begin
                    inherited create();
                end;

        //destructor
            destructor TDirect2DGeomDrawer.destroy();
                begin
                    inherited destroy();
                end;

        //draw all geometry
            procedure TDirect2DGeomDrawer.drawAllGeometry(  const canvasWidthIn, canvasHeightIn : integer;
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
