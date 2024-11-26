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
                    //draw all geometry
                        procedure drawAllGeometry(  const canvasHeightIn, canvasWidthIn : integer;
                                                    const canvasIn                      : TDirect2DCanvas );
            public
                //constructor
                    constructor create();
                //destructor
                    destructor destroy(); override;
                //draw all geometry
                    function drawAllGeometryToBitmap(const canvasHeightIn, canvasWidthIn : integer) : TBitmap;
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

            //draw all geometry
                procedure TDirect2DGeomDrawer.drawAllGeometry(  const canvasHeightIn, canvasWidthIn : integer;
                                                                const canvasIn                      : TDirect2DCanvas );
                    begin
                        //set canvas
                            setDrawingCanvas( canvasIn );

                            Direct2DDrawingCanvas.RenderTarget.SetAntialiasMode( D2D1_ANTIALIAS_MODE.D2D1_ANTIALIAS_MODE_PER_PRIMITIVE );

                            Direct2DDrawingCanvas.BeginDraw();

                            Direct2DDrawingCanvas.Brush.Color := drawingBackgroundColour;

                            Direct2DDrawingCanvas.FillRect( Rect(0, 0, canvasWidthIn, canvasHeightIn) );

                        //draw all geometry
                            inherited drawAllGeometry( canvasHeightIn, canvasWidthIn );

                            Direct2DDrawingCanvas.EndDraw();
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
            function TDirect2DGeomDrawer.drawAllGeometryToBitmap(const canvasHeightIn, canvasWidthIn : integer) : TBitmap;
                var
                    bitmapOut : TBitmap;
                    D2DCanvas : TDirect2DCanvas;
                begin
                    //creat bitmap
                        BitmapOut := TBitmap.Create( canvasWidthIn, canvasHeightIn );

                    //create D2D canvas
                        D2DCanvas := TDirect2DCanvas.Create( bitmapOut.Canvas, Rect(0, 0, canvasWidthIn, canvasHeightIn) );

                    //draw all geometry to the canvas (and indirectly the bitmap)
                        drawAllGeometry( canvasHeightIn, canvasWidthIn, D2DCanvas );

                    //free D2D canvas
                        FreeAndNil( D2DCanvas );
//
                    result := BitmapOut;
//                    result := drawToBitMap( canvasWidthIn, canvasHeightIn, clBtnFace );
                end;

end.
