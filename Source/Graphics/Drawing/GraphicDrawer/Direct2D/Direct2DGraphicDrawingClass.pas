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
            GeometryBaseClass;

    type
        TDirect2DGraphicDrawer = class(TGraphicDrawerAxisConversionInterface)
            public
                //constructor
                    constructor create(); override;
                //destructor
                    destructor destroy(); override;
                //draw all geometry
                    procedure drawAll(  const canvasWidthIn, canvasHeightIn : integer;
                                        const drawingBackgroundColourIn     : TColor;
                                        var D2DCanvasInOut                  : TDirect2DCanvas );
        end;

implementation

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
            procedure TDirect2DGraphicDrawer.drawAll(   const canvasWidthIn, canvasHeightIn : integer;
                                                        const drawingBackgroundColourIn     : TColor;
                                                        var D2DCanvasInOut                  : TDirect2DCanvas   );
                begin
                    //draw all geometry
                        inherited drawAll(
                                            canvasWidthIn,
                                            canvasHeightIn,
                                            drawingBackgroundColourIn,
                                            D2DCanvasInOut
                                         );
                end;

end.
