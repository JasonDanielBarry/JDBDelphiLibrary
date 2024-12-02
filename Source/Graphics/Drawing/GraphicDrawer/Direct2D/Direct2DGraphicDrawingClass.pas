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
                    constructor create();
                //destructor
                    destructor destroy(); override;
                //draw all geometry
                    procedure drawAllGeometry(  const canvasWidthIn, canvasHeightIn : integer;
                                                const drawingBackgroundColourIn     : TColor;
                                                const canvasIn                      : TDirect2DCanvas );
        end;

implementation

    //private


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
            procedure TDirect2DGraphicDrawer.drawAllGeometry(   const canvasWidthIn, canvasHeightIn : integer;
                                                                const drawingBackgroundColourIn     : TColor;
                                                                const canvasIn                      : TDirect2DCanvas   );
                begin
                    Direct2DDrawingCanvas := canvasIn;

                    //draw all geometry
                        inherited drawAllGeometry( canvasWidthIn, canvasHeightIn, drawingBackgroundColourIn );
                end;

end.
