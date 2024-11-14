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
                    procedure drawAllGeometry(  const canvasIn          : TDirect2DCanvas;
                                                const axisConverterIn   : TDrawingAxisConverter );
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
                                                Direct2DDrawingCanvas,
                                                false                   );
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
            procedure TDirect2DGeomDrawer.drawAllGeometry(  const canvasIn          : TDirect2DCanvas;
                                                            const axisConverterIn   : TDrawingAxisConverter );
                begin
                    //set canvas
                        setDrawingCanvas( canvasIn );

                    //draw all geometry
                        inherited drawAllGeometry();
                end;

end.
