unit SkiaDrawingClass;

interface

    uses
        //Delphi
            system.SysUtils, system.types, system.UITypes,
            System.Skia, Vcl.Skia,
        //custom
            DrawingAxisConversionClass,
            DrawingGeometryClass,
            GeomDrawerAxisConversionInterfaceClass,
            GeometryBaseClass,
            SkiaDrawingMethods;

    type
        TSkiaGeomDrawer = class(TGeomDrawerAxisConversionInterface)
            private
                var
                    skiaDrawingCanvas : ISkCanvas;
                //modifiers
                    //drawing canvas
                        procedure setDrawingCanvas(const canvasIn : ISkCanvas);
                //drawing procedures
                    //auto detect geom type
                        procedure drawGeometry(const drawingGeometryIn : TDrawingGeometry); override;
            public
                //constructor
                    constructor create();
                //destructor
                    destructor destroy(); override;
                //draw all geometry
                    procedure drawAllGeometry(  const canvasHeightIn, canvasWidthIn : integer;
                                                const canvasIn                      : ISkCanvas );
                    function drawAllGeometryToSurface(const canvasHeightIn, canvasWidthIn : integer) : ISkSurface;
        end;

implementation

    //private
        //modifiers
            //set the drawing canvas
                procedure TSkiaGeomDrawer.setDrawingCanvas(const canvasIn : ISkCanvas);
                    begin
                        skiaDrawingCanvas := canvasIn;
                    end;

        //drawing procedures

            //auto detect geom type
                procedure TSkiaGeomDrawer.drawGeometry(const drawingGeometryIn : TDrawingGeometry);
                    begin
                        drawSkiaGeometry(   drawingGeometryIn,
                                            axisConverter,
                                            skiaDrawingCanvas,
                                            false               );
                    end;

    //public
        //constructor
            constructor TSkiaGeomDrawer.create();
                begin
                    inherited create();
                end;

        //destructor
            destructor TSkiaGeomDrawer.destroy();
                begin
                    inherited destroy();
                end;

        //draw all geometry
            procedure TSkiaGeomDrawer.drawAllGeometry(  const canvasHeightIn, canvasWidthIn : integer;
                                                        const canvasIn                      : ISkCanvas );
                begin
                    //set canvas
                        setDrawingCanvas( canvasIn );

                    //draw all geometry
                        inherited drawAllGeometry( canvasHeightIn, canvasWidthIn );
                end;

            function TSkiaGeomDrawer.drawAllGeometryToSurface(const canvasHeightIn, canvasWidthIn : integer) : ISkSurface;
                var
                    skiaSurface : ISkSurface;
                begin
                    //create a skia skiaSurface
                        skiaSurface := TSkSurface.MakeRaster( canvasWidthIn, canvasHeightIn );

                    //clear the skiaSurface
                        skiaSurface.Canvas.Clear( drawingBackgroundColour );

                    //draw all geometry on skiaSurface canvas
                        drawAllGeometry(canvasWidthIn, canvasHeightIn,
                                        skiaSurface.Canvas              );

                    result := skiaSurface
                end;

end.
