unit SkiaDrawingClass;

interface

    uses
        //Delphi
            system.SysUtils, system.types, system.UITypes,
            System.Skia, Vcl.Skia,
        //custom
            DrawingAxisConversionClass,
            DrawingGeometryClass,
            GeomDrawerBaseClass,
            GeometryBaseClass,
            SkiaDrawingMethods;

    type
        TSkiaGeomDrawer = class(TGeomDrawerBase)
            private
                var
                    skiaDrawingCanvas : ISkCanvas;
                //modifiers
                    //drawing canvas
                        procedure setDrawingCanvas(const canvasIn : ISkCanvas);
                //drawing procedures
                    //auto detect geom type
                        procedure drawGeometry( const drawingGeometryIn : TDrawingGeometry;
                                                const axisConverterIn   : TDrawingAxisConverter); override;
            public
                //constructor
                    constructor create();
                //destructor
                    destructor destroy(); override;
                //draw all geometry
                    procedure drawAllGeometry(  const canvasIn          : ISkCanvas;
                                                const axisConverterIn   : TDrawingAxisConverter );
                    function drawAllGeometryToSurface(  const heightIn, widthIn : integer;
                                                        const axisConverterIn   : TDrawingAxisConverter ) : ISkSurface;
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
                procedure TSkiaGeomDrawer.drawGeometry( const drawingGeometryIn : TDrawingGeometry;
                                                        const axisConverterIn   : TDrawingAxisConverter);
                    begin
                        drawSkiaGeometry(   drawingGeometryIn,
                                            axisConverterIn,
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
            procedure TSkiaGeomDrawer.drawAllGeometry(  const canvasIn          : ISkCanvas;
                                                        const axisConverterIn   : TDrawingAxisConverter );
                begin
                    //set canvas
                        setDrawingCanvas( canvasIn );

                    //draw all geometry
                        inherited drawAllGeometry( axisConverterIn );
                end;

            function TSkiaGeomDrawer.drawAllGeometryToSurface(  const heightIn, widthIn : integer;
                                                                const axisConverterIn   : TDrawingAxisConverter ) : ISkSurface;
                var
                    skiaSurface : ISkSurface;
                begin
                    //create a skia skiaSurface
                        skiaSurface := TSkSurface.MakeRaster( widthIn, heightIn );

                    //clear the skiaSurface
                        skiaSurface.Canvas.Clear( drawingBackgroundColour );

                    //give axis converter canvas dimensions
                        axisConverterIn.setCanvasDimensions( heightIn, widthIn );

                    //draw all geometry on skiaSurface canvas
                        drawAllGeometry( skiaSurface.Canvas, axisConverterIn );

                    result := skiaSurface
                end;

end.
