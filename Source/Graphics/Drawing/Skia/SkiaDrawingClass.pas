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

end.
