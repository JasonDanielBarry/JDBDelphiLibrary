unit GraphicLineClass;

interface

    uses
        //Delphi
            system.SysUtils, system.types, system.UITypes, System.UIConsts,
            Winapi.D2D1, Vcl.Direct2D,
            vcl.Graphics,
        //custom
            GraphicGeometryClass,
            DrawingAxisConversionClass,
            GeometryTypes,
            GeometryBaseClass
            ;

    type
        TGraphicLine = class(TGraphicGeometry)
            //constructor
                constructor create( const   lineThicknessIn : integer;
                                    const   lineColourIn    : TColor;
                                    const   lineStyleIn     : TPenStyle;
                                    const   geometryIn      : TGeomBase );
            //destructor
                destructor destroy(); override;
            //draw to canvas
                procedure drawToCanvas( const axisConverterIn   : TDrawingAxisConverter;
                                        var canvasInOut         : TDirect2DCanvas       ); override;
        end;

implementation

    //public
        //constructor
            constructor TGraphicLine.create(const   lineThicknessIn : integer;
                                            const   lineColourIn    : TColor;
                                            const   lineStyleIn     : TPenStyle;
                                            const   geometryIn      : TGeomBase );
                begin
                    inherited create(   lineThicknessIn,
                                        lineColourIn,
                                        lineStyleIn,
                                        geometryIn          );
                end;

        //destructor
            destructor TGraphicLine.destroy();
                begin
                    inherited destroy();
                end;

        //draw to canvas
            procedure TGraphicLine.drawToCanvas(const axisConverterIn   : TDrawingAxisConverter;
                                                var canvasInOut         : TDirect2DCanvas       );
                var
                    pathGeometry : ID2D1PathGeometry;
                begin
                    if (length( geometryPoints ) < 2) then
                        exit();

                    pathGeometry := createPathGeometry(
                                                            D2D1_FIGURE_BEGIN.D2D1_FIGURE_BEGIN_HOLLOW,
                                                            D2D1_FIGURE_END.D2D1_FIGURE_END_OPEN,
                                                            axisConverterIn
                                                      );

                    setLineProperties( canvasInOut );

                    canvasInOut.DrawGeometry( pathGeometry );
                end;

end.
