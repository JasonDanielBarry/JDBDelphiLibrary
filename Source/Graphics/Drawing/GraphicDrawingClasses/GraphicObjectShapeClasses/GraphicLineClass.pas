unit GraphicLineClass;

interface

    uses
        //Delphi
            system.SysUtils, system.types, system.UITypes, System.UIConsts,
            Winapi.D2D1, Vcl.Direct2D,
            vcl.Graphics,
        //custom
            GraphicDrawingTypes,
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
            //modifiers
                procedure setStartPoint(const xIn, yIn : double);
                procedure setEndPoint(const xIn, yIn : double);
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

        //modifiers
            procedure TGraphicLine.setStartPoint(const xIn, yIn : double);
                begin
                    geometryPoints[0].setPoint( xIn, yIn );
                end;

            procedure TGraphicLine.setEndPoint(const xIn, yIn : double);
                begin
                    geometryPoints[1].setPoint( xIn, yIn );
                end;

        //draw to canvas
            procedure TGraphicLine.drawToCanvas(const axisConverterIn   : TDrawingAxisConverter;
                                                var canvasInOut         : TDirect2DCanvas       );
                var
                    pathGeometry : ID2D1PathGeometry;
                begin
                    if (length( geometryPoints ) < 2) then
                        exit();

                    pathGeometry := createOpenPathGeometry(
                                                                geometryPoints,
                                                                axisConverterIn
                                                          );

                    setLineProperties( canvasInOut );

                    canvasInOut.DrawGeometry( pathGeometry );
                end;

end.
