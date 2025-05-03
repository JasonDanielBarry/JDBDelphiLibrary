unit GraphicPolylineClass;

interface

    uses
        //Delphi
            system.SysUtils, system.types, system.UITypes, System.UIConsts,
            Winapi.D2D1, Vcl.Direct2D,
            vcl.Graphics,
        //custom
            GraphicGeometryClass,
            GeometryBaseClass,
            DrawingAxisConversionClass
            ;

    type
        TGraphicPolyline = class(TGraphicGeometry)
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
            constructor TGraphicPolyline.create(const   lineThicknessIn : integer;
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
            destructor TGraphicPolyline.destroy();
                begin
                    inherited destroy();
                end;

        //draw to canvas
            procedure TGraphicPolyline.drawToCanvas(const axisConverterIn   : TDrawingAxisConverter;
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
