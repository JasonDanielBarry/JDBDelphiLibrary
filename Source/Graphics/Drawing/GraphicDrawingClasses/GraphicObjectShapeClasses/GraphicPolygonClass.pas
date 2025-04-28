unit GraphicPolygonClass;

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
        TGraphicPolygon = class(TGraphicGeometry)
            //constructor
                constructor create( const   filledIn        : boolean;
                                    const   lineThicknessIn : integer;
                                    const   fillColourIn,
                                            lineColourIn    : TColor;
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
            constructor TGraphicPolygon.create( const   filledIn        : boolean;
                                                const   lineThicknessIn : integer;
                                                const   fillColourIn,
                                                        lineColourIn    : TColor;
                                                const   lineStyleIn     : TPenStyle;
                                                const   geometryIn      : TGeomBase );
                begin
                    inherited create(   filledIn,
                                        lineThicknessIn,
                                        fillColourIn,
                                        lineColourIn,
                                        lineStyleIn,
                                        geometryIn          );
                end;

        //destructor
            destructor TGraphicPolygon.destroy();
                begin
                    inherited destroy();
                end;

        //draw to canvas
            procedure TGraphicPolygon.drawToCanvas( const axisConverterIn   : TDrawingAxisConverter;
                                                    var canvasInOut         : TDirect2DCanvas       );
                var
                    pathGeometry : ID2D1PathGeometry;
                begin
                    if (length( geometryPoints ) < 3) then
                        exit();

                    //get path geometry
                        pathGeometry := createPathGeometry(
                                                                D2D1_FIGURE_BEGIN.D2D1_FIGURE_BEGIN_FILLED,
                                                                D2D1_FIGURE_END.D2D1_FIGURE_END_CLOSED,
                                                                axisConverterIn
                                                          );

                    //draw fill
                        if ( setFillProperties( canvasInOut ) ) then
                            canvasInOut.FillGeometry( pathGeometry );

                    //draw line
                        setLineProperties( canvasInOut );
                        canvasInOut.DrawGeometry( pathGeometry );
                end;

end.
