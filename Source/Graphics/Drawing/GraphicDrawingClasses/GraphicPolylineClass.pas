unit GraphicPolylineClass;

interface

    uses
        //Delphi
            system.SysUtils, system.types, system.UITypes, System.UIConsts,
            vcl.Graphics,
        //custom
            GraphicLineClass,
            GeometryBaseClass
            ;

    type
        TGraphicPolyline = class(TGraphicLine)
            //constructor
                constructor create( const   lineThicknessIn : integer;
                                    const   lineColourIn    : TColor;
                                    const   lineStyleIn     : TPenStyle;
                                    const   geometryIn      : TGeomBase );
            //destructor
                destructor destroy(); override;
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

end.
