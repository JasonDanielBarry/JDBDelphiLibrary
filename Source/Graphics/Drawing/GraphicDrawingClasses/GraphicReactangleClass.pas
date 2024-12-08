unit GraphicReactangleClass;

interface

    uses
        Winapi.D2D1,
        system.Types,
        Vcl.Direct2D, vcl.Graphics,
        GeometryTypes,
        GeomBox,
        GraphicDrawingTypes,
        GraphicObjectBaseClass,
        DrawingAxisConversionClass
        ;

    type
        TGraphicReactangle = class(TGraphicObject)
            private
                dimensions  : TSizeF;
                centrePoint : TGeomPoint; asdf
        end;

implementation

end.
