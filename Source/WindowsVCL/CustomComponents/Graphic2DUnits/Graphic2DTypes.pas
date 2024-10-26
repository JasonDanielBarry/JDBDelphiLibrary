unit Graphic2DTypes;

interface

    uses
        system.SysUtils,
        SkiaDrawingClass
        ;

    type
        TGraphicUpdateGeometryEvent = procedure(ASender : TObject; var ASkiaDrawer : TSkiaGeomDrawer) of object;

implementation

end.
