unit Graphic2DTypes;

interface

    uses
        system.SysUtils,
        Winapi.Messages,
        SkiaDrawingClass
        ;

    const
        WM_USER_REDRAWGRAPHIC = WM_USER + 1;

    type
        TGraphicUpdateGeometryEvent = procedure(ASender : TObject; var ASkiaDrawer : TSkiaGeomDrawer) of object;

implementation

end.
