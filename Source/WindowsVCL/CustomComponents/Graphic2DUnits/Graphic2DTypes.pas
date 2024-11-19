unit Graphic2DTypes;

interface

    uses
        system.SysUtils,
        Winapi.Messages,
        GeomDrawerBaseClass
        ;

    const
        WM_USER_REDRAWGRAPHIC = WM_USER + 1;

    type
        TGraphicUpdateGeometryEvent = procedure(ASender : TObject; var AGeomDrawer : TGeomDrawer) of object;

implementation

end.
