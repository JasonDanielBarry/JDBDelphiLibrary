unit Graphic2DTypes;

interface

    uses
        system.SysUtils,
        Winapi.Messages,
        GraphicDrawerBaseClass
        ;

    const
        WM_USER_REDRAWGRAPHIC = WM_USER + 1;

    type
        TGraphicUpdateGeometryEvent = procedure(ASender : TObject; var AGeomDrawer : TGraphicDrawer) of object;

implementation

end.
