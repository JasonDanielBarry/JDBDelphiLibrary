unit Drawer2DTypes;

interface

    uses
        system.SysUtils,
        GraphicDrawerObjectAdderClass
        ;

    type
        TGraphicUpdateGeometryEvent = procedure(ASender : TObject; var AGeomDrawer : TGraphicDrawerObjectAdder) of object;

implementation

end.
