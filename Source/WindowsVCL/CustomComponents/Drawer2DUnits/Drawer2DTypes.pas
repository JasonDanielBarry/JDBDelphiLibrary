unit Drawer2DTypes;

interface

    uses
        system.SysUtils,
        GraphicObjectListClass
        ;

    type
        TUpdateGraphicsEvent = procedure(ASender : TObject; var AGraphicList : TGraphicObjectList) of object;

implementation

end.
