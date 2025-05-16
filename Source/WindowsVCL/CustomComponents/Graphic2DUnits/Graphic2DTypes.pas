unit Graphic2DTypes;

interface

    uses
        system.SysUtils,
        GraphicsListClass
        ;

    type
        TUpdateGraphicsEvent = procedure(ASender : TObject; var AGraphicsList : TGraphicsList) of object;

implementation

end.
