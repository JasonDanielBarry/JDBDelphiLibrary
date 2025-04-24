unit CustomComponentRegistrationUnit;

interface

    uses
        system.SysUtils, System.Classes,
        Drawer2DPaintBoxClass,
        Graphic2DComponent
        ;

    procedure register();

implementation

    procedure register();
        begin
            RegisterComponents('JDBDelphiLibrary', [TJDBGraphic2D]);
            RegisterComponents('JDBDelphiLibrary', [TJDBDrawer2DPaintBox]);
        end;

end.
