unit CustomComponentRegistrationUnit;

interface

    uses
        system.SysUtils, System.Classes,
        Graphic2DComponent
        ;

    procedure register();

implementation

    procedure register();
        begin
            RegisterComponents('JDBDelphiLibrary', [TJDBGraphic2D]);
        end;

end.
