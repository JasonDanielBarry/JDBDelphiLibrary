unit CustomComponentRegistrationUnit;

interface

    uses
        system.SysUtils, System.Classes,

        Graphic2DComponent, GraphXYComponent, CustomStringGridClass
        ;

    procedure register();

implementation

    procedure register();
        begin
            RegisterComponents(
                                    'JDBDelphiLibrary',

                                    [
                                        TJDBGraphic2D,
                                        TJDBGraphXY,
                                        TJDBStringGrid
                                    ]
                              );
        end;

end.
