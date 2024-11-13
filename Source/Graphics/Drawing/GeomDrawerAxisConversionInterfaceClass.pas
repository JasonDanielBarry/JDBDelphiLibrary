unit GeomDrawerAxisConversionInterfaceClass;

interface

    uses
        system.SysUtils, system.math,
        Winapi.Messages,
        vcl.Controls,
        DrawingAxisConversionClass,
        GeomDrawerBaseClass
        ;

    type
        TGeomDrawerAxisConversionInterface = class(TGeomDrawerBase)
            private
                var
                    axisConverter : TDrawingAxisConverter;
            public
                //constructor
                    constructor create(const graphicControlComponentIn : TGraphicControl);
                //destructor
                    destructor destroy(); override;
                //process windows messages
                    function processWindowsMessages(var messageInOut : TMessage) : boolean;
        end;

implementation

    //public
        //constructor
            constructor TGeomDrawerAxisConversionInterface.create(const graphicControlComponentIn : TGraphicControl);
                begin
                    inherited create();

                    axisConverter := TDrawingAxisConverter.create( graphicControlComponentIn );
                end;

        //destructor
            destructor TGeomDrawerAxisConversionInterface.destroy();
                begin
                    FreeAndNil( axisConverter );

                    inherited destroy();
                end;

        //process windows messages
            function TGeomDrawerAxisConversionInterface.processWindowsMessages(var messageInOut : TMessage) : boolean;
                begin
                    result := axisConverter.processWindowsMessages( messageInOut );
                end;


end.
