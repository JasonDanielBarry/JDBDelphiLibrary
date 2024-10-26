unit DrawingAxisConversionClass;

interface

    uses
        System.SysUtils, system.Math, system.Types,
        GeometryTypes,
        DrawingAxisConversionPanningClass;

    type
        TDrawingAxisConverter = class(TDrawingAxisPanningConverter)
            private
                //zooming methods
            public
                //constructor
                    constructor create();
                //destructor
                    destructor destroy(); override;

        end;

implementation

    //public
        //constructor
            constructor TDrawingAxisConverter.create();
                begin
                    inherited create();
                end;

        //destructor
            destructor TDrawingAxisConverter.destroy();
                begin
                    inherited destroy();
                end;



        

end.
