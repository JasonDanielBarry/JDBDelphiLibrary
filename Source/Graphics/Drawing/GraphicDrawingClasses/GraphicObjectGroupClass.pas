unit GraphicObjectGroupClass;

interface

    uses
        Winapi.D2D1,
        system.SysUtils, system.UITypes,
        Vcl.Direct2D, vcl.Graphics,
        GeomBox,
        GraphicDrawingTypes,
        DrawingAxisConversionClass,
        GraphicObjectBaseClass
        ;

    type
        TGraphicObjectGroup = class(TGraphicObject)
            private
                arrGraphicObjects : TArray<TGraphicObject>;
            protected
                procedure addGraphicObject(const graphicObjectIn : TGraphicObject);
            public
                //constructor
                    constructor create();
                //destructor
                    destructor destroy(); override;
        end;

implementation

    //protected
        procedure TGraphicObjectGroup.addGraphicObject(const graphicObjectIn : TGraphicObject);
            var
                arrLen : integer;
            begin
                arrLen := length( arrGraphicObjects );

                SetLength( arrGraphicObjects, arrLen + 1 );

                arrGraphicObjects[ arrLen ] := graphicObjectIn;
            end;

    //public
        //constructor
            constructor TGraphicObjectGroup.create();
                begin
                    inherited create();

                    SetLength( arrGraphicObjects, 0 );
                end;

        //destructor
            destructor TGraphicObjectGroup.destroy();
                var
                    i, arrLen : integer;
                begin
                    arrLen := length( arrGraphicObjects );

                    for i := 0 to ( arrLen - 1 ) do
                        FreeAndNil( arrGraphicObjects[i] );

                    SetLength( arrGraphicObjects, 0 );

                    inherited destroy();
                end;


end.
