unit GraphXYComponent;

interface

    uses
        System.SysUtils, System.Classes,
        Vcl.Controls,
        CustomComponentPanelClass,
        GraphXYFrame;

    type
        TJDBGraphXY = class(TCustomComponentPanel)
            private
                customGraphXY : TCustomGraphXY;
            public
                constructor Create(AOwner: TComponent); override;
                destructor Destroy(); override;
        end;

implementation

    //public
        constructor TJDBGraphXY.Create(AOwner: TComponent);
            begin
                inherited create( AOwner );

                customGraphXY := TCustomGraphXY.create(Self);
                customGraphXY.parent    := self;
                customGraphXY.Align     := TAlign.alClient;
                customGraphXY.Visible   := True;
            end;

        destructor TJDBGraphXY.Destroy();
            begin
                FreeAndNil( customGraphXY );

                inherited Destroy();
            end;


end.
