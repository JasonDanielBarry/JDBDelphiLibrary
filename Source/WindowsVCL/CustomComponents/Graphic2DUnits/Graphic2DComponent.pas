unit Graphic2DComponent;

interface

    uses
        Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
        Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls,
        CustomComponentPanelClass,
        Graphic2DTypes,
        Graphic2DFrame;

    type
        TGraphic2D = class(TCustomComponentPanel)
            private
                var
                    customGraphic : TCustomGraphic2D;
                procedure setOnGraphicUpdateGeometryEvent(const graphicDrawEventIn : TGraphicUpdateGeometryEvent);
                function getOnGraphicDrawEvent() : TGraphicUpdateGeometryEvent;
            public
                constructor Create(AOwner: TComponent); override;
                destructor Destroy; override;
                procedure redrawGraphic();
                procedure updateGeometry();
            published
                property OnUpdateGeometry : TGraphicUpdateGeometryEvent read getOnGraphicDrawEvent write setOnGraphicUpdateGeometryEvent;
        end;

implementation

    //private
        procedure TGraphic2D.setOnGraphicUpdateGeometryEvent(const graphicDrawEventIn : TGraphicUpdateGeometryEvent);
            begin
                customGraphic.setOnGraphicUpdateGeometryEvent(graphicDrawEventIn);
            end;

        function TGraphic2D.getOnGraphicDrawEvent() : TGraphicUpdateGeometryEvent;
            begin
                result := customGraphic.getOnGraphicUpdateGeometryEvent();
            end;

    //public
        constructor TGraphic2D.Create(AOwner: TComponent);
            begin
                inherited create(AOwner);

                customGraphic := TCustomGraphic2D.create(Self);
                customGraphic.parent := self;
                customGraphic.Align := TAlign.alClient;
                customGraphic.Visible := True;

                updateGeometry();
                customGraphic.zoomAll();
            end;

        destructor TGraphic2D.Destroy();
            begin
                FreeAndNil( customGraphic );

                inherited Destroy();
            end;

        procedure TGraphic2D.redrawGraphic();
            begin
                customGraphic.redrawGraphic();
            end;

        procedure TGraphic2D.updateGeometry();
            begin
                customGraphic.updateGeometry();
            end;

end.
