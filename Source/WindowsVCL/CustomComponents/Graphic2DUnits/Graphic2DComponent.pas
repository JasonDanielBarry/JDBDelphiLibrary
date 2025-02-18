unit Graphic2DComponent;

interface

    uses
        Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
        Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls,
        CustomComponentPanelClass,
        Graphic2DTypes,
        Graphic2DFrame;

    type
        TJDBGraphic2D = class(TCustomComponentPanel)
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
                procedure zoomAll();
            published
                property OnUpdateGeometry : TGraphicUpdateGeometryEvent read getOnGraphicDrawEvent write setOnGraphicUpdateGeometryEvent;
        end;

implementation

    //private
        procedure TJDBGraphic2D.setOnGraphicUpdateGeometryEvent(const graphicDrawEventIn : TGraphicUpdateGeometryEvent);
            begin
                customGraphic.setOnGraphicUpdateGeometryEvent(graphicDrawEventIn);
            end;

        function TJDBGraphic2D.getOnGraphicDrawEvent() : TGraphicUpdateGeometryEvent;
            begin
                result := customGraphic.getOnGraphicUpdateGeometryEvent();
            end;

    //public
        constructor TJDBGraphic2D.Create(AOwner: TComponent);
            begin
                inherited create( AOwner );

                customGraphic := TCustomGraphic2D.create(Self);
                customGraphic.parent := self;
                customGraphic.Align := TAlign.alClient;
                customGraphic.Visible := True;
            end;

        destructor TJDBGraphic2D.Destroy();
            begin
                FreeAndNil( customGraphic );

                inherited Destroy();
            end;

        procedure TJDBGraphic2D.redrawGraphic();
            begin
                customGraphic.redrawGraphic();
            end;

        procedure TJDBGraphic2D.updateGeometry();
            begin
                customGraphic.updateGeometry();
            end;

        procedure TJDBGraphic2D.zoomAll();
            begin
                customGraphic.zoomAll();
            end;

end.
