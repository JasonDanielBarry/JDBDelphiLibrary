unit Graphic2DComponent;

interface

    uses
        System.SysUtils, System.Classes,
        Vcl.Controls,
        CustomComponentPanelClass,
        Drawer2DTypes,
        Graphic2DFrame;

    type
        TJDBGraphic2D = class(TCustomComponentPanel)
            private
                var
                    customGraphic2D : TCustomGraphic2D;
                procedure setOnGraphicUpdateGeometryEvent(const graphicDrawEventIn : TGraphicUpdateGeometryEvent);
                function getOnGraphicDrawEvent() : TGraphicUpdateGeometryEvent;
            public
                constructor Create(AOwner: TComponent); override;
                destructor Destroy(); override;
                procedure redrawGraphic();
                procedure updateBackgroundColour();
                procedure updateGeometry();
                procedure zoomAll();
            published
                property OnUpdateGeometry : TGraphicUpdateGeometryEvent read getOnGraphicDrawEvent write setOnGraphicUpdateGeometryEvent;
        end;

implementation

    //private
        procedure TJDBGraphic2D.setOnGraphicUpdateGeometryEvent(const graphicDrawEventIn : TGraphicUpdateGeometryEvent);
            begin
                customGraphic2D.setOnGraphicUpdateGeometryEvent(graphicDrawEventIn);
            end;

        function TJDBGraphic2D.getOnGraphicDrawEvent() : TGraphicUpdateGeometryEvent;
            begin
                result := customGraphic2D.getOnGraphicUpdateGeometryEvent();
            end;

    //public
        constructor TJDBGraphic2D.Create(AOwner: TComponent);
            begin
                inherited create( AOwner );

                customGraphic2D := TCustomGraphic2D.create(Self);
                customGraphic2D.parent  := self;
                customGraphic2D.Align   := TAlign.alClient;
                customGraphic2D.Visible := True;
            end;

        destructor TJDBGraphic2D.Destroy();
            begin
                FreeAndNil( customGraphic2D );

                inherited Destroy();
            end;

        procedure TJDBGraphic2D.redrawGraphic();
            begin
                customGraphic2D.redrawGraphic();
            end;

        procedure TJDBGraphic2D.updateBackgroundColour();
            begin
                customGraphic2D.updateBackgroundColour();
            end;

        procedure TJDBGraphic2D.updateGeometry();
            begin
                customGraphic2D.updateGeometry();
            end;

        procedure TJDBGraphic2D.zoomAll();
            begin
                customGraphic2D.zoomAll();
            end;

end.
