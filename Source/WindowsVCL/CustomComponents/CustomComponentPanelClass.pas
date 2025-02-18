unit CustomComponentPanelClass;

interface

    uses
        Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
        Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls;

    type
        TCustomComponentPanel = class(TPanel)
            public
                constructor Create(AOwner: TComponent); override;
                destructor Destroy; override;
        end;

implementation

    //public
        constructor TCustomComponentPanel.Create(AOwner: TComponent);
            begin
                inherited create(AOwner);

                //disable accepting new controls
                    self.ControlStyle := self.ControlStyle - [csAcceptsControls];

                //border
                    self.BorderStyle    := bsNone;
                    self.BevelKind      := bkNone;
                    self.BevelInner     := bvNone;
                    self.BevelOuter     := bvNone;
                    self.BevelEdges     := [];
                    self.ShowCaption    := False;

                //parent control
                    self.ParentBackground       := True;
                    self.ParentBiDiMode         := True;
                    self.ParentColor            := True;
                    self.ParentCtl3D            := True;
                    self.ParentCustomHint       := True;
                    self.ParentDoubleBuffered   := True;
                    self.ParentFont             := True;

                    self.ShowHint := True;

                //style
                    self.StyleElements := [seFont,seClient,seBorder];
            end;

        destructor TCustomComponentPanel.Destroy();
            begin
                inherited Destroy();
            end;

end.
