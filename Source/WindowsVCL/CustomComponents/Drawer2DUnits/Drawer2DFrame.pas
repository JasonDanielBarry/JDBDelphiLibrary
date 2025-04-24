unit Drawer2DFrame;

interface

    uses
        Winapi.Windows, Winapi.Messages,
        System.SysUtils, System.Variants, System.Classes, system.Types, system.UITypes, system.UIConsts,
        Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls,

        GraphicDrawerObjectAdderClass, Direct2DGraphicDrawingClass,
        Drawer2DTypes

        ;

    type
        TCustomDrawer2D = class(TFrame)
            PaintBoxDrawer2D: TPaintBox;
            private
                var
                    mustRedrawGraphic               : boolean;
                    graphicBackgroundColour         : TColor;
                    currentGraphicBuffer            : TBitmap;
                    D2DGraphicDrawer                : TDirect2DGraphicDrawer;
                    onGraphicUpdateGeometryEvent    : TGraphicUpdateGeometryEvent;
            public
                //constructor
                    constructor create(AOwner : TComponent); override;
                //destructor
                    destructor destroy(); override;
        end;

implementation

{$R *.dfm}

    //public
        //constructor
            constructor TCustomDrawer2D.create(AOwner : TComponent);
                begin
                    inherited Create( AOwner );

                    //create required classes
                        currentGraphicBuffer    := TBitmap.create();
                        D2DGraphicDrawer        := TDirect2DGraphicDrawer.create();
                end;

        //destructor
            destructor TCustomDrawer2D.destroy();
                begin
                    //free classes
                        FreeAndNil( currentGraphicBuffer );
                        FreeAndNil( D2DGraphicDrawer );

                    inherited destroy();
                end;

end.
