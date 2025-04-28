unit GraphXYFrame;

interface

uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
    Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls,

    Drawer2DPaintBoxClass;

    type
        TCustomGraphXY = class(TFrame)
            PBGraphXY: TPaintBox;
            private

            protected
                //process windows messages
                    procedure wndProc(var messageInOut : TMessage); override;
            public
                //constructor
                    constructor create(AOwner : TComponent); override;
                //destructor
                    destructor destroy(); override;
                //replot graphs
                    procedure replot();

        end;

implementation

{$R *.dfm}

    //protected
        //process windows messages
            procedure TCustomGraphXY.wndProc(var messageInOut : TMessage);
                var
                    mouseInputRequiresRedraw        : boolean;
                    currentMousePositionOnPaintbox  : TPoint;
                begin
                    if ( Assigned( PBGraphXY ) ) then
                        begin
                            //drawing messages
                                PBGraphXY.processWindowsMessages( messageInOut );

                            //more messages
                                //
                        end;

                    inherited wndProc(messageInOut);
                end;

    //public
        //constructor
            constructor TCustomGraphXY.create(AOwner : TComponent);
                begin
                    inherited Create( AOwner );

                    PBGraphXY.setCallingControl( self );
                end;

        //destructor
            destructor TCustomGraphXY.destroy();
                begin
                    inherited destroy();
                end;

        //replot graphs
            procedure TCustomGraphXY.replot();
                begin

                end;

end.
