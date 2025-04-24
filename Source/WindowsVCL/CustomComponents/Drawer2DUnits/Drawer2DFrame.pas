unit Drawer2DFrame;

interface

    uses
        Vcl.Direct2D, Winapi.D2D1,
        Winapi.Windows, Winapi.Messages,
        System.SysUtils, System.Variants, System.Classes, system.Types, system.UITypes, system.UIConsts,
        Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.Themes,

        GraphicDrawerObjectAdderClass, Direct2DGraphicDrawingClass,
        Drawer2DTypes

        ;

    type
        TCustomDrawer2D = class(TFrame)
            PaintBoxDrawer2D: TPaintBox;
            //events
                procedure PaintBoxDrawer2DPaint(Sender: TObject);
                procedure PaintBoxGraphicMouseEnter(Sender: TObject);
                procedure PaintBoxGraphicMouseLeave(Sender: TObject);
                procedure FrameResize(Sender: TObject);
            private
                const
                    WM_USER_REDRAWGRAPHIC = WM_USER + 1;
                var
                    mustRedrawGraphic               : boolean;
                    graphicBackgroundColour         : TColor;
                    currentGraphicBuffer            : TBitmap;
                    D2DGraphicDrawer                : TDirect2DGraphicDrawer;
                    onGraphicUpdateGeometryEvent    : TGraphicUpdateGeometryEvent;
                //background colour
                    procedure setGraphicBackgroundColour();
                //mouse cursor
                    procedure setMouseCursor(const messageIn : TMessage);
                //update buffer
                    procedure updateGraphicBuffer();
            protected
                //drawing procedures
                    procedure preDrawGraphic(const canvasIn : TDirect2DCanvas); virtual;
                    procedure postDrawGraphic(const canvasIn : TDirect2DCanvas); virtual;
                //process windows messages
                    procedure wndProc(var messageInOut : TMessage); override;
            public
                //constructor
                    constructor create(AOwner : TComponent); override;
                //destructor
                    destructor destroy(); override;
                //accessors
                    function getOnGraphicUpdateGeometryEvent() : TGraphicUpdateGeometryEvent;
                //modifiers
                    procedure setOnGraphicUpdateGeometryEvent(const graphicDrawEventIn : TGraphicUpdateGeometryEvent);
                //redraw the graphic
                    procedure redrawGraphic();
                    procedure updateBackgroundColour();
                    procedure updateGeometry();
                //access graphic drawer
                    property GraphicDrawer : TDirect2DGraphicDrawer read D2DGraphicDrawer;
        end;

implementation

{$R *.dfm}

    //events
        procedure TCustomDrawer2D.PaintBoxDrawer2DPaint(Sender: TObject);
            begin
                //draw buffer to screen
                    PaintBoxDrawer2D.Canvas.Draw( 0, 0, currentGraphicBuffer );

                mustRedrawGraphic := False;
            end;

        procedure TCustomDrawer2D.PaintBoxGraphicMouseEnter(Sender: TObject);
            begin
                D2DGraphicDrawer.activateMouseControl();
            end;

        procedure TCustomDrawer2D.PaintBoxGraphicMouseLeave(Sender: TObject);
            begin
                D2DGraphicDrawer.deactivateMouseControl();
            end;

        procedure TCustomDrawer2D.FrameResize(Sender: TObject);
            begin
                redrawGraphic();
            end;

    //private
        //background colour
            procedure TCustomDrawer2D.setGraphicBackgroundColour();
                begin
                    //set the background colour according to the application theme
                        graphicBackgroundColour := TStyleManager.ActiveStyle.GetStyleColor( TStyleColor.scGenericBackground );
                end;

        //mouse cursor
            procedure TCustomDrawer2D.setMouseCursor(const messageIn : TMessage);
                begin
                    //if the graphic drawer is nil then nothing can happen
                        if ( NOT(Assigned(D2DGraphicDrawer)) ) then
                            exit();

                    //set the cursor based on the user input
                        if ( NOT(D2DGraphicDrawer.getMouseControlActive()) ) then
                            begin
                                PaintBoxDrawer2D.Cursor := crDefault;
                                exit();
                            end;

                        case (messageIn.Msg) of
                            WM_MBUTTONDOWN:
                                PaintBoxDrawer2D.Cursor := crSizeAll;
                            WM_MBUTTONUP:
                                PaintBoxDrawer2D.Cursor := crDefault;
                        end;
                end;

        //update buffer
            procedure TCustomDrawer2D.updateGraphicBuffer();
                var
                    D2DBufferCanvas : TDirect2DCanvas;
                begin
                    //create new D2D canvas for new drawing
                        currentGraphicBuffer.SetSize( PaintBoxDrawer2D.Width, PaintBoxDrawer2D.Height );

                        D2DBufferCanvas := TDirect2DCanvas.Create( currentGraphicBuffer.Canvas, Rect(0, 0, PaintBoxDrawer2D.Width, PaintBoxDrawer2D.Height) );

                        D2DBufferCanvas.RenderTarget.SetAntialiasMode( TD2D1AntiAliasMode.D2D1_ANTIALIAS_MODE_PER_PRIMITIVE );

                        D2DBufferCanvas.RenderTarget.SetTextAntialiasMode( TD2D1TextAntiAliasMode.D2D1_TEXT_ANTIALIAS_MODE_CLEARTYPE );

                    //draw to the D2D canvas
                        D2DBufferCanvas.BeginDraw();

                            //preDrawGraphic( D2DBufferCanvas );

                            D2DGraphicDrawer.drawAll(
                                                        PaintBoxDrawer2D.Width,
                                                        PaintBoxDrawer2D.Height,
                                                        graphicBackgroundColour,
                                                        D2DBufferCanvas
                                                    );

                            //postDrawGraphic( D2DBufferCanvas );

                        D2DBufferCanvas.EndDraw();

                        mustRedrawGraphic := True;

                    //free the D2D canvas
                        FreeAndNil( D2DBufferCanvas );
                end;

    //protected
        //drawing procedures
            procedure TCustomDrawer2D.preDrawGraphic(const canvasIn : TDirect2DCanvas);
                begin
                    //nothing here
                end;

            procedure TCustomDrawer2D.postDrawGraphic(const canvasIn : TDirect2DCanvas);
                begin
                    //nothing here
                end;

        //process windows messages
            procedure TCustomDrawer2D.wndProc(var messageInOut : TMessage);
                var
                    mouseInputRequiresRedraw        : boolean;
                    currentMousePositionOnPaintbox  : TPoint;
                begin
                    //drawing graphic-----------------------------------------------------------------------------------------------
                        //update the mouse position
                            if (messageInOut.Msg = WM_MOUSEMOVE) then
                                currentMousePositionOnPaintbox := PaintBoxDrawer2D.ScreenToClient( mouse.CursorPos );

                        //process windows message in axis converter
                            mouseInputRequiresRedraw := D2DGraphicDrawer.processWindowsMessages( messageInOut, currentMousePositionOnPaintbox );

                        //render image off screen
                            if ( mouseInputRequiresRedraw OR (messageInOut.Msg = WM_USER_REDRAWGRAPHIC) ) then
                                updateGraphicBuffer();

                        //paint rendered image to screen
                            if (mustRedrawGraphic) then
                                PaintBoxDrawer2D.Repaint();
                    //--------------------------------------------------------------------------------------------------------------

                    //set the cursor to drag or default
                        setMouseCursor( messageInOut );

                    inherited wndProc(messageInOut);
                end;

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

        //accessors
            function TCustomDrawer2D.getOnGraphicUpdateGeometryEvent() : TGraphicUpdateGeometryEvent;
                begin
                    result := onGraphicUpdateGeometryEvent;
                end;

        //modifiers
            procedure TCustomDrawer2D.setOnGraphicUpdateGeometryEvent(const graphicDrawEventIn : TGraphicUpdateGeometryEvent);
                begin
                    onGraphicUpdateGeometryEvent := graphicDrawEventIn;
                end;

        //redraw the graphic
            procedure TCustomDrawer2D.redrawGraphic();
                begin
                    //this message is sent to wndProc where the graphic is updated and redrawn
                        PostMessage( self.Handle, WM_USER_REDRAWGRAPHIC, 0, 0 );
                end;

            procedure TCustomDrawer2D.updateBackgroundColour();
                begin
                    setGraphicBackgroundColour();
                    redrawGraphic();
                end;

            procedure TCustomDrawer2D.updateGeometry();
                begin
                    setGraphicBackgroundColour();

                    //reset the stored geometry
                        D2DGraphicDrawer.resetDrawingGeometry();

                    //update the D2DGraphicDrawer geometry
                        if ( Assigned( onGraphicUpdateGeometryEvent ) ) then
                            onGraphicUpdateGeometryEvent( self, TGraphicDrawerObjectAdder( D2DGraphicDrawer ) );

                    //activate all drawing layers
                        D2DGraphicDrawer.activateAllDrawingLayers();

                    //send message to redraw
                        redrawGraphic();

                    //must ensure that geometry is updated to draw
                        Application.ProcessMessages();
                end;


end.
