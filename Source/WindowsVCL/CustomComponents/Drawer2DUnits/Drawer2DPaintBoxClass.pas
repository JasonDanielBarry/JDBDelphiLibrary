unit Drawer2DPaintBoxClass;

interface

    uses
        Vcl.Direct2D, Winapi.D2D1,
        Winapi.Windows, Winapi.Messages,
        System.SysUtils, System.Classes,
        Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.ExtCtrls, Vcl.Themes,

        GraphicGridClass,
        GraphicObjectListBaseClass, Direct2DGraphicDrawingClass
        ;

    type
        TPaintBox = class(Vcl.ExtCtrls.TPaintBox)
            private
                const
                    WM_USER_REDRAWGRAPHIC = WM_USER + 1;
                var
                    mustRedrawGraphic       : boolean;
                    gridVisibilitySettings  : TGridVisibilitySettings;
                    graphicBackgroundColour : TColor;
                    currentGraphicBuffer    : TBitmap;
                    D2DGraphicDrawer        : TDirect2DGraphicDrawer;
                //events
                    procedure PaintBoxDrawer2DPaint(Sender: TObject);
                    procedure PaintBoxGraphicMouseEnter(Sender: TObject);
                    procedure PaintBoxGraphicMouseLeave(Sender: TObject);
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
            public
                //constructor
                    constructor create(AOwner : TComponent); override;
                //destructor
                    destructor destroy(); override;
                //modifiers
                    procedure setGridEnabled(const enabledIn : boolean);
                    procedure setGridElementsVisiblity(const gridVisibilitySettingsIn : TGridVisibilitySettings);
                //redraw the graphic
                    procedure postRedrawGraphicMessage(const callingControlIn : TWinControl);
                    procedure updateBackgroundColour(const callingControlIn : TWinControl);
                    procedure updateGraphics(   const callingControlIn      : TWinControl;
                                                const graphicObjectListIn   : TGraphicObjectListBase);
                //process windows messages
                    procedure processWindowsMessages(var messageInOut : TMessage; out graphicWasRedrawnOut : boolean);
                //access graphic drawer
                    property GraphicDrawer : TDirect2DGraphicDrawer read D2DGraphicDrawer;
        end;

implementation

    //private
        //events
            procedure TPaintBox.PaintBoxDrawer2DPaint(Sender: TObject);
                begin
                    //draw buffer to screen
                        self.Canvas.Draw( 0, 0, currentGraphicBuffer );

                    mustRedrawGraphic := False;
                end;

            procedure TPaintBox.PaintBoxGraphicMouseEnter(Sender: TObject);
                begin
                    D2DGraphicDrawer.activateMouseControl();
                end;

            procedure TPaintBox.PaintBoxGraphicMouseLeave(Sender: TObject);
                begin
                    D2DGraphicDrawer.deactivateMouseControl();
                end;

        //background colour
            procedure TPaintBox.setGraphicBackgroundColour();
                begin
                    //set the background colour according to the application theme
                        graphicBackgroundColour := TStyleManager.ActiveStyle.GetStyleColor( TStyleColor.scGenericBackground );
                end;

        //mouse cursor
            procedure TPaintBox.setMouseCursor(const messageIn : TMessage);
                begin
                    //if the graphic drawer is nil then nothing can happen
                        if NOT( Assigned(D2DGraphicDrawer) ) then
                            exit();

                    //set the cursor based on the user input
                        if NOT(D2DGraphicDrawer.getMouseControlActive() ) then
                            begin
                                self.Cursor := crDefault;
                                exit();
                            end;

                        case (messageIn.Msg) of
                            WM_MBUTTONDOWN:
                                self.Cursor := crSizeAll;

                            WM_MBUTTONUP:
                                self.Cursor := crDefault;
                        end;
                end;

        //update buffer
            procedure TPaintBox.updateGraphicBuffer();
                var
                    D2DBufferCanvas : TDirect2DCanvas;
                begin
                    //create new D2D canvas for new drawing
                        currentGraphicBuffer.SetSize( self.Width, self.Height );

                        D2DBufferCanvas := TDirect2DCanvas.Create( currentGraphicBuffer.Canvas, Rect(0, 0, self.Width, self.Height) );

                        D2DBufferCanvas.RenderTarget.SetAntialiasMode( TD2D1AntiAliasMode.D2D1_ANTIALIAS_MODE_PER_PRIMITIVE );

                        D2DBufferCanvas.RenderTarget.SetTextAntialiasMode( TD2D1TextAntiAliasMode.D2D1_TEXT_ANTIALIAS_MODE_CLEARTYPE );

                    //draw to the D2D canvas
                        D2DBufferCanvas.BeginDraw();

                            //preDrawGraphic( D2DBufferCanvas );

                            D2DGraphicDrawer.drawAll(
                                                        self.Width,
                                                        self.Height,
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
            procedure TPaintBox.preDrawGraphic(const canvasIn : TDirect2DCanvas);
                begin
                    //nothing here
                end;

            procedure TPaintBox.postDrawGraphic(const canvasIn : TDirect2DCanvas);
                begin
                    //nothing here
                end;

    //public
        //constructor
            constructor TPaintBox.create(AOwner : TComponent);
                begin
                    inherited Create( AOwner );

                    //create required classes
                        currentGraphicBuffer    := TBitmap.create();
                        D2DGraphicDrawer        := TDirect2DGraphicDrawer.create();

                    //assign events
                        self.OnPaint        := PaintBoxDrawer2DPaint;
                        self.OnMouseEnter   := PaintBoxGraphicMouseEnter;
                        self.OnMouseLeave   := PaintBoxGraphicMouseLeave;

                    //for design time to ensure the colour is not black on the form builder
                        setGraphicBackgroundColour();

                    //grid is not visible by default
                        setGridEnabled( False );
                end;

        //destructor
            destructor TPaintBox.destroy();
                begin
                    //free classes
                        FreeAndNil( currentGraphicBuffer );
                        FreeAndNil( D2DGraphicDrawer );

                    inherited destroy();
                end;

        //modifiers
            procedure TPaintBox.setGridEnabled(const enabledIn : boolean);
                begin
                    GraphicDrawer.setGridEnabled( enabledIn );
                end;

            procedure TPaintBox.setGridElementsVisiblity(const gridVisibilitySettingsIn : TGridVisibilitySettings);
                begin
                    gridVisibilitySettings.copyOther( gridVisibilitySettingsIn );
                end;

        //redraw the graphic
            procedure TPaintBox.postRedrawGraphicMessage(const callingControlIn : TWinControl);
                begin
                    //this message is sent to callingControlIn.wndProc() where the graphic is updated and redrawn
                    //the self.processWindowsMessages() method must be called in callingControlIn.wndProc()
                        PostMessage( callingControlIn.Handle, WM_USER_REDRAWGRAPHIC, 0, 0 );
                end;

            procedure TPaintBox.updateBackgroundColour(const callingControlIn : TWinControl);
                begin
                    setGraphicBackgroundColour();
                    postRedrawGraphicMessage( callingControlIn );
                end;

            procedure TPaintBox.updateGraphics( const callingControlIn      : TWinControl;
                                                const graphicObjectListIn   : TGraphicObjectListBase);
                begin
                    //set background to match theme
                        setGraphicBackgroundColour();

                    //reset the stored graphics
                        D2DGraphicDrawer.clearGraphicObjects();

                    //update the D2DGraphicDrawer graphics
                        D2DGraphicDrawer.setGridElementsVisiblity( gridVisibilitySettings );

                        D2DGraphicDrawer.readGraphicObjectList( graphicObjectListIn );

                    //activate all drawing layers
                        D2DGraphicDrawer.activateAllDrawingLayers();

                    //send message to redraw
                        postRedrawGraphicMessage( callingControlIn );
                end;

        //process windows messages
            procedure TPaintBox.processWindowsMessages(var messageInOut : TMessage; out graphicWasRedrawnOut : boolean);
                var
                    mouseInputRequiresRedraw        : boolean;
                    currentMousePositionOnPaintbox  : TPoint;
                begin
                    //drawing graphic-----------------------------------------------------------------------------------------------
                        //update the mouse position
                            if (messageInOut.Msg = WM_MOUSEMOVE) then
                                currentMousePositionOnPaintbox := self.ScreenToClient( mouse.CursorPos );

                        //process windows message in axis converter
                            mouseInputRequiresRedraw := D2DGraphicDrawer.windowsMessageRequiredRedraw( messageInOut, currentMousePositionOnPaintbox );

                        //render image off screen
                            if ( mouseInputRequiresRedraw OR (messageInOut.Msg = WM_USER_REDRAWGRAPHIC) ) then
                                updateGraphicBuffer();

                        //paint rendered image to screen
                            if (mustRedrawGraphic) then
                                begin
                                    self.Repaint();
                                    graphicWasRedrawnOut := True;
                                end
                            else
                                graphicWasRedrawnOut := False;
                    //--------------------------------------------------------------------------------------------------------------

                    //set the cursor to drag or default
                        setMouseCursor( messageInOut );
                end;

end.
