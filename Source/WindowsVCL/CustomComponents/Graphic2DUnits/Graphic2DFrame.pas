unit Graphic2DFrame;

interface

    uses
        Winapi.Windows, Winapi.Messages,
        System.SysUtils, System.Variants, System.Classes, system.Types, system.UITypes,
        system.UIConsts, system.Threading, system.Math, system.Diagnostics, System.Actions,
        Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.Skia,
        Vcl.Buttons, Vcl.ExtCtrls, Vcl.Skia, Vcl.StdCtrls, Vcl.ActnList, Vcl.Menus, vcl.Themes,
        GeneralComponentHelperMethods,
        ColourMethods,
        GeometryTypes, GeomBox,
        SkiaDrawingClass,
        Graphic2DTypes
        ;

    type
        TCustomGraphic2D = class(TFrame)
            SkPaintBoxGraphic: TSkPaintBox;
            SpeedButtonZoomIn: TSpeedButton;
            SpeedButtonZoomOut: TSpeedButton;
            SpeedButtonZoomExtents: TSpeedButton;
            SpeedButtonShiftLeft: TSpeedButton;
            SpeedButtonShiftRight: TSpeedButton;
            SpeedButtonShiftUp: TSpeedButton;
            SpeedButtonShiftDown: TSpeedButton;
            SpeedButtonUpdateGeometry: TSpeedButton;
            ComboBoxZoomPercent: TComboBox;
            GridPanelDirectionalPan: TGridPanel;
            PanelGraphicControls: TPanel;
            SpeedButtonCentre: TSpeedButton;
            labelCoords: TLabel;
            ActionListControls: TActionList;
            ActionZoomIn: TAction;
            ActionZoomOut: TAction;
            ActionZoomExtents: TAction;
            ActionRecentre: TAction;
            ActionUpdateGeometry: TAction;
            ActionPanLeft: TAction;
            ActionPanRight: TAction;
            ActionPanUp: TAction;
            ActionPanDown: TAction;
            PopupMenuGraphicControls: TPopupMenu;
            ZoomExtents1: TMenuItem;
            ZoomIn1: TMenuItem;
            ZoomOut1: TMenuItem;
            Recentre1: TMenuItem;
            N1: TMenuItem;
            UpdateGeometry1: TMenuItem;
            N2: TMenuItem;
            PanLeft1: TMenuItem;
            PanRight1: TMenuItem;
            PanDown1: TMenuItem;
            PanRight2: TMenuItem;
            GridPanelAxisOptions: TGridPanel;
            LabelXAxis: TLabel;
            EditXMin: TEdit;
            LabelXBounds: TLabel;
            EditXMax: TEdit;
            LabelYAxis: TLabel;
            EditYMin: TEdit;
            LabelYBounds: TLabel;
            EditYMax: TEdit;
            SpeedButtonAxisSettings: TSpeedButton;
            ActionEditAxes: TAction;
            N3: TMenuItem;
            EditAxes1: TMenuItem;
            //events
                procedure SkPaintBoxGraphicDraw(ASender         : TObject;
                                                const ACanvas   : ISkCanvas;
                                                const ADest     : TRectF;
                                                const AOpacity  : Single    );
                procedure ComboBoxZoomPercentChange(Sender: TObject);
                procedure SkPaintBoxGraphicMouseEnter(Sender: TObject);
                procedure SkPaintBoxGraphicMouseLeave(Sender: TObject);
                procedure FrameResize(Sender: TObject);
                procedure ActionRecentreExecute(Sender: TObject);
                procedure ActionZoomExtentsExecute(Sender: TObject);
                procedure ActionZoomInExecute(Sender: TObject);
                procedure ActionZoomOutExecute(Sender: TObject);
                procedure ActionUpdateGeometryExecute(Sender: TObject);
                procedure ActionPanLeftExecute(Sender: TObject);
                procedure ActionPanRightExecute(Sender: TObject);
                procedure ActionPanUpExecute(Sender: TObject);
                procedure ActionPanDownExecute(Sender: TObject);
                procedure ActionEditAxesExecute(Sender: TObject);
                procedure EditAxisValueKeyPress(Sender: TObject; var Key: Char);
            private
                var
                    axisSettingsVisible,
                    mustRedrawGraphic               : boolean;
                    currentGraphicImage             : ISkImage;
                    graphicBackgroundColour         : TAlphaColor;
                    skiaGeomDrawer                  : TSkiaGeomDrawer;
                    onGraphicUpdateGeometryEvent    : TGraphicUpdateGeometryEvent;
                //axis Settings
                    procedure updateAxisSettingsValues();
                    procedure writeAxisSettingsValuesToAxisConverter();
                //background colour
                    procedure setGraphicBackgroundColour();
                //mouse methods
                    procedure updateMouseCoordinates();
                    procedure setMouseCursor(const messageIn : TMessage);
                //zooming methods
                    procedure updateZoomPercentage();
            protected
                //drawing procedures
                    procedure preDrawGraphic(); virtual;
                    procedure postDrawGraphic(const canvasIn : ISkCanvas); virtual;
                    function updateGraphicImage() : ISkImage;
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
                    procedure updateGeometry();
                //zooming methods
                    procedure zoomAll();
        end;

implementation

{$R *.dfm}

    //events
        procedure TCustomGraphic2D.SkPaintBoxGraphicDraw(   ASender         : TObject;
                                                            const ACanvas   : ISkCanvas;
                                                            const ADest     : TRectF;
                                                            const AOpacity  : Single    );
            begin
                ACanvas.DrawImage( currentGraphicImage, 0, 0 );

                mustRedrawGraphic := False;
            end;

        procedure TCustomGraphic2D.SkPaintBoxGraphicMouseEnter(Sender: TObject);
            begin
                skiaGeomDrawer.activateMouseControl();
            end;

        procedure TCustomGraphic2D.SkPaintBoxGraphicMouseLeave(Sender: TObject);
            begin
                skiaGeomDrawer.deactivateMouseControl();
            end;

        procedure TCustomGraphic2D.ComboBoxZoomPercentChange(Sender: TObject);
            var
                newZoomPercent : double;
            begin
                try
                    newZoomPercent := StrToFloat( ComboBoxZoomPercent.Text );
                except
                    newZoomPercent := 1;
                end;

                skiaGeomDrawer.setZoom( newZoomPercent );

                redrawGraphic();
            end;

        procedure TCustomGraphic2D.FrameResize(Sender: TObject);
            begin
                redrawGraphic();
            end;

        procedure TCustomGraphic2D.ActionEditAxesExecute(Sender: TObject);
            begin
                axisSettingsVisible := NOT(axisSettingsVisible);

                EditAxes1.Checked := axisSettingsVisible;

                if (axisSettingsVisible) then
                    setSpeedButtonDown(1, SpeedButtonAxisSettings)
                else
                    SpeedButtonAxisSettings.Down := False;

                if (axisSettingsVisible) then
                    begin
                        GridPanelAxisOptions.Left   := SpeedButtonAxisSettings.Left;
                        GridPanelAxisOptions.Top    := SkPaintBoxGraphic.top + 1;

                        GridPanelAxisOptions.Visible := True;
                        GridPanelAxisOptions.BringToFront;
                    end
                else
                    GridPanelAxisOptions.Visible := False;
            end;

        procedure TCustomGraphic2D.ActionPanDownExecute(Sender: TObject);
            begin
                skiaGeomDrawer.shiftRange( 10 );

                redrawGraphic();
            end;

        procedure TCustomGraphic2D.ActionPanLeftExecute(Sender: TObject);
            begin
                skiaGeomDrawer.shiftDomain( 10 );

                redrawGraphic();
            end;

        procedure TCustomGraphic2D.ActionPanRightExecute(Sender: TObject);
            begin
                skiaGeomDrawer.shiftDomain( -10 );

                redrawGraphic();
            end;

        procedure TCustomGraphic2D.ActionPanUpExecute(Sender: TObject);
            begin
                skiaGeomDrawer.shiftRange( -10 );

                redrawGraphic();
            end;

        procedure TCustomGraphic2D.ActionRecentreExecute(Sender: TObject);
            begin
                skiaGeomDrawer.recentre();

                redrawGraphic();
            end;

        procedure TCustomGraphic2D.ActionUpdateGeometryExecute(Sender: TObject);
            begin
                updateGeometry();
            end;

        procedure TCustomGraphic2D.ActionZoomExtentsExecute(Sender: TObject);
            begin
                zoomAll();
            end;

        procedure TCustomGraphic2D.ActionZoomInExecute(Sender: TObject);
            begin
                skiaGeomDrawer.zoomIn(10);

                redrawGraphic();
            end;

        procedure TCustomGraphic2D.ActionZoomOutExecute(Sender: TObject);
            begin
                skiaGeomDrawer.zoomOut(10);

                redrawGraphic();
            end;

        procedure TCustomGraphic2D.EditAxisValueKeyPress(   Sender  : TObject;
                                                            var Key : Char      );
            begin
                if ( integer(key) = VK_RETURN ) then
                    writeAxisSettingsValuesToAxisConverter();
            end;

    //private
        //axis Settings
            procedure TCustomGraphic2D.updateAxisSettingsValues();
                var
                    xMin, xMax,
                    yMin, yMax      : double;
                    drawingRegion   : TGeomBox;
                begin
                    drawingRegion := skiaGeomDrawer.getDrawingRegion();

                    xMin := drawingRegion.minPoint.x;
                    xMax := drawingRegion.maxPoint.x;
                    yMin := drawingRegion.minPoint.y;
                    yMax := drawingRegion.maxPoint.y;

                    EditXMin.Text := FloatToStrF(xMin, ffFixed, 5, 2);
                    EditXMax.Text := FloatToStrF(xMax, ffFixed, 5, 2);
                    EditYMin.Text := FloatToStrF(yMin, ffFixed, 5, 2);
                    EditYMax.Text := FloatToStrF(yMax, ffFixed, 5, 2);
                end;

            procedure TCustomGraphic2D.writeAxisSettingsValuesToAxisConverter();
                var
                    validXmin, validXMax,
                    validYMin, validYMax,
                    validValues             : boolean;
                    newDrawingRegion        : TGeomBox;
                begin
                    //check for valid values
                        validXmin := TryStrToFloat( EditXMin.Text, newDrawingRegion.minPoint.x );
                        validXMax := TryStrToFloat( EditXMax.Text, newDrawingRegion.maxPoint.x );

                        validYmin := TryStrToFloat( EditYMin.Text, newDrawingRegion.minPoint.y );
                        validYMax := TryStrToFloat( EditYMax.Text, newDrawingRegion.maxPoint.y );

                        validValues := (validXmin AND validXMax AND validYMin AND validYMax);

                        if ( NOT(validValues) ) then
                            exit();

                    //write new drawing region to axis converter
                        newDrawingRegion.minPoint.z := 0;
                        newDrawingRegion.maxPoint.z := 0;

                        skiaGeomDrawer.setDrawingRegion(0, newDrawingRegion);

                    redrawGraphic();
                end;

        //background colour
            procedure TCustomGraphic2D.setGraphicBackgroundColour();
                var
                    themeColour : TColor;
                begin
                    //get the colour of the parent and convert it to an alpha colour
                        themeColour := TStyleManager.ActiveStyle.GetStyleColor(TStyleColor.scPanel);

                        graphicBackgroundColour := colourToAlphaColour( themeColour );
                end;

        //mouse methods
            procedure TCustomGraphic2D.updateMouseCoordinates();
                var
                    mouseCoordStr   : string;
                    mousePointXY    : TGeomPoint;
                begin
                    if (NOT( skiaGeomDrawer.getMouseControlActive() )) then
                        exit();

                    //convert mouse position to XY coordinate
                        mousePointXY := skiaGeomDrawer.getMouseCoordinatesXY();

                        mouseCoordStr := '(' + FloatToStrF(mousePointXY.x, ffFixed, 5, 2) + ', ' + FloatToStrF(mousePointXY.x, ffFixed, 5, 2) + ')';

                    //write to label
                        labelCoords.Caption := mouseCoordStr;
                end;

            procedure TCustomGraphic2D.setMouseCursor(const messageIn : TMessage);
                begin
                    try
                        if NOT(skiaGeomDrawer.getMouseControlActive()) then
                            begin
                                SkPaintBoxGraphic.Cursor := crDefault;
                                exit();
                            end;

                        case (messageIn.Msg) of
                            WM_MBUTTONDOWN:
                                SkPaintBoxGraphic.Cursor := crSizeAll;
                            WM_MBUTTONUP:
                                SkPaintBoxGraphic.Cursor := crDefault;
                        end;
                    except

                    end;
                end;

        //zooming methods
            procedure TCustomGraphic2D.updateZoomPercentage();
                var
                    currentZoomPercentage : double;
                begin
                    currentZoomPercentage := skiaGeomDrawer.getCurrentZoomPercentage();
                    ComboBoxZoomPercent.Text := FloatToStrF( currentZoomPercentage, ffNumber, 5, 0 );
                end;

    //protected
        //drawing procedures
            procedure TCustomGraphic2D.preDrawGraphic();
                begin
                    //make sure canvas is the same colour as the parent
                        skiaGeomDrawer.setDrawingBackgroundColour( graphicBackgroundColour );
                end;

            procedure TCustomGraphic2D.postDrawGraphic(const canvasIn : ISkCanvas);
                var
                    paint : ISkPaint;
                begin
                    //draw a border around the paintbox edge
                        paint               := TSkPaint.Create( TSkPaintStyle.Stroke );
                        paint.StrokeWidth   := 1;
                        paint.Color         := TAlphaColors.Silver;

                        canvasIn.DrawRect(
                                            RectF(0, 0, SkPaintBoxGraphic.Width - 1, SkPaintBoxGraphic.Height - 1),
                                            paint
                                         );
                end;

            function TCustomGraphic2D.updateGraphicImage() : ISkImage;
                var
                    surface : ISkSurface;
                begin
                    //draw to the surface
                        preDrawGraphic();

                        surface := skiaGeomDrawer.drawAllGeometryToSurface( SkPaintBoxGraphic.Height,
                                                                            SkPaintBoxGraphic.Width );

                        postDrawGraphic( surface.Canvas );

                    //update relevate properties
                        updateZoomPercentage();

                        updateAxisSettingsValues();

                        mustRedrawGraphic := True;

                    //export to image
                        result := surface.MakeImageSnapshot();
                end;

        //process windows messages
            procedure TCustomGraphic2D.wndProc(var messageInOut : TMessage);
                var
                    mouseInputRequiresRedraw,
                    mustUpdateGraphicImage      : boolean;
                    currentMousePosition        : TPoint;
                begin
                    //update the mouse position
                        if (messageInOut.Msg = WM_MOUSEMOVE) then
                            currentMousePosition := SkPaintBoxGraphic.ScreenToClient( mouse.CursorPos );

                    //process windows message in axis converter
                        mouseInputRequiresRedraw := skiaGeomDrawer.processWindowsMessages( messageInOut, currentMousePosition );

                    //determine if redrawing is required
                        mustUpdateGraphicImage := ( mouseInputRequiresRedraw OR (messageInOut.Msg = WM_USER_REDRAWGRAPHIC) );

                    //render image off screen
                        if (mustUpdateGraphicImage) then
                            currentGraphicImage := updateGraphicImage();

                    //paint rendered image to screen
                        if (mustRedrawGraphic) then
                            SkPaintBoxGraphic.Redraw();

                    //set the cursor to drag or default
                        setMouseCursor( messageInOut );

                    //update mouse XY coordinates
                        if (messageInOut.Msg = WM_MOUSEMOVE) then
                            updateMouseCoordinates();

                    inherited wndProc(messageInOut);
                end;

    //public
        //constructor
            constructor TCustomGraphic2D.create(AOwner : TComponent);
                begin
                    inherited create(AOwner);

                    //create required classes
                        skiaGeomDrawer := TSkiaGeomDrawer.create();

                    //set up graphic controls
                        //coordinates label
                            labelCoords.Left := labelCoords.Height div 2;
                            labelCoords.top := PanelGraphicControls.Height + SkPaintBoxGraphic.Height - 3 * labelCoords.Height div 2;

                        //direction pan
                            GridPanelDirectionalPan.Left := PanelGraphicControls.Width - GridPanelDirectionalPan.Width - 1;
                            GridPanelDirectionalPan.top := PanelGraphicControls.Height + 1;
                            GridPanelDirectionalPan.BringToFront();

                        //axis settings
                            axisSettingsVisible := False;
                            SpeedButtonAxisSettings.Down := axisSettingsVisible;
                            GridPanelAxisOptions.Visible := axisSettingsVisible;
                            GridPanelAxisOptions.Width := self.Width - SpeedButtonAxisSettings.Left - 2;
                end;

        //destructor
            destructor TCustomGraphic2D.destroy();
                begin
                    FreeAndNil( skiaGeomDrawer );

                    inherited destroy();
                end;

        //accessors
            function TCustomGraphic2D.getOnGraphicUpdateGeometryEvent() : TGraphicUpdateGeometryEvent;
                begin
                    result := onGraphicUpdateGeometryEvent;
                end;

        //modifiers
            procedure TCustomGraphic2D.setOnGraphicUpdateGeometryEvent(const graphicDrawEventIn : TGraphicUpdateGeometryEvent);
                begin
                    onGraphicUpdateGeometryEvent := graphicDrawEventIn;
                end;

        //redraw the graphic
            procedure TCustomGraphic2D.redrawGraphic();
                begin
                    //this message is sent to wndProc where the graphic is updated and redrawn
                        PostMessage(self.Handle, WM_USER_REDRAWGRAPHIC, 0, 0);
                end;

            procedure TCustomGraphic2D.updateGeometry();
                var
                    newGeometryBoundary : TGeomBox;
                begin
                    setGraphicBackgroundColour();

                    //reset the stored geometry
                        skiaGeomDrawer.resetDrawingGeometry();

                    //update the skiaGeomDrawer geometry
                        if ( Assigned(onGraphicUpdateGeometryEvent) ) then
                            onGraphicUpdateGeometryEvent( self, skiaGeomDrawer );

                    //send message to redraw
                        redrawGraphic();

                    //must ensure that geometry is updated to draw
                        Application.ProcessMessages();
                end;

        //zooming methods
            procedure TCustomGraphic2D.zoomAll();
                begin
                    //make the drawing boundary the drawing region
                        skiaGeomDrawer.zoomAll();

                    redrawGraphic();
                end;

end.
