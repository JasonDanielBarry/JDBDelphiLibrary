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
      GeometryTypes,
      DrawingAxisConversionClass,
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
                    axisConverter                   : TDrawingAxisConverter;
                    onGraphicUpdateGeometryEvent    : TGraphicUpdateGeometryEvent;
                //axis Settings
                    procedure updateAxisSettingsValues();
                    procedure writeAxisSettingsValuesToAxisConverter();
                //background colour
                    procedure setGraphicBackgroundColour(); inline;
                //mouse location
                    procedure updateMouseCoordinates();
                //panning methods
                    procedure recentreAll();
                    procedure shiftDomain(const shiftXIn : double);
                    procedure shiftRange(const shiftYIn : double);
                    procedure shiftRegion(const shiftXIn, shiftYIn : double);
                //zooming methods
                    procedure zoomIn(const zoomPercentageIn : double);
                    procedure zoomOut(const zoomPercentageIn : double);
                    procedure setZoom(const zoomPercentageIn : double);
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
                axisConverter.activateMouseControl();
            end;

        procedure TCustomGraphic2D.SkPaintBoxGraphicMouseLeave(Sender: TObject);
            begin
                axisConverter.deactivateMouseControl();
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

                setZoom( newZoomPercent );
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
            var
                drawingRange : double;
            begin
                drawingRange := axisConverter.calculateRegionRange();

                shiftRange( drawingRange / 10 );
            end;

        procedure TCustomGraphic2D.ActionPanLeftExecute(Sender: TObject);
            var
                drawingDomain : double;
            begin
                drawingDomain := axisConverter.calculateRegionDomain();

                shiftDomain( drawingDomain / 10 );
            end;

        procedure TCustomGraphic2D.ActionPanRightExecute(Sender: TObject);
            var
                drawingDomain : double;
            begin
                drawingDomain := axisConverter.calculateRegionDomain();

                shiftDomain( -drawingDomain / 10 );
            end;

        procedure TCustomGraphic2D.ActionPanUpExecute(Sender: TObject);
            var
                drawingRange : double;
            begin
                drawingRange := axisConverter.calculateRegionRange();

                shiftRange( -drawingRange / 10 );
            end;

        procedure TCustomGraphic2D.ActionRecentreExecute(Sender: TObject);
            begin
                recentreAll();
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
                zoomIn(10);
            end;

        procedure TCustomGraphic2D.ActionZoomOutExecute(Sender: TObject);
            begin
                zoomOut(10);
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
                    drawingRegion := axisConverter.getDrawingRegion();

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

                        axisConverter.setDrawingRegion(0, newDrawingRegion);

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

        //mouse location
            procedure TCustomGraphic2D.updateMouseCoordinates();
                var
                    mouseCoordStr   : string;
                    currentMousePos : TPoint;
                    mousePointXY    : TGeomPoint;
                begin
                    if (NOT( axisConverter.MouseControlActive )) then
                        exit();

                    //get current mouse position
                        currentMousePos := SkPaintBoxGraphic.ScreenToClient( mouse.CursorPos );

                    //convert mouse position to XY coordinate
                        mousePointXY := axisConverter.LT_to_XY( currentMousePos );

                        mouseCoordStr := '(' + FloatToStrF(mousePointXY.x, ffFixed, 5, 2) + ', ' + FloatToStrF(mousePointXY.x, ffFixed, 5, 2) + ')';

                    //write to label
                        labelCoords.Caption := mouseCoordStr;
                end;

        //panning methods
            procedure TCustomGraphic2D.recentreAll();
                begin
                    axisConverter.recentreDrawingRegion();

                    redrawGraphic();
                end;

            procedure TCustomGraphic2D.shiftDomain(const shiftXIn : double);
                begin
                    axisConverter.shiftDrawingDomain( shiftXIn );

                    redrawGraphic();
                end;

            procedure TCustomGraphic2D.shiftRange(const shiftYIn : double);
                begin
                    axisConverter.shiftDrawingRange( shiftYIn );

                    redrawGraphic();
                end;

            procedure TCustomGraphic2D.shiftRegion(const shiftXIn, shiftYIn : double);
                begin
                    axisConverter.shiftDrawingRegion( shiftXIn, shiftYIn );

                    redrawGraphic();
                end;

        //zooming methods
            procedure TCustomGraphic2D.zoomIn(const zoomPercentageIn : double);
                begin
                    axisConverter.zoomIn( zoomPercentageIn );

                    redrawGraphic();
                end;

            procedure TCustomGraphic2D.zoomOut(const zoomPercentageIn : double);
                begin
                    axisConverter.zoomOut( zoomPercentageIn );

                    redrawGraphic();
                end;

            procedure TCustomGraphic2D.setZoom(const zoomPercentageIn : double);
                begin
                    axisConverter.setZoom( zoomPercentageIn );

                    redrawGraphic();
                end;

            procedure TCustomGraphic2D.updateZoomPercentage();
                var
                    currentZoomPercentage : double;
                begin
                    currentZoomPercentage := axisConverter.getCurrentZoomPercentage();
                    ComboBoxZoomPercent.Text := FloatToStrF( currentZoomPercentage, ffNumber, 5, 0 );
                end;

    //protected
        //drawing procedures
            procedure TCustomGraphic2D.preDrawGraphic();
                begin
                    //make sure canvas is the same colour as the parent
                        skiaGeomDrawer.setDrawingBackgroundColour( graphicBackgroundColour );

                    axisConverter.setDrawingSpaceRatioOneToOne();
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
                                                                            SkPaintBoxGraphic.Width,
                                                                            axisConverter           );

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
                begin
                    case (messageInOut.Msg) of
                        WM_MOUSEMOVE:
                            updateMouseCoordinates();
                    end;

                    mouseInputRequiresRedraw := axisConverter.processWindowsMessages( messageInOut );

                    mustUpdateGraphicImage := ( mouseInputRequiresRedraw OR (messageInOut.Msg = WM_USER_REDRAWGRAPHIC) );

                    if (mustUpdateGraphicImage) then
                        currentGraphicImage := updateGraphicImage();

                    if (mustRedrawGraphic) then
                        SkPaintBoxGraphic.Redraw();

                    inherited wndProc(messageInOut);
                end;

    //public
        //constructor
            constructor TCustomGraphic2D.create(AOwner : TComponent);
                begin
                    inherited create(AOwner);

                    //create required classes
                        axisConverter := TDrawingAxisConverter.create( SkPaintBoxGraphic );
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
                    FreeAndNil( axisConverter );
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

                    //determine the geometry group boundary
                        newGeometryBoundary := skiaGeomDrawer.determineGeomBoundingBox();

                    //store the geometry group boundary in the axis converter for quick access
                        axisConverter.setGeometryBoundary( newGeometryBoundary );

                    //send message to redraw
                        redrawGraphic();

                    //must ensure that geometry is updated to draw
                        Application.ProcessMessages();
                end;

        //zooming methods
            procedure TCustomGraphic2D.zoomAll();
                begin
                    //make the drawing boundary the drawing region
                        axisConverter.resetDrawingRegionToGeometryBoundary();

                    redrawGraphic();
                end;

end.
