unit Graphic2DFrame;

interface

    uses
        Winapi.Windows, Winapi.Messages,
        Vcl.Direct2D, Winapi.D2D1,
        System.SysUtils, System.Variants, System.Classes, system.Types, system.UITypes,
        system.UIConsts, system.Threading, system.Math, system.Diagnostics, System.Actions,
        Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.Skia,
        Vcl.Buttons, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.ActnList, Vcl.Menus, vcl.Themes,
        GeneralComponentHelperMethods,
        ColourMethods,
        GeometryTypes, GeomBox,
        GeomDrawerBaseClass, GeomDrawerAxisConversionInterfaceClass, Direct2DDrawingClass,
        Graphic2DTypes, Vcl.CheckLst
        ;

    type
        TCustomGraphic2D = class(TFrame)
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
            CheckListBoxLayerTable: TCheckListBox;
            ActionEditLayerTable: TAction;
            SpeedButtonLayerTable: TSpeedButton;
            EditLayerTable1: TMenuItem;
            PaintBoxGraphic: TPaintBox;
            //events
                procedure PaintBoxGraphicPaint(Sender: TObject);
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
                procedure ActionEditLayerTableExecute(Sender: TObject);
                procedure CheckListBoxLayerTableClick(Sender: TObject);
            private
                var
                    axisSettingsVisible,
                    layerTableVisible,
                    mustRedrawGraphic               : boolean;
                    graphicBackgroundColour         : TAlphaColor;
                    currentGraphicBuffer            : TBitmap;
                    D2DBufferCanvas                 : TDirect2DCanvas;
                    D2DGeomDrawer                   : TDirect2DGeomDrawer;
                    onGraphicUpdateGeometryEvent    : TGraphicUpdateGeometryEvent;
                //axis Settings
                    procedure updateAxisSettingsValues();
                    procedure writeAxisSettingsValuesToAxisConverter();
                //background colour
                    procedure setGraphicBackgroundColour();
                //components positions
                    procedure positionComponents();
                //destroy and create Direct2D canvas using the size of the paintbox
                    procedure recreateD2DCanvas();
                //layer table
                    procedure getActiveLayers();
                    procedure updateLayerTable();
                //mouse methods
                    procedure updateMouseCoordinates();
                    procedure setMouseCursor(const messageIn : TMessage);
                //zooming methods
                    procedure updateZoomPercentage();
            protected
                //drawing procedures
                    procedure preDrawGraphic(const canvasIn : TDirect2DCanvas); virtual;
                    procedure postDrawGraphic(const canvasIn : TDirect2DCanvas); virtual;
                    procedure updateGraphicBuffer();
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
        procedure TCustomGraphic2D.PaintBoxGraphicPaint(Sender: TObject);
            begin
                //draw buffer to screen
                    PaintBoxGraphic.Canvas.Draw( 0, 0, currentGraphicBuffer );

                mustRedrawGraphic := False;
            end;

        procedure TCustomGraphic2D.SkPaintBoxGraphicMouseEnter(Sender: TObject);
            begin
                D2DGeomDrawer.activateMouseControl();
            end;

        procedure TCustomGraphic2D.SkPaintBoxGraphicMouseLeave(Sender: TObject);
            begin
                D2DGeomDrawer.deactivateMouseControl();
            end;

        procedure TCustomGraphic2D.CheckListBoxLayerTableClick(Sender: TObject);
            begin
                getActiveLayers();

                redrawGraphic();
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

                D2DGeomDrawer.setZoom( newZoomPercent );

                redrawGraphic();
            end;

        procedure TCustomGraphic2D.FrameResize(Sender: TObject);
            begin
                positionComponents();

                redrawGraphic();
            end;

        procedure TCustomGraphic2D.ActionEditAxesExecute(Sender: TObject);
            begin
                //hide layer table
                    layerTableVisible               := False;
                    EditLayerTable1.Checked         := layerTableVisible;
                    SpeedButtonLayerTable.Down      := layerTableVisible;
                    CheckListBoxLayerTable.Visible  := layerTableVisible;

                //hide or show the axis settings
                    axisSettingsVisible             := NOT(axisSettingsVisible);
                    EditAxes1.Checked               := axisSettingsVisible;
                    positionComponents();
                    GridPanelAxisOptions.Visible    := axisSettingsVisible;

                //early return
                    if NOT(axisSettingsVisible) then
                        begin
                            SpeedButtonAxisSettings.Down := False;
                            exit();
                        end;

                //adjust button
                    setSpeedButtonDown(1, SpeedButtonAxisSettings);

                    GridPanelAxisOptions.BringToFront();
            end;

        procedure TCustomGraphic2D.ActionEditLayerTableExecute(Sender: TObject);
            begin
                //hide axis settings
                    axisSettingsVisible             := False;
                    EditAxes1.Checked               := axisSettingsVisible;
                    SpeedButtonAxisSettings.Down    := axisSettingsVisible;
                    GridPanelAxisOptions.Visible    := axisSettingsVisible;

                //show or hide the layer table
                    layerTableVisible               := NOT(layerTableVisible);
                    EditLayerTable1.Checked         := layerTableVisible;
                    positionComponents();
                    CheckListBoxLayerTable.Visible  := layerTableVisible;

                if (NOT(layerTableVisible)) then
                    begin
                        SpeedButtonLayerTable.Down := False;
                        exit();
                    end;

                //adjust buttons
                    setSpeedButtonDown(1, SpeedButtonLayerTable);

                    CheckListBoxLayerTable.BringToFront();
            end;

        procedure TCustomGraphic2D.ActionPanDownExecute(Sender: TObject);
            begin
                D2DGeomDrawer.shiftRange( 10 );

                redrawGraphic();
            end;

        procedure TCustomGraphic2D.ActionPanLeftExecute(Sender: TObject);
            begin
                D2DGeomDrawer.shiftDomain( 10 );

                redrawGraphic();
            end;

        procedure TCustomGraphic2D.ActionPanRightExecute(Sender: TObject);
            begin
                D2DGeomDrawer.shiftDomain( -10 );

                redrawGraphic();
            end;

        procedure TCustomGraphic2D.ActionPanUpExecute(Sender: TObject);
            begin
                D2DGeomDrawer.shiftRange( -10 );

                redrawGraphic();
            end;

        procedure TCustomGraphic2D.ActionRecentreExecute(Sender: TObject);
            begin
                D2DGeomDrawer.recentre();

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
                D2DGeomDrawer.zoomIn(10);

                redrawGraphic();
            end;

        procedure TCustomGraphic2D.ActionZoomOutExecute(Sender: TObject);
            begin
                D2DGeomDrawer.zoomOut(10);

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
                    drawingRegion := D2DGeomDrawer.getDrawingRegion();

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

                        D2DGeomDrawer.setDrawingRegion(0, newDrawingRegion);

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

        //components positions
            procedure TCustomGraphic2D.positionComponents();
                begin
                    //axisSettings
                        if (axisSettingsVisible) then
                            begin
                                GridPanelAxisOptions.Left   := SpeedButtonAxisSettings.Left;
                                GridPanelAxisOptions.Top    := PaintBoxGraphic.top + 1;
                                GridPanelAxisOptions.Refresh();
                            end;

                    //layer table
                        if (layerTableVisible) then
                            begin
                                CheckListBoxLayerTable.Left := SpeedButtonLayerTable.left;
                                CheckListBoxLayerTable.Top  := PanelGraphicControls.Height + 1;
                                CheckListBoxLayerTable.Refresh();
                            end;
                end;

        //destroy and create Direct2D canvas using the size of the paintbox
            procedure TCustomGraphic2D.recreateD2DCanvas();
                begin
                    try
                        FreeAndNil( D2DBufferCanvas );
                    except

                    end;

                    currentGraphicBuffer.SetSize( PaintBoxGraphic.Width, PaintBoxGraphic.Height );

                    D2DBufferCanvas := TDirect2DCanvas.Create( currentGraphicBuffer.Canvas, Rect(0, 0, PaintBoxGraphic.Width, PaintBoxGraphic.Height) );

                    D2DBufferCanvas.RenderTarget.SetAntialiasMode( D2D1_ANTIALIAS_MODE.D2D1_ANTIALIAS_MODE_PER_PRIMITIVE );
                end;

        //layer table
            procedure TCustomGraphic2D.getActiveLayers();
                var
                    i, activeLayerCount : integer;
                    arrActiveLayers     : TArray<string>;
                begin
                    activeLayerCount := 0;

                    SetLength( arrActiveLayers, activeLayerCount );

                    for i := 0 to (CheckListBoxLayerTable.Count - 1) do
                        begin
                            if (CheckListBoxLayerTable.Checked[i]) then
                                begin
                                    inc( activeLayerCount );

                                    SetLength( arrActiveLayers, activeLayerCount );

                                    arrActiveLayers[ activeLayerCount - 1 ] := CheckListBoxLayerTable.Items[i];
                                end;
                        end;

                    //catch error of no layers being selected
                        if (activeLayerCount < 1) then
                            begin
                                Application.MessageBox('Cannot disable all layers', 'Error');

                                arrActiveLayers := [CheckListBoxLayerTable.Items[0]];

                                CheckListBoxLayerTable.Checked[0] := True;
                            end;

                    D2DGeomDrawer.setActiveDrawingLayers( arrActiveLayers );

                    D2DGeomDrawer.setGeomBoundingBox();
                end;

            procedure TCustomGraphic2D.updateLayerTable();
                var
                    itemIndex,
                    tableHeight         : integer;
                    layer               : string;
                    arrDrawingLayers    : TArray<string>;
                begin
                    arrDrawingLayers := D2DGeomDrawer.getAllDrawingLayers();

                    CheckListBoxLayerTable.Items.Clear();

                    itemIndex := 0;
                    tableHeight := 0;

                    for layer in arrDrawingLayers do
                        begin
                            CheckListBoxLayerTable.Items.Add( layer );
                            CheckListBoxLayerTable.Checked[ itemIndex ] := True;

                            inc( itemIndex );
                        end;

                    tableHeight := max( GridPanelDirectionalPan.Height, CheckListBoxLayerTable.ItemHeight * CheckListBoxLayerTable.Count + round(5 * self.ScaleFactor) );

                    tableHeight := min( tableHeight, CheckListBoxLayerTable.ItemHeight * 10 + round(5 * self.ScaleFactor) );

                    CheckListBoxLayerTable.Height := tableHeight;
                end;

        //mouse methods
            procedure TCustomGraphic2D.updateMouseCoordinates();
                var
                    mouseCoordStr   : string;
                    mousePointXY    : TGeomPoint;
                begin
                    if (NOT( D2DGeomDrawer.getMouseControlActive() )) then
                        exit();

                    //convert mouse position to XY coordinate
                        mousePointXY := D2DGeomDrawer.getMouseCoordinatesXY();

                        mouseCoordStr := '(' + FloatToStrF(mousePointXY.x, ffFixed, 5, 2) + ', ' + FloatToStrF(mousePointXY.x, ffFixed, 5, 2) + ')';

                    //write to label
                        labelCoords.Caption := mouseCoordStr;
                end;

            procedure TCustomGraphic2D.setMouseCursor(const messageIn : TMessage);
                begin
                    try
                        if NOT(D2DGeomDrawer.getMouseControlActive()) then
                            begin
                                PaintBoxGraphic.Cursor := crDefault;
                                exit();
                            end;

                        case (messageIn.Msg) of
                            WM_MBUTTONDOWN:
                                PaintBoxGraphic.Cursor := crSizeAll;
                            WM_MBUTTONUP:
                                PaintBoxGraphic.Cursor := crDefault;
                        end;
                    except

                    end;
                end;

        //zooming methods
            procedure TCustomGraphic2D.updateZoomPercentage();
                var
                    currentZoomPercentage : double;
                begin
                    currentZoomPercentage := D2DGeomDrawer.getCurrentZoomPercentage();
                    ComboBoxZoomPercent.Text := FloatToStrF( currentZoomPercentage, ffNumber, 5, 0 );
                end;

    //protected
        //drawing procedures
            procedure TCustomGraphic2D.preDrawGraphic(const canvasIn : TDirect2DCanvas);
                begin
                    //make sure canvas is the same colour as the parent
                        D2DGeomDrawer.setDrawingBackgroundColour( graphicBackgroundColour );
                end;

            procedure TCustomGraphic2D.postDrawGraphic(const canvasIn : TDirect2DCanvas);
                begin
                    //draw a border around the paintbox edge
                        canvasIn.brush.Color  := TColors.Silver;

                        canvasIn.FrameRect(
                                                Rect(0, 0, PaintBoxGraphic.Width - 1, PaintBoxGraphic.Height - 1)
                                          );
                end;

            procedure TCustomGraphic2D.updateGraphicBuffer();
                begin
                    //create new D2D canvas for new drawing
                        recreateD2DCanvas();

                    //draw to the surface
                        D2DBufferCanvas.BeginDraw();

                            preDrawGraphic( D2DBufferCanvas );

                            D2DGeomDrawer.drawAllGeometry( PaintBoxGraphic.Height, PaintBoxGraphic.Width, D2DBufferCanvas );

                            postDrawGraphic( D2DBufferCanvas );

                        D2DBufferCanvas.EndDraw();

                    //update relevate properties
                        updateZoomPercentage();

                        updateAxisSettingsValues();

                        mustRedrawGraphic := True;
                end;

        //process windows messages
            procedure TCustomGraphic2D.wndProc(var messageInOut : TMessage);
                var
                    mouseInputRequiresRedraw,
                    mustUpdateGraphicImage      : boolean;
                    currentMousePosition        : TPoint;
                begin
                    //drawing graphic-----------------------------------------------------------------------------------------------
                        //update the mouse position
                            if (messageInOut.Msg = WM_MOUSEMOVE) then
                                currentMousePosition := PaintBoxGraphic.ScreenToClient( mouse.CursorPos );

                        //process windows message in axis converter
                            mouseInputRequiresRedraw := D2DGeomDrawer.processWindowsMessages( messageInOut, currentMousePosition );

                        //determine if redrawing is required
                            mustUpdateGraphicImage := ( mouseInputRequiresRedraw OR (messageInOut.Msg = WM_USER_REDRAWGRAPHIC) );

                        //render image off screen
                            if (mustUpdateGraphicImage) then
                                updateGraphicBuffer();

                        //paint rendered image to screen
                            if (mustRedrawGraphic) then
                                PaintBoxGraphic.Repaint();
                    //--------------------------------------------------------------------------------------------------------------

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
                        currentGraphicBuffer := TBitmap.create();

                        D2DGeomDrawer := TDirect2DGeomDrawer.create();

                    //set up graphic controls
                        //coordinates label
                            labelCoords.Left := labelCoords.Height div 2;
                            labelCoords.top := PanelGraphicControls.Height + PaintBoxGraphic.Height - 3 * labelCoords.Height div 2;

                        //direction pan
                            GridPanelDirectionalPan.Left := PanelGraphicControls.Width - GridPanelDirectionalPan.Width - 1;
                            GridPanelDirectionalPan.top := PanelGraphicControls.Height + 1;
                            GridPanelDirectionalPan.BringToFront();

                        //axis settings
                            axisSettingsVisible             := False;
                            SpeedButtonAxisSettings.Down    := axisSettingsVisible;
                            GridPanelAxisOptions.Visible    := axisSettingsVisible;
                            GridPanelAxisOptions.Width      := self.Width - SpeedButtonAxisSettings.Left - 2;

                        //layer table
                            layerTableVisible               := False;
                            CheckListBoxLayerTable.Visible  := layerTableVisible;
                            SpeedButtonLayerTable.down      := layerTableVisible;
                            CheckListBoxLayerTable.Width    := self.Width - SpeedButtonLayerTable.Left - 2;
                end;

        //destructor
            destructor TCustomGraphic2D.destroy();
                begin
                    FreeAndNil( currentGraphicBuffer );
                    FreeAndNil( D2DBufferCanvas );
                    FreeAndNil( D2DGeomDrawer );

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
                        D2DGeomDrawer.resetDrawingGeometry();

                    //update the D2DGeomDrawer geometry
                        if ( Assigned(onGraphicUpdateGeometryEvent) ) then
                            onGraphicUpdateGeometryEvent( self, tGeomDrawer(D2DGeomDrawer) );

                    //do layer table
                        updateLayerTable();
                        getActiveLayers();

                    //send message to redraw
                        redrawGraphic();

                    //must ensure that geometry is updated to draw
                        Application.ProcessMessages();
                end;

        //zooming methods
            procedure TCustomGraphic2D.zoomAll();
                begin
                    //make the drawing boundary the drawing region
                        D2DGeomDrawer.zoomAll();

                    redrawGraphic();
                end;

end.
