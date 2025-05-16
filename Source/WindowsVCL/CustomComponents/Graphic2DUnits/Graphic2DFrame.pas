unit Graphic2DFrame;

interface

    uses
        Winapi.Windows, Winapi.Messages,
        Vcl.Direct2D, Winapi.D2D1,
        System.SysUtils, System.Variants, System.Classes, system.Types, system.UITypes,
        system.UIConsts, system.Threading, system.Math, system.Diagnostics, System.Actions,
        Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.Skia,
        Vcl.Buttons, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.ActnList, Vcl.Menus, vcl.Themes, Vcl.CheckLst,
        GeneralComponentHelperMethods,
        GeometryTypes, GeomBox,
        GraphicsListClass, Direct2DGraphicDrawingClass,
        Graphic2DTypes,
        Drawer2DPaintBoxClass
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
            LabelCoords: TLabel;
            ActionListControls: TActionList;
            ActionZoomIn: TAction;
            ActionZoomOut: TAction;
            ActionZoomExtents: TAction;
            ActionRecentre: TAction;
    ActionUpdateGraphics: TAction;
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
            PBDrawer2D: TPaintBox;
            N4: TMenuItem;
            ActionShowHideControls: TAction;
            ShowToolbar1: TMenuItem;
            //events
                procedure ComboBoxZoomPercentChange(Sender: TObject);
                procedure FrameResize(Sender: TObject);
                procedure ActionRecentreExecute(Sender: TObject);
                procedure ActionZoomExtentsExecute(Sender: TObject);
                procedure ActionZoomInExecute(Sender: TObject);
                procedure ActionZoomOutExecute(Sender: TObject);
                procedure ActionUpdateGraphicsExecute(Sender: TObject);
                procedure ActionPanLeftExecute(Sender: TObject);
                procedure ActionPanRightExecute(Sender: TObject);
                procedure ActionPanUpExecute(Sender: TObject);
                procedure ActionPanDownExecute(Sender: TObject);
                procedure ActionEditAxesExecute(Sender: TObject);
                procedure EditAxisValueKeyPress(Sender: TObject; var Key: Char);
                procedure ActionEditLayerTableExecute(Sender: TObject);
                procedure CheckListBoxLayerTableClick(Sender: TObject);
                procedure ActionShowHideControlsExecute(Sender: TObject);
                procedure CheckListBoxLayerTableDblClick(Sender: TObject);
            private
                var
                    axisSettingsVisible,
                    layerTableVisible,
                    toolbarVisible          : boolean;
                    onUpdateGraphicsEvent   : TUpdateGraphicsEvent;
                //axis Settings
                    procedure updateAxisSettingsValues();
                    procedure writeAxisSettingsValuesToAxisConverter();
                //components positions
                    procedure positionComponents();
                //layer table
                    procedure activeOnlySelectedLayer(const selectedLayerIndexIn : integer);
                    procedure sendActiveLayersToGraphicDrawer();
                    procedure updateLayerTable();
                //mouse methods
                    procedure updateMouseCoordinatesLabel();
                //zooming methods
                    procedure updateZoomPercentage();
            protected
                //process windows messages
                    procedure wndProc(var messageInOut : TMessage); override;
            public
                //constructor
                    constructor create(AOwner : TComponent); override;
                //destructor
                    destructor destroy(); override;
                //accessors
                    function getOnUpdateGraphicsEvent() : TUpdateGraphicsEvent;
                //modifiers
                    procedure setOnUpdateGraphicsEvent(const onUpdateGraphicsEventIn : TUpdateGraphicsEvent);
                //redraw the graphic
                    procedure redrawGraphic();
                    procedure updateBackgroundColour();
                    procedure updateGraphics();
                //zooming methods
                    procedure zoomAll();
        end;

implementation

{$R *.dfm}

    //events
        procedure TCustomGraphic2D.CheckListBoxLayerTableClick(Sender: TObject);
            begin
                sendActiveLayersToGraphicDrawer();
            end;

        procedure TCustomGraphic2D.CheckListBoxLayerTableDblClick(Sender: TObject);
            var
                checkedIndex,
                selectedIndex,
                i, checkedLayerCount : integer;
            begin
                checkedIndex    := 0;
                selectedIndex   := CheckListBoxLayerTable.ItemIndex;

                checkedLayerCount := 0;

                for i := 0 to ( CheckListBoxLayerTable.Count - 1 ) do
                    if ( CheckListBoxLayerTable.Checked[i] ) then
                        begin
                            checkedIndex := i;
                            inc( checkedLayerCount );
                        end;

                if ( (1 < checkedLayerCount) OR (checkedIndex <> selectedIndex) ) then
                    begin
                        activeOnlySelectedLayer( selectedIndex );
                        exit();
                    end;

                CheckListBoxLayerTable.CheckAll( TCheckBoxState.cbChecked, False, False );
                sendActiveLayersToGraphicDrawer();
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

                PBDrawer2D.GraphicDrawer.setZoom( newZoomPercent );

                redrawGraphic();
            end;

        procedure TCustomGraphic2D.FrameResize(Sender: TObject);
            begin
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
                PBDrawer2D.GraphicDrawer.shiftRange( 10 );

                redrawGraphic();
            end;

        procedure TCustomGraphic2D.ActionPanLeftExecute(Sender: TObject);
            begin
                PBDrawer2D.GraphicDrawer.shiftDomain( 10 );

                redrawGraphic();
            end;

        procedure TCustomGraphic2D.ActionPanRightExecute(Sender: TObject);
            begin
                PBDrawer2D.GraphicDrawer.shiftDomain( -10 );

                redrawGraphic();
            end;

        procedure TCustomGraphic2D.ActionPanUpExecute(Sender: TObject);
            begin
                PBDrawer2D.GraphicDrawer.shiftRange( -10 );

                redrawGraphic();
            end;

        procedure TCustomGraphic2D.ActionRecentreExecute(Sender: TObject);
            begin
                PBDrawer2D.GraphicDrawer.recentre();

                redrawGraphic();
            end;

        procedure TCustomGraphic2D.ActionShowHideControlsExecute(Sender: TObject);
            begin
                toolbarVisible := NOT( toolbarVisible );

                PanelGraphicControls.Visible    := toolbarVisible;
                ShowToolbar1.Checked            := toolbarVisible;
                GridPanelDirectionalPan.Visible := toolbarVisible;

                redrawGraphic();
            end;

        procedure TCustomGraphic2D.ActionUpdateGraphicsExecute(Sender: TObject);
            begin
                updateGraphics();
            end;

        procedure TCustomGraphic2D.ActionZoomExtentsExecute(Sender: TObject);
            begin
                zoomAll();
            end;

        procedure TCustomGraphic2D.ActionZoomInExecute(Sender: TObject);
            begin
                PBDrawer2D.GraphicDrawer.zoomIn(10);

                redrawGraphic();
            end;

        procedure TCustomGraphic2D.ActionZoomOutExecute(Sender: TObject);
            begin
                PBDrawer2D.GraphicDrawer.zoomOut(10);

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
                    drawingRegion : TGeomBox;
                begin
                    drawingRegion := PBDrawer2D.GraphicDrawer.getDrawingRegion();

                    EditXMin.Text := FloatToStrF( drawingRegion.xMin, ffFixed, 5, 2 );
                    EditXMax.Text := FloatToStrF( drawingRegion.xMax, ffFixed, 5, 2 );
                    EditYMin.Text := FloatToStrF( drawingRegion.yMin, ffFixed, 5, 2 );
                    EditYMax.Text := FloatToStrF( drawingRegion.yMax, ffFixed, 5, 2 );
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

                        PBDrawer2D.GraphicDrawer.setDrawingRegion( 0, newDrawingRegion );

                    redrawGraphic();
                end;

        //components positions
            procedure TCustomGraphic2D.positionComponents();
                begin
                    //axis settings
                        if (axisSettingsVisible) then
                            begin
                                GridPanelAxisOptions.Left   := SpeedButtonAxisSettings.Left;
                                GridPanelAxisOptions.Top    := PBDrawer2D.top + 1;
                                GridPanelAxisOptions.BringToFront();
                                GridPanelAxisOptions.Refresh();
                            end;

                    //layer table
                        if (layerTableVisible) then
                            begin
                                CheckListBoxLayerTable.Left := SpeedButtonLayerTable.left;
                                CheckListBoxLayerTable.Top  := PBDrawer2D.top + 1;
                                CheckListBoxLayerTable.BringToFront();
                                CheckListBoxLayerTable.Refresh();
                            end;
                end;

        //layer table
            procedure TCustomGraphic2D.activeOnlySelectedLayer(const selectedLayerIndexIn : integer);
                var
                    mustShowLayer   : boolean;
                    layerIndex      : integer;
                begin
                    for layerIndex := 0 to ( CheckListBoxLayerTable.Items.Count - 1 ) do
                        begin
                            mustShowLayer := ( layerIndex = selectedLayerIndexIn );

                            CheckListBoxLayerTable.Checked[ layerIndex ] := mustShowLayer;
                        end;

                    sendActiveLayersToGraphicDrawer();
                end;

            procedure TCustomGraphic2D.sendActiveLayersToGraphicDrawer();
                var
                    i               : integer;
                    arrActiveLayers : TArray<string>;
                    activeLayerList : TStringList;
                begin
                    activeLayerList := TStringList.create();
                    activeLayerList.Clear();

                    for i := 0 to (CheckListBoxLayerTable.Count - 1) do
                        begin
                            if ( NOT(CheckListBoxLayerTable.Checked[i]) ) then
                                continue;

                            activeLayerList.add( CheckListBoxLayerTable.Items[i] );
                        end;

                    //catch error of no layers selected
                        if ( activeLayerList.Count < 1 ) then
                            begin
                                Application.MessageBox('Cannot disable all layers', 'Error');

                                activeLayerList.add( CheckListBoxLayerTable.Items[0] );

                                CheckListBoxLayerTable.Checked[0] := True;
                            end;

                    arrActiveLayers := activeLayerList.ToStringArray();

                    PBDrawer2D.GraphicDrawer.setActiveDrawingLayers( arrActiveLayers );

                    redrawGraphic();

                    FreeAndNil( activeLayerList );
                end;

            procedure TCustomGraphic2D.updateLayerTable();
                var
                    itemIndex,
                    tableHeight         : integer;
                    layer               : string;
                    arrDrawingLayers    : TArray<string>;
                begin
                    arrDrawingLayers := PBDrawer2D.GraphicDrawer.getAllDrawingLayers();

                    CheckListBoxLayerTable.Items.Clear();

                    itemIndex := 0;

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
            procedure TCustomGraphic2D.updateMouseCoordinatesLabel();
                var
                    mouseCoordStr   : string;
                    mousePointXY    : TGeomPoint;
                begin
                    if (NOT( PBDrawer2D.GraphicDrawer.getMouseControlActive() )) then
                        exit();

                    //convert mouse position to XY coordinate
                        mousePointXY := PBDrawer2D.GraphicDrawer.getMouseCoordinatesXY();

                        mouseCoordStr := '(' + FloatToStrF(mousePointXY.x, ffFixed, 5, 2) + ', ' + FloatToStrF(mousePointXY.y, ffFixed, 5, 2) + ')';

                    //write to label
                        labelCoords.BringToFront();
                        labelCoords.Caption := mouseCoordStr;
                end;

        //zooming methods
            procedure TCustomGraphic2D.updateZoomPercentage();
                var
                    currentZoomPercentage : double;
                begin
                    currentZoomPercentage := PBDrawer2D.GraphicDrawer.getCurrentZoomPercentage();
                    ComboBoxZoomPercent.Text := FloatToStrF( currentZoomPercentage, ffNumber, 5, 0 );
                end;

    //protected
        //process windows messages
            procedure TCustomGraphic2D.wndProc(var messageInOut : TMessage);
                var
                    graphicWasRedrawn : boolean;
                begin
                    if ( Assigned( PBDrawer2D ) ) then
                        begin
                            //drawing messages
                                PBDrawer2D.processWindowsMessages( messageInOut, graphicWasRedrawn );

                            //update mouse XY coordinates
                                if (messageInOut.Msg = WM_MOUSEMOVE) then
                                    updateMouseCoordinatesLabel();

                            //update relevate properties
                                if ( graphicWasRedrawn ) then
                                    updateZoomPercentage();

                                updateAxisSettingsValues();
                        end;

                    inherited wndProc(messageInOut);
                end;

    //public
        //constructor
            constructor TCustomGraphic2D.create(AOwner : TComponent);
                begin
                    inherited create( AOwner );

                    //set up graphic controls
                        //tool bar
                            toolbarVisible                  := True;
                            PanelGraphicControls.Visible    := toolbarVisible;
                            ShowToolbar1.Checked            := toolbarVisible;

                        //coordinates label
                            labelCoords.Left    := labelCoords.Height div 2;
                            labelCoords.top     := PanelGraphicControls.Height + PBDrawer2D.Height - 3 * labelCoords.Height div 2;

                        //direction pan
                            GridPanelDirectionalPan.Left    := PanelGraphicControls.Width - GridPanelDirectionalPan.Width;
                            GridPanelDirectionalPan.top     := PanelGraphicControls.Height;
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

                    //hide components in design time
                        for var tempComponent : TControl in [CheckListBoxLayerTable, GridPanelAxisOptions, GridPanelDirectionalPan, LabelCoords] do
                            if csDesigning in tempComponent.ComponentState then
                                tempComponent.left := 3000;
                end;

        //destructor
            destructor TCustomGraphic2D.destroy();
                begin
                    inherited destroy();
                end;

        //accessors
            function TCustomGraphic2D.getOnUpdateGraphicsEvent() : TUpdateGraphicsEvent;
                begin
                    result := onUpdateGraphicsEvent;
                end;

        //modifiers
            procedure TCustomGraphic2D.setOnUpdateGraphicsEvent(const onUpdateGraphicsEventIn : TUpdateGraphicsEvent);
                begin
                    onUpdateGraphicsEvent := onUpdateGraphicsEventIn;
                end;

        //redraw the graphic
            procedure TCustomGraphic2D.redrawGraphic();
                begin
                    PBDrawer2D.postRedrawGraphicMessage( self );
                end;

            procedure TCustomGraphic2D.updateBackgroundColour();
                begin
                    PBDrawer2D.updateBackgroundColour( self );
                end;

            procedure TCustomGraphic2D.updateGraphics();
                var
                    graphicsList : TGraphicsList;
                begin
                    if NOT( Assigned( onUpdateGraphicsEvent ) ) then
                        exit();

                    try
                        graphicsList := TGraphicsList.create();

                        onUpdateGraphicsEvent( self, graphicsList );

                        PBDrawer2D.updateGraphics( self, graphicsList );

                        updateLayerTable();
                    finally
                        FreeAndNil( graphicsList );
                    end;
                end;

        //zooming methods
            procedure TCustomGraphic2D.zoomAll();
                begin
                    //make the drawing boundary the drawing region
                        PBDrawer2D.GraphicDrawer.zoomAll();

                    redrawGraphic();
                end;

end.
