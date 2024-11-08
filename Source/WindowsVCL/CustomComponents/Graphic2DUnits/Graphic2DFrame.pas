unit Graphic2DFrame;

interface

    uses
      Winapi.Windows, Winapi.Messages,
      System.SysUtils, System.Variants, System.Classes, system.Types, system.UITypes, system.UIConsts, system.Threading, system.Math, system.Diagnostics,
      Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.Skia,
      Vcl.Buttons, Vcl.ExtCtrls, Vcl.Skia, Vcl.StdCtrls,
      ColourMethods,
      GeometryTypes,
      DrawingAxisConversionClass,
      SkiaDrawingClass,
      Graphic2DTypes, System.Actions, Vcl.ActnList, Vcl.Menus;

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
            ActionList1: TActionList;
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

            private
                var
                    mouseOnCanvas, mousePanningActive,
                    mustRedrawGraphic                   : boolean;
                    graphicImage                        : ISkImage;
                    currentMousePos, mousePanningOrigin : TPoint;
                    regionPanningOrigin                 : TGeomPoint;
                    skiaGeomDrawer                      : TSkiaGeomDrawer;
                    axisConverter                       : TDrawingAxisConverter;
                    onGraphicUpdateGeometryEvent        : TGraphicUpdateGeometryEvent;
                //mouse location
                    procedure updateMouseCoordinates();
                //panning methods
                    procedure shiftDomain(const shiftXIn : double);
                    procedure shiftRange(const shiftYIn : double);
                    procedure shiftRegion(const shiftXIn, shiftYIn : double);
                //zooming methods
                    procedure zoomIn(const zoomPercentageIn : double);
                    procedure zoomOut(const zoomPercentageIn : double);
                    procedure setZoom(const zoomPercentageIn : double);
                    procedure updateZoomPercentage();
            protected
                //drawing procedure
                    procedure preDrawGraphic(); virtual;
                    procedure postDrawGraphic(const canvasIn : ISkCanvas); virtual;
                    procedure updateGraphicImage();
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
                    procedure updateGeometry(const mustRedrawGraphicIn : boolean = False);
                //panning methods
                    procedure recentreAll(const mustRedrawGraphicIn : boolean = False);
                //zooming methods
                    procedure zoomAll(const mustRedrawGraphicIn : boolean = False);
        end;



implementation

{$R *.dfm}

    //events
        procedure TCustomGraphic2D.SkPaintBoxGraphicDraw(   ASender         : TObject;
                                                            const ACanvas   : ISkCanvas;
                                                            const ADest     : TRectF;
                                                            const AOpacity  : Single    );
            begin
                ACanvas.DrawImage( graphicImage, 0, 0 );

                mustRedrawGraphic := False;
            end;

        procedure TCustomGraphic2D.SkPaintBoxGraphicMouseEnter(Sender: TObject);
            begin
                axisConverter.activateMouseControl();
                mouseOnCanvas := True;
            end;

        procedure TCustomGraphic2D.SkPaintBoxGraphicMouseLeave(Sender: TObject);
            begin
                axisConverter.deactivateMouseControl();
                mouseOnCanvas := False;
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
                updateGraphicImage();
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

    //private
        //mouse location
            procedure TCustomGraphic2D.updateMouseCoordinates();
                var
                    mouseCoordStr   : string;
                    mousePointXY    : TGeomPoint;
                begin
                    if (not(mouseOnCanvas)) then
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
            procedure TCustomGraphic2D.shiftDomain(const shiftXIn : double);
                begin
                    axisConverter.shiftDrawingDomain( shiftXIn );

                    updateGraphicImage();
                end;

            procedure TCustomGraphic2D.shiftRange(const shiftYIn : double);
                begin
                    axisConverter.shiftDrawingRange( shiftYIn );

                    updateGraphicImage();
                end;

            procedure TCustomGraphic2D.shiftRegion(const shiftXIn, shiftYIn : double);
                begin
                    axisConverter.shiftDrawingRegion( shiftXIn, shiftYIn );

                    updateGraphicImage();
                end;

        //zooming methods
            procedure TCustomGraphic2D.zoomIn(const zoomPercentageIn : double);
                begin
                    axisConverter.zoomIn( zoomPercentageIn );

                    updateGraphicImage();
                end;

            procedure TCustomGraphic2D.zoomOut(const zoomPercentageIn : double);
                begin
                    axisConverter.zoomOut( zoomPercentageIn );

                    updateGraphicImage();
                end;

            procedure TCustomGraphic2D.setZoom(const zoomPercentageIn : double);
                begin
                    axisConverter.setZoom( zoomPercentageIn );

                    updateGraphicImage();
                end;

            procedure TCustomGraphic2D.updateZoomPercentage();
                var
                    currentZoomPercentage : double;
                begin
                    currentZoomPercentage := axisConverter.getCurrentZoomPercentage();
                    ComboBoxZoomPercent.Text := FloatToStrF( currentZoomPercentage, ffNumber, 5, 0 );
                end;

    //protected
        //drawing procedure
            procedure TCustomGraphic2D.preDrawGraphic();
                var
                    graphicBackgroundColour : TAlphaColor;
                begin
                    //get the colour of the parent and convert it to an alpha colour
                        graphicBackgroundColour := colourToAlphaColour( self.Color );

                    //make sure canvas is the same colour as the parent
                        skiaGeomDrawer.setDrawingBackgroundColour( TAlphaColors.Null );

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

            procedure TCustomGraphic2D.updateGraphicImage();
                var
                    surface : ISkSurface;
                begin
                    //draw to the surface
                        preDrawGraphic();

                        surface := skiaGeomDrawer.drawAllGeometryToSurface( SkPaintBoxGraphic.Height,
                                                                            SkPaintBoxGraphic.Width,
                                                                            axisConverter           );

                        postDrawGraphic( surface.Canvas );

                    //export to image
                        graphicImage := surface.MakeImageSnapshot();

                    updateZoomPercentage();

                    mustRedrawGraphic := True;
                end;

        //process windows messages
            procedure TCustomGraphic2D.wndProc(var messageInOut : TMessage);
                var
                    mustUpdateGraphicImage : boolean;
                begin
                    case (messageInOut.Msg) of
                        WM_MOUSEMOVE:
                            updateMouseCoordinates();
                    end;

                    mustUpdateGraphicImage := axisConverter.processWindowsMessages( currentMousePos, messageInOut );

                    if (mustUpdateGraphicImage) then
                        updateGraphicImage();

                    if (mustRedrawGraphic) then
                        SkPaintBoxGraphic.Redraw();

                    inherited wndProc(messageInOut);
                end;

    //public
        //constructor
            constructor TCustomGraphic2D.create(AOwner : TComponent);
                begin
                    //create required classes (must be created before inherited as inherited calls wndProc with axisConverter = nil)
                        axisConverter := TDrawingAxisConverter.create();
                        skiaGeomDrawer := TSkiaGeomDrawer.create();

                    inherited create(AOwner);

                    //place graphic controls
                        labelCoords.Left := labelCoords.Height div 2;
                        labelCoords.top := PanelGraphicControls.Height + SkPaintBoxGraphic.Height - 3 * labelCoords.Height div 2;

                        GridPanelDirectionalPan.Left := PanelGraphicControls.Width - GridPanelDirectionalPan.Width - 1;
                        GridPanelDirectionalPan.top := PanelGraphicControls.Height + 1;

                        GridPanelDirectionalPan.BringToFront();
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
                    updateGraphicImage();

                    SkPaintBoxGraphic.Redraw();
                end;

            procedure TCustomGraphic2D.updateGeometry(const mustRedrawGraphicIn : boolean = False);
                var
                    newGeometryBoundary : TGeomBox;
                begin
                    //reset the stored geometry
                        skiaGeomDrawer.resetDrawingGeometry();

                    //update the skiaGeomDrawer geometry
                        if ( Assigned(onGraphicUpdateGeometryEvent) ) then
                            onGraphicUpdateGeometryEvent( self, skiaGeomDrawer );

                    //determine the geometry group boundary
                        newGeometryBoundary := skiaGeomDrawer.determineGeomBoundingBox();

                    //store the geometry group boundary in the axis converter for quick access
                        axisConverter.setGeometryBoundary( newGeometryBoundary );

                    if (mustRedrawGraphicIn) then
                        redrawGraphic()
                    else
                        updateGraphicImage();
                end;

        //panning methods
            procedure TCustomGraphic2D.recentreAll(const mustRedrawGraphicIn : boolean = False);
                begin
                    axisConverter.recentreDrawingRegion();

                    if (mustRedrawGraphicIn) then
                        redrawGraphic()
                    else
                        updateGraphicImage();
                end;

        //zooming methods
            procedure TCustomGraphic2D.zoomAll(const mustRedrawGraphicIn : boolean = False);
                begin
                    //make the drawing boundary the drawing region
                        axisConverter.resetDrawingRegionToGeometryBoundary();

                    if (mustRedrawGraphicIn) then
                        redrawGraphic()
                    else
                        updateGraphicImage();
                end;

end.
