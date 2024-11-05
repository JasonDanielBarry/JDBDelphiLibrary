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
      Graphic2DTypes;

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
            PanelZoom: TPanel;
            SpeedButtonCentre: TSpeedButton;
            labelCoords: TLabel;
            //events
                procedure SkPaintBoxGraphicDraw(ASender         : TObject;
                                                const ACanvas   : ISkCanvas;
                                                const ADest     : TRectF;
                                                const AOpacity  : Single    );
                procedure SpeedButtonZoomExtentsClick(Sender: TObject);
                procedure SpeedButtonUpdateGeometryClick(Sender: TObject);
                procedure SpeedButtonZoomInClick(Sender: TObject);
                procedure SpeedButtonZoomOutClick(Sender: TObject);
                procedure ComboBoxZoomPercentChange(Sender: TObject);
                procedure SpeedButtonShiftLeftClick(Sender: TObject);
                procedure SpeedButtonShiftRightClick(Sender: TObject);
                procedure SpeedButtonShiftUpClick(Sender: TObject);
                procedure SpeedButtonShiftDownClick(Sender: TObject);
                procedure SpeedButtonCentreClick(Sender: TObject);
                procedure SkPaintBoxGraphicMouseEnter(Sender: TObject);
                procedure SkPaintBoxGraphicMouseLeave(Sender: TObject);
                procedure FrameResize(Sender: TObject);
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
                    procedure shiftRegionWithMouse();
                    procedure activateMousePanning();
                //zooming methods
                    procedure zoomIn(const zoomPercentageIn : double);
                    procedure zoomInRelativeToMouse();
                    procedure zoomOut(const zoomPercentageIn : double);
                    procedure zoomOutRelativeToMouse();
                    procedure zoomRelativeToMouse(const messageIn : TMessage);
                    procedure setZoom(const zoomPercentageIn : double);
                    procedure updateZoomPercentage();
            protected
                //drawing procedure
                    procedure preDrawGraphic(const canvasIn : ISkCanvas); virtual;
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
                    procedure updateGeometry();
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
                if (NOT(mustRedrawGraphic)) then
                    exit();

                ACanvas.DrawImage( graphicImage, 0, 0 );

                mustRedrawGraphic := False;
            end;

        procedure TCustomGraphic2D.SkPaintBoxGraphicMouseEnter(Sender: TObject);
            begin
                mouseOnCanvas := True;
            end;

        procedure TCustomGraphic2D.SkPaintBoxGraphicMouseLeave(Sender: TObject);
            begin
                mouseOnCanvas := False;
            end;

        procedure TCustomGraphic2D.SpeedButtonCentreClick(Sender: TObject);
            begin
                recentreAll();
            end;

        procedure TCustomGraphic2D.SpeedButtonShiftDownClick(Sender: TObject);
            var
                drawingRange : double;
            begin
                drawingRange := axisConverter.calculateRegionRange();

                shiftRange( drawingRange / 10 );
            end;

        procedure TCustomGraphic2D.SpeedButtonShiftLeftClick(Sender: TObject);
            var
                drawingDomain : double;
            begin
                drawingDomain := axisConverter.calculateRegionDomain();

                shiftDomain( drawingDomain / 10 );
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

        procedure TCustomGraphic2D.SpeedButtonShiftRightClick(Sender: TObject);
            var
                drawingDomain : double;
            begin
                drawingDomain := axisConverter.calculateRegionDomain();

                shiftDomain( -drawingDomain / 10 );
            end;

        procedure TCustomGraphic2D.SpeedButtonShiftUpClick(Sender: TObject);
            var
                drawingRange : double;
            begin
                drawingRange := axisConverter.calculateRegionRange();

                shiftRange( -drawingRange / 10 );
            end;

        procedure TCustomGraphic2D.SpeedButtonUpdateGeometryClick(Sender: TObject);
            begin
                updateGeometry();
            end;

        procedure TCustomGraphic2D.SpeedButtonZoomExtentsClick(Sender: TObject);
            begin
                zoomAll();
            end;

        procedure TCustomGraphic2D.SpeedButtonZoomInClick(Sender: TObject);
            begin
                zoomIn(10);
            end;

        procedure TCustomGraphic2D.SpeedButtonZoomOutClick(Sender: TObject);
            begin
                zoomOut(10);
            end;

        procedure TCustomGraphic2D.FrameResize(Sender: TObject);
            begin
                updateGraphicImage();
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

            procedure TCustomGraphic2D.shiftRegionWithMouse();
                var
                    mouse_dL,           mouse_dT            : integer;
                    regionShiftX,       regionShiftY,
                    newRegionCentreX,   newRegionCentreY    : double;
                begin
                    if (NOT(mousePanningActive)) then
                        exit();

                    //calculate how much the mouse moves from the point where the middle mouse button is pressed down
                        mouse_dL := mousePanningOrigin.X - currentMousePos.X;
                        mouse_dT := mousePanningOrigin.Y - currentMousePos.Y;

                    //convert mouse shift to drawing shift
                        regionShiftX := axisConverter.dL_To_dX( mouse_dL );
                        regionShiftY := axisConverter.dT_To_dY( mouse_dT );

                    //calculate new region centre point
                        newRegionCentreX := regionPanningOrigin.x + regionShiftX;
                        newRegionCentreY := regionPanningOrigin.y + regionShiftY;

                    //move region to new position
                        axisConverter.setDrawingRegionShift(newRegionCentreX, newRegionCentreY);

                    updateGraphicImage();
                end;

            procedure TCustomGraphic2D.activateMousePanning();
                begin
                    if (NOT(mouseOnCanvas)) then
                        begin
                            exit();
                            mousePanningActive := False;
                        end;

                    if (mousePanningActive) then
                        exit();

                    var mousePoint : TPoint := mouse.CursorPos;

                    mousePanningActive := True;

                    mousePanningOrigin  := SkPaintBoxGraphic.ScreenToClient( mousePoint );
                    regionPanningOrigin := axisConverter.getCurrentRegionCentreShift();
                end;

        //zooming methods
            procedure TCustomGraphic2D.zoomIn(const zoomPercentageIn : double);
                begin
                    axisConverter.zoomIn( zoomPercentageIn );

                    updateGraphicImage();
                end;

            procedure TCustomGraphic2D.zoomInRelativeToMouse();
                var
                    mousePoint  : TPoint;
                    regionPoint : TGeomPoint;
                begin
                    mousePoint := SkPaintBoxGraphic.ScreenToClient(mouse.CursorPos);

                    regionPoint := axisConverter.LT_to_XY(mousePoint);

                    axisConverter.zoomIn(10, regionPoint);

                    updateGraphicImage();
                end;

            procedure TCustomGraphic2D.zoomOut(const zoomPercentageIn : double);
                begin
                    axisConverter.zoomOut( zoomPercentageIn );

                    updateGraphicImage();
                end;

            procedure TCustomGraphic2D.zoomOutRelativeToMouse();
                var
                    mousePoint  : TPoint;
                    regionPoint : TGeomPoint;
                begin
                    mousePoint := SkPaintBoxGraphic.ScreenToClient(mouse.CursorPos);

                    regionPoint := axisConverter.LT_to_XY(mousePoint);

                    axisConverter.zoomOut(10, regionPoint);

                    updateGraphicImage();
                end;

            procedure TCustomGraphic2D.zoomRelativeToMouse(const messageIn : TMessage);
                begin
                    if (messageIn.WParam = 7864320) then
                        zoomInRelativeToMouse()
                    else
                        zoomOutRelativeToMouse();
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
            procedure TCustomGraphic2D.preDrawGraphic(const canvasIn : ISkCanvas);
                var
                    parentColour : TAlphaColor;
                begin
                    //get the colour of the parent and convert it to an alpha colour
                        parentColour := colourToAlphaColour(self.Color);

                    //make sure canvas is the same colour as the parent
                        canvasIn.Clear( parentColour );

                    //give axis converter canvas dimensions
                        axisConverter.setCanvasRegion(SkPaintBoxGraphic.Height, SkPaintBoxGraphic.Width);

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
                    //create a skia surface
                        surface := TSkSurface.MakeRaster( SkPaintBoxGraphic.Width, SkPaintBoxGraphic.Height );

                    //draw to the surface
                        preDrawGraphic( surface.Canvas );

                        skiaGeomDrawer.drawAllGeometry( surface.Canvas, axisConverter );

                        postDrawGraphic( surface.Canvas );

                    //export to image
                        graphicImage := surface.MakeImageSnapshot();

                    updateZoomPercentage();

                    mustRedrawGraphic := True;
                end;

        //process windows messages
            procedure TCustomGraphic2D.wndProc(var messageInOut : TMessage);
                begin
                    case (messageInOut.Msg) of
                        WM_MBUTTONDOWN:
                            activateMousePanning();

                        WM_MBUTTONUP:
                            mousePanningActive := False;

                        WM_MBUTTONDBLCLK:
                            zoomAll();

                        WM_MOUSEWHEEL:
                            zoomRelativeToMouse(messageInOut);

                        WM_MOUSEMOVE:
                            begin
                                updateMouseCoordinates();

                                shiftRegionWithMouse();
                            end;
                    end;

                    if (mustRedrawGraphic) then
                        SkPaintBoxGraphic.Redraw();

                    inherited wndProc(messageInOut);
                end;

    //public
        //constructor
            constructor TCustomGraphic2D.create(AOwner : TComponent);
                begin
                    inherited create(AOwner);

                    labelCoords.Left := labelCoords.Height div 2;
                    labelCoords.top := PanelZoom.Height + SkPaintBoxGraphic.Height - 3 * labelCoords.Height div 2;

                    GridPanelDirectionalPan.Left := PanelZoom.Width - GridPanelDirectionalPan.Width - 1;
                    GridPanelDirectionalPan.top := PanelZoom.Height + 1;

                    GridPanelDirectionalPan.BringToFront();

                    axisConverter := TDrawingAxisConverter.create();
                    skiaGeomDrawer := TSkiaGeomDrawer.create();

                    updateGeometry();
                    zoomall();
                end;

        //destructor
            destructor TCustomGraphic2D.destroy();
                begin
                    FreeAndNil(axisConverter);

                    inherited Destroy();
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

            procedure TCustomGraphic2D.updateGeometry();
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
