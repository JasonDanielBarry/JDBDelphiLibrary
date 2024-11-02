unit Graphic2DFrame;

interface

    uses
      Winapi.Windows, Winapi.Messages,
      System.SysUtils, System.Variants, System.Classes, system.Types, system.UITypes, system.Threading, system.Math, system.Diagnostics,
      Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.Skia,
      Vcl.Buttons, Vcl.ExtCtrls, Vcl.Skia,
      GeometryTypes,
      DrawingAxisConversionClass,
      SkiaDrawingClass,
      Graphic2DTypes, Vcl.StdCtrls;

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
                    mouseOnCanvas, mousePanningActive   : boolean;
                    graphicImage                        : ISkImage;
                    stopWatch                           : TStopwatch;
                    mousePanningOrigin                  : TPoint;
                    regionPanningOrigin                 : TGeomPoint;
                    skiaGeomDrawer                      : TSkiaGeomDrawer;
                    axisConverter                       : TDrawingAxisConverter;
                    onGraphicUpdateGeometryEvent        : TGraphicUpdateGeometryEvent;
            protected
                //drawing procedure
                    procedure preDrawGraphic(const canvasIn : ISkCanvas); virtual;
                    procedure postDrawGraphic(const canvasIn : ISkCanvas); virtual;
                    procedure updateGraphicImage();
                //panning methods
                    procedure shiftDomain(const shiftXIn : double);
                    procedure shiftRange(const shiftYIn : double);
                    procedure shiftRegion(const shiftXIn, shiftYIn : double);
                    procedure shiftRegionWithMouse(const currentMouseXIn, currentMouseYIn : integer);
                    procedure activateMousePanning();
                //zooming methods
                    procedure zoomIn(const zoomPercentageIn : double);
                    procedure zoomInRelativeToMouse();
                    procedure zoomOut(const zoomPercentageIn : double);
                    procedure zoomOutRelativeToMouse();
                    procedure setZoom(const zoomPercentageIn : double);
                    procedure updateZoomPercentage();
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
                    procedure recentreAll();
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
                ACanvas.DrawImage( graphicImage, 0, 0 );
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
                redrawGraphic();
            end;

    //protected
        //drawing procedure
            procedure TCustomGraphic2D.preDrawGraphic(const canvasIn : ISkCanvas);
                var
                    currentZoomPercentage : double;
                begin
                    //make sure canvas is clear
                        canvasIn.Clear( TAlphaColors.Null );

                    //give axis converter canvas dimensions
                        axisConverter.setCanvasRegion(SkPaintBoxGraphic.Height, SkPaintBoxGraphic.Width);

                    //make sure setting ratio 1:1 does not cause drawing to shrink by catching the correct zoom percentage
                        currentZoomPercentage := axisConverter.getCurrentZoomPercentage();

                        axisConverter.setDrawingSpaceRatioOneToOne();

                        axisConverter.setZoom( currentZoomPercentage );
                end;

            procedure TCustomGraphic2D.postDrawGraphic(const canvasIn : ISkCanvas);
                var
                    paint : ISkPaint;
                begin
                    //draw a border around the paintbox edge
                        paint       := TSkPaint.Create( TSkPaintStyle.Stroke );
                        paint.Color := TAlphaColors.Silver;

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
                end;

        //panning methods
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

            procedure TCustomGraphic2D.shiftRegionWithMouse(const currentMouseXIn, currentMouseYIn : integer);
                var
                    mouse_dL,           mouse_dT            : integer;
                    regionShiftX,       regionShiftY,
                    newRegionCentreX,   newRegionCentreY    : double;
                begin
                    if (NOT(mousePanningActive)) then
                        exit();

                    //calculate how much the mouse moves from the point where the middle mouse button is pressed down
                        mouse_dL := mousePanningOrigin.X - currentMouseXIn;
                        mouse_dT := mousePanningOrigin.Y - currentMouseYIn;

                    //convert mouse shift to drawing shift
                        regionShiftX := axisConverter.dL_To_dX( mouse_dL );
                        regionShiftY := axisConverter.dT_To_dY( mouse_dT );

                    //calculate new region centre point
                        newRegionCentreX := regionPanningOrigin.x + regionShiftX;
                        newRegionCentreY := regionPanningOrigin.y + regionShiftY;

                    //move region to new position
                        axisConverter.setDrawingRegionShift(newRegionCentreX, newRegionCentreY);

                    var mouseShift : integer := round(sqrt( power(mouse_dL, 2) + power(mouse_dT, 2) ));

                    if ( (mouseShift mod 6) = 0 ) then
                        redrawGraphic();
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
                    regionPanningOrigin := axisConverter.getCurrentRegionCentrePoint();
                end;

        //zooming methods
            procedure TCustomGraphic2D.zoomIn(const zoomPercentageIn : double);
                begin
                    axisConverter.zoomIn( zoomPercentageIn );

                    redrawGraphic();
                end;

            procedure TCustomGraphic2D.zoomInRelativeToMouse();
                var
                    mousePoint  : TPoint;
                    regionPoint : TGeomPoint;
                begin
                    mousePoint := SkPaintBoxGraphic.ScreenToClient(mouse.CursorPos);

                    regionPoint := axisConverter.LT_to_XY(mousePoint);

                    axisConverter.zoomIn(10, regionPoint);

                    redrawGraphic();
                end;

            procedure TCustomGraphic2D.zoomOut(const zoomPercentageIn : double);
                begin
                    axisConverter.zoomOut( zoomPercentageIn );

                    redrawGraphic();
                end;

            procedure TCustomGraphic2D.zoomOutRelativeToMouse();
                var
                    mousePoint  : TPoint;
                    regionPoint : TGeomPoint;
                begin
                    mousePoint := SkPaintBoxGraphic.ScreenToClient(mouse.CursorPos);

                    regionPoint := axisConverter.LT_to_XY(mousePoint);

                    axisConverter.zoomOut(10, regionPoint);

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

        //process windows messages
            procedure TCustomGraphic2D.wndProc(var messageInOut : TMessage);
                begin
                    case (messageInOut.Msg) of
                        WM_MBUTTONDOWN:
                            activateMousePanning();

                        WM_MBUTTONUP:
                            mousePanningActive := False;

                        WM_MOUSEWHEEL:
                            begin
                                if (messageInOut.WParam = 7864320) then
                                    zoomInRelativeToMouse()
                                else
                                    zoomOutRelativeToMouse();
                            end;

                        WM_MOUSEMOVE:
                            begin
                                var currentMousePos : TPoint := SkPaintBoxGraphic.ScreenToClient( mouse.CursorPos );

                                shiftRegionWithMouse(currentMousePos.X, currentMousePos.Y);
                            end;
                    end;

                    inherited wndProc(messageInOut);
                end;

    //public
        //constructor
            constructor TCustomGraphic2D.create(AOwner : TComponent);
                begin
                    inherited create(AOwner);

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
                    newDrawingBoundary : TGeomBox;
                begin
                    //reset the stored geometry
                        skiaGeomDrawer.resetDrawingGeometry();

                    //update the skiaGeomDrawer geometry
                        if ( Assigned(onGraphicUpdateGeometryEvent) ) then
                            onGraphicUpdateGeometryEvent( self, skiaGeomDrawer );

                    //determine the geometry group boundary
                        newDrawingBoundary := skiaGeomDrawer.determineGeomBoundingBox();

                    //store the group boundary in the axis converter for quick access
                        axisConverter.setGeometryBoundary( newDrawingBoundary );
                end;

        //panning methods
            procedure TCustomGraphic2D.recentreAll();
                begin
                    axisConverter.recentreDrawingRegion();

                    redrawGraphic();
                end;

        //zooming methods
            procedure TCustomGraphic2D.zoomAll();
                begin
                    //make the drawing boundary the drawing region
                        axisConverter.resetDrawingRegionToGeometryBoundary();

                    redrawGraphic();
                end;

end.
