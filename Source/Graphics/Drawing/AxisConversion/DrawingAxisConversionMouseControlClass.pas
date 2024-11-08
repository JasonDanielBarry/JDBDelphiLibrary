unit DrawingAxisConversionMouseControlClass;

interface

    uses
        system.SysUtils, system.math, system.Types,
        {vcl.Controls,} Winapi.Windows, winapi.Messages,
        DrawingAxisConversionBaseClass, DrawingAxisConversionCalculationsClass, DrawingAxisConversionPanningClass,
        GeometryTypes
        ;

    type
        TDrawingAxisMouseControlConverter = class(TDrawingAxisPanningConverter)
            private
                var
                    mouseControlIsActive,
                    mousePanningIsActive    : boolean;
                    currentMousePosition,
                    mousePanningOrigin      : TPoint;
                    regionPanningOrigin     : TGeomPoint;
                //activate/deactivate mouse panning
                    procedure activateMousePanning();
                    procedure deactivateMousePanning(); inline;
                //panning with mouse
                    procedure panRegionWithMouse();
                //zooming relative to mouse
                    procedure zoomInRelativeToMouse(); inline;
                    procedure zoomOutRelativeToMouse(); inline;
                    procedure zoomRelativeToMouse(const messageIn : TMessage);
            public
                //constructor
                    constructor create();
                //destructor
                    destructor destroy(); override;
                //activate/deactivate mouse control
                    procedure activateMouseControl();
                    procedure deactivateMouseControl();
                //set current mouse position
                    procedure setCurrentMousePosition(const mousePositionIn : TPoint);
                //process windows messages
                    function processWindowsMessages(const [ref] mousePositionIn : TPoint;
                                                    const messageInOut          : Tmessage) : boolean;
        end;

implementation

    //private
        //activate mouse panning
            procedure TDrawingAxisMouseControlConverter.activateMousePanning();
                begin
                    //this function must only
                        if (NOT(mouseControlIsActive)) then
                            begin
                                deactivateMouseControl();
                                exit();
                            end;

                    //if mouse panning is already active then the function must exit so as not to override the panning origin points
                        if (mousePanningIsActive) then
                            exit();

                    mousePanningIsActive := True;

                    //origin points for panning region
                        mousePanningOrigin  := currentMousePosition;
                        regionPanningOrigin := getCurrentRegionCentreShift();
                end;

            procedure TDrawingAxisMouseControlConverter.deactivateMousePanning();
                begin
                    mousePanningIsActive := False;
                end;

        //panning with mouse
            procedure TDrawingAxisMouseControlConverter.panRegionWithMouse();
                var
                    mouse_dL,           mouse_dT            : integer;
                    regionShiftX,       regionShiftY,
                    newRegionCentreX,   newRegionCentreY    : double;
                begin
                    if (NOT(mousePanningIsActive)) then
                        exit();

                    //calculate how much the mouse moves from the point where the middle mouse button is pressed down
                        mouse_dL := mousePanningOrigin.X - currentMousePosition.X;
                        mouse_dT := mousePanningOrigin.Y - currentMousePosition.Y;

                    //convert mouse shift to drawing region shift
                        regionShiftX := dL_To_dX( mouse_dL );
                        regionShiftY := dT_To_dY( mouse_dT );

                    //calculate new region centre point
                        newRegionCentreX := regionPanningOrigin.x + regionShiftX;
                        newRegionCentreY := regionPanningOrigin.y + regionShiftY;

                    //move region to new position
                        setDrawingRegionShift( newRegionCentreX, newRegionCentreY );
                end;

        //zooming relative to mouse
            procedure TDrawingAxisMouseControlConverter.zoomInRelativeToMouse();
                var
                    regionPoint : TGeomPoint;
                begin
                    regionPoint := LT_to_XY( currentMousePosition );

                    zoomIn( 10, regionPoint );
                end;

            procedure TDrawingAxisMouseControlConverter.zoomOutRelativeToMouse();
                var
                    regionPoint : TGeomPoint;
                begin
                    regionPoint := LT_to_XY( currentMousePosition );

                    zoomOut( 10, regionPoint );
                end;

            procedure TDrawingAxisMouseControlConverter.zoomRelativeToMouse(const messageIn : TMessage);
                begin
                    if (messageIn.WParam = 7864320) then
                        zoomInRelativeToMouse()
                    else
                        zoomOutRelativeToMouse();
                end;

    //public
        //constructor
            constructor TDrawingAxisMouseControlConverter.create();
                begin
                    inherited create();

                    deactivateMouseControl();
                end;

        //destructor
            destructor TDrawingAxisMouseControlConverter.destroy();
                begin
                    inherited destroy();
                end;

        //activate/deactivate mouse control
            procedure TDrawingAxisMouseControlConverter.activateMouseControl();
                begin
                    mouseControlIsActive := True;
                end;

            procedure TDrawingAxisMouseControlConverter.deactivateMouseControl();
                begin
                    deactivateMousePanning();

                    mouseControlIsActive := False;
                end;

        //set current mouse position
            procedure TDrawingAxisMouseControlConverter.setCurrentMousePosition(const mousePositionIn : TPoint);
                begin
                    currentMousePosition := mousePositionIn;
                end;

        //process windows messages
            function TDrawingAxisMouseControlConverter.processWindowsMessages(  const [ref] mousePositionIn : TPoint;
                                                                                const messageInOut          : Tmessage  ) : boolean;
                begin
                    result := false;

                    if (NOT(mouseControlIsActive)) then
                        exit();

                    case (messageInOut.Msg) of
                        WM_MBUTTONDOWN:
                            activateMousePanning();

                        WM_MBUTTONUP:
                            deactivateMousePanning();

                        WM_MBUTTONDBLCLK:
                            begin
                                resetDrawingRegionToGeometryBoundary(); //has effect of zoom all
                                result := True;
                            end;

                        WM_MOUSEWHEEL:
                            begin
                                zoomRelativeToMouse(messageInOut);
                                result := True;
                            end;

                        WM_MOUSEMOVE:
                            begin
                                setCurrentMousePosition(mousePositionIn);

                                panRegionWithMouse();

                                result := mousePanningIsActive; // panRegionWithMouse only occurs if mouse panning is active
                            end;
                    end;
                end;

end.
