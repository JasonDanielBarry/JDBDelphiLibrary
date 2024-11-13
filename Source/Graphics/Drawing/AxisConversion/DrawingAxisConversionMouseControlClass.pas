unit DrawingAxisConversionMouseControlClass;

interface

    uses
        system.SysUtils, system.math, system.Types,
        vcl.Controls, Winapi.Windows, winapi.Messages,
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
                    graphicControlComponent : TGraphicControl;
                //activate/deactivate mouse panning
                    procedure activateMousePanning();
                    procedure deactivateMousePanning(); inline;
                //panning with mouse
                    procedure panRegionWithMouse();
                //set current mouse position
                    procedure updateCurrentMousePosition();
                //set the graphic control
                    procedure setGraphicControlComponent(const componentIn : TGraphicControl);
                //zooming relative to mouse
                    procedure zoomInRelativeToMouse(); inline;
                    procedure zoomOutRelativeToMouse(); inline;
                    procedure zoomRelativeToMouse(const messageIn : TMessage);
            public
                //constructor
                    constructor create(const graphicControlComponentIn : TGraphicControl); virtual;
                //destructor
                    destructor destroy(); override;
                //accessors
                    property MouseControlActive : boolean read mouseControlIsActive;
                //activate/deactivate mouse control
                    procedure activateMouseControl();
                    procedure deactivateMouseControl();
                //match the graphic control dimensions with the axis converter canvas size
                    procedure matchGraphicControlDimensionsToCanvas();
                //process windows messages
                    function processWindowsMessages(const messageIn : Tmessage) : boolean;
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
            constructor TDrawingAxisMouseControlConverter.create(const graphicControlComponentIn : TGraphicControl);
                begin
                    inherited create();

                    setGraphicControlComponent( graphicControlComponentIn );

                    deactivateMouseControl();
                end;

        //destructor
            destructor TDrawingAxisMouseControlConverter.destroy();
                begin
                    inherited destroy();
                end;

        //set the graphic control
            procedure TDrawingAxisMouseControlConverter.setGraphicControlComponent(const componentIn : TGraphicControl);
                begin
                    graphicControlComponent := componentIn;
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

        //match the graphic control dimensions with the axis converter canvas size
            procedure TDrawingAxisMouseControlConverter.matchGraphicControlDimensionsToCanvas();
                begin
                    setCanvasDimensions( graphicControlComponent.Height, graphicControlComponent.Width );
                end;

        //set current mouse position
            procedure TDrawingAxisMouseControlConverter.updateCurrentMousePosition();
                begin
                    currentMousePosition := graphicControlComponent.ScreenToClient( mouse.CursorPos );
                end;

        //process windows messages
            function TDrawingAxisMouseControlConverter.processWindowsMessages(const messageIn : Tmessage) : boolean;
                begin
                    result := false;

                    //ensure the axis convertsion class is created before processing any messages
                        if (self = nil) then
                            exit();

                    //procedure must only run if mouse control is activated
                        if ( NOT(mouseControlIsActive) ) then
                            exit();

                    case (messageIn.Msg) of
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
                                zoomRelativeToMouse(messageIn);
                                result := True;
                            end;

                        WM_MOUSEMOVE:
                            begin
                                updateCurrentMousePosition();

                                panRegionWithMouse();

                                result := mousePanningIsActive; // panRegionWithMouse only occurs if mouse panning is active
                            end;
                    end;
                end;

end.
