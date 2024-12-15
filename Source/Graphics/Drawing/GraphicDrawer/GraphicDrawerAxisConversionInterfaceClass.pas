unit GraphicDrawerAxisConversionInterfaceClass;

interface

    uses
        system.SysUtils, system.math, system.Types,
        Winapi.Messages,
        vcl.Controls,
        GeometryTypes, GeomBox,
        DrawingAxisConversionClass,
        GraphicDrawerLayersClass
        ;

    type
        TGraphicDrawerAxisConversionInterface = class(TGraphicDrawerLayers)
            public
                //constructor
                    constructor create(); override;
                //destructor
                    destructor destroy(); override;
                //axis conversion interface
                    //drawing region
                        function getDrawingRegion() : TGeomBox;
                        procedure setDrawingRegion( const bufferIn : double;
                                                    const regionIn : TGeomBox );
                    //draw space ratio
                        procedure setDrawingSpaceRatio(const ratioIn : double);
                    //mouse coordinates
                        function getMouseCoordinatesXY() : TGeomPoint;
                    //panning
                        procedure recentre();
                        procedure shiftDomain(const percentageIn : double);
                        procedure shiftRange(const percentageIn : double);
                    //zooming
                        function getCurrentZoomPercentage() : double;
                        procedure setZoom(const percentageIn : double);
                        procedure zoomIn(const percentageIn : double);
                        procedure zoomOut(const percentageIn : double);
                        procedure zoomAll();
                //process windows messages
                    procedure activateMouseControl();
                    procedure deactivateMouseControl();
                    function getMouseControlActive() : boolean;
                    function processWindowsMessages(const messageIn             : Tmessage;
                                                    const newMousePositionIn    : TPoint    ) : boolean;
        end;

implementation

    //public
        //constructor
            constructor TGraphicDrawerAxisConversionInterface.create();
                begin
                    inherited create();
                end;

        //destructor
            destructor TGraphicDrawerAxisConversionInterface.destroy();
                begin
                    inherited destroy();
                end;

        //axis conversion methods
            //drawing region
                function TGraphicDrawerAxisConversionInterface.getDrawingRegion() : TGeomBox;
                    begin
                        result := axisConverter.getDrawingRegion();
                    end;

                procedure TGraphicDrawerAxisConversionInterface.setDrawingRegion(  const bufferIn : double;
                                                                                const regionIn : TGeomBox   );
                    begin
                        axisConverter.setDrawingRegion(bufferIn, regionIn);
                    end;

            //draw space ratio
                procedure TGraphicDrawerAxisConversionInterface.setDrawingSpaceRatio(const ratioIn : double);
                    begin
                        axisConverter.setDrawingSpaceRatio( ratioIn );
                    end;

            //mouse coordinates
                function TGraphicDrawerAxisConversionInterface.getMouseCoordinatesXY() : TGeomPoint;
                    begin
                        result := axisConverter.getMouseCoordinatesXY();
                    end;

            //panning
                procedure TGraphicDrawerAxisConversionInterface.recentre();
                    begin
                        axisConverter.recentreDrawingRegion();
                    end;

                procedure TGraphicDrawerAxisConversionInterface.shiftDomain(const percentageIn : double);
                    var
                        domainShift, regionDomain : double;
                    begin
                        regionDomain := axisConverter.calculateRegionDomain();

                        domainShift := (percentageIn / 100) * regionDomain;

                        axisConverter.shiftDrawingDomain( domainShift );
                    end;

                procedure TGraphicDrawerAxisConversionInterface.shiftRange(const percentageIn : double);
                    var
                        rangeShift, regionRange : double;
                    begin
                        regionRange := axisConverter.calculateRegionDomain();

                        rangeShift := (percentageIn / 100) * regionRange;

                        axisConverter.shiftDrawingRange( rangeShift );
                    end;

            //zooming
                function TGraphicDrawerAxisConversionInterface.getCurrentZoomPercentage() : double;
                    begin
                        result := axisConverter.calculateCurrentZoomPercentage();
                    end;

                procedure TGraphicDrawerAxisConversionInterface.setZoom(const percentageIn : double);
                    begin
                        axisConverter.setZoom( percentageIn );
                    end;

                procedure TGraphicDrawerAxisConversionInterface.zoomIn(const percentageIn : double);
                    begin
                        axisConverter.zoomIn( percentageIn );
                    end;

                procedure TGraphicDrawerAxisConversionInterface.zoomOut(const percentageIn : double);
                    begin
                        axisConverter.zoomOut( percentageIn );
                    end;

                procedure TGraphicDrawerAxisConversionInterface.zoomAll();
                    begin
                        axisConverter.resetDrawingRegionToGeometryBoundary();
                    end;

        //process windows messages
            procedure TGraphicDrawerAxisConversionInterface.activateMouseControl();
                begin
                    axisConverter.activateMouseControl();
                end;

            procedure TGraphicDrawerAxisConversionInterface.deactivateMouseControl();
                begin
                    axisConverter.deactivateMouseControl();
                end;

            function TGraphicDrawerAxisConversionInterface.getMouseControlActive() : boolean;
                begin
                    result := axisConverter.MouseControlActive;
                end;

            function TGraphicDrawerAxisConversionInterface.processWindowsMessages(  const messageIn             : Tmessage;
                                                                                    const newMousePositionIn    : TPoint    ) : boolean;
                begin
                    if (self = nil) then
                        begin
                            result := false;
                            exit();
                        end;

                    result := axisConverter.processWindowsMessages( messageIn, newMousePositionIn );
                end;


end.
