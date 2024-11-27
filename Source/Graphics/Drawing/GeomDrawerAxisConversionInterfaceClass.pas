unit GeomDrawerAxisConversionInterfaceClass;

interface

    uses
        system.SysUtils, system.math, system.Types,
        Winapi.Messages,
        vcl.Controls,
        GeometryTypes, GeomBox,
        DrawingAxisConversionClass,
        GeomDrawerLayersClass
        ;

    type
        TGeomDrawerAxisConversionInterface = class(TGeomDrawerLayers)
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
                        function getMouseCoordinatesXY() : TGeomPoint; inline;
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
                    function getMouseControlActive() : boolean; inline;
                    function processWindowsMessages(const messageIn             : Tmessage;
                                                    const newMousePositionIn    : TPoint    ) : boolean;
        end;

implementation

    //public
        //constructor
            constructor TGeomDrawerAxisConversionInterface.create();
                begin
                    inherited create();
                end;

        //destructor
            destructor TGeomDrawerAxisConversionInterface.destroy();
                begin
                    inherited destroy();
                end;

        //axis conversion methods
            //drawing region
                function TGeomDrawerAxisConversionInterface.getDrawingRegion() : TGeomBox;
                    begin
                        result := axisConverter.getDrawingRegion();
                    end;

                procedure TGeomDrawerAxisConversionInterface.setDrawingRegion(  const bufferIn : double;
                                                                                const regionIn : TGeomBox   );
                    begin
                        axisConverter.setDrawingRegion(bufferIn, regionIn);
                    end;

            //draw space ratio
                procedure TGeomDrawerAxisConversionInterface.setDrawingSpaceRatio(const ratioIn : double);
                    begin
                        axisConverter.setDrawingSpaceRatio( ratioIn );
                    end;

            //mouse coordinates
                function TGeomDrawerAxisConversionInterface.getMouseCoordinatesXY() : TGeomPoint;
                    begin
                        result := axisConverter.getMouseCoordinatesXY();
                    end;

            //panning
                procedure TGeomDrawerAxisConversionInterface.recentre();
                    begin
                        axisConverter.recentreDrawingRegion();
                    end;

                procedure TGeomDrawerAxisConversionInterface.shiftDomain(const percentageIn : double);
                    var
                        domainShift, regionDomain : double;
                    begin
                        regionDomain := axisConverter.calculateRegionDomain();

                        domainShift := (percentageIn / 100) * regionDomain;

                        axisConverter.shiftDrawingDomain( domainShift );
                    end;

                procedure TGeomDrawerAxisConversionInterface.shiftRange(const percentageIn : double);
                    var
                        rangeShift, regionRange : double;
                    begin
                        regionRange := axisConverter.calculateRegionDomain();

                        rangeShift := (percentageIn / 100) * regionRange;

                        axisConverter.shiftDrawingRange( rangeShift );
                    end;

            //zooming
                function TGeomDrawerAxisConversionInterface.getCurrentZoomPercentage() : double;
                    begin
                        result := axisConverter.getCurrentZoomPercentage();
                    end;

                procedure TGeomDrawerAxisConversionInterface.setZoom(const percentageIn : double);
                    begin
                        axisConverter.setZoom( percentageIn );
                    end;

                procedure TGeomDrawerAxisConversionInterface.zoomIn(const percentageIn : double);
                    begin
                        axisConverter.zoomIn( percentageIn );
                    end;

                procedure TGeomDrawerAxisConversionInterface.zoomOut(const percentageIn : double);
                    begin
                        axisConverter.zoomOut( percentageIn );
                    end;

                procedure TGeomDrawerAxisConversionInterface.zoomAll();
                    begin
                        axisConverter.resetDrawingRegionToGeometryBoundary();
                    end;

        //process windows messages
            procedure TGeomDrawerAxisConversionInterface.activateMouseControl();
                begin
                    axisConverter.activateMouseControl();
                end;

            procedure TGeomDrawerAxisConversionInterface.deactivateMouseControl();
                begin
                    axisConverter.deactivateMouseControl();
                end;

            function TGeomDrawerAxisConversionInterface.getMouseControlActive() : boolean;
                begin
                    result := axisConverter.MouseControlActive;
                end;

            function TGeomDrawerAxisConversionInterface.processWindowsMessages( const messageIn             : Tmessage;
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
