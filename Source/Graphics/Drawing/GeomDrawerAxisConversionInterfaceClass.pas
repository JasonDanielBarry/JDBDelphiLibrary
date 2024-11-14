unit GeomDrawerAxisConversionInterfaceClass;

interface

    uses
        system.SysUtils, system.math, system.Types,
        Winapi.Messages,
        vcl.Controls,
        DrawingAxisConversionClass,
        GeomDrawerBaseClass
        ;

    type
        TGeomDrawerAxisConversionInterface = class(TGeomDrawerBase)
            public
                //constructor
                    constructor create(); override;
                //destructor
                    destructor destroy(); override;
                //axis conversion interface
                    //drawing region


                        asdf    //make methods to get and set the drawing region
                                //also modify the TGeomBox so that is has the functionality contained in the axis converter base class
                                //so that the region can be used as easily as possible to get rid of unnecessary functions
                                //to improve performance


                    //panning
                        procedure recentre();
                        procedure shiftDomain(const percentageIn : double);
                        procedure shiftRange(const percentageIn : double);
                    //zooming
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
                    result := axisConverter.processWindowsMessages( messageIn, newMousePositionIn );
                end;


end.
