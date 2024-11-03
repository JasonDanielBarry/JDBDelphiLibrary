unit DrawingAxisConversionClass;

interface

    uses
        System.SysUtils, system.Math, system.Types,
        GeometryTypes,
        DrawingAxisConversionPanningClass;

    type
        TDrawingAxisConverter = class(TDrawingAxisPanningConverter)
            private
                //helper methods
                    procedure adjustDrawingRegionByDomain(const ratioIn : double);
                    procedure adjustDrawingRegionByRange(const ratioIn : double);
                //set the drawing region range/domain ratio
                    procedure setDrawingSpaceRatio( const adjustByDomainIn    : boolean;
                                                    const ratioIn             : double    ); overload;
            public
                //constructor
                    constructor create();
                //destructor
                    destructor destroy(); override;
                //set the drawing region range/domain ratio
                    procedure setDrawingSpaceRatio(const ratioIn : double); overload;
                    procedure setDrawingSpaceRatioOneToOne();
        end;

implementation

    //private
        //helper methods
            procedure TDrawingAxisConverter.adjustDrawingRegionByDomain(const ratioIn : double);
                var
                    drawDomain,
                    newRangeMin, newRangeMax,
                    rangeMiddle, newRange       : double;
                begin
                    drawDomain := calculateRegionDomain();

                    //calculate new range: R = D(1/r)(h/w)
                        newRange := (1 / ratioIn) * drawDomain * ( canvasHeight() / canvasWidth() );

                    //calculate the range middle
                        rangeMiddle := calculateRegionRangeCentre();

                    //calcualte the range top and bottom
                        newRangeMin := rangeMiddle - newRange / 2;
                        newRangeMax := rangeMiddle + newRange / 2;

                    setRange( newRangeMin, newRangeMax );
                end;

            procedure TDrawingAxisConverter.adjustDrawingRegionByRange(const ratioIn : double);
                var
                    drawRange,
                    newDomainMin, newDomainMax,
                    domainMiddle, newDomain     : double;
                begin
                    drawRange := calculateRegionRange();

                    //calculate new domain: D = R(r)(w/h)
                        newDomain := ratioIn * drawRange * ( canvasWidth() / canvasHeight() );

                    //calculate the domain middle
                        domainMiddle := calculateRegionDomainCentre();

                    //calculate the domain left and right
                        newDomainMin := domainMiddle - newDomain / 2;
                        newDomainMax := domainMiddle + newDomain / 2;

                    setDomain( newDomainMin, newDomainMax );
                end;

        //set the drawing region range/domain ratio
            procedure TDrawingAxisConverter.setDrawingSpaceRatio(   const adjustByDomainIn    : boolean;
                                                                    const ratioIn             : double    );
                var
                    currentZoomPercentage : double;
                begin
                    //catch current zoom percentage
                        currentZoomPercentage := getCurrentZoomPercentage();

                    //the ratio is defined as the value that satisfies: h/w = r(R/D)
                    //                                                  D/w = r(R/h)
                    //adjust-by-domain means that the domain remains constant and
                    //a new range is calculated to match the domain based on the input ratio

                    if (adjustByDomainIn) then
                        adjustDrawingRegionByDomain( ratioIn )
                    else
                        adjustDrawingRegionByRange( ratioIn );

                    //set the correct zoom percentage
                        setZoom( currentZoomPercentage );
                end;

    //public
        //constructor
            constructor TDrawingAxisConverter.create();
                begin
                    inherited create();
                end;

        //destructor
            destructor TDrawingAxisConverter.destroy();
                begin
                    inherited destroy();
                end;


            procedure TDrawingAxisConverter.setDrawingSpaceRatio(const ratioIn : double);
                var
                    adjustByDomain          : boolean;
                    domainRatio, rangeRatio : double;
                begin
                    //if the domain/width ratio is larger you must size by the domain
                        domainRatio := ( calculateRegionDomain() / canvasWidth() );

                    //if the range/height ratio is larger you must size by the range
                        rangeRatio := ratioIn * ( calculateRegionRange() / canvasHeight() );

                        adjustByDomain := ( domainRatio > rangeRatio );

                    setDrawingSpaceRatio(adjustByDomain, ratioIn);
                end;

            procedure TDrawingAxisConverter.setDrawingSpaceRatioOneToOne();
                begin
                    setDrawingSpaceRatio(1);
                end;

end.
