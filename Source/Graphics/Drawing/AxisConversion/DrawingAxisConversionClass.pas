unit DrawingAxisConversionClass;

interface

    uses
        System.SysUtils, system.Math, system.Types,
        GeometryTypes,
        DrawingAxisConversionPanningClass;

    type
        TDrawingAxisConverter = class(TDrawingAxisPanningConverter)
            public
                //constructor
                    constructor create();
                //destructor
                    destructor destroy(); override;
                //set the drawing region range/domain ratio
                    procedure setDrawingSpaceRatio( const adjustByDomainIn    : boolean;
                                                        const ratioIn             : double    );
                    procedure setDrawingSpaceRatioOneToOne();
        end;

implementation

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

        //set the drawing region range/domain ratio
            procedure TDrawingAxisConverter.setDrawingSpaceRatio(   const adjustByDomainIn    : boolean;
                                                                const ratioIn             : double    );
                begin
                    //the ratio is defined as the value that satisfies: h/w = r(R/D)

                    //adjust-by-domain means that the domain remains constant and
                    //a new range is calculated to match the domain based on the input ratio

                    if (adjustByDomainIn) then
                        begin
                            var drawDomain, newRange, newRangeMin, rangeMiddle, newRangeMax : double;

                            drawDomain := calculateRegionDomain();

                            //calculate new range: R = D(1/r)(h/w)
                                newRange := (1 / ratioIn) * drawDomain * ( canvasHeight() / canvasWidth() );

                            //calculate the range middle
                                rangeMiddle := calculateRegionRangeCentre();

                            //calcualte the range top and bottom
                                newRangeMin := rangeMiddle - newRange / 2;
                                newRangeMax := rangeMiddle + newRange / 2;

                            setRange( newRangeMin, newRangeMax );
                        end
                    else
                        begin
                            var drawRange, newDomain, newDomainMin, domainMiddle, newDomainMax : double;

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
                end;

            procedure TDrawingAxisConverter.setDrawingSpaceRatioOneToOne();
                var
                    adjustByDomain          : boolean;
                    domainRatio, rangeRatio : double;
                    drawingRegionTemp       : TGeomBox;
                begin
                    drawingRegionTemp := drawingRegion;

//                        //this function ensures that if a drawing space ratio is set 1:1 the drawing does not shrink as the window is resized
//                        //must be called before adjustByDomain is calculated
//                            zoomForConstantDrawingSpaceRatio();

                    //if the domain/width ratio is larger you must size by the domain
                        domainRatio := ( calculateRegionDomain() / canvasWidth() );

                    //if the range/height ratio is larger you must size by the range
                        rangeRatio := ( calculateRegionRange() / canvasHeight() );

                        adjustByDomain := ( domainRatio > rangeRatio );

                    setDrawingSpaceRatio(adjustByDomain, 1);

                    //shift to correct position
//                            panForConstantDrawingSpaceRatio(drawingRegionTemp);
                end;

        

end.
