unit DrawingAxisConversionClass;

interface

    uses
        System.SysUtils, system.Math, system.Types, vcl.Controls,
        GeometryTypes,
        DrawingAxisConversionBaseClass, DrawingAxisConversionMouseControlClass;

    type
        TDrawingAxisConverter = class(TDrawingAxisMouseControlConverter)
            private
                //adjust the drawing region to assume the desired aspect ratio
                    procedure adjustDrawingRegionAspectRatio(const ratioIn : double);
            public
                //constructor
                    constructor create(const graphicControlComponentIn : TGraphicControl); override;
                //destructor
                    destructor destroy(); override;
                //set the drawing region range/domain ratio
                    procedure setDrawingSpaceRatio(const ratioIn : double); overload;
                    procedure setDrawingSpaceRatioOneToOne();
        end;

implementation

    //private
        //adjust the drawing region to assume the desired aspect ratio
            procedure TDrawingAxisConverter.adjustDrawingRegionAspectRatio(const ratioIn : double);
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

                    //calculate the new domain min and max
                        newDomainMin := domainMiddle - newDomain / 2;
                        newDomainMax := domainMiddle + newDomain / 2;

                    setDomain( newDomainMin, newDomainMax );
                end;

    //public
        //constructor
            constructor TDrawingAxisConverter.create(const graphicControlComponentIn : TGraphicControl);
                begin
                    inherited create( graphicControlComponentIn );
                end;

        //destructor
            destructor TDrawingAxisConverter.destroy();
                begin
                    inherited destroy();
                end;


            procedure TDrawingAxisConverter.setDrawingSpaceRatio(const ratioIn : double);
                var
                    currentZoomPercentage : double;
                begin
                    //catch current zoom percentage
                        currentZoomPercentage := getCurrentZoomPercentage();

                    //set the aspect ratio as desired
                        adjustDrawingRegionAspectRatio( ratioIn );

                    //set the correct zoom percentage
                        setZoom( currentZoomPercentage );
                end;

            procedure TDrawingAxisConverter.setDrawingSpaceRatioOneToOne();
                begin
                    setDrawingSpaceRatio(1);
                end;

end.
