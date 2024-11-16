unit DrawingAxisConversionBaseClass;

interface

    uses
        System.SysUtils, system.Math, system.Types,
        GeometryTypes,
        GeomBox;

    type
        TDrawingAxisConverterBase = class
            protected
                var
                    canvasDimensions    : TRect;
                    drawingRegion       : TGeomBox;
                //modifiers
                    //drawing space boundaries
                        procedure setDrawingRegion(const domainMinIn, domainMaxIn, rangeMinIn, rangeMaxIn : double); overload;
                        procedure setDrawingRegion( const bufferIn : double;
                                                    const regionIn : TGeomBox ); overload; virtual;
            public
                //constructor
                    constructor create(); virtual;
                //destructor
                    destructor destroy(); override;
                //accessors
                    function getDrawingRegion() : TGeomBox;
                //modifiers
                    //canvas boundaries
                        procedure setCanvasDimensions(const canvasHeightIn, canvasWidthIn : integer); inline;
                //helper methods
                    //domain
                        function calculateRegionDomain() : double;
                    //range
                        function calculateRegionRange() : double;

        end;

implementation

    //protected
        //modifiers
            //drawingRegion space boundaries
                procedure TDrawingAxisConverterBase.setDrawingRegion(const domainMinIn, domainMaxIn, rangeMinIn, rangeMaxIn : double);
                    begin
                        drawingRegion.setBounds(domainMinIn,    domainMaxIn,
                                                rangeMinIn,     rangeMaxIn,
                                                0,              0           );
                    end;
        
    //public
        //constructor
            constructor TDrawingAxisConverterBase.create();
                begin
                    inherited create();

                    canvasDimensions.Left   := 0;
                    canvasDimensions.Top    := 0;

                    setDrawingRegion( 0, 0, 0, 0 );
                end;

        //destructor
            destructor TDrawingAxisConverterBase.destroy();
                begin
                    inherited destroy();
                end;

        //accessors
            function TDrawingAxisConverterBase.getDrawingRegion() : TGeomBox;
                begin
                    result := drawingRegion;
                end;

        //modifiers
            //canvasSpace boundaries
                procedure TDrawingAxisConverterBase.setCanvasDimensions(const canvasHeightIn, canvasWidthIn : integer);
                    begin
                        canvasDimensions.Height := canvasHeightIn;
                        canvasDimensions.Width := canvasWidthIn;
                    end;

            //drawingRegion space boundaries
                procedure TDrawingAxisConverterBase.setDrawingRegion(   const bufferIn : double;
                                                                        const regionIn : TGeomBox );
                    var
                        buffer,
                        regionInDomain, domainBuffer,
                        newDomainMin,   newDomainMax,
                        regionInRange,  rangeBuffer,
                        newRangeMin,    newRangeMax     : double;
                    begin
                        //set valid buffer
                            buffer := min(5, bufferIn);
                            buffer := max(buffer, 0);

                        //test buffer is valid
                            if (bufferIn < 0) then
                                exit();

                        //calculate the domain and range of regionIn
                            regionInDomain   := regionIn.maxPoint.x - regionIn.minPoint.x;
                            regionInRange    := regionIn.maxPoint.y - regionIn.minPoint.y;

                        //calculate the region buffers
                            domainBuffer := (buffer / 100) * regionInDomain;
                            rangeBuffer  := (buffer / 100) * regionInRange;

                        //calculate new mins and maxes
                            newDomainMin := regionIn.minPoint.x - domainBuffer / 2;
                            newDomainMax := regionIn.maxPoint.x + domainBuffer / 2;

                            newRangeMin := regionIn.minPoint.y - rangeBuffer  / 2;
                            newRangeMax := regionIn.maxPoint.y + rangeBuffer  / 2;

                        setDrawingRegion( newDomainMin, newDomainMax, newRangeMin, newRangeMax );
                    end;

        //helper methods
            //domain
                function TDrawingAxisConverterBase.calculateRegionDomain() : double;
                    begin
                        result := drawingRegion.calculateXDimension();
                    end;

            //range
                function TDrawingAxisConverterBase.calculateRegionRange() : double;
                    begin
                        result := drawingRegion.calculateYDimension();
                    end;

end.
