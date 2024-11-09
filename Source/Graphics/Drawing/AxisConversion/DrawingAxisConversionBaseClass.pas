unit DrawingAxisConversionBaseClass;

interface

    uses
        System.SysUtils, system.Math, system.Types,
        GeometryTypes;

    type
        TDrawingAxisConverterBase = class
            private
                var
                    canvasSpace : TRect;
                //modifiers
                    //canvas boundaries
                        procedure setCanvasHeight(const heightIn : integer); inline;
                        procedure setCanvasWidth(const widthIn : integer); inline;
                    //drawing space boundaries
                        //domain
                            procedure setDomainMin(const domainMinIn : double); inline;
                            procedure setDomainMax(const domainMaxIn : double); inline;
                        //range
                            procedure setRangeMin(const rangeMinIn : double); inline;
                            procedure setRangeMax(const rangeMaxIn : double); inline;
            protected
                var
                    drawingRegion : TGeomBox;
                //helper methods
                    //canvas
                        function canvasHeight() : integer; inline;
                        function canvasWidth() : integer; inline;
                    //domain
                        function regionDomainMin() : double; inline;
                        function regionDomainMax() : double; inline;
                        function calculateRegionDomainCentre() : double; inline;
                    //range
                        function regionRangeMin() : double; inline;
                        function regionRangeMax() : double; inline;
                        function calculateRegionRangeCentre() : double; inline;
                //modifiers
                    //drawing space boundaries
                        procedure setDomain(const domainMinIn, domainMaxIn : double); inline;
                        procedure setRange(const rangeMinIn, rangeMaxIn : double); inline;
                        procedure setDrawingRegion(const domainMinIn, domainMaxIn, rangeMinIn, rangeMaxIn : double); overload; inline;
                        procedure setDrawingRegion( const bufferIn : double;
                                                    const regionIn : TGeomBox ); overload; virtual;
            public
                //constructor
                    constructor create();
                //destructor
                    destructor destroy(); override;
                //accessors
                    function getDrawingRegion() : TGeomBox;
                //modifiers
                    //canvas boundaries
                        procedure setCanvasRegion(const heightIn, widthIn : integer);
                //helper methods
                    //domain
                        function calculateRegionDomain() : double;
                    //range
                        function calculateRegionRange() : double;

        end;

implementation

    //private
        //modifiers
            //drawingRegion space boundaries
                //x bounds
                    procedure TDrawingAxisConverterBase.setDomainMin(const domainMinIn : double);
                        begin
                            drawingRegion.minPoint.x := domainMinIn;
                        end;

                    procedure TDrawingAxisConverterBase.setDomainMax(const domainMaxIn : double);
                        begin
                            drawingRegion.maxPoint.x := domainMaxIn;
                        end;

                //y bounds
                    procedure TDrawingAxisConverterBase.setRangeMin(const rangeMinIn : double);
                        begin
                            drawingRegion.minPoint.y := rangeMinIn;
                        end;

                    procedure TDrawingAxisConverterBase.setRangeMax(const rangeMaxIn : double);
                        begin
                            drawingRegion.maxPoint.y := rangeMaxIn;
                        end;



    //protected
        //helper methods
            //canvas
                function TDrawingAxisConverterBase.canvasHeight() : integer;
                    begin
                        result := canvasSpace.Height;
                    end;

                function TDrawingAxisConverterBase.canvasWidth() : integer;
                    begin
                        result := canvasSpace.Width;
                    end;

            //domain
                function TDrawingAxisConverterBase.regionDomainMin() : double;
                    begin
                        result := drawingRegion.minPoint.x;
                    end;

                function TDrawingAxisConverterBase.regionDomainMax() : double;
                    begin
                        result := drawingRegion.maxPoint.x;
                    end;

                function TDrawingAxisConverterBase.calculateRegionDomainCentre() : double;
                    begin
                        result := drawingRegion.getCentreX();
                    end;

            //range
                function TDrawingAxisConverterBase.regionRangeMin() : double;
                    begin
                        result := drawingRegion.minPoint.y;
                    end;

                function TDrawingAxisConverterBase.regionRangeMax() : double;
                    begin
                        result := drawingRegion.maxPoint.y;
                    end;

                function TDrawingAxisConverterBase.calculateRegionRangeCentre() : double;
                    begin
                        result := drawingRegion.getCentreY();
                    end;

        //modifiers
            //canvasSpace boundaries
                procedure TDrawingAxisConverterBase.setCanvasHeight(const heightIn : integer);
                    begin
                        canvasSpace.height := heightIn;
                    end;

                procedure TDrawingAxisConverterBase.setCanvasWidth(const widthIn : integer);
                    begin
                        canvasSpace.width := widthIn;
                    end;

            //drawingRegion space boundaries
                procedure TDrawingAxisConverterBase.setDomain(const domainMinIn, domainMaxIn : double);
                    begin
                        setDomainMin(domainMinIn);
                        setDomainMax(domainMaxIn);
                    end;

                procedure TDrawingAxisConverterBase.setRange(const rangeMinIn, rangeMaxIn : double);
                    begin
                        setRangeMin(rangeMinIn);
                        setRangeMax(rangeMaxIn);
                    end;

                procedure TDrawingAxisConverterBase.setDrawingRegion(const domainMinIn, domainMaxIn, rangeMinIn, rangeMaxIn : double);
                    begin
                        setDomain(domainMinIn, domainMaxIn);
                        setRange(rangeMinIn, rangeMaxIn);
                    end;
        
    //public
        //constructor
            constructor TDrawingAxisConverterBase.create();
                begin
                    inherited create();

                    canvasSpace.Left := 0;
                    canvasSpace.Top  := 0;

                    drawingRegion.minPoint.setPoint( 0, 0, 0 );
                    drawingRegion.maxPoint.setPoint( 0, 0, 0 );
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
                procedure TDrawingAxisConverterBase.setCanvasRegion(const heightIn, widthIn : integer);
                    begin
                        setCanvasHeight(heightIn);
                        setCanvasWidth(widthIn);
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
                            domainBuffer := (bufferIn / 100) * regionInDomain;
                            rangeBuffer  := (bufferIn / 100) * regionInRange;

                        //calculate new mins and maxes
                            newDomainMin := regionIn.minPoint.x - domainBuffer / 2;
                            newDomainMax := regionIn.maxPoint.x + domainBuffer / 2;

                            newRangeMin := regionIn.minPoint.y - rangeBuffer  / 2;
                            newRangeMax := regionIn.maxPoint.y + rangeBuffer  / 2;

                        setDrawingRegion(newDomainMin, newDomainMax, newRangeMin, newRangeMax);
                    end;

        //helper methods
            //domain
                function TDrawingAxisConverterBase.calculateRegionDomain() : double;
                    begin
                        result := regionDomainMax() - regionDomainMin();
                    end;

            //range
                function TDrawingAxisConverterBase.calculateRegionRange() : double;
                    begin
                        result := regionRangeMax() - regionRangeMin();
                    end;



end.
