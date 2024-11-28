unit DrawingGeometryClass;

interface

    uses
        //Delphi
            system.SysUtils, system.types, system.UITypes, System.UIConsts,
        //custom
            ColourMethods,
            DrawingAxisConversionClass,
            GeometryTypes,
            GeometryBaseClass
            ;

    type
        TDrawingGeometry = class
            strict private
                var
                    lineThickness   : integer;
                    fillColour,
                    lineColour      : TColor;
                    geometry        : TGeomBase;
                    drawingPoints   : TArray<TGeomPoint>;
                //free geometry object
                    procedure freeGeometry();
                //set values
                    procedure setValues(const   lineThicknessIn : integer;
                                        const   geometryIn      : TGeomBase);
            public
                //constructor
                    constructor create( const   lineThicknessIn : integer;
                                        const   fillColourIn,
                                                lineColourIn    : TColor;
                                        const   geometryIn      : TGeomBase );
                //destructor
                    destructor destroy(); override;
                //line thickness
                    function getLineThickness() : integer;
                //colours
                    function getFillColour() : TColor;
                    function getLineColour() : TColor;
                //get the geometry object
                    function getGeometry() : TGeomBase;
                //get drawing points
                    function getDrawingPoints() : TArray<TGeomPoint>;
        end;

implementation

    //private
        procedure TDrawingGeometry.freeGeometry();
            begin
                try
                    FreeAndNil( geometry );
                except

                end;
            end;

        //set values
            procedure TDrawingGeometry.setValues(   const   lineThicknessIn : integer;
                                                    const   geometryIn      : TGeomBase );
                begin
                    freeGeometry();

                    lineThickness   := lineThicknessIn;
                    geometry        := geometryIn;

                    drawingPoints   := geometryIn.getDrawingPoints();
                end;

    //public
        //constructor
            constructor TDrawingGeometry.create(const   lineThicknessIn : integer;
                                                const   fillColourIn,
                                                        lineColourIn    : TColor;
                                                const   geometryIn      : TGeomBase );
                begin
                    inherited create();

                    setValues( lineThicknessIn, geometryIn );

                    fillColour := fillColourIn;
                    lineColour := lineColourIn;

                    drawingPoints := geometryIn.getDrawingPoints();
                end;

        //destructor
            destructor TDrawingGeometry.destroy();
                begin
                    freeGeometry();

                    inherited destroy();
                end;

        //line thickness
            function TDrawingGeometry.getLineThickness() : integer;
                begin
                    result := lineThickness;
                end;

        //colours
            function TDrawingGeometry.getFillColour() : TColor;
                begin
                    result := fillColour;
                end;

            function TDrawingGeometry.getLineColour() : TColor;
                begin
                    result := lineColour;
                end;

        //get the geometry object
            function TDrawingGeometry.getGeometry() : TGeomBase;
                begin
                    result := geometry;
                end;

        //get drawing points
            function TDrawingGeometry.getDrawingPoints() : TArray<TGeomPoint>;
                begin
                    result := drawingPoints;
                end;

end.
