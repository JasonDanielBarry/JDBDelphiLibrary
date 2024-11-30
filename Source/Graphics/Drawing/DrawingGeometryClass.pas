unit DrawingGeometryClass;

interface

    uses
        //Delphi
            system.SysUtils, system.types, system.UITypes, System.UIConsts,
        //custom
            ColourMethods,
            DrawingTypes,
            DrawingObjectBaseClass,
            DrawingAxisConversionClass,
            GeometryTypes,
            GeometryBaseClass
            ;

    type
        TDrawingGeometry = class(TDrawingObject)
            strict private
                var
                    lineThickness   : integer;
                    fillColour,
                    lineColour      : TColor;
                    geometry        : TGeomBase;
                    drawingPoints   : TArray<TGeomPoint>;
                //free geometry object
                    procedure freeGeometry();
            public
                //constructor
                    constructor create( const   lineThicknessIn : integer;
                                        const   fillColourIn,
                                                lineColourIn    : TColor;
                                        const   geometryIn      : TGeomBase );
                //destructor
                    destructor destroy(); override;
                //line thickness
                    function getLineThickness() : integer; inline;
                //colours
                    function getFillColour() : TColor; inline;
                    function getLineColour() : TColor; inline;
                //get the geometry object
                    function getGeometry() : TGeomBase; inline;
                //get drawing points
                    function getDrawingPoints() : TArray<TGeomPoint>; inline;
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

    //public
        //constructor
            constructor TDrawingGeometry.create(const   lineThicknessIn : integer;
                                                const   fillColourIn,
                                                        lineColourIn    : TColor;
                                                const   geometryIn      : TGeomBase );
                begin
                    inherited create();

                    freeGeometry();

                    lineThickness   := lineThicknessIn;
                    geometry        := geometryIn;
                    fillColour      := fillColourIn;
                    lineColour      := lineColourIn;

                    drawingPoints := geometryIn.getDrawingPoints();

                    setDrawingType( geometry.getDrawingType );
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
