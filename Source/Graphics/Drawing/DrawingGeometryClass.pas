unit DrawingGeometryClass;

interface

    uses
        //Delphi
            system.SysUtils, system.types, system.UITypes, System.UIConsts,
        //custom
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
                    lineColour      : TAlphaColor;
                    geometry        : TGeomBase;
                //free geometry object
                    procedure freeGeometry();
            public
                //constructor
                    constructor create( const   lineThicknessIn : integer;
                                        const   fillColourIn,
                                                lineColourIn    : TAlphaColor;
                                        const   geometryIn      : TGeomBase     );
                //destructor
                    destructor destroy(); override;
                //line thickness
                    function getLineThickness() : integer;
                //colours
                    function getFillColour() : TAlphaColor;
                    function getLineColour() : TAlphaColor;
                //get the geometry object
                    function getGeometry() : TGeomBase;
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
                                                        lineColourIn    : TAlphaColor;
                                                const   geometryIn      : TGeomBase );
                begin
                    inherited create();

                    freeGeometry();

                    lineThickness   := lineThicknessIn;
                    fillColour      := fillColourIn;
                    lineColour      := lineColourIn;
                    geometry        := geometryIn;
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
            function TDrawingGeometry.getFillColour() : TAlphaColor;
                begin
                    result := fillColour;
                end;

            function TDrawingGeometry.getLineColour() : TAlphaColor;
                begin
                    result := lineColour;
                end;

        //get the geometry object
            function TDrawingGeometry.getGeometry() : TGeomBase;
                begin
                    result := geometry;
                end;

end.