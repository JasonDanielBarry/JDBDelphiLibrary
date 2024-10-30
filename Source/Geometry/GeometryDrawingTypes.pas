unit GeometryDrawingTypes;

interface

    uses
        //Delphi
            system.SysUtils, system.types, system.UITypes,
        //custom
            GeometryTypes,
            GeometryBaseClass,
            GeomLineClass, GeomPolyLineClass
            ;

    type
//----------------------------------------------------------------------------------------------------
        TDrawingGeometry = class
            private
                var
                    lineThickness   : integer;
                    geometry        : TGeomBase;
                procedure freeGeometry();
            public
                constructor create( const   lineThicknessIn : integer;
                                    const   geometryIn      : TGeomBase     );
                destructor destroy(); override;
                function getLineThickness() : integer;
                function getGeometry() : TGeomBase;
        end;
//----------------------------------------------------------------------------------------------------
        TSkiaDrawingGeometry = class(TDrawingGeometry)
            private
                var
                    fillColour, lineColour : TAlphaColor;
            public
                constructor create( const   lineThicknessIn : integer;
                                    const   fillColourIn,
                                            lineColourIn    : TAlphaColor;
                                    const   geometryIn      : TGeomBase     );
                destructor destroy(); override;
                function getFillColour() : TAlphaColor;
                function getLineColour() : TAlphaColor;
        end;
//----------------------------------------------------------------------------------------------------
        TD2DDrawingGeometry = class(TDrawingGeometry)
            private
                var
                    fillColour, lineColour : TColor;
            public
                constructor create( const   lineThicknessIn : integer;
                                    const   fillColourIn,
                                            lineColourIn    : TColor;
                                    const   geometryIn      : TGeomBase );
                destructor destroy(); override;
                function getFillColour() : TColor;
                function getLineColour() : TColor;
        end;
//----------------------------------------------------------------------------------------------------

implementation

//----------------------------------------------------------------------------------------------------
    //private
        procedure TDrawingGeometry.freeGeometry();
            begin
                try
                    FreeAndNil( geometry );
                except

                end;
            end;

    //public
        constructor TDrawingGeometry.create(const   lineThicknessIn : integer;
                                            const   geometryIn      : TGeomBase     );
            begin
                inherited create();

                freeGeometry();

                lineThickness   := lineThicknessIn;
                geometry        := geometryIn;
            end;

        destructor TDrawingGeometry.destroy();
            begin
                freeGeometry();

                inherited destroy();
            end;

        function TDrawingGeometry.getLineThickness() : integer;
            begin
                result := lineThickness;
            end;

        function TDrawingGeometry.getGeometry() : TGeomBase;
            begin
                result := geometry;
            end;
//----------------------------------------------------------------------------------------------------
    constructor TSkiaDrawingGeometry.create(const   lineThicknessIn : integer;
                                            const   fillColourIn,
                                                    lineColourIn    : TAlphaColor;
                                            const   geometryIn      : TGeomBase     );
        begin
            inherited create(lineThicknessIn, geometryIn);

            fillColour := fillColourIn;
            lineColour := lineColourIn;
        end;

    destructor TSkiaDrawingGeometry.destroy();
        begin
            inherited destroy();
        end;

    function TSkiaDrawingGeometry.getFillColour() : TAlphaColor;
        begin
            result := fillColour;
        end;

    function TSkiaDrawingGeometry.getLineColour() : TAlphaColor;
        begin
            result := lineColour;
        end;
//----------------------------------------------------------------------------------------------------
    constructor TD2DDrawingGeometry.create( const   lineThicknessIn : integer;
                                            const   fillColourIn,
                                                    lineColourIn    : TColor;
                                            const   geometryIn      : TGeomBase );
        begin
            inherited create(lineThicknessIn, geometryIn);

            fillColour := fillColourIn;
            lineColour := lineColourIn;
        end;

    destructor TD2DDrawingGeometry.destroy();
        begin
            inherited destroy();
        end;

    function TD2DDrawingGeometry.getFillColour() : TColor;
        begin
            result := fillColour;
        end;

    function TD2DDrawingGeometry.getLineColour() : TColor;
        begin
            result := lineColour;
        end;
//----------------------------------------------------------------------------------------------------

end.
