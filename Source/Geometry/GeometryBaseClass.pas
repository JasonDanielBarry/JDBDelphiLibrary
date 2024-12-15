unit GeometryBaseClass;

interface

    uses
        system.SysUtils, system.Math,
        GraphicDrawingTypes,
        GeometryTypes, GeomBox,
        DrawingAxisConversionClass;

    type
        TGeomBase = class
            private
                //
            protected
                var
                    arrGeomPoints : TArray<TGeomPoint>;
            public
                //constructor
                    constructor create();
                //destructor
                    destructor destroy(); override;
                //get drawing type
                    function getDrawingType() : EGraphicObjectType; virtual; abstract;
                //bounding box
                    function boundingBox() : TGeomBox; virtual; abstract;
                    class function determineBoundingBox(arrGeomIn : TArray<TGeomBase>) : TGeomBox; static;
                //get drawing points
                    function getDrawingPoints() : TArray<TGeomPoint>; virtual; abstract;
                //shift the geometry
                    procedure shift(const deltaXIn, deltaYIn, deltaZIn : double); overload; virtual; abstract;
                    procedure shift(const deltaXIn, deltaYIn : double); overload;
        end;



implementation

    constructor TGeomBase.create();
        begin
            inherited create();
        end;

    destructor TGeomBase.destroy();
        begin
            SetLength(arrGeomPoints, 0);

            inherited destroy();
        end;

    procedure TGeomBase.shift(const deltaXIn, deltaYIn : double);
        begin
            self.shift(deltaXIn, deltaYIn, 0);
        end;

    class function TGeomBase.determineBoundingBox(arrGeomIn : TArray<TGeomBase>) : TGeomBox;
        var
            i, geomCount    : integer;
            boxOut          : TGeomBox;
            arrGeomBox      : TArray<TGeomBox>;
        begin
            geomCount := length(arrGeomIn);

            if (geomCount < 1) then
                exit();

            SetLength(arrGeomBox, geomCount);

            for i := 0 to (geomCount - 1) do
                arrGeomBox[i] := arrGeomIn[i].boundingBox();

            boxOut := TGeomBox.create(arrGeomBox);

            result := boxOut;
        end;

end.
