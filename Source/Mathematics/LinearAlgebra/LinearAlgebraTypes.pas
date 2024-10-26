unit LinearAlgebraTypes;

interface

    type
        TLAVector = Tarray<double>;
        TLAMatrix = TArray< TLAVector >;

        TLAMatrixSize = record
            rows, cols  : integer;
            function isSquare() : boolean;
            function isEqual(const sizeIn : TLAMatrixSize) : boolean;
        end;

implementation

    function TLAMatrixSize.isSquare() : boolean;
        begin
            result := (rows = cols);
        end;

    function TLAMatrixSize.isEqual(const sizeIn : TLAMatrixSize) : boolean;
        var
            colsEqual, rowsEqual : boolean;
        begin
            colsEqual := (self.cols = sizeIn.cols);
            rowsEqual := (self.rows = sizeIn.rows);

            result := (colsEqual AND rowsEqual);
        end;

end.
