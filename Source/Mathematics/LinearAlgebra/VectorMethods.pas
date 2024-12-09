unit VectorMethods;

interface

    uses
        system.SysUtils, system.Math, math.Vectors,
        LinearAlgebraTypes
        ;

    //test is vectors are the same size
        function vectorsAreSameSize(const vector1In, vector2In : TLAVector) : boolean;

    //copy vector
        procedure copyVector(   const readVectorIn  : TLAVector;
                                var writeVectorIn   : TLAVector     );

    //equality test
        function vectorsEqual(const vector1In, vector2In : TLAVector) : boolean;

    //scalar multiplication
        function vectorScalarMultiplication(const scalarIn  : double;
                                            const vectorIn  : TLAVector) : TLAVector;

    //vector addition
        function vectorAddition(const vector1In, vector2In : TLAVector) : TLAVector;

    //vector subtraction
        function vectorSubtraction(const vectorHeadIn, vectorTailIn : TLAVector) : TLAVector;

    //vector dot product
        function vectorDotProduct(const vector1In, vector2In : TLAVector) : double;

    //vector normalise
        function vectorNormalise(const vectorIn : TLAVector) : double;

    //vector entries product
        function vectorEntriesProduct(const vectorIn : TLAVector) : double;

    //unit vector
        function vectorUnitVector(const vectorIn : TLAVector) : TLAVector;

implementation

    //test is vectors are the same size
        function vectorsAreSameSize(const vector1In, vector2In : TLAVector) : boolean;
            var
                size1, size2 : integer;
            begin
                size1 := length(vector1In);
                size2 := length(vector2In);

                result := (size1 = size2);
            end;

    //copy vector
        procedure copyVector(   const readVectorIn  : TLAVector;
                                var writeVectorIn   : TLAVector     );
            var
                i, arrLen : integer;
            begin
                arrLen := length( readVectorIn );

                SetLength( writeVectorIn, arrLen );

                for i := 0 to (arrLen - 1) do
                    writeVectorIn[i] := readVectorIn[i];
            end;

    //equality test
        function vectorsEqual(const vector1In, vector2In : TLAVector) : boolean;
            var
                elementsAreEqual    : boolean;
                i                   : integer;
            begin
                if ( NOT(vectorsAreSameSize(vector1In, vector2In)) ) then
                    begin
                        result := False;
                        exit();
                    end;

                for i := 0 to (Length(vector1In) - 1) do
                    begin
                        elementsAreEqual := SameValue(vector1In[i], vector2In[i]);

                        if (NOT(elementsAreEqual)) then
                            begin
                                result := False;
                                exit();
                            end;
                    end;

                result := True;
            end;

    //scalar multiplication
        function vectorScalarMultiplication(const scalarIn  : double;
                                            const vectorIn  : TLAVector) : TLAVector;
            var
                i, size     : integer;
                vectorOut   : TLAVector;
            begin
                size := length(vectorIn);

                SetLength(vectorOut, size);

                for i := 0 to (size - 1) do
                    vectorOut[i] := scalarIn * vectorIn[i];

                result := vectorOut;
            end;

    //vector addition
        function vectorAddition(const vector1In, vector2In : TLAVector) : TLAVector;
            var
                i, size     : integer;
                vectorOut   : TLAVector;
            begin
                //sum can only occur if the vectors have the same size
                    if ( NOT(vectorsAreSameSize(vector1In, vector2In)) ) then
                        exit();

                //sum each component pair into the result vector
                    size := length(vector1In);

                    setlength(vectorOut, size);

                    for i := 0 to (size - 1) do
                        vectorOut[i] := vector1In[i] + vector2In[i];

                result := vectorOut;
            end;

    //vector subtraction
        function vectorSubtraction(const vectorHeadIn, vectorTailIn : TLAVector) : TLAVector;
            var
                negativeTailVector : TLAVector;
            begin
                //subtraction can only occur if the vectors have the same size
                    if ( NOT(vectorsAreSameSize(vectorHeadIn, vectorTailIn)) ) then
                        exit();

                //the tail is subtracted from the head
                    //subtraction is the same as adding a negative vector
                        negativeTailVector := vectorScalarMultiplication(-1, vectorTailIn);

                result := vectorAddition(vectorHeadIn, negativeTailVector);
            end;

    //vector dot product
        function vectorDotProduct(const vector1In, vector2In : TLAVector) : double;
            var
                i               : integer;
                dotProductSum   : double;
            begin
                //dot product can only occur if the vectors have the same size
                    if ( NOT(vectorsAreSameSize(vector1In, vector2In)) ) then
                        exit();

                //multiply each element of V1 and V2 and add
                    dotProductSum := 0;

                    for i := 0 to (Length(vector1In) - 1) do
                        dotProductSum := dotProductSum + (vector1In[i] * vector2In[i]);

                result := dotProductSum;
            end;

    //vector normalise
        function vectorNormalise(const vectorIn : TLAVector) : double;
            var
                i               : integer;
                entrySquared,
                normalOut,
                normalSquared   : double;
            begin
                normalSquared := 0;

                for i := 0 to (length(vectorIn) - 1) do
                    begin
                        entrySquared := Sqr(vectorIn[i]);

                        normalSquared := normalSquared + entrySquared;
                    end;

                normalOut := Sqrt(normalSquared);

                result := normalOut;
            end;

    //vector entries product
        function vectorEntriesProduct(const vectorIn : TLAVector) : double;
            var
                i           : integer;
                productOut  : double;
            begin
                productOut := 1;

                for i := 0 to (Length(vectorIn) - 1) do
                    productOut := productOut * vectorIn[i];

                result := productOut;
            end;

    //unit vector
        function vectorUnitVector(const vectorIn : TLAVector) : TLAVector;
            var
                i, arrLen       : integer;
                vectorLength    : double;
                unitVectorOut   : TLAVector;
            begin
                vectorLength := vectorNormalise( vectorIn );

                arrLen := length(vectorIn);

                SetLength( unitVectorOut, arrLen );

                for i := 0 to (arrLen - 1) do
                    unitVectorOut[i] := vectorIn[i] / vectorLength;

                result := unitVectorOut;
            end;

end.
