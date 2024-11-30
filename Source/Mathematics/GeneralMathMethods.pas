unit GeneralMathMethods;

interface

    uses
        System.SysUtils, system.Math, system.Math.Vectors;

    //max betweem three values
        function max(const value1In, value2In, value3In : double) : double; overload;

    //scale line
        procedure scaleLinear(  const   startValueIn,       endValueIn,
                                        scaleAboutValueIn,  scaleFactorIn   : double;
                                out     newStartValueOut,   newEndValueOut  : double );

        procedure resizeLine(   const   startValueIn,       endValueIn,
                                        scaleAboutValueIn,  newLengthIn     : double;
                                out     newStartValueOut,   newEndValueOut  : double    );

implementation

    //max betweem three values
        function max(const value1In, value2In, value3In : double) : double;
            begin
                result := system.math.max(value1In, value2In);

                result := system.math.max(result, value3In);
            end;

    //scale line
        procedure scaleLinear(  const   startValueIn,       endValueIn,
                                        scaleAboutValueIn,  scaleFactorIn   : double;
                                out     newStartValueOut,   newEndValueOut  : double );
            var
                changeFactor,
                lengthChange, lengthLow, lengthHigh, lengthOld : double;
            begin
                //check if the start value and end value are identical - cannot divide by zero
                    if ( SameValue(startValueIn, endValueIn) ) then
                        begin
                            newStartValueOut := startValueIn;
                            newEndValueOut := endValueIn;

                            exit();
                        end;

                //calculate old length
                    lengthOld := endValueIn - startValueIn;

                //calculate length low and high
                    lengthLow   := scaleAboutValueIn - startValueIn;
                    lengthHigh  := endValueIn - scaleAboutValueIn;

                //calculate change in length
                    lengthChange := (scaleFactorIn - 1) * lengthOld;

                //calculate change factor
                    changeFactor := lengthChange / lengthOld;

                //calculate new start value
                    newStartValueOut := startValueIn - changeFactor * lengthLow;

                //calculate new end value
                    newEndValueOut := endValueIn + changeFactor * lengthHigh;
            end;

        procedure resizeLine(   const   startValueIn,       endValueIn,
                                        scaleAboutValueIn,  newLengthIn     : double;
                                out     newStartValueOut,   newEndValueOut  : double    );
            var
                scaleFactor : double;
            begin
                //the scale factor is the ratio of the new line length to the old line length
                    scaleFactor := newLengthIn / (endValueIn - startValueIn);

                scaleLinear(startValueIn,       endValueIn,
                            scaleAboutValueIn,  scaleFactor,
                            newStartValueOut,   newEndValueOut);
            end;




end.
