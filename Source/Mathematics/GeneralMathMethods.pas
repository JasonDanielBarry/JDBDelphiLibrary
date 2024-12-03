unit GeneralMathMethods;

interface

    uses
        System.SysUtils, system.Math, system.Math.Vectors;

    //max betweem three values
        function max(const value1In, value2In, value3In : double) : double; overload;

    //scale line
        procedure scaleLinear(  const   startValueIn, endValueIn,
                                        scaleFactorIn           : double;
                                out     newEndValueOut          : double ); overload;

        procedure scaleLinear(  const   startValueIn,       endValueIn,
                                        scaleAboutValueIn,  scaleFactorIn   : double;
                                out     newStartValueOut,   newEndValueOut  : double ); overload;

        procedure resizeLine(   const   startValueIn, endValueIn,
                                        newLengthIn             : double;
                                out     newEndValueOut          : double    ); overload;

        procedure resizeLine(   const   startValueIn,       endValueIn,
                                        scaleAboutValueIn,  newLengthIn     : double;
                                out     newStartValueOut,   newEndValueOut  : double    ); overload;

implementation

    //max betweem three values
        function max(const value1In, value2In, value3In : double) : double;
            begin
                result := system.math.max(value1In, value2In);

                result := system.math.max(result, value3In);
            end;

    //scale line
        procedure scaleLinear(  const   startValueIn, endValueIn,
                                        scaleFactorIn           : double;
                                out     newEndValueOut          : double );
            var
                changeFactor,
                lengthChange, lengthLow, lengthHigh, lengthOld : double;
            begin
                //check if the start value and end value are identical - cannot divide by zero
                    if ( SameValue(startValueIn, endValueIn) ) then
                        begin
                            newEndValueOut := endValueIn;

                            exit();
                        end;

                //calculate old length
                    lengthOld := endValueIn - startValueIn;

                //calculate change in length
                    lengthChange := (scaleFactorIn - 1) * lengthOld;

                //calculate new end value
                    newEndValueOut := endValueIn + lengthChange;
            end;

        procedure scaleLinear(  const   startValueIn,       endValueIn,
                                        scaleAboutValueIn,  scaleFactorIn   : double;
                                out     newStartValueOut,   newEndValueOut  : double );
            begin
                //scale from start value to scale-about-value
                    scaleLinear( scaleAboutValueIn, startValueIn, scaleFactorIn, newStartValueOut );

                //scale from scale-about-value to end value
                    scaleLinear( scaleAboutValueIn, endValueIn, scaleFactorIn, newEndValueOut );
            end;

        procedure resizeLine(   const   startValueIn, endValueIn,
                                        newLengthIn             : double;
                                out     newEndValueOut          : double    );
            var
                scaleFactor : double;
            begin
                //the scale factor is the ratio of the new line length to the old line length
                    scaleFactor := abs( newLengthIn / (endValueIn - startValueIn) );

                scaleLinear(startValueIn, endValueIn,
                            scaleFactor,
                            newEndValueOut          );
            end;

        procedure resizeLine(   const   startValueIn,       endValueIn,
                                        scaleAboutValueIn,  newLengthIn     : double;
                                out     newStartValueOut,   newEndValueOut  : double    );
            var
                scaleFactor : double;
            begin
                //the scale factor is the ratio of the new line length to the old line length
                    scaleFactor := abs( newLengthIn / (endValueIn - startValueIn) );

                scaleLinear(startValueIn,       endValueIn,
                            scaleAboutValueIn,  scaleFactor,
                            newStartValueOut,   newEndValueOut );
            end;




end.
