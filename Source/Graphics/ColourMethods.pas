unit ColourMethods;

interface

    uses
        Winapi.Windows, system.UIConsts, system.UITypes;

    function colourToAlphaColour(   const colourIn  : TColor;
                                    const opacityIn : integer = 255) : TAlphaColor;

implementation

    function colourToAlphaColour(   const colourIn  : TColor;
                                    const opacityIn : integer = 255) : TAlphaColor;
        var
            colourReferenceHex  : LongInt;
            redGreenBlue        : TRGBTriple;
        begin
            colourReferenceHex := TColorRec.ColorToRGB(colourIn);

            redGreenBlue.rgbtRed    := GetRValue( colourReferenceHex );
            redGreenBlue.rgbtGreen  := GetGValue( colourReferenceHex );
            redGreenBlue.rgbtBlue   := GetBValue( colourReferenceHex );

            result := MakeColor( redGreenBlue.rgbtRed, redGreenBlue.rgbtGreen, redGreenBlue.rgbtBlue, opacityIn );
        end;

end.
