unit ColourMethods;

interface

    uses
        Winapi.Windows, system.UIConsts, system.UITypes;

    function colourToAlphaColour(   const colourIn  : TColor;
                                    const opacityIn : integer = 255) : TAlphaColor;

    function makeAlphaColour(redIn, greenIn, blueIn, opacityIn : byte) : TAlphaColor; inline;

    function makeColour(redIn, greenIn, blueIn : byte) : TColor;

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

    function makeAlphaColour(redIn, greenIn, blueIn, opacityIn : byte) : TAlphaColor;
        begin
            result := MakeColor( redIn, greenIn, blueIn, opacityIn );
        end;

    function makeColour(redIn, greenIn, blueIn : byte) : TColor;
        var
            tempAlphaColour : TAlphaColor;
        begin
            tempAlphaColour := MakeColor(redIn, greenIn, blueIn, 255);

            result := AlphaColorToColor( tempAlphaColour );
        end;

end.
