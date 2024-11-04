unit ColourMethods;

interface

    uses
        Winapi.Windows, system.UIConsts, system.UITypes;

    function colourToAlphaColour(const colourIn : TColor) : TAlphaColor;

implementation

    function colourToAlphaColour(const colourIn : TColor) : TAlphaColor;
        begin
            result := TColorRec.ColorToRGB(colourIn);
        end;

end.
