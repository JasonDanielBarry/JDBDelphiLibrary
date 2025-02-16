unit FileReaderWriterClass;

interface

    uses
        system.SysUtils, system.Classes
        ;

    type
        TFileReaderWriter = class
            private
                const
                    VALUE_DELIMITER : char = ':';
                var
                    fileName        : string;
                    fileContents    : TStringList;
            public
                //constructor
                    constructor create(const fileNameIn : string);
                //destructor
                    destructor destroy(); override;
                //load file
                    function loadFile() : boolean;
                //save file
                    procedure saveFile();
                //read methods
                    function readBool(const identifierIn : string; const defaultValueIn : boolean = False) : boolean;
                    function readInteger(const identifierIn : string; const defaultValueIn : integer = 0) : integer;
                    function readDouble(const identifierIn : string; const defaultValueIn : double = 0) : double;
                    function readChar(const identifierIn : string; const defaultValueIn : char = ' ') : char;
                    function readString(const identifierIn : string; const defaultValueIn : string = '') : string;
                //write methods
                    procedure writeBool(const identifierIn : string; const valueIn : boolean);
                    procedure writeInteger(const identifierIn : string; const valueIn : integer);
                    procedure writeDouble(const identifierIn : string; const valueIn : double);
                    procedure writeChar(const identifierIn : string; const valueIn : char);
                    procedure writeString(const identifierIn, valueIn : string);
        end;

implementation

    //public
        //constructor
            constructor TFileReaderWriter.create(const fileNameIn : string);
                begin
                    fileName := fileNameIn;

                    fileContents := TStringList.Create();
                    fileContents.Clear();
                end;

        //destructor
            destructor TFileReaderWriter.destroy();
                begin
                    inherited destroy();
                end;

        //load file
            function TFileReaderWriter.loadFile() : boolean;
                var
                    fileDoesNotExist : boolean;
                begin
                    fileDoesNotExist := NOT(FileExists( fileName ));

                    if (fileDoesNotExist) then
                        exit( false );

                    fileContents.Clear();

                    fileContents.LoadFromFile( fileName );
                end;

        //save file
            procedure TFileReaderWriter.saveFile();
                begin
                    fileContents.SaveToFile( fileName );
                end;

        //read methods
            function TFileReaderWriter.readBool(const identifierIn : string; const defaultValueIn : boolean = False) : boolean;
                begin
                    asdf
                end;

            function TFileReaderWriter.readInteger(const identifierIn : string; const defaultValueIn : integer = 0) : integer;
                begin
                    asdf
                end;

            function TFileReaderWriter.readDouble(const identifierIn : string; const defaultValueIn : double = 0) : double;
                begin
                    asdf
                end;

            function TFileReaderWriter.readChar(const identifierIn : string; const defaultValueIn : char = ' ') : char;
                begin
                    asdf
                end;

            function TFileReaderWriter.readString(const identifierIn : string; const defaultValueIn : string = '') : string;
                begin
                    asdf
                end;

        //write methods
            procedure TFileReaderWriter.writeBool(const identifierIn : string; const valueIn : boolean);
                var
                    boolString : string;
                begin
                    boolString := BoolToStr( valueIn );

                    writeString( identifierIn, boolString );
                end;

            procedure TFileReaderWriter.writeInteger(const identifierIn : string; const valueIn : integer);
                var
                    integerString : string;
                begin
                    integerString := IntToStr( valueIn );

                    writeString( identifierIn, integerString );
                end;

            procedure TFileReaderWriter.writeDouble(const identifierIn : string; const valueIn : double);
                var
                    doubleString : string;
                begin
                    doubleString := FloatToStr( valueIn );

                    writeString( identifierIn, doubleString );
                end;

            procedure TFileReaderWriter.writeChar(const identifierIn : string; const valueIn : char);
                begin
                    writeString( identifierIn, valueIn ); //char can cast to string automatically
                end;

            procedure TFileReaderWriter.writeString(const identifierIn, valueIn : string);
                var
                    ID_And_Value : string;
                begin
                    ID_And_Value := identifierIn + ' ' + VALUE_DELIMITER + ' ' + valueIn;

                    fileContents.add( ID_And_Value );
                end;


end.
