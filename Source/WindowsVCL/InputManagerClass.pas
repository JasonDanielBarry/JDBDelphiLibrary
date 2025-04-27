unit InputManagerClass;

interface

    uses
        system.SysUtils, System.Classes, system.Generics.Collections, System.UITypes,
        Vcl.StdCtrls
        ;

    type
        TInputManager = class
            private
                var
                    errorList       : TStringList;
                    ListBoxErrors   : TListBox;
                //send errors to error list box
                    procedure populateErrorListBox();
            protected
                const
                    CONTROL_MARGIN : integer = 5; //the VCL controls must have and edge space = 5
                //add an error
                    procedure addError(const errorMessageIn : string);
                //check for input errors
                    procedure checkForInputErrors(); virtual;
                //set error list box width
                    procedure setListBoxErrorsWidth(const widthIn : integer);
                //setup input controls
                    procedure setupInputControls(); virtual;
            public
                //constructor
                    constructor create(const errorListBoxIn : TListBox);
                //destructor
                    destructor destroy(); override;
                //reset controls
                    procedure resetInputControls(); virtual;
                    class procedure resetAllInputControls(const arrInputManagerIn : TArray<TInputManager>);  static;
                //process input
                    //read input
                        function readFromInputControls() : boolean; virtual;
                        class function readFromAllControls(const arrInputManagerIn : TArray<TInputManager>) : boolean; static;
                    //write to input controls
                        procedure writeToInputControls(const updateEmptyControlsIn : boolean = False); virtual;
                        class procedure writeToAllControls(const arrInputManagerIn : TArray<TInputManager>; const updateEmptyControlsIn : boolean = False); static;
                //count errors
                    function errorCount() : integer;
                    class function countInputErrors(const arrInputManagerIn : TArray<TInputManager>) : integer; static;
        end;

implementation

    //private
        //send errors to error list box
            procedure TInputManager.populateErrorListBox();
                var
                    i : integer;
                begin
                    //perform error checking
                        checkForInputErrors();

                    //initialise list box for error posting
                        ListBoxErrors.Clear();

                    //exit if there are not errors
                        if ( errorCount() < 1 ) then
                            begin
                                ListBoxErrors.Visible := False;
                                exit();
                            end;

                    //add the error message to the list box and show
                        ListBoxErrors.Items.Add( 'ERRORS:' );

                        for i := 0 to (errorCount - 1) do
                            ListBoxErrors.Items.Add( errorList[i] );

                        ListBoxErrors.Visible := True;
                end;

    //protected
        //add an error
            procedure TInputManager.addError(const errorMessageIn : string);
                begin
                    errorList.Add( errorMessageIn );
                end;

        //check for input errors
            procedure TInputManager.checkForInputErrors();
                begin
                    errorList.Clear();
                end;

        //set error list box width
            procedure TInputManager.setListBoxErrorsWidth(const widthIn : integer);
                begin
                    ListBoxErrors.Width := widthIn;
                end;

        //setup input controls
            procedure TInputManager.setupInputControls();
                begin
                    //set list box initially to non visible
                        ListBoxErrors.Visible := False;
                end;

    //public
        //constructor
            constructor TInputManager.create(const errorListBoxIn : TListBox);

                begin
                    inherited create();

                    //create error list
                        errorList := TStringList.create();
                        errorList.Clear();

                    //store error list (***this is a >>>POINTER/REFERENCE<<< on the heap - changes here take effect everywhere***)
                        ListBoxErrors := errorListBoxIn;

                    setupInputControls();
                end;

        //destructor
            destructor TInputManager.destroy();
                begin
                    FreeAndNil( errorList );

                    inherited destroy();
                end;

        //reset controls
            procedure TInputManager.resetInputControls();
                begin
                    //do nothing
                end;

            class procedure TInputManager.resetAllInputControls(const arrInputManagerIn : TArray<TInputManager>);
                var
                    inputManager : TInputManager;
                begin
                    for inputManager in arrInputManagerIn do
                        inputManager.resetInputControls();
                end;

        //process input
            //read input
                function TInputManager.readFromInputControls() : boolean;
                    begin
                        //nothing here for now
                        result := True;
                    end;

                class function TInputManager.readFromAllControls(const arrInputManagerIn : TArray<TInputManager>) : boolean;
                    var
                        inputManager : TInputManager;
                    begin
                        result := True;

                        for inputManager in arrInputManagerIn do
                            result := inputManager.readFromInputControls() AND result;
                    end;

            //write to input controls
                procedure TInputManager.writeToInputControls(const updateEmptyControlsIn : boolean = False);
                    begin
                        populateErrorListBox();
                    end;

                class procedure TInputManager.writeToAllControls(const arrInputManagerIn : TArray<TInputManager>; const updateEmptyControlsIn : boolean = False);
                    var
                        inputManager : TInputManager;
                    begin
                        for inputManager in arrInputManagerIn do
                            inputManager.writeToInputControls( updateEmptyControlsIn );
                    end;

        //count errors
            function TInputManager.errorCount() : integer;
                begin
                    result := errorList.Count;
                end;

            class function TInputManager.countInputErrors( const arrInputManagerIn : TArray<TInputManager>) : integer;
                var
                    totalErrorCount : integer;
                    inputManager : TInputManager;
                begin
                    totalErrorCount := 0;

                    for inputManager in arrInputManagerIn do
                        totalErrorCount := totalErrorCount + inputManager.errorCount();

                    result := totalErrorCount;
                end;

end.
