IncludeFile "NeuroShmeegGUI.pbf"

Enumeration MyCONST
  #ExitFromProgramm = 64000
EndEnumeration


UseSQLiteDatabase()

Global dbFile.s = "database.sqlite"
Global cID = 0
Global cMode = 0
Global sessID, ts.q
Global query.s

sET = ElapsedMilliseconds()

Procedure FillState(query.s)
  If FileSize(dbFile) > 0
    OpenDatabase(0, dbFile, "", "")
  Else
    CreateFile(0, dbFile) : CloseFile(0)
    OpenDatabase(0, dbFile, "", "")
    
    makeDB.s = "CREATE TABLE provisors(id INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL, orgName TEXT, CSNumber INT, CSCategory TEXT, CSAddres TEXT, CSDescription TEXT, "+
               "provisorName TEXT, provisorTNumber TEXT UNIQUE, provisorSex TEXT, provisorAge INT,  provisorEducation TEXT, provisorCategory INT, provisorDescription TEXT);"+
               "CREATE TABLE observations (sessid INT, inqnum INT, tos INT, stdabbrev VARCHAR(50), timestamp INT)"
    ;Debug makeDB
    If DatabaseUpdate(0, makeDB) = 0
      Debug DatabaseError()
    EndIf
  EndIf
  If DatabaseUpdate(0, query) = 0
    Debug DatabaseError()
  EndIf
  CloseDatabase(0)
EndProcedure
Procedure EDnableIMT(state.i)
  If(state = -1 ) : state = 1 : Else : state = 0 : EndIf
  state = state * (-1)
  DisableGadget(#b08, state) : DisableGadget(#b09, state)
  SetGadgetState(#b08, 0) : SetGadgetState(#b09, 0)
EndProcedure
Procedure EDnableRxPlus(state.i)
  If(state = -1 ) : state = 1 : Else : state = 0 : EndIf
  DisableGadget(#b02, state) : DisableGadget(#b03, state) : DisableGadget(#b11, state)
  SetGadgetState(#b02, 0) : SetGadgetState(#b03, 0) : SetGadgetState(#b11, 0)
EndProcedure
Procedure TogleCommon()
  SetGadgetState(#b04, 0)
  SetGadgetState(#b05, 0)
  SetGadgetState(#b06, 0)
  SetGadgetState(#b07, 0)
  SetGadgetState(#b10, 0)
EndProcedure
Procedure EDnableCommon(state.i)
  DisableGadget(#b04, state) ;: SetGadgetState(#b04, 0)
  DisableGadget(#b05, state) ;: SetGadgetState(#b05, 0)
  DisableGadget(#b06, state) ;: SetGadgetState(#b06, 0)
  DisableGadget(#b07, state) ;: SetGadgetState(#b07, 0)
  DisableGadget(#b10, state) ;: SetGadgetState(#b10, 0)  
  If(state = 1) : TogleCommon() : EndIf    
EndProcedure
Procedure EndMaintaince()
  EDnableCommon(1) : EDnableIMT(-1) : EDnableRxPlus(-1)
  RemoveGadgetItem(#ListIconQueue, GetGadgetState(#ListIconQueue))
  cMode = 0 : SetGadgetState(#ButtonRx, 0) : SetGadgetState(#ButtonNotRx, 0) : SetGadgetState(#ButtonIMT, 0)
  For x = 0 To CountGadgetItems(#ListIconQueue)
    SetGadgetItemText(#ListIconQueue, x, Str(x+1), 0)
  Next  
EndProcedure
Procedure GadgetsUpdate()
  If OpenDatabase(0, dbFile, "", "")
    If DatabaseQuery(0, "SELECT orgName, provisorName FROM provisors")
      ClearGadgetItems(#ComboCSName) : ClearGadgetItems(#ComboOrganisationName) : ClearGadgetItems(#ComboProvisor)
      While NextDatabaseRow(0)
        AddGadgetItem(#ComboOrganisationName, -1, GetDatabaseString(0, 0))
        AddGadgetItem(#ComboProvisor, -1, GetDatabaseString(0, 1))
      Wend
      FinishDatabaseQuery(0)
    EndIf
    CloseDatabase(0)
  Else
    Debug "Can't open database !"
  EndIf
  
;   #ComboCSName
;   #ComboOrganisationName
;   #ComboProvisor
EndProcedure

OpenWindowMain() : StickyWindow(#WindowMain, 1)
SetWindowTitle(#WindowMain, "NeuroShmeeg 0.3."+Str(#Pb_editor_BuildCount)+"."+Str(#Pb_Editor_CompileCount))

AddKeyboardShortcut(#WindowMain, #PB_Shortcut_Escape, #ExitFromProgramm)
AddKeyboardShortcut(#WindowMain, #PB_Shortcut_F1, #ButtonRx)
AddKeyboardShortcut(#WindowMain, #PB_Shortcut_F2, #ButtonNotRx)
AddKeyboardShortcut(#WindowMain, #PB_Shortcut_F3, #ButtonIMT)
AddKeyboardShortcut(#WindowMain, #PB_Shortcut_Return, #bExit)

For x = #b02 To #b11 : DisableGadget(x, 1) : Next
DisableGadget(#b00, 1)
GadgetsUpdate()
FillState("")

;{ TODO
; + сделать так, чтобы при открытии окон автоматически обновлялись гаджеты
; сделать так, чтобы при выборе провизора или аптеки обновлялась информация о них
; + таксирование рецепта
; + снятие копии рецепта
; + снятие копии рецепта
; + ПОВЫСИТЬ ГРАНУЛЯРНОСТЬ!""2221111
;}





Repeat
  Event=WaitWindowEvent()
  Gadget=EventGadget()
  Type=EventType()
  Window=EventWindow()
  Menu=EventMenu()
  Timer=EventTimer()
  
  
  If (Event = #PB_Event_Gadget And (Type = #PB_EventType_LeftClick Or Type = #PB_EventType_Change)) Or Event = #PB_Event_Menu
    If Menu = #ExitFromProgramm : End : EndIf
    ;{ заставляем ввести провизора при отстуствии такового в #ComboProvisor
    If GetGadgetText(#ComboProvisor) = "" And GetGadgetState(#Panel) = 0
      OpenDatabase(0, dbFile, "", "")
      DatabaseQuery(0, "SELECT COUNT(value) FROM employeesDescription")
      NextDatabaseRow(0)
      If GetDatabaseString(0, 0) <> "0"
        MessageRequester("", "Для начала работы с системой выберите провизора!", #MB_ICONERROR)
      Else
        MessageRequester("", "Для начала работы с системой добавьте провизора!", #MB_ICONERROR)
        SetGadgetState(Gadget, 0) : SetGadgetState(#Panel, 1)
      EndIf
      CloseDatabase(0)
      GadgetsUpdate()
      Continue
    EndIf
    ;}
    ;{ если событие сгененрировано шорткатом, присваиваем его значение гаджету и обрабатываем как кнопку
    If Event = #PB_Event_Menu
      Gadget = Menu
      If Gadget <> #ComboProvisor
        SetGadgetState(Gadget, 1)
      EndIf
    EndIf
    ;}
    
    Select Gadget
      Case #b00 ; постановка в очередь
        ts = Date()*1000+ElapsedMilliseconds()-sET
        AddGadgetItem(#ListIconQueue, -1, Str(CountGadgetItems(#ListIconQueue)+1)+Chr(10)+Str(ts))
        cID = Val(GetGadgetItemText(#ListIconQueue, GetGadgetState(#ListIconQueue), 1))
        query = "INSERT INTO observations (sessid, inqnum, tos, stdabbrev, timestamp) VALUES ('"+sessID+"', '"+cID+"', '"+cMode+"', 'inq', '"+Str(ts)+"')"
        FillState(query)
      Case #b01 ; поступил на обслуживание
        ts = Date()*1000+ElapsedMilliseconds()-sET
        If CountGadgetItems(#ListIconQueue) = 0
          AddGadgetItem(#ListIconQueue, -1, Str(CountGadgetItems(#ListIconQueue)+1)+Chr(10)+Str(ts))
        EndIf
        SetGadgetState(#ListIconQueue, 0)
        cID = Val(GetGadgetItemText(#ListIconQueue, GetGadgetState(#ListIconQueue), 1))
        query = "INSERT INTO observations (sessid, inqnum, tos, stdabbrev, timestamp) VALUES ('"+sessID+"', '"+cID+"', '"+cMode+"', 'inm', '"+Str(ts)+"')"
        FillState(query)
      Case #b02
        ts = Date()
        cID = Val(GetGadgetItemText(#ListIconQueue, GetGadgetState(#ListIconQueue), 1))
        query = "INSERT INTO observations (sessid, inqnum, tos, stdabbrev, timestamp) VALUES ('"+sessID+"', '"+cID+"', '"+cMode+"', 'phe', '"+Str(ts)+"')"
        FillState(query)
      Case #b03
        ts = Date()
        cID = Val(GetGadgetItemText(#ListIconQueue, GetGadgetState(#ListIconQueue), 1))
        query = "INSERT INTO observations (sessid, inqnum, tos, stdabbrev, timestamp) VALUES ('"+sessID+"', '"+cID+"', '"+cMode+"', 'sea', '"+Str(ts)+"')"
        FillState(query)
      Case #b04
        ts = Date()
        cID = Val(GetGadgetItemText(#ListIconQueue, GetGadgetState(#ListIconQueue), 1))
        query = "INSERT INTO observations (sessid, inqnum, tos, stdabbrev, timestamp) VALUES ('"+sessID+"', '"+cID+"', '"+cMode+"', 'phc', '"+Str(ts)+"')"
        FillState(query)
      Case #b05
        ts = Date()
        cID = Val(GetGadgetItemText(#ListIconQueue, GetGadgetState(#ListIconQueue), 1))
        query = "INSERT INTO observations (sessid, inqnum, tos, stdabbrev, timestamp) VALUES ('"+sessID+"', '"+cID+"', '"+cMode+"', 'sis', '"+Str(ts)+"')"
        FillState(query)
      Case #b06
        ts = Date()
        cID = Val(GetGadgetItemText(#ListIconQueue, GetGadgetState(#ListIconQueue), 1))
        query = "INSERT INTO observations (sessid, inqnum, tos, stdabbrev, timestamp) VALUES ('"+sessID+"', '"+cID+"', '"+cMode+"', 'ssp', '"+Str(ts)+"')"
        FillState(query)
      Case #b07
        ts = Date()
        cID = Val(GetGadgetItemText(#ListIconQueue, GetGadgetState(#ListIconQueue), 1))
        query = "INSERT INTO observations (sessid, inqnum, tos, stdabbrev, timestamp) VALUES ('"+sessID+"', '"+cID+"', '"+cMode+"', 'fch', '"+Str(ts)+"')"
        FillState(query)
      Case #b08
        ts = Date()
        cID = Val(GetGadgetItemText(#ListIconQueue, GetGadgetState(#ListIconQueue), 1))
        query = "INSERT INTO observations (sessid, inqnum, tos, stdabbrev, timestamp) VALUES ('"+sessID+"', '"+cID+"', '"+cMode+"', 'prd', '"+Str(ts)+"')"
        FillState(query)
      Case #b09
        ts = Date()
        cID = Val(GetGadgetItemText(#ListIconQueue, GetGadgetState(#ListIconQueue), 1))
        query = "INSERT INTO observations (sessid, inqnum, tos, stdabbrev, timestamp) VALUES ('"+sessID+"', '"+cID+"', '"+cMode+"', 'gar', '"+Str(ts)+"')"
        FillState(query)
      Case #b10
        If MessageRequester("Буэээ","Расчет осуществлен при помощи наличных денеждных значков?", #MB_YESNO | #MB_ICONQUESTION) = #PB_MessageRequester_Ok
          ts = Date()
          cID = Val(GetGadgetItemText(#ListIconQueue, GetGadgetState(#ListIconQueue), 1))
          query = "INSERT INTO observations (sessid, inqnum, tos, stdabbrev, timestamp) VALUES ('"+sessID+"', '"+cID+"', '"+cMode+"', 'pcs', '"+Str(ts)+"')"
          FillState(query)
        Else
          ts = Date()
          cID = Val(GetGadgetItemText(#ListIconQueue, GetGadgetState(#ListIconQueue), 1))
          query = "INSERT INTO observations (sessid, inqnum, tos, stdabbrev, timestamp) VALUES ('"+sessID+"', '"+cID+"', '"+cMode+"', 'pcr', '"+Str(ts)+"')"
          FillState(query)
        EndIf
      Case #b11
        ts = Date()
        cID = Val(GetGadgetItemText(#ListIconQueue, GetGadgetState(#ListIconQueue), 1))
        query = "INSERT INTO observations (sessid, inqnum, tos, stdabbrev, timestamp) VALUES ('"+sessID+"', '"+cID+"', '"+cMode+"', 'bsd', '"+Str(ts)+"')"
        FillState(query)
      Case #bExit
        ts = Date()
        cID = Val(GetGadgetItemText(#ListIconQueue, GetGadgetState(#ListIconQueue), 1))
        query = "INSERT INTO observations (sessid, inqnum, tos, stdabbrev, timestamp) VALUES ('"+sessID+"', '"+cID+"', '"+cMode+"', 'END', '"+Str(ts)+"')"
        FillState(query)
        EndMaintaince()        
        ; -------------------------------------------------------  
      Case #ButtonRx
        If GetGadgetState(#ButtonRx) = 0 : i = -1 : Else : i = 1 : EndIf
        cMode = cMode + i
        EDnableRxPlus(i)
      Case  #ButtonRxL
        If GetGadgetState(#ButtonRxL) = 0 : i = -1 : Else : i = 1 : EndIf
        cMode = cMode + i*15
        EDnableRxPlus(i)
      Case #ButtonNotRx
        Debug 2
        If GetGadgetState(#ButtonNotRx) = 0 : i = -1 : Else : i = 1 : EndIf
        cMode = cMode + i*2
      Case #ButtonIMT
        Debug 3
        If GetGadgetState(#ButtonIMT) = 0 : i = -1 : Else : i = 1 : EndIf
        cMode = cMode + i*4
        EDnableIMT(i)
        ;-------------------------------------------------------------
      Case #ComboProvisor
        sessID = Date()
      Case #ButtonAdd
        For x = #EditorCSDescription To #EditorProvisorDescription
          If GetGadgetText(x) = ""
            MessageRequester("Заполнены не все поля!","Полей не так и много, я уверен, еще раз и все получится!", #MB_ICONERROR)
            isAllFieldFilled = #False
            Break
          Else
            isAllFieldFilled = #True
          EndIf
        Next
        If isAllFieldFilled
          query.s = "INSERT INTO provisors (orgName, CSNumber, CSCategory, CSAddres, CSDescription, provisorName, provisorTNumber, provisorSex, provisorAge, provisorEducation, provisorCategory, provisorDescription)"+
                    " VALUES ('"+GetGadgetText(#ComboOrganisationName)+"', '"+GetGadgetText(#ComboCSName)+"', '"+GetGadgetText(#ComboCSCategory)+"', "+
                    "'"+GetGadgetText(#StringCSAddres)+"', '"+GetGadgetText(#EditorCSDescription)+"','"+GetGadgetText(#StringProvisorName)+"',"+
                    "'"+GetGadgetText(#StringProvisorTNumber)+"', '"+GetGadgetText(#ComboProvisorSex)+"', '"+GetGadgetText(#StringProvisorAge)+"', '"+GetGadgetText(#ComboProvisorEducation)+"', '"+GetGadgetText(#ComboProvisorCategory)+"', '"+GetGadgetText(#EditorProvisorDescription)+"')"
          FillState(query)
          Debug query
          SetGadgetText(#ComboOrganisationName, "")  : SetGadgetText(#ComboCSName, "")
          SetGadgetText(#ComboCSCategory, "")        : SetGadgetText(#StringCSAddres, "")
          SetGadgetText(#EditorCSDescription, "")    : SetGadgetText(#StringProvisorName, "")
          SetGadgetText(#StringProvisorTNumber, "")  : SetGadgetText(#ComboProvisorSex, "")
          SetGadgetText(#StringProvisorAge, "")      : SetGadgetText(#ComboProvisorEducation, "")
          SetGadgetText(#ComboProvisorCategory, "")  : SetGadgetText(#EditorProvisorDescription, "")
        EndIf
    EndSelect
  EndIf
  
  If CountGadgetItems(#ListIconQueue) = 0 : DisableGadget(#b00, 1) : Else : DisableGadget(#b00, 0) : EndIf
  If cMode = 0 : EDnableCommon(1) : Else : EDnableCommon(0) : EndIf
  
Until Event=#PB_Event_CloseWindow
; IDE Options = PureBasic 5.20 LTS (Windows - x86)
; CursorPosition = 276
; FirstLine = 142
; Folding = Ay
; EnableUnicode
; EnableXP
; EnableCompileCount = 29
; EnableBuildCount = 0
; IncludeVersionInfo