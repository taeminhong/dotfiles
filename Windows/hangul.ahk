#Requires AutoHotkey v2.0
DetectHiddenWindows true

prevHwnd := 0
prevHkl := 0
SetTimer(Watch, 100)

$!Space::Send "#{Space}"

Watch() {
    global prevHwnd, prevHkl
    hwnd := WinActive("A")
    if !hwnd
        return
    tid := DllCall("GetWindowThreadProcessId", "Ptr", hwnd, "Ptr", 0, "UInt")
    hkl := DllCall("GetKeyboardLayout", "UInt", tid, "Ptr")
    ; Only react to a layout change inside the same window; switching windows must not touch the IME state.
    ; The IME isn't ready right after the layout changes, so wait a bit before setting the mode.
    ; Don't build these timers in a for loop: the callbacks never ran that way.
    if hwnd = prevHwnd && hkl != prevHkl && (hkl & 0xFFFF) = 0x0412 {
        Schedule(hwnd, 100)
    }
    prevHwnd := hwnd
    prevHkl := hkl
}

Schedule(hwnd, delay) {
    SetTimer(() => SetHangul(hwnd), -delay)
}

SetHangul(hwnd) {
    ime := DllCall("imm32\ImmGetDefaultIMEWnd", "Ptr", hwnd, "Ptr")
    ; WM_IME_CONTROL(0x283). SMTO_ABORTIFHUNG(0x2), 50ms.
    ; IMC_SETOPENSTATUS(0x6): some windows (e.g. Explorer) keep the IME closed while the conversion mode still reads NATIVE.
    ; IMC_SETCONVERSIONMODE(0x2), IME_CMODE_NATIVE(0x1).
    r := 0
    DllCall("SendMessageTimeoutW", "Ptr", ime, "UInt", 0x283, "Ptr", 0x6, "Ptr", 0x1
        , "UInt", 0x2, "UInt", 50, "Ptr*", &r)
    DllCall("SendMessageTimeoutW", "Ptr", ime, "UInt", 0x283, "Ptr", 0x2, "Ptr", 0x1
        , "UInt", 0x2, "UInt", 50, "Ptr*", &r)
}
