#Requires AutoHotkey v2.0


LShift & RShift::
{
	SetCapsLockState !GetKeyState("CapsLock", "T")
}

RShift & LShift::
{
	SetCapsLockState !GetKeyState("CapsLock", "T")
}