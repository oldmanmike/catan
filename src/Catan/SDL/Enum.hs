{-# LANGUAGE PatternSynonyms #-}
-------------------------------------------------------------------------------
-- |
-- Copyright        : (c) 2015 Michael Carpenter
-- License          : GPL3
-- Maintainer       : Michael Carpenter <oldmanmike.dev@gmail.com>
-- Stability        : provisional
-- Portability      : portable
--
-------------------------------------------------------------------------------
module Catan.SDL.Enum (
    Event (..),
    PhysicalKey (..),
) where

data Event = Keyboard !PhysicalKey 
           | Mouse !Int !Int 
           | Something 
           deriving (Show,Read,Eq,Ord)

-- PhysicalKeys are associated with scancodes.
data PhysicalKey = Unknown
         | A | B | C | D | E | F | G | H | I 
         | J | K | L | M | N | O | P | Q | R 
         | S | T | U | V | W | X | Y | Z

         | Num1 | Num2 | Num3 | Num4 | Num5
         | Num6 | Num7 | Num8 | Num9 | Num0

         | Return | Escape | Backspace | Tab | Space

         | Minus
         | Equals
         | LeftBracket
         | RightBracket
         | BackSlash
         | NonUSHash
         | Semicolon
         | Apostrophe
         | Grave
         | Comma
         | Period
         | Slash
         
         | Capslock

         | F1 | F2 | F3 | F4  | F5  | F6
         | F7 | F8 | F9 | F10 | F11 | F12

         | PrintScreen | ScrollLock | Pause | Insert

         | Home | PageUp | Delete | End | PageDown 
         
         | RightArrow | LeftArrow | DownArrow | UpArrow

         | NumLockClear

         | KP_Divide | KP_Multiply | KP_Minus | KP_Plus | KP_Enter
         | KP_1 | KP_2 | KP_3
         | KP_4 | KP_5 | KP_6
         | KP_7 | KP_8 | KP_9
         | KP_0 | KP_Period

         | NonUSBackSlash

         | Application | Power

         | KP_Equals

         | F13 | F14 | F15 | F16 | F17 | F18 
         | F19 | F20 | F21 | F22 | F23 | F24

         | Execute | Help | Menu | Select
         | Stop | Again | Undo | Cut | Copy 
         | Paste | Find | Mute | VolumeUp | VolumeDown 

         | KP_Comma | KP_EqualSAS400

         | International1 
         | International2
         | International3
         | International4
         | International5
         | International6
         | International7
         | International8
         | International9
         | Lang1
         | Lang2
         | Lang3
         | Lang4
         | Lang5
         | Lang6
         | Lang7
         | Lang8
         | Lang9

         | Alterase | SysReq | Cancel | Clear | Prior | Return2
         | Separator | Out | Oper | ClearAgain | Crsel | Exsel

         | KP_00 | KP_000 
         | ThousandSSeparator | DecimalSeparator
         | CurrencyUnit | CurrencySubunit
         | KP_LeftParen | KP_RightParen
         | KP_LeftBrace | KP_RightBrace
         | KP_Tab | KP_Backspace
         | KP_A | KP_B | KP_C | KP_D | KP_E | KP_F
         | KP_XOR | KP_Power | KP_Percent | KP_Less
         | KP_Greater | KP_Ampersand | KP_DBLAmpersand
         | KP_VerticalBar | KP_DBLVerticalBar | KP_Colon
         | KP_Hash | KP_Space | KP_At | KP_Exclam
         | KP_MemStore | KP_MemRecall | KP_MemClear
         | KP_MemAdd | KP_MemSubtract | KP_MemMultiply | KP_MemDivide
         | KP_PlusMinus | KP_Clear | KP_ClearEntry | KP_Binary | KP_Octal
         | KP_Decimal | KP_Hexadecimal

         | LCTRL | LShift | LALT | LGUI | RCTRL | RShift | RALT | RGUI

         | Mode

         | AudioNext | AudioPrev | AudioStop | AudioPlay | AudioMute
         | MediaSelect | WWW | Mail | Calculator | Computer
         | AC_Search | AC_Home | AC_Back | AC_Forward | AC_Stop | AC_Refresh | AC_Bookmarks

         | BrightnessDown | BrightnessUp | DisplaySwitch

         | KBDIllumToggle | KBDIllumDown | KBDIllumUp
         | Eject | Sleep

         | App1 | App2
         deriving (Show,Ord,Read,Eq)

instance Enum PhysicalKey where
    fromEnum A = 4
    fromEnum B = 5
    fromEnum C = 6
    fromEnum D = 7 
    fromEnum E = 8
    fromEnum F = 9
    fromEnum G = 10
    fromEnum H = 11
    fromEnum I = 12
    fromEnum J = 13
    fromEnum K = 14
    fromEnum L = 15
    fromEnum M = 16
    fromEnum N = 17
    fromEnum O = 18
    fromEnum P = 19
    fromEnum Q = 20
    fromEnum R = 21
    fromEnum S = 22
    fromEnum T = 23
    fromEnum U = 24
    fromEnum V = 25
    fromEnum W = 26
    fromEnum X = 27
    fromEnum Y = 28
    fromEnum Z = 29

    fromEnum Num1 = 30
    fromEnum Num2 = 31
    fromEnum Num3 = 32
    fromEnum Num4 = 33
    fromEnum Num5 = 34
    fromEnum Num6 = 35
    fromEnum Num7 = 36
    fromEnum Num8 = 37
    fromEnum Num9 = 38
    fromEnum Num0 = 39
         
    fromEnum Return = 40
    fromEnum Escape = 41
    fromEnum Backspace = 42
    fromEnum Tab = 43
    fromEnum Space = 44

    fromEnum Minus = 45
    fromEnum Equals = 46
    fromEnum LeftBracket = 47
    fromEnum RightBracket = 48
    fromEnum BackSlash = 49
    fromEnum NonUSHash = 50
    fromEnum Semicolon = 51
    fromEnum Apostrophe = 52
    fromEnum Grave = 53
    fromEnum Comma = 54
    fromEnum Period = 55
    fromEnum Slash = 56
    
    fromEnum Capslock = 57

    fromEnum F1 = 58
    fromEnum F2 = 59
    fromEnum F3 = 60
    fromEnum F4 = 61
    fromEnum F5 = 62
    fromEnum F6 = 63
    fromEnum F7 = 64
    fromEnum F8 = 65
    fromEnum F9 = 66
    fromEnum F10 = 67
    fromEnum F11 = 68
    fromEnum F12 = 69

    fromEnum PrintScreen = 70
    fromEnum ScrollLock = 71
    fromEnum Pause = 72
    fromEnum Insert = 73

    fromEnum Home = 74
    fromEnum PageUp = 75
    fromEnum Delete = 76
    fromEnum End = 77
    fromEnum PageDown = 78

    fromEnum RightArrow = 79
    fromEnum LeftArrow = 80
    fromEnum DownArrow = 81
    fromEnum UpArrow = 82

    fromEnum NumLockClear = 83

    fromEnum KP_Divide = 84
    fromEnum KP_Multiply = 85
    fromEnum KP_Minus = 86
    fromEnum KP_Plus = 87
    fromEnum KP_Enter = 88
    fromEnum KP_1 = 89
    fromEnum KP_2 = 90
    fromEnum KP_3 = 91
    fromEnum KP_4 = 92
    fromEnum KP_5 = 93
    fromEnum KP_6 = 94
    fromEnum KP_7 = 95
    fromEnum KP_8 = 96
    fromEnum KP_9 = 97
    fromEnum KP_0 = 98
    fromEnum KP_Period = 99

    fromEnum NonUSBackSlash = 100

    fromEnum Application = 101
    fromEnum Power = 102

    fromEnum KP_Equals = 103
    fromEnum F13 = 104
    fromEnum F14 = 105
    fromEnum F15 = 106
    fromEnum F16 = 107
    fromEnum F17 = 108
    fromEnum F18 = 109 
    fromEnum F19 = 110
    fromEnum F20 = 111
    fromEnum F21 = 112
    fromEnum F22 = 113
    fromEnum F23 = 114
    fromEnum F24 = 115

    fromEnum Execute = 116
    fromEnum Help = 117
    fromEnum Menu = 118
    fromEnum Select = 119
    fromEnum Stop = 120
    fromEnum Again = 121
    fromEnum Undo = 122
    fromEnum Cut = 123
    fromEnum Copy = 124
    fromEnum Paste = 125
    fromEnum Find = 126
    fromEnum Mute = 127
    fromEnum VolumeUp = 128
    fromEnum VolumeDown = 129

    fromEnum KP_Comma = 133
    fromEnum KP_EqualSAS400 = 134

    fromEnum International1 = 135
    fromEnum International2 = 136
    fromEnum International3 = 137
    fromEnum International4 = 138
    fromEnum International5 = 139
    fromEnum International6 = 140
    fromEnum International7 = 141
    fromEnum International8 = 142
    fromEnum International9 = 143
    fromEnum Lang1 = 144
    fromEnum Lang2 = 145
    fromEnum Lang3 = 146
    fromEnum Lang4 = 147
    fromEnum Lang5 = 148
    fromEnum Lang6 = 149
    fromEnum Lang7 = 150
    fromEnum Lang8 = 151
    fromEnum Lang9 = 152

    fromEnum Alterase = 153
    fromEnum SysReq = 154
    fromEnum Cancel = 155
    fromEnum Clear = 156
    fromEnum Prior = 157
    fromEnum Return2 = 158
    fromEnum Separator = 159
    fromEnum Out = 160
    fromEnum Oper = 161
    fromEnum ClearAgain = 162
    fromEnum Crsel = 163
    fromEnum Exsel = 164

    fromEnum KP_00 = 176
    fromEnum KP_000 = 177
    fromEnum ThousandSSeparator = 178
    fromEnum DecimalSeparator = 179
    fromEnum CurrencyUnit = 180
    fromEnum CurrencySubunit = 181
    fromEnum KP_LeftParen = 182
    fromEnum KP_RightParen = 183
    fromEnum KP_LeftBrace = 184
    fromEnum KP_RightBrace = 185
    fromEnum KP_Tab = 186
    fromEnum KP_Backspace = 187
    fromEnum KP_A = 188
    fromEnum KP_B = 189
    fromEnum KP_C = 190
    fromEnum KP_D = 191
    fromEnum KP_E = 192
    fromEnum KP_F = 193
    fromEnum KP_XOR = 194
    fromEnum KP_Power = 195
    fromEnum KP_Percent = 196
    fromEnum KP_Less = 197
    fromEnum KP_Greater = 198
    fromEnum KP_Ampersand = 199
    fromEnum KP_DBLAmpersand = 200
    fromEnum KP_VerticalBar = 201
    fromEnum KP_DBLVerticalBar = 202
    fromEnum KP_Colon = 203
    fromEnum KP_Hash = 204
    fromEnum KP_Space = 205
    fromEnum KP_At = 206
    fromEnum KP_Exclam = 207
    fromEnum KP_MemStore = 208
    fromEnum KP_MemRecall = 209
    fromEnum KP_MemClear = 210
    fromEnum KP_MemAdd = 211
    fromEnum KP_MemSubtract = 212
    fromEnum KP_MemMultiply = 213
    fromEnum KP_MemDivide = 214
    fromEnum KP_PlusMinus = 215
    fromEnum KP_Clear = 216
    fromEnum KP_ClearEntry = 217
    fromEnum KP_Binary = 218
    fromEnum KP_Octal = 219
    fromEnum KP_Decimal = 220
    fromEnum KP_Hexadecimal = 221

    fromEnum LCTRL = 224
    fromEnum LShift = 225
    fromEnum LALT = 226
    fromEnum LGUI = 227
    fromEnum RCTRL = 228
    fromEnum RShift = 229
    fromEnum RALT = 230
    fromEnum RGUI = 231

    fromEnum Mode = 257

    fromEnum AudioNext = 258
    fromEnum AudioPrev = 259
    fromEnum AudioStop = 260
    fromEnum AudioPlay = 261
    fromEnum AudioMute = 262
    fromEnum MediaSelect = 263
    fromEnum WWW = 264
    fromEnum Mail = 265
    fromEnum Calculator = 266
    fromEnum Computer = 267
    fromEnum AC_Search = 268
    fromEnum AC_Home = 269
    fromEnum AC_Back = 270
    fromEnum AC_Forward = 271
    fromEnum AC_Stop = 272
    fromEnum AC_Refresh = 273
    fromEnum AC_Bookmarks = 274

    fromEnum BrightnessDown = 275
    fromEnum BrightnessUp = 276
    fromEnum DisplaySwitch = 277

    fromEnum KBDIllumToggle = 278
    fromEnum KBDIllumDown = 279
    fromEnum KBDIllumUp = 280
    fromEnum Eject = 281
    fromEnum Sleep = 282

    fromEnum App1 = 283
    fromEnum App2 = 284

    fromEnum Unknown = 0

    toEnum 4 = A
    toEnum 5 = B
    toEnum 6 = C
    toEnum 7 = D
    toEnum 8 = E
    toEnum 9 = F
    toEnum 10 = G
    toEnum 11 = H
    toEnum 12 = I
    toEnum 13 = J
    toEnum 14 = K
    toEnum 15 = L
    toEnum 16 = M
    toEnum 17 = N
    toEnum 18 = O
    toEnum 19 = P
    toEnum 20 = Q
    toEnum 21 = R
    toEnum 22 = S
    toEnum 23 = T
    toEnum 24 = U
    toEnum 25 = V
    toEnum 26 = W
    toEnum 27 = X
    toEnum 28 = Y
    toEnum 29 = Z

    toEnum 30 = Num1
    toEnum 31 = Num2
    toEnum 32 = Num3
    toEnum 33 = Num4
    toEnum 34 = Num5
    toEnum 35 = Num6
    toEnum 36 = Num7
    toEnum 37 = Num8
    toEnum 38 = Num9
    toEnum 39 = Num0
         
    toEnum 40 = Return
    toEnum 41 = Escape
    toEnum 42 = Backspace
    toEnum 43 = Tab
    toEnum 44 = Space

    toEnum 45 = Minus
    toEnum 46 = Equals
    toEnum 47 = LeftBracket
    toEnum 48 = RightBracket
    toEnum 49 = BackSlash
    toEnum 50 = NonUSHash
    toEnum 51 = Semicolon
    toEnum 52 = Apostrophe
    toEnum 53 = Grave
    toEnum 54 = Comma
    toEnum 55 = Period
    toEnum 56 = Slash
    
    toEnum 57 = Capslock

    toEnum 58 = F1
    toEnum 59 = F2
    toEnum 60 = F3
    toEnum 61 = F4
    toEnum 62 = F5
    toEnum 63 = F6
    toEnum 64 = F7
    toEnum 65 = F8
    toEnum 66 = F9
    toEnum 67 = F10
    toEnum 68 = F11
    toEnum 69 = F12

    toEnum 70 = PrintScreen
    toEnum 71 = ScrollLock
    toEnum 72 = Pause
    toEnum 73 = Insert

    toEnum 74 = Home
    toEnum 75 = PageUp
    toEnum 76 = Delete
    toEnum 77 = End
    toEnum 78 = PageDown

    toEnum 79 = RightArrow
    toEnum 80 = LeftArrow
    toEnum 81 = DownArrow
    toEnum 82 = UpArrow

    toEnum 83 = NumLockClear

    toEnum 84 = KP_Divide
    toEnum 85 = KP_Multiply
    toEnum 86 = KP_Minus
    toEnum 87 = KP_Plus
    toEnum 88 = KP_Enter
    toEnum 89 = KP_1
    toEnum 90 = KP_2
    toEnum 91 = KP_3
    toEnum 92 = KP_4
    toEnum 93 = KP_5
    toEnum 94 = KP_6
    toEnum 95 = KP_7
    toEnum 96 = KP_8
    toEnum 97 = KP_9
    toEnum 98 = KP_0
    toEnum 99 = KP_Period

    toEnum 100 = NonUSBackSlash

    toEnum 101 = Application
    toEnum 102 = Power

    toEnum 103 = KP_Equals
    toEnum 104 = F13
    toEnum 105 = F14
    toEnum 106 = F15
    toEnum 107 = F16
    toEnum 108 = F17
    toEnum 109 = F18
    toEnum 110 = F19
    toEnum 111 = F20
    toEnum 112 = F21
    toEnum 113 = F22
    toEnum 114 = F23
    toEnum 115 = F24

    toEnum 116 = Execute
    toEnum 117 = Help
    toEnum 118 = Menu
    toEnum 119 = Select
    toEnum 120 = Stop
    toEnum 121 = Again
    toEnum 122 = Undo
    toEnum 123 = Cut
    toEnum 124 = Copy
    toEnum 125 = Paste
    toEnum 126 = Find
    toEnum 127 = Mute
    toEnum 128 = VolumeUp
    toEnum 129 = VolumeDown

    toEnum 133 = KP_Comma
    toEnum 134 = KP_EqualSAS400

    toEnum 135 = International1
    toEnum 136 = International2
    toEnum 137 = International3
    toEnum 138 = International4
    toEnum 139 = International5
    toEnum 140 = International6
    toEnum 141 = International7
    toEnum 142 = International8
    toEnum 143 = International9
    toEnum 144 = Lang1
    toEnum 145 = Lang2
    toEnum 146 = Lang3
    toEnum 147 = Lang4
    toEnum 148 = Lang5
    toEnum 149 = Lang6
    toEnum 150 = Lang7
    toEnum 151 = Lang8
    toEnum 152 = Lang9

    toEnum 153 = Alterase
    toEnum 154 = SysReq
    toEnum 155 = Cancel
    toEnum 156 = Clear
    toEnum 157 = Prior
    toEnum 158 = Return2
    toEnum 159 = Separator
    toEnum 160 = Out
    toEnum 161 = Oper
    toEnum 162 = ClearAgain
    toEnum 163 = Crsel
    toEnum 164 = Exsel

    toEnum 176 = KP_00
    toEnum 177 = KP_000
    toEnum 178 = ThousandSSeparator
    toEnum 179 = DecimalSeparator
    toEnum 180 = CurrencyUnit
    toEnum 181 = CurrencySubunit
    toEnum 182 = KP_LeftParen
    toEnum 183 = KP_RightParen
    toEnum 184 = KP_LeftBrace
    toEnum 185 = KP_RightBrace
    toEnum 186 = KP_Tab
    toEnum 187 = KP_Backspace
    toEnum 188 = KP_A
    toEnum 189 = KP_B
    toEnum 190 = KP_C
    toEnum 191 = KP_D
    toEnum 192 = KP_E
    toEnum 193 = KP_F
    toEnum 194 = KP_XOR
    toEnum 195 = KP_Power
    toEnum 196 = KP_Percent
    toEnum 197 = KP_Less
    toEnum 198 = KP_Greater
    toEnum 199 = KP_Ampersand
    toEnum 200 = KP_DBLAmpersand
    toEnum 201 = KP_VerticalBar
    toEnum 202 = KP_DBLVerticalBar
    toEnum 203 = KP_Colon
    toEnum 204 = KP_Hash
    toEnum 205 = KP_Space
    toEnum 206 = KP_At
    toEnum 207 = KP_Exclam
    toEnum 208 = KP_MemStore
    toEnum 209 = KP_MemRecall
    toEnum 210 = KP_MemClear
    toEnum 211 = KP_MemAdd
    toEnum 212 = KP_MemSubtract
    toEnum 213 = KP_MemMultiply
    toEnum 214 = KP_MemDivide
    toEnum 215 = KP_PlusMinus
    toEnum 216 = KP_Clear
    toEnum 217 = KP_ClearEntry
    toEnum 218 = KP_Binary
    toEnum 219 = KP_Octal
    toEnum 220 = KP_Decimal
    toEnum 221 = KP_Hexadecimal

    toEnum 224 = LCTRL
    toEnum 225 = LShift
    toEnum 226 = LALT
    toEnum 227 = LGUI
    toEnum 228 = RCTRL
    toEnum 229 = RShift
    toEnum 230 = RALT
    toEnum 231 = RGUI

    toEnum 257 = Mode

    toEnum 258 = AudioNext
    toEnum 259 = AudioPrev
    toEnum 260 = AudioStop
    toEnum 261 = AudioPlay
    toEnum 262 = AudioMute
    toEnum 263 = MediaSelect
    toEnum 264 = WWW
    toEnum 265 = Mail
    toEnum 266 = Calculator
    toEnum 267 = Computer
    toEnum 268 = AC_Search
    toEnum 269 = AC_Home
    toEnum 270 = AC_Back
    toEnum 271 = AC_Forward
    toEnum 272 = AC_Stop
    toEnum 273 = AC_Refresh
    toEnum 274 = AC_Bookmarks

    toEnum 275 = BrightnessDown
    toEnum 276 = BrightnessUp
    toEnum 277 = DisplaySwitch

    toEnum 278 = KBDIllumToggle
    toEnum 279 = KBDIllumDown
    toEnum 280 = KBDIllumUp
    toEnum 281 = Eject
    toEnum 282 = Sleep

    toEnum 283 = App1
    toEnum 284 = App2
    toEnum _ = Unknown
