module Draw.Color.Schemes.Internal.ColorBrewer2 where



import           Data.Vector (Vector)
import qualified Data.Vector as V

import Draw.Color.Schemes.Internal.Common



rgb256 :: Double -> Double -> Double -> RGB
rgb256 r g b = RGB (r/256) (g/256) (b/256)

divisive_Spectral :: Vector RGB
divisive_Spectral = V.fromList $
    [ rgb256 158 1   66
    , rgb256 213 62  79
    , rgb256 244 109 67
    , rgb256 253 174 97
    , rgb256 254 224 139
    , rgb256 255 255 191
    , rgb256 230 245 152
    , rgb256 171 221 164
    , rgb256 102 194 165
    , rgb256 50  136 189
    , rgb256 94  79  162
    ]

divisive_RdYlGn :: Vector RGB
divisive_RdYlGn = V.fromList $
    [ rgb256 165 0   38
    , rgb256 215 48  39
    , rgb256 244 109 67
    , rgb256 253 174 97
    , rgb256 254 224 139
    , rgb256 255 255 191
    , rgb256 217 239 139
    , rgb256 166 217 106
    , rgb256 102 189 99
    , rgb256 26  152 80
    , rgb256 0   104 55
    ]

divisive_RdBu :: Vector RGB
divisive_RdBu = V.fromList $
    [ rgb256 103 0   31
    , rgb256 178 24  43
    , rgb256 214 96  77
    , rgb256 244 165 130
    , rgb256 253 219 199
    , rgb256 247 247 247
    , rgb256 209 229 240
    , rgb256 146 197 222
    , rgb256 67  147 195
    , rgb256 33  102 172
    , rgb256 5   48  97
    ]

divisive_PiYG :: Vector RGB
divisive_PiYG = V.fromList $
    [ rgb256 142 1   82
    , rgb256 197 27  125
    , rgb256 222 119 174
    , rgb256 241 182 218
    , rgb256 253 224 239
    , rgb256 247 247 247
    , rgb256 230 245 208
    , rgb256 184 225 134
    , rgb256 127 188 65
    , rgb256 77  146 33
    , rgb256 39  100 25
    ]

divisive_PRGn :: Vector RGB
divisive_PRGn = V.fromList $
    [ rgb256 64  0   75
    , rgb256 118 42  131
    , rgb256 153 112 171
    , rgb256 194 165 207
    , rgb256 231 212 232
    , rgb256 247 247 247
    , rgb256 217 240 211
    , rgb256 166 219 160
    , rgb256 90  174 97
    , rgb256 27  120 55
    , rgb256 0   68  27
    ]

divisive_RdYlBu :: Vector RGB
divisive_RdYlBu = V.fromList $
    [ rgb256 165 0   38
    , rgb256 215 48  39
    , rgb256 244 109 67
    , rgb256 253 174 97
    , rgb256 254 224 144
    , rgb256 255 255 191
    , rgb256 224 243 248
    , rgb256 171 217 233
    , rgb256 116 173 209
    , rgb256 69  117 180
    , rgb256 49  54  149
    ]

divisive_BrBG :: Vector RGB
divisive_BrBG = V.fromList $
    [ rgb256 84  48  5
    , rgb256 140 81  10
    , rgb256 191 129 45
    , rgb256 223 194 125
    , rgb256 246 232 195
    , rgb256 245 245 245
    , rgb256 199 234 229
    , rgb256 128 205 193
    , rgb256 53  151 143
    , rgb256 1   102 94
    , rgb256 0   60  48
    ]

divisive_RdGy :: Vector RGB
divisive_RdGy = V.fromList $
    [ rgb256 103 0   31
    , rgb256 178 24  43
    , rgb256 214 96  77
    , rgb256 244 165 130
    , rgb256 253 219 199
    , rgb256 255 255 255
    , rgb256 224 224 224
    , rgb256 186 186 186
    , rgb256 135 135 135
    , rgb256 77  77  77
    , rgb256 26  26  26
    ]

divisive_PuOr :: Vector RGB
divisive_PuOr = V.fromList $
    [ rgb256 127 59  8
    , rgb256 179 88  6
    , rgb256 224 130 20
    , rgb256 253 184 99
    , rgb256 254 224 182
    , rgb256 247 247 247
    , rgb256 216 218 235
    , rgb256 178 171 210
    , rgb256 128 115 172
    , rgb256 84  39  136
    , rgb256 45  0   75
    ]

qualitative_Set2 :: Vector RGB
qualitative_Set2 = V.fromList $
    [ rgb256 102 194 165
    , rgb256 252 141 98
    , rgb256 141 160 203
    , rgb256 231 138 195
    , rgb256 166 216 84
    , rgb256 255 217 47
    , rgb256 229 196 148
    , rgb256 179 179 179
    ]

qualitative_Accent :: Vector RGB
qualitative_Accent = V.fromList $
    [ rgb256 127 201 127
    , rgb256 190 174 212
    , rgb256 253 192 134
    , rgb256 255 255 153
    , rgb256 56  108 176
    , rgb256 240 2   127
    , rgb256 191 91  23
    , rgb256 102 102 102
    ]

qualitative_Set1 :: Vector RGB
qualitative_Set1 = V.fromList $
    [ rgb256 228 26  28
    , rgb256 55  126 184
    , rgb256 77  175 74
    , rgb256 152 78  163
    , rgb256 255 127 0
    , rgb256 255 255 51
    , rgb256 166 86  40
    , rgb256 247 129 191
    , rgb256 153 153 153
    ]

qualitative_Set3 :: Vector RGB
qualitative_Set3 = V.fromList $
    [ rgb256 141 211 199
    , rgb256 255 255 179
    , rgb256 190 186 218
    , rgb256 251 128 114
    , rgb256 128 177 211
    , rgb256 253 180 98
    , rgb256 179 222 105
    , rgb256 252 205 229
    , rgb256 217 217 217
    , rgb256 188 128 189
    , rgb256 204 235 197
    , rgb256 255 237 111
    ]

qualitative_Dark2 :: Vector RGB
qualitative_Dark2 = V.fromList $
    [ rgb256 27  158 119
    , rgb256 217 95  2
    , rgb256 117 112 179
    , rgb256 231 41  138
    , rgb256 102 166 30
    , rgb256 230 171 2
    , rgb256 166 118 29
    , rgb256 102 102 102
    ]

qualitative_Paired :: Vector RGB
qualitative_Paired = V.fromList $
    [ rgb256 166 206 227
    , rgb256 31  120 180
    , rgb256 178 223 138
    , rgb256 51  160 44
    , rgb256 251 154 153
    , rgb256 227 26  28
    , rgb256 253 191 111
    , rgb256 255 127 0
    , rgb256 202 178 214
    , rgb256 106 61  154
    , rgb256 255 255 153
    , rgb256 177 89  40
    ]

qualitative_Pastel2 :: Vector RGB
qualitative_Pastel2 = V.fromList $
    [ rgb256 179 226 205
    , rgb256 253 205 172
    , rgb256 203 213 232
    , rgb256 244 202 228
    , rgb256 230 245 201
    , rgb256 255 242 174
    , rgb256 241 226 204
    , rgb256 204 204 204
    ]

qualitative_Pastel1 :: Vector RGB
qualitative_Pastel1 = V.fromList $
    [ rgb256 251 180 174
    , rgb256 179 205 227
    , rgb256 204 235 197
    , rgb256 222 203 228
    , rgb256 254 217 166
    , rgb256 255 255 204
    , rgb256 229 216 189
    , rgb256 253 218 236
    , rgb256 242 242 242
    ]

sequential_OrRd :: Vector RGB
sequential_OrRd = V.fromList $
    [ rgb256 255 247 236
    , rgb256 254 232 200
    , rgb256 253 212 158
    , rgb256 253 187 132
    , rgb256 252 141 89
    , rgb256 239 101 72
    , rgb256 215 48  31
    , rgb256 179 0   0
    , rgb256 127 0   0
    ]

sequential_PuBu :: Vector RGB
sequential_PuBu = V.fromList $
    [ rgb256 255 247 251
    , rgb256 236 231 242
    , rgb256 208 209 230
    , rgb256 166 189 219
    , rgb256 116 169 207
    , rgb256 54  144 192
    , rgb256 5   112 176
    , rgb256 4   90  141
    , rgb256 2   56  88
    ]

sequential_BuPu :: Vector RGB
sequential_BuPu = V.fromList $
    [ rgb256 247 252 253
    , rgb256 224 236 244
    , rgb256 191 211 230
    , rgb256 158 188 218
    , rgb256 140 150 198
    , rgb256 140 107 177
    , rgb256 136 65  157
    , rgb256 129 15  124
    , rgb256 77  0   75
    ]

sequential_Oranges :: Vector RGB
sequential_Oranges = V.fromList $
    [ rgb256 255 245 235
    , rgb256 254 230 206
    , rgb256 253 208 162
    , rgb256 253 174 107
    , rgb256 253 141 60
    , rgb256 241 105 19
    , rgb256 217 72  1
    , rgb256 166 54  3
    , rgb256 127 39  4
    ]

sequential_BuGn :: Vector RGB
sequential_BuGn = V.fromList $
    [ rgb256 247 252 253
    , rgb256 229 245 249
    , rgb256 204 236 230
    , rgb256 153 216 201
    , rgb256 102 194 164
    , rgb256 65  174 118
    , rgb256 35  139 69
    , rgb256 0   109 44
    , rgb256 0   68  27
    ]

sequential_YlOrBr :: Vector RGB
sequential_YlOrBr = V.fromList $
    [ rgb256 255 255 229
    , rgb256 255 247 188
    , rgb256 254 227 145
    , rgb256 254 196 79
    , rgb256 254 153 41
    , rgb256 236 112 20
    , rgb256 204 76  2
    , rgb256 153 52  4
    , rgb256 102 37  6
    ]

sequential_YlGn :: Vector RGB
sequential_YlGn = V.fromList $
    [ rgb256 255 255 229
    , rgb256 247 252 185
    , rgb256 217 240 163
    , rgb256 173 221 142
    , rgb256 120 198 121
    , rgb256 65  171 93
    , rgb256 35  132 67
    , rgb256 0   104 55
    , rgb256 0   69  41
    ]

sequential_Reds :: Vector RGB
sequential_Reds = V.fromList $
    [ rgb256 255 245 240
    , rgb256 254 224 210
    , rgb256 252 187 161
    , rgb256 252 146 114
    , rgb256 251 106 74
    , rgb256 239 59  44
    , rgb256 203 24  29
    , rgb256 165 15  21
    , rgb256 103 0   13
    ]

sequential_RdPu :: Vector RGB
sequential_RdPu = V.fromList $
    [ rgb256 255 247 243
    , rgb256 253 224 221
    , rgb256 252 197 192
    , rgb256 250 159 181
    , rgb256 247 104 161
    , rgb256 221 52  151
    , rgb256 174 1   126
    , rgb256 122 1   119
    , rgb256 73  0   106
    ]

sequential_Greens :: Vector RGB
sequential_Greens = V.fromList $
    [ rgb256 247 252 245
    , rgb256 229 245 224
    , rgb256 199 233 192
    , rgb256 161 217 155
    , rgb256 116 196 118
    , rgb256 65  171 93
    , rgb256 35  139 69
    , rgb256 0   109 44
    , rgb256 0   68  27
    ]

sequential_YlGnBu :: Vector RGB
sequential_YlGnBu = V.fromList $
    [ rgb256 255 255 217
    , rgb256 237 248 177
    , rgb256 199 233 180
    , rgb256 127 205 187
    , rgb256 65  182 196
    , rgb256 29  145 192
    , rgb256 34  94  168
    , rgb256 37  52  148
    , rgb256 8   29  88
    ]

sequential_Purples :: Vector RGB
sequential_Purples = V.fromList $
    [ rgb256 252 251 253
    , rgb256 239 237 245
    , rgb256 218 218 235
    , rgb256 188 189 220
    , rgb256 158 154 200
    , rgb256 128 125 186
    , rgb256 106 81  163
    , rgb256 84  39  143
    , rgb256 63  0   125
    ]

sequential_GnBu :: Vector RGB
sequential_GnBu = V.fromList $
    [ rgb256 247 252 240
    , rgb256 224 243 219
    , rgb256 204 235 197
    , rgb256 168 221 181
    , rgb256 123 204 196
    , rgb256 78  179 211
    , rgb256 43  140 190
    , rgb256 8   104 172
    , rgb256 8   64  129
    ]

sequential_Greys :: Vector RGB
sequential_Greys = V.fromList $
    [ rgb256 255 255 255
    , rgb256 240 240 240
    , rgb256 217 217 217
    , rgb256 189 189 189
    , rgb256 150 150 150
    , rgb256 115 115 115
    , rgb256 82  82  82
    , rgb256 37  37  37
    , rgb256 0   0   0
    ]

sequential_YlOrRd :: Vector RGB
sequential_YlOrRd = V.fromList $
    [ rgb256 255 255 204
    , rgb256 255 237 160
    , rgb256 254 217 118
    , rgb256 254 178 76
    , rgb256 253 141 60
    , rgb256 252 78  42
    , rgb256 227 26  28
    , rgb256 177 0   38
    ]

sequential_PuRd :: Vector RGB
sequential_PuRd = V.fromList $
    [ rgb256 247 244 249
    , rgb256 231 225 239
    , rgb256 212 185 218
    , rgb256 201 148 199
    , rgb256 223 101 176
    , rgb256 231 41  138
    , rgb256 206 18  86
    , rgb256 152 0   67
    , rgb256 103 0   31
    ]

sequential_Blues :: Vector RGB
sequential_Blues = V.fromList $
    [ rgb256 247 251 255
    , rgb256 222 235 247
    , rgb256 198 219 239
    , rgb256 158 202 225
    , rgb256 107 174 214
    , rgb256 66  146 198
    , rgb256 33  113 181
    , rgb256 8   81  156
    , rgb256 8   48  107
    ]

sequential_PuBuGn :: Vector RGB
sequential_PuBuGn = V.fromList $
    [ rgb256 255 247 251
    , rgb256 236 226 240
    , rgb256 208 209 230
    , rgb256 166 189 219
    , rgb256 103 169 207
    , rgb256 54  144 192
    , rgb256 2   129 138
    , rgb256 1   108 89
    , rgb256 1   70  54
    ]
