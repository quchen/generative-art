module Draw.Color.Schemes.Internal.ColorBrewer2 (
    module Draw.Color.Schemes.Internal.ColorBrewer2
) where



import           Data.Vector (Vector)
import qualified Data.Vector as V

import Draw.Color.Schemes.Internal.Common



divisive_Spectral :: Vector RGB
divisive_Spectral = V.fromList
    [ rgbFF 158 1   66
    , rgbFF 213 62  79
    , rgbFF 244 109 67
    , rgbFF 253 174 97
    , rgbFF 254 224 139
    , rgbFF 255 255 191
    , rgbFF 230 245 152
    , rgbFF 171 221 164
    , rgbFF 102 194 165
    , rgbFF 50  136 189
    , rgbFF 94  79  162
    ]

divisive_RdYlGn :: Vector RGB
divisive_RdYlGn = V.fromList
    [ rgbFF 165 0   38
    , rgbFF 215 48  39
    , rgbFF 244 109 67
    , rgbFF 253 174 97
    , rgbFF 254 224 139
    , rgbFF 255 255 191
    , rgbFF 217 239 139
    , rgbFF 166 217 106
    , rgbFF 102 189 99
    , rgbFF 26  152 80
    , rgbFF 0   104 55
    ]

divisive_RdBu :: Vector RGB
divisive_RdBu = V.fromList
    [ rgbFF 103 0   31
    , rgbFF 178 24  43
    , rgbFF 214 96  77
    , rgbFF 244 165 130
    , rgbFF 253 219 199
    , rgbFF 247 247 247
    , rgbFF 209 229 240
    , rgbFF 146 197 222
    , rgbFF 67  147 195
    , rgbFF 33  102 172
    , rgbFF 5   48  97
    ]

divisive_PiYG :: Vector RGB
divisive_PiYG = V.fromList
    [ rgbFF 142 1   82
    , rgbFF 197 27  125
    , rgbFF 222 119 174
    , rgbFF 241 182 218
    , rgbFF 253 224 239
    , rgbFF 247 247 247
    , rgbFF 230 245 208
    , rgbFF 184 225 134
    , rgbFF 127 188 65
    , rgbFF 77  146 33
    , rgbFF 39  100 25
    ]

divisive_PRGn :: Vector RGB
divisive_PRGn = V.fromList
    [ rgbFF 64  0   75
    , rgbFF 118 42  131
    , rgbFF 153 112 171
    , rgbFF 194 165 207
    , rgbFF 231 212 232
    , rgbFF 247 247 247
    , rgbFF 217 240 211
    , rgbFF 166 219 160
    , rgbFF 90  174 97
    , rgbFF 27  120 55
    , rgbFF 0   68  27
    ]

divisive_RdYlBu :: Vector RGB
divisive_RdYlBu = V.fromList
    [ rgbFF 165 0   38
    , rgbFF 215 48  39
    , rgbFF 244 109 67
    , rgbFF 253 174 97
    , rgbFF 254 224 144
    , rgbFF 255 255 191
    , rgbFF 224 243 248
    , rgbFF 171 217 233
    , rgbFF 116 173 209
    , rgbFF 69  117 180
    , rgbFF 49  54  149
    ]

divisive_BrBG :: Vector RGB
divisive_BrBG = V.fromList
    [ rgbFF 84  48  5
    , rgbFF 140 81  10
    , rgbFF 191 129 45
    , rgbFF 223 194 125
    , rgbFF 246 232 195
    , rgbFF 245 245 245
    , rgbFF 199 234 229
    , rgbFF 128 205 193
    , rgbFF 53  151 143
    , rgbFF 1   102 94
    , rgbFF 0   60  48
    ]

divisive_RdGy :: Vector RGB
divisive_RdGy = V.fromList
    [ rgbFF 103 0   31
    , rgbFF 178 24  43
    , rgbFF 214 96  77
    , rgbFF 244 165 130
    , rgbFF 253 219 199
    , rgbFF 255 255 255
    , rgbFF 224 224 224
    , rgbFF 186 186 186
    , rgbFF 135 135 135
    , rgbFF 77  77  77
    , rgbFF 26  26  26
    ]

divisive_PuOr :: Vector RGB
divisive_PuOr = V.fromList
    [ rgbFF 127 59  8
    , rgbFF 179 88  6
    , rgbFF 224 130 20
    , rgbFF 253 184 99
    , rgbFF 254 224 182
    , rgbFF 247 247 247
    , rgbFF 216 218 235
    , rgbFF 178 171 210
    , rgbFF 128 115 172
    , rgbFF 84  39  136
    , rgbFF 45  0   75
    ]

qualitative_Set2 :: Vector RGB
qualitative_Set2 = V.fromList
    [ rgbFF 102 194 165
    , rgbFF 252 141 98
    , rgbFF 141 160 203
    , rgbFF 231 138 195
    , rgbFF 166 216 84
    , rgbFF 255 217 47
    , rgbFF 229 196 148
    , rgbFF 179 179 179
    ]

qualitative_Accent :: Vector RGB
qualitative_Accent = V.fromList
    [ rgbFF 127 201 127
    , rgbFF 190 174 212
    , rgbFF 253 192 134
    , rgbFF 255 255 153
    , rgbFF 56  108 176
    , rgbFF 240 2   127
    , rgbFF 191 91  23
    , rgbFF 102 102 102
    ]

qualitative_Set1 :: Vector RGB
qualitative_Set1 = V.fromList
    [ rgbFF 228 26  28
    , rgbFF 55  126 184
    , rgbFF 77  175 74
    , rgbFF 152 78  163
    , rgbFF 255 127 0
    , rgbFF 255 255 51
    , rgbFF 166 86  40
    , rgbFF 247 129 191
    , rgbFF 153 153 153
    ]

qualitative_Set3 :: Vector RGB
qualitative_Set3 = V.fromList
    [ rgbFF 141 211 199
    , rgbFF 255 255 179
    , rgbFF 190 186 218
    , rgbFF 251 128 114
    , rgbFF 128 177 211
    , rgbFF 253 180 98
    , rgbFF 179 222 105
    , rgbFF 252 205 229
    , rgbFF 217 217 217
    , rgbFF 188 128 189
    , rgbFF 204 235 197
    , rgbFF 255 237 111
    ]

qualitative_Dark2 :: Vector RGB
qualitative_Dark2 = V.fromList
    [ rgbFF 27  158 119
    , rgbFF 217 95  2
    , rgbFF 117 112 179
    , rgbFF 231 41  138
    , rgbFF 102 166 30
    , rgbFF 230 171 2
    , rgbFF 166 118 29
    , rgbFF 102 102 102
    ]

qualitative_Paired :: Vector RGB
qualitative_Paired = V.fromList
    [ rgbFF 166 206 227
    , rgbFF 31  120 180
    , rgbFF 178 223 138
    , rgbFF 51  160 44
    , rgbFF 251 154 153
    , rgbFF 227 26  28
    , rgbFF 253 191 111
    , rgbFF 255 127 0
    , rgbFF 202 178 214
    , rgbFF 106 61  154
    , rgbFF 255 255 153
    , rgbFF 177 89  40
    ]

qualitative_Pastel2 :: Vector RGB
qualitative_Pastel2 = V.fromList
    [ rgbFF 179 226 205
    , rgbFF 253 205 172
    , rgbFF 203 213 232
    , rgbFF 244 202 228
    , rgbFF 230 245 201
    , rgbFF 255 242 174
    , rgbFF 241 226 204
    , rgbFF 204 204 204
    ]

qualitative_Pastel1 :: Vector RGB
qualitative_Pastel1 = V.fromList
    [ rgbFF 251 180 174
    , rgbFF 179 205 227
    , rgbFF 204 235 197
    , rgbFF 222 203 228
    , rgbFF 254 217 166
    , rgbFF 255 255 204
    , rgbFF 229 216 189
    , rgbFF 253 218 236
    , rgbFF 242 242 242
    ]

sequential_OrRd :: Vector RGB
sequential_OrRd = V.fromList
    [ rgbFF 255 247 236
    , rgbFF 254 232 200
    , rgbFF 253 212 158
    , rgbFF 253 187 132
    , rgbFF 252 141 89
    , rgbFF 239 101 72
    , rgbFF 215 48  31
    , rgbFF 179 0   0
    , rgbFF 127 0   0
    ]

sequential_PuBu :: Vector RGB
sequential_PuBu = V.fromList
    [ rgbFF 255 247 251
    , rgbFF 236 231 242
    , rgbFF 208 209 230
    , rgbFF 166 189 219
    , rgbFF 116 169 207
    , rgbFF 54  144 192
    , rgbFF 5   112 176
    , rgbFF 4   90  141
    , rgbFF 2   56  88
    ]

sequential_BuPu :: Vector RGB
sequential_BuPu = V.fromList
    [ rgbFF 247 252 253
    , rgbFF 224 236 244
    , rgbFF 191 211 230
    , rgbFF 158 188 218
    , rgbFF 140 150 198
    , rgbFF 140 107 177
    , rgbFF 136 65  157
    , rgbFF 129 15  124
    , rgbFF 77  0   75
    ]

sequential_Oranges :: Vector RGB
sequential_Oranges = V.fromList
    [ rgbFF 255 245 235
    , rgbFF 254 230 206
    , rgbFF 253 208 162
    , rgbFF 253 174 107
    , rgbFF 253 141 60
    , rgbFF 241 105 19
    , rgbFF 217 72  1
    , rgbFF 166 54  3
    , rgbFF 127 39  4
    ]

sequential_BuGn :: Vector RGB
sequential_BuGn = V.fromList
    [ rgbFF 247 252 253
    , rgbFF 229 245 249
    , rgbFF 204 236 230
    , rgbFF 153 216 201
    , rgbFF 102 194 164
    , rgbFF 65  174 118
    , rgbFF 35  139 69
    , rgbFF 0   109 44
    , rgbFF 0   68  27
    ]

sequential_YlOrBr :: Vector RGB
sequential_YlOrBr = V.fromList
    [ rgbFF 255 255 229
    , rgbFF 255 247 188
    , rgbFF 254 227 145
    , rgbFF 254 196 79
    , rgbFF 254 153 41
    , rgbFF 236 112 20
    , rgbFF 204 76  2
    , rgbFF 153 52  4
    , rgbFF 102 37  6
    ]

sequential_YlGn :: Vector RGB
sequential_YlGn = V.fromList
    [ rgbFF 255 255 229
    , rgbFF 247 252 185
    , rgbFF 217 240 163
    , rgbFF 173 221 142
    , rgbFF 120 198 121
    , rgbFF 65  171 93
    , rgbFF 35  132 67
    , rgbFF 0   104 55
    , rgbFF 0   69  41
    ]

sequential_Reds :: Vector RGB
sequential_Reds = V.fromList
    [ rgbFF 255 245 240
    , rgbFF 254 224 210
    , rgbFF 252 187 161
    , rgbFF 252 146 114
    , rgbFF 251 106 74
    , rgbFF 239 59  44
    , rgbFF 203 24  29
    , rgbFF 165 15  21
    , rgbFF 103 0   13
    ]

sequential_RdPu :: Vector RGB
sequential_RdPu = V.fromList
    [ rgbFF 255 247 243
    , rgbFF 253 224 221
    , rgbFF 252 197 192
    , rgbFF 250 159 181
    , rgbFF 247 104 161
    , rgbFF 221 52  151
    , rgbFF 174 1   126
    , rgbFF 122 1   119
    , rgbFF 73  0   106
    ]

sequential_Greens :: Vector RGB
sequential_Greens = V.fromList
    [ rgbFF 247 252 245
    , rgbFF 229 245 224
    , rgbFF 199 233 192
    , rgbFF 161 217 155
    , rgbFF 116 196 118
    , rgbFF 65  171 93
    , rgbFF 35  139 69
    , rgbFF 0   109 44
    , rgbFF 0   68  27
    ]

sequential_YlGnBu :: Vector RGB
sequential_YlGnBu = V.fromList
    [ rgbFF 255 255 217
    , rgbFF 237 248 177
    , rgbFF 199 233 180
    , rgbFF 127 205 187
    , rgbFF 65  182 196
    , rgbFF 29  145 192
    , rgbFF 34  94  168
    , rgbFF 37  52  148
    , rgbFF 8   29  88
    ]

sequential_Purples :: Vector RGB
sequential_Purples = V.fromList
    [ rgbFF 252 251 253
    , rgbFF 239 237 245
    , rgbFF 218 218 235
    , rgbFF 188 189 220
    , rgbFF 158 154 200
    , rgbFF 128 125 186
    , rgbFF 106 81  163
    , rgbFF 84  39  143
    , rgbFF 63  0   125
    ]

sequential_GnBu :: Vector RGB
sequential_GnBu = V.fromList
    [ rgbFF 247 252 240
    , rgbFF 224 243 219
    , rgbFF 204 235 197
    , rgbFF 168 221 181
    , rgbFF 123 204 196
    , rgbFF 78  179 211
    , rgbFF 43  140 190
    , rgbFF 8   104 172
    , rgbFF 8   64  129
    ]

sequential_Greys :: Vector RGB
sequential_Greys = V.fromList
    [ rgbFF 255 255 255
    , rgbFF 240 240 240
    , rgbFF 217 217 217
    , rgbFF 189 189 189
    , rgbFF 150 150 150
    , rgbFF 115 115 115
    , rgbFF 82  82  82
    , rgbFF 37  37  37
    , rgbFF 0   0   0
    ]

sequential_YlOrRd :: Vector RGB
sequential_YlOrRd = V.fromList
    [ rgbFF 255 255 204
    , rgbFF 255 237 160
    , rgbFF 254 217 118
    , rgbFF 254 178 76
    , rgbFF 253 141 60
    , rgbFF 252 78  42
    , rgbFF 227 26  28
    , rgbFF 177 0   38
    ]

sequential_PuRd :: Vector RGB
sequential_PuRd = V.fromList
    [ rgbFF 247 244 249
    , rgbFF 231 225 239
    , rgbFF 212 185 218
    , rgbFF 201 148 199
    , rgbFF 223 101 176
    , rgbFF 231 41  138
    , rgbFF 206 18  86
    , rgbFF 152 0   67
    , rgbFF 103 0   31
    ]

sequential_Blues :: Vector RGB
sequential_Blues = V.fromList
    [ rgbFF 247 251 255
    , rgbFF 222 235 247
    , rgbFF 198 219 239
    , rgbFF 158 202 225
    , rgbFF 107 174 214
    , rgbFF 66  146 198
    , rgbFF 33  113 181
    , rgbFF 8   81  156
    , rgbFF 8   48  107
    ]

sequential_PuBuGn :: Vector RGB
sequential_PuBuGn = V.fromList
    [ rgbFF 255 247 251
    , rgbFF 236 226 240
    , rgbFF 208 209 230
    , rgbFF 166 189 219
    , rgbFF 103 169 207
    , rgbFF 54  144 192
    , rgbFF 2   129 138
    , rgbFF 1   108 89
    , rgbFF 1   70  54
    ]
