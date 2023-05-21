module Draw.Plotting.PaperSize (
    -- * DIN A5
      paper_a5_short_mm, paper_a5_long_mm
    -- * DIN A4
    , paper_a4_short_mm, paper_a4_long_mm
    -- * DIN A3
    , paper_a3_short_mm, paper_a3_long_mm
    -- * DIN A2
    , paper_a2_short_mm, paper_a2_long_mm
    -- * DIN A1
    , paper_a1_short_mm, paper_a1_long_mm
) where


paper_a5_short_mm, paper_a5_long_mm :: Double
paper_a5_short_mm = 148
paper_a5_long_mm = 210

paper_a4_short_mm, paper_a4_long_mm :: Double
paper_a4_short_mm = paper_a5_long_mm
paper_a4_long_mm = 297

paper_a3_short_mm, paper_a3_long_mm :: Double
paper_a3_short_mm = paper_a4_long_mm
paper_a3_long_mm = 420

paper_a2_short_mm, paper_a2_long_mm :: Double
paper_a2_short_mm = paper_a3_long_mm
paper_a2_long_mm = 594

paper_a1_short_mm, paper_a1_long_mm :: Double
paper_a1_short_mm = paper_a2_long_mm
paper_a1_long_mm = 841
