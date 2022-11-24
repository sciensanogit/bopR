## save data into package data
usethis::use_data(BE_POP_MUNTY, BE_POP_ARRD, BE_POP_PROV, BE_POP_RGN,
                  BE_POP, BE_POP_PROJ_ARRD, BE_POP_PROJ_PROV, BE_POP_PROJ_RGN,
                  BE_POP_PROJ,
                  ARROND,FPB,
                  overwrite = TRUE,
                  compress = "xz", version = 2)
