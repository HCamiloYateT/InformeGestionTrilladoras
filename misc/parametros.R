# Grupos con acceso total a todas las sucursales (en mayusculas para comparacion directa)
GRUPOS_FULL <- c("ANALÍTICA", "CONTROL INTERNO", "MESA DE NEGOCIOS", "TALENTO HUMANO", "CALIDAD")

# Mapeo grupo (mayusculas) -> wrapper del menu que debe hacerse visible
ACCESO_TABS <- list("TRILLADORA ARENALES"    = "wrap_arenales",
                    "TRILLADORA BACHUÉ"      = "wrap_bachue",
                    "TRILLADORA MEDELLÍN"    = "wrap_medellin",
                    "TRILLADORA POPAYÁN"     = "wrap_popayan",
                    "TRILLADORA ARMENIA"     = "wrap_armenia",
                    "TRILLADORA PEREIRA"     = "wrap_pereira",
                    "TRILLADORA BUCARAMANGA" = "wrap_bucaramanga",
                    "TRILLADORA HUILA"       = "wrap_huila")

# Vector con todos los wrappers del sidebar para iterar en toggle
TODOS_WRAPPERS <- c("wrap_consolidado", "wrap_arenales", "wrap_bachue", "wrap_medellin",
                    "wrap_popayan", "wrap_armenia", "wrap_pereira", "wrap_bucaramanga",
                    "wrap_huila")

# Mapeo tabName -> nombre de sucursal en los datos
TAB_A_SUCURSAL <- c(consolidado = "CONSOLIDADO", arenales = "ARENALES",
                    bachue = "BACHUÉ", medellin = "MEDELLÍN",
                    popayan = "POPAYÁN", armenia = "ARMENIA",
                    pereira = "PEREIRA", bucaramanga = "BUCARAMANGA",
                    huila = "HUILA")