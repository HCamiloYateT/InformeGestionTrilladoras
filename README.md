# Informe Gestión Trilladoras

## Dependencias entre módulos

Este proyecto es una aplicación **Shiny + bs4Dash** con carga dinámica de código desde `misc/`.

### 1) Flujo principal de arranque

```text
app.R implícito (global.R + ui.R + server.R)
  ├─ global.R
  │   ├─ carga paquetes y datos globales
  │   └─ ejecuta load_modules("misc") para registrar funciones/UI/modules
  ├─ ui.R
  │   └─ construye bs4DashPage usando objetos UI definidos en misc/ui/*.R
  └─ server.R
      └─ inicializa el módulo Consolidado(...)
```

### 2) Dependencias de UI

```text
ui.R
  └─ misc/ui/body.R
      └─ usa ConsolidadoUI("Consolidado")
          └─ definido en misc/modules/Consolidado.R
```

### 3) Dependencias funcionales entre módulos

```text
Consolidado (misc/modules/Consolidado.R)
  ├─ usa DetalleProv(...)
  │   └─ definido en misc/modules/DetalleProveedores.R
  ├─ usa DetalleMun(...)
  │   └─ definido en misc/modules/DetalleMunicipio.R
  │       └─ usa MapaDoble(...)
  │           └─ definido en misc/modules/MapaDoble.R
  └─ usa componentes compartidos (p. ej. TablaReactable/UI y helpers racafe*)
```

### 4) Dependencias de datos y objetos globales

- `global.R` carga objetos geográficos (`geo_dpto`, `geo_mun`, `geo_centroides`, `GeoTrilladoras`) y datos preprocesados desde `data/data.RData`.
- `server.R` alimenta `Consolidado(...)` con reactivos y tablas de indicadores/detalle (`data_trl`, `Indicadores_*`, `detalle_prov`, `detalle_mun`).
- `DetalleMun` consume `GeoTrilladoras` para ubicar marcadores en mapa.

### 5) Orden de carga recomendado (ya implementado)

`load_modules()` recorre `misc/**/*.R` por profundidad de ruta y reintenta archivos fallidos en múltiples pasadas; esto permite resolver dependencias cruzadas entre módulos sin hardcodear un orden único.

## Nota de mantenimiento

Cuando se agregue un módulo nuevo:

1. Definir su `*UI` y su `moduleServer` en `misc/modules/`.
2. Enlazarlo desde el módulo padre (por ejemplo, `Consolidado`) o desde `misc/ui/body.R`.
3. Documentar aquí su dependencia de entrada/salida para mantener actualizado el mapa de módulos.
