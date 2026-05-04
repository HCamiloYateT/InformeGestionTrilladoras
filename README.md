# Informe Gestión Trilladoras

Este proyecto es una aplicación **Shiny** organizada en scripts modulares para UI, servidor, funciones auxiliares y parámetros.

## Estructura del proyecto

```text
.
├── global.R
├── server.R
├── ui.R
└── misc
    ├── functions.R
    ├── parametros.R
    ├── values.R
    ├── modules
    │   ├── Consolidado.R
    │   ├── DetalleMunicipios.R
    │   └── DetalleProveedores.R
    └── ui
        ├── body.R
        ├── controlbar.R
        ├── footer.R
        ├── header.R
        ├── preloader.R
        └── sidebar.R
```

## Descripción rápida

- `ui.R`: define la interfaz principal de la aplicación.
- `server.R`: contiene la lógica del servidor.
- `global.R`: carga librerías, configuración global y recursos compartidos.
- `misc/functions.R`: funciones de apoyo reutilizables.
- `misc/parametros.R`: parámetros de configuración de la app.
- `misc/values.R`: valores compartidos/constantes.
- `misc/modules/`: módulos funcionales de la aplicación.
- `misc/ui/`: componentes de interfaz divididos por secciones.
