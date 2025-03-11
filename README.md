# Analizador de Markdown en Haskell

Este proyecto implementa un analizador de Markdown en Haskell, capaz de interpretar distintos elementos de formato y convertirlos en una representación estructurada utilizando un tipo de dato personalizado.

## Características principales

- **Encabezados**: Soporta niveles del 1 al 6 (por ejemplo, `# Encabezado 1` hasta `###### Encabezado 6`).
- **Formato de texto**:
  - Negrita (`**texto**`).
  - Cursiva (`*texto*`).
- **Listas**:
  - Ordenadas (`1. Elemento`, `2. Elemento`...).
  - No ordenadas (`- Elemento` o `* Elemento`).
- **Párrafos**: Detecta bloques de texto sin formato adicional.

## Implementación

El código define un tipo de dato `MarkdownElement` que representa los distintos elementos de Markdown. Para el análisis, se implementan funciones especializadas:

- `parseHeader`: Detecta encabezados basados en el número de `#`.
- `parseLists`: Identifica listas ordenadas y no ordenadas, agrupando elementos consecutivos.
- `parseFormatting`: Interpreta texto en negrita y cursiva.
- `parseMarkdown`: Función principal que procesa el texto línea por línea.

## Ejemplo de uso

El archivo `main` contiene un ejemplo de Markdown de prueba que se parsea y se imprime en consola:

```haskell
main :: IO ()
main = do
    let mdExample = "# Titulo Principal\n"
                    ++ "## Subtitulo\n"
                    ++ "Texto normal con **negrita** y *cursiva*.\n\n"
                    ++ "1. Primer item\n"
                    ++ "2. Segundo item\n\n"
                    ++ "- Elemento de lista\n"
                    ++ "- Otro elemento\n"
    let parsed = parseMarkdown mdExample  -- Procesa el texto Markdown de prueba
    mapM_ print parsed  -- Imprime la lista de elementos Markdown parseados
```

## Requisitos

- GHC (Glasgow Haskell Compiler)
- Módulo `Data.List` para manipulación de listas

## Ejecución

Compila y ejecuta el código con los siguientes comandos:

```sh
ghc -o markdownParser Main.hs
./markdownParser
```

Este programa es ideal para interpretar y procesar archivos Markdown dentro de proyectos en Haskell.

