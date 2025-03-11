import Data.List

-- Definir el tipo de dato para Markdown
-- Cada tipo de dato representa una estructura específica de Markdown
data MarkdownElement = Header Int String  -- Representa encabezados de distintos niveles (Int indica el nivel)
                      | Bold String       -- Representa texto en negrita
                      | Italic String     -- Representa texto en cursiva
                      | OrderedList [String]  -- Representa listas ordenadas (con números)
                      | UnorderedList [String] -- Representa listas no ordenadas (con guiones o asteriscos)
                      | Paragraph String  -- Representa párrafos de texto normal
                      deriving (Show)  -- Permite imprimir estos elementos en consola

-- Función para detectar encabezados
parseHeader :: String -> Maybe MarkdownElement
parseHeader str = case length (takeWhile (== '#') str) of  -- Cuenta la cantidad de '#' al inicio
    n | n > 0 && n <= 6 && (length str > n) && (str !! n == ' ') -> Just (Header n (drop (n + 1) str))  -- Si es válido, devuelve un Header
    _ -> Nothing  -- Si no es un encabezado válido, devuelve Nothing

-- Función para agrupar listas consecutivas
parseLists :: [String] -> ([MarkdownElement], [String])
parseLists [] = ([], [])  -- Caso base: si la lista está vacía, devuelve listas vacías
parseLists (x:xs)
    -- Detecta listas no ordenadas ('- ' o '* ')
    | "- " `isPrefixOf` x || "* " `isPrefixOf` x =
        let (items, rest) = parseLists xs  -- Procesa el resto de las líneas
        in case items of
            (UnorderedList ys : ysRest) -> (UnorderedList ([drop 2 x] ++ ys) : ysRest, rest)  -- Agrega al inicio de la lista existente
            _ -> (UnorderedList [drop 2 x] : items, rest)  -- Crea una nueva lista no ordenada
    -- Detecta listas ordenadas ('1. ', '2. ', etc.)
    | "1. " `isPrefixOf` x || "2. " `isPrefixOf` x =
        let (items, rest) = parseLists xs  -- Procesa el resto de las líneas
        in case items of
            (OrderedList ys : ysRest) -> (OrderedList ([drop 3 x] ++ ys) : ysRest, rest)  -- Agrega al inicio de la lista existente
            _ -> (OrderedList [drop 3 x] : items, rest)  -- Crea una nueva lista ordenada
    | otherwise = ([], x:xs)  -- No es una lista, devuelve la cadena intacta

-- Función para detectar texto en negrita y cursiva
parseFormatting :: String -> [MarkdownElement]
parseFormatting "" = []  -- Caso base: si la cadena está vacía, no hay elementos
parseFormatting str =
    case breakOn "**" str of  -- Busca el primer marcador de negrita "**"
        (before, "") -> parseItalics before  -- Si no hay "**", busca cursiva
        (before, rest) ->
            let (bold, after) = breakOn "**" (drop 0 rest)  -- Extrae el texto en negrita entre "**"
            in if null bold then parseItalics str  -- Si no hay contenido válido, procesa cursiva
               else filterNonEmpty [Paragraph before, Bold ("" ++ bold ++ "")] ++ parseFormatting after  -- Devuelve texto formateado
  where
    -- Busca texto en cursiva ('*')
    parseItalics s = case breakOn "*" s of  -- Busca el primer marcador de cursiva "*"
        (before, "") -> [Paragraph before]  -- Si no hay marcador, lo trata como párrafo
        (before, rest) ->
            let (italic, after) = breakOn "*" (drop 0 rest)  -- Extrae el texto en cursiva entre "*"
            in if null italic then [Paragraph s]  -- Si no hay contenido válido, trata como párrafo
               else filterNonEmpty [Paragraph before, Italic ("" ++ italic ++ "")] ++ parseFormatting after  -- Devuelve texto formateado

-- Función auxiliar para dividir una cadena sin perder caracteres
breakOn :: String -> String -> (String, String)
breakOn _ "" = ("", "")  -- Caso base: cadena vacía
breakOn delim str = 
    case findIndex (isPrefixOf delim) (tails str) of  -- Busca la primera aparición del delimitador
        Just idx -> (take idx str, drop (idx + length delim) str)  -- Divide en la primera aparición de delim
        Nothing  -> (str, "")  -- Si no encuentra el delimitador, devuelve la cadena completa

-- Función auxiliar para eliminar párrafos vacíos
filterNonEmpty :: [MarkdownElement] -> [MarkdownElement]
filterNonEmpty = filter (\e -> case e of Paragraph "" -> False; _ -> True)  -- Filtra párrafos vacíos

-- Función principal de parsing
parseMarkdown :: String -> [MarkdownElement]
parseMarkdown input = parseLines (lines input)  -- Divide el texto en líneas y las procesa

-- Procesa línea por línea el texto Markdown
parseLines :: [String] -> [MarkdownElement]
parseLines [] = []  -- Caso base: si no hay líneas, devuelve una lista vacía
parseLines (l:ls) =
    case parseHeader l of
        Just header -> header : parseLines ls  -- Si es encabezado, lo procesa y continúa
        Nothing ->
            let (lists, rest) = parseLists (l:ls)  -- Detecta listas
            in if not (null lists) then lists ++ parseLines rest  -- Si hay listas, las procesa
               else parseFormatting l ++ parseLines ls  -- Si no es lista, procesa formato de texto

-- Ejemplo de uso
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
