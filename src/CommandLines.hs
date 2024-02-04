{-
-- EPITECH PROJECT, 2024
-- GLaDOS
-- File description:
-- CommandLines.hs
-}

module CommandLines (quitCommand, helpCommand, manCommand) where

quitCommand :: IO ()
quitCommand = putStrLn "Quitting Glados."

manCommand :: IO ()
manCommand = putStrLn $ "Welcome to the manual !\n\n"
    ++ "Program Structure:\n"
    ++ "func name(x, y) {\n \t// I"
    ++ "nstructions\n}\n\n"
    ++ "Data Types:\n"
    ++ "    - Integers (e.g. 1, 42)\n"
    ++ "    - Character strings "
    ++ "(e.g. “Hello, World!”)\n\n"
    ++ "Instructions:\n\n"
    ++ "Declaration of Variables:\n"
    ++ "variable = value;\n\n"
    ++ "If-Else condition:\n"
    ++ "if (condition) {\n"
    ++ "\t// Instructions if the "
    ++ "condition is true\n"
    ++ "} else {\n"
    ++ "\t// Instructions if the "
    ++ "condition is false\n}\n\n"
    ++ "While Loop:\n"
    ++ "while (condition) {\n"
    ++ "\t// Instructions to repeat as long as the "
    ++ "condition is true\n}\n\n"
    ++ "Pre-defined functions:\n"
    ++ "    - add(a, b): Addition of two numbers.\n"
    ++ "    - sub(a, b): Subtraction of b from a.\n"
    ++ "    - mul(a, b): Multiplication of two numb"
    ++ "ers.\n"
    ++ "    - div(a, b): Division of "
    ++ "a by b.\n"
    ++ "    - mod(a, b): Remainder of "
    ++ "dividing a by b.\n"
    ++ "    - equal(a, b): Checks if "
    ++ "a is equal to b.\n"
    ++ "    - ne(a, b): Checks if a "
    ++ "is not equal to b.\n"
    ++ "    - greater(a, b): Checks if a is"
    ++ "strictly greater than b.\n"
    ++ "    - lower(a, b): Checks if a is "
    ++ "strictly lower than b.\n"
    ++ "    - leq(a, b): Check if a is "
    ++ "less than or equal to b.\n"
    ++ "    - geq(a, b): Check if a is "
    ++ "greater than or equal to b.\n"
    ++ "    - print(value): Show content.\n"

helpCommand :: IO ()
helpCommand = putStrLn $ "./glados [--compile file | --disassemble file | --vm file | --man | --help]\n\n"
    ++ "--compile file: Compile and run the "
    ++ "GLaDOS program from the specified file.\n"
    ++ "--disassemble file: Disassemble the bytecode "
    ++ "of the GLaDOS program from the specified file.\n"
    ++ "--vm file: Run the GLaDOS program using the virtual "
    ++ "machine from the specified file.\n"
    ++ "--man: Display the manual for GLaDOS.\n"
    ++ "--help: Display this help message.\n"
