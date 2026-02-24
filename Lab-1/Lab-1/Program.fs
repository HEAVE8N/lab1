open System

// --- Вспомогательные типы и функции для Задания 3 ---
type Complex = { re: float; im: float }

let add c1 c2 = { re = c1.re + c2.re; im = c1.im + c2.im }//add - сложение

let sub c1 c2 = { re = c1.re - c2.re; im = c1.im - c2.im }//sub - вычитания
let mul c1 c2 =                                          // mul - умножени
    { re = c1.re * c2.re - c1.im * c2.im; 
      im = c1.re * c2.im + c1.im * c2.re }
let div c1 c2 =                                          //div - деление
    let denom = c2.re * c2.re + c2.im * c2.im
    { re = (c1.re * c2.re + c1.im * c2.im) / denom;
      im = (c1.im * c2.re - c1.re * c2.im) / denom }
let rec pow (c: Complex) (n: int) =                     // pow - степень
    if n < 0 then
        printfn "Степень должна быть неотрицательной"
        { re = 1.0; im = 0.0 }
    elif n = 0 then { re = 1.0; im = 0.0 }
    elif n = 1 then c
    else
        let p = pow c (n / 2)
        let p2 = mul p p
        if n % 2 = 0 then p2 else mul c p2

let printComplex label c =                          //функция для выводав комплексного числа
    printfn "%s: %.2f + (%.2fi)" label c.re c.im

let readFloat prompt =                              //функция для чтения комплексного числа
    printf "%s" prompt
    match Double.TryParse(Console.ReadLine()) with

    | true, value -> value
    | false, _ -> 
        printfn "Ошибка ввода! Используем 0.0"
        0.0

// --- Точка входа ---
[<EntryPoint>]
let main argv =
    let mutable running = true                     // флаг работы

    while running do                               
        printfn "Выберите номер задания (1, 2, 3) или '0' для выхода:"
        let choice = Console.ReadLine()

        match choice with                          // для вывбора лабораторная, по примеру c++ switchcase()

        | "1" ->
            printf "Введите максимальную степень для числа 2: "
            let input = Console.ReadLine()
            match Int32.TryParse(input) with       // для безопасной обработки ошибок
            | true, x ->
                let y = [ for i in 0 .. x -> pown 2 i ]
                printfn "\nРезультат: %A" y

            | _ -> printfn "Ошибка: введите целое число!"

        | "2" ->
            printf "Введите натуральное число: "
            let input = Console.ReadLine()
            let success, y = Int32.TryParse(input)  // разделяет проверку функции на 2 шага ( проверка переменой y на наличие числа)
            if not success then
                printfn "Ошибка: Вы ввели не число!"
            elif y <= 0 then
                printfn "Ошибка: Число должно быть натуральным (больше 0)!"
            else
                let mutable n = y
                while n >= 10 do n <- n / 10
                printfn "Первая цифра числа %d — это %d" y n


        | "3" ->
            printfn "=== Комплексные числа ==="
            let a1 = readFloat "Введите a1 (c1): "
            let b1 = readFloat "Введите b1 (c1): "
            let c1 = { re = a1; im = b1 }

            let a2 = readFloat "Введите a2 (c2): "
            let b2 = readFloat "Введите b2 (c2): "
            let c2 = { re = a2; im = b2 }
            let b2 = readFloat "Введите степень (c3): "
            let c3 =b2|> int


            printfn "\n--- Результаты ---"
            printComplex "Сложение " (add c1 c2)
            printComplex "Вычитание" (sub c1 c2)
            printComplex "Умножение" (mul c1 c2)
            printComplex "Степень" (pow c2 c3)

            if c2.re = 0.0 && c2.im = 0.0 then
                printfn "Деление : Ошибка (деление на ноль)!"
            else
                printComplex "Деление  " (div c1 c2)

        | "0" -> 
            running <- false                    // команда смены состояния флага
            printfn "Выход из программы..."


        | _ -> 
            printfn "Некорректный выбор. Попробуйте еще раз."

    if running = false then                    //проверка флага ( ожидандает любой клавиши для закрытия )

        printfn "\nПрограмма завершена. Нажми Enter для закрытия окна..."
        Console.ReadLine() |> ignore
    
    
    0
