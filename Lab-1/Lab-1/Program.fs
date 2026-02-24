open System

// --- Вспомогательные типы и функции для Задания 3 ---
type Complex = { re: float; im: float }

let add c1 c2 = { re = c1.re + c2.re; im = c1.im + c2.im }
let sub c1 c2 = { re = c1.re - c2.re; im = c1.im - c2.im }
let mul c1 c2 = 
    { re = c1.re * c2.re - c1.im * c2.im; 
      im = c1.re * c2.im + c1.im * c2.re }
let div c1 c2 = 
    let denom = c2.re * c2.re + c2.im * c2.im
    { re = (c1.re * c2.re + c1.im * c2.im) / denom;
      im = (c1.im * c2.re - c1.re * c2.im) / denom }

let printComplex label c =
    printfn "%s: %.2f + (%.2fi)" label c.re c.im

let readFloat prompt =
    printf "%s" prompt
    match Double.TryParse(Console.ReadLine()) with

    | true, value -> value
    | false, _ -> 
        printfn "Ошибка ввода! Используем 0.0"
        0.0

// --- Основной цикл программы ---
let mutable running = true

while running do
    printfn "Выберите номер задания (1, 2, 3) или '0' для выхода:"
    let choice = Console.ReadLine()

    match choice with

    | "1" ->
        printf "Введите максимальную степень для числа 2: "
        let input = Console.ReadLine()
        match Int32.TryParse(input) with
        | true, x ->
            let y = [ for i in 0 .. x -> pown 2 i ]
            printfn "\nРезультат: %A" y

        | _ -> printfn "Ошибка: введите целое число!"

    | "2" ->
        printf "Введите натуральное число: "
        let input = Console.ReadLine()
        let success, y = Int32.TryParse(input)
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

        printfn "\n--- Результаты ---"
        printComplex "Сложение " (add c1 c2)
        printComplex "Вычитание" (sub c1 c2)
        printComplex "Умножение" (mul c1 c2)
        if c2.re = 0.0 && c2.im = 0.0 then
            printfn "Деление : Ошибка (деление на ноль)!"
        else
            printComplex "Деление  " (div c1 c2)

    | "0" -> 
        running <- false
        printfn "Выход из программы..."


    | _ -> 
        printfn "Некорректный выбор. Попробуйте еще раз."

if running = false then
    printfn "\nПрограмма завершена. Нажми Enter для закрытия окна..."
    Console.ReadLine() |> ignore
