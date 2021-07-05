#r "IntSorti.dll"
open intSort

#r "FsCheck"
open FsCheck

let testSort xs =
  [
    selectionSort xs = List.sort xs |@ "SelectionSort"
    bubbleSort xs = List.sort xs |@ "BubbleSort"
    insertionSort xs = List.sort xs |@ "InsertionSort"
    quickSort xs = List.sort xs |@ "QuickSort"
    mergeSort xs = List.sort xs |@ "MergeSort"
    heapSort xs = List.sort xs |@ "HeapSort"
    radixSort (List.map (fun x -> abs x) xs) =
      List.sort (List.map (fun x -> abs x) xs) |@ "RadixSort" // items must be positive
  ]

// scan n from script.sh

let n =
  try 
    System.Console.ReadLine() |> int
  with
    | :? System.FormatException ->
      -1

if n < 0 then failwithf "Wrong input!"

let genRandomNumbers n = List.init n (fun _ -> System.Random().Next ())

printfn "List length = n = %i; number of bits required to store each key = k = 32.\n" n

// testing sorting algorithms' fairness

printf "Checking algorithms' fairness: "
do Check.Quick testSort
printfn ""

let shuffledList = genRandomNumbers (n)
let orderedList = List.sort shuffledList
let revList = List.rev orderedList

// testing sorting algorithms' exec time

// List.sort time
printfn "List.sorting ordered list:"
#time;; List.sort orderedList;; #time;;
printfn "List.sorting reversed list:"
#time;; List.sort revList;; #time;;
printfn "List.sorting shuffled list:"
#time;; List.sort shuffledList;; #time;;
printfn ""

printfn "*** O(n^2) sorting algorithms. ***\n"

// SelectionSort time
printfn "SelectionSorting ordered list:"
#time;; selectionSort orderedList;; #time;;
printfn "SelectionSorting reversed list:"
#time;; selectionSort revList;; #time;;
printfn "SelectionSorting shuffled list:"
#time;; selectionSort shuffledList;; #time;;
printfn ""

// BubbleSort time
printfn "BubbleSorting ordered list:"
#time;; bubbleSort orderedList;; #time;;
printfn "BubbleSorting reversed list:"
#time;; bubbleSort revList;; #time;;
printfn "BubbleSorting shuffled list:"
#time;; bubbleSort shuffledList;; #time;;
printfn ""

// InsertionSort time
printfn "InsertionSorting ordered list:"
#time;; insertionSort orderedList;; #time;;
printfn "InsertionSorting reversed list:"
#time;; insertionSort revList;; #time;;
printfn "InsertionSorting shuffled list:"
#time;; insertionSort shuffledList;; #time;;
printfn ""

printfn "*** O(n·log(n)) sorting algorithms. ***\n"

// QuickSort time
printfn "QuickSorting ordered list:"
#time;; quickSort orderedList;; #time;;
printfn "QuickSorting reversed list:"
#time;; quickSort revList;; #time;;
printfn "QuickSorting reversed list:"
#time;; quickSort shuffledList;; #time;;
printfn ""

// MergeSort time
printfn "MergeSorting ordered list:"
#time;; mergeSort orderedList ;;#time;;
printfn "MergeSorting reversed list:"
#time;; mergeSort revList;; #time;;
printfn "MergeSorting reversed list:"
#time;; mergeSort shuffledList;; #time;;
printfn ""

// HeapSort time
printfn "HeapSorting ordered list:"
#time;; heapSort orderedList;; #time;;
printfn "HeapSorting reversed list:"
#time;; heapSort revList;; #time;;
printfn "HeapSorting reversed list:"
#time;; heapSort shuffledList;; #time;;
printfn ""

printfn "*** O(k·n) (non-comparative) sorting algorithm. ***\n"

// RadixSort time
printfn "RadixSorting ordered list:"
#time;; radixSort orderedList;; #time;;
printfn "RadixSorting reversed list:"
#time;; radixSort revList;; #time;;
printfn "RadixSorting shuffled list:"
#time;; radixSort shuffledList;; #time