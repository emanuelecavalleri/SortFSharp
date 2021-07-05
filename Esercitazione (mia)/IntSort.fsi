module intSort

// n = length
// k = number of bits required to store each key

// O(n^2):
val selectionSort : int list -> int list
val bubbleSort : int list -> int list
val insertionSort : int list -> int list

// O(n·log(n)):
val quickSort : int list -> int list // (average performance)
val mergeSort : int list -> int list
// type LeftistHeap<'a>
val heapSort : int list -> int list

// O(k·n), but non-comparative sorting algorithm:
val radixSort : int list -> int list // items must be positive