
import java.util.LinkedList;
import java.util.List;
import java.util.Optional;
import java.util.function.Predicate;

public class javaExamples {

    public static void main(String[] args) {
        List<Integer> numbers = List.of(1, 2, 3, 4, 5);
        traditionalSum(numbers);
        streamsSum(numbers);

        traditionalEvenMaybeFilter(numbers);
        traditionalMaybeFilter((Integer x) -> x % 2 == 0, numbers);
        streamsEvenMaybeFilter(numbers);
        streamsMaybeFilter((Integer x) -> x % 2 == 0, numbers);
    }

    private static void traditionalSum(List<Integer> numbers) {
        int sum = 0;
        for (int number : numbers) {
            sum += number;
        }
        System.out.println("taditionalSum: " + sum);
    }

    private static void streamsSum(List<Integer> numbers) {
        int sum = numbers.stream().reduce(0, (a, b) -> a + b); // foldr (\x xs -> x + xs) 0 ≈ reduce(0, (a,b) -> a + b)
        System.out.println("streamsSum: " + sum);
    }

    // MaybeFilter mit map
    // maybeFilter soll alle Daten auf die Prädikat zutrifft in Just speichern, sonst Nothing
    // bsp: 
    // maybeFilter even [1..5] soll dann [Nothing, Just 2, Nothing, Just 4, Nothing] liefern.
    private static void traditionalEvenMaybeFilter(List<Integer> numbers) {
        List<Optional<Integer>> resultList = new LinkedList<>();
        for (int number : numbers) {
            if (isEven(number)) {
                resultList.add(Optional.of(number));
            } else {
                resultList.add(Optional.empty());
            }
        }

        System.out.println("traditionalEvenMaybeFilter: " + resultList);
    }

    private static boolean isEven(int number) {
        return (number % 2 == 0);
    }

    private static <T> void traditionalMaybeFilter(Predicate<T> p, List<T> lst) {
        List<Optional<T>> resultList = new LinkedList<>();
        for (T ele : lst) {
            if (p.test(ele)) {
                resultList.add(Optional.of(ele));
            } else {
                resultList.add(Optional.empty());
            }
        }

        System.out.println("traditionalMaybeFilter: " + resultList);
    }

    private static void streamsEvenMaybeFilter(List<Integer> numbers) {
        List<Optional<? extends Object>> resultList = numbers.stream()
                .map(x -> {
                    if (isEven(x)) {
                        return Optional.of(x);
                    } else {
                        return Optional.empty();
                    }
                })
                .toList();

        System.out.println("streamsEvenMaybeFilter: " + resultList);
    }

    private static <T> void streamsMaybeFilter(Predicate<T> p, List<T> lst) {
        List<Optional<? extends Object>> resultList = lst.stream()
                .map(x -> p.test(x) ? Optional.of(x) : Optional.empty()).toList(); 
                
                // haskell implementation: maybeFilter p xs = map(\x -> if p x then Just x else Nothing) xs

        System.out.println("streamsMaybeFilter: " + resultList);
    }
}
