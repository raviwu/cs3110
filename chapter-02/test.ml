open OUnit2
open Operators
open Fun
open Date
open Fun_math

let ae exp got _test_ctxt = assert_equal exp got

let tests = [
  "Multiplier" >:: (fun _ ->
    assert_equal 420 (multiplier 42 10)
  );
  "Divider" >:: (fun _ ->
    assert_equal 1.57 (divider 3.14 2.0)
  );
  "Power" >:: (fun _ ->
    assert_equal (4.2 *. 4.2 *. 4.2 *. 4.2 *. 4.2 *. 4.2 *. 4.2) (power 4.2 7)
  );
  "Structural Equality Int Test" >:: ae true (42 = 42);
  "Structural Equality String Test" >:: ae true ("hi" = "hi");
  "If test true" >:: (fun _ ->
    assert_equal 42 (if 2 > 1 then 42 else 7)
  );
  "Double Test 01" >:: ae 6 (double 3);
  "Double Test 02" >:: ae 14 (double 7);
  "Double Test 03" >:: ae 220 (double 110);
  "Cube Test 01" >:: ae 27. (cube 3.);
  "Cube Test 02" >:: ae 8. (cube 2.);
  "Cube Test 03" >:: ae 64. (cube 4.);
  "Sign Test 01" >:: ae 1 (sign 3);
  "Sign Test 02" >:: ae 0 (sign 0);
  "Sign Test 03" >:: ae (-1) (sign (-9));
  "Circle Area Test 01" >:: ae 314. (floor((circle_area 1.) *. 100.));
  "Circle Area Test 02" >:: ae 1256. (floor((circle_area 2.) *. 100.));
  "RMS Test 01" >:: ae 353. (floor((rms 3. 4.) *. 100.));
  "RMS Test 02" >:: ae 552. (floor((rms 5. 6.) *. 100.));
  "Date Test Jan 01" >:: ae true (valid_date "Jan" 31);
  "Date Test Jan 02" >:: ae false (valid_date "Jan" 32);
  "Date Test Feb 01" >:: ae true (valid_date "Feb" 29);
  "Date Test Feb 02" >:: ae false (valid_date "Feb" 30);
  "Date Test Mar 01" >:: ae true (valid_date "Mar" 31);
  "Date Test Mar 02" >:: ae false (valid_date "Mar" 32);
  "Date Test Apr 01" >:: ae true (valid_date "Apr" 30);
  "Date Test Apr 02" >:: ae false (valid_date "Apr" 31);
  "Date Test May 01" >:: ae true (valid_date "May" 31);
  "Date Test May 02" >:: ae false (valid_date "May" 32);
  "Date Test Jun 01" >:: ae true (valid_date "Jun" 30);
  "Date Test Jun 02" >:: ae false (valid_date "Jun" 31);
  "Date Test Jul 01" >:: ae true (valid_date "Jul" 31);
  "Date Test Jul 02" >:: ae false (valid_date "Jul" 32);
  "Date Test Aug 01" >:: ae true (valid_date "Aug" 31);
  "Date Test Aug 02" >:: ae false (valid_date "Aug" 32);
  "Date Test Sep 01" >:: ae true (valid_date "Sep" 30);
  "Date Test Sep 02" >:: ae false (valid_date "Sep" 31);
  "Date Test Oct 01" >:: ae true (valid_date "Oct" 31);
  "Date Test Oct 02" >:: ae false (valid_date "Oct" 32);
  "Date Test Nov 01" >:: ae true (valid_date "Nov" 30);
  "Date Test Nov 02" >:: ae false (valid_date "Nov" 31);
  "Date Test Dec 01" >:: ae true (valid_date "Dec" 31);
  "Date Test Dec 02" >:: ae false (valid_date "Dec" 32);
  "Fibonacci Test 01" >:: ae 1 (fib 1);
  "Fibonacci Test 02" >:: ae 1 (fib 1);
  "Fibonacci Test 03" >:: ae 817770325994397771 (fib_fast 1000);
  "Average Test 01" >:: ae 1.5 (1. +/. 2.);
  "Average Test 02" >:: ae 0. (0. +/. 0.);
]

let () =
  run_test_tt_main ("Chapter 2 tests" >::: tests)
