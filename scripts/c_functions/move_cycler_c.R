cppFunction('IntegerMatrix move_cycler_c(IntegerMatrix cyclers_cards_slot,  IntegerMatrix ijk, IntegerMatrix slots_squares, IntegerMatrix slipstream_possible) {
  int n = cyclers_cards_slot.nrow();
            IntegerVector landing_slot(n);
            int k = ijk.nrow();
            IntegerVector played_card(n);
            IntegerVector start_pos(n);
            int matrix_start_pos;
            int matrix_card;
            int y = 0;
            int x = 0;

            int slots_length = slots_squares.nrow();
            IntegerVector landing_square(n);
            int landing_slot_after_blocks;

            IntegerVector occupied_squares(slots_length);
            IntegerMatrix cyc_diff_slot_exh_square(n, 5);
            //print(ijk);
            for(int i = 0; i < n; ++i) {
              played_card(i) = cyclers_cards_slot(i, 1);
              start_pos(i) =  cyclers_cards_slot(i, 2);
             // Rcout << start_pos(i) << std::endl;
            //  Rcout << played_card(i) << std::endl;
              x = -1;
              while(x < k) {
                ++x;
                matrix_start_pos = ijk(x, 0);

                if (matrix_start_pos == start_pos(i)) {
                  matrix_card = ijk(x, 2);

                  if (matrix_card == played_card(i)) {
                        //            Rcout << matrix_start_pos << std::endl;
                    //    Rcout << matrix_card << std::endl;
                    //       Rcout << "matrix_card" << std::endl;
                    // here we found new slot

                       //   Rcout << x << std::endl;
                    landing_slot(i) = ijk(x, 1);
                  // print(landing_slot);
                    y = 0;
                    while (y < slots_length) {

                      //finding game_slot

                      if (slots_squares(y, 1) == landing_slot(i)) {
                        // slot found, now find free square, col 0 is square
                        landing_square(i) = slots_squares(y, 0);

                       //  print(landing_square);
                        // minus one as vectors start from 0

                          while (occupied_squares(landing_square(i) - 1) > 0) {
                          //actual squares are less, but y is higher as the slots_squares matrix is reversed

                          --landing_square(i);

                          ++y;

                          }
                         landing_slot_after_blocks = slots_squares(y, 1);

                        occupied_squares(landing_square(i) - 1) = cyclers_cards_slot(i, 0);
                         //print(occupied_squares);
                        y = 1000000;
                        x = 1000000;
                      }
                          ++y;
                    }




                    //square has been found here, find slot again. Remember that square_slots are in reverse order
                    cyc_diff_slot_exh_square(i, 2) = landing_slot_after_blocks;
                   // print(landing_square);
                    // save cycler_id
                      cyc_diff_slot_exh_square(i, 0) = cyclers_cards_slot(i, 0);

                    // save square to result matrix
                        cyc_diff_slot_exh_square(i, 4) = landing_square(i);
                           cyc_diff_slot_exh_square(i, 1) = cyc_diff_slot_exh_square(i, 2) - start_pos(i) - played_card(i);

                  }
                }

              }
            }
            // SLIPSTREAM CACLULCATION


    //  #start looping from behind
     // if cycler found. check next 2 slots. If slipstream possible, do.
        //start over

        //loop through squares to find the order

      int cyclers;
      int other_cyclers;
      int found_slot_ahead;
      int found_two_slots_ahead;
      int slip_squares = slipstream_possible.nrow();



      int cycler_slipped_this_iteration;
          x = slip_squares + 1;

              while(x > -1) {
              // first scan slots from start to finish

                x--;
                cycler_slipped_this_iteration = 0;

                cyclers = 0;

                while(cyclers < n) {



                if (cyc_diff_slot_exh_square(cyclers, 4) == x) {
                // cycler found
                // can get slipstream?
                if (slipstream_possible(x, 2) == 1) {
                //yes, can get slipstram
                  other_cyclers = 0;

                  found_slot_ahead = 0;
                  found_two_slots_ahead = 0;
                   while(other_cyclers < n) {

                        // Rcout << cyc_diff_slot_exh_square(other_cyclers, 2)  << std::endl;
                      //    Rcout << slipstream_possible(x, 3) << std::endl;
                      if (cyc_diff_slot_exh_square(other_cyclers, 2)  == slipstream_possible(x, 3)) {
                      found_slot_ahead = 1;
                      }
                       if (cyc_diff_slot_exh_square(other_cyclers, 2) - 1  == slipstream_possible(x, 3)) {
                      found_two_slots_ahead = 1;
                      }
                   other_cyclers++;
                   }
                   //now check if we move the cycler or not
                   if (found_slot_ahead == 0 && found_two_slots_ahead == 1) {
                   //move slipstream
                     // Rcout << "victory" << std::endl;
                      cyc_diff_slot_exh_square(cyclers, 4) = slipstream_possible(x, 1);
                      cyc_diff_slot_exh_square(cyclers, 2) = slipstream_possible(x, 3);
                      cycler_slipped_this_iteration = 1;
                        // calc move diff
                       cyc_diff_slot_exh_square(cyclers, 1) = cyc_diff_slot_exh_square(cyclers, 2) - start_pos(cyclers) - played_card(cyclers);


                   }
                }

                }
               // #if we slipped cyclers, start over
                if (cycler_slipped_this_iteration == 1) {
                  cycler_slipped_this_iteration = 0;
                  x = slip_squares + 1;
                }
                  cyclers++;
                }
              }




            // EXHAUST CALCULATION.

              int zz = 0;

              // while is here as inner for crashed R
              for(int j = 0; j < n; ++j) {
                // default is that get exhaust
               cyc_diff_slot_exh_square(j, 3) = 1;
                while (zz < n) {

                    if (cyc_diff_slot_exh_square(j, 2) == (cyc_diff_slot_exh_square(zz, 2) - 1)) {

                          // someone is ahead me, no exh
                          cyc_diff_slot_exh_square(j, 3) = 0;
                    }
                zz++;
                }
            zz = 0;
            }
            return(cyc_diff_slot_exh_square);
            }')
