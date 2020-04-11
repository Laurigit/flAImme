#tavotteena, että move_cycler_c antais outputtina matriisin cycler, move_diff, slot exhaust, square sortattuna move_orderin_mukaan
cppFunction('IntegerMatrix move_cycler_c(IntegerMatrix cyclers_cards_slot,  IntegerMatrix ijk, IntegerMatrix slots_squares) {
  int n = cyclers_cards_slot.nrow();
            IntegerVector landing_slot(n);
            int k = ijk.nrow();
            int played_card;
            int start_pos;
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
              played_card = cyclers_cards_slot(i, 1);
              start_pos =  cyclers_cards_slot(i, 2);
             // Rcout << start_pos << std::endl;
            //  Rcout << played_card << std::endl;
              x = -1;
              while(x < k) {
                ++x;
                matrix_start_pos = ijk(x, 0);

                if (matrix_start_pos == start_pos) {
                  matrix_card = ijk(x, 2);

                  if (matrix_card == played_card) {
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
                    // calc move diff
                       cyc_diff_slot_exh_square(i, 1) = cyc_diff_slot_exh_square(i, 2) - start_pos - played_card;
                    // save square to result matrix
                        cyc_diff_slot_exh_square(i, 4) = landing_square(i);

                  }
                }

              }
            }
            // EXHAUST CALCULATION

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


