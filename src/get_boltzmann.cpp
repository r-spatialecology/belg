#include <RcppArmadillo.h>
#include "utils.h"

using namespace Rcpp;

// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]
double get_boltzmann_default(arma::mat x, std::string base, bool relative){
  // float to int
  x = round(x);
  double res = 0;
  while ((x.n_rows != 1) && (x.n_cols != 1)) {
    int num_r = x.n_rows - 1;
    int num_c = x.n_cols - 1;
    arma::mat scaled(num_r, num_c);
    arma::mat result(num_r, num_c);

    for (int i = 0; i < num_r; i++) {
      for (int j = 0; j < num_c; j++) {
        arma::mat sub_x = x.submat(i, j, i + 1, j + 1);
        // conversion + search for NA values
        arma::vec sub_x_v = arma::conv_to<arma::vec>::from(sub_x.elem(find_finite(sub_x)));
        double wu;
        // Rcout << "The value of sub_x_v.n_elem : " << sub_x_v.n_elem << "\n";

        if ((sub_x_v.n_elem <= 4) && (sub_x_v.n_elem > 0)){
          if (sub_x_v.n_elem == 4){
            // if there are no NAs
            long int s = arma::sum(sub_x_v);
            int maxi = arma::max(sub_x_v);
            int mini = arma::min(sub_x_v);

            // Rcout << "The value of s : " << s << "\n";
            // Rcout << "The value of maxi : " << maxi << "\n";
            // Rcout << "The value of mini : " << mini << "\n";

            double temp = (s - maxi - mini) / 2.0;
            int x_a = floor(temp);
            int x_b = ceil(temp);
            int d_a = x_a - mini;
            int d_b = maxi - x_b;
            int d = std::min(d_a, d_b);
            // Rcout << "The value of x_a : " << x_a << "\n";
            // Rcout << "The value of x_b : " << x_b << "\n";
            // Rcout << "The value of d_a : " << d_a << "\n";
            // Rcout << "The value of d_b : " << d_b << "\n";
            // Rcout << "The value of d : " << d << "\n";
            wu = wu_calc(d, d_a, d_b, x_a, x_b);
          } else if (sub_x_v.n_elem > 1){
            // if there are between one and two NAs
            wu = count_permutations(sub_x_v);
          } else if (sub_x_v.n_elem == 1){
            // if three values are NA
            wu = 1;
          }
          // Rcout << "The value of wu : " << wu << "\n";
          scaled(i, j) = round(arma::mean(sub_x_v));
          if (base == "log"){
            result(i, j) = log(wu);
          } else if (base == "log10"){
            result(i, j) = log10(wu);
          } else if (base == "log2"){
            result(i, j) = log2(wu);
          }
        } else{
          // if all values are NA
          scaled(i, j) = NA_REAL;
          result(i, j) = 0;
        }
      }
    }
    for (int ro = 0; ro < num_r; ro++) {
      for (int co = 0; co < num_c; co++) {
        res += result(ro, co);
      }
    }
    if (relative == true){
      break;
    } else {
      x = scaled;
    }
  }
  return(res);
}

/*** R
# library(raster)
# rmin = as.matrix(readRDS("../../../belg-paper/rmin.rds"))
# get_boltzmann_default(rmin, base = "log10", relative = TRUE)
# ny = structure(c(100039552, 212529040, 285473024, 360117248, 427238912,
#                  476029376, 312039744, 61431160, 22714140, 62591, 33609, 152470624,
#                  304966432, 368911296, 384795392, 314194592, 223704000, 276490560,
#                  218708944, 74520976, 39740696, 10826316, 318377824, 367851296,
#                  429647456, 436877792, 541029888, 577235968, 571007680, 366394848,
#                  118517272, 17429328, 23439872, 312633664, 346144192, 446120704,
#                  537654720, 581675904, 609859456, 578005184, 357322272, 124735288,
#                  21573314, 14239494, 270379392, 176187264, 458922592, 583865728,
#                  622321856, 633172672, 585002752, 348349152, 97166416, 36981704,
#                  1384833, 302328640, 249477552, 455968864, 566583168, 597594048,
#                  602070464, 551481472, 330367712, 91360720, 8845392, 16480, 305356192,
#                  343877856, 438139168, 528136032, 551075008, 552829760, 504453920,
#                  305686240, 60264732, 28686, 14216, 328075552, 370821824, 433255456,
#                  495689088, 506334016, 499716064, 446452544, 260351984, 38085932,
#                  26554, 19155, 361910176, 404104960, 442673856, 481242752, 466927136,
#                  424626624, 370424320, 212225280, 51632, 31271, 30467, 395744800,
#                  437388096, 452092256, 466796448, 427520256, 367312160, 287330624,
#                  164864816, 37410, 35987, 24082, 362608032, 384622784, 380558976,
#                  364527264, 321581632, 256355824, 161278960, 46678752, 13745809,
#                  19662, 4321104), .Dim = c(x = 11L, y = 11L))
# get_boltzmann_default(ny,
#                       base = "log10", relative = TRUE)
#
# ua_pop2 = structure(c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
#                       NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
#                       NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
#                       NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
#                       NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
#                       NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
#                       NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
#                       NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
#                       NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
#                       NA, NA, NA, 27741384, 34436488, 41257648, 41532492, 67418936,
#                       68382552, 60985116, 34680164, 8165829, 1549363, 855768, NA, 71125984,
#                       71708984, 50149648, 10528977, 10528977, 52469, 42169, NA, NA,
#                       NA, NA, NA, 40809992, 30339312, 22183144, 52469, 52469, 52469,
#                       47530, NA, NA, NA, NA, NA, 51383, 51997, 52207, 52417, 52469,
#                       52469, 47620, NA, NA, NA, NA, NA, 47035, 50108, 51158, 52207,
#                       52469, 52469, 47710, NA, NA, NA, NA, NA, 6204001, 48219, 50108,
#                       51997, 52469, 52469, 47800, NA, NA, NA, NA, NA, 44794, 49164,
#                       50633, 52102, 52469, 52469, 47890, NA, NA, NA, NA, NA, 393510,
#                       409565, 414617, 419668, 420930, 420930, 384911, NA, NA, NA, NA,
#                       NA, 419307, 420930, 420930, 420930, 420930, 420930, 385632, NA,
#                       NA, NA, NA, NA, 420029, 420930, 420930, 420930, 420930, 420930,
#                       386354, NA, NA, NA, NA, NA, 420750, 420930, 420930, 420930, 420930,
#                       420930, 387076, NA, NA, NA, NA, NA, 420930, 420930, 420930, 420930,
#                       420930, 420930, 387797, NA, NA, NA, NA, NA, 56451524, 420930,
#                       420930, 420930, 420930, 420930, 388519, NA, NA, NA, NA, NA, 547210304,
#                       89190600, 420930, 420930, 420930, 420930, 389240, 145822, NA,
#                       NA, NA, NA, 182684048, 420930, 420930, 420930, 420930, 420930,
#                       389962, 266088, NA, NA, NA, NA), .Dim = c(x = 12L, y = 27L))
# get_boltzmann_default(ua_pop2, base = "log10", relative = TRUE)
*/
