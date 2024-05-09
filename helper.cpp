#include <cmath>
#include <algorithm>
#include <Rcpp.h>

using namespace Rcpp;

int findIndex(StringVector vec, std::string element) {
  auto it = std::find(vec.begin(), vec.end(), element);
  
  if (it != vec.end()) {
    // Return the index of the found element
    return std::distance(vec.begin(), it);
  } else {
    // Return the length of the vector if the element is not found
    return vec.size();
  }
}

// [[Rcpp::export]]
IntegerMatrix word_in_doc(std::string doc, StringVector wanted_words) {
  StringVector word_vec;
  
  std::istringstream iss(doc);
  std::string word;
  
  while (iss >> word) {
    word_vec.push_back(word);
  }
  
  // need to initialize variable for the vector length of input
  int m = static_cast<int>(word_vec.length());
  int n = static_cast<int>(wanted_words.length());
  // calculate the size and create the result matrix
  IntegerMatrix ans(1, n);
  
  // need a variable for iteration and storing index value
  int i, inx;
  
  // need a variable for storing current string (must have it because of Rcpp bug)
  // must need this for the findName function to work
  std::string temp;
  
  // Now we consider each case of the word_vec
  for (i = 0; i < n; i++) {
    temp = wanted_words[i];
    inx = findIndex(word_vec, temp);
    if (inx != m) {
      ans(0, i) = 1;
    }
  }

  // return the value
  return ans;
}

// [[Rcpp::export]]
IntegerMatrix count_window_has_word(std::string doc, StringVector wanted_words, int window) {
  StringVector word_vec;
  
  std::istringstream iss(doc);
  std::string word;
  
  while (iss >> word) {
    word_vec.push_back(word);
  }
  
  // need to initialize variable for the vector length of input
  int m = static_cast<int>(word_vec.length());
  int n = static_cast<int>(wanted_words.length());
  // calculate the size and create the result matrix
  // first calculate the number of window
  int num_window = std::max(1, m - window + 1);
  IntegerMatrix ans(num_window, n);
  
  // need a variable for iteration and storing index value
  int i, j, inx;
  
  // need a variable for storing current string (must have it because of Rcpp bug)
  // must need this for the findName function to work
  std::string temp;
  
  // need a variable for storing when each of word is last seen
  int last_seen[n];
  
  for (i = 0; i < n; i++) {
    last_seen[i] = -1;
  }
  
  // Now we consider each case of the word_vec
  
  if (m < window) {
    // 1 window only, so just need to check if a vector is in it
    for (i = 0; i < n; i++) {
      temp = wanted_words[i];
      inx = findIndex(word_vec, temp);
      if (inx != m) {
        ans(0, i) += 1;
      }
    }
  } else {
    // check for each window
    for (i = 0; i < num_window; i++) {
      if (i == 0) {
        // consider first window
        for (j = 0; j < window; j++) {
          temp = word_vec[j];
          inx = findIndex(wanted_words, temp);
          if (inx != n) {
            ans(0, inx) = 1;
            last_seen[inx] = j;
          } 
        }
      } else {
        // update first element of previous window after removed
        temp = word_vec[i-1];
        inx = findIndex(wanted_words, temp);
        if (inx != n) {
          if (i-1 == last_seen[inx]) {
            last_seen[inx] = -1;
          }
        }
        // add new word to the window
        temp = word_vec[i + window - 1];
        inx = findIndex(wanted_words, temp);
        if (inx != n) {
          last_seen[inx] = i + window - 1;
        }
        // consider all element of last_seen
        for (j = 0; j < n; j++) {
          if (last_seen[j] >= 0) {
            ans(i, j) += 1;
          }
        }
      }
    }
  }
  // return the value
  return ans;
}

// [[Rcpp::export]]
double count_matrix_to_dieng_contrib(IntegerMatrix count_mat) {
  // need to store the dimension for later use
  int m = count_mat.nrow();
  int n = count_mat.ncol();
  
  // need some variables for iteration
  int i, j;
  
  // need a vector as helper so that we can calculate column sum by using inner product, as normal loop cause error
  IntegerVector helper_vec (m,1); // sum of element in a vector v: inner_product(v, ones_vector)
  IntegerVector z (m);
  
  // First we need an array (for faster retrieve) to store the probability that a word is in a window
  int count_col[n];
  double prob_in_window[n];
  
  for (i = 0; i < n; i++) {
    z = count_mat(_, i);
    count_col[i] = std::inner_product(z.begin(), z.end(), helper_vec.begin(), 0);
    prob_in_window[i] = count_col[i] / double(m);
  }
  
  // now we make a variable for storing the answer
  double ans;
  
  // need 2 numeric vector for storing current vector that we consider
  NumericVector x(m);
  NumericVector y(m);
  
  // integer variable for storing current count , prob, and epsilon
  int count_ij;
  double p_ij, npmi, e;
  
  // calculate pairwise npmi values
  for (i = 0; i < n; i++) {
    for (j = i; j < n; j++) {
      if (i != j) {
        // get count of both 1 in both vector using dot product
        x = count_mat(_, i);
        y = count_mat(_, j);
        count_ij = std::inner_product(x.begin(), x.end(), y.begin(), 0.0);
        if (count_ij != 0) {
          p_ij = double(count_ij) / m;
          npmi = std::log2( (p_ij) / (prob_in_window[i] * prob_in_window[j]) ) / (-std::log2(p_ij));
        } else {
          npmi = -1.0;
        }
        ans += npmi;
      }
    }
  }
  return ans;
}

double cosine_similarity(NumericVector v1, NumericVector v2) {
  return std::inner_product(v1.begin(), v1.end(), v2.begin(), 0.0) / std::sqrt(std::inner_product(v1.begin(), v1.end(), v1.begin(), 0.0) * std::inner_product(v2.begin(), v2.end(), v2.begin(), 0.0));
}

// [[Rcpp::export]]
double count_matrix_to_cv_contrib(IntegerMatrix count_mat) {
  // need to store the dimension for later use
  int m = count_mat.nrow();
  int n = count_mat.ncol();
  
  // need some variables for iteration
  int i, j;
  
  // need a vector as helper so that we can calculate column sum by using inner product, as normal loop cause error
  IntegerVector helper_vec (m,1); // sum of element in a vector v: inner_product(v, ones_vector)
  IntegerVector z (m);
  
  // First we need an array (for faster retrieve) to store the probability that a word is in a window
  int count_col[n];
  double prob_in_window[n];
  
  for (i = 0; i < n; i++) {
    z = count_mat(_, i);
    count_col[i] = std::inner_product(z.begin(), z.end(), helper_vec.begin(), 0);
    prob_in_window[i] = count_col[i] / double(m);
  }
  
  // now we make a numeric matrix for storing all npmi values
  NumericMatrix npmi_mat(n, n);
  
  // need 2 numeric vector for storing current vector that we consider
  NumericVector x(m);
  NumericVector y(m);
  
  // integer variable for storing current count , prob, and epsilon
  int count_ij;
  double p_ij, npmi, e;
  
  // calculate pairwise npmi values
  for (i = 0; i < n; i++) {
    for (j = i; j < n; j++) {
      if (i != j) {
        // get count of both 1 in both vector using dot product
        x = count_mat(_, i);
        y = count_mat(_, j);
        count_ij = std::inner_product(x.begin(), x.end(), y.begin(), 0.0);
        if (count_ij != 0) {
          p_ij = double(count_ij) / m;
          npmi = std::log2( (p_ij) / (prob_in_window[i] * prob_in_window[j]) ) / (-std::log2(p_ij));
        } else {
          npmi = -1.0;
        }
        npmi_mat(i, j) = npmi;
        npmi_mat(j, i) = npmi;
      } else {
        npmi_mat(i, i) = 1;
      }
    }
  }
  
  // now we make a variable for storing the answer
  double ans;
  
  // we need a variable for storing a vector for sum of columns
  NumericVector sum_vec(n);
  for (i = 0; i < n; i++) {
    sum_vec = sum_vec + npmi_mat(i, _);
  }
  
  // now calculate cosine similarity and add to ans
  NumericVector temp;
  for (i = 0; i < n; i++) {
    temp = npmi_mat(i, _);
    ans += cosine_similarity(temp, sum_vec);
  }
  
  return ans;
}