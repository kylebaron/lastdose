#include <Rcpp.h>

#define isna(a) Rcpp::NumericVector::is_na(a)

class  record {
public:
  record(double time_, double amt_, int evid_, bool from_data_, bool comment_);
  bool is_dose();
  bool real_observation();
  double time;
  double amt;
  int evid;
  bool from_data;
  int pos;
  bool comment;
};

record::record(double time_, double amt_, int evid_, bool from_data_,
               bool comment_) {
  time = time_;
  amt = amt_;
  evid = evid_;
  from_data = from_data_;
  pos = -1;
  comment = comment_;
}

bool record::is_dose() {
  return (evid==1 || evid==4) && (!comment);
}

bool is_dose(const int evid) {
  return evid==1 || evid==4;
}

bool is_dose(const int evid, const bool comment) {
  return (evid==1 || evid==4) && (!comment);
}

bool record::real_observation() {
  return evid==0 && !comment;
}

typedef std::vector<record> recs;

bool Comp1(const record& a, const record& b) {
  if(a.time == b.time) {
    return a.pos < b.pos;
  }
  return a.time < b.time;
}

bool obs_before_dose(recs::iterator it, recs::iterator it_end) {
  auto it_next = std::next(it,1);
  while(it_next != it_end) {
    if(it_next->real_observation()) {
      return true;
    }
    if(it_next->is_dose()) {
      return false;
    }
    ++it_next;
  }
  return false;
}

// [[Rcpp::export]]
Rcpp::List lastdose_impl(Rcpp::NumericVector id,
                         Rcpp::NumericVector time,
                         Rcpp::NumericVector amt,
                         Rcpp::NumericVector evid,
                         Rcpp::NumericVector addl,
                         Rcpp::NumericVector ii,
                         Rcpp::NumericVector fill,
                         Rcpp::LogicalVector back_calc,
                         Rcpp::LogicalVector sort1,
                         Rcpp::LogicalVector comment,
                         Rcpp::LogicalVector include_occ) {

  amt = Rcpp::clone(amt);
  bool obs_first = sort1[0];
  bool use_fill = !back_calc[0];
  bool get_occ = include_occ[0];
  std::vector<double> idn;
  std::vector<int> idstart;
  std::vector<int> idend;
  double lastid = -1E9;
  int nrows = id.size();
  const int max_addl_pos = nrows*10;
  for(int i = 0; i < nrows; ++i) {
    if(isna(id[i]) || isna(evid[i]) || isna(addl[i]) || isna(ii[i])) {
      std::string col;
      if(isna(id[i]))   col = "ID/id";
      if(isna(evid[i])) col = "EVID/evid";
      if(isna(addl[i])) col = "ADDL/addl";
      if(isna(ii[i]))   col = "II/ii";
      throw Rcpp::exception(
          tfm::format(
            "missing values not allowed in col %s at row %i", col, (i+1)
          ).c_str(),
          false
      );
    }
    if(id[i] != lastid) {
      idn.push_back(id[i]);
      lastid = id[i];
      idstart.push_back(i);
      if(i > 0) idend.push_back(i-1);
    }
  }
  idend.push_back(id.size()-1);
  int crow = 0;
  Rcpp::NumericVector tad(id.size());  // return vector for TAD
  Rcpp::NumericVector ldos(id.size()); // return vector for LDOS
  Rcpp::NumericVector tafd(id.size()); // return vector for TAFD
  Rcpp::NumericVector occ(id.size());  // return vector for OCC
  std::vector<double> tofd;            // time of first dose
  tofd.assign(idn.size(),-1.0);
  int nid = idn.size();
  for(int i = 0; i < nid; ++i) {
    double max_time = time[idend[i]];
    recs this_id;
    this_id.reserve((idend[i] - idstart[i]));
    bool found_dose = false;
    double last_time = -1E9;
    for(int j = idstart[i]; j <= idend[i]; ++j) {
      // If time is missing
      if(isna(time[j])) {
        tad[crow] = NA_REAL;
        ldos[crow] = NA_REAL;
        ++crow;
        continue;
      }
      if(time[j] < last_time) {
        throw Rcpp::exception(
            tfm::format(
              "the data set is out of time order at row %i", (crow+1)
            ).c_str(),
            false
        );
      } else {
        last_time = time[j];
      }
      // Deal with missing dose
      bool missing_amt = Rcpp::NumericVector::is_na(amt[j]);
      if(is_dose(evid[j], comment[j])) {
        if(!found_dose) {
          found_dose = true;
          tofd[i] = time[j];
        }
        if(missing_amt) {
          throw Rcpp::exception(
              tfm::format(
                "dosing record cannot contain missing amount at row %i", (crow+1)
              ).c_str(),
              false
          );
        }
      }
      if(missing_amt) amt[j] = 0;
      // done with missing dose
      record this_rec(time[j],amt[j],evid[j],true,comment[j]);
      this_rec.pos = crow;
      this_id.push_back(this_rec);
      if((addl[j] > 0) && this_rec.is_dose()) {
        if(ii[j] <= 0.0) {
          throw Rcpp::exception(
              tfm::format(
                "ADDL doses requested, but II is not positive at row %i", (j+1)
              ).c_str(),
              false
          );
        }
        for(int k = 0; k < addl[j]; ++k) {
          record addl_rec(0.0,amt[j],evid[j],false,false);
          addl_rec.pos = obs_first ? max_addl_pos : -1;
          addl_rec.time = time[j] + ii[j]*double(k+1);
          if(addl_rec.time >= (max_time)) break;
          this_id.push_back(addl_rec);
        }
      }
      ++crow;
    }
    std::sort(this_id.begin(), this_id.end(), Comp1);
    double last_dose = 0;
    bool had_dose = false;
    bool no_dose = tofd[i] == -1;

    last_time = 0;
    for(recs::iterator it = this_id.begin(); it !=this_id.end(); ++it) {
      if(it->is_dose()) {
        had_dose = true;
        last_dose = it->amt;
        last_time = it->time;
      }
      if(it->from_data) {
        if(had_dose) {
          tad[it->pos] = it->time - last_time;
          tafd[it->pos] = it->time - tofd[i];
        } else {
          tad[it->pos] = (use_fill || no_dose) ? fill[0] : (it->time - tofd[i]);
          tafd[it->pos] = tad[it->pos];
        }
        ldos[it->pos] = last_dose;
      }
      // To resort for OCC calculation
      if(!it->from_data) {
        it->pos = -1;
      }
    }
    // Start occ calculation --------------------------------------------------
    if(get_occ) {
      int occ_n = 0;
      std::sort(this_id.begin(), this_id.end(), Comp1);
      for(auto it = this_id.begin(); it !=this_id.end(); ++it) {
        if(it->is_dose() && obs_before_dose(it, this_id.end())) {
          ++occ_n;
        }
        if(it->from_data) {
          occ[it->pos] = occ_n;
        }
      }
    }
    // End occ calculation -----------------------------------------------------
  }
  Rcpp::List ans;
  ans["tad"] = tad;
  ans["tafd"] = tafd;
  ans["ldos"] = ldos;
  if(get_occ) {
    ans["occ"] = occ;
  }
  return ans;
}
