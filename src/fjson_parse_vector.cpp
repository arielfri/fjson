#include <Rcpp.h>
using namespace Rcpp;

//' Reads multiple rows of JSON and returns a list of data frames
//'
//' @name fjson_parse_vector
//'
//' @param strings A vector of strings.
//' @export
// [[Rcpp::export]]
List fjson_parse_vector(std::vector<std::string> strings) {
  int num_strings = strings.size();
  List out(num_strings);
  std::vector<std::string> names;
  std::vector<std::string> values;
  std::string st_names;
  std::string st_values;
  std::string st_unquoted;
  int len_substr;
  int len_substr_unquoted;
  bool value = FALSE;
  bool quoted = FALSE;
  std::string tmp_char;
  std::string tmp_char_m;
  std::string tmp_char_p;
  List row;
  
  std::string tmp_char_p2;
  std::string tmp_char_p3;
  std::string tmp_char_p4;
  std::string tmp_char_p5;
  std::string tmp_char_p6;
  std::string tmp_char_p7;
  std::string tmp_char_p8;
  std::string tmp_char_p9;
  std::string tmp_char_p10;
  std::string tmp_char_p11;
  unsigned long codepoint = 0;
  unsigned long codepoint2 = 0;

  for(int i = 0; i<num_strings; i++) {

    len_substr = strings[i].length();
    names.clear();
    values.clear();

    for(int j = 1; j<len_substr; j++) {

      tmp_char = strings[i].at(j);

      //beginning of parsing escaped characters
      
      if ((value==FALSE || quoted==TRUE) && tmp_char=="\\") {
        
        tmp_char_p = strings[i].at(j+1);
        
        if (tmp_char_p=="t") {
          tmp_char = "\t";
          j++;
        } else if (tmp_char_p=="b") {
          tmp_char = "\b";
          j++;
        } else if (tmp_char_p=="f") {
          tmp_char = "\f";
          j++;
        } else if (tmp_char_p=="n") {
          tmp_char = "\n";
          j++;
        } else if (tmp_char_p=="r") {
          tmp_char = "\r";
          j++;
        } else if (tmp_char_p=="\\") {
          tmp_char = "\\";
          j++;
        } else if (tmp_char_p=="/") {
          tmp_char = "/";
          j++;
        } else if (tmp_char_p=="\"") {
          tmp_char = "\"";
          j++;
        } else if (tmp_char_p=="u" && j+5<len_substr) {
          
          tmp_char_p2 = strings[i].at(j+2);
          tmp_char_p3 = strings[i].at(j+3);
          tmp_char_p4 = strings[i].at(j+4);
          tmp_char_p5 = strings[i].at(j+5);
          
          if ((tmp_char_p2=="0" || tmp_char_p2=="1" || tmp_char_p2=="2" || tmp_char_p2=="3" || tmp_char_p2=="4" || tmp_char_p2=="5" || tmp_char_p2=="6" || tmp_char_p2=="7" || tmp_char_p2=="8" || tmp_char_p2=="9" || tmp_char_p2=="a" || tmp_char_p2=="b" || tmp_char_p2=="c" || tmp_char_p2=="d" || tmp_char_p2=="e" || tmp_char_p2=="f" || tmp_char_p2=="A" || tmp_char_p2=="B" || tmp_char_p2=="C" || tmp_char_p2=="D" || tmp_char_p2=="E" || tmp_char_p2=="F") && 
              (tmp_char_p3=="0" || tmp_char_p3=="1" || tmp_char_p3=="2" || tmp_char_p3=="3" || tmp_char_p3=="4" || tmp_char_p3=="5" || tmp_char_p3=="6" || tmp_char_p3=="7" || tmp_char_p3=="8" || tmp_char_p3=="9" || tmp_char_p3=="a" || tmp_char_p3=="b" || tmp_char_p3=="c" || tmp_char_p3=="d" || tmp_char_p3=="e" || tmp_char_p3=="f" || tmp_char_p3=="A" || tmp_char_p3=="B" || tmp_char_p3=="C" || tmp_char_p3=="D" || tmp_char_p3=="E" || tmp_char_p3=="F") && 
              (tmp_char_p4=="0" || tmp_char_p4=="1" || tmp_char_p4=="2" || tmp_char_p4=="3" || tmp_char_p4=="4" || tmp_char_p4=="5" || tmp_char_p4=="6" || tmp_char_p4=="7" || tmp_char_p4=="8" || tmp_char_p4=="9" || tmp_char_p4=="a" || tmp_char_p4=="b" || tmp_char_p4=="c" || tmp_char_p4=="d" || tmp_char_p4=="e" || tmp_char_p4=="f" || tmp_char_p4=="A" || tmp_char_p4=="B" || tmp_char_p4=="C" || tmp_char_p4=="D" || tmp_char_p4=="E" || tmp_char_p4=="F") && 
              (tmp_char_p5=="0" || tmp_char_p5=="1" || tmp_char_p5=="2" || tmp_char_p5=="3" || tmp_char_p5=="4" || tmp_char_p5=="5" || tmp_char_p5=="6" || tmp_char_p5=="7" || tmp_char_p5=="8" || tmp_char_p5=="9" || tmp_char_p5=="a" || tmp_char_p5=="b" || tmp_char_p5=="c" || tmp_char_p5=="d" || tmp_char_p5=="e" || tmp_char_p5=="f" || tmp_char_p5=="A" || tmp_char_p5=="B" || tmp_char_p5=="C" || tmp_char_p5=="D" || tmp_char_p5=="E" || tmp_char_p5=="F")) {
            
            codepoint = std::strtoul(std::string(strings[i].substr(j+2, 4)).c_str(), nullptr, 16);
            
            if (j+11<len_substr) {
              tmp_char_p6 = strings[i].at(j+6);
              tmp_char_p7 = strings[i].at(j+7);
              if (tmp_char_p6=="\\" && tmp_char_p7=="u") {
                
                tmp_char_p8 = strings[i].at(j+8);
                tmp_char_p9 = strings[i].at(j+9);
                tmp_char_p10 = strings[i].at(j+10);
                tmp_char_p11 = strings[i].at(j+11);
                
                if ((tmp_char_p8=="0" || tmp_char_p8=="1" || tmp_char_p8=="2" || tmp_char_p8=="3" || tmp_char_p8=="4" || tmp_char_p8=="5" || tmp_char_p8=="6" || tmp_char_p8=="7" || tmp_char_p8=="8" || tmp_char_p8=="9" || tmp_char_p8=="a" || tmp_char_p8=="b" || tmp_char_p8=="c" || tmp_char_p8=="d" || tmp_char_p8=="e" || tmp_char_p8=="f" || tmp_char_p8=="A" || tmp_char_p8=="B" || tmp_char_p8=="C" || tmp_char_p8=="D" || tmp_char_p8=="E" || tmp_char_p8=="F") && 
                    (tmp_char_p9=="0" || tmp_char_p9=="1" || tmp_char_p9=="2" || tmp_char_p9=="3" || tmp_char_p9=="4" || tmp_char_p9=="5" || tmp_char_p9=="6" || tmp_char_p9=="7" || tmp_char_p9=="8" || tmp_char_p9=="9" || tmp_char_p9=="a" || tmp_char_p9=="b" || tmp_char_p9=="c" || tmp_char_p9=="d" || tmp_char_p9=="e" || tmp_char_p9=="f" || tmp_char_p9=="A" || tmp_char_p9=="B" || tmp_char_p9=="C" || tmp_char_p9=="D" || tmp_char_p9=="E" || tmp_char_p9=="F") && 
                    (tmp_char_p10=="0" || tmp_char_p10=="1" || tmp_char_p10=="2" || tmp_char_p10=="3" || tmp_char_p10=="4" || tmp_char_p10=="5" || tmp_char_p10=="6" || tmp_char_p10=="7" || tmp_char_p10=="8" || tmp_char_p10=="9" || tmp_char_p10=="a" || tmp_char_p10=="b" || tmp_char_p10=="c" || tmp_char_p10=="d" || tmp_char_p10=="e" || tmp_char_p10=="f" || tmp_char_p10=="A" || tmp_char_p10=="B" || tmp_char_p10=="C" || tmp_char_p10=="D" || tmp_char_p10=="E" || tmp_char_p10=="F") && 
                    (tmp_char_p11=="0" || tmp_char_p11=="1" || tmp_char_p11=="2" || tmp_char_p11=="3" || tmp_char_p11=="4" || tmp_char_p11=="5" || tmp_char_p11=="6" || tmp_char_p11=="7" || tmp_char_p11=="8" || tmp_char_p11=="9" || tmp_char_p11=="a" || tmp_char_p11=="b" || tmp_char_p11=="c" || tmp_char_p11=="d" || tmp_char_p11=="e" || tmp_char_p11=="f" || tmp_char_p11=="A" || tmp_char_p11=="B" || tmp_char_p11=="C" || tmp_char_p11=="D" || tmp_char_p11=="E" || tmp_char_p11=="F")) {
                  
                  codepoint2 = std::strtoul(std::string(strings[i].substr(j+8, 4)).c_str(), nullptr, 16);
                }}}
            
            // check if codepoint is a high surrogate
            if (codepoint >= 0xD800 && codepoint <= 0xDBFF) {
              // check if codepoint2 is a low surrogate
              if (codepoint2 >= 0xDC00 and codepoint2 <= 0xDFFF) {
                codepoint = (codepoint << 10) + codepoint2 - 0x35FDC00;
                j += 11;
              }} else {
                j += 5;
              }
              
              tmp_char.clear();
              if (codepoint < 0x80) {
                // 1-byte characters: 0xxxxxxx (ASCII)
                tmp_char.append(1, static_cast<typename std::string::value_type>(codepoint));
              }
              else if (codepoint <= 0x7ff) {
                // 2-byte characters: 110xxxxx 10xxxxxx
                tmp_char.append(1, static_cast<typename std::string::value_type>(0xC0 | ((codepoint >> 6) & 0x1F)));
                tmp_char.append(1, static_cast<typename std::string::value_type>(0x80 | (codepoint & 0x3F)));
              } else if (codepoint <= 0xffff) {
                // 3-byte characters: 1110xxxx 10xxxxxx 10xxxxxx
                tmp_char.append(1, static_cast<typename std::string::value_type>(0xE0 | ((codepoint >> 12) & 0x0F)));
                tmp_char.append(1, static_cast<typename std::string::value_type>(0x80 | ((codepoint >> 6) & 0x3F)));
                tmp_char.append(1, static_cast<typename std::string::value_type>(0x80 | (codepoint & 0x3F)));
              } else if (codepoint <= 0x10ffff) {
                // 4-byte characters: 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
                tmp_char.append(1, static_cast<typename std::string::value_type>(0xF0 | ((codepoint >> 18) & 0x07)));
                tmp_char.append(1, static_cast<typename std::string::value_type>(0x80 | ((codepoint >> 12) & 0x3F)));
                tmp_char.append(1, static_cast<typename std::string::value_type>(0x80 | ((codepoint >> 6) & 0x3F)));
                tmp_char.append(1, static_cast<typename std::string::value_type>(0x80 | (codepoint & 0x3F)));
              }
              continue;
              
              // end of parsing escaped characters
              
          }}} else if (value==FALSE && tmp_char==":") {

        tmp_char_m = strings[i].at(j-1);
        if (tmp_char_m=="\"") {
          len_substr_unquoted = st_names.length();
          if (len_substr_unquoted>1) {
            value = TRUE;
            st_unquoted = st_names.substr(1, len_substr_unquoted-2);
            names.push_back(st_unquoted);
            st_names.clear();
            tmp_char_p = strings[i].at(j+1);
            if(tmp_char_p=="\"") {
              quoted = TRUE;
            } else {
              quoted = FALSE;
            }
            continue;

          }}} else if (value==TRUE && (tmp_char=="," || tmp_char=="}")) {

            if (quoted==TRUE) {
              tmp_char_m = strings[i].at(j-1);
              if (tmp_char_m=="\"") {
                len_substr_unquoted = st_values.length();
                if (len_substr_unquoted>1) {
                  value = FALSE;
                  st_unquoted = st_values.substr(1, len_substr_unquoted-2);
                  values.push_back(st_unquoted);
                  st_values.clear();
                  continue;
                }}} else {
                  value = FALSE;
                  values.push_back(st_values);
                  st_values.clear();
                  continue;
                }}

          if (value==FALSE) {
            st_names += tmp_char;
          } else {
            st_values += tmp_char;
          }
    }

    row = values;
    row.names() = names;
    row.attr("class") = "data.frame";
    row.attr("row.names") = "";
    out(i) = row;
  }
  return out;
}
