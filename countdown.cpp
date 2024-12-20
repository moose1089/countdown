#include <iostream>
#include <cassert>
#include <string>
#include <cstring>
#include <cctype>
#include <stack>
#include <list>

using namespace std;

string postfix_to_infix(string pf, int number[6]) {
  if (pf == "none")
    return pf;
  
  stack<string> s;
  
  for (int i=0; pf[i]; i++) {
    if (isdigit(pf[i])) {
      int idx = pf[i] - '1';
      assert(idx >=0 && idx < 6);
      char num[100];
      sprintf(num, "%d", number[idx]);
      s.push(num);
    } else {
      string arg1 = s.top();
      s.pop();
      string arg2 = s.top();
      s.pop();
      s.push("(" + arg2 + pf[i] + arg1 + ")");      
    }
  }
  return s.top();
}
  
bool found_in(const char *str, int limit, char target) {
  for (int n=0; n<limit && str[n]; n++)
    if (str[n] == target)
      return true;
  return false;
}

int evaluate(const char *str, int numbers[6], bool &error_flag) {

  error_flag = false;
  
  stack<int> s;
  
  for (int n = 0; str[n] ; n++) {
    if (isdigit(str[n])) {
      int index = str[n] - '1';
      assert(index >= 0 && index <6);
      s.push(numbers[index]);
      continue;
    }

    if (s.empty()) {
      error_flag = true;
      return 0;
    }
    int two = s.top();
    s.pop();
    if (s.empty()) {
      error_flag = true;
      return 0;
    }
    int one = s.top();
    s.pop();
    switch(str[n]) {
    case '+':
      s.push(one+two);
      break;
    case '-':
      s.push(one-two);
      break;
    case '*':
      s.push(one*two);
      break;
    case '/':
      if (two == 0 || one % two) {
	error_flag = true;
	s.push(0);
      } else {
	s.push(one/two);
      }
      break;
    default:
      error_flag = true;
    }
  }

  return error_flag ? 0 : s.top();
}

void gen_instances(const char *shape, char *concrete, list<string> &collection, int index = 0) {
  const char op[] = {'+','-','*','/'};
  
  if (!shape[index]) {
    concrete[index] = '\0';
    collection.push_back(concrete);// << endl;
    return;
  }

  switch (shape[index]) {
  case 'O':
    for (int n=0; n<4; n++) {
      concrete[index] = op[n];
      gen_instances(shape, concrete, collection, index+1);
    }
    break;
  case 'L':
    for (int n=0; n<6; n++) {
      if (found_in(concrete, index, n+'1'))
	continue;
      concrete[index] = n + '1';
      gen_instances(shape, concrete, collection, index+1);
    }
    break;
    default: cerr << "error" << endl; exit(1);
  }
}


list<string> get_instances_list(const char *shape) {
  list<string> collection;

  char concrete[1000];
  gen_instances(shape, concrete, collection);
  
  return collection;
}


int gen_shape(const char *start, int unused, list<string> &candidates, int index = 0) {
  //  for each L in start, either leave it or replace it with OLL

  if (!index) {
    candidates.push_back(start);
    //  cout << "start = " << start << " unused = " << unused << endl;
  }

  if (!start[index])
    return candidates.size();

  if (start[index] == 'O') {
    return gen_shape(start, unused, candidates, index + 1);
  }

  int result;
  if (start[index] == 'L') {
    if (unused > 1) {
      char *next = new char[strlen(start) + 3];
      int n,p;
      for (n=0,p=0; n<index; n++,p++)
	next[n] = start[n];
      next[n++] = 'L';
      next[n++] = 'L';
      next[n++] = 'O';
      p++;
      for (; start[p]; p++, n++) 
	next[n] = start[p];
      next[n++] = '\0';	
      result = gen_shape(next, unused - 1, candidates, 0) + 1;
    }
    result= gen_shape(start, unused, candidates, index + 1);
  }
  return candidates.size();
}

list<string> gen_shape_list(const char *start, int size) {
  list<string> candidates;
  gen_shape(start, size, candidates);
  return candidates;
}

string find_solution(int target, int numbers[6], int &achieved) {
  
  string best_shape ="none";
  int best_score = 1000;

  for (int target_length = 1; target_length <=6; target_length++) {
  list<string> shapes = gen_shape_list("L", target_length);
  for (string s: shapes) {
    //    cout << s << endl;
    list<string> collection = get_instances_list(s.c_str());
    for (string c : collection) {
      bool error_flag = false;
      int score = evaluate(c.c_str(), numbers, error_flag);
      if (!error_flag && abs(score - target) <= best_score) {
	best_shape = c;
	best_score = abs(score - target);
	achieved = score;
	if (best_score == 0) {
	  cout << "ideal " << best_shape << endl;
	  return postfix_to_infix(best_shape, numbers);
	}
      }
    }    
  }
  }
  cout << best_shape << endl;
  return postfix_to_infix(best_shape, numbers);
}


int main() {

  bool error_flag = false;

  int achieved;

  int numbers[6] = { 500, 100, 5, 3, 2, 1 };
  // cout << "12/=" << evaluate("12/", numbers, error_flag) << endl;
  // cout << (!error_flag ? "no " : "") << "error detected" << endl;
  // cout << "34/=" << evaluate("34/", numbers, error_flag) << endl;
  // cout << (!error_flag ? "no " : "") << "error detected" << endl;
  // cout << "34/=" << evaluate("34/", numbers, error_flag) << endl;
  // cout << (!error_flag ? "no " : "") << "error detected" << endl;

  cout << find_solution(676, numbers, achieved) << endl;
  cout << "achieved = " << achieved << endl;
  
  return 0;
}
