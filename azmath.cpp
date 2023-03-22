#include "azmath.h"

#include <utility>
#include <iostream>

void az::Function::parse(const std::string& expr) {
    if (!check_parentheses(expr)) {
        std::cout << "Bad par";
    } else {

        for (auto &i: expr) {
            std::cout << i << std::endl;
        }
    }
}

double az::Function::eval_sub_expr(double x){
    double sum=0.0;

    for(size_t i=0; i<sub_expressions_.size(); i++){
        if(signs_[i]==1) { sum += sub_expressions_[i]->calc_value(x); }
        else { sum -=sub_expressions_[i]->calc_value(x); }
    }

    return sum;
}

double az::Function::eval_right(double x) {
    double sum=0.0;

    for(size_t i=0; i<after_power_.size(); i++){
        if(right_signs_[i]==1) { sum += after_power_[i]->calc_value(x); }
        else { sum -=after_power_[i]->calc_value(x); }
    }

    return sum;
}

void az::Function::start(const std::string &expr){
    sub_expressions_.clear();
    signs_.clear();
    after_power_.clear();
    right_signs_.clear();
    create_function(expr,sub_expressions_,signs_);
}

void az::Function::create_function(std::string expression, std::vector<std::unique_ptr<Function>> &sub_expr,
                                   std::vector<char> &signs){
    size_t i=0;
    while(expression[i]!=0){
        switch(expression[i]){
            case '(': { extract_brackets(i,expression,sub_expr,signs); }
                break;
            case '-':
                signs_.push_back(-1);
                i++;
                break;
            case '+':
                signs_.push_back(1);
                i++;
                break;
            case 'x': { extract_x(i,expression,sub_expr,signs); }
                break;
            case 's': {
                if(expression[i+1]=='i') { extract_3_sign<az::Sin>(i, expression, sub_expr, signs); }
                else if(expression[i+1]=='q') { extract_4_sign<az::Sqrt>(i,expression,sub_expr,signs); }
            }
                break;
            case 'c':{
                if(expression[i+1]=='t') { extract_3_sign<az::Cot>(i,expression,sub_expr,signs); }
                else if(expression[i+1]=='o') { extract_3_sign<az::Cos>(i, expression, sub_expr, signs); }
            }
                break;
            case 't': {
                extract_2_sign<az::Tan>(i, expression, sub_expr, signs); }
                break;
            case 'a':{
                if(expression[i+1]=='s') { extract_4_sign<az::Asin>(i, expression, sub_expr, signs); }
                else if(expression[i+1]=='t') { extract_3_sign<az::Atan>(i,expression,sub_expr,signs); }
                else if(expression[i+1]=='c'){
                    if(expression[i+2]=='o') { extract_4_sign<az::Acos>(i,expression,sub_expr,signs); }
                    else if(expression[i+2]=='t') { extract_4_sign<az::Acot>(i,expression,sub_expr,signs); }
                }
            }
                break;
            case 'l':{
                if(expression[i+1]=='n') { extract_2_sign<az::Ln>(i,expression,sub_expr,signs); }
                else if(expression[i+1]=='g') { extract_2_sign<az::Lg>(i,expression, sub_expr, signs); }
                else if(expression[i+1]=='o') { extract_3_sign<az::Log>(i,expression,sub_expr,signs); }
            }
                break;
            case 'e':{
                if(i==0) { signs.push_back(1); }
                //if after x isn't multiplication or division symbol, simply add x function to sub_expr
                if(expression[i+1]!='*' && expression[i+1]!='/' && expression[i+1]!='^'){
                    sub_expr.push_back(std::make_unique<Const>(E));
                    i++;
                }else{ //otherwise find end of multiplication/division subexpression and add it to function
                    add_two_arg_func(i,1,expression,sub_expr);
                }
            }
                break;
            case 'p':{
                if(i==0) { signs.push_back(1); }
                //if after x isn't multiplication or division symbol, simply add x function to sub_expr
                if(expression[i+2]!='*' && expression[i+2]!='/' && expression[i+2]!='^'){
                    sub_expr.push_back(std::make_unique<Const>(PI));
                    i+=2;
                }else{ //otherwise find end of multiplication/division subexpression and add it to function
                    add_two_arg_func(i,2,expression,sub_expr);
                }
            }
                break;
            default:{ find_numbers(i,expression,sub_expr,signs); }
        }
    }
}

void az::Function::add_two_arg_func(size_t &i, size_t sign_pos, std::string expression,
                                    std::vector<std::unique_ptr<Function>> &container){
    size_t start=i;
    char sign=expression[i+sign_pos];
    i+=sign_pos;
    while(expression[i]!='+' && expression[i]!='-' && expression[i]!=0){
        if(expression[i]=='(') { i+=brackets_find_end(expression.substr(i)); }
        else { i++; }
    }
    size_t block=i-start;
    std::string expr=expression.substr(start,block);
    if(sign=='*') { container.push_back(std::make_unique<Multiply>(expr,sign_pos)); }
    else if(sign=='/') { container.push_back(std::make_unique<Divide>(expr,sign_pos)); }
    else { container.push_back(std::make_unique<Power>(expr,sign_pos)); }
}

size_t az::Function::brackets_find_end(std::string expr) {
    std::stack<char> brackets;
    size_t i=0;
    brackets.push(expr[i]);
    i++;

    while(!brackets.empty()){
        if(expr[i]=='('){ brackets.push(expr[i]); }
        if(expr[i]==')'){ brackets.pop(); }
        i++;
    }

    return i;
}

void az::Function::find_numbers(size_t &i, std::string &expr, std::vector<std::unique_ptr<Function>> &funcs,
                                std::vector<char> &signs){
    if(expr[i]>=48 && expr[i]<=57){
        if(i==0) { signs.push_back(1); }
        std::string number;
        size_t start=i;
        size_t sign_pos=0;
        char atom=expr[i];
        while(atom!='+' && atom!='-' && atom!=0){
            number+=atom;
            if((atom == '*' || atom=='/' || atom=='^') && sign_pos==0) { sign_pos=i-start; }
            if(atom=='('){
                i+=brackets_find_end(expr.substr(i));
                atom=expr[i];
            }else{
                atom = expr[++i];
            }
        }
        if(sign_pos) { add_two_arg_func(start,sign_pos,expr,funcs); }
        else { funcs.push_back(std::make_unique<Const>(number)); }
    }
}

void az::Function::extract_x(size_t &i, std::string &expr, std::vector<std::unique_ptr<Function>> &funcs,
                             std::vector<char> &signs){
    if(i==0){ signs.push_back(1); }
    //if after x isn't multiplication or division symbol, simply add x function to sub_expr
    if(expr[i+1]!='*' && expr[i+1]!='/' && expr[i+1]!='^'){
        funcs.push_back(std::make_unique<X>());
        i++;
    }else{ //otherwise find end of multiplication/division subexpression and add it to function
        add_two_arg_func(i,1,expr,funcs);
    }
}

void az::Function::extract_brackets(size_t &i, std::string &expr, std::vector<std::unique_ptr<Function>> &funcs,
                                    std::vector<char> &signs) {
    if(i==0) { signs.push_back(1); }
    size_t end=brackets_find_end(expr.substr(i));
    if(expr[i+end]!='*' && expr[i+end]!='/' && expr[i+end]!='^'){
        size_t block = end - 2;
        std::string sub_expr=expr.substr(i+1,block);
        funcs.push_back(std::make_unique<Brackets>(sub_expr));
        i += end;
    }else{
        add_two_arg_func(i,end,expr,funcs);
    }
}

std::istream &az::operator>>(std::istream &input, az::Function &f) {
    std::string s;
    input >> s;
    f.start(s);

    return input;
}

bool az::Function::check_parentheses(const std::string &expr) {
    std::stack<char> parentheses;
    for (const char& c:expr) {
        if (c == '(') {
            parentheses.push(c);
        }
        if (c == ')') {
            if (parentheses.top() == ')') {
                parentheses.pop();
            } else {
                return false;
            }
        }
    }

    return parentheses.empty();
}

template<class T>
void az::Function::extract_3_sign(size_t &i, std::string &expr, std::vector<std::unique_ptr<Function>> &funcs,
                                  std::vector<char> &signs){
    if(i==0) { signs.push_back(1); }
    size_t end=brackets_find_end(expr.substr(i + 3));
    funcs.push_back(std::make_unique<T>(expr.substr(i, end + 3)));
    i += end + 3;
}

template<class T>
void az::Function::extract_2_sign(size_t &i, std::string &expr, std::vector<std::unique_ptr<Function>> &funcs,
                                  std::vector<char> &signs){
    if(i==0) { signs.push_back(1); }
    size_t end=brackets_find_end(expr.substr(i + 2));
    funcs.push_back(std::make_unique<T>(expr.substr(i, end + 2)));
    i += end + 2;
}

template<class T>
void az::Function::extract_4_sign(size_t &i, std::string &expr, std::vector<std::unique_ptr<Function>> &funcs,
                                  std::vector<char> &signs){
    if(i==0) { signs.push_back(1); }
    size_t end=brackets_find_end(expr.substr(i + 4));
    funcs.push_back(std::make_unique<T>(expr.substr(i, end + 4)));
    i += end + 4;
}

double az::Cot::calc_value(double x){
    if (tan(eval_sub_expr(x))!=0) { return 1/tan(eval_sub_expr(x)); }

    throw OutOfDomain();
}

double az::Asin::calc_value(double x){
    if(eval_sub_expr(x)>1 || eval_sub_expr(x)<-1) { throw OutOfDomain(); }

    return asin(eval_sub_expr(x));
}

double az::Acos::calc_value(double x){
    if(eval_sub_expr(x)>1 || eval_sub_expr(x)<-1) { throw OutOfDomain(); }

    return acos(eval_sub_expr(x));
}

double az::Ln::calc_value(double x){
    if(eval_sub_expr(x)<=0) { throw OutOfDomain(); }

    return log(eval_sub_expr(x));
}

double az::Lg::calc_value(double x){
    if(eval_sub_expr(x)<=0) { throw OutOfDomain(); }

    return log2(eval_sub_expr(x));
}

double az::Log::calc_value(double x){
    if(eval_sub_expr(x)<=0) { throw OutOfDomain(); }

    return log10(eval_sub_expr(x));
}

double az::Sqrt::calc_value(double x){
    if(eval_sub_expr(x)<0) { throw OutOfDomain(); }

    return sqrt(eval_sub_expr(x));
}

double az::Divide::calc_value(double x){
    if(eval_right(x)==0) { throw OutOfDomain(); }

    return eval_sub_expr(x)/eval_right(x);
}

az::Divide::Divide(const std::string &expr, size_t div_pos){
    create_function(expr.substr(0,div_pos),sub_expressions_,signs_);
    create_function(expr.substr(div_pos+1),after_power_,right_signs_);
}

az::Brackets::Brackets(std::string expression){
    create_function(std::move(expression),sub_expressions_,signs_);
}

az::Multiply::Multiply(const std::string& expr, size_t mult_pos){
    create_function(expr.substr(0,mult_pos),sub_expressions_,signs_);
    create_function(expr.substr(mult_pos+1),after_power_,right_signs_);
}

az::Sin::Sin(const std::string& expr){
    create_function(expr.substr(3),sub_expressions_,signs_);
}

az::Cos::Cos(const std::string& expr){
    create_function(expr.substr(3),sub_expressions_,signs_);
}

az::Tan::Tan(const std::string &expr){
    create_function(expr.substr(2),sub_expressions_,signs_);
}

az::Cot::Cot(const std::string &expr){
    create_function(expr.substr(3),sub_expressions_,signs_);
}

az::Asin::Asin(const std::string &expr){
    create_function(expr.substr(4),sub_expressions_,signs_);
}

az::Acos::Acos(const std::string &expr){
    create_function(expr.substr(4),sub_expressions_,signs_);
}

az::Atan::Atan(const std::string &expr){
    create_function(expr.substr(3),sub_expressions_,signs_);
}

az::Acot::Acot(const std::string &expr){
    create_function(expr.substr(4),sub_expressions_,signs_);
}

az::Ln::Ln(const std::string &expr){
    create_function(expr.substr(2),sub_expressions_,signs_);
}

az::Lg::Lg(const std::string &expr){
    create_function(expr.substr(2),sub_expressions_,signs_);
}

az::Log::Log(const std::string &expr){
    create_function(expr.substr(3),sub_expressions_,signs_);
}

az::Sqrt::Sqrt(const std::string &expr){
    create_function(expr.substr(4),sub_expressions_,signs_);
}

az::Power::Power(const std::string &expr, size_t power_pos){
    create_function(expr.substr(0, power_pos),sub_expressions_,signs_);
    create_function(expr.substr(power_pos + 1),after_power_,right_signs_);
}

az::Const::Const(const std::string& expr){
    c=std::stod(expr);
}

az::Const::Const(double x){
    c=x;
}
