#ifndef AZ_MATH_AZMATH_H
#define AZ_MATH_AZMATH_H

#include <cmath>
#include <memory>
#include <vector>
#include <exception>
#include <string>
#include <stack>

namespace az{
    /**π constant.*/
    const double PI=3.1415926535897932;

    /**e constant.*/
    const double E=2.7182818284590452;

    /**
    * Exception class which throws when argument is out of domain.
    */
    class OutOfDomain : public std::exception{
    public:
        /**
         * @return Information that argument is out of domain.
         */
         [[nodiscard]] const char* what() const noexcept override { return "Argument out of domain!"; }
    };

    /**
    * Function is an interface for all standard mathematics functions.
    */
    class Function{
    protected:
        /**
          * Constant value. Only used in constant function.
          */
        double c;

        /**
          * Contains other expressions necessary to calculate function's value.
          */
        std::vector<std::unique_ptr<Function>> sub_expressions_;

        /**
          * Vector of signs preceding sub_expressions.
          */
        std::vector<char> signs_;

        /**
          * Contains subexpressions which occurs after power sign. Used only in two arguments function.
          */
        std::vector<std::unique_ptr<Function>> after_power_;

        /**
          * Vector of signs preceding sub_expressions after power symbol. Used only in two arguments function.
          */
        std::vector<char> right_signs_;

        /**
          * @param x Argument for which we evaluate subexpressions.
          * @return Sum of all sub_expressions.
          */
        double eval_sub_expr(double x);

        /**
          * Used only in power function.
          * @param x Argument for which we evaluate subexpression.
          * @return Sum of all subexpressions.
          */
        double eval_right(double x);

        /**
         * Finds end of first bracket in expr. Used in making functions which use brackets. Assumes that first
         * character of expr is '('.
         * @param expr Expression from which we want to get position of the closing bracket.
         * @return Position of the closing bracket.
         */
        static size_t brackets_find_end(std::string expr);

        /**
         * Function to handle two argument functions.
         * @param i Current position in expression.
         * @param sign_pos Multi function symbol's position.
         * @param expression Expression from which we are extracting function.
         * @param container Container where functions will be stored (sub_expressions_ or after_power_)
         */
        static void add_two_arg_func(size_t &i, size_t sign_pos, std::string expression,
                              std::vector<std::unique_ptr<Function>> &container);

        /**
         * Function to finding numbers in expression.
         * @param i Expression iterator.
         * @param expr Expression where numbers are searched.
         * @param funcs Vector to which subexpressions will be added.
         * @param signs Vector to which signs will be added.
         */
        static void find_numbers(size_t &i, std::string &expr, std::vector<std::unique_ptr<Function>> &funcs,
                          std::vector<char> &signs);

        /**
         * Function to finding x in expression.
         * @param i Expression iterator.
         * @param expr Expression where numbers are searched.
         * @param funcs Vector to which subexpressions will be added.
         * @param signs Vector to which signs will be added.
         */
        static void extract_x(size_t &i, std::string &expr, std::vector<std::unique_ptr<Function>> &funcs,
                              std::vector<char> &signs);

        /**
         * Function to extracting brackets function from expression.
         * @param i Expression iterator.
         * @param expr Expression from which brackets are extracted.
         * @param funcs Vector to which subexpressions will be added.
         * @param signs Vector to which signs will be added.
         */
        static void extract_brackets(size_t &i, std::string &expr, std::vector<std::unique_ptr<Function>> &funcs,
                              std::vector<char> &signs);

        /**
         * Function to extracting sine function from expression.
         * @param i Expression iterator.
         * @param expr Expression from which sine are extracted.
         * @param funcs Vector to which subexpressions will be added.
         * @param signs Vector to which signs will be added.
         */
        template<class T>
        static void extract_3_sign(size_t &i, std::string &expr, std::vector<std::unique_ptr<Function>> &funcs,
                                   std::vector<char> &signs);

        /**
         * Function to extracting tangent function from expression.
         * @param i Expression iterator.
         * @param expr Expression from which tangent are extracted.
         * @param funcs Vector to which subexpressions will be added.
         * @param signs Vector to which signs will be added.
         */
        template<class T>
        static void extract_2_sign(size_t &i, std::string &expr, std::vector<std::unique_ptr<Function>> &funcs,
                                   std::vector<char> &signs);

        /**
         * Function to extracting arc sine function from expression.
         * @param i Expression iterator.
         * @param expr Expression from which cotangent are extracted.
         * @param funcs Vector to which subexpressions will be added.
         * @param signs Vector to which signs will be added.
         */
        template<class T>
        static void extract_4_sign(size_t &i, std::string &expr, std::vector<std::unique_ptr<Function>> &funcs,
                                   std::vector<char> &signs);

    public:
        /**
          * Virtual destructor.
          */
        virtual ~Function()=default;

        /**
          * Virtual function for returning function value.
          * @param x Argument for which we evaluate function value.
          * @return Value of certain function.
          */
        virtual double calc_value(double x) { return eval_sub_expr(x); }

        /**
         * Function used to start evaluation.
         * @param expr Expression which is turned into function.
         */
        void start(const std::string& expr);

        /**
         * Function interpreting given expression.
         * @param expression Expression which is turned into function.
         * @param sub_expr Vector where subexpressions will be placed.
         * @param signs Vector where signs will be placed.
         */
        void create_function(std::string expression, std::vector<std::unique_ptr<Function>> &sub_expr,
                             std::vector<char> &signs);
    };

    /**
      * Class representing bracket. Used only to calculate value of bracket not preceded by any function, or preceded
      * only by power operator.
      */
    class Brackets : public Function{
    public:
        /**
         * Constructor
         * @param expression Expression from which we will make function contained in brackets.
         */
        explicit Brackets(std::string expression);
        /**
           * @param x Argument for which we calculate value in brackets.
           * @return Evaluated value of subexpressions in brackets.
           */
        double calc_value(double x) override {return eval_sub_expr(x);}
    };

    /**
      * Class representing sine function. Can calculate sine value of another expression.
      */
    class Sin : public Function{
    public:
        /**
         * Constructor.
         * @param expr Expression from which function is created.
         */
        explicit Sin(const std::string& expr);
        /**
          * @param x Argument for which we evaluate function value.
          * @return Sine value of added subexpressions.
          */
        double calc_value(double x) override { return sin(eval_sub_expr(x)); }
    };

    /**
      * Class representing cosine function. Can calculate cosine value of another expression.
      */
    class Cos : public Function{
    public:
        /**
         * Constructor.
         * @param expr Expression from which function is created.
         */
        explicit Cos(const std::string& expr);
        /**
          * @param x Argument for which we evaluate function value.
          * @return Cosine value of added subexpressions.
          */
        double calc_value(double x) override { return cos(eval_sub_expr(x)); }
    };

    /**
      * Class representing tangent function. Can calculate tangent value of other expression.
      */
    class Tan : public Function{
    public:
        /**
         * Constructor.
         * @param expr Expression from which function is created.
         */
        explicit Tan(const std::string& expr);
        /**
          * @param x Argument for which we evaluate function value.
          * @return Tangent value of added subexpressions.
          */
        double calc_value(double x) override { return tan(eval_sub_expr(x)); }
    };

    /**
      * Class representing cotangent function. Can calculate cotangent value of other expression.
      */
    class Cot : public Function{
    public:
        /**
         * Constructor.
         * @param expr Expression from which function is created.
         */
        explicit Cot(const std::string& expr);

        /**
          * Throws exception if tangent of value of evaluated subexpressions is equal to 0.
          * @param x Argument for which we evaluate function value.
          * @return Cotangent value of added subexpressions.
          */
        double calc_value(double x) override;
    };

    /**
      * Class representing arc sine function. Can calculate arc sine value of expression.
      */
    class Asin : public Function{
    public:
        /**
         * Constructor.
         * @param expr Expression from which function is created.
         */
        explicit Asin(const std::string& expr);

        /**
          * Throws exception when value of evaluated subexpressions isn't in interval [-1,1].
          * @param x Argument for which we evaluate function value.
          * @return Arc sine value of added subexpressions.
          */
        double calc_value(double x) override;
    };

    /**
      * Class representing arc cosine function. Can calculate arc sine value of expression.
      */
    class Acos : public Function{
    public:
        /**
         * Constructor;
         * @param expr Expression from which function is created.
         */
        explicit Acos(const std::string& expr);

        /**
          * Throws exception when value of evaluated subexpressions isn't in interval [-1,1].
          * @param x Argument for which we evaluate function value.
          * @return Arc cosine value of added subexpressions.
          */
        double calc_value(double x) override;
    };

    /**
      * Class representing arc tangent function. Can calculate arc tangent value of expression.
      */
    class Atan : public Function{
    public:
        /**
         * Constructor.
         * @param expr Expression from which function is created.
         */
        explicit Atan(const std::string& expr);

        /**
          * @param x Argument for which we evaluate function value.
          * @return Arc tangent value of added subexpressions.
          */
        double calc_value(double x) override { return atan(eval_sub_expr(x)); };
    };

    /**
      * Class representing arc cotangent function. Can calculate arc cotangent value of expression.
      */
    class Acot : public Function{
    public:
        /**
         * Constructor.
         * @param expr Expression from which function is created.
         */
        explicit Acot(const std::string& expr);

        /**
          * @param x Argument for which we evaluate function value.
          * @return Arc cotangent of added subexpressions.
          */
        double calc_value(double x) override { return PI/2-atan(eval_sub_expr(x)); };
    };

    /**
      * Class representing all kind of power functions: exponential, polynomial atoms or others, like x^x. Can
      * calculate values of such functions.
      */
    class Power : public Function{
    public:
        /**
         * Constructor.
         * @param expr Expression from which function is created.
         * @param power_pos Position of power sign, in relation to which, left and right side is settled.
         */
        Power(const std::string& expr, size_t power_pos);
        /**
          * @param x Argument for which we evaluate function value.
          * @return Left side of power sign risen to the power of right side of the power sign.
          */
        double calc_value(double x) override { return pow(eval_sub_expr(x), eval_right(x)); }
    };

    /**
      * Class representing natural logarithm. Can calculate natural logarithm of expression.
      */
    class Ln : public Function{
    public:
        /**
         * Constructor.
         * @param expr Expression from which function is created.
         */
        explicit Ln(const std::string& expr);

        /**
          * Throws expression if value of evaluated subexpressions isn't in interval (0, +inf).
          * @param x Argument for which we evaluate function value.
          * @return Natural logarithm of added subexpressions.
          */
        double calc_value(double x) override;
    };

    /**
      * Class representing binary (base 2) logarithm. Can calculate binary logarithm of expression.
      */
    class Lg : public Function{
    public:
        /**
         * Constructor.
         * @param expr Expression from which function is created.
         */
         explicit Lg(const std::string& expr);

        /**
          * Throws expression if value of evaluated subexpression isn't in interval (0, +inf).
          * @param x Argument for which we evaluate function value.
          * @return Binary logarithm of subexpressions.
          */
        double calc_value(double x) override;
    };

    /**
      * Class representing common (base 10) logarithm. Can calculate common logarithm of expression.
      */
    class Log : public Function{
    public:
        /**
         * Constructor.
         * @param expr Expression from which function is created.
         */
        explicit Log(const std::string& expr);

        /**
          * Throws expression if value of evaluated subexpression isn't in interval (0, +inf).
          * @param x Argument for which we evaluate function value.
          * @return Common logarithm of subexpressions.
          */
        double calc_value(double x) override;
    };

    /**
     * Class representing square root of expressions.
     */
    class Sqrt : public Function{
    public:
        /**
         * Constructor.
         * @param expr Expression from which function is created.
         */
        explicit Sqrt(const std::string& expr);

        /**
         * Throws expression if value of evaluated subexpression isn't in interval [0,+inf).
         * @param x Argument for which we evaluate function value.
         * @return Square root of subexpressions.
         */
        double calc_value(double x) override;
    };

    /**
       * Class representing multiplication of two expressions.
       */
    class Multiply : public Function{
    public:
        /**
         * Constructor.
         * @param expr Expression from which function is created.
         * @param mult_pos Position of multiplication sign, in relation to which, left and right side is settled.
         */
        Multiply(const std::string& expr, size_t mult_pos);
        /**
          * @param x Argument for which we evaluate function value.
          * @return Product of left and right expression.
          */
        double calc_value(double x) override { return eval_sub_expr(x)*eval_right(x); };
    };

    /**
       * Class representing division of two expressions.
       */
    class Divide : public Function{
    public:
        /**
         * Constructor.
         * @param expr Expression from which function is created.
         * @param div_pos Position of division sign, in relation to which, left and right side is settled.
         */
        Divide(const std::string& expr, size_t div_pos);
        /**
          * Throw exception if value of right side of division mark is equal to 0.
          * @param x Argument for which we evaluate function value.
          * @return Quotient of left and right expression.
          */
        double calc_value(double x) override;
    };

    /**
      * Class representing constant value. One of base cases of expression.
      */
    class Const : public Function{
    public:
        /**
         * Constructor.
         * @param expr Expression from which function is created.
         */
        explicit Const(const std::string& expr);

        /**
         * Constructor.
         * @param x Constant value which will be assigned.
         */
        explicit Const(double x);

        /**
          * @param x Argument for which we evaluate function value.
          * @return Constant function parameter.
          */
        double calc_value(double x) override { return c; };
    };

    /**
      * Class representing x. One of base cases of expression.
      */
    class X : public Function{
        /**
          * @param x Argument we will return.
          * @return Obtained argument.
          */
        double calc_value(double x) override { return x; }
    };
}

#endif //AZ_MATH_AZMATH_H
