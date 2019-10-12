abstract type AbstractExpression end 

# A constant can either be a single letter variable or a floating point number
struct Constant{T <: Union{String,Float32}} <: AbstractExpression
    val::T 

    Constant(x::Symbol) = new{String}(string(x))
    Constant(x::Real) = new{Float32}(x)
end

con = Constant

Base.show(io::IO, k::Constant) = print(io, k.val)

deriv(k::Constant) = con(0) # The derivative of any constant is just the constant 0

struct Variable <: AbstractExpression
    var::String 

    Variable(sym::Symbol) = new(string(sym))
end

var = Variable

Base.show(io::IO, v::Variable) = print(io, v.var)

deriv(v::Variable) = con(1)


abstract type AbstractBinaryOperator <: AbstractExpression end 

macro binary_operator(name, symbol)
    struct_def = quote 
        struct $name{L <: AbstractExpression,R <: AbstractExpression} <: AbstractBinaryOperator
            left::L 
            right::R 
        end 
    end

    # Returns string representation of the operator symbol when passed a variable of type name 
    symbol_function = :(sym(_x::$name) = $(string(symbol)))

    quote 
        $(struct_def)

        $(esc(symbol_function))

        # Add method for symbol that accepts 2 expression and yields an instance of the struct 
        $(Meta.parse("Base.:$symbol(left::AbstractExpression, right::AbstractExpression) = $name(left, right)"))

        # Constant Folding for the binary operator
        $(Meta.parse("Base.:$symbol(left::Constant{Float32}, right::Constant{Float32}) = con(left.val $symbol right.val)"))
    end
end

@binary_operator(Add, +)
@binary_operator(Subtract, -)
@binary_operator(Multiply, *)
@binary_operator(Divide, /)
@binary_operator(Power, ^)

Base.:^(left::Symbol, right::Real) = Power(Variable(left), con(right)) # Matches :x^2
Base.:^(left::Constant{T}, right::Real) where T = Power(left, con(right)) # Matches con(2)^3 and con(:x)^3

# Pruning for any operator that encounters a 0
Base.:*(left::Constant{Float32}, right::AbstractExpression) = left.val == 0 ? con(0) : Multiply(left, right)
Base.:*(left::AbstractExpression, right::Constant{Float32}) = right.val == 0 ? con(0) : Multiply(left, right)
Base.:/(left::Constant{Float32}, right::AbstractExpression) = left.val == 0 ? con(0) : Divide(left, right)
Base.:+(left::Constant{Float32}, right::AbstractExpression) = left.val == 0 ? right : Add(left, right)
Base.:+(left::AbstractExpression, right::Constant{Float32}) = right.val == 0 ? left : Add(left, right)
Base.:-(left::AbstractExpression, right::Constant{Float32}) = right.val == 0 ? left : Subtract(left, right)
Base.:^(left::AbstractExpression, right::Constant{Float32}) = right.val == 0 ? con(1) : Power(left, right)

function Base.show(io::IO, t::Power{T,Constant{Float32}}) where T <: Union{Variable,Constant{String}}
    print(io, t.left)
    if t.right.val != 1.0 
        print(io, "^", t.right)
    end
end
Base.show(io::IO, t::AbstractBinaryOperator) = print(io, "(", t.left, sym(t), t.right, ")")

deriv(x::Add) = deriv(x.left) + deriv(x.right)
deriv(x::Subtract) = deriv(x.left) - deriv(x.right)
deriv(x::Multiply) = deriv(x.left) * x.right + x.left * deriv(x.right)
deriv(x::Divide) = (deriv(x.left) * x.right - x.left * deriv(x.right)) / (x.right * x.right) 

deriv(x::Power{Constant{K},T}) where {K,T} = con(0)
deriv(x::Power{Variable,Constant{T}}) where T = x.right * x.left^(x.right - con(1))



abstract type AbstractFunction <: AbstractExpression end

macro unary_function(name)
    quote 
        struct $name{T <: AbstractExpression} <: AbstractFunction 
            arg::T 
        end 

        $(esc(:sym))(_x::$name) = $(string(name))
    end
end

@unary_function(Sin)
@unary_function(Cos)

Base.show(io::IO, t::AbstractFunction) = print(io, sym(t), "(", t.arg, ")")

deriv(x::Sin) = deriv(x.arg) * Cos(x.arg)
deriv(x::Cos) = deriv(x.arg) * (con(-1) * Sin(x.arg))


# Examples Code 

const general_quadratic = con(:a) * :x^2 + con(:b) * var(:x) + con(:c)
const generic_trig = con(:a) * Sin(var(:x)) + con(:b) * Cos(var(:x))
const big_polynomial = :x^8 + con(:a) * Sin(:x^2 + Cos(:x^3)) + :x^2 / (:x^4 + con(1))

println("Derivative of ", general_quadratic, " is ", deriv(general_quadratic))
println("Derivative of ", generic_trig, " is ", deriv(generic_trig))

@time deriv(:x^2)
@time deriv(:x^2 + :x^4)