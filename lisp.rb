def debug(*args)
  #print(*args, "\n")
end

def info(*args)
  print(*args, "\n")
end

def is_ws(c)
  return c =~ /\s/
end

def is_nl(c)
  return (c == "\n" or c == "\r")
end

def is_comment(c)
  # let's allow # and ; both for now but # breaks lisp syntax in editors
  return (c == ';' or c == '#')
end

def is_sexp(c)
  return c == '('
end

def is_end(c)
  return c == ')'
end

def is_str(c)
  return (c == '"' or c == "'")
end

def is_num(token)
  return token =~ /^[0-9]/
end

def parse_num(token)
  if token.include? '.'
    # float
    # .to_f(0) defaults to 0 if garbage
    return Float(token)
  else
    # integer (whatever ruby supports)
    # .to_i(0) defaults to 0 if garbage
    return Integer(token, 0)
  end
end

def is_eof(code, i)
  if code.length > i
    return false
  elsif code.length == i
    return true
  else
    raise 'BUG: already reading past end!'
  end
end

class UnexpectedEnd < Exception
end

def next_char(code, i)
  if is_eof(code, i)
    raise UnexpectedEnd.new('unexpected end')
  else
    return i+1
  end
end

def skip_comment(code, i)
  #debug "skip_comment at #{i} {#{code.slice(i,10)]}}"
  raise "not comment: #{code.slice(i,10)}" if not is_comment(code[i])
  # consume #
  i = next_char(code, i)

  while not is_eof(code, i) and not is_nl(code[i])
    i = next_char(code, i)
  end
  return i
end

def skip_ws(code, i)
  while not is_eof(code, i) and is_ws(code[i])
    i = next_char(code, i)
  end
  return i
end

class StringType < String
end

def read_str(code, i)
  #debug "read_str #{i} {#{code.slice(i,10)}}"
  raise "not str: #{code.slice(i,10)}" if not is_str(code[i])
  # ' or "
  quote = code[i]
  # consume quote
  i = next_char(code, i)
  str = StringType.new
  loop do
    raise "unexpected end of string" if is_eof(code, i)
    c = code[i]
    if c == '\\'
      # consume quote char
      i = next_char(code, i)
      case code[i]
      when 'n'
        str << "\n"
      when 'r'
        str << "\r"
      when 't'
        str << "\t"
      when '0'
        str << "\0"
      else str << code[i]
      end
      # consume quoted char
      i = next_char(code, i)
      next
    elsif c != quote
      # normal char
      str << code[i]
      # consume char
      i = next_char(code, i)
      next
    else
      # end quote
      break
    end
  end

  # consume quote
  i = next_char(code, i)
  debug "read_str=#{str}"
  return i, str
end

def seek_char(code, i)
  while not is_eof(code,i)
    # skip whitespace
    i = skip_ws(code, i)

    # skip comment
    if is_eof(code, i)
      return i
    elsif is_comment(code[i])
      i = skip_comment(code, i)
      next
    else
      # finally an important char
      return i
    end
  end
end

class Token < String
end

def read_token(code, i)
  #debug "read_token at #{i} {#{code.slice(i,10)}}"
  i = seek_char(code, i)

  token = Token.new
  c = code[i]
  if is_sexp(c)
    i, sexp = read_sexp(code, i)
    return i, sexp
  elsif is_str(c)
    i, str = read_str(code, i)
    return i, str
  else
    # read until whitespace, comment or another sexp starts
    while not is_eof(code, i) and not is_ws(c) and not is_comment(c) and
         not is_end(c)
      token << c
      i = next_char(code, i)
      # this will try to fetch out of boundaries
      c = code[i]
    end
  end

  if is_num(token)
    # if we don't do this we can reassign numbers
    num = parse_num(token)
    return i, num
  end

  # no more tokens
  token = nil if token.length == 0
  debug "read_token=#{token}"
  return i, token
end

class Sexp0 < Array
  def car
    return self[0]
  end

  def car=(value)
    self[0] = value
  end

  # this is ugly because Sexp is Array
  def cdr
    return self.class.new(self[1..-1])
  end

  # this obviously has different semantics than conses when
  # destructively modifying
  def cdr=(value)
    car = self[0]
    self.clear
    self[1] = value
  end

  def string
    return self.to_s
  end
end

class Cons
  @@count = 0
  attr_accessor :car
  attr_accessor :cdr

  def initialize(items=nil)
    @car = nil
    @cdr = nil
    @@count += 1
  end

  def Cons.count
    return @@count
  end
end

# FIXME just approximation, would need to be reworked but not sure how
#       complicated that would be - need to stop using any Enumerable and
#       use only compatible functions everywhere (NIL can probably stay nil)
# TODO nil should be same as empty list but this is (nil . nil) for empty list
#      no difference between (nil) and ()
#      would need to be reimplemented with functions: car, cdr, last, length
class Sexp1 < Cons
  @@count = 0

  def initialize(items=nil)
    super

    if items.is_a? Sexp
      # this should do it but it's not used
      @car = items.car
      @cdr = items.cdr
    elsif items
      cons = self
      i = 0
      items.each do |car|
        i += 1
        cons.car = car
        cons.cdr = Sexp.new() if i < items.length
        cons = cons.cdr
      end
    end
  end

  include Enumerable
  # used by .map
  def each
    self.iterate_list { |cons| yield cons.car }
  end

  def length
    cnt = 0
    self.iterate_list do |cons|
      if cons.cdr != nil
        cnt += 1
      elsif cons.car != nil
        cnt += 1
      end
    end
    return cnt
  end

  # proper list
  def iterate_list
    this = self
    yield this
    while this.cdr != nil
      if this.cdr.is_a? Sexp
        this = this.cdr
        yield this
      else
        break
      end
    end
    return nil
  end

  def _to_s
    # dotted
    str = '('
    # recursive
    str += "#{self.car} . #{self.cdr}"
    str += ')'
    return str
  end
  # def to_s
  #   return _to_s
  # end
  def to_s
    return string
  end

  def string
    str = '('
    self.iterate_list {|cons|
      # list
      if cons.car.is_a? Sexp
        # we don't use this
        str += cons.car.string
      else
        if cons.car != nil
          if cons.car.is_a? Token
            str += "#{cons.car}"
          else
            str += "#{cons.car.inspect}"
          end
        elsif cons.cdr != nil
          str += "NIL"
        end
      end

      if cons.cdr == nil
        # end of list
      else
        if cons.cdr.is_a? Sexp
          # next yield
          str += ' '
        else
          if cons.cdr != nil
            str += ' '
            str += ". #{cons.cdr}"
          else
            str += cons.cdr.to_s
          end
        end
      end
    }
    str += ')'
    return str
  end

  def last
    this = self
    while this.cdr != nil
      this = this.cdr
    end
    return this
  end

  def <<(value)
    cons = last()
    if cons.car == nil
      cons.car = value
    else
      cons.cdr = self.class.new([value])
    end
    return self
  end

  # def unshift
  #   # basically this but handle edge cases
  #   @car, @cdr = @cdr.car, @cdr.cdr
  # end
end
Sexp = Sexp1

def read_sexp(code, i)
  #debug "read_sexp at #{i} {#{code.slice(i,10)}}"
  raise "not sexp: #{code.slice(i,10)}" if not is_sexp(code[i])
  # consume (
  i = next_char(code, i)

  sexp = Sexp.new
  while not is_eof(code,i) and not is_end(code[i])
    i, token = read_token(code, i)
    sexp << token if token
  end
  # consume )
  i = next_char(code, i)

  debug "read_sexp=#{sexp}"
  return i, sexp
end

def read(code)
  tokens = []
  i = 0
  while not is_eof(code, i)
    i, token = read_token(code, i)
    break if not token
    tokens << token
  end

  return tokens
end

$defvar = {
  'nil' => nil,
  # happens in each scope by default
  #nil => nil,
}
# note that we will allow to redeclare with new value in same context
def defvar(env, name, token)
  debug "defvar #{name} with #{token} in env #{env}"

  value = eval_token(token, env)
  # we track call stack now, env passing is not necessary
  #env = $stack[0][2] if $stack.length > 0
  if env
    env[name] = value
  else
    $defvar[name] = value
  end

  debug "defvar #{name}=#{value}"
  return value
end

def setq(env, name, token)
  debug "setq #{name} with #{token} in env #{env}"
  env, ctx = env

  # find in call stack
  env = hasvar(name)
  raise "unknown variable #{name}" if env == nil

  value = eval_token(token, ctx)
  env[name] = value

  debug "setq #{name}=#{value}"
  return value
end

def progn(env, tokens)
  result = nil
  tokens.each do |token|
    result = eval_token(token, env)
  end
  return result
end

def boundp(env, sym)
  # whatever is there will be used as symbol
  if hasvar(sym)
    return true
  # elsif env and env.include?(sym)
  #   return true
  # else
  #   return $defvar.include?(sym)
  end
end

# if_fn, boundp, not - everything implicitly relies on what ruby says is true
def if_fn(env, cond, a, b=nil)
  if eval_token(cond, env)
    return eval_token(a, env)
  else
    return eval_token(b, env)
  end
end

# as above - whatever ruby says, no explicit rules or T/F values
def eq_fn(env, a, b)
  return eval_token(a, env) == eval_token(b, env)
end

def not_fn(env, a)
  return !eval_token(a, env)
end

def car_fn(env, token)
  list = eval_token(token, env)
  if list.is_a? Sexp
    return list.car()
  else
    raise "expected a Sexp and got #{list.class}: #{list}"
  end
end

def cdr_fn(env, token)
  list = eval_token(token, env)
  if list.is_a? Sexp
    return list.cdr()
  else
    raise "expected a Sexp and got #{list.class}: #{list}"
  end
end

def cdr_fn(env, token)
  list = eval_token(token, env)
  if list.is_a? Sexp
    return list.cdr()
  else
    raise "expected a Sexp and got #{list.class}: #{list}"
  end
end

def length_fn(env, obj)
  if obj.is_a? Sexp or
    return obj.length
  elsif obj.is_a? StringType
    return obj.length
  else
    raise "expected a Sexp/StringType and got #{obj.class}: #{obj}"
  end
end

def list_fn(env, args)
  return Sexp.new(args.map {|x| eval_token(x, env)})
end

def cons_fn(env, car, cdr)
  s = Sexp.new()
  s.car = eval_token(car, env)
  s.cdr = eval_token(cdr, env)
  return s
end

def resolve_fn(env, fn)
  # function supplied (lambda or variable)
  if fn.is_a? Proc
    # ok
  elsif fn.is_a? Sexp
    callable = eval_sexp(env, fn)
  elsif hasvar(fn)
    callable = getvar(fn)
  # elsif env and env.include? fn
  #   callable = env[fn]
  # elsif $defvar.include? fn
  #   callable = $defvar[fn]
  else
    raise "function token #{fn} is undefined or not callable"
  end
end

def funcall_fn(env, fn, *args)
  callable = resolve_fn(env, fn)
  args = args.map {|x| eval_token(x, env)}
  return call(env, callable, args)
end

# (apply fn [args] list) is tricky
def apply_fn(env, fn, *args)
  if args.length < 1
    raise 'apply requires arguments'
  elsif not args[-1].is_a? Sexp
    raise 'apply requires list as last argument (1)'
  end

  list = args.pop
  args = args.map {|x| eval_token(x, env)}
  list = eval_sexp(env, list)
  if not list.is_a? Sexp
    raise 'apply requires list as last argument (2)'
  end

  args.push(*list)
  callable = resolve_fn(env, fn)
  return call(env, callable, args)
end

def map_fn(env, fn, token)
  result = Sexp.new
  eval_token(token).each do |item|
    result << call(env, fn, [item])
  end
  return result
end

def reduce_fn(env, fn, token)
  result = eval_token(token).reduce do |a,b|
    call(env, fn, [a, b])
  end
  return result
end

def print_fn(env, args)
  args.each {|x| print eval_token(x, env) }
  return nil
end

def mod_fn(env, a, b)
  return (eval_token(a, env) % eval_token(b, env))
end


def reduce_proc(fn, init=nil, min=0, &proc)
  return lambda do |env, args|
    if args.length < min
      raise "function #{fn} requires at least #{min} arguments"
    end
    args = args.map {|x| eval_token(x, env)}
    args.unshift(init) if init
    args.reduce(&proc)
  end
end

$defun = {
  '+' => reduce_proc(0) {|a,b| a+b },
  '-' => reduce_proc(fn='-', init=0, min=1) {|a,b| a-b },
  '*' => reduce_proc(1) {|a,b| a*b },
  '/' => reduce_proc(fn='/', init=1.0, min=1) {|a,b| a/b },
  'mod' => lambda { |env,args| mod_fn(env, *args) },
  'ruby' => lambda { |env,args| eval(*args.map {|x| eval_token(x, env)}) },
  'puts' => lambda { |env,args| puts args.map {|x| eval_token(x, env)} },
  'print' => lambda { |env,args| print_fn(env, args.map {|x| eval_token(x,env)}) },
  'exit' => lambda { |env,args| exit(*args.map {|x| eval_token(x, env)}) },
  'backtrace' => lambda { |env,args| backtrace_fn(env, *args) },

  # run in env but write to $defvar
  # only supports one variable to set
  #'setq' => lambda { |env,args| setq([nil, env], *args) },
  'defvar' => lambda { |env,args| defvar(env, *args) },
  'setq' => lambda { |env,args| setq(env, *args) },
  # same as defvar
  'let' => lambda { |env,args| defvar(env, *args) },

  # no arguments
  'defun0' => lambda { |env,args| defun0(*args) },

  # arguments
  # creates new context (only arglist and global variables)
  'defun1' => lambda { |env,args| defun1(*args) },
  # rewritten with lambda
  'defun' => lambda { |env,args| defun(*args) },
  'function' => lambda { |env,args| function_fn(env, *args) },
  'lambda' => lambda { |env,args| lambda_fn(*args) },

  'funcall' => lambda { |env,args| funcall_fn(env,*args) },
  'apply' => lambda { |env,args| apply_fn(env, *args) },
  'map' => lambda { |env,args| map_fn(env, *args) },
  'reduce' => lambda { |env,args| reduce_fn(env, *args) },
  'progn' => lambda { |env,args| progn(env, args) },
  # this is better than when/unless
  'if' => lambda { |env,args| if_fn(env, *args) },
  'not' => lambda { |env,args| not_fn(env, *args) },
  '=' => lambda { |env,args| eq_fn(env, *args) },
  'boundp' => lambda { |env,args| boundp(env, *args) },
  'length' => lambda { |env,args| length_fn(env, *args) },
  'list' => lambda { |env,args| list_fn(env, args) },
  'cons' => lambda { |env,args| cons_fn(env, *args) },

  # Sexp is Array
  'car' => lambda { |env,args| car_fn(env, *args) },
  'cdr' => lambda { |env,args| cdr_fn(env, *args) },
  'last' => lambda { |env,args| last_fn(env, *args) },
}

# all arguments ignored
def defun0(name, sexp)
  debug "defun0 #{name} with body #{sexp}"
  fn = lambda { |env,args| eval_token(sexp, env) }
  $defun[name] = fn
  debug "defun0 #{name}=#{sexp}"
  # this could be useful
  return fn
end

def defun1(name, arglist, sexp)
  debug "defun #{name} with arglist #{arglist} and body #{sexp}"
  fn = lambda do |env,args|
    # bind variables locally
    # only arglist and global variables will be recognized
    env = {}
    arglist.zip(args).each do |arg_name, arg_token|
      setq(env, arg_name, arg_token)
    end
    eval_token(sexp, env)
  end
  $defun[name] = fn
  debug "defun #{name}=#{sexp}"
  # this could be useful
  return fn
end

# rewritten with lambda
def defun(name, arglist, sexp)
  debug "defun #{name} with arglist #{arglist} and body #{sexp}"
  fn = lambda_fn(arglist, sexp, name)
  $defun[name] = fn
  debug "defun #{name}=#{fn}"
  # this could be useful
  return fn
end

$stack = []
# only lambda creates new context
def lambda_fn(arglist, sexp, name=nil)
  debug "lambda with arglist #{arglist} and body #{sexp}"
  fn = lambda do |env,args|
    begin
      if arglist.length != args.length
        raise "lambda wants #{arglist.length} arguments, got #{args.length}"
      end

      env = {}
      # allows us to look for variables
      $stack.unshift([fn, args.to_s, env])

      arglist.zip(args).each do |arg_name, arg_token|
        defvar(env, arg_name, arg_token)
      end

      eval_token(sexp, env)
    rescue => e
      puts "globals: #{$defvar}"
      puts "call stack: #{$stack}"
      raise e
    ensure
      $stack.shift
    end
  end
  debug "lambda #{fn}"
  return fn
end

def backtrace_fn(env)
  #s = Sexp.new
  s = "Call stack:\n"
  $stack.each do |frame|
    fn, argss, env = frame
    #s << Sexp.new([x[0], x[2]])
    fn = "anonymous\n\t\tcode: #{fn}" if fn.is_a? Sexp
    fn = "lambda" if fn == nil
    s << "\t#{fn}\n\t\targs: #{argss}\n\t\tlocals: #{env}\n"
  end
  return s
end

def function_fn(env, fn)
  if fn.is_a? Sexp
    # maybe a lambda
    return eval_sexp(env, fn)
  elsif $defun.include? fn
    return $defun[fn]
  else
    raise "unknown function #{fn}"
  end
end

def call(env, fn, args)
  callable = nil
  if fn.is_a? Token
    # call by name
    if $defun.include? fn
      callable = $defun[fn]
    else
      raise "unknown function #{fn}"
    end
  else
    # function supplied
    # we are searching in $defun without (function fn) or #'fn
    # use funcall/apply for lambda in variable
    callable = eval_token(fn, env)
    if not callable.is_a? Proc
      raise "function token #{fn} is not callable"
    end
  end

  return callable.(env, args)
end

def eval_sexp(env, sexp)
  raise "not a sexp: #{sexp}" if not sexp.is_a? Sexp
  debug sexp.to_s
  #fn, *args = sexp
  fn = sexp.car
  args = sexp.cdr
  args = [] if args == nil
  debug "eval #{fn} with #{args} in env #{env}"
  # call by name or supplied function
  result = call(env, fn, args)
  debug "eval=#{result}"
  return result
end

# returns appropriate env
def hasvar(token)
  $stack.each do |fn, args, env|
    return env if env and env.include? token
  end
  if $defvar.include? token
    return $defvar
  else
    return nil
  end
end

# don't use this as predicate, it could resolve to nil!
def getvar(token)
  $stack.each do |fn, args, env|
    return env[token] if env and env.include? token
  end
  return $defvar[token] if $defvar.include? token
  raise "unknown variable #{token}"
end

def eval_token(token, env=nil)
  if token.is_a? Sexp
    return eval_sexp(env, token)
  elsif not token.is_a? Token
    # already evaluated from token
    return token
  elsif hasvar(token)
    return getvar(token)
  # now env is not required if we track call stack, handled above
  # elsif env and env.include? token
  #   # local variable
  #   return env[token]
  # elsif $defvar.include? token
  #   # global variable
  #   return $defvar[token]
  elsif token.is_a? Token
    if is_num(token)
      return parse_num(token)
    else
      raise "unknown token #{token}"
    end
  else
    raise "unknown type #{token.class}: #{token}"
  end
end

def eval_code(code)
  internal = read(code).each { |token|
    eval_token(token, nil)
  }
end

def repl
  require "readline"
  input = ''
  while buf = Readline.readline("> ", true)
    # TODO it's confusing and we have currently no way to stop
    # if code is not complete append
    # if input.length > 0
    #   input += "\n"
    #   Readline::HISTORY.pop
    #   Readline::HISTORY.pop
    #   Readline::HISTORY.push input+buf
    # end
    # input += buf
    input = buf
    begin
      read(input).each do |token|
        puts "EVAL> #{token}"
        begin
          result = eval_token(token, nil)
          puts "TYPE> #{result.class}"
          puts "RESULT> #{result}"
        rescue => e
      puts "ERROR> #{e}"
          puts e.backtrace
        end
      end
      input = ''
    rescue UnexpectedEnd => e
      # replace incomplete line with current input
      # Readline::HISTORY.pop
      # Readline::HISTORY.push input
      puts "ERROR> #{e}"
      puts e.backtrace
    rescue => e
      puts "ERROR> #{e}"
      puts e.backtrace
      # clear input to allow fix
      input = ''
    end
  end
end

at_exit { info('consed: ', Cons.count) }

if __FILE__ != 'test.rb'
  puts ARGV
  if ARGV.length == 0
    repl()
  else
    code = File.read(ARGV[0])
    read(code).each do |token|
      puts "EVAL> #{token}"
      result = eval_token(token, nil)
      puts "TYPE> #{result.class}"
      puts "RESULT> #{result}"
    end
  end
else
  # s = Sexp.new([1,2,3])
  # puts s.string
  # s.last.cdr = 4
  # puts s.string
  # fn,*args = s
  # puts
  # puts fn
  # puts args.to_s
  # s = Sexp.new
  # 1.upto(5) {|i| s << i; puts s.string }
  # puts s.length
  # proably also apply index access
  # s = Sexp.new()
  # puts s.length
  # puts s.string
  # exit(0)

  puts $defun.keys.map { |fn| "``#{fn}``"}.join(', ')
  code = "1 (+ (- (* (/ 1 2) 3) 4) 5 6 7)"
  code += "1 (+ (- (* (/ 0b1 2) 3) 4) 5 0x6 007)"
  code += "1 (+ (- (* (/ 0b1 2) 3) 4) 5 0x6 007 0.0)"
  #code += %q{(ruby "puts 'str'")}
  #code += %q{(ruby 'puts "str"')}
  #code += %q{(ruby 'puts "\'str\'"')}
  # %Q does not require " to be quoted but still madness
  code += %Q{(ruby "puts \\" \\\\\\" str \\\\\\" \\"")}
  code += <<EOF
(ruby "puts \\" \\\\\\\" str \\\\\\\" \\" ")
EOF
  code += %q{(ruby "Kernel.exit(1)")}
  code = File.read('test.lisp')
  debug code
  # now we can implement functions in our language
  eval_code("(defun internal () (puts 'internal'))")
  evald = []
  read(code).each do |token|
    puts "EVAL> #{token}"
    evald << token
    result = eval_token(token, nil)
    puts "TYPE> #{result.class}"
    puts "RESULT> #{result}"
  end

  #require 'pp'
  #pp evald
end
