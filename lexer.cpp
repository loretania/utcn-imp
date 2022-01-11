// This file is part of the IMP project.

#include <sstream>

#include "lexer.h"



// -----------------------------------------------------------------------------
Token::Token(const Token &that)
  : loc_(that.loc_)
  , kind_(that.kind_)
{
  switch (kind_) {
    case Kind::STRING:
    case Kind::IDENT: {
      value_.StringValue = new std::string(*that.value_.StringValue);
      break;
    }
    case Kind::INT:
      value_.IntValue = that.value_.IntValue;
      break;
    default: {
      break;
    }
  }
}

// -----------------------------------------------------------------------------
Token &Token::operator=(const Token &that)
{
  switch (kind_) {
    case Kind::STRING:
    case Kind::IDENT: {
      delete value_.StringValue;
      break;
    }
    default: {
      break;
    }
  }
  loc_ = that.loc_;
  kind_ = that.kind_;
  switch (kind_) {
    case Kind::STRING:
    case Kind::IDENT: {
      value_.StringValue = new std::string(*that.value_.StringValue);
      break;
    case Kind::INT:
      value_.IntValue = that.value_.IntValue;
      break;
    }
    default: {
      break;
    }
  }
  return *this;
}

// -----------------------------------------------------------------------------
Token::~Token()
{
  switch (kind_) {
    case Kind::STRING:
    case Kind::IDENT: {
      delete value_.StringValue;
      break;
    }
    default: {
      break;
    }
  }
}

// -----------------------------------------------------------------------------
Token Token::Ident(const Location &l, const std::string &str)
{
  Token tk(l, Kind::IDENT);
  tk.value_.StringValue = new std::string(str);
  return tk;
}

// -----------------------------------------------------------------------------
Token Token::String(const Location &l, const std::string &str)
{
  Token tk(l, Kind::STRING);
  tk.value_.StringValue = new std::string(str);
  return tk;
}

// -----------------------------------------------------------------------------
Token Token::Integer(const Location &l, const uint64_t value)
{
  Token tk(l, Kind::INT);
  tk.value_.IntValue = value;
  return tk;
}

// -----------------------------------------------------------------------------
void Token::Print(std::ostream &os) const
{
  os << kind_;
  switch (kind_) {
    case Kind::INT: {
      os << "(" << value_.IntValue << ")";
      break;
    }
    case Kind::STRING: {
      os << "(\"" << *value_.StringValue << "\")";
      break;
    }
    case Kind::IDENT: {
      os << "(" << *value_.StringValue << ")";
      break;
    }
    default: {
      break;
    }
  }
}

// -----------------------------------------------------------------------------
std::ostream &operator<<(std::ostream &os, const Token::Kind kind)
{
  switch (kind) {
    case Token::Kind::FUNC: return os << "func";
    case Token::Kind::RETURN: return os << "return";
    case Token::Kind::WHILE: return os << "while";
    case Token::Kind::IF: return os << "if";
    case Token::Kind::ELSE: return os << "else";
    case Token::Kind::LPAREN: return os << "(";
    case Token::Kind::RPAREN: return os << ")";
    case Token::Kind::LBRACE: return os << "{";
    case Token::Kind::RBRACE: return os << "}";
    case Token::Kind::COLON: return os << ":";
    case Token::Kind::SEMI: return os << ";";
    case Token::Kind::EQUAL: return os << "=";
    case Token::Kind::EQUALITY: return os << "==";
    case Token::Kind::COMMA: return os << ",";
    case Token::Kind::PLUS: return os << "+";
    case Token::Kind::END: return os << "END";
    case Token::Kind::INT: return os << "INT";
    case Token::Kind::STRING: return os << "STRING";
    case Token::Kind::IDENT: return os << "IDENT";
    case Token::Kind::STAR: return os << "*";
    case Token::Kind::SLASH: return os << "/";
    case Token::Kind::PROCENT: return os << "%";
    case Token::Kind::MINUS: return os << "-";
  
  }
  return os;
}

// -----------------------------------------------------------------------------
static std::string FormatMessage(const Location &loc, const std::string &msg)
{
  std::ostringstream os;
  os << "[" << loc.Name << ":" << loc.Line << ":" << loc.Column << "] " << msg;
  return os.str();
}

// -----------------------------------------------------------------------------
LexerError::LexerError(const Location &loc, const std::string &msg)
  : std::runtime_error(FormatMessage(loc, msg))
{
}

// -----------------------------------------------------------------------------
Lexer::Lexer(const std::string &name)
  : name_(name)
  , is_(name)
{
  NextChar();
  Next();
}

// -----------------------------------------------------------------------------
static bool IsIdentStart(char chr)
{
  return chr == '_' || isalpha(chr);
}

// -----------------------------------------------------------------------------
static bool IsIdentLetter(char chr)
{
  return IsIdentStart(chr) || isdigit(chr);
}

// -----------------------------------------------------------------------------
const Token &Lexer::Next()
{
  // Skip all whitespace until a valid token.
  while (isspace(chr_)) { NextChar(); }

  // Return a token based on the character.
  auto loc = GetLocation();
  switch (chr_) {
    case '\0': return tk_ = Token::End(loc);
    case '(': return NextChar(), tk_ = Token::LParen(loc);
    case ')': return NextChar(), tk_ = Token::RParen(loc);
    case '{': return NextChar(), tk_ = Token::LBrace(loc);
    case '}': return NextChar(), tk_ = Token::RBrace(loc);
    case ':': return NextChar(), tk_ = Token::Colon(loc);
    case ';': return NextChar(), tk_ = Token::Semi(loc);
    case '=': {
      NextChar();
      if (chr_ == '=')
      {
        return NextChar(), tk_ = Token::Equality(loc);
      }
      return tk_ = Token::Equal(loc);
    }
    case '+': return NextChar(), tk_ = Token::Plus(loc);
    case '-': return NextChar(), tk_ = Token::Minus(loc);
    case ',': return NextChar(), tk_ = Token::Comma(loc);
    case '"': {
      std::string word;
      NextChar();
      while (chr_ != '"') {
        word.push_back(chr_);
        NextChar();
        if (chr_ == '\0') {
          Error("string not terminated");
        }
      }
      NextChar();
      return tk_ = Token::String(loc, word);
    }
    case '*': return NextChar(), tk_ = Token::Star(loc);
    case '/': return NextChar(), tk_ = Token::Slash(loc);
    case '%': return NextChar(), tk_ = Token::Procent(loc);
    default: {
      if (IsIdentStart(chr_)) {
        std::string word;
        do {
          word.push_back(chr_);
          NextChar();
        } while (IsIdentLetter(chr_));
        if (word == "func") return tk_ = Token::Func(loc);
        if (word == "return") return tk_ = Token::Return(loc);
        if (word == "while") return tk_ = Token::While(loc);
        if (word == "if") return tk_ = Token::If(loc);
        if (word == "else") return tk_ = Token::Else(loc);
        return tk_ = Token::Ident(loc, word);
      }

      if (isdigit(chr_)) {
        std::string digits;
        do {
            digits.push_back(chr_);
            NextChar();
        } while(isdigit(chr_));
        int value = 0;
        int p = 1;
        for (auto it=digits.rbegin(); it != digits.rend(); ++it)
        {
            value += (*it - '0') * p;
            p *= 10;
        }
        return tk_ = Token::Integer(loc, value);
      }
      Error("unknown character '" + std::string(1, chr_) + "'");
    }
  }
}

// -----------------------------------------------------------------------------
void Lexer::NextChar()
{
  if (is_.eof()) {
    chr_ = '\0';
  } else {
    if (chr_ == '\n') {
      lineNo_++;
      charNo_ = 1;
    } else {
      charNo_++;
    }
    is_.get(chr_);
  }
}

// -----------------------------------------------------------------------------
void Lexer::Error(const std::string &msg)
{
  throw LexerError(GetLocation(), msg);
}
