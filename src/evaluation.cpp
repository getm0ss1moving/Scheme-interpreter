#include "Def.hpp"
#include "value.hpp"
#include "expr.hpp"
#include "RE.hpp"
#include "syntax.hpp"
#include <cstring>
#include <vector>
#include <map>
using std::vector;
extern std :: map<std :: string, ExprType> primitives;
extern std :: map<std :: string, ExprType> reserved_words;
void checkVar(std::string x){
    if(x.empty()){
        throw RuntimeError("Variable name cannot be empty40");
    }else{
        if(x[0]!='.'&&x[0]!='@'&&!isdigit(x[0])){
            return;
        }else{
            throw RuntimeError("Invalid variable name39");
        }
    }
}
Value Let::eval(Assoc &env) {
    Assoc cur = env;
    vector<std::pair<std::string,Value>>bind_v;
    for(auto i:bind){
        bind_v.push_back({i.first,i.second->eval(env)});
    }
    for(auto j:bind_v){
        cur=extend(j.first,j.second,cur);
    }
    return body->eval(cur);
} // let expression

Value Lambda::eval(Assoc &env) {
    return ClosureV(x,e,env);
} // lambda expression

Value Apply::eval(Assoc &e) {
    Value func = rator->eval(e);
    Closure* func_ = dynamic_cast<Closure*>(func.get());
    Assoc cur = func_->env;
    vector<std::pair<std::string,Value>>bind_v;
    if(func_->parameters.size()==rand.size()){
        for(int i = 0;i<func_->parameters.size();i++){
            bind_v.push_back({func_->parameters[i], rand[i]->eval(e)});
        }
        for(auto j:bind_v){
            cur=extend(j.first,j.second,cur);
        }
        return func_->e->eval(cur);
    }else{
        throw RuntimeError("Invalid number of arguments38");
    }

    
} // for function calling

Value Letrec::eval(Assoc &env) {
    Assoc cur = env;
    vector<Value> bind_;
    for(auto i:bind){
        cur = extend(i.first,Value(nullptr),cur);
    }
    for(auto j:bind){
        bind_.push_back(j.second->eval(cur));
    }
    for(int k = 0; k < bind_.size();k++){
        modify(bind[k].first,bind_[k],cur);
    }
    return body->eval(cur);
} // letrec expression

Value Var::eval(Assoc &e) {
    checkVar(x);
    Value var = find (x,e);
    if(var.get()==nullptr){
        if(primitives.count(x)!=0){
            std::vector<std::string>para;
            switch (primitives[x]){
                case E_MUL:{
                    para.push_back("1");
                    para.push_back("2");
                    Expr exp = (new Mult(new Var("1"),new Var("2")));
                    Assoc newe = e;
                    return ClosureV(para,exp,newe);
                }
                case E_PLUS:{
                    para.push_back("1");
                    para.push_back("2");
                    Expr exp = (new Plus(new Var("1"),new Var("2")));
                    Assoc newe = e;
                    return ClosureV(para,exp,newe);
                   
                }
                case E_MINUS:{
                    para.push_back("1");
                    para.push_back("2");
                    Expr exp = (new Minus(new Var("1"),new Var("2")));
                    Assoc newe = e;
                    return ClosureV(para,exp,newe);
                   
                }
                case E_LT:{
                    para.push_back("1");
                    para.push_back("2");
                    Expr exp = (new Less(new Var("1"),new Var("2")));
                    Assoc newe = e;
                    return ClosureV(para,exp,newe);
                    
                }
                case E_LE:{
                    para.push_back("1");
                    para.push_back("2");
                    Expr exp = (new LessEq(new Var("1"),new Var("2")));
                    Assoc newe = e;
                    return ClosureV(para,exp,newe);
                   
                }
                case E_GT:{
                    para.push_back("1");
                    para.push_back("2");
                    Expr exp = (new Greater(new Var("1"),new Var("2")));
                    Assoc newe = e;
                    return ClosureV(para,exp,newe);
                    
                }
                 case E_GE:{
                    para.push_back("1");
                    para.push_back("2");
                    Expr exp = (new GreaterEq(new Var("1"),new Var("2")));
                    Assoc newe = e;
                    return ClosureV(para,exp,newe);
                    
                }
                case E_EQ:{
                    para.push_back("1");
                    para.push_back("2");
                    Expr exp = (new Equal(new Var("1"),new Var("2")));
                    Assoc newe = e;
                    return ClosureV(para,exp,newe);
                   
                }
                case E_EQQ:{
                    para.push_back("1");
                    para.push_back("2");
                    Expr exp = (new IsEq(new Var("1"),new Var("2")));
                    Assoc newe = e;
                    return ClosureV(para,exp,newe);
                    
                }
                case E_CONS:{
                    para.push_back("1");
                    para.push_back("2");
                    Expr exp = (new Cons(new Var("1"),new Var("2")));
                    Assoc newe = e;
                    return ClosureV(para,exp,newe);
                    
                }
                case E_BOOLQ:{
                    para.push_back("1");
                    Expr exp = (new IsBoolean(new Var("1")));
                    Assoc newe = e;
                    return ClosureV(para,exp,newe);
                }
                case E_INTQ:{
                    para.push_back("1");
                    Expr exp = (new IsFixnum(new Var("1")));
                    Assoc newe = e;
                    return ClosureV(para,exp,newe);
                    
                }
                case E_NULLQ:{
                    para.push_back("1");
                    Expr exp = (new IsFixnum(new Var("1")));
                    Assoc newe = e;
                    return ClosureV(para,exp,newe);
                    
                }
                case E_PAIRQ:{
                    para.push_back("1");
                    Expr exp = (new IsPair(new Var("1")));
                    Assoc newe = e;
                    return ClosureV(para,exp,newe);
                    
                }
                case E_PROCQ:{
                    para.push_back("1");
                    Expr exp = (new IsProcedure(new Var("1")));
                    Assoc newe = e;
                    return ClosureV(para,exp,newe);
                    
                }
                case E_SYMBOLQ:{
                    para.push_back("1");
                    Expr exp = (new IsSymbol(new Var("1")));
                    Assoc newe = e;
                    return ClosureV(para,exp,newe);
                    
                }
                case E_NOT:{
                    para.push_back("1");
                    Expr exp = (new Not(new Var("1")));
                    Assoc newe = e;
                    return ClosureV(para,exp,newe);
                    
                }
                case E_CAR:{
                    para.push_back("1");
                    Expr exp = (new Car(new Var("1")));
                    Assoc newe = e;
                    return ClosureV(para,exp,newe);
                    
                }
                case E_CDR:{
                    para.push_back("1");
                    Expr exp = (new Cdr(new Var("1")));
                    Assoc newe = e;
                    return ClosureV(para,exp,newe);
                    
                }
                case E_VOID:{
                    Expr exp = (new MakeVoid());
                    Assoc newe = e;
                    return ClosureV(para,exp,newe);
                    
                }
                case E_EXIT:{
                    Expr exp = (new Exit());
                    Assoc newe = e;
                    return ClosureV(para,exp,newe);
                    
                }
                default:{
                    throw RuntimeError ("Undifined primitives");
                }
                
            }
        }else{
            throw RuntimeError ("Undefined var");
        }
    }else{
        return var;
    }
    //error
} // evaluation of variable

Value Fixnum::eval(Assoc &e) {
    return IntegerV(n);
} // evaluation of a fixnum

Value If::eval(Assoc &e) {
    Value cond_ = cond->eval(e);
    if(cond_->v_type==V_BOOL){
        if(dynamic_cast<Boolean*>(cond_.get())->b==false){
            return alter->eval(e);
        }
    }
    return conseq->eval(e);
} // if expression

Value True::eval(Assoc &e) {
    return BooleanV(true);
} // evaluation of #t

Value False::eval(Assoc &e) {
    return BooleanV(false);
} // evaluation of #f

Value Begin::eval(Assoc &e) {
    if(!es.empty()){    
        for(auto i: es){
            i->eval(e);
        }
        return es[es.size()-1]->eval(e);
    }else{
        return NullV();
    }

} // begin expression

Value Quote::eval(Assoc &e) {
    if(dynamic_cast<List*>(s.get())){
        int count=0;
        List *lst = dynamic_cast<List*>(s.get());
        for(int i = 0; i < lst->stxs.size();i++){
            if(dynamic_cast<Identifier*>(lst->stxs[i].get())){
                if(dynamic_cast<Identifier*>(lst->stxs[i].get())->s=="."){
                    count++;
                }
            }
        }
        if(count>=2){
            throw RuntimeError("Too many dots in quote36");
        }else{
            if(lst->stxs.size()==0){
                return NullV();
            }
            Syntax a = lst->stxs[0];
            Quote q(a);
            Value v = q.eval(e);
            if(lst->stxs.size()==3){
                if(dynamic_cast<Identifier*>(lst->stxs[1].get())){
                    if(dynamic_cast<Identifier*>(lst->stxs[1].get())->s=="."){
                        Syntax a1 = lst->stxs[2];
                        Quote q1(a1);
                        Value v1 = q1.eval(e);
                        return Value(new Pair(v,v1));
                    }
                }
            }
            List *nxt = new List();
            for(int j = 1;j<lst->stxs.size();j++){
                nxt->stxs.push_back(lst->stxs[j]);
            }
            Syntax a2(nxt);
            Expr b2(new Quote(a2));
            return Value(new Pair(v,b2->eval(e)));

        }
    }else if(dynamic_cast<Identifier*>(s.get())){
        Identifier *id = dynamic_cast<Identifier*>(s.get());
        return SymbolV(id->s);
    }
    else if(dynamic_cast<TrueSyntax*>(s.get())){
        return BooleanV(true);
    }else if(dynamic_cast<FalseSyntax*>(s.get())){
        return BooleanV(false);
    }else if(auto it = dynamic_cast<Number*>(s.get())){
        return IntegerV(it->n);
    }
    
} // quote expression

Value MakeVoid::eval(Assoc &e) {
    return VoidV();
} // (void)

Value Exit::eval(Assoc &e) {
    return TerminateV();
} // (exit)

Value Binary::eval(Assoc &e) {
    if(e_type == E_PLUS||e_type == E_MINUS|| e_type ==E_MUL
    ||e_type == E_LT||e_type == E_LE||e_type == E_EQ
    ||e_type == E_GT||e_type == E_GE ){
        Value rand1_ = rand1->eval(e);
        Value rand2_ = rand2->eval(e);
        if(rand1_.get()->v_type==V_INT&&rand2_.get()->v_type==V_INT){
            return evalRator(rand1_,rand2_);
        }else{
            throw RuntimeError("wrong type35");
        }
    }
    if(e_type == E_CONS||e_type == E_EQQ){
        Value rand1_ = rand1->eval(e);
        Value rand2_ = rand2->eval(e);
        return evalRator(rand1_,rand2_);
    }
} // evaluation of two-operators primitive

Value Unary::eval(Assoc &e) {
    Value rand_=rand->eval(e);
    return evalRator(rand_);
} // evaluation of single-operator primitive

Value Mult::evalRator(const Value &rand1, const Value &rand2) {
    Integer* rand1_=dynamic_cast<Integer*>(rand1.get());
    Integer* rand2_=dynamic_cast<Integer*>(rand2.get());
    int answer=rand1_->n * rand2_->n;
    return IntegerV(answer);   
} // *

Value Plus::evalRator(const Value &rand1, const Value &rand2) {
    Integer* rand1_=dynamic_cast<Integer*>(rand1.get());
    Integer* rand2_=dynamic_cast<Integer*>(rand2.get());
    int answer=rand1_->n + rand2_->n;
    return IntegerV(answer);
} // +

Value Minus::evalRator(const Value &rand1, const Value &rand2) {
    Integer* rand1_=dynamic_cast<Integer*>(rand1.get());
    Integer* rand2_=dynamic_cast<Integer*>(rand2.get());
    int answer=rand1_->n - rand2_->n;
    return IntegerV(answer);
} // -

Value Less::evalRator(const Value &rand1, const Value &rand2) {
    Integer* rand1_=dynamic_cast<Integer*>(rand1.get());
    Integer* rand2_=dynamic_cast<Integer*>(rand2.get());
    return BooleanV(rand1_->n < rand2_->n);
} // <

Value LessEq::evalRator(const Value &rand1, const Value &rand2) {
    Integer* rand1_=dynamic_cast<Integer*>(rand1.get());
    Integer* rand2_=dynamic_cast<Integer*>(rand2.get());
    return BooleanV(rand1_->n <= rand2_->n);
} // <=

Value Equal::evalRator(const Value &rand1, const Value &rand2) {
    Integer* rand1_=dynamic_cast<Integer*>(rand1.get());
    Integer* rand2_=dynamic_cast<Integer*>(rand2.get());
    return BooleanV(rand1_->n == rand2_->n);
} // =

Value GreaterEq::evalRator(const Value &rand1, const Value &rand2) {
    Integer* rand1_=dynamic_cast<Integer*>(rand1.get());
    Integer* rand2_=dynamic_cast<Integer*>(rand2.get());
    return BooleanV(rand1_->n >= rand2_->n);
} // >=

Value Greater::evalRator(const Value &rand1, const Value &rand2) {
    Integer* rand1_=dynamic_cast<Integer*>(rand1.get());
    Integer* rand2_=dynamic_cast<Integer*>(rand2.get());
    return BooleanV(rand1_->n > rand2_->n);
} // >

Value IsEq::evalRator(const Value &rand1, const Value &rand2) {
    if(rand1.get()->v_type==rand2.get()->v_type){
        if(rand1.get()->v_type==V_INT&&rand2.get()->v_type==V_INT){
            Integer* rand1_=dynamic_cast<Integer*>(rand1.get());
            Integer* rand2_=dynamic_cast<Integer*>(rand2.get());
            return BooleanV(rand1_->n==rand2_->n);
        }
        else if(rand1.get()->v_type==V_STRING&&rand2.get()->v_type==V_STRING){
            Symbol* rand1_=dynamic_cast<Symbol*>(rand1.get());
            Symbol* rand2_=dynamic_cast<Symbol*>(rand2.get());
            return BooleanV(rand1_->s==rand2_->s);
        }else if(rand1.get()->v_type==V_BOOL&&rand2.get()->v_type==V_BOOL){
            Boolean* rand1_=dynamic_cast<Boolean*>(rand1.get());
            Boolean* rand2_=dynamic_cast<Boolean*>(rand2.get());
            return BooleanV(rand1_->b==rand2_->b);
        }else if(rand1.get()->v_type==V_NULL&&rand2.get()->v_type==V_NULL
        || rand1.get()->v_type==V_VOID&&rand2.get()->v_type==V_VOID){
            return BooleanV(true);
        }
        else{
            return BooleanV(rand1.get()==rand2.get());
        }
    }else{
        return BooleanV(false);
    }
    
    
} // eq?

Value Cons::evalRator(const Value &rand1, const Value &rand2) {
    return PairV(rand1,rand2);
} // cons

Value IsBoolean::evalRator(const Value &rand) {
    if(rand.get()->v_type==V_BOOL){
        return BooleanV(true);
    }else{
        return BooleanV(false);
    }
} // boolean?

Value IsFixnum::evalRator(const Value &rand) {
    if(rand.get()->v_type==V_INT){
        return BooleanV(true);
    }
    return BooleanV(false);
} // fixnum?

Value IsSymbol::evalRator(const Value &rand) {
    if(rand.get()->v_type==V_SYM){
        return BooleanV(true);
    }
    return BooleanV(false);
} // symbol?

Value IsNull::evalRator(const Value &rand) {
    if(rand.get()->v_type==V_NULL){
        return BooleanV(true);
    }
    return BooleanV(false);
} // null?

Value IsPair::evalRator(const Value &rand) {
    if(rand.get()->v_type==V_PAIR){
        return BooleanV(true);
    }
    return BooleanV(false);
} // pair?

Value IsProcedure::evalRator(const Value &rand) {
    if(rand.get()->v_type==V_PROC){
        return BooleanV(true);
    }
    return BooleanV(false);
} // procedure?

Value Not::evalRator(const Value &rand) {
    if(rand.get()->v_type==V_BOOL){
        Boolean* rand_=dynamic_cast<Boolean*>(rand.get());
        return BooleanV(!rand_->b);
    }
    return BooleanV(false);
} // not

Value Car::evalRator(const Value &rand) {
    if(rand.get()->v_type==V_PAIR){
        Pair* car_=dynamic_cast<Pair*>(rand.get());
        return car_->car;
    }   
    else{
        throw RuntimeError("not a pair2");
    }
} // car

Value Cdr::evalRator(const Value &rand) {
    if(rand.get()->v_type==V_PAIR){
        Pair* cdr_=dynamic_cast<Pair*>(rand.get());
        return cdr_->cdr;
    }   
    else{
        throw RuntimeError("not a pair1");
    }
} // cdr
