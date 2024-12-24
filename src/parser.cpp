#ifndef PARSER 
#define PARSER

// parser of myscheme 

#include "RE.hpp"
#include "Def.hpp"
#include "syntax.hpp"
#include "expr.hpp"
#include "value.hpp"
#include <map>
#include <cstring>
#include <iostream>
#include <vector>
#include <string>
#define mp make_pair
using std :: string;
using std :: vector;
using std :: pair;

extern std :: map<std :: string, ExprType> primitives;
extern std :: map<std :: string, ExprType> reserved_words;
bool check (Assoc &e,Identifier *id){
    if(find(id->s,e).get()==nullptr){
        return false;
    }
    return true;
}

Expr globalcheck(Assoc &e,vector<Syntax>stxs,Identifier *id){
        Expr lamb = stxs[0]->parse(e);
        vector<Expr>args;
        for(int i = 1 ; i<stxs.size() ; i++){
            args.push_back(stxs[i]->parse(e));
        }
        return Expr (new Apply(lamb,args));
}
Expr Syntax :: parse(Assoc &env) {
    if(dynamic_cast<Number*>(ptr.get())){
        Number *n = dynamic_cast<Number*>(ptr.get());
        return n->parse(env);
    }
    else if(dynamic_cast<Identifier*>(ptr.get())){
        Identifier *i = dynamic_cast<Identifier*>(ptr.get());
        return i->parse(env);
    }
    else if(dynamic_cast<TrueSyntax*>(ptr.get())){
        TrueSyntax *t = dynamic_cast<TrueSyntax*>(ptr.get());
        return t->parse(env);
    }
    else if(dynamic_cast<FalseSyntax*>(ptr.get())){
        FalseSyntax *f = dynamic_cast<FalseSyntax*>(ptr.get());
        return f->parse(env);
    }
    else if(dynamic_cast<List*>(ptr.get())){
        List *l = dynamic_cast<List*>(ptr.get());
        return l->parse(env);
    }
    else {
        throw RuntimeError("Invalid input1");
    }//error
}

Expr Number :: parse(Assoc &env) {
    return Expr(new Fixnum(n));
}

Expr Identifier :: parse(Assoc &env) {
        return Expr(new Var(s));
}

Expr TrueSyntax :: parse(Assoc &env) {
    return Expr(new True());
}

Expr FalseSyntax :: parse(Assoc &env) {
    return Expr(new False());
}

Expr List :: parse(Assoc &env) {
    if(dynamic_cast<Identifier*>(stxs[0].get())==nullptr) {
       
        Expr func = stxs[0]->parse(env);
        std::vector<Expr> argu;
        for(int i =1 ; i < stxs.size();i++){
            argu.push_back(stxs[i]->parse(env));
        }
        return Expr(new Apply(func,argu));
    }
        
    if(dynamic_cast<Identifier*>(stxs[0].get())){
        Expr init = stxs[0].get()->parse(env);
        Identifier *id = dynamic_cast<Identifier*>(stxs[0].get());
        if(check(env,id)){
            return globalcheck(env,stxs,id);
        }
        else if(primitives.count(id->s)!=0){  //the function belongs to "primitiveS"
             switch(primitives[id->s]){
                case E_CONS:{
                    if(stxs.size()!=3){   //error
                        throw RuntimeError("Wrong number of arguments2");
                    }
                    else{
                        Expr e1 = stxs[1]->parse(env);
                        Expr e2 = stxs[2]->parse(env);
                        return Expr(new Cons(e1,e2));
                    }
                    break;
                }
                case E_CAR:{
                    if(stxs.size()==2){
                        Expr e1 = stxs[1]->parse(env);
                        return Expr(new Car(e1));
                    }
                    else{
                        throw RuntimeError("Wrong number of arguments3");
                        //error
                    }
                    break;
                }
                case E_CDR:{
                    if(stxs.size()==2){
                        Expr e1 = stxs[1]->parse(env);
                        return Expr(new Cdr(e1));
                    }
                    else{
                        throw RuntimeError("Wrong number of arguments4");
                    }
                    break;
                }
                case E_PLUS:{
                    if(stxs.size()!=3){   //error
                        throw RuntimeError("Wrong number of arguments5");
                    }
                    else{
                        Expr e1 = stxs[1]->parse(env);
                        Expr e2 = stxs[2]->parse(env);
                        return Expr(new Plus(e1,e2));
                    }
                    break;
                }
                case E_MINUS:{ 
                    if(stxs.size()!=3){   //error
                        throw RuntimeError("Wrong number of arguments6");
                    }
                    else{
                        Expr e1 = stxs[1]->parse(env);
                        Expr e2 = stxs[2]->parse(env);
                        return Expr(new Minus(e1,e2));
                    }
                    break;
                }
                case E_MUL:{
                    if(stxs.size()==3){  
                         //error
                        Expr e1 = stxs[1]->parse(env);
                        Expr e2 = stxs[2]->parse(env);
                        return Expr(new Mult(e1,e2));
                    }
                    else{
                        throw RuntimeError("Wrong number of arguments7");
                    }
                    break;
                }case E_VOID:{
                    if(stxs.size()==1){
                        return Expr(new MakeVoid());
                    }
                    throw RuntimeError("Wrong number of arguments8");
                    break;
                }case E_EXIT:{
                    if(stxs.size()==1){
                        return Expr(new Exit());
                    }
                    throw RuntimeError("Wrong number of arguments9");
                    break;
                }
                case E_LT:{
                    if(stxs.size()!=3){   //error
                        throw RuntimeError("Wrong number of arguments10");
                    }
                    else{
                        Expr e1 = stxs[1]->parse(env);
                        Expr e2 = stxs[2]->parse(env);
                        return Expr(new Less(e1,e2));
                    }
                    break;
                }case E_GT:{
                    if(stxs.size()!=3){   //error
                        throw RuntimeError("Wrong number of arguments11");
                    }
                    else{
                        Expr e1 = stxs[1]->parse(env);
                        Expr e2 = stxs[2]->parse(env);
                        return Expr(new Greater(e1,e2));
                    }
                    break;
                }case E_LE:{
                    if(stxs.size()!=3){   //error
                        throw RuntimeError("Wrong number of arguments12");
                    }
                    else{
                        Expr e1 = stxs[1]->parse(env);
                        Expr e2 = stxs[2]->parse(env);
                        return Expr(new LessEq(e1,e2));
                    }
                    break;
                }case E_GE:{
                    if(stxs.size()!=3){   //error
                        throw RuntimeError("Wrong number of arguments13");
                    }
                    else{
                        Expr e1 = stxs[1]->parse(env);
                        Expr e2 = stxs[2]->parse(env);
                        return Expr(new GreaterEq(e1,e2));
                    }
                    break;
                }case E_EQ:{
                    if(stxs.size()!=3){   //error
                        throw RuntimeError("Wrong number of arguments14");
                    }
                    else{
                        Expr e1 = stxs[1]->parse(env);
                        Expr e2 = stxs[2]->parse(env);
                        return Expr(new Equal(e1,e2));
                    }
                    break;
                }case E_EQQ:{
                    if(stxs.size()!=3){   //error
                        throw RuntimeError("Wrong number of arguments15");
                    }
                    else{
                        Expr e1 = stxs[1]->parse(env);
                        Expr e2 = stxs[2]->parse(env);
                        return Expr(new IsEq(e1,e2));
                    }
                    break;
                }case E_INTQ:{
                    if(stxs.size()==2){
                        Expr e1 = stxs[1]->parse(env);
                        return Expr(new IsFixnum(e1));
                    }else{
                        throw RuntimeError("Wrong number of arguments16");
                    }
                    break;
                }case E_BOOLQ:{
                    if(stxs.size()==2){
                        Expr e1 = stxs[1]->parse(env);
                        return Expr(new IsBoolean(e1));
                    }else{
                        throw RuntimeError("Wrong number of arguments17");                        
                    }
                    break;
                }case E_SYMBOLQ:{
                    if(stxs.size()==2){
                        Expr e1 = stxs[1]->parse(env);
                        return Expr(new IsSymbol(e1));
                    }else{
                        throw RuntimeError("Wrong number of arguments18");                        
                    }
                    break;
                }case E_NULLQ:{
                    if(stxs.size()==2){
                        Expr e1 = stxs[1]->parse(env);
                        return Expr(new IsNull(e1));
                    }else{
                        throw RuntimeError("Wrong number of arguments19");                        
                    }
                    break;
                }case E_PAIRQ:{
                    if(stxs.size()==2){
                        Expr e1 = stxs[1]->parse(env);
                        return Expr(new IsPair(e1));
                    }else{
                        throw RuntimeError("Wrong number of arguments20");                        
                    }
                    break;
                }case E_NOT:{
                    if(stxs.size()==2){
                        Expr e1 = stxs[1]->parse(env);
                        return Expr(new Not(e1));
                    }else{
                        throw RuntimeError("Wrong number of arguments21");                         
                    }
                    break;
                }case E_PROCQ:{
                    if(stxs.size()==2){
                        Expr e1 = stxs[1]->parse(env);
                        return Expr(new IsProcedure(e1));
                    }else{
                        throw RuntimeError("Wrong number of arguments22");                        
                    }
                    break;
                }default:{
                    throw RuntimeError("Unknown expression");
                }
            }
        } 
        else if(reserved_words.count(id->s)!=0){
            switch(reserved_words[id->s]){
                case E_IF:{
                    if(stxs.size()==4){
                        Expr e1 = stxs[1]->parse(env);
                        Expr e2 = stxs[2]->parse(env);
                        Expr e3 = stxs[3]->parse(env);
                        return Expr(new If(e1,e2,e3));
                    }
                    else{
                        throw RuntimeError("Wrong number of arguments23");
                    }
                    break;
                }case E_QUOTE: {
                    if(stxs.size()==2){
                        return Expr(new Quote(stxs[1]));
                    }else{
                        throw RuntimeError("Wrong number of arguments24");
                    }
                    break;

                }
                case E_BEGIN: {
                    std::vector<Expr>es;
                    for(int i = 1;i<stxs.size();i++){
                        es.push_back(stxs[i]->parse(env));
                    }
                    return Expr(new Begin(es));
                    break;
                }case E_LETREC:{
                    if(stxs.size()!=3){
                        throw RuntimeError("Wrong number of arguments letrec1");
                    }else{
                        Assoc newe = env;
                        std::vector<std::pair<std::string,Expr>>bind_;
                        if(dynamic_cast<List*>(stxs[1].get())){
                            List*func=dynamic_cast<List*>(stxs[1].get());
                            for(auto &i:func->stxs){// 中括号
                                if(dynamic_cast<List*>(i.get())){
                                    List* lst=dynamic_cast<List*>(i.get());
                                    if(lst->stxs.size()==2){
                                        Identifier* id_ = dynamic_cast<Identifier*>(lst->stxs[0].get());
                                        std::string id = id_->s;
                                        newe = extend(id,NullV(),newe);
                                    } else{
                                        throw RuntimeError("Wrong number of arguments letrec2");
                                    }
                                } else{
                                    throw RuntimeError("Wrong number of arguments letrec3");
                                }
                            }
                            for(auto &j:func->stxs){
                                if(dynamic_cast<List*>(j.get())){
                                    List* lst2 = dynamic_cast<List*>(j.get());
                                    Identifier* id__ = dynamic_cast<Identifier*>(lst2->stxs[0].get());
                                    Expr final = (lst2->stxs[1].get())->parse(newe);
                                    bind_.push_back(std::mp(id__->s,final));
                                }
                            }
                        }
                        else throw RuntimeError("Wrong number of arguments let1");
                        return Expr (new Letrec(bind_,stxs[2]->parse(newe)));
                    }
                }case E_LET:{
                    if(stxs.size()!=3){
                        throw RuntimeError("Wrong number of arguments let2");
                    }else{
                        Assoc newe =env;
                        std::vector<std::pair<std::string,Expr>>bind_;
                        if(dynamic_cast<List*>(stxs[1].get())){
                            List*func=dynamic_cast<List*>(stxs[1].get());
                            for(auto &i:func->stxs){// 中括号
                                if(dynamic_cast<List*>(i.get())){
                                    List* lst=dynamic_cast<List*>(i.get());
                                    if(lst->stxs.size()==2){
                                        Identifier* id_ = dynamic_cast<Identifier*>(lst->stxs[0].get());
                                        std::string id = id_->s;
                                        Expr temp = (lst->stxs[1].get())->parse(env);
                                        newe = extend(id,NullV(),newe);
                                        bind_.push_back(std::mp(id,temp));
                                    } else{
                                        throw RuntimeError("Wrong number of arguments let3");
                                    }
                                } else{
                                    throw RuntimeError("Wrong number of arguments let4");;
                                }
                            }
                        }
                        else throw RuntimeError("Wrong number of arguments let5");
                        return Expr (new Let(bind_,stxs[2]->parse(newe)));
                    }
                }case E_LAMBDA:{
                    if(stxs.size()!=3){
                        throw RuntimeError("Wrong number of arguments lambda1");
                    }
                    else {
                        Assoc newe = env;
                        std::vector<std::string>vars;
                        if(dynamic_cast<List*>(stxs[1].get())){
                            List *lst = dynamic_cast<List*>(stxs[1].get());
                            for(auto &i: lst->stxs)
                            {
                                Expr exp = (i.get())->parse(env);
                                if(dynamic_cast<Var*>(exp.get())){
                                    auto var = dynamic_cast<Var*>(exp.get());
                                    vars.push_back(var->x);
                                    newe = extend(var->x,NullV(),newe);
                                } else{
                                    throw RuntimeError("Wrong number of arguments34");;
                                }
                            }
                            return Expr(new Lambda(vars,stxs[2]->parse(newe)));
                        }
                    }
                }
            }
        }else{
       
            Expr func = stxs[0]->parse(env);
            std::vector<Expr> argu;
            for(int i =1 ; i < stxs.size();i++){
                argu.push_back(stxs[i]->parse(env));
            }
            return Expr(new Apply(func,argu));
    
    
        }
    }
    
}
        // }else{
        //     auto& temp =env;
        //     while(temp.get()!=nullptr){
        //         if(id->s==temp.get()->x){
        //             std::vector<Expr>es;
        //             for(auto i:stxs){es.push_back(i->parse(env));}
        //             return Expr(new Apply(,es));
        //         }
        //         else{
        //             temp =temp.get()->next;
        //         }
        //     }
        // }  


#endif