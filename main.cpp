/* Copyright (c) 2012-2017 The ANTLR Project. All rights reserved.
 * Use of this file is governed by the BSD 3-clause license that
 * can be found in the LICENSE.txt file in the project root.
 */

//
//  main.cpp
//  antlr4-cpp-demo
//
//  Created by Mike Lischke on 13.03.16.
//

#include <algorithm>
#include <any>
#include <cstdio>
#include <fstream>
#include <initializer_list>
#include <ios>
#include <iterator>
#include <sstream>
#include <string>
#include <vector>
#include <fmt/format.h>
#include <tuple>

#include "SystemVerilogParserBaseVisitor.h"
#include "SystemVerilogParserVisitor.h"
#include "antlr4-runtime.h"
#include "SystemVerilogLexer.h"
#include "SystemVerilogParser.h"
#include <fmt/core.h>
#include <fmt/ranges.h>

#define N_IFC_PARAM_NAME  "N_IFC"
#define IFC_BW_PARAM_NAME  "IFC_BW"
#define GENVAR_NAME  "idx"
#define SEL_IDX_NAME  "i_sel_idx"


struct OutEnv{
  int indentation_level;
  std::stringstream& o_string;
  OutEnv(std::stringstream& o_string):o_string(o_string),indentation_level(0){}
  OutEnv(OutEnv& ori):o_string(ori.o_string),indentation_level(ori.indentation_level+1){}
  std::stringstream& newline(){
    for (int i=0; i<indentation_level; i++) {
      o_string<<'\t';
    }
    return o_string;
  }
};
struct Expr{
  std::string content;
  Expr(){}
  Expr(std::string content):content(content){}
  Expr(const char* content):content(content){}
  std::vector<Expr*> children;
  Expr& operator+=(Expr* other){children.emplace_back(other); return *this;}
  void writeChildren(OutEnv& out) const{
    for(auto& child:children){
      (*child)(out);
    }
  }
  virtual void operator()(OutEnv& out){
    if (content!="") {
      auto iter=content.begin();
      while (iter<content.end()) {
        auto next_iter=std::find(iter,content.end(),'\n');
        out.newline()<<std::string(iter,next_iter+1);
        iter=next_iter+1;
      }      
    }
    writeChildren(out);
  }
};

struct Module:public Expr{
  std::string module_name;
  std::string param_list;
  std::vector<std::string> port_list;
  virtual void operator()(OutEnv& out) override{
    out.newline()<<"module "<<module_name<<" "<<param_list<<" (\n";
    OutEnv port_env(out);
    if(!port_list.empty()){
      auto last=port_list.back();
      port_list.pop_back();
    for(auto& port:port_list){
      port_env.newline()<<port<<",\n";
    }
    port_env.newline()<<last<<"\n";
    }
    out.o_string<<");\n\n";
    OutEnv child_env(out);
    writeChildren(child_env);
    out.newline()<<"endmodule : "<<module_name<<"\n";
  }
};

struct ForLoop:public Expr{
  std::string loop_var_name;
  std::string start;
  std::string end;
  std::string loop_id;
  virtual void operator()(OutEnv& out) override{
    out.newline()<<fmt::format(
      "for({loop_var}={start};{loop_var}<{end};{loop_var}++) begin: {loop_id}\n"
      ,fmt::arg("loop_var", loop_var_name),
      fmt::arg("start", start),
      fmt::arg("end", end),
      fmt::arg("loop_id", loop_id)
      );
    OutEnv content(out);
    writeChildren(content);
    out.newline()<<"end : "<<loop_id<<"\n";
  }
};

struct IfCond:public Expr{
  std::string cond;
  Expr true_cond;
  Expr false_cond;
  virtual void operator()(OutEnv& out) override{
    out.newline()<<fmt::format(
      "if({cond}) begin\n"
      ,fmt::arg("cond", cond));
    OutEnv true_out(out);
    true_cond(true_out);
    out.newline()<<"end else begin\n";
    OutEnv false_out(out);
    false_cond(false_out);
    out.newline()<<"end\n";
  }
};
struct Generate:public Expr{
  std::string genvar;
  virtual void operator()(OutEnv& out) override{
    if(genvar!=""){
      out.newline()<<"genvar "<<genvar<<";\n";
    }
    out.newline()<<"generate\n";
    OutEnv content(out);
    writeChildren(content);
    out.newline()<<"endgenerate\n";
  }
};


template<typename L_mod,typename R_mod>
struct Assign:public Expr{
  const std::vector<std::string>& lhs;
  const std::vector<std::string>& rhs;
  L_mod l;
  R_mod r;
  Assign(const std::vector<std::string>& lhs,const std::vector<std::string>& rhs,L_mod l,R_mod r):lhs(lhs),rhs(rhs),l(l),r(r){}
  virtual void operator()(OutEnv& out) override{
    for (size_t count=0; count<lhs.size(); count++) {
      out.newline()<<"assign "<<l(lhs[count])<<"="<<r(rhs[count])<<";\n";
    }
  }
};

template<typename L_mod,typename R_mod>
Assign<L_mod, R_mod>* make_assign_block(const std::vector<std::string>& lhs,const std::vector<std::string>& rhs,L_mod l,R_mod r){
  return new Assign<L_mod, R_mod>(lhs,rhs,l,r);
}
template<typename L_mod,typename R_mod>
struct Always_ff:public Expr{
  const std::vector<std::string>& lhs;
  const std::vector<std::string>& rhs;
  L_mod l;
  R_mod r;
  Always_ff(const std::vector<std::string>& lhs,const std::vector<std::string>& rhs,L_mod l,R_mod r):lhs(lhs),rhs(rhs),l(l),r(r){}
  virtual void operator()(OutEnv& out) override{
    out.newline()<<"always_ff @(posedge clk) begin\n";
    OutEnv content(out);
    for (size_t count=0; count<lhs.size(); count++) {
      content.newline()<<l(lhs[count])<<"<="<<r(rhs[count])<<";\n";
    }
    out.newline()<<"end\n";
  }
};
template<typename L_mod,typename R_mod>
Always_ff<L_mod, R_mod>* make_always_ff(
  const std::vector<std::string>& lhs,
  const std::vector<std::string>& rhs,
  L_mod l,
  R_mod r){
    return new Always_ff<L_mod, R_mod>(lhs,rhs,l,r);
  }
template<typename L_mod,typename R_mod>
struct Always_ff_SR:public Expr{
  const std::vector<std::string>& lhs;
  const std::vector<std::string>& rhs;
  L_mod l;
  R_mod r;
  std::string reset_val;
  Always_ff_SR(const std::vector<std::string>& lhs,const std::vector<std::string>& rhs,L_mod l,R_mod r,std::string reset_val):lhs(lhs),rhs(rhs),l(l),r(r),reset_val(reset_val){}
  virtual void operator()(OutEnv& out) override{
    out.newline()<<"always_ff @(posedge clk) begin\n";
    OutEnv content(out);
    content.newline()<<"if (rst) begin\n";
    OutEnv reset_env(content);
    for (size_t count=0; count<lhs.size(); count++) {
      reset_env.newline()<<l(lhs[count])<<"<="<<reset_val<<";\n";
    }
    content.newline()<<"end else begin\n";
    OutEnv other_env(content);
    for (size_t count=0; count<lhs.size(); count++) {
      other_env.newline()<<l(lhs[count])<<"<="<<r(rhs[count])<<";\n";
    }
    content.newline()<<"end\n";
    out.newline()<<"end\n";
  }
};
template<typename L_mod,typename R_mod>
Always_ff_SR<L_mod, R_mod>* make_always_ff_sr(
  std::vector<std::string>& lhs,
  std::vector<std::string>& rhs,
  L_mod l,
  R_mod r,
  std::string reset_val){
    return new Always_ff_SR<L_mod, R_mod>(lhs,rhs,l,r,reset_val);
  }
using namespace fmt::literals;
using namespace antlrcpptest;
using namespace antlr4;
struct IdentifierAndUnpackedDim{
  std::string identifier;
  std::string unpacked_dim;
  IdentifierAndUnpackedDim(SystemVerilogParser::Net_decl_assignmentContext* ctx)
  :identifier(ctx->net_identifier()->getText())
  {
    for (auto dims : ctx->unpacked_dimension()) {
      unpacked_dim+=dims->getText();
    }
  }
  IdentifierAndUnpackedDim(SystemVerilogParser::Variable_decl_assignmentContext *ctx){
    if(!ctx->variable_identifier()){
      fprintf(stderr, "unsupported on line %zu, %s\n",ctx->getStart()->getLine(),ctx->getText().c_str());
    }
    identifier=ctx->variable_identifier()->getText();
    for (auto dims : ctx->variable_dimension()) {
      unpacked_dim+=dims->getText();
    }
  }
};

template <> struct fmt::formatter<IdentifierAndUnpackedDim> {
  bool pipe=false;
  constexpr auto parse(fmt::format_parse_context& ctx) -> decltype(ctx.begin()) {
    auto it = ctx.begin(), end = ctx.end();
    if (it != end){
      switch (*it++) {
        case 'p': pipe=true;break;
        case '}': return it-1;
      }
    }
    if (it != end && *it != '}') throw fmt::format_error("invalid format");
    return it;
  }
  template <typename FormatContext>
  auto format(const IdentifierAndUnpackedDim& p, FormatContext& ctx) const -> decltype(ctx.out()) {
    // ctx.out() is an output iterator to write to.
    return  fmt::format_to(
      ctx.out(), pipe?"{ident}_pipe{unpacked_dim}":"{ident}[{N_IFC_PARAM_NAME}-1:0]{unpacked_dim}", 
      fmt::arg("ident", p.identifier),
      "N_IFC_PARAM_NAME"_a=N_IFC_PARAM_NAME,
      fmt::arg("unpacked_dim", p.unpacked_dim));
  }
};
struct NetDeclInfo{
  std::string net_type;
  std::vector<IdentifierAndUnpackedDim> identifiers;
  std::string data_type;
};
template <> struct fmt::formatter<NetDeclInfo> {
  std::string fmt_spec;
  auto parse(fmt::format_parse_context& ctx) -> decltype(ctx.begin()) {
    auto end_iter=std::find(ctx.begin(),ctx.end(),'}');
    fmt_spec=std::string(ctx.begin(),end_iter);
    return end_iter;
  }
  template <typename FormatContext>
  auto format(const NetDeclInfo& p, FormatContext& ctx) const -> decltype(ctx.out()) {
    // ctx.out() is an output iterator to write to.
    return  fmt::format_to(
      ctx.out(), "{net_type} {data_type} {vars:"+ fmt_spec+"};", 
      fmt::arg("net_type", p.net_type),
      fmt::arg("data_type", p.data_type),
      fmt::arg("vars", fmt::join(p.identifiers,",")));
  }
};
struct ModPortInfo{
  std::string identifier;
  std::vector<std::string> in_ports;
  std::vector<std::string> out_ports;
};
bool need_reset(std::string identifier){
  return identifier.find("ready")!=std::string::npos||identifier.find("valid")!=std::string::npos;
}
std::string join(std::string delim,std::initializer_list<std::string> to_join){
  std::string result;
  if (to_join.size()) {
    auto iter=to_join.begin();
    result=*iter;
    iter++;
    for(;iter<to_join.end();iter++){
      result+=(delim+*iter);
    }
  }
  return result;
}
class InterfaceVisitor: public SystemVerilogParserBaseVisitor{
  std::string ifc_name;
  std::string param_decl;
  std::vector<NetDeclInfo> elements;
  std::vector<std::string> local_param_decl;
  std::vector<ModPortInfo> mod_ports;

  std::vector<std::string> find_need_pipe(const ModPortInfo& i_mod){
    std::vector<std::string> out;
    for (const auto& port : i_mod.in_ports) {
      if(!need_reset(port)){
        out.emplace_back(port);
      }
    }
    return out;
  }
  std::vector<NetDeclInfo> filter_need_pipe(std::vector<std::string> & need_pipe){
    std::unordered_set<std::string> identifier(need_pipe.begin(),need_pipe.end());
    std::vector<NetDeclInfo> out;
    for(const auto & temp: elements){
      decltype(temp.identifiers) to_get;
      for(const auto& id:temp.identifiers){
        if(identifier.count(id.identifier)){
          to_get.emplace_back(id);
        }
      }
      if(!to_get.empty()){
        auto this_type=temp;
        this_type.identifiers=std::move(to_get);
        out.emplace_back(std::move(this_type));
      }
    }
    return out;
  }

  std::string produce_mux(const ModPortInfo& i_mod,const ModPortInfo& o_mod ){
    Module mux_module;
    mux_module.module_name=join("_", {ifc_name,i_mod.identifier,o_mod.identifier,"mux"});
    mux_module.param_list=
      (param_decl==""?"#(":param_decl.substr(0, param_decl.size()-1))
      +",parameter " N_IFC_PARAM_NAME "=16,REGISTERED=1,localparam " IFC_BW_PARAM_NAME "=$clog2(" N_IFC_PARAM_NAME "))";
    mux_module.port_list={
      "input clk",
      "input rst",
      "input [" IFC_BW_PARAM_NAME"-1:0] " SEL_IDX_NAME,
      ifc_name+"."+i_mod.identifier +" i [" N_IFC_PARAM_NAME "-1:0]",
      ifc_name+"."+o_mod.identifier +" o"
      };
    mux_module+=new Expr(fmt::format("{}",fmt::join(local_param_decl,"\n")));
    mux_module+=new Expr("\n");
    mux_module+=new Expr(fmt::format("{}",fmt::join(elements,"\n")));
    mux_module+=new Expr("\n");
    //mux_module+=fmt::format("{:p}",fmt::join(filter_need_pipe(i_mod),"\n"));
    //mux_module+="\n";

    Generate* gen_block=new Generate;
    gen_block->genvar=GENVAR_NAME;

    ForLoop *assign_for_loop=new ForLoop;
    assign_for_loop->loop_var_name=GENVAR_NAME;
    assign_for_loop->start="0";
    assign_for_loop->end=N_IFC_PARAM_NAME;
    assign_for_loop->loop_id="mux_prep";

    (*assign_for_loop)+=make_assign_block(i_mod.in_ports, i_mod.in_ports, 
    [](const std::string& in){
      return in+"[" GENVAR_NAME "]";
    }
    , 
    [](const std::string& in){
      return "i[" GENVAR_NAME "]."+in;
    }
    );

    (*gen_block)+=assign_for_loop;
    IfCond* out_block=new IfCond;
    out_block->cond="REGISTERED";
    std::vector<std::string> id_no_reset;
    std::vector<std::string> id_need_reset;
    for (auto& identifier : i_mod.in_ports) {
      if(need_reset(identifier)){
        id_need_reset.emplace_back(identifier);
      }else {
        id_no_reset.emplace_back(identifier);
      }
    }
    auto out_block_lhs=[](const std::string& in){
      return std::string("o")+"."+in;
    };
    auto out_block_rhs=[](const std::string& in){
      return in+"[" SEL_IDX_NAME "]";
    };
    out_block->true_cond+=make_always_ff(id_no_reset, id_no_reset, 
    out_block_lhs,out_block_rhs);

    out_block->true_cond+=make_always_ff_sr(id_need_reset, id_need_reset, out_block_lhs,out_block_rhs,"0");
    out_block->false_cond+=make_assign_block(i_mod.in_ports,i_mod.in_ports, out_block_lhs,out_block_rhs);
    (*gen_block)+=out_block;
    std::stringstream string_out;
    mux_module+=gen_block;
    OutEnv outenv(string_out);
    mux_module(outenv);
    return string_out.str();
  }
  std::any visitDescription(SystemVerilogParser::DescriptionContext *ctx) override {
    auto ifc_spec=ctx->interface_declaration();
    if (ifc_spec) {
      elements.clear();
      local_param_decl.clear();
      mod_ports.clear();
      visit(ifc_spec);
      std::cout<<produce_mux(mod_ports[0].identifier=="slave"?mod_ports[0]:mod_ports[1], mod_ports[0].identifier=="master"?mod_ports[0]:mod_ports[1]);

    }
    return 0;
  }
  std::any visitModport_item(SystemVerilogParser::Modport_itemContext *ctx) override {
    ModPortInfo this_info;
    this_info.identifier=ctx->modport_identifier()->getText();
    for (auto port_decl : ctx->modport_ports_declaration()) {
      if (!port_decl->modport_simple_ports_declaration()) {
        fprintf(stderr, "unsuported at line %zu: %s\n", port_decl->start->getLine(),port_decl->getText().c_str());
        continue;
      }
      auto port_content=port_decl->modport_simple_ports_declaration();
      if (!(port_content->port_direction()->INPUT()||port_content->port_direction()->OUTPUT())) {
        fprintf(stderr, "unsuported at line %zu: %s\n", port_decl->start->getLine(),port_decl->getText().c_str());
        continue;
      }
      std::vector<std::string> & dir=port_content->port_direction()->INPUT()?this_info.in_ports:this_info.out_ports;
      for (auto id : port_content->modport_simple_port()) {
        dir.emplace_back(id->port_identifier()->getText());
      }
    }
    mod_ports.push_back(this_info);
    return 0;
  }
  std::any visitInterface_identifier(SystemVerilogParser::Interface_identifierContext *ctx) override {
    ifc_name=ctx->getText();
    return 0;
  }
  std::any visitParameter_port_list(SystemVerilogParser::Parameter_port_listContext *ctx) override{
    param_decl=ctx->getText();
    return 0;
  }
  std::any visitLocal_parameter_declaration(SystemVerilogParser::Local_parameter_declarationContext *ctx) override {
    local_param_decl.emplace_back(ctx->getText());
    return 0;
  }
  std::any visitNet_declaration(SystemVerilogParser::Net_declarationContext *ctx) override {
    NetDeclInfo this_info;
    if(ctx->INTERCONNECT()){
      fprintf(stderr, "unsupported: %s\n", ctx->getText().c_str());
      return 0;
    }
    if (ctx->net_type()) {
      this_info.net_type=ctx->net_type()->getText();
    }
    if(ctx->data_type_or_implicit()){
      this_info.data_type=ctx->data_type_or_implicit()->getText();
    }
    this_info.identifiers=std::any_cast<typeof this_info.identifiers>(visit(ctx->list_of_net_decl_assignments()));
    elements.emplace_back(std::move(this_info));
    return 0;
  }
  std::any visitData_declaration(SystemVerilogParser::Data_declarationContext *ctx) override  {
    NetDeclInfo this_info;
    if(ctx->type_declaration()||ctx->package_import_declaration()||ctx->net_type_declaration()){
      fprintf(stderr, "unsupported: %s\n", ctx->getText().c_str());
      return 0;
    }
    if(ctx->data_type_or_implicit()){
      this_info.data_type=ctx->data_type_or_implicit()->getText();
    }
    this_info.identifiers=std::any_cast<typeof this_info.identifiers>(visit(ctx->list_of_variable_decl_assignments()));
    elements.emplace_back(std::move(this_info));
    return 0;
  }
  std::any visitList_of_variable_decl_assignments(SystemVerilogParser::List_of_variable_decl_assignmentsContext *ctx) override {
    std::vector<IdentifierAndUnpackedDim> eles;
    eles.reserve(ctx->variable_decl_assignment().size());
    for(auto id:ctx->variable_decl_assignment()){
      eles.emplace_back(id);
    }
    return eles;
  }
  std::any visitVariable_decl_assignment(SystemVerilogParser::Variable_decl_assignmentContext *ctx) override {
    return visitChildren(ctx);
  }
  std::any visitList_of_net_decl_assignments(SystemVerilogParser::List_of_net_decl_assignmentsContext *ctx) override {
    std::vector<IdentifierAndUnpackedDim> eles;
    eles.reserve(ctx->net_decl_assignment().size());
    for(auto id:ctx->net_decl_assignment()){
      eles.emplace_back(id);
    }
    return eles;
  }

};
int main(int argc , const char ** argv) {
  std::fstream in_file(argv[1],std::ios_base::in);
  ANTLRInputStream input(in_file);
  SystemVerilogLexer lexer(&input);
  CommonTokenStream tokens(&lexer);
  tokens.fill();
  SystemVerilogParser parser(&tokens);
  tree::ParseTree* tree = parser.source_text();
  InterfaceVisitor visitor;
  visitor.visitChildren(tree);
  return 0;
}
