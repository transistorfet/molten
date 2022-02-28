
use crate::types::Type;
use crate::misc::UniqueID;
use crate::analysis::hir::{ MatchCase, Expr };
use crate::analysis::visitor::Visitor;

use crate::misc::{ r };
use crate::transform::transform::Transformer;
use crate::llvm::llcode::{ LLType, LLLit, LLRef, LLCC, LLCmpType, LLLink, LLExpr, LLGlobal };


pub static EXCEPTION_POINT_NAME: &str = "__ExceptionPoint__";

impl<'sess> Transformer<'sess> {
    pub fn initialize_exception_type(&mut self) {
        let global = self.session.map.get_global();

        let expoint_id = UniqueID::generate();
        global.define_type(EXCEPTION_POINT_NAME, expoint_id).unwrap();
        self.session.set_type(expoint_id, Type::Object(String::from(EXCEPTION_POINT_NAME), expoint_id, vec!()));
        self.set_type(expoint_id, LLType::Ptr(r(LLType::ExceptionPoint)));
    }

    pub fn transform_try(&mut self, code: &Expr, cases: &Vec<MatchCase>) -> Vec<LLExpr> {
        let mut exprs = vec!();

        let exp_id = UniqueID::generate();
        let expoint = self.create_exception_point(&mut exprs, exp_id);
        let expoint_id = UniqueID::generate();
        exprs.push(LLExpr::SetValue(expoint_id, r(expoint)));

        let tryblock = self.with_exception(exp_id, |transform| {
            transform.visit_node(code).unwrap()
        });


        let exret_id = UniqueID::generate();
        exprs.push(LLExpr::SetValue(exret_id, r(LLExpr::GetItem(r(LLExpr::GetLocal(exp_id)), 1))));
        let matchblock = self.create_match(LLExpr::GetValue(exret_id), cases);

        exprs.extend(self.create_exception_block(LLExpr::GetValue(expoint_id), tryblock, matchblock));
        exprs
    }

    pub fn declare_global_exception_point(&mut self, exp_id: UniqueID) {
        self.add_global(LLGlobal::DefGlobal(exp_id, LLLink::Once, String::from("__global_exception__"), LLType::ExceptionPoint, true));
    }

    pub fn create_exception_point(&mut self, exprs: &mut Vec<LLExpr>, exp_id: UniqueID) -> LLExpr {
        exprs.push(LLExpr::DefLocal(exp_id, String::from("__exception__"), LLType::ExceptionPoint, r(LLExpr::Literal(LLLit::Null(LLType::ExceptionPoint)))));
        self.init_exception_point(exprs, exp_id)
    }

    pub fn init_exception_point(&mut self, exprs: &mut Vec<LLExpr>, exp_id: UniqueID) -> LLExpr {
        let ret_id = UniqueID::generate();
        exprs.push(LLExpr::SetValue(ret_id, r(LLExpr::CallC(r(LLExpr::GetNamed("setjmp".to_string())), vec!(LLExpr::Cast(LLType::Ptr(r(LLType::I8)), r(LLExpr::GetValue(exp_id)))), LLCC::CCC))));
        LLExpr::GetValue(ret_id)
    }

    pub fn create_exception_block(&mut self, expoint: LLExpr, tryblock: Vec<LLExpr>, catch: Vec<LLExpr>) -> Vec<LLExpr> {
        let mut exprs = vec!();
        let iszero = LLExpr::Cmp(LLCmpType::Equal, r(expoint), r(LLExpr::Literal(LLLit::I32(0))));
        exprs.push(LLExpr::Phi(vec!(vec!(iszero), vec!(LLExpr::Literal(LLLit::I1(true)))), vec!(tryblock, catch)));
        exprs
    }

    pub fn transform_raise(&mut self, id: UniqueID, valexpr: &Expr) -> Vec<LLExpr> {
        let mut exprs = vec!();
        let exp_id = self.get_exception().unwrap();
        let value = self.transform_as_result(&mut exprs, valexpr);

        exprs.push(LLExpr::StoreRef(r(LLExpr::AccessRef(r(LLExpr::GetValue(exp_id)), vec!(LLRef::Field(1)))), r(LLExpr::Cast(LLType::Ptr(r(LLType::I8)), r(value)))));
        exprs.push(LLExpr::CallC(r(LLExpr::GetNamed("longjmp".to_string())), vec!(LLExpr::Cast(LLType::Ptr(r(LLType::I8)), r(LLExpr::GetValue(exp_id))), LLExpr::Literal(LLLit::I32(1))), LLCC::CCC));

        let ltype = self.transform_value_type(&self.session.get_type(id).unwrap());
        exprs.push(LLExpr::Cast(ltype.clone(), r(LLExpr::Literal(LLLit::Null(ltype)))));
        exprs
    }
}

