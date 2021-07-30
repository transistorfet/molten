
use crate::misc::r;
use crate::types::Type;
use crate::hir::{ NodeID, Expr };
use crate::defs::traits::{ TraitDefRef };
use crate::visitor::{ Visitor };

use crate::transform::transform::{ CodeContext, Transformer };
use crate::transform::classes::{ VtableTransform };
use crate::llvm::llcode::{ LLType, LLLit, LLRef, LLExpr };


impl<'sess> Transformer<'sess> {
    pub fn transform_trait_def(&mut self, defid: NodeID, traitname: &str, body: &Vec<Expr>) -> Vec<LLExpr> {
        let traitdef = self.session.get_def(defid).unwrap().as_trait_def().unwrap();
        traitdef.vtable.build_vtable(self.session, body);

        VtableTransform::declare_vtable_type(self, format!("{}_vtable", traitname), &traitdef.vtable);

        let lltype = self.transform_trait_def_type(traitdef.clone());
        self.set_type(defid, lltype);

        VtableTransform::define_vtable_type(self, &traitdef.vtable);

        vec!()
    }

    pub fn transform_trait_impl(&mut self, id: NodeID, body: &Vec<Expr>) -> Vec<LLExpr> {
        let impl_id = self.session.get_ref(id).unwrap();
        let defid = self.session.get_ref(impl_id).unwrap();
        let traitdef = self.session.get_def(defid).unwrap().as_trait_def().unwrap();
        let traitimpl = self.session.get_def(impl_id).unwrap().as_trait_impl().unwrap();

        // The trait def type has already been set, and the type of the impl's vtable should be identical to the type of the def's vtable
        self.set_type(traitimpl.vtable.id, self.get_type(traitdef.vtable.id).unwrap());

        traitimpl.vtable.inherit(&traitdef.vtable);
        traitimpl.vtable.build_vtable(self.session, body);

        let tscope = self.session.map.get(&traitimpl.id);
        let mut exprs = self.with_scope(tscope, |transform| {
            transform.visit_vec(body)
        }).unwrap();

        let tscope = self.session.map.get(&traitimpl.id);
        match self.get_context() {
            Some(CodeContext::Import) =>
                exprs.extend(VtableTransform::decl_vtable(self, format!("__{}_vtable", tscope.get_basename()), &traitimpl.vtable)),
            _ =>
                exprs.extend(VtableTransform::init_vtable(self, format!("__{}_vtable", tscope.get_basename()), &traitimpl.vtable)),
        }

        exprs
    }

    pub fn transform_trait_def_type(&mut self, traitdef: TraitDefRef) -> LLType {
        let vttype = self.get_type(traitdef.vtable.id).unwrap();
        LLType::Struct(vec!(vttype, LLType::Ptr(r(LLType::I8))))
    }

    pub fn transform_unpack_trait_obj(&mut self, code: &Expr) -> Vec<LLExpr> {
        let mut exprs = vec!();
        let value = self.transform_as_result(&mut exprs, code);
        let result = self.convert_unpack_trait_obj(value);
        exprs.push(result);
        exprs
    }

    pub fn convert_pack_trait_obj(&mut self, _exprs: &mut Vec<LLExpr>, traitdef: Option<TraitDefRef>, src_type: &Type, value: LLExpr) -> LLExpr {
        let (lltype, vtable) = match traitdef {
            Some(traitdef) => {
                let lltype = self.transform_trait_def_type(traitdef.clone());
                let traitimpl = traitdef.find_impl(self.session, &src_type).unwrap();
                let vtable = LLExpr::GetLocal(traitimpl.vtable.id);
                (lltype, vtable)
            },
            None => {
                let ptr_type = LLType::Ptr(r(LLType::I8));
                (LLType::Struct(vec!(ptr_type.clone(), ptr_type.clone())), LLExpr::Literal(LLLit::Null(ptr_type)))
            },
        };

        let id = NodeID::generate();
        LLExpr::DefStruct(id, lltype, vec!(
            vtable, LLExpr::Cast(LLType::Ptr(r(LLType::I8)), r(value))
        ))
    }

    pub fn convert_unpack_trait_obj(&mut self, value: LLExpr) -> LLExpr {
        LLExpr::GetItem(r(value), 1)
    }

    pub fn transform_trait_access_method(&mut self, traitdef: TraitDefRef, objval: LLExpr, field_id: NodeID) -> Vec<LLExpr> {
        // TODO this works to get the vtable method, but the object also needs to be converted for traits, unlike with classes which takes the same object value
        let index = traitdef.vtable.get_index_by_id(field_id).unwrap();
        let vtable = LLExpr::Cast(self.get_type(traitdef.vtable.id).unwrap(), r(LLExpr::GetItem(r(objval), 0)));
        vec!(LLExpr::LoadRef(r(LLExpr::AccessRef(r(vtable), vec!(LLRef::Field(index))))))
    }

    pub fn check_transform_to_trait(&mut self, exprs: &mut Vec<LLExpr>, dest_type: &Type, src: &Expr) -> LLExpr {
        let src_type = self.session.get_type(src.id).unwrap();
        let value = self.transform_as_result(exprs, src);
        self.check_convert_to_trait(exprs, dest_type, &src_type, value)
    }

    pub fn check_convert_to_trait(&mut self, exprs: &mut Vec<LLExpr>, dest_type: &Type, src_type: &Type, value: LLExpr) -> LLExpr {
        match (&dest_type, &src_type) {
            // If the source of data is also a universal, then don't convert
            (Type::Universal(_, _), Type::Universal(_, _)) => { },
            (Type::Universal(_, id), _) => {
                let constraints = self.session.get_constraints(*id);
                let opt_traitdef = match constraints.len() > 0 {
                    true => Some(self.session.get_def(constraints[0]).unwrap().as_trait_def().unwrap()),
                    false => None,
                };
                return self.convert_pack_trait_obj(exprs, opt_traitdef, &src_type, value)
            },
            (_, Type::Universal(_, _)) => {
                return self.convert_unpack_trait_obj(value);
            },
            _ => { },
        }
        return value;
    }
}

