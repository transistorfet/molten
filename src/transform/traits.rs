
use crate::misc::r;
use crate::types::Type;
use crate::misc::UniqueID;
use crate::defs::traits::TraitDefRef;
use crate::analysis::hir::Expr;
use crate::analysis::visitor::Visitor;

use crate::transform::transform::{ CodeContext, Transformer };
use crate::transform::classes::VtableTransform;
use crate::llvm::llcode::{ LLType, LLLit, LLRef, LLExpr };


impl<'sess> Transformer<'sess> {
    pub fn transform_trait_def(&mut self, defid: UniqueID, traitname: &str, body: &Vec<Expr>) -> Vec<LLExpr> {
        let traitdef = self.session.get_def(defid).unwrap().as_trait_def().unwrap();
        traitdef.vtable.build_vtable(self.session, body);

        VtableTransform::declare_vtable_type(self, format!("{}_vtable", traitname), &traitdef.vtable);

        let lltype = self.transform_trait_def_type(traitdef.clone());
        self.set_type(defid, lltype);

        VtableTransform::define_vtable_type(self, &traitdef.vtable);

        vec!()
    }

    pub fn transform_trait_impl(&mut self, refid: UniqueID, body: &Vec<Expr>) -> Vec<LLExpr> {
        let impl_id = self.session.get_ref(refid).unwrap();
        let traitimpl = self.session.get_def(impl_id).unwrap().as_trait_impl().unwrap();
        let traitdef = self.session.get_def(traitimpl.trait_id).unwrap().as_trait_def().unwrap();

        // The trait def type has already been set, and the type of the impl's vtable should be identical to the type of the def's vtable
        self.set_type(traitimpl.vtable.id, self.get_type(traitdef.vtable.id).unwrap());

        traitimpl.vtable.inherit(&traitdef.vtable);
        traitimpl.vtable.build_vtable(self.session, body);

        let tscope = self.session.map.get(traitimpl.id).unwrap();
        let mut exprs = self.with_scope(tscope, |transform| {
            transform.visit_vec(body)
        }).unwrap();

        let tscope = self.session.map.get(traitimpl.id).unwrap();
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

    pub fn convert_pack_trait_obj(&mut self, _exprs: &mut Vec<LLExpr>, traitdef: Option<TraitDefRef>, src_type: &Type, value: LLExpr) -> LLExpr {
        let (lltype, vtable) = match traitdef {
            Some(traitdef) => {
                let lltype = self.transform_trait_def_type(traitdef.clone());
                let traitimpl = traitdef.find_impl(self.session, src_type).unwrap();
                let vtable = LLExpr::GetLocal(traitimpl.vtable.id);
                (lltype, vtable)
            },
            None => {
                let ptr_type = LLType::Ptr(r(LLType::I8));
                (LLType::Struct(vec!(ptr_type.clone(), ptr_type.clone())), LLExpr::Literal(LLLit::Null(ptr_type)))
            },
        };

        let id = UniqueID::generate();
        LLExpr::DefStruct(id, lltype, vec!(
            vtable,
            LLExpr::Cast(LLType::Ptr(r(LLType::I8)), r(LLExpr::PackUniversal(LLType::Ptr(r(LLType::I8)), r(value))))
        ))
    }

    pub fn convert_unpack_trait_obj(&mut self, dest_type: &Type, value: LLExpr) -> LLExpr {
        LLExpr::UnpackUniversal(self.transform_value_type(dest_type), r(LLExpr::GetItem(r(value), 1)))
    }

    pub fn transform_trait_access_method(&mut self, traitdef: TraitDefRef, objval: LLExpr, field_id: UniqueID) -> Vec<LLExpr> {
        // TODO this works to get the vtable method, but the object also needs to be converted for traits, unlike with classes which takes the same object value
        let index = traitdef.vtable.get_index_by_id(field_id).unwrap();
        let vtable = LLExpr::Cast(self.get_type(traitdef.vtable.id).unwrap(), r(LLExpr::GetItem(r(objval), 0)));
        vec!(LLExpr::LoadRef(r(LLExpr::AccessRef(r(vtable), vec!(LLRef::Field(index))))))
    }

    pub fn convert_to_trait_universal(&mut self, exprs: &mut Vec<LLExpr>, dest_type: &Type, src_type: &Type, value: LLExpr) -> LLExpr {
        let id = dest_type.get_id().unwrap();
        let constraints = self.session.get_constraints(id);
        let opt_traitdef = match !constraints.is_empty() {
            true => Some(self.session.get_def(constraints[0]).unwrap().as_trait_def().unwrap()),
            false => None,
        };

        self.convert_pack_trait_obj(exprs, opt_traitdef, src_type, value)
    }

    pub fn convert_from_trait_universal(&mut self, _exprs: &mut Vec<LLExpr>, dest_type: &Type, value: LLExpr) -> LLExpr {
        self.convert_unpack_trait_obj(dest_type, value)
    }

    pub fn convert_impl_func_args(&mut self, func_id: UniqueID, fargs: &mut Vec<(UniqueID, String)>, body: &mut Vec<LLExpr>) {
        let traitfunc = self.session.get_def(func_id).unwrap().as_trait_func().unwrap();
        let deftype = self.session.get_type(traitfunc.def_func_id.borrow().unwrap()).unwrap();
        let trait_id = traitfunc.trait_id;

        for (arg, argtype) in fargs.iter_mut().zip(deftype.get_argtypes().unwrap().as_vec()) {
            if argtype.get_id() == Ok(trait_id) {
                let converted_id = arg.0;
                arg.0 = UniqueID::generate();
                let dest_type = self.session.get_type(converted_id).unwrap();
                body.insert(0, LLExpr::SetValue(converted_id, r(self.convert_unpack_trait_obj(&dest_type, LLExpr::GetValue(arg.0)))));
            }
        }

        let rettype = deftype.get_rettype().unwrap();
        if rettype.get_id() == Ok(trait_id) {
            let impltype = self.session.get_type(func_id).unwrap().get_rettype().unwrap().clone();
            let traitdef = self.session.get_def(trait_id).unwrap().as_trait_def().unwrap();
            let last = body.pop().unwrap();
            let value = self.convert_pack_trait_obj(body, Some(traitdef), &impltype, last);
            body.push(value);
        }

        self.session.set_type(func_id, deftype);
    }
}

