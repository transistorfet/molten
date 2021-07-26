
use misc::r;
use types::Type;
use hir::{ NodeID, Expr };
use defs::{ Def };
use defs::traits::{ TraitDefRef };
use visitor::{ Visitor };

use transform::transform::{ CodeContext, Transformer };
use transform::classes::{ VtableTransform };
use llvm::llcode::{ LLType, LLRef, LLExpr };


impl<'sess> Transformer<'sess> {
    pub fn transform_trait_def_type(&mut self, traitdef: TraitDefRef) -> LLType {
        let vttype = self.get_type(traitdef.vtable.id).unwrap();
        LLType::Struct(vec!(vttype, LLType::Ptr(r(LLType::I8))))
    }

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

    pub fn transform_unpack_trait_obj(&mut self, expr: &Expr) -> Vec<LLExpr> {
        let mut exprs = vec!();

        let objval = self.transform_as_result(&mut exprs, expr);
        exprs.push(LLExpr::GetItem(r(objval), 1));
        exprs
    }

    pub fn transform_trait_access_method(&mut self, traitdef: TraitDefRef, objval: LLExpr, field_id: NodeID) -> Vec<LLExpr> {
        // TODO this works to get the vtable method, but the object also needs to be converted for traits, unlike with classes which takes the same object value
        let index = traitdef.vtable.get_index_by_id(field_id).unwrap();
        let vtable = LLExpr::GetItem(r(objval), 0);
        vec!(LLExpr::LoadRef(r(LLExpr::AccessRef(r(vtable), vec!(LLRef::Field(index))))))
    }

    // TODO how will the trait object data pointer be accessed?  Do you pass in the trait object itself into the trait implementation
    //      functions, or do you get the second element (the data) and cast and pass that into the implementation function?  The second
    //      would make more sense, since it's type should be known to the implementation-specific function, but that means the method
    //      call would need to be transformed a bit differently (the first argument is currently the same as the object that we access
    //      the method on, but we need instead to pass in the second element of that object...


    // TODO for converting from a concrete type to a trait object, perhaps you only need to check in certain places, like def, invoke, and annotations

    pub fn check_convert_to_trait(&mut self, dest_type: Type, src: &Expr) -> Vec<LLExpr> {
        let def = dest_type.get_id().map(|id| self.session.get_def(id));
        match def {
            Ok(Ok(Def::TraitDef(traitdef))) => {
                let src_type = self.session.get_type(src.id).unwrap();
                match src_type.get_id() {
                    Ok(id) if id != traitdef.id =>
                        return self.convert_to_trait(traitdef, src),
                    _ => { },
                }
            },
            _ => { },
        }
        return self.visit_node(src).unwrap();
    }

    pub fn convert_to_trait(&mut self, traitdef: TraitDefRef, expr: &Expr) -> Vec<LLExpr> {
        let mut exprs = vec!();

        let lltype = self.transform_trait_def_type(traitdef.clone());

        let expr_type = self.session.get_type(expr.id).unwrap();
        let traitimpl = traitdef.find_impl(self.session, expr_type).unwrap();
        let vtable_id = traitimpl.vtable.id;

        let id = NodeID::generate();
        let object = self.transform_as_result(&mut exprs, expr);
        exprs.push(LLExpr::DefStruct(id, lltype, vec!(
            LLExpr::GetLocal(vtable_id), LLExpr::Cast(LLType::Ptr(r(LLType::I8)), r(object))
        )));

        exprs
    }
}

