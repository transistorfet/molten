
use crate::misc::r;
use crate::types::Type;
use crate::misc::UniqueID;

use crate::defs::enums::EnumDefRef;

use crate::transform::transform::Transformer;
use crate::transform::functions::CFuncTransform;
use crate::llvm::llcode::{ LLType, LLLit, LLCC, LLLink, LLExpr, LLGlobal };


impl<'sess> Transformer<'sess> {
    pub fn transform_enum_def(&mut self, refid: UniqueID, name: &str) -> Vec<LLExpr> {
        let defid = self.session.get_ref(refid).unwrap();
        let selector = LLType::I8;
        let enumdef = self.session.get_def(defid).unwrap().as_enum().unwrap();

        self.add_global(LLGlobal::DefNamedStruct(defid, name.to_string(), false));
        self.set_type(defid, LLType::Alias(defid));

        let mut types = vec!();
        for (i, variant) in enumdef.variants.borrow().iter().enumerate() {
            let name = format!("{}_{}", name, variant.name);
            self.transform_enum_variant(defid, variant.id, i as i8, name, selector.clone(), variant.ttype.clone());
            if variant.ttype.is_some() {
                types.push(self.transform_value_type(variant.ttype.as_ref().unwrap()));
            }
        }

        self.add_global(LLGlobal::SetStructBody(defid, vec!(selector, LLType::Largest(types)), false));
        vec!()
    }

    pub fn transform_enum_variant(&mut self, defid: UniqueID, variant_id: UniqueID, variant: i8, name: String, selector: LLType, ttype: Option<Type>) {
        let struct_id = UniqueID::generate();
        let etype = ttype.clone().map(|t| self.transform_value_type(&t));
        self.create_enum_struct(struct_id, name.clone(), selector, etype);
        self.set_type(variant_id, LLType::Alias(struct_id));

        if ttype.is_some() {
            let ftype = self.session.get_type(variant_id).unwrap();
            let (argtypes, rettype, _) = ftype.get_function_types().unwrap();
            let lftype = CFuncTransform::transform_def_type(self, &argtypes.as_vec(), rettype);

            let mut params = vec!();
            let mut tuple_items = vec!();
            for (i, _) in argtypes.as_vec().iter().enumerate() {
                let arg_id = UniqueID::generate();
                tuple_items.push(LLExpr::GetValue(arg_id));
                params.push((arg_id, format!("value{}", i)));
            }

            let body = vec!(LLExpr::Cast(LLType::Alias(defid), r(LLExpr::DefStruct(UniqueID::generate(), self.get_type(struct_id).unwrap(), vec!(
                LLExpr::Literal(LLLit::I8(variant as i8)),
                LLExpr::DefStruct(UniqueID::generate(), self.transform_value_type(argtypes), tuple_items)
            )))));
            self.add_global(LLGlobal::DefCFunc(variant_id, LLLink::Once, name, lftype, params, body, LLCC::CCC));
        }
    }

    pub fn create_enum_struct(&mut self, id: UniqueID, name: String, selector: LLType, ltype: Option<LLType>) {
        let mut body = vec!(selector);
        if let Some(ltype) = ltype {
            body.push(ltype);
        }

        self.add_global(LLGlobal::DefNamedStruct(id, name, false));
        self.add_global(LLGlobal::SetStructBody(id, body, false));
        self.set_type(id, LLType::Alias(id));
    }

    pub fn transform_enum_resolve_constructor(&mut self, enumdef: EnumDefRef, field_id: UniqueID) -> Vec<LLExpr> {
        match enumdef.get_variant_type_by_id(field_id) {
            Some(_) => vec!(LLExpr::GetValue(field_id)),
            None => {
                let variant = enumdef.get_variant_by_id(field_id).unwrap();
                vec!(LLExpr::Cast(LLType::Alias(enumdef.id), r(LLExpr::DefStruct(UniqueID::generate(), self.get_type(field_id).unwrap(), vec!(LLExpr::Literal(LLLit::I8(variant as i8)))))))
            },
        }
    }

    pub fn transform_enum_access_method(&mut self, defid: UniqueID) -> Vec<LLExpr> {
        vec!(LLExpr::GetLocal(defid))
    }
}

