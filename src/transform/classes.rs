
use hir::{ NodeID, Expr, ExprKind };
use visitor::{ Visitor };

use defs::classes::{ ClassDefRef, StructDef, Vtable };

use misc::{ r };
use transform::transform::{ Transformer };
use transform::llcode::{ LLType, LLRef, LLLink, LLExpr, LLGlobal };


impl<'sess> Transformer<'sess> {
    pub fn transform_class_type_data(&mut self, classdef: ClassDefRef, body: &Vec<Expr>) {
        classdef.build_vtable(self.session, body);
        classdef.build_structdef(self.session, body);

        StructTransform::declare_struct(self, classdef.id, classdef.classname.clone());

        VtableTransform::declare_vtable(self, format!("{}_vtable", classdef.classname.clone()), &classdef.vtable);

        // The struct definition refers to the type of the vtable, so the vtable needs to be declared before the struct body can be defined
        StructTransform::define_struct(self, classdef.id, &classdef.structdef);

        VtableTransform::define_vtable(self, &classdef.vtable);
    }


    pub fn transform_class_vtable_init(&mut self, classdef: ClassDefRef) -> Vec<LLExpr> {
        if !classdef.has_vtable() {
            return vec!();
        }

        let tscope = self.session.map.get(&classdef.id);
        VtableTransform::init_vtable(self, format!("__{}_vtable", tscope.get_basename()), &classdef.vtable)
    }


    pub fn transform_class_body(&mut self, id: NodeID, body: &Vec<Expr>) -> Vec<LLExpr> {
        let defid = self.session.get_ref(id).unwrap();
        let mut exprs = vec!();
        let tscope = self.session.map.get(&defid);
        let classdef = self.session.get_def(defid).unwrap().as_class().unwrap();

        self.transform_class_type_data(classdef.clone(), body);

        self.with_scope(tscope, |transform| {
            for node in body {
                match &node.kind {
                    ExprKind::Function(vis, ident, args, _, body, abi) => {
                        exprs.extend(transform.transform_func_def(*abi, node.id, *vis, ident.as_ref().map(|ident| &ident.name), args, body));
                    },
                    ExprKind::Declare(vis, ident, _) => {
                        let ttype = transform.session.get_type_from_ref(node.id).unwrap();
                        exprs.extend(transform.transform_func_decl(ttype.get_abi().unwrap(), node.id, *vis, &ident.name, &ttype));
                    },
                    ExprKind::Definition(_, _, _, _) => { },
                    _ => panic!("Not Implemented: {:?}", node),
                }
            }
            Ok(vec!())
        }).unwrap();

        exprs.extend(self.transform_class_vtable_init(classdef));
        exprs
    }

}


pub struct StructTransform;

impl StructTransform {
    pub fn declare_struct(transform: &mut Transformer, id: NodeID, name: String) {
        transform.add_global(LLGlobal::DefNamedStruct(id, name, true));
        transform.set_type(id, LLType::Alias(id));
    }

    pub fn define_struct(transform: &mut Transformer, id: NodeID, structdef: &StructDef) {
        let mut items = vec!();
        structdef.foreach_field(|_, _, ttype| {
            items.push(transform.transform_value_type(ttype));
        });
        transform.add_global(LLGlobal::SetStructBody(id, items, true));
    }

    pub fn get_type(transform: &mut Transformer, structdef: &StructDef) -> LLType {
        let mut items = vec!();
        structdef.foreach_field(|_, _, ttype| {
            items.push(transform.transform_value_type(ttype));
        });
        LLType::Struct(items)
    }
}


pub struct VtableTransform;

impl VtableTransform {
    pub fn declare_vtable(transform: &mut Transformer, name: String, vtable: &Vtable) {
        transform.add_global(LLGlobal::DefNamedStruct(vtable.id, name, true));
        transform.set_type(vtable.id, LLType::Alias(vtable.id));
    }

    pub fn define_vtable(transform: &mut Transformer, vtable: &Vtable) {
        let mut items = vec!();
        vtable.foreach_entry(|_, _, ttype| {
            items.push(transform.transform_value_type(ttype));
        });
        transform.add_global(LLGlobal::SetStructBody(vtable.id, items, true));
    }

    pub fn init_vtable(transform: &mut Transformer, name: String, vtable: &Vtable) -> Vec<LLExpr> {
        let mut exprs = vec!();

        transform.add_global(LLGlobal::DefGlobal(vtable.id, LLLink::Once, name, transform.get_type(vtable.id).unwrap()));
        // TODO should vtables be dynamically allocated, or should we add a LLType::ElementOf() type or something to GetElement an aliased type
        exprs.push(LLExpr::SetGlobal(vtable.id, r(LLExpr::AllocRef(NodeID::generate(), transform.get_type(vtable.id).unwrap(), None))));
        vtable.foreach_enumerated(|i, id, _, ttype| {
            let ltype = transform.transform_value_type(ttype);
            let field = LLExpr::AccessRef(r(LLExpr::GetGlobal(vtable.id)), vec!(LLRef::Field(i)));
            exprs.push(LLExpr::StoreRef(r(field), r(LLExpr::Cast(ltype, r(LLExpr::GetValue(id))))));
            //exprs.push(LLExpr::SetItem(r(LLExpr::GetLocal(vtable.id)), i, r(LLExpr::Cast(ltype, r(LLExpr::GetValue(id))))));
        });

        exprs
    }
}

