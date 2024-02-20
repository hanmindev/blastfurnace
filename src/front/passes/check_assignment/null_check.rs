use std::collections::HashSet;
use std::ops::{Deref, DerefMut};
use std::rc::Rc;
use crate::front::ast_types::GlobalResolvedName;

enum ParentOrUsedWhileNull {
    Parent(Box<Option<NullCheck>>),
    UsedWhileNull(HashSet<Rc<GlobalResolvedName>>),
}

pub struct NullCheck {
    not_null: HashSet<Rc<GlobalResolvedName>>,
    pouwn: ParentOrUsedWhileNull,
}

impl NullCheck {
    pub fn new() -> NullCheck {
        NullCheck {
            not_null: Default::default(),
            pouwn: ParentOrUsedWhileNull::UsedWhileNull(Default::default()),
        }
    }

    pub fn new_with_parent(parent: NullCheck) -> NullCheck {
        NullCheck {
            not_null: Default::default(),
            pouwn: ParentOrUsedWhileNull::Parent(Box::from(Some(parent))),
        }
    }

    pub fn is_null(&self, name: &Rc<GlobalResolvedName>) -> bool {
        if self.not_null.contains(name) {
            false
        } else if let ParentOrUsedWhileNull::Parent(parent) = &self.pouwn {
            parent.as_ref().as_ref().unwrap().is_null(name)
        } else {
            true
        }
    }

    pub fn confirm_not_null(&mut self, name: Rc<GlobalResolvedName>) {
        self.not_null.insert(name);
    }

    pub fn used_as_null(&mut self, name: Rc<GlobalResolvedName>) -> bool {
        if !self.is_null(&name) {
            return false;
        }

        match &mut self.pouwn {
            ParentOrUsedWhileNull::Parent(x) => { return x.as_mut().as_mut().unwrap().used_as_null(name); }
            ParentOrUsedWhileNull::UsedWhileNull(x) => { x.insert(name) }
        };
        return true;
    }

    pub fn merge_children(&mut self, mut children: Vec<HashSet<Rc<GlobalResolvedName>>>) {
        // set children that are not null in all branches to not null

        if let Some(first) = children.pop() {
            let mut intersection = first;

            for child in children {
                intersection.retain(|x| child.contains(x));

                if intersection.is_empty() {
                    return;
                }
            }
            self.not_null.extend(intersection);
        }
    }

    pub fn take_parent(&mut self) -> NullCheck {
        if let ParentOrUsedWhileNull::Parent(ref mut parent) = &mut self.pouwn {
            return parent.as_mut().take().unwrap();
        }
        panic!("NullCheck does not have a parent");
    }

    pub fn take_not_null(&mut self) -> HashSet<Rc<GlobalResolvedName>> {
        return self.not_null.drain().collect();
    }

    pub fn force_take_used_while_null(&mut self) -> HashSet<Rc<GlobalResolvedName>> {
        if let ParentOrUsedWhileNull::UsedWhileNull(x) = &mut self.pouwn {
            return x.drain().collect();
        }
        panic!("NullCheck does not have a used while null");
    }
}