package Assignment3.SourceHelpers
import scala.collection.immutable.ListMap
import Assignment3.Utility.Utility._

import Assignment3.Source.Source._

/*
* Beware, brave adventurer, for here be dragons!
*/


object SourceHelpers {
  type AliasEnv = ListMap[Variable, Type]

  // -- Resolve type variables
  def resolveTypeAliases(e: Expr): Expr = {
    def resolveTypeAliasesInner(env: ListMap[TypeVariable, Type], e: Expr): Expr = {
      def rtai(e: Expr): Expr = { resolveTypeAliasesInner(env, e) }
      def resolveFields(fields: ListMap[Label, Expr]): ListMap[Label, Expr] = {
        fields.map((entry: (Label, Expr)) => {
          val (lbl, field) = entry;
          (lbl, rtai(field))
        })
      }

      e match {
        case Var(x) => Var(x)
        case Func(params, body) => {
          val resolvedParams =
            params.map((entry: (Variable, Type)) => {
            val (v, ann) = entry;
            (v, resolveTypeSynonyms(env, ann))
          });
          val resolvedBody = rtai(body)
          Func(resolvedParams, resolvedBody)
        }
        case Apply(e, args) => {
          val resolvedF = rtai(e);
          val resolvedArgs = args.map((arg: Expr) => rtai(arg))
          Apply(resolvedF, resolvedArgs)
        }
        case SelectField(o, lbl) => SelectField(rtai(o), lbl)
        case MethodUpdate(o, lbl, sv, ann, newBody) =>
          MethodUpdate(rtai(o), lbl, sv, resolveTypeSynonyms(env, ann), rtai(newBody))
        case FieldUpdate(o, lbl, newBody) => FieldUpdate(rtai(o), lbl, rtai(newBody))
        case New(e) => New(rtai(e))
        case RootClass => RootClass
        case TypeIn(tv, ann, e) => {
          val resolved = resolveTypeSynonyms(env, ann)
          resolveTypeAliasesInner(env + (tv -> resolved), e)
        }
        case LetIn(x, e1, e2) => LetIn(x, rtai(e1), rtai(e2))
        case Object(sb, sty, fields) => {
          val resolvedFields = resolveFields(fields);
          Object(sb, resolveTypeSynonyms(env, sty), resolvedFields)
        }
        case Class(sb, sty, extendsData, fields, overridden) => {
          // yawn
          val resolvedSty = resolveTypeSynonyms(env, sty)
          val resolvedExtendsData = extendsData match {
            case Some((name, ann)) => {
              Some((name, resolveTypeSynonyms(env, ann)))
            }
            case None => None
          }
          val resolvedFields = resolveFields(fields);
          val resolvedOverrides = resolveFields(overridden);
          Class(sb, resolvedSty, resolvedExtendsData,
            resolvedFields, resolvedOverrides)
        }
        case IfThenElse(e1, e2, e3) => IfThenElse(rtai(e1), rtai(e2), rtai(e3))
        case BinOp(op, e1, e2) => BinOp(op, rtai(e1), rtai(e2))
        case NotOp(e) => NotOp(rtai(e))
        case Bool(b) => Bool(b)
        case Num(n) => Num(n)
        case Str(s) => Str(s)
      }
    }
    resolveTypeAliasesInner(ListMap(): ListMap[TypeVariable, Type], e)
  }

  def resolveTypeSynonyms(typeEnv: AliasEnv, ann: Type): Type = {
    def resolveTypeSynonymsInner(
      typeEnv: AliasEnv,
      boundVariables: Set[TypeVariable],
      ann: Type): Type =
      ann match {
        case TyVar(atv) => {
          // Only swap if the variable is free
          if (boundVariables.contains(atv)) {
            TyVar(atv)
          } else {
            if (typeEnv.contains(atv)) {
              typeEnv(atv)
            } else {
              sys.error("Error: Undefined type alias " + atv)
            }
          }
        }

        case TyObject(bnd, fields) => {
          val bvs = boundVariables + bnd;
          val resolvedFields = fields.map((entry: (Label, Type)) => {
              val (lbl, ann) = entry;
              (lbl, resolveTypeSynonymsInner(typeEnv, bvs, ann))
            }
          )
          TyObject(bnd, resolvedFields)
        }

        case TyFun(params, ret) => {
          val resolvedParams =
            params.map((pty: Type) =>
              resolveTypeSynonymsInner(typeEnv, boundVariables, pty));
          val resolvedRet = resolveTypeSynonymsInner(typeEnv, boundVariables, ret);
          TyFun(resolvedParams, resolvedRet)

        }

        case TyClass(ty) => TyClass(resolveTypeSynonymsInner(typeEnv, boundVariables, ty))
        case TyTop => TyTop
        case TyBool => TyBool
        case TyInt => TyInt
        case TyString => TyString
      }

      resolveTypeSynonymsInner(typeEnv, Set(): Set[TypeVariable], ann)
    }


  // -- Type well-formedness rules
  def assertWellFormed(ty: Type): Unit = {
    def awfInner(env: ListMap[TypeVariable, Type], ty: Type): Unit = ty match {
      case TyVar(tv) => {
        if (!env.contains(tv)) {
          sys.error("Error: ill-formed type; type variable " + tv + " is unbound")
        }
      }
      case TyClass(oTy) => awfInner(env, oTy)
      case TyFun(paramTys, resTy) => {
        paramTys.foreach((ty: Type) => awfInner(env, ty));
        awfInner(env, resTy)
      }
      case TyObject(bnd, fields) => {
        // ListMaps ensure that labels are unique. But we need to ensure this when constructing
        // the ListMap -- otherwise we will get odd failures!
        val chkEnv = env + (bnd -> TyTop)
        fields.foreach((entry: (Label, Type)) => awfInner(chkEnv, entry._2))
      }
      case _ => () // Base types / top are all fine
    }

    awfInner(new ListMap(), ty)
  }

  // -- Subtyping rules
  def subtypeOf(ty1: Type, ty2: Type): Boolean = {
    def failwith(msg: String): Boolean = {
      false
      //Console.err.println(msg); false
    }
    // Reflexivity: equal types are subtypes of each other
    if (equivTypes(ty1, ty2)) {
      true
    } else {
      ty2 match {
        case TyVar(_) =>
          sys.error("ERROR: ty2 *cannot* be a type variable when checking subtyping! Got: " + ty2.toString)
        case _ => ()
      };

      // Otherwise we need to proceed by cases
      (ty1, ty2) match {

        case (TyFun(args1, res1), TyFun(args2, res2)) => {
          // Functions are covariant in their return type
          val resTyResult = subtypeOf(res1, res2)
          if (!resTyResult) {
            failwith("Error in subtyping function: " + res2.toString
              + " is not a subtype of " + res1.toString)
          } else {
            // ...but contravariant in their argument types
            val argTysResult =
              args1.zip(args2)
                .forall((entry: (Type, Type)) => subtypeOf(entry._2, entry._1));

            if (!argTysResult) {
              failwith("Error in subtyping function: " + args2.toString +
                " are not pointwise subtypes of " + args1.toString)
            } else {
              true; // If both checks pass, the subtyping relation holds
            }
          }
        }

        case (TyObject(bnd1, fields1), TyObject(bnd2, fields2)) => {
          // OK, this is the fun one!
          // Firstly, both objects must be well-formed types
          assertWellFormed(ty1);
          assertWellFormed(ty2);

          // Next, need to check that labels of fields2 are a subset of labels of fields1
          if (fields2.keySet.subsetOf(fields1.keySet)) {
            // Substitute A' for X' in superFieldTy, then check *equivalence* (as we don't
            // support depth subtyping)
            fields2.forall((entry: (Label, Type)) => {
              val (superFieldLbl, superFieldTy) = entry;
              val selfFieldTy = fields1(superFieldLbl);
              val substTy2 = typeSubst(superFieldTy, ty2, bnd2);
              equivTypes(selfFieldTy, substTy2)
              //println("superFieldTy: " + superFieldTy.toString + ", bnd2: " + bnd2 + ", substTy2: " + substTy2.toString);
            })
          } else {
            failwith("Error subtyping object: " + fields2.keySet.toString +
              " is not a subset of " + fields1.keySet.toString)
          }
        }

        case (ty1, ty2) =>
          failwith("Error: type " + ty1.toString +
          " is not a subtype of " + ty2.toString + " as they are structurally different.")
      }
    }
  }

  def typeSwap(ty: Type, tv1: TypeVariable, tv2: TypeVariable): Type = ty match {
    case TyVar(v) => TyVar(swapVar(v, tv1, tv2))
    case TyClass(ty) => TyClass(typeSwap(ty, tv1, tv2))
    case TyObject(tv, fields) => {
      val swappedFields = fields.map((entry: (Label, Type)) => {
        val (lbl, ty) = entry;
        (lbl, typeSwap(ty, tv1, tv2))
      });
      TyObject(swapVar(tv, tv1, tv2), swappedFields)
    }
    case TyFun(paramTys, resTy) => {
      val swappedParams = paramTys.map ((ty: Type) => typeSwap(ty, tv1, tv2));
      TyFun(swappedParams, typeSwap(resTy, tv1, tv2))
    }
    case TyTop => TyTop
    case TyBool => TyBool
    case TyInt => TyInt
    case TyString => TyString

  }

  // Identify types up to alpha-equivalence
  def equivTypes(ty1: Type, ty2: Type): Boolean = {
    if (ty1 == ty2) {
      true
    } else {
      (ty1, ty2) match {
        case (TyObject(bnd1, fields1), TyObject(bnd2, fields2)) => {
          val freshBnd = Gensym.gensym(bnd1)
          if (fields1.keySet != fields2.keySet) {
            false
          } else {
            fields1.forall((entry: (Label, Type)) => {
              val (label, ty) = entry;
              if(!fields2.contains(label)) {
                false
              } else {
                val swappedTy1 = typeSwap(ty, freshBnd, bnd1);
                val swappedTy2 = typeSwap(fields2(label), freshBnd, bnd2);
                equivTypes(swappedTy1, swappedTy2)
              }
            });
          }
        }
        case (TyClass(ty1), TyClass(ty2)) => equivTypes(ty1, ty2)
        case (TyFun(paramTys1, resTy1), TyFun(paramTys2, resTy2)) => {
          (paramTys1.zip(paramTys2).forall((entry: (Type, Type)) =>
            equivTypes(entry._1, entry._2))) && equivTypes(resTy1, resTy2)
        }
        case _ => false
      }
    }
  }

  def typeSubst(ty1: Type, ty2: Type, tyVar: TypeVariable): Type = ty1 match {
    case TyVar(v) =>
      if (v == tyVar) {
        ty2
      } else {
        TyVar(v)
      }
    case TyClass(ty) => TyClass(typeSubst(ty, ty2, tyVar))
    case TyObject(tv, fields) => {
      // freshen type variable
      val freshTV = Gensym.gensym(tv);

      // swap & subst in each fields
      val substitutedFields =
        fields.map((entry: (Label, Type)) => {
        val (lbl, ty) = entry;
        val swappedTy = typeSwap(ty, tv, freshTV);
        val substitutedTy = typeSubst(swappedTy, ty2, tyVar);
        (lbl, substitutedTy)
      });

      TyObject(freshTV, substitutedFields)
    }
    case TyTop => TyTop
    case TyBool => TyBool
    case TyInt => TyInt
    case TyString => TyString
    case TyFun(paramTys, resTy) => {
      val substTys = paramTys.map((ty: Type) => typeSubst(ty, ty2, tyVar))
      TyFun(substTys, typeSubst(resTy, ty2, tyVar))
    }

  }
}

// vim: set ts=2 sw=2 et sts=2:
