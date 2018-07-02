open Prims
type free_vars_and_fvars =
  (FStar_Syntax_Syntax.free_vars,FStar_Ident.lident FStar_Util.set)
    FStar_Pervasives_Native.tuple2
let (no_free_vars :
  (FStar_Syntax_Syntax.free_vars,FStar_Ident.lident FStar_Util.set)
    FStar_Pervasives_Native.tuple2)
  =
  let uu____13 = FStar_Syntax_Syntax.new_fv_set ()  in
  ({
     FStar_Syntax_Syntax.free_names = [];
     FStar_Syntax_Syntax.free_uvars = [];
     FStar_Syntax_Syntax.free_univs = [];
     FStar_Syntax_Syntax.free_univ_names = []
   }, uu____13)
  
let (singleton_fvar :
  FStar_Syntax_Syntax.fv ->
    (FStar_Syntax_Syntax.free_vars,FStar_Ident.lident FStar_Util.set)
      FStar_Pervasives_Native.tuple2)
  =
  fun fv  ->
    let uu____29 =
      let uu____32 = FStar_Syntax_Syntax.new_fv_set ()  in
      FStar_Util.set_add
        (fv.FStar_Syntax_Syntax.fv_name).FStar_Syntax_Syntax.v uu____32
       in
    ((FStar_Pervasives_Native.fst no_free_vars), uu____29)
  
let (singleton_bv :
  FStar_Syntax_Syntax.bv ->
    (FStar_Syntax_Syntax.free_vars,FStar_Ident.lident FStar_Util.set)
      FStar_Pervasives_Native.tuple2)
  =
  fun x  ->
    ((let uu___88_53 = FStar_Pervasives_Native.fst no_free_vars  in
      {
        FStar_Syntax_Syntax.free_names = [x];
        FStar_Syntax_Syntax.free_uvars =
          (uu___88_53.FStar_Syntax_Syntax.free_uvars);
        FStar_Syntax_Syntax.free_univs =
          (uu___88_53.FStar_Syntax_Syntax.free_univs);
        FStar_Syntax_Syntax.free_univ_names =
          (uu___88_53.FStar_Syntax_Syntax.free_univ_names)
      }), (FStar_Pervasives_Native.snd no_free_vars))
  
let (singleton_uv :
  FStar_Syntax_Syntax.ctx_uvar ->
    (FStar_Syntax_Syntax.free_vars,FStar_Ident.lident FStar_Util.set)
      FStar_Pervasives_Native.tuple2)
  =
  fun x  ->
    ((let uu___89_72 = FStar_Pervasives_Native.fst no_free_vars  in
      {
        FStar_Syntax_Syntax.free_names =
          (uu___89_72.FStar_Syntax_Syntax.free_names);
        FStar_Syntax_Syntax.free_uvars = [x];
        FStar_Syntax_Syntax.free_univs =
          (uu___89_72.FStar_Syntax_Syntax.free_univs);
        FStar_Syntax_Syntax.free_univ_names =
          (uu___89_72.FStar_Syntax_Syntax.free_univ_names)
      }), (FStar_Pervasives_Native.snd no_free_vars))
  
let (singleton_univ :
  FStar_Syntax_Syntax.universe_uvar ->
    (FStar_Syntax_Syntax.free_vars,FStar_Ident.lident FStar_Util.set)
      FStar_Pervasives_Native.tuple2)
  =
  fun x  ->
    ((let uu___90_91 = FStar_Pervasives_Native.fst no_free_vars  in
      {
        FStar_Syntax_Syntax.free_names =
          (uu___90_91.FStar_Syntax_Syntax.free_names);
        FStar_Syntax_Syntax.free_uvars =
          (uu___90_91.FStar_Syntax_Syntax.free_uvars);
        FStar_Syntax_Syntax.free_univs = [x];
        FStar_Syntax_Syntax.free_univ_names =
          (uu___90_91.FStar_Syntax_Syntax.free_univ_names)
      }), (FStar_Pervasives_Native.snd no_free_vars))
  
let (singleton_univ_name :
  FStar_Syntax_Syntax.univ_name ->
    (FStar_Syntax_Syntax.free_vars,FStar_Ident.lident FStar_Util.set)
      FStar_Pervasives_Native.tuple2)
  =
  fun x  ->
    ((let uu___91_110 = FStar_Pervasives_Native.fst no_free_vars  in
      {
        FStar_Syntax_Syntax.free_names =
          (uu___91_110.FStar_Syntax_Syntax.free_names);
        FStar_Syntax_Syntax.free_uvars =
          (uu___91_110.FStar_Syntax_Syntax.free_uvars);
        FStar_Syntax_Syntax.free_univs =
          (uu___91_110.FStar_Syntax_Syntax.free_univs);
        FStar_Syntax_Syntax.free_univ_names = [x]
      }), (FStar_Pervasives_Native.snd no_free_vars))
  
let (union :
  free_vars_and_fvars ->
    free_vars_and_fvars ->
      (FStar_Syntax_Syntax.free_vars,FStar_Ident.lident FStar_Util.set)
        FStar_Pervasives_Native.tuple2)
  =
  fun f1  ->
    fun f2  ->
      let uu____131 =
        FStar_Util.set_union (FStar_Pervasives_Native.snd f1)
          (FStar_Pervasives_Native.snd f2)
         in
      ({
         FStar_Syntax_Syntax.free_names =
           (FStar_List.append
              (FStar_Pervasives_Native.fst f1).FStar_Syntax_Syntax.free_names
              (FStar_Pervasives_Native.fst f2).FStar_Syntax_Syntax.free_names);
         FStar_Syntax_Syntax.free_uvars =
           (FStar_List.append
              (FStar_Pervasives_Native.fst f1).FStar_Syntax_Syntax.free_uvars
              (FStar_Pervasives_Native.fst f2).FStar_Syntax_Syntax.free_uvars);
         FStar_Syntax_Syntax.free_univs =
           (FStar_List.append
              (FStar_Pervasives_Native.fst f1).FStar_Syntax_Syntax.free_univs
              (FStar_Pervasives_Native.fst f2).FStar_Syntax_Syntax.free_univs);
         FStar_Syntax_Syntax.free_univ_names =
           (FStar_List.append
              (FStar_Pervasives_Native.fst f1).FStar_Syntax_Syntax.free_univ_names
              (FStar_Pervasives_Native.fst f2).FStar_Syntax_Syntax.free_univ_names)
       }, uu____131)
  
let rec (free_univs : FStar_Syntax_Syntax.universe -> free_vars_and_fvars) =
  fun u  ->
    let uu____161 = FStar_Syntax_Subst.compress_univ u  in
    match uu____161 with
    | FStar_Syntax_Syntax.U_zero  -> no_free_vars
    | FStar_Syntax_Syntax.U_bvar uu____162 -> no_free_vars
    | FStar_Syntax_Syntax.U_unknown  -> no_free_vars
    | FStar_Syntax_Syntax.U_name uname -> singleton_univ_name uname
    | FStar_Syntax_Syntax.U_succ u1 -> free_univs u1
    | FStar_Syntax_Syntax.U_max us ->
        FStar_List.fold_left
          (fun out  ->
             fun x  -> let uu____173 = free_univs x  in union out uu____173)
          no_free_vars us
    | FStar_Syntax_Syntax.U_unif u1 -> singleton_univ u1
  
let rec (free_names_and_uvs' :
  FStar_Syntax_Syntax.term -> Prims.bool -> free_vars_and_fvars) =
  fun tm  ->
    fun use_cache  ->
      let aux_binders bs from_body =
        let from_binders =
          FStar_All.pipe_right bs
            (FStar_List.fold_left
               (fun n1  ->
                  fun uu____299  ->
                    match uu____299 with
                    | (x,uu____305) ->
                        let uu____306 =
                          free_names_and_uvars x.FStar_Syntax_Syntax.sort
                            use_cache
                           in
                        union n1 uu____306) no_free_vars)
           in
        union from_binders from_body  in
      let t = FStar_Syntax_Subst.compress tm  in
      match t.FStar_Syntax_Syntax.n with
      | FStar_Syntax_Syntax.Tm_delayed uu____308 -> failwith "Impossible"
      | FStar_Syntax_Syntax.Tm_name x -> singleton_bv x
      | FStar_Syntax_Syntax.Tm_uvar (uv,uu____333) -> singleton_uv uv
      | FStar_Syntax_Syntax.Tm_type u -> free_univs u
      | FStar_Syntax_Syntax.Tm_bvar uu____351 -> no_free_vars
      | FStar_Syntax_Syntax.Tm_fvar fv -> singleton_fvar fv
      | FStar_Syntax_Syntax.Tm_constant uu____353 -> no_free_vars
      | FStar_Syntax_Syntax.Tm_lazy uu____354 -> no_free_vars
      | FStar_Syntax_Syntax.Tm_unknown  -> no_free_vars
      | FStar_Syntax_Syntax.Tm_uinst (t1,us) ->
          let f = free_names_and_uvars t1 use_cache  in
          FStar_List.fold_left
            (fun out  ->
               fun u  -> let uu____367 = free_univs u  in union out uu____367)
            f us
      | FStar_Syntax_Syntax.Tm_abs (bs,t1,uu____370) ->
          let uu____391 = free_names_and_uvars t1 use_cache  in
          aux_binders bs uu____391
      | FStar_Syntax_Syntax.Tm_arrow (bs,c) ->
          let uu____410 = free_names_and_uvars_comp c use_cache  in
          aux_binders bs uu____410
      | FStar_Syntax_Syntax.Tm_refine (bv,t1) ->
          let uu____417 = free_names_and_uvars t1 use_cache  in
          aux_binders [(bv, FStar_Pervasives_Native.None)] uu____417
      | FStar_Syntax_Syntax.Tm_app (t1,args) ->
          let uu____448 = free_names_and_uvars t1 use_cache  in
          free_names_and_uvars_args args uu____448 use_cache
      | FStar_Syntax_Syntax.Tm_match (t1,pats) ->
          let uu____493 =
            let uu____512 = free_names_and_uvars t1 use_cache  in
            FStar_List.fold_left
              (fun n1  ->
                 fun uu____535  ->
                   match uu____535 with
                   | (p,wopt,t2) ->
                       let n11 =
                         match wopt with
                         | FStar_Pervasives_Native.None  -> no_free_vars
                         | FStar_Pervasives_Native.Some w ->
                             free_names_and_uvars w use_cache
                          in
                       let n2 = free_names_and_uvars t2 use_cache  in
                       let n3 =
                         let uu____573 = FStar_Syntax_Syntax.pat_bvs p  in
                         FStar_All.pipe_right uu____573
                           (FStar_List.fold_left
                              (fun n3  ->
                                 fun x  ->
                                   let uu____583 =
                                     free_names_and_uvars
                                       x.FStar_Syntax_Syntax.sort use_cache
                                      in
                                   union n3 uu____583) n1)
                          in
                       let uu____584 = union n11 n2  in union n3 uu____584)
              uu____512
             in
          FStar_All.pipe_right pats uu____493
      | FStar_Syntax_Syntax.Tm_ascribed (t1,asc,uu____601) ->
          let u1 = free_names_and_uvars t1 use_cache  in
          let u2 =
            match FStar_Pervasives_Native.fst asc with
            | FStar_Util.Inl t2 -> free_names_and_uvars t2 use_cache
            | FStar_Util.Inr c2 -> free_names_and_uvars_comp c2 use_cache  in
          (match FStar_Pervasives_Native.snd asc with
           | FStar_Pervasives_Native.None  -> union u1 u2
           | FStar_Pervasives_Native.Some tac ->
               let uu____689 = union u1 u2  in
               let uu____690 = free_names_and_uvars tac use_cache  in
               union uu____689 uu____690)
      | FStar_Syntax_Syntax.Tm_let (lbs,t1) ->
          let uu____709 =
            let uu____716 = free_names_and_uvars t1 use_cache  in
            FStar_List.fold_left
              (fun n1  ->
                 fun lb  ->
                   let uu____722 =
                     let uu____723 =
                       free_names_and_uvars lb.FStar_Syntax_Syntax.lbtyp
                         use_cache
                        in
                     let uu____724 =
                       free_names_and_uvars lb.FStar_Syntax_Syntax.lbdef
                         use_cache
                        in
                     union uu____723 uu____724  in
                   union n1 uu____722) uu____716
             in
          FStar_All.pipe_right (FStar_Pervasives_Native.snd lbs) uu____709
      | FStar_Syntax_Syntax.Tm_quoted (tm1,qi) ->
          (match qi.FStar_Syntax_Syntax.qkind with
           | FStar_Syntax_Syntax.Quote_static  -> no_free_vars
           | FStar_Syntax_Syntax.Quote_dynamic  ->
               free_names_and_uvars tm1 use_cache)
      | FStar_Syntax_Syntax.Tm_meta (t1,m) ->
          let u1 = free_names_and_uvars t1 use_cache  in
          (match m with
           | FStar_Syntax_Syntax.Meta_pattern args ->
               FStar_List.fold_right
                 (fun a  ->
                    fun acc  -> free_names_and_uvars_args a acc use_cache)
                 args u1
           | FStar_Syntax_Syntax.Meta_monadic (uu____785,t') ->
               let uu____791 = free_names_and_uvars t' use_cache  in
               union u1 uu____791
           | FStar_Syntax_Syntax.Meta_monadic_lift (uu____792,uu____793,t')
               ->
               let uu____799 = free_names_and_uvars t' use_cache  in
               union u1 uu____799
           | FStar_Syntax_Syntax.Meta_labeled uu____800 -> u1
           | FStar_Syntax_Syntax.Meta_desugared uu____807 -> u1
           | FStar_Syntax_Syntax.Meta_named uu____808 -> u1)

and (free_names_and_uvars :
  FStar_Syntax_Syntax.term' FStar_Syntax_Syntax.syntax ->
    Prims.bool -> free_vars_and_fvars)
  =
  fun t  ->
    fun use_cache  ->
      let t1 = FStar_Syntax_Subst.compress t  in
      let uu____814 = FStar_ST.op_Bang t1.FStar_Syntax_Syntax.vars  in
      match uu____814 with
      | FStar_Pervasives_Native.Some n1 when
          let uu____841 = should_invalidate_cache n1 use_cache  in
          Prims.op_Negation uu____841 ->
          let uu____842 = FStar_Syntax_Syntax.new_fv_set ()  in
          (n1, uu____842)
      | uu____847 ->
          (FStar_ST.op_Colon_Equals t1.FStar_Syntax_Syntax.vars
             FStar_Pervasives_Native.None;
           (let n1 = free_names_and_uvs' t1 use_cache  in
            FStar_ST.op_Colon_Equals t1.FStar_Syntax_Syntax.vars
              (FStar_Pervasives_Native.Some (FStar_Pervasives_Native.fst n1));
            n1))

and (free_names_and_uvars_args :
  (FStar_Syntax_Syntax.term' FStar_Syntax_Syntax.syntax,FStar_Syntax_Syntax.aqual)
    FStar_Pervasives_Native.tuple2 Prims.list ->
    (FStar_Syntax_Syntax.free_vars,FStar_Ident.lident FStar_Util.set)
      FStar_Pervasives_Native.tuple2 ->
      Prims.bool ->
        (FStar_Syntax_Syntax.free_vars,FStar_Ident.lident FStar_Util.set)
          FStar_Pervasives_Native.tuple2)
  =
  fun args  ->
    fun acc  ->
      fun use_cache  ->
        FStar_All.pipe_right args
          (FStar_List.fold_left
             (fun n1  ->
                fun uu____944  ->
                  match uu____944 with
                  | (x,uu____952) ->
                      let uu____957 = free_names_and_uvars x use_cache  in
                      union n1 uu____957) acc)

and (free_names_and_uvars_binders :
  FStar_Syntax_Syntax.binders ->
    free_vars_and_fvars -> Prims.bool -> free_vars_and_fvars)
  =
  fun bs  ->
    fun acc  ->
      fun use_cache  ->
        FStar_All.pipe_right bs
          (FStar_List.fold_left
             (fun n1  ->
                fun uu____977  ->
                  match uu____977 with
                  | (x,uu____983) ->
                      let uu____984 =
                        free_names_and_uvars x.FStar_Syntax_Syntax.sort
                          use_cache
                         in
                      union n1 uu____984) acc)

and (free_names_and_uvars_comp :
  FStar_Syntax_Syntax.comp' FStar_Syntax_Syntax.syntax ->
    Prims.bool -> free_vars_and_fvars)
  =
  fun c  ->
    fun use_cache  ->
      let uu____989 = FStar_ST.op_Bang c.FStar_Syntax_Syntax.vars  in
      match uu____989 with
      | FStar_Pervasives_Native.Some n1 ->
          let uu____1016 = should_invalidate_cache n1 use_cache  in
          if uu____1016
          then
            (FStar_ST.op_Colon_Equals c.FStar_Syntax_Syntax.vars
               FStar_Pervasives_Native.None;
             free_names_and_uvars_comp c use_cache)
          else
            (let uu____1042 = FStar_Syntax_Syntax.new_fv_set ()  in
             (n1, uu____1042))
      | uu____1047 ->
          let n1 =
            match c.FStar_Syntax_Syntax.n with
            | FStar_Syntax_Syntax.GTotal (t,FStar_Pervasives_Native.None ) ->
                free_names_and_uvars t use_cache
            | FStar_Syntax_Syntax.Total (t,FStar_Pervasives_Native.None ) ->
                free_names_and_uvars t use_cache
            | FStar_Syntax_Syntax.GTotal (t,FStar_Pervasives_Native.Some u)
                ->
                let uu____1085 = free_univs u  in
                let uu____1086 = free_names_and_uvars t use_cache  in
                union uu____1085 uu____1086
            | FStar_Syntax_Syntax.Total (t,FStar_Pervasives_Native.Some u) ->
                let uu____1095 = free_univs u  in
                let uu____1096 = free_names_and_uvars t use_cache  in
                union uu____1095 uu____1096
            | FStar_Syntax_Syntax.Comp ct ->
                let us =
                  let uu____1105 =
                    free_names_and_uvars ct.FStar_Syntax_Syntax.result_typ
                      use_cache
                     in
                  free_names_and_uvars_args
                    ct.FStar_Syntax_Syntax.effect_args uu____1105 use_cache
                   in
                FStar_List.fold_left
                  (fun us1  ->
                     fun u  ->
                       let uu____1117 = free_univs u  in union us1 uu____1117)
                  us ct.FStar_Syntax_Syntax.comp_univs
             in
          (FStar_ST.op_Colon_Equals c.FStar_Syntax_Syntax.vars
             (FStar_Pervasives_Native.Some (FStar_Pervasives_Native.fst n1));
           n1)

and (should_invalidate_cache :
  FStar_Syntax_Syntax.free_vars -> Prims.bool -> Prims.bool) =
  fun n1  ->
    fun use_cache  ->
      (Prims.op_Negation use_cache) ||
        ((FStar_All.pipe_right n1.FStar_Syntax_Syntax.free_uvars
            (FStar_Util.for_some
               (fun u  ->
                  let uu____1152 =
                    FStar_Syntax_Unionfind.find
                      u.FStar_Syntax_Syntax.ctx_uvar_head
                     in
                  match uu____1152 with
                  | FStar_Pervasives_Native.Some uu____1155 -> true
                  | uu____1156 -> false)))
           ||
           (FStar_All.pipe_right n1.FStar_Syntax_Syntax.free_univs
              (FStar_Util.for_some
                 (fun u  ->
                    let uu____1165 = FStar_Syntax_Unionfind.univ_find u  in
                    match uu____1165 with
                    | FStar_Pervasives_Native.Some uu____1168 -> true
                    | FStar_Pervasives_Native.None  -> false))))

let (compare_uv :
  FStar_Syntax_Syntax.ctx_uvar -> FStar_Syntax_Syntax.ctx_uvar -> Prims.int)
  =
  fun uv1  ->
    fun uv2  ->
      let uu____1179 =
        FStar_Syntax_Unionfind.uvar_id uv1.FStar_Syntax_Syntax.ctx_uvar_head
         in
      let uu____1180 =
        FStar_Syntax_Unionfind.uvar_id uv2.FStar_Syntax_Syntax.ctx_uvar_head
         in
      uu____1179 - uu____1180
  
let (new_uv_set : unit -> FStar_Syntax_Syntax.uvars) =
  fun uu____1185  -> FStar_Util.new_set compare_uv 
let (compare_universe_uvar :
  FStar_Syntax_Syntax.universe_uvar ->
    FStar_Syntax_Syntax.universe_uvar -> Prims.int)
  =
  fun x  ->
    fun y  ->
      let uu____1196 = FStar_Syntax_Unionfind.univ_uvar_id x  in
      let uu____1197 = FStar_Syntax_Unionfind.univ_uvar_id y  in
      uu____1196 - uu____1197
  
let (new_universe_uvar_set :
  unit -> FStar_Syntax_Syntax.universe_uvar FStar_Util.set) =
  fun uu____1204  -> FStar_Util.new_set compare_universe_uvar 
let (empty : FStar_Syntax_Syntax.bv FStar_Util.set) =
  FStar_Util.new_set FStar_Syntax_Syntax.order_bv 
let (names :
  FStar_Syntax_Syntax.term -> FStar_Syntax_Syntax.bv FStar_Util.set) =
  fun t  ->
    let uu____1216 =
      let uu____1219 =
        let uu____1220 = free_names_and_uvars t true  in
        FStar_Pervasives_Native.fst uu____1220  in
      uu____1219.FStar_Syntax_Syntax.free_names  in
    FStar_Util.as_set uu____1216 FStar_Syntax_Syntax.order_bv
  
let (uvars :
  FStar_Syntax_Syntax.term -> FStar_Syntax_Syntax.ctx_uvar FStar_Util.set) =
  fun t  ->
    let uu____1236 =
      let uu____1239 =
        let uu____1240 = free_names_and_uvars t true  in
        FStar_Pervasives_Native.fst uu____1240  in
      uu____1239.FStar_Syntax_Syntax.free_uvars  in
    FStar_Util.as_set uu____1236 compare_uv
  
let (univs :
  FStar_Syntax_Syntax.term ->
    FStar_Syntax_Syntax.universe_uvar FStar_Util.set)
  =
  fun t  ->
    let uu____1256 =
      let uu____1259 =
        let uu____1260 = free_names_and_uvars t true  in
        FStar_Pervasives_Native.fst uu____1260  in
      uu____1259.FStar_Syntax_Syntax.free_univs  in
    FStar_Util.as_set uu____1256 compare_universe_uvar
  
let (univnames :
  FStar_Syntax_Syntax.term -> FStar_Syntax_Syntax.univ_name FStar_Util.set) =
  fun t  ->
    let uu____1276 =
      let uu____1279 =
        let uu____1280 = free_names_and_uvars t true  in
        FStar_Pervasives_Native.fst uu____1280  in
      uu____1279.FStar_Syntax_Syntax.free_univ_names  in
    FStar_Util.as_set uu____1276 FStar_Syntax_Syntax.order_univ_name
  
let (univnames_comp :
  FStar_Syntax_Syntax.comp -> FStar_Syntax_Syntax.univ_name FStar_Util.set) =
  fun c  ->
    let uu____1296 =
      let uu____1299 =
        let uu____1300 = free_names_and_uvars_comp c true  in
        FStar_Pervasives_Native.fst uu____1300  in
      uu____1299.FStar_Syntax_Syntax.free_univ_names  in
    FStar_Util.as_set uu____1296 FStar_Syntax_Syntax.order_univ_name
  
let (fvars : FStar_Syntax_Syntax.term -> FStar_Ident.lident FStar_Util.set) =
  fun t  ->
    let uu____1316 = free_names_and_uvars t false  in
    FStar_Pervasives_Native.snd uu____1316
  
let (names_of_binders :
  FStar_Syntax_Syntax.binders -> FStar_Syntax_Syntax.bv FStar_Util.set) =
  fun bs  ->
    let uu____1332 =
      let uu____1335 =
        let uu____1336 = free_names_and_uvars_binders bs no_free_vars true
           in
        FStar_Pervasives_Native.fst uu____1336  in
      uu____1335.FStar_Syntax_Syntax.free_names  in
    FStar_Util.as_set uu____1332 FStar_Syntax_Syntax.order_bv
  