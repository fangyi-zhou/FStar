
open Prims
open FStar_Pervasives

let disentangle_abbrevs_from_bundle : FStar_Syntax_Syntax.sigelt Prims.list  ->  FStar_Syntax_Syntax.qualifier Prims.list  ->  FStar_Ident.lident Prims.list  ->  FStar_Range.range  ->  (FStar_Syntax_Syntax.sigelt * FStar_Syntax_Syntax.sigelt Prims.list) = (fun sigelts quals members rng -> (

let sigattrs = (FStar_List.collect (fun s -> s.FStar_Syntax_Syntax.sigattrs) sigelts)
in (

let type_abbrev_sigelts = (FStar_All.pipe_right sigelts (FStar_List.collect (fun x -> (match (x.FStar_Syntax_Syntax.sigel) with
| FStar_Syntax_Syntax.Sig_let ((false, ({FStar_Syntax_Syntax.lbname = FStar_Util.Inr (uu____63); FStar_Syntax_Syntax.lbunivs = uu____64; FStar_Syntax_Syntax.lbtyp = uu____65; FStar_Syntax_Syntax.lbeff = uu____66; FStar_Syntax_Syntax.lbdef = uu____67})::[]), uu____68) -> begin
(x)::[]
end
| FStar_Syntax_Syntax.Sig_let (uu____87, uu____88) -> begin
(failwith "mutrecty: disentangle_abbrevs_from_bundle: type_abbrev_sigelts: impossible")
end
| uu____95 -> begin
[]
end))))
in (match (type_abbrev_sigelts) with
| [] -> begin
(({FStar_Syntax_Syntax.sigel = FStar_Syntax_Syntax.Sig_bundle (((sigelts), (members))); FStar_Syntax_Syntax.sigrng = rng; FStar_Syntax_Syntax.sigquals = quals; FStar_Syntax_Syntax.sigmeta = FStar_Syntax_Syntax.default_sigmeta; FStar_Syntax_Syntax.sigattrs = sigattrs}), ([]))
end
| uu____108 -> begin
(

let type_abbrevs = (FStar_All.pipe_right type_abbrev_sigelts (FStar_List.map (fun x -> (match (x.FStar_Syntax_Syntax.sigel) with
| FStar_Syntax_Syntax.Sig_let ((uu____127, ({FStar_Syntax_Syntax.lbname = FStar_Util.Inr (fv); FStar_Syntax_Syntax.lbunivs = uu____129; FStar_Syntax_Syntax.lbtyp = uu____130; FStar_Syntax_Syntax.lbeff = uu____131; FStar_Syntax_Syntax.lbdef = uu____132})::[]), uu____133) -> begin
fv.FStar_Syntax_Syntax.fv_name.FStar_Syntax_Syntax.v
end
| uu____152 -> begin
(failwith "mutrecty: disentangle_abbrevs_from_bundle: type_abbrevs: impossible")
end))))
in (

let unfolded_type_abbrevs = (

let rev_unfolded_type_abbrevs = (FStar_Util.mk_ref [])
in (

let in_progress = (FStar_Util.mk_ref [])
in (

let not_unfolded_yet = (FStar_Util.mk_ref type_abbrev_sigelts)
in (

let remove_not_unfolded = (fun lid -> (

let uu____181 = (

let uu____184 = (FStar_ST.read not_unfolded_yet)
in (FStar_All.pipe_right uu____184 (FStar_List.filter (fun x -> (match (x.FStar_Syntax_Syntax.sigel) with
| FStar_Syntax_Syntax.Sig_let ((uu____204, ({FStar_Syntax_Syntax.lbname = FStar_Util.Inr (fv); FStar_Syntax_Syntax.lbunivs = uu____206; FStar_Syntax_Syntax.lbtyp = uu____207; FStar_Syntax_Syntax.lbeff = uu____208; FStar_Syntax_Syntax.lbdef = uu____209})::[]), uu____210) -> begin
(not ((FStar_Ident.lid_equals lid fv.FStar_Syntax_Syntax.fv_name.FStar_Syntax_Syntax.v)))
end
| uu____229 -> begin
true
end)))))
in (FStar_ST.write not_unfolded_yet uu____181)))
in (

let rec unfold_abbrev_fv = (fun t fv -> (

let replacee = (fun x -> (match (x.FStar_Syntax_Syntax.sigel) with
| FStar_Syntax_Syntax.Sig_let ((uu____252, ({FStar_Syntax_Syntax.lbname = FStar_Util.Inr (fv'); FStar_Syntax_Syntax.lbunivs = uu____254; FStar_Syntax_Syntax.lbtyp = uu____255; FStar_Syntax_Syntax.lbeff = uu____256; FStar_Syntax_Syntax.lbdef = uu____257})::[]), uu____258) when (FStar_Ident.lid_equals fv'.FStar_Syntax_Syntax.fv_name.FStar_Syntax_Syntax.v fv.FStar_Syntax_Syntax.fv_name.FStar_Syntax_Syntax.v) -> begin
FStar_Pervasives_Native.Some (x)
end
| uu____277 -> begin
FStar_Pervasives_Native.None
end))
in (

let replacee_term = (fun x -> (match ((replacee x)) with
| FStar_Pervasives_Native.Some ({FStar_Syntax_Syntax.sigel = FStar_Syntax_Syntax.Sig_let ((uu____290, ({FStar_Syntax_Syntax.lbname = uu____291; FStar_Syntax_Syntax.lbunivs = uu____292; FStar_Syntax_Syntax.lbtyp = uu____293; FStar_Syntax_Syntax.lbeff = uu____294; FStar_Syntax_Syntax.lbdef = tm})::[]), uu____296); FStar_Syntax_Syntax.sigrng = uu____297; FStar_Syntax_Syntax.sigquals = uu____298; FStar_Syntax_Syntax.sigmeta = uu____299; FStar_Syntax_Syntax.sigattrs = uu____300}) -> begin
FStar_Pervasives_Native.Some (tm)
end
| uu____329 -> begin
FStar_Pervasives_Native.None
end))
in (

let uu____334 = (

let uu____339 = (FStar_ST.read rev_unfolded_type_abbrevs)
in (FStar_Util.find_map uu____339 replacee_term))
in (match (uu____334) with
| FStar_Pervasives_Native.Some (x) -> begin
x
end
| FStar_Pervasives_Native.None -> begin
(

let uu____355 = (FStar_Util.find_map type_abbrev_sigelts replacee)
in (match (uu____355) with
| FStar_Pervasives_Native.Some (se) -> begin
(

let uu____359 = (

let uu____360 = (FStar_ST.read in_progress)
in (FStar_List.existsb (fun x -> (FStar_Ident.lid_equals x fv.FStar_Syntax_Syntax.fv_name.FStar_Syntax_Syntax.v)) uu____360))
in (match (uu____359) with
| true -> begin
(

let msg = (FStar_Util.format1 "Cycle on %s in mutually recursive type abbreviations" fv.FStar_Syntax_Syntax.fv_name.FStar_Syntax_Syntax.v.FStar_Ident.str)
in (FStar_Pervasives.raise (FStar_Errors.Error (((msg), ((FStar_Ident.range_of_lid fv.FStar_Syntax_Syntax.fv_name.FStar_Syntax_Syntax.v)))))))
end
| uu____370 -> begin
(unfold_abbrev se)
end))
end
| uu____371 -> begin
t
end))
end)))))
and unfold_abbrev = (fun x -> (match (x.FStar_Syntax_Syntax.sigel) with
| FStar_Syntax_Syntax.Sig_let ((false, (lb)::[]), uu____376) -> begin
(

let quals1 = (FStar_All.pipe_right x.FStar_Syntax_Syntax.sigquals (FStar_List.filter (fun uu___211_397 -> (match (uu___211_397) with
| FStar_Syntax_Syntax.Noeq -> begin
false
end
| uu____398 -> begin
true
end))))
in (

let lid = (match (lb.FStar_Syntax_Syntax.lbname) with
| FStar_Util.Inr (fv) -> begin
fv.FStar_Syntax_Syntax.fv_name.FStar_Syntax_Syntax.v
end
| uu____401 -> begin
(failwith "mutrecty: disentangle_abbrevs_from_bundle: rename_abbrev: lid: impossible")
end)
in ((

let uu____407 = (

let uu____410 = (FStar_ST.read in_progress)
in (lid)::uu____410)
in (FStar_ST.write in_progress uu____407));
(match (()) with
| () -> begin
((remove_not_unfolded lid);
(match (()) with
| () -> begin
(

let ty' = (FStar_Syntax_InstFV.inst unfold_abbrev_fv lb.FStar_Syntax_Syntax.lbtyp)
in (

let tm' = (FStar_Syntax_InstFV.inst unfold_abbrev_fv lb.FStar_Syntax_Syntax.lbdef)
in (

let lb' = (

let uu___212_425 = lb
in {FStar_Syntax_Syntax.lbname = uu___212_425.FStar_Syntax_Syntax.lbname; FStar_Syntax_Syntax.lbunivs = uu___212_425.FStar_Syntax_Syntax.lbunivs; FStar_Syntax_Syntax.lbtyp = ty'; FStar_Syntax_Syntax.lbeff = uu___212_425.FStar_Syntax_Syntax.lbeff; FStar_Syntax_Syntax.lbdef = tm'})
in (

let sigelt' = FStar_Syntax_Syntax.Sig_let (((((false), ((lb')::[]))), ((lid)::[])))
in ((

let uu____438 = (

let uu____441 = (FStar_ST.read rev_unfolded_type_abbrevs)
in ((

let uu___213_449 = x
in {FStar_Syntax_Syntax.sigel = sigelt'; FStar_Syntax_Syntax.sigrng = uu___213_449.FStar_Syntax_Syntax.sigrng; FStar_Syntax_Syntax.sigquals = quals1; FStar_Syntax_Syntax.sigmeta = uu___213_449.FStar_Syntax_Syntax.sigmeta; FStar_Syntax_Syntax.sigattrs = uu___213_449.FStar_Syntax_Syntax.sigattrs}))::uu____441)
in (FStar_ST.write rev_unfolded_type_abbrevs uu____438));
(match (()) with
| () -> begin
((

let uu____455 = (

let uu____458 = (FStar_ST.read in_progress)
in (FStar_List.tl uu____458))
in (FStar_ST.write in_progress uu____455));
(match (()) with
| () -> begin
tm'
end);
)
end);
)))))
end);
)
end);
)))
end
| uu____469 -> begin
(failwith "mutrecty: disentangle_abbrevs_from_bundle: rename_abbrev: impossible")
end))
in (

let rec aux = (fun uu____475 -> (

let uu____476 = (FStar_ST.read not_unfolded_yet)
in (match (uu____476) with
| (x)::uu____486 -> begin
(

let _unused = (unfold_abbrev x)
in (aux ()))
end
| uu____490 -> begin
(

let uu____493 = (FStar_ST.read rev_unfolded_type_abbrevs)
in (FStar_List.rev uu____493))
end)))
in (aux ())))))))
in (

let filter_out_type_abbrevs = (fun l -> (FStar_List.filter (fun lid -> (FStar_List.for_all (fun lid' -> (not ((FStar_Ident.lid_equals lid lid')))) type_abbrevs)) l))
in (

let inductives_with_abbrevs_unfolded = (

let find_in_unfolded = (fun fv -> (FStar_Util.find_map unfolded_type_abbrevs (fun x -> (match (x.FStar_Syntax_Syntax.sigel) with
| FStar_Syntax_Syntax.Sig_let ((uu____540, ({FStar_Syntax_Syntax.lbname = FStar_Util.Inr (fv'); FStar_Syntax_Syntax.lbunivs = uu____542; FStar_Syntax_Syntax.lbtyp = uu____543; FStar_Syntax_Syntax.lbeff = uu____544; FStar_Syntax_Syntax.lbdef = tm})::[]), uu____546) when (FStar_Ident.lid_equals fv'.FStar_Syntax_Syntax.fv_name.FStar_Syntax_Syntax.v fv.FStar_Syntax_Syntax.fv_name.FStar_Syntax_Syntax.v) -> begin
FStar_Pervasives_Native.Some (tm)
end
| uu____567 -> begin
FStar_Pervasives_Native.None
end))))
in (

let unfold_fv = (fun t fv -> (

let uu____577 = (find_in_unfolded fv)
in (match (uu____577) with
| FStar_Pervasives_Native.Some (t') -> begin
t'
end
| uu____587 -> begin
t
end)))
in (

let unfold_in_sig = (fun x -> (match (x.FStar_Syntax_Syntax.sigel) with
| FStar_Syntax_Syntax.Sig_inductive_typ (lid, univs1, bnd, ty, mut, dc) -> begin
(

let bnd' = (FStar_Syntax_InstFV.inst_binders unfold_fv bnd)
in (

let ty' = (FStar_Syntax_InstFV.inst unfold_fv ty)
in (

let mut' = (filter_out_type_abbrevs mut)
in ((

let uu___214_620 = x
in {FStar_Syntax_Syntax.sigel = FStar_Syntax_Syntax.Sig_inductive_typ (((lid), (univs1), (bnd'), (ty'), (mut'), (dc))); FStar_Syntax_Syntax.sigrng = uu___214_620.FStar_Syntax_Syntax.sigrng; FStar_Syntax_Syntax.sigquals = uu___214_620.FStar_Syntax_Syntax.sigquals; FStar_Syntax_Syntax.sigmeta = uu___214_620.FStar_Syntax_Syntax.sigmeta; FStar_Syntax_Syntax.sigattrs = uu___214_620.FStar_Syntax_Syntax.sigattrs}))::[])))
end
| FStar_Syntax_Syntax.Sig_datacon (lid, univs1, ty, res, npars, mut) -> begin
(

let ty' = (FStar_Syntax_InstFV.inst unfold_fv ty)
in (

let mut' = (filter_out_type_abbrevs mut)
in ((

let uu___215_640 = x
in {FStar_Syntax_Syntax.sigel = FStar_Syntax_Syntax.Sig_datacon (((lid), (univs1), (ty'), (res), (npars), (mut'))); FStar_Syntax_Syntax.sigrng = uu___215_640.FStar_Syntax_Syntax.sigrng; FStar_Syntax_Syntax.sigquals = uu___215_640.FStar_Syntax_Syntax.sigquals; FStar_Syntax_Syntax.sigmeta = uu___215_640.FStar_Syntax_Syntax.sigmeta; FStar_Syntax_Syntax.sigattrs = uu___215_640.FStar_Syntax_Syntax.sigattrs}))::[]))
end
| FStar_Syntax_Syntax.Sig_let (uu____643, uu____644) -> begin
[]
end
| uu____649 -> begin
(failwith "mutrecty: inductives_with_abbrevs_unfolded: unfold_in_sig: impossible")
end))
in (FStar_List.collect unfold_in_sig sigelts))))
in (

let new_members = (filter_out_type_abbrevs members)
in (

let new_bundle = {FStar_Syntax_Syntax.sigel = FStar_Syntax_Syntax.Sig_bundle (((inductives_with_abbrevs_unfolded), (new_members))); FStar_Syntax_Syntax.sigrng = rng; FStar_Syntax_Syntax.sigquals = quals; FStar_Syntax_Syntax.sigmeta = FStar_Syntax_Syntax.default_sigmeta; FStar_Syntax_Syntax.sigattrs = sigattrs}
in ((new_bundle), (unfolded_type_abbrevs))))))))
end))))




