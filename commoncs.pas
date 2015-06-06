//Copyright (c) 2012 by Donald R. Ziesig
//
//Donald.at.Ziesig.org
//
//This file is part of MagicLibrary.
//
//MagicLibrary is free software: you can redistribute it and/or modify
//it under the terms of the GNU General Public License as published by
//the Free Software Foundation, either version 3 of the License, or
//(at your option) any later version.
//
//MagicLibrary is distributed in the hope that it will be useful,
//but WITHOUT ANY WARRANTY; without even the implied warranty of
//MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//GNU General Public License for more details.
//
//You should have received a copy of the GNU General Public License
//along with MagicLibrary.  If not, see <http://www.gnu.org/licenses/>.

{ Computer sciency stuff }

unit CommonCS;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComCtrls;

{==============================================================================}
{ Inheritance tests                                                            }
{==============================================================================}

function IsAncestor( Obj, Anc : TClass ) : Boolean;

{==============================================================================}
{ TTreeview Support stuff.                                                     }
{==============================================================================}

type
  {: Callback to use to copy the data of a treenode when the
     node itself is copied by CopySubtree.
   @param oldnode is the old node
   @param newnode is the new node
   @Desc Use a callback of this type to implement your own algorithm
     for a node copy. The default just uses the Assign method, which
     produces a shallow copy of the nodes Data property. }
  TCopyDataProc = procedure(oldnode, newnode : TTreenode);
  TCopyDataProcObject = procedure(oldnode, newnode : TTreenode) of object;

{-- CopySubtree
-------------------------------------------------------}
{: Copies the source node with all child nodes to the target treeview.
@Param sourcenode is the node to copy
@Param target is the treeview to insert the copied nodes into
@Param targetnode is the node to insert the copy under, can be nil to
  make the copy a top-level node.
@Param CopyProc is the (optional) callback to use to copy a node.
  If Nil is passed for this parameter theDefaultCopyDataProc will be
used.
@Precondition  sourcenode <> nil, target <> nil, targetnode is either
  nil or a node of target
@Raises Exception if targetnode happens to be in the subtree rooted in
  sourcenode. Handling that special case is rather complicated, so we
  simply refuse to do it at the moment.
}{ Created 2003-04-09 by P. Below -----------------------------------------------------------------------
}
{function CopySubtree(sourcenode : TTreenode; target : TTreeview;
  targetnode : TTreenode; CopyProc : TCopyDataProc = nil) : TTreeNode; overload;}

{ TODO 3 -odonz -cTest : Implement Unit Test }
function CopySubtree( sourcenode : TTreenode; target : TTreeview;
                      targetnode : TTreenode; CopyProc : TCopyDataProcObject;
                      LabelCopy : Boolean; IsChild : Boolean = False) : TTreeNode; overload;

procedure CopyTreeView( SourceTree, DestTree : TTreeview; { TODO 3 -odonz -cTest : Implement Unit Test }
                        CopyProc : TCopyDataProcObject;
                        LabelCopy : Boolean = False);



implementation

{==============================================================================}
{ Inheritance tests                                                            }
{==============================================================================}

function IsAncestor( Obj, Anc : TClass ) : Boolean;
var
  ObjCR, AncCR : TClass;
begin
  ObjCR := Obj;
  AncCR := Anc;
  while ObjCR <> nil do
    if ObjCR = AncCR then
      begin
        Result := True;
        Exit;
      end
    else
      ObjCR := ObjCR.ClassParent;
end;

{==============================================================================}
{ TTreeview Support stuff.                                                     }
{==============================================================================}

{: The default operation is to do a shallow copy of the node, via
Assign. }
procedure DefaultCopyDataProc(oldnode, newnode : TTreenode);
begin
  newnode.Assign(oldnode);
end;

{-- CopySubtree
-------------------------------------------------------}
{: Copies the source node with all child nodes to the target treeview.
@Param sourcenode is the node to copy
@Param target is the treeview to insert the copied nodes into
@Param targetnode is the node to insert the copy under, can be nil to
  make the copy a top-level node.
@Param CopyProc is the (optional) callback to use to copy a node.
  If Nil is passed for this parameter theDefaultCopyDataProc will be
used.
@Precondition  sourcenode <> nil, target <> nil, targetnode is either
  nil or a node of target
@Raises Exception if targetnode happens to be in the subtree rooted in
  sourcenode. Handling that special case is rather complicated, so we
  simply refuse to do it at the moment.
}{ Created 2003-04-09 by P. Below -----------------------------------------------------------------------
}
{$ifdef NO}
function CopySubtree(sourcenode : TTreenode; target : TTreeview;
  targetnode : TTreenode; CopyProc : TCopyDataProc = nil) : TTreeNode;
var
  anchor : TTreenode;
  child : TTreenode;
begin { CopySubtree }
  Assert(Assigned(sourcenode),
    'CopySubtree:sourcenode cannot be nil');
  Assert(Assigned(target),
    'CopySubtree: target treeview cannot be nil');
  Assert((targetnode = nil) or (targetnode.TreeView = target),
    'CopySubtree: targetnode has to be a node in the target treeview.');

  if (sourcenode.TreeView = target) and
    (targetnode.HasAsParent(sourcenode) or (sourcenode =
    targetnode)) then
    raise Exception.Create('CopySubtree cannot copy a subtree to one of the ' +
      'subtrees nodes.');

  if not Assigned(CopyProc) then
    CopyProc := DefaultCopyDataProc;

  anchor := target.Items.AddChild(targetnode, sourcenode.Text);
  CopyProc(sourcenode, anchor);
  anchor.Text := anchor.Text  + ' [Copy]';
  Result := anchor;
  child := sourcenode.GetFirstChild;
  while Assigned(child) do
  begin
    CopySubtree(child, target, anchor, CopyProc);
    child := child.getNextSibling;
  end; { While }
end; { CopySubtree }
{$endif}

function CopySubtree(sourcenode : TTreenode; target : TTreeview;
  targetnode : TTreenode; CopyProc : TCopyDataProcObject;
  LabelCopy : Boolean; IsChild : Boolean) : TTreeNode;
var
  anchor : TTreenode;
  child : TTreenode;
begin { CopySubtree }
  Assert(Assigned(sourcenode),
    'CopySubtree:sourcenode cannot be nil');
  Assert(Assigned(target),
    'CopySubtree: target treeview cannot be nil');
  Assert((targetnode = nil) or (targetnode.TreeView = target),
    'CopySubtree: targetnode has to be a node in the target treeview.');

  if (sourcenode.TreeView = target) and ((targetnode <> nil) and
    (targetnode.HasAsParent(sourcenode) or (sourcenode =
    targetnode))) then
    raise Exception.Create('CopySubtree cannot copy a subtree to one of the ' +
      'subtrees nodes.');

  anchor := target.Items.AddChild(targetnode, sourcenode.Text);
  CopyProc(sourcenode, anchor);
  if IsChild and LabelCopy then
    anchor.Text := anchor.Text  + ' [Copy]';
  Result := anchor;
  child := sourcenode.GetFirstChild;
  while Assigned(child) do
  begin
    CopySubtree(child, target, anchor, CopyProc, true);
    child := child.getNextSibling;
  end; { While }
end; { CopySubtree }

procedure CopyTreeView( SourceTree, DestTree : TTreeview;
                        CopyProc : TCopyDataProcObject;
                        LabelCopy : Boolean);
var
  Node : TTreeNode;
begin
  DestTree.Items.Clear;
  if SourceTree.Items.Count = 0 then exit;
  Node := SourceTree.Items[0];
  while Node <> nil do
    begin
      CopySubtree( Node, DestTree,nil,CopyProc,LabelCopy);
      Node := Node.GetNextSibling;
    end;
end;

end.

