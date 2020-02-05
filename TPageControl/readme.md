## Docking a Form in a TPageControl

### Form To Dock

In the `FormCreate` method add the following line of code to make it possible to dock the form into anything
```pascal
  DragKind := dkDock
```

In the `FormClose` make sure to free to form.
```pascal
Action := caFree;
```
Add the `OnStartDock` event to the form and create an transparent window while dragging the form, this shows when moving the form around.

```pascal
DragObject:= TTransparentDragDockObject.Create(Self);
```

### Form with the TPageControl

In the `FormCreate` of the form containing the pagecontrol, add the following line of code to make the pagecontrol accept the docking other objects
```pascal
PageControl.DockSite := True;
```

In the `OnDockOver` event set the `Accept` to true it the object is a form of the correct type

```pascal
Accept := Source.Control is TMyBaseForm;
``` 
 Also add the `OnGetSiteInfo` event and set the `AllowDock` 

```pascal
  CanDock := DockClient is TMyBaseForm;
```
You can use the `OnDockDrop` and `OnUnDock` method to change some things when docking and undocking the form from the pagecontrol

## Adding a close button to the tabs on a TPageControl

When you docking form to a pagecontrol you might want to close the tab on the pagecontrol and also close the form. But Tabs on a pagecontrol do not have a close button by default. 

You can create your own `OnDrawTab` event and draw a close button on every tab. You will need to implement some `OnMouse` events to make everything work.