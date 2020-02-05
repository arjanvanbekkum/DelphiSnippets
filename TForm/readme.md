## Borderless form

Override the the `CreateParams` method with a protected method.

Implement the folowing lines in the new method, this will make the border disappear.

```pascal
  Params.ExStyle := Params.ExStyle or WS_EX_STATICEDGE;
  Params.Style := Params.Style or WS_SIZEBOX;
```