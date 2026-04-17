module type S = EzReqEioS.S
module type Interface = EzReqEioS.Interface
module Make(_ : Interface) : S
