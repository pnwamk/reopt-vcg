
/- Low-level API doing file IO -/

namespace Galois

/- c.f. Prim.Fs.handle in library/init/io.lean -/

inductive Fs.Mode
| read | write | readWrite | append

inductive Fs.Whence
| set | cur | seek_end | hole | data

constant Fs.handle : Type := Unit

namespace IO
namespace Prim

open Fs

@[extern 4 "galois_io_prim_handle_mk"]
constant handle.mk (s : @& String) (m : Mode) (bin : Bool := false) : IO handle := arbitrary _
-- @[extern 2 cpp "galois_io_prim_handle_do_is_eof"]
-- constant handle.isEof (h : @& handle) : IO Bool := arbitrary _
-- @[extern 2 cpp "galois_io_prim_handle_do_flush"]
-- constant handle.flush (h : @& handle) : IO Unit := arbitrary _
@[extern 2 "galois_io_prim_handle_do_close"]
constant handle.close (h : @& handle) : IO Unit := arbitrary _
@[extern 3 "galois_io_prim_handle_do_read"]
constant handle.read (h : @& handle) (len : Nat) : IO ByteArray := arbitrary _
@[extern 3 "galois_io_prim_handle_do_write"]
constant handle.write (h : @& handle) (data : @& ByteArray) : IO Unit := arbitrary _
@[extern 4 "galois_io_prim_handle_do_lseek"]
constant handle.lseek (h : @& handle) (off : Int) (whence : Whence): IO Nat := arbitrary _

@[extern 2 "galois_io_prim_system"]
constant system (command : @& String) : IO Unit := arbitrary _

end Prim

section
variables {m : Type → Type} [Monad m] [MonadIO m]
@[inline] def system (command : String) : m Unit :=  IO.Prim.liftIO (IO.Prim.system command)
end
end IO
end Galois
