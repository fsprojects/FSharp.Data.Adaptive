namespace rec FSharp.Data.Adaptive

open System
open System.Runtime.InteropServices
open System.Runtime.CompilerServices
open Microsoft.FSharp.NativeInterop
open MBrace.FsPickler
open MBrace.FsPickler.Combinators

#nowarn "9"
#nowarn "51"

[<StructLayout(LayoutKind.Sequential)>]
type Arr32 =
    struct
        val mutable public E00 : byte
        val mutable public E01 : byte
        val mutable public E02 : byte
        val mutable public E03 : byte
        val mutable public E04 : byte
        val mutable public E05 : byte
        val mutable public E06 : byte
        val mutable public E07 : byte
        val mutable public E08 : byte
        val mutable public E09 : byte
        val mutable public E10 : byte
        val mutable public E11 : byte
        val mutable public E12 : byte
        val mutable public E13 : byte
        val mutable public E14 : byte
        val mutable public E15 : byte
        val mutable public E16 : byte
        val mutable public E17 : byte
        val mutable public E18 : byte
        val mutable public E19 : byte
        val mutable public E20 : byte
        val mutable public E21 : byte
        val mutable public E22 : byte
        val mutable public E23 : byte
        val mutable public E24 : byte
        val mutable public E25 : byte
        val mutable public E26 : byte
        val mutable public E27 : byte
        val mutable public E28 : byte
        val mutable public E29 : byte
        val mutable public E30 : byte
        val mutable public E31 : byte
     
        member x.ToArray() =    
            [| 
                x.E00; x.E01; x.E02; x.E03; x.E04; x.E05; x.E06; x.E07; 
                x.E08; x.E09; x.E10; x.E11; x.E12; x.E13; x.E14; x.E15; 
                x.E16; x.E17; x.E18; x.E19; x.E20; x.E21; x.E22; x.E23; 
                x.E24; x.E25; x.E26; x.E27; x.E28; x.E29; x.E30; x.E31
            |]

        member x.Item
            with [<MethodImpl(MethodImplOptions.Unmanaged)>]
                get(i : int) = 
                if i >= 32 || i < 0 then raise <| IndexOutOfRangeException()
                NativePtr.get (NativePtr.ofNativeInt<byte> (NativePtr.toNativeInt &&x)) i
             
             and [<MethodImpl(MethodImplOptions.Unmanaged)>]
                set (i : int) (value : byte) =
                if i >= 32 || i < 0 then raise <| IndexOutOfRangeException()
                NativePtr.set (NativePtr.ofNativeInt (NativePtr.toNativeInt &&x)) i value

    end


[<AutoOpen>]
module private TLSHImplementationHelpers =
    open MBrace.FsPickler
    
    let inline private cont (a : byte) (c1 : char) (res : byref<byte>) =
        if c1 >= '0' && c1 <= '9' then
            res <- a ||| (byte c1 - 48uy)
            true
        elif c1 >= 'a' && c1 <= 'f' then
            res <- a ||| (byte c1 - 87uy)
            true
        elif c1 >= 'A' && c1 <= 'F' then
            res <- a ||| (byte c1 - 55uy)
            true
        else
            false

    let parseHexByte (str : string) (o : int) (res : byref<byte>) =
        let o1 = o + 1
        if o1 < str.Length then
            let c0 = str.[o]
            let c1 = str.[o1]

            if c0 >= '0' && c0 <= '9' then 
                let a = (byte c0 - 48uy) <<< 4
                cont a c1 &res
            elif c0 >= 'a' && c0 <= 'f' then 
                let a = (byte c0 - 87uy) <<< 4
                cont a c1 &res
            elif c0 >= 'A' && c0 <= 'F' then
                let a = (byte c0 - 55uy) <<< 4
                cont a c1 &res
            else 
                false
        else
            false

    let inline private contSwapped (a : byte) (c1 : char) (res : byref<byte>) =
        if c1 >= '0' && c1 <= '9' then
            res <- a ||| ((byte c1 - 48uy) <<< 4)
            true
        elif c1 >= 'a' && c1 <= 'f' then
            res <- a ||| ((byte c1 - 87uy) <<< 4)
            true
        elif c1 >= 'A' && c1 <= 'F' then
            res <- a ||| ((byte c1 - 55uy) <<< 4)
            true
        else
            false

    let parseHexByteSwapped (str : string) (o : int) (res : byref<byte>) =
        let o1 = o + 1
        if o1 < str.Length then
            let c0 = str.[o]
            let c1 = str.[o1]


            if c0 >= '0' && c0 <= '9' then 
                let a = (byte c0 - 48uy)
                contSwapped a c1 &res
            elif c0 >= 'a' && c0 <= 'f' then 
                let a = (byte c0 - 87uy)
                contSwapped a c1 &res
            elif c0 >= 'A' && c0 <= 'F' then
                let a = (byte c0 - 55uy)
                contSwapped a c1 &res
            else 
                false
        else
            false
 
    let pickler = FsPickler.CreateBinarySerializer()
    
    let pearsonRandomTable =
        [|
            1uy; 87uy; 49uy; 12uy; 176uy; 178uy; 102uy; 166uy; 121uy; 193uy; 6uy; 84uy; 249uy; 230uy; 44uy; 163uy;
            14uy; 197uy; 213uy; 181uy; 161uy; 85uy; 218uy; 80uy; 64uy; 239uy; 24uy; 226uy; 236uy; 142uy; 38uy; 200uy;
            110uy; 177uy; 104uy; 103uy; 141uy; 253uy; 255uy; 50uy; 77uy; 101uy; 81uy; 18uy; 45uy; 96uy; 31uy; 222uy;
            25uy; 107uy; 190uy; 70uy; 86uy; 237uy; 240uy; 34uy; 72uy; 242uy; 20uy; 214uy; 244uy; 227uy; 149uy; 235uy;
            97uy; 234uy; 57uy; 22uy; 60uy; 250uy; 82uy; 175uy; 208uy; 5uy; 127uy; 199uy; 111uy; 62uy; 135uy; 248uy;
            174uy; 169uy; 211uy; 58uy; 66uy; 154uy; 106uy; 195uy; 245uy; 171uy; 17uy; 187uy; 182uy; 179uy; 0uy; 243uy;
            132uy; 56uy; 148uy; 75uy; 128uy; 133uy; 158uy; 100uy; 130uy; 126uy; 91uy; 13uy; 153uy; 246uy; 216uy; 219uy;
            119uy; 68uy; 223uy; 78uy; 83uy; 88uy; 201uy; 99uy; 122uy; 11uy; 92uy; 32uy; 136uy; 114uy; 52uy; 10uy;
            138uy; 30uy; 48uy; 183uy; 156uy; 35uy; 61uy; 26uy; 143uy; 74uy; 251uy; 94uy; 129uy; 162uy; 63uy; 152uy;
            170uy; 7uy; 115uy; 167uy; 241uy; 206uy; 3uy; 150uy; 55uy; 59uy; 151uy; 220uy; 90uy; 53uy; 23uy; 131uy;
            125uy; 173uy; 15uy; 238uy; 79uy; 95uy; 89uy; 16uy; 105uy; 137uy; 225uy; 224uy; 217uy; 160uy; 37uy; 123uy;
            118uy; 73uy; 2uy; 157uy; 46uy; 116uy; 9uy; 145uy; 134uy; 228uy; 207uy; 212uy; 202uy; 215uy; 69uy; 229uy;
            27uy; 188uy; 67uy; 124uy; 168uy; 252uy; 42uy; 4uy; 29uy; 108uy; 21uy; 247uy; 19uy; 205uy; 39uy; 203uy;
            233uy; 40uy; 186uy; 147uy; 198uy; 192uy; 155uy; 33uy; 164uy; 191uy; 98uy; 204uy; 165uy; 180uy; 117uy; 76uy;
            140uy; 36uy; 210uy; 172uy; 41uy; 54uy; 159uy; 8uy; 185uy; 232uy; 113uy; 196uy; 231uy; 47uy; 146uy; 120uy;
            51uy; 65uy; 28uy; 144uy; 254uy; 221uy; 93uy; 189uy; 194uy; 139uy; 112uy; 43uy; 71uy; 109uy; 184uy; 209uy
        |]

    [<Literal>]
    let windowSize = 5
    [<Literal>]
    let rangeSize = 5
    [<Literal>]
    let minDataLength = 50
    [<Literal>]
    let codeSize = 32
    [<Literal>]
    let effectiveBuckets = 128
    [<Literal>]
    let bucketCount = 256
    [<Literal>]
    let minNonZero = 64 //4*codeSize/2

    let inline wrapIndex i = (i+rangeSize) % rangeSize

    let inline b_mapping (salt : byte) (i : byte) (j : byte) (k : byte) =
        let h = pearsonRandomTable.[int salt]
        let h = pearsonRandomTable.[int (h ^^^ i)]
        let h = pearsonRandomTable.[int (h ^^^ j)]
        let h = pearsonRandomTable.[int (h ^^^ k)]
        h
        
    let inline inc (a : byref<int>) = a <- a + 1
          
    let inline pinc (a : nativeptr<int>) (element : int) = 
        let ptr = NativePtr.add a element
        NativePtr.write ptr (NativePtr.read ptr + 1)
          
    let findQuartile (data : int[]) =
        let s = Array.take effectiveBuckets data

        let quartile = effectiveBuckets >>> 2
        let p1 = quartile-1
        let p2 = p1 + quartile
        let p3 = p2 + quartile

        System.Array.Sort(s, 0, s.Length)
        struct(s.[p1], s.[p2], s.[p3])

    let lenCapturing len =
        let l =
            if len < 656 then
                floor ( log (float len) / 0.4054651 ) |> int
            elif len < 3199 then
                floor ( log (float len) / 0.26236426 - 8.72777) |> int
            else
                floor ( log (float len) / 0.095310180 - 62.5472) |> int
        byte l

    let inline modDiff x y r =
        //abs (x - y) % r
        if y > x then
            let a = y - x
            let b = x + r - y
            min a b
        else
            let a = x - y
            let b = y + r - x
            min a b

    let CRangeLValue = 256
    let CRangeQRatio = 16

    type TlshBuilder() =
        let buckets = Array.zeroCreate<int> bucketCount
        let win = Array.zeroCreate<byte> windowSize
        let mutable totalLength = 0
        let mutable checksum = 0uy

        member x.Append(ptr : nativeptr<'a>, count : int) =
            use pBuckets = fixed buckets
            let pdata = NativePtr.ofNativeInt<byte> (NativePtr.toNativeInt ptr)
            let length = sizeof<'a> * count

            let mutable j = totalLength % rangeSize
            let mutable j1 = wrapIndex(j - 1)
            let mutable j2 = wrapIndex(j - 2)
            let mutable j3 = wrapIndex(j - 3)
            let mutable j4 = wrapIndex(j - 4)

            let mutable psrc = pdata

            let mutable fedLength = totalLength
            for i in 0 .. length - 1 do
                win.[j] <- NativePtr.read psrc

                if fedLength >= 4 then
                    checksum <- b_mapping 0uy win.[j] win.[j1] checksum

                    pinc pBuckets (int (b_mapping 2uy win.[j] win.[j1] win.[j2])  )
                    pinc pBuckets (int (b_mapping 3uy win.[j] win.[j1] win.[j3])  )
                    pinc pBuckets (int (b_mapping 5uy win.[j] win.[j2] win.[j3])  )
                    pinc pBuckets (int (b_mapping 7uy win.[j] win.[j2] win.[j4])  )
                    pinc pBuckets (int (b_mapping 11uy win.[j] win.[j1] win.[j4]) )
                    pinc pBuckets (int (b_mapping 13uy win.[j] win.[j3] win.[j4]) )

                let t = j4 
                j4 <- j3
                j3 <- j2
                j2 <- j1
                j1 <- j
                j <- t
                inc &fedLength
                psrc <- NativePtr.add psrc 1
            totalLength <- totalLength + length
            
        member x.Append(ptr : nativeptr<'a>) =
            x.Append(ptr, 1)

        member x.Append(str : string) =
            let span = str.AsSpan()
            x.Append(&&span.GetPinnableReference(), span.Length)

        member x.AppendValue(value : 'a) =
            let mutable v = value
            x.Append(&&v, 1)

        member x.Append(data : byte[], offset : int, length : int) =
            if offset < 0 || length < 0 || offset + length > data.Length then
                raise <| IndexOutOfRangeException()
            use pBuckets = fixed buckets
            use pData = fixed data
            let data = ()
            let buckets = ()

            let mutable j = totalLength % rangeSize
            let mutable j1 = wrapIndex(j - 1)
            let mutable j2 = wrapIndex(j - 2)
            let mutable j3 = wrapIndex(j - 3)
            let mutable j4 = wrapIndex(j - 4)

            let mutable psrc = NativePtr.add pData offset

            let mutable fedLength = totalLength
            for i in 0 .. length - 1 do
                win.[j] <- NativePtr.read psrc

                if fedLength >= 4 then
                    checksum <- b_mapping 0uy win.[j] win.[j1] checksum

                    pinc pBuckets (int (b_mapping 2uy win.[j] win.[j1] win.[j2])  )
                    pinc pBuckets (int (b_mapping 3uy win.[j] win.[j1] win.[j3])  )
                    pinc pBuckets (int (b_mapping 5uy win.[j] win.[j2] win.[j3])  )
                    pinc pBuckets (int (b_mapping 7uy win.[j] win.[j2] win.[j4])  )
                    pinc pBuckets (int (b_mapping 11uy win.[j] win.[j1] win.[j4]) )
                    pinc pBuckets (int (b_mapping 13uy win.[j] win.[j3] win.[j4]) )

                let t = j4 
                j4 <- j3
                j3 <- j2
                j2 <- j1
                j1 <- j
                j <- t
                inc &fedLength
                psrc <- NativePtr.add psrc 1
            totalLength <- totalLength + length
            
        member x.Append(data : byte[]) =
            x.Append(data, 0, data.Length)

        member x.CanGetHash =
            totalLength >= minDataLength &&
            (Array.sumBy (fun i -> if i > 0 then 1 else 0) buckets >= minNonZero)

        member x.Finish(c : byref<byte>, lValue : byref<byte>, q1Ratio : byref<byte>, q2Ratio : byref<byte>, code : byref<Arr32>) =
            let struct(q1, q2, q3) = findQuartile buckets

            for i in 0 .. codeSize - 1 do
                let mutable h = 0uy
                for j in 0 .. 3 do
                    let k = buckets.[4*i+j]
                        
                    if q3 < k then
                        h <- h + (3uy <<< (j*2))
                    elif q2 < k then
                        h <- h + (2uy <<< (j*2))
                    elif q1 < k then
                        h <- h + (1uy <<< (j*2))

                code.[i] <- h
                    
            c <- checksum
            lValue <- lenCapturing totalLength
            q1Ratio <- byte (int (float q1 * 100.0 / float q3) % 16)
            q2Ratio <- byte (int (float q2 * 100.0 / float q3) % 16)

            //let mutable h = TLSHHash()
            //h.Checksum <- checksum
            //h.LValue <- lValue
            //h.Q1Ratio <- byte q1Ratio
            //h.Q2Ratio <- byte q2Ratio
            //h.Code <- code
            //h


[<Sealed>]
type TLSHPickler private(fmt) =
    inherit FsPicklerSerializer(fmt)

    static let tlshPicklerFormat : IPickleFormatProvider =
        { new IPickleFormatProvider with
            member x.CreateReader(_,_,_,_) =
                failwith "no hash reader"
            member x.CreateWriter(dst,_,_,leaveOpen) =
                let builder = TlshBuilder()
                { new IPickleFormatWriter with
                    member x.Dispose() =
                        let mutable c = 0uy
                        let mutable l = 0uy
                        let mutable q1 = 0uy
                        let mutable q2 = 0uy
                        let mutable code = Arr32()
                        builder.Finish(&c, &l, &q1, &q2, &code)
                        let res = TLSHash(code, c, l, q1, q2)
                        let arr = res.ToByteArray()
                        dst.Write(arr, 0, arr.Length)
                        if not leaveOpen then
                            dst.Dispose()

                    member x.Flush() = 
                        let mutable c = 0uy
                        let mutable l = 0uy
                        let mutable q1 = 0uy
                        let mutable q2 = 0uy
                        let mutable code = Arr32()
                        builder.Finish(&c, &l, &q1, &q2, &code)
                        let res = TLSHash(code, c, l, q1, q2)
                        let arr = res.ToByteArray()
                        dst.Write(arr, 0, arr.Length)
                        dst.Flush()
                    member x.EndWriteRoot() = ()
                    member x.EndWriteObject() = ()
                    member x.WriteNextSequenceElement _ = ()
                    member x.IsPrimitiveArraySerializationSupported = true
                    member x.PreferLengthPrefixInSequences = false
                    member x.SerializeUnionCaseNames = true
                    member x.UseNamedEnumSerialization = false
                    member x.BeginWriteRoot (tag : string) = 
                        builder.Append tag

                    member x.BeginWriteObject (tag : string) (flags : ObjectFlags) =   
                        builder.Append tag

                    member x.WriteBigInteger(tag : string) (v : bigint) : unit =  
                        builder.Append tag
                        builder.AppendValue v

                    member x.WriteBoolean(tag : string) (v : bool) : unit = 
                        builder.Append tag
                        builder.AppendValue(if v then 1uy else 0uy)

                    member x.WriteByte(tag : string) (v : byte) : unit =
                        builder.Append tag
                        builder.AppendValue v

                    member x.WriteSByte(tag : string) (v : sbyte) : unit =
                        builder.Append tag
                        builder.AppendValue v

                    member x.WriteInt16(tag : string) (v : int16) : unit =
                        builder.Append tag
                        builder.AppendValue v

                    member x.WriteUInt16(tag : string) (v : uint16) : unit =
                        builder.Append tag
                        builder.AppendValue v

                    member x.WriteInt32(tag : string) (v : int32) : unit =
                        builder.Append tag
                        builder.AppendValue v

                    member x.WriteUInt32(tag : string) (v : uint32) : unit =
                        builder.Append tag
                        builder.AppendValue v

                    member x.WriteInt64(tag : string) (v : int64) : unit =
                        builder.Append tag
                        builder.AppendValue v

                    member x.WriteUInt64(tag : string) (v : uint64) : unit =
                        builder.Append tag
                        builder.AppendValue v

                    member x.WritePrimitiveArray(tag : string) (arr : Array) =
                        builder.Append tag
                        let size = arr.Length * Marshal.SizeOf (arr.GetType().GetElementType())
                        let gc = GCHandle.Alloc(arr, GCHandleType.Pinned)
                        try builder.Append(NativePtr.ofNativeInt<byte> (gc.AddrOfPinnedObject()), size)
                        finally gc.Free()
                    member x.WriteChar(tag : string) (v : char) =
                        builder.Append tag
                        builder.AppendValue v

                    member x.WriteDecimal(tag : string) (v : decimal) =
                        builder.Append tag
                        builder.AppendValue v

                    member x.WriteSingle(tag : string) (v : float32) =
                        builder.Append tag
                        builder.AppendValue v

                    member x.WriteDouble(tag : string) (v : float) =
                        builder.Append tag
                        builder.AppendValue v

                    member x.WriteString(tag : string) (v : string) =
                        builder.Append tag
                        builder.Append v

                    member x.WriteGuid(tag : string) (v : Guid) =
                        builder.Append tag
                        builder.AppendValue v

                    member x.WriteDateTime(tag : string) (v : DateTime) =
                        builder.Append tag
                        builder.AppendValue v

                    member x.WriteDateTimeOffset(tag : string) (v : DateTimeOffset) =
                        builder.Append tag
                        builder.AppendValue v

                    member x.WriteTimeSpan(tag : string) (v : TimeSpan) =
                        builder.Append tag
                        builder.AppendValue v

                    member x.WriteBytes(tag : string) (v : byte[]) =
                        builder.Append tag
                        builder.Append v
                                
                    member x.WriteCachedObjectId(id : int64) =
                        builder.AppendValue id
                }
            member x.DefaultEncoding =
                System.Text.Encoding.UTF8
            member x.Name = 
                "TLSH"
        }

    static let instance = TLSHPickler(tlshPicklerFormat)

    static member ComputeHash(value : 'a) =
        let data = instance.Pickle value
        TLSHash.FromByteArray(data, 0, data.Length)
    
[<Struct; StructLayout(LayoutKind.Sequential); CustomPickler>]
type TLSHash(code : Arr32, checksum : byte, lValue : byte, q1Ratio : byte, q2Ratio : byte) =
    

    member x.Code = code
    member x.Checksum = checksum
    member x.LValue = lValue
    member x.Q1Ratio = q1Ratio
    member x.Q2Ratio = q2Ratio

    static member private CreatePickler (r : IPicklerResolver) =
        let arrPickler = Pickler.array Pickler.byte
        Pickler.FromPrimitives(
            (fun rs -> 
                let arr = arrPickler.Read rs "hash"
                TLSHash.FromByteArray(arr, 0, arr.Length)
            ),
            (fun ws (v : TLSHash) -> 
                arrPickler.Write ws "hash" (v.ToByteArray())
            )
        )

    member x.ToByteArray() =
            
        let inline swapbyte (b : byte) =
            ((b &&& 0xFuy) <<< 4) ||| (b >>> 4)

        let res = Array.zeroCreate 35
        res.[0] <- swapbyte x.Checksum
        res.[1] <- swapbyte x.LValue
        res.[2] <- (x.Q1Ratio <<< 4) ||| x.Q2Ratio
            
        let ptr = &&x
        let bptr = NativePtr.ofNativeInt<byte> (NativePtr.toNativeInt ptr)
        let mutable o = 3
        for i in 31 .. -1 .. 0 do
            res.[o] <- NativePtr.get bptr i
            o <- o + 1

        res

    override x.ToString() =
        let sb = System.Text.StringBuilder()
        //sb.Append "T1" |> ignore
            
        let inline swapbyte (b : byte) =
            ((b &&& 0xFuy) <<< 4) ||| (b >>> 4)

        sb.AppendFormat("{0:X2}", swapbyte x.Checksum) |> ignore
        sb.AppendFormat("{0:X2}", swapbyte x.LValue) |> ignore
        sb.AppendFormat("{0:X1}", x.Q1Ratio) |> ignore
        sb.AppendFormat("{0:X1}", x.Q2Ratio) |> ignore

        let ptr = &&x
        let bptr = NativePtr.ofNativeInt<byte> (NativePtr.toNativeInt ptr)

        for i in 31 .. -1 .. 0 do
            sb.AppendFormat("{0:X2}", (NativePtr.get bptr i)) |> ignore

        sb.ToString()

    static member FromByteArray(array : byte[], offset : int, length : int) =
        if length <> 35 then raise <| ArgumentException("array must have length 35")

        let mutable o = offset
        let c = array.[o]
        inc &o
        let l = array.[o]
        inc &o
        let q1 = array.[o] >>> 4
        let q2 = array.[o] &&& 0xFuy
        inc &o
        let mutable code = Arr32()
        for i in 0 .. 31 do
            code.[i] <- array.[o]
            inc &o
        TLSHash(code, c, l, q1, q2)

    static member TryParse(str : string, res : outref<TLSHash>) =
        let mutable tmp = 0uy
        res <- Unchecked.defaultof<TLSHash>

        let mutable sOff = 0
        if str.StartsWith "T1" then 
            sOff <- 2

        let mutable c = 0uy
        let mutable l = 0uy
        let mutable q1 = 0uy
        let mutable q2 = 0uy

        if parseHexByteSwapped str sOff &c then
            sOff <- sOff + 2
            if parseHexByteSwapped str sOff &l then
                sOff <- sOff + 2
                if parseHexByte str sOff &tmp then
                    sOff <- sOff + 2
                    q2 <- tmp &&& 0xFuy
                    q1 <- tmp >>> 4

                    let mutable i = 0
                    let mutable o = 31
                    let mutable success = true
                    let mutable code = Arr32()
                    while success && i < 32 do
                        if parseHexByte str sOff &tmp then
                            code.[o] <- tmp
                            sOff <- sOff + 2
                            o <- o - 1
                            i <- i + 1
                        else
                            success <- false

                    if success && sOff = str.Length then 
                        res <- TLSHash(code, c, l, q1, q2)
                        true
                    else false
                else
                    false
            else
                false
        else
            false
        
    static member Parse(str : string) =
        let mutable res = TLSHash()
        if TLSHash.TryParse(str, &res) then res
        else invalidArg "str" (sprintf "%A is not a TLSHash" str)

    static member Compute (array : byte[], offset : int, length : int) =
        let b = TlshBuilder()
        b.Append(array, offset, length)
        while not b.CanGetHash do
            b.Append(array, offset, length)


        let mutable c = 0uy
        let mutable l = 0uy
        let mutable q1 = 0uy
        let mutable q2 = 0uy
        let mutable code = Arr32()
        b.Finish(&c, &l, &q1, &q2, &code)
        TLSHash(code, c, l, q1, q2)
        
    static member Compute (array : byte[]) =
        TLSHash.Compute(array, 0, array.Length)

    static member Compute (str : string) =
        let arr = System.Text.Encoding.UTF8.GetBytes str
        TLSHash.Compute arr

    static member Compute (value : 'a) =
        if typeof<'a> = typeof<byte[]> then 
            TLSHash.Compute (unbox<byte[]> value)
        elif typeof<'a> = typeof<string> then
            TLSHash.Compute (unbox<string> value)
        else
            TLSHPickler.ComputeHash value

    static member Distance (a : TLSHash, b : TLSHash) =
        let mutable diff = 0
        // 14*12*2 + 1 + 32*6
        let q1 = modDiff (int a.Q1Ratio) (int b.Q1Ratio) CRangeQRatio
        if q1 <= 1 then diff <- diff + q1
        else diff <- diff + (q1 - 1) * 12
        
        let q2 = modDiff (int a.Q2Ratio) (int b.Q2Ratio) CRangeQRatio
        if q2 <= 1 then diff <- diff + q2
        else diff <- diff + (q2 - 1) * 12

        if a.Checksum <> b.Checksum then
            diff <- diff + 1

        let pa = NativePtr.stackalloc 2
        let pb = NativePtr.add pa 1
        NativePtr.write pa a
        NativePtr.write pb b

        let pa = NativePtr.ofNativeInt<byte> (NativePtr.toNativeInt pa)
        let pb = NativePtr.ofNativeInt<byte> (NativePtr.toNativeInt pb)

        for i in 0 .. 31 do
            let mutable va = NativePtr.get pa i
            let mutable vb = NativePtr.get pb i

            for i in 0 .. 3 do
                let d = abs (int(va &&& 3uy) - int(vb &&& 3uy))
                if d = 3 then diff <- diff + 6
                else diff <- diff + int d

                va <- va >>> 2
                vb <- vb >>> 2


        diff

