
// 用于表示HTML标签
type HTMLTag = {
    name : string                   // 标签名
    children : HTMLElement list     // 子元素
}

// 用于表示HTML标签的属性
and HTMLAttribute = {
    key : string                    // 属性名
    value : string                  // 属性值
}

// 表示任何HTML元素
and HTMLElement =                   
| Tag of HTMLTag                    // 如果是Tag的情况，则存储一个HTMLTag
| Attribute of HTMLAttribute        // 如果是Attribute的情况，则存储一个HTMLAttribute
| Text of string                    // 如果是Text的情况，存储的是HTML标签的文字，用一个字符串表示

// 这个函数用于把标签名和子元素打包成一个HTMLElement
// 类型：string -> HTMLElement list -> HTMLElement
let tag (tagName:string) (children:HTMLElement list) : HTMLElement = Tag {
    name = tagName
    children = children
}

// 一些基本标签
// 类型：HTMLElement list -> HTMLElement
let html = tag "html"
let head = tag "head"
let body = tag "body"
let script = tag "script"
let title = tag "title"

// 一个HTML文档
// 类型： HTMLElement
let myHtmlDocument = 
    html [
        head [
            script [
                Attribute {key="src"; value="../xx.js"}
            ]
            title []]
        body []]


// 这个函数用于把HTML元素转换成HTML代码
// 类型：HTMLElement -> string
let rec toString (element:HTMLElement) : string =
    match element with  
    | Text text -> text     // 如果它是一段文本，则直接返回文本
    | Tag tag ->            // 如果它是一个标签，则执行下面的运算

        // 这个函数用于判断一个element是不是Attribute
        // 类型：HTMLElement -> bool
        let isAttribute (element:HTMLElement) : bool =
            match element with
            | Attribute _ -> true
            | _ -> false

        // 这个函数用于把一个string list加到一个字符串里，变成单个字符串
        // 类型：string list -> string
        let stringSum (x:string list) = 
            if x.IsEmpty then ""
            else List.reduce (fun a b -> a + b) x

        // 这里从children里取得了所有的Attribute，并转换成了HTML代码
        // 类型：string
        let attributes =
            tag.children
            |> List.filter isAttribute  // 筛选出所有的Attribute
            |> List.map toString        // 转换成HTML代码
            |> stringSum                // 把所有的字符串加到一起
        
        let a = "<" + tag.name + attributes + ">"   // HTML的属性和开标签，类型：string
        let b = "</" + tag.name + ">"               // HTML的闭标签，类型：string

        // 这里从children里取得了所有的非Attribute的部分，并转换成了HTML代码
        // 类型：string
        let children =  
            tag.children
            |> List.filter (isAttribute >> not)     // 筛选出所有非Attribute的元素
            |> List.map toString                    // 转换成HTML代码
            |> stringSum                            // 把所有字符串加到一起
        a + children + b                            // 最终得到的当前标签的HTML代码，类型：string

    | Attribute attr ->     // 如果element是一个Attribute
        " " + attr.key + "=\"" + attr.value + "\""  // 则生成Attribute的HTML代码
        


printfn "%s" (toString myHtmlDocument)
