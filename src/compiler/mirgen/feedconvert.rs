use crate::hir::expr::Expr;
use crate::utils::metadata::WithMeta;


struct FeedConvContext{
    feedid:i64,
    
}

// pub fn convert_feed(expr:Expr)->Expr{
//     match expr{
//         Expr::Feed(x,body)=>{
//             let newx = x.0;
//             let newbody = 
//             Expr::Lambda(x,)
//         }
//     }
// }